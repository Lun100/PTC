#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "semantic.h"

struct TreeNode {
    char *name;
    char *value;
    int line;
    int is_token;
    int child_count;
    struct TreeNode **children;
};

typedef enum {
    TYPE_INVALID,
    TYPE_BASIC,
    TYPE_ARRAY,
    TYPE_STRUCT
} TypeKind;

typedef enum {
    BASIC_INT,
    BASIC_FLOAT
} BasicKind;

typedef struct Type Type;
typedef struct Field Field;
typedef struct VarSymbol VarSymbol;
typedef struct StructSymbol StructSymbol;
typedef struct FunctionSymbol FunctionSymbol;

struct Type {
    TypeKind kind;
    union {
        BasicKind basic;
        struct {
            Type *elem;
        } array;
        struct {
            char *name;
            Field *fields;
        } structure;
    } u;
};

struct Field {
    char *name;
    Type *type;
    int line;
    Field *next;
};

struct VarSymbol {
    char *name;
    Type *type;
    int line;
    VarSymbol *next;
};

struct StructSymbol {
    char *name;
    Type *type;
    int line;
    StructSymbol *next;
};

struct FunctionSymbol {
    char *name;
    Type *return_type;
    Field *params;
    int declared_line;
    int defined_line;
    int is_defined;
    FunctionSymbol *next;
};

typedef struct {
    Type *type;
    int is_lvalue;
} ExpResult;

static VarSymbol *var_symbols = NULL;
static StructSymbol *struct_symbols = NULL;
static FunctionSymbol *function_symbols = NULL;
static int semantic_error_count = 0;
static int anonymous_struct_id = 0;

static Type invalid_type = { TYPE_INVALID, { BASIC_INT } };
static Type int_type = { TYPE_BASIC, { BASIC_INT } };
static Type float_type = { TYPE_BASIC, { BASIC_FLOAT } };

static int is_node(TreeNode *node, const char *name);
static TreeNode *child(TreeNode *node, int index);
static char *dup_string(const char *src);
static void report_semantic_error(int type, int line, const char *message);
static Type *new_array_type(Type *elem);
static Type *new_struct_type(const char *name, Field *fields);
static Field *new_field(const char *name, Type *type, int line);
static Type *analyze_specifier(TreeNode *node);
static Type *analyze_struct_specifier(TreeNode *node);
static Field *analyze_deflist(TreeNode *node, int in_struct, Type *function_type);
static Field *analyze_dec_list(TreeNode *node, Type *base_type, int in_struct, Type *function_type);
static Field *analyze_dec(TreeNode *node, Type *base_type, int in_struct, Type *function_type);
static Type *build_vardec_type(TreeNode *node, Type *base_type, char **name_out, int *line_out);
static void analyze_extdeflist(TreeNode *node);
static void analyze_extdef(TreeNode *node);
static void analyze_compst(TreeNode *node, Type *function_type);
static void analyze_stmtlist(TreeNode *node, Type *function_type);
static void analyze_stmt(TreeNode *node, Type *function_type);
static ExpResult analyze_exp(TreeNode *node);
static Field *analyze_args(TreeNode *node);
static void check_pending_function_definitions(void);
static int type_equal(Type *left, Type *right);
static int field_list_equal(Field *left, Field *right);
static int is_basic_numeric(Type *type);
static int is_basic_int(Type *type);
static int count_array_dimensions(Type *type);
static Type *array_base_type(Type *type);
static VarSymbol *find_var(const char *name);
static StructSymbol *find_struct(const char *name);
static FunctionSymbol *find_function(const char *name);
static Field *find_field(Field *fields, const char *name);
static int add_variable_symbol(const char *name, Type *type, int line);
static int add_struct_symbol(const char *name, Type *type, int line);
static void declare_or_define_function(const char *name, Type *return_type, Field *params, int line, int is_definition);
static int function_signature_equal(FunctionSymbol *func, Type *return_type, Field *params);
static Field *extract_function_params(TreeNode *node, int register_params);
static void free_field_list(Field *field);
static void check_struct_field_duplicates(Field *fields);

int semantic_analyze(TreeNode *root) {
    var_symbols = NULL;
    struct_symbols = NULL;
    function_symbols = NULL;
    semantic_error_count = 0;
    anonymous_struct_id = 0;

    if (root != NULL) {
        analyze_extdeflist(child(root, 0));
        check_pending_function_definitions();
    }

    return semantic_error_count;
}

static int is_node(TreeNode *node, const char *name) {
    return node != NULL && strcmp(node->name, name) == 0;
}

static TreeNode *child(TreeNode *node, int index) {
    if (node == NULL || index < 0 || index >= node->child_count) {
        return NULL;
    }
    return node->children[index];
}

static char *dup_string(const char *src) {
    size_t len;
    char *dst;

    if (src == NULL) {
        return NULL;
    }

    len = strlen(src) + 1;
    dst = (char *)malloc(len);
    if (dst == NULL) {
        fprintf(stderr, "out of memory\n");
        exit(1);
    }
    memcpy(dst, src, len);
    return dst;
}

static void report_semantic_error(int type, int line, const char *message) {
    ++semantic_error_count;
    printf("Error type %d at Line %d: %s.\n", type, line, message);
}

static Type *new_array_type(Type *elem) {
    Type *type = (Type *)calloc(1, sizeof(Type));
    if (type == NULL) {
        fprintf(stderr, "out of memory\n");
        exit(1);
    }
    type->kind = TYPE_ARRAY;
    type->u.array.elem = elem;
    return type;
}

static Type *new_struct_type(const char *name, Field *fields) {
    Type *type = (Type *)calloc(1, sizeof(Type));
    if (type == NULL) {
        fprintf(stderr, "out of memory\n");
        exit(1);
    }
    type->kind = TYPE_STRUCT;
    type->u.structure.name = dup_string(name);
    type->u.structure.fields = fields;
    return type;
}

static Field *new_field(const char *name, Type *type, int line) {
    Field *field = (Field *)calloc(1, sizeof(Field));
    if (field == NULL) {
        fprintf(stderr, "out of memory\n");
        exit(1);
    }
    field->name = dup_string(name);
    field->type = type;
    field->line = line;
    return field;
}

static VarSymbol *find_var(const char *name) {
    VarSymbol *current = var_symbols;
    while (current != NULL) {
        if (strcmp(current->name, name) == 0) {
            return current;
        }
        current = current->next;
    }
    return NULL;
}

static StructSymbol *find_struct(const char *name) {
    StructSymbol *current = struct_symbols;
    while (current != NULL) {
        if (strcmp(current->name, name) == 0) {
            return current;
        }
        current = current->next;
    }
    return NULL;
}

static FunctionSymbol *find_function(const char *name) {
    FunctionSymbol *current = function_symbols;
    while (current != NULL) {
        if (strcmp(current->name, name) == 0) {
            return current;
        }
        current = current->next;
    }
    return NULL;
}

static Field *find_field(Field *fields, const char *name) {
    Field *current = fields;
    while (current != NULL) {
        if (strcmp(current->name, name) == 0) {
            return current;
        }
        current = current->next;
    }
    return NULL;
}

static int add_variable_symbol(const char *name, Type *type, int line) {
    VarSymbol *symbol;
    char message[256];

    if (find_var(name) != NULL || find_struct(name) != NULL) {
        snprintf(message, sizeof(message), "Redefined variable \"%s\"", name);
        report_semantic_error(3, line, message);
        return 0;
    }

    symbol = (VarSymbol *)calloc(1, sizeof(VarSymbol));
    if (symbol == NULL) {
        fprintf(stderr, "out of memory\n");
        exit(1);
    }
    symbol->name = dup_string(name);
    symbol->type = type;
    symbol->line = line;
    symbol->next = var_symbols;
    var_symbols = symbol;
    return 1;
}

static int add_struct_symbol(const char *name, Type *type, int line) {
    StructSymbol *symbol;
    char message[256];

    if (find_struct(name) != NULL || find_var(name) != NULL) {
        snprintf(message, sizeof(message), "Duplicated name \"%s\"", name);
        report_semantic_error(16, line, message);
        return 0;
    }

    symbol = (StructSymbol *)calloc(1, sizeof(StructSymbol));
    if (symbol == NULL) {
        fprintf(stderr, "out of memory\n");
        exit(1);
    }
    symbol->name = dup_string(name);
    symbol->type = type;
    symbol->line = line;
    symbol->next = struct_symbols;
    struct_symbols = symbol;
    return 1;
}

static int count_array_dimensions(Type *type) {
    int count = 0;
    while (type != NULL && type->kind == TYPE_ARRAY) {
        ++count;
        type = type->u.array.elem;
    }
    return count;
}

static Type *array_base_type(Type *type) {
    while (type != NULL && type->kind == TYPE_ARRAY) {
        type = type->u.array.elem;
    }
    return type;
}

static int type_equal(Type *left, Type *right) {
    if (left == right) {
        return 1;
    }
    if (left == NULL || right == NULL) {
        return 0;
    }
    if (left->kind == TYPE_INVALID || right->kind == TYPE_INVALID) {
        return 1;
    }
    if (left->kind != right->kind) {
        return 0;
    }

    switch (left->kind) {
        case TYPE_BASIC:
            return left->u.basic == right->u.basic;
        case TYPE_ARRAY:
            return count_array_dimensions(left) == count_array_dimensions(right) &&
                   type_equal(array_base_type(left), array_base_type(right));
        case TYPE_STRUCT:
            return strcmp(left->u.structure.name, right->u.structure.name) == 0;
        case TYPE_INVALID:
        default:
            return 1;
    }
}

static int field_list_equal(Field *left, Field *right) {
    while (left != NULL && right != NULL) {
        if (!type_equal(left->type, right->type)) {
            return 0;
        }
        left = left->next;
        right = right->next;
    }
    return left == NULL && right == NULL;
}

static int is_basic_numeric(Type *type) {
    return type != NULL && type->kind == TYPE_BASIC &&
           (type->u.basic == BASIC_INT || type->u.basic == BASIC_FLOAT);
}

static int is_basic_int(Type *type) {
    return type != NULL && type->kind == TYPE_BASIC && type->u.basic == BASIC_INT;
}

static int function_signature_equal(FunctionSymbol *func, Type *return_type, Field *params) {
    return type_equal(func->return_type, return_type) && field_list_equal(func->params, params);
}

static void declare_or_define_function(const char *name, Type *return_type, Field *params, int line, int is_definition) {
    FunctionSymbol *func = find_function(name);
    char message[256];

    if (func == NULL) {
        func = (FunctionSymbol *)calloc(1, sizeof(FunctionSymbol));
        if (func == NULL) {
            fprintf(stderr, "out of memory\n");
            exit(1);
        }
        func->name = dup_string(name);
        func->return_type = return_type;
        func->params = params;
        func->declared_line = line;
        func->defined_line = is_definition ? line : 0;
        func->is_defined = is_definition;
        func->next = function_symbols;
        function_symbols = func;
        return;
    }

    if (is_definition) {
        if (func->is_defined) {
            snprintf(message, sizeof(message), "Redefined function \"%s\"", name);
            report_semantic_error(4, line, message);
            return;
        }
        if (!function_signature_equal(func, return_type, params)) {
            snprintf(message, sizeof(message), "Inconsistent declaration of function \"%s\"", name);
            report_semantic_error(19, line, message);
        }
        func->is_defined = 1;
        func->defined_line = line;
        if (func->declared_line == 0) {
            func->declared_line = line;
        }
        return;
    }

    if (!function_signature_equal(func, return_type, params)) {
        snprintf(message, sizeof(message), "Inconsistent declaration of function \"%s\"", name);
        report_semantic_error(19, line, message);
    }

    if (func->declared_line == 0) {
        func->declared_line = line;
    }
}

static Type *build_vardec_type(TreeNode *node, Type *base_type, char **name_out, int *line_out) {
    if (node == NULL) {
        return &invalid_type;
    }

    if (node->child_count == 1) {
        TreeNode *id = child(node, 0);
        if (name_out != NULL) {
            *name_out = id != NULL ? id->value : NULL;
        }
        if (line_out != NULL) {
            *line_out = id != NULL ? id->line : node->line;
        }
        return base_type;
    }

    return new_array_type(build_vardec_type(child(node, 0), base_type, name_out, line_out));
}

static Field *append_field(Field *head, Field *tail) {
    Field *current;

    if (head == NULL) {
        return tail;
    }

    current = head;
    while (current->next != NULL) {
        current = current->next;
    }
    current->next = tail;
    return head;
}

static Type *analyze_struct_specifier(TreeNode *node) {
    TreeNode *opt_tag;
    TreeNode *tag;
    TreeNode *deflist;
    Type *type;
    char hidden_name[64];

    if (node->child_count == 2) {
        tag = child(child(node, 1), 0);
        if (tag == NULL || find_struct(tag->value) == NULL) {
            char message[256];
            snprintf(message, sizeof(message), "Undefined structure \"%s\"", tag != NULL ? tag->value : "");
            report_semantic_error(17, tag != NULL ? tag->line : node->line, message);
            return &invalid_type;
        }
        return find_struct(tag->value)->type;
    }

    opt_tag = child(node, 1);
    deflist = child(node, 3);
    if (opt_tag != NULL) {
        TreeNode *id = child(opt_tag, 0);
        type = new_struct_type(id->value, analyze_deflist(deflist, 1, NULL));
        if (!add_struct_symbol(id->value, type, id->line)) {
            return type;
        }
        return type;
    }

    snprintf(hidden_name, sizeof(hidden_name), "__anonymous_struct_%d", ++anonymous_struct_id);
    return new_struct_type(hidden_name, analyze_deflist(deflist, 1, NULL));
}

static Type *analyze_specifier(TreeNode *node) {
    TreeNode *actual;

    if (node == NULL) {
        return &invalid_type;
    }

    actual = child(node, 0);
    if (actual == NULL) {
        return &invalid_type;
    }

    if (is_node(actual, "TYPE")) {
        if (strcmp(actual->value, "int") == 0) {
            return &int_type;
        }
        return &float_type;
    }

    return analyze_struct_specifier(actual);
}

static Field *analyze_dec(TreeNode *node, Type *base_type, int in_struct, Type *function_type) {
    Type *type;
    ExpResult init_result;
    char *name = NULL;
    int line = node->line;
    Field *field;
    char message[256];

    type = build_vardec_type(child(node, 0), base_type, &name, &line);
    field = new_field(name, type, line);

    if (in_struct) {
        if (child(node, 1) != NULL) {
            snprintf(message, sizeof(message), "Illegal initialization of field \"%s\"", name);
            report_semantic_error(15, line, message);
        }
        return field;
    }

    add_variable_symbol(name, type, line);

    if (node->child_count == 3) {
        init_result = analyze_exp(child(node, 2));
        if (!type_equal(type, init_result.type)) {
            report_semantic_error(5, line, "Type mismatched for assignment");
        }
    }

    (void)function_type;
    return field;
}

static Field *analyze_dec_list(TreeNode *node, Type *base_type, int in_struct, Type *function_type) {
    Field *head;
    Field *next;

    if (node == NULL) {
        return NULL;
    }

    if (node->child_count == 1) {
        return analyze_dec(child(node, 0), base_type, in_struct, function_type);
    }

    head = analyze_dec(child(node, 0), base_type, in_struct, function_type);
    next = analyze_dec_list(child(node, 2), base_type, in_struct, function_type);
    return append_field(head, next);
}

static void check_struct_field_duplicates(Field *fields) {
    Field *outer;
    Field *inner;
    char message[256];

    for (outer = fields; outer != NULL; outer = outer->next) {
        for (inner = outer->next; inner != NULL; inner = inner->next) {
            if (strcmp(outer->name, inner->name) == 0) {
                snprintf(message, sizeof(message), "Redefined field \"%s\"", inner->name);
                report_semantic_error(15, inner->line, message);
            }
        }
    }
}

static Field *analyze_deflist(TreeNode *node, int in_struct, Type *function_type) {
    Type *specifier_type;
    Field *fields;
    Field *rest;

    if (node == NULL || node->child_count == 0) {
        return NULL;
    }

    specifier_type = analyze_specifier(child(child(node, 0), 0));
    fields = analyze_dec_list(child(child(node, 0), 1), specifier_type, in_struct, function_type);
    rest = analyze_deflist(child(node, 1), in_struct, function_type);

    fields = append_field(fields, rest);
    if (in_struct) {
        check_struct_field_duplicates(fields);
    }

    return fields;
}

static Field *extract_function_params(TreeNode *node, int register_params) {
    TreeNode *var_list;
    Field *head = NULL;
    Field *tail = NULL;

    if (node == NULL || node->child_count == 3) {
        return NULL;
    }

    var_list = child(node, 2);
    while (var_list != NULL && var_list->child_count > 0) {
        TreeNode *param_dec = child(var_list, 0);
        Type *specifier_type = analyze_specifier(child(param_dec, 0));
        char *name = NULL;
        int line = param_dec->line;
        Type *param_type = build_vardec_type(child(param_dec, 1), specifier_type, &name, &line);
        Field *field = new_field(name, param_type, line);

        if (register_params) {
            add_variable_symbol(name, param_type, line);
        }

        if (head == NULL) {
            head = tail = field;
        } else {
            tail->next = field;
            tail = field;
        }

        if (var_list->child_count == 1 || child(var_list, 2) == NULL) {
            break;
        }
        var_list = child(var_list, 2);
    }

    return head;
}

static void analyze_extdef(TreeNode *node) {
    TreeNode *specifier_node;
    TreeNode *second;
    TreeNode *third;
    Type *type;

    if (node == NULL) {
        return;
    }

    specifier_node = child(node, 0);
    second = child(node, 1);
    third = child(node, 2);
    type = analyze_specifier(specifier_node);

    if (second != NULL && is_node(second, "ExtDecList")) {
        analyze_dec_list(second, type, 0, NULL);
        return;
    }

    if (second != NULL && is_node(second, "FunDec")) {
        TreeNode *id = child(second, 0);
        Field *params = extract_function_params(second, third != NULL && is_node(third, "CompSt"));

        declare_or_define_function(id->value, type, params, id->line, third != NULL && is_node(third, "CompSt"));
        if (third != NULL && is_node(third, "CompSt")) {
            analyze_compst(third, type);
        }
        return;
    }
}

static void analyze_extdeflist(TreeNode *node) {
    if (node == NULL || node->child_count == 0) {
        return;
    }
    analyze_extdef(child(node, 0));
    analyze_extdeflist(child(node, 1));
}

static Field *analyze_args(TreeNode *node) {
    Field *head;
    Field *tail;
    ExpResult result;

    if (node == NULL) {
        return NULL;
    }

    if (node->child_count == 1) {
        result = analyze_exp(child(node, 0));
        return new_field(NULL, result.type, node->line);
    }

    result = analyze_exp(child(node, 0));
    head = new_field(NULL, result.type, node->line);
    tail = head;
    while (tail->next != NULL) {
        tail = tail->next;
    }
    tail->next = analyze_args(child(node, 2));
    return head;
}

static void analyze_compst(TreeNode *node, Type *function_type) {
    if (node == NULL) {
        return;
    }
    analyze_deflist(child(node, 1), 0, function_type);
    analyze_stmtlist(child(node, 2), function_type);
}

static void analyze_stmtlist(TreeNode *node, Type *function_type) {
    if (node == NULL || node->child_count == 0) {
        return;
    }

    analyze_stmt(child(node, 0), function_type);
    analyze_stmtlist(child(node, 1), function_type);
}

static void analyze_stmt(TreeNode *node, Type *function_type) {
    TreeNode *first;
    ExpResult result;

    if (node == NULL || node->child_count == 0) {
        return;
    }

    first = child(node, 0);
    if (is_node(first, "Exp")) {
        analyze_exp(first);
        return;
    }

    if (is_node(first, "CompSt")) {
        analyze_compst(first, function_type);
        return;
    }

    if (is_node(first, "RETURN")) {
        result = analyze_exp(child(node, 1));
        if (!type_equal(function_type, result.type)) {
            report_semantic_error(8, first->line, "Type mismatched for return");
        }
        return;
    }

    if (is_node(first, "IF")) {
        result = analyze_exp(child(node, 2));
        if (result.type->kind != TYPE_INVALID && !is_basic_int(result.type)) {
            report_semantic_error(7, child(node, 2)->line, "Type mismatched for operands");
        }
        analyze_stmt(child(node, 4), function_type);
        if (node->child_count == 7) {
            analyze_stmt(child(node, 6), function_type);
        }
        return;
    }

    if (is_node(first, "WHILE")) {
        result = analyze_exp(child(node, 2));
        if (result.type->kind != TYPE_INVALID && !is_basic_int(result.type)) {
            report_semantic_error(7, child(node, 2)->line, "Type mismatched for operands");
        }
        analyze_stmt(child(node, 4), function_type);
    }
}

static ExpResult make_result(Type *type, int is_lvalue) {
    ExpResult result;
    result.type = type;
    result.is_lvalue = is_lvalue;
    return result;
}

static ExpResult analyze_exp(TreeNode *node) {
    TreeNode *first;
    TreeNode *second;
    TreeNode *third;
    ExpResult left;
    ExpResult right;
    VarSymbol *var;
    FunctionSymbol *func;
    StructSymbol *struct_symbol;
    Field *field;
    Field *args;
    char message[256];

    if (node == NULL || node->child_count == 0) {
        return make_result(&invalid_type, 0);
    }

    first = child(node, 0);
    second = child(node, 1);
    third = child(node, 2);

    if (node->child_count == 1) {
        if (is_node(first, "ID")) {
            var = find_var(first->value);
            if (var == NULL) {
                snprintf(message, sizeof(message), "Undefined variable \"%s\"", first->value);
                report_semantic_error(1, first->line, message);
                return make_result(&invalid_type, 0);
            }
            return make_result(var->type, 1);
        }
        if (is_node(first, "INT")) {
            return make_result(&int_type, 0);
        }
        if (is_node(first, "FLOAT")) {
            return make_result(&float_type, 0);
        }
    }

    if (node->child_count == 2) {
        right = analyze_exp(second);
        if (right.type->kind == TYPE_INVALID) {
            return make_result(&invalid_type, 0);
        }
        if (is_node(first, "MINUS")) {
            if (!is_basic_numeric(right.type)) {
                report_semantic_error(7, first->line, "Type mismatched for operands");
                return make_result(&invalid_type, 0);
            }
            return make_result(right.type, 0);
        }
        if (is_node(first, "NOT")) {
            if (!is_basic_int(right.type)) {
                report_semantic_error(7, first->line, "Type mismatched for operands");
                return make_result(&invalid_type, 0);
            }
            return make_result(&int_type, 0);
        }
    }

    if (is_node(first, "LP")) {
        return make_result(analyze_exp(second).type, 0);
    }

    if (node->child_count == 3 && is_node(first, "ID") && is_node(second, "LP")) {
        func = find_function(first->value);
        var = find_var(first->value);
        struct_symbol = find_struct(first->value);

        if (func == NULL) {
            if (var != NULL || struct_symbol != NULL) {
                snprintf(message, sizeof(message), "\"%s\" is not a function", first->value);
                report_semantic_error(11, first->line, message);
            } else {
                snprintf(message, sizeof(message), "Undefined function \"%s\"", first->value);
                report_semantic_error(2, first->line, message);
            }
            return make_result(&invalid_type, 0);
        }

        if (func->params != NULL) {
            report_semantic_error(9, first->line, "Function is not applicable for arguments");
        }
        return make_result(func->return_type, 0);
    }

    if (node->child_count == 4 && is_node(first, "ID") && is_node(second, "LP")) {
        func = find_function(first->value);
        var = find_var(first->value);
        struct_symbol = find_struct(first->value);

        if (func == NULL) {
            if (var != NULL || struct_symbol != NULL) {
                snprintf(message, sizeof(message), "\"%s\" is not a function", first->value);
                report_semantic_error(11, first->line, message);
            } else {
                snprintf(message, sizeof(message), "Undefined function \"%s\"", first->value);
                report_semantic_error(2, first->line, message);
            }
            return make_result(&invalid_type, 0);
        }

        args = analyze_args(third);
        if (!field_list_equal(func->params, args)) {
            report_semantic_error(9, first->line, "Function is not applicable for arguments");
        }
        return make_result(func->return_type, 0);
    }

    if (node->child_count == 4 && is_node(second, "LB")) {
        left = analyze_exp(first);
        right = analyze_exp(third);
        if (left.type->kind == TYPE_INVALID || right.type->kind == TYPE_INVALID) {
            return make_result(&invalid_type, 0);
        }
        if (left.type->kind != TYPE_ARRAY && left.type->kind != TYPE_INVALID) {
            if (is_node(first, "ID") && child(first, 0) != NULL) {
                snprintf(message, sizeof(message), "\"%s\" is not an array", child(first, 0)->value);
            } else {
                snprintf(message, sizeof(message), "Illegal use of \"[]\"");
            }
            report_semantic_error(10, node->line, message);
            return make_result(&invalid_type, 0);
        }
        if (!is_basic_int(right.type)) {
            report_semantic_error(12, third->line, "Array index is not an integer");
            return make_result(left.type->kind == TYPE_ARRAY ? left.type->u.array.elem : &invalid_type, 1);
        }
        if (left.type->kind == TYPE_ARRAY) {
            return make_result(left.type->u.array.elem, 1);
        }
        return make_result(&invalid_type, 1);
    }

    if (node->child_count == 3 && is_node(second, "DOT")) {
        left = analyze_exp(first);
        if (left.type->kind == TYPE_INVALID) {
            return make_result(&invalid_type, 0);
        }
        if (left.type->kind != TYPE_STRUCT && left.type->kind != TYPE_INVALID) {
            report_semantic_error(13, first->line, "Illegal use of \".\"");
            return make_result(&invalid_type, 0);
        }
        if (left.type->kind != TYPE_STRUCT) {
            return make_result(&invalid_type, 0);
        }
        field = find_field(left.type->u.structure.fields, third->value);
        if (field == NULL) {
            snprintf(message, sizeof(message), "Non-existent field \"%s\"", third->value);
            report_semantic_error(14, third->line, message);
            return make_result(&invalid_type, 0);
        }
        return make_result(field->type, 1);
    }

    if (node->child_count == 3) {
        left = analyze_exp(first);
        right = analyze_exp(third);
        if (left.type->kind == TYPE_INVALID || right.type->kind == TYPE_INVALID) {
            return make_result(&invalid_type, 0);
        }

        if (is_node(second, "ASSIGNOP")) {
            if (!left.is_lvalue) {
                report_semantic_error(6, second->line, "The left-hand side of an assignment must be a variable");
                return make_result(&invalid_type, 0);
            }
            if (!type_equal(left.type, right.type)) {
                report_semantic_error(5, second->line, "Type mismatched for assignment");
            }
            return make_result(left.type, 0);
        }

        if (is_node(second, "RELOP")) {
            if (!is_basic_numeric(left.type) || !type_equal(left.type, right.type)) {
                report_semantic_error(7, second->line, "Type mismatched for operands");
                return make_result(&invalid_type, 0);
            }
            return make_result(&int_type, 0);
        }

        if (is_node(second, "AND") || is_node(second, "OR")) {
            if (!is_basic_int(left.type) || !is_basic_int(right.type)) {
                report_semantic_error(7, second->line, "Type mismatched for operands");
                return make_result(&invalid_type, 0);
            }
            return make_result(&int_type, 0);
        }

        if (is_node(second, "PLUS") || is_node(second, "MINUS") ||
            is_node(second, "STAR") || is_node(second, "DIV")) {
            if (!is_basic_numeric(left.type) || !type_equal(left.type, right.type)) {
                report_semantic_error(7, second->line, "Type mismatched for operands");
                return make_result(&invalid_type, 0);
            }
            return make_result(left.type, 0);
        }
    }

    return make_result(&invalid_type, 0);
}

static void check_pending_function_definitions(void) {
    FunctionSymbol *func = function_symbols;
    char message[256];

    while (func != NULL) {
        if (!func->is_defined) {
            snprintf(message, sizeof(message), "Undefined function \"%s\"", func->name);
            report_semantic_error(18, func->declared_line, message);
        }
        func = func->next;
    }
}

static void free_field_list(Field *field) {
    Field *next;
    while (field != NULL) {
        next = field->next;
        free(field->name);
        free(field);
        field = next;
    }
}
