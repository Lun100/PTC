%{
#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>

#ifndef TREE_NODE_DECLARED
#define TREE_NODE_DECLARED
typedef struct TreeNode {
    char *name;
    char *value;
    int line;
    int is_token;
    int child_count;
    struct TreeNode **children;
} TreeNode;
#endif

TreeNode *syntax_tree_root = NULL;
int syntax_error_count = 0;
static int last_syntax_error_line = -1;

extern int yylineno;
extern int yylex(void);
extern int suppress_eof_syntax_error;
extern int last_lexical_error_line;

TreeNode *create_nonterminal(const char *name, int line, int child_count, ...);
TreeNode *create_token_node(const char *name, const char *value, int line);
void print_tree(TreeNode *root, int depth);
void free_tree(TreeNode *root);
void report_syntax_error(const char *msg, int line);
void yyerror(const char *msg);
int is_anonymous_struct_specifier(TreeNode *node);
%}

%union {
    TreeNode *node;
}

%token <node> ID TYPE INT FLOAT
%token <node> SEMI COMMA ASSIGNOP RELOP PLUS MINUS STAR DIV
%token <node> AND OR DOT NOT COMPASSIGN
%token <node> LP RP LB RB LC RC
%token <node> STRUCT RETURN IF ELSE WHILE

%type <node> Program ExtDefList ExtDef ExtDecList Specifier StructSpecifier OptTag Tag
%type <node> VarDec FunDec VarList ParamDec CompSt StmtList Stmt DefList Def DecList Dec
%type <node> Exp Args

%nonassoc LOWER_THAN_ELSE
%nonassoc ELSE
%right ASSIGNOP
%left OR
%left AND
%left RELOP
%left PLUS MINUS
%left STAR DIV
%right NOT
%right UMINUS
%left LP RP LB RB DOT

%%

Program
    : ExtDefList { $$ = create_nonterminal("Program", $1 ? $1->line : yylineno, 1, $1); syntax_tree_root = $$; }
    ;

ExtDefList
    : ExtDef ExtDefList { $$ = create_nonterminal("ExtDefList", $1 ? $1->line : 0, 2, $1, $2); }
    | /* empty */ { $$ = NULL; }
    ;

ExtDef
    : Specifier ExtDecList SEMI { $$ = create_nonterminal("ExtDef", $1->line, 3, $1, $2, $3); }
    | Specifier VarDec ASSIGNOP Exp SEMI {
        report_syntax_error("Global variable initialization not allowed", $3->line);
        $$ = create_nonterminal("ExtDef", $1->line, 5, $1, $2, $3, $4, $5);
      }
    | Specifier FunDec SEMI { $$ = create_nonterminal("ExtDef", $1->line, 3, $1, $2, $3); }
    | Specifier SEMI { $$ = create_nonterminal("ExtDef", $1->line, 2, $1, $2); }
    | Specifier FunDec CompSt { $$ = create_nonterminal("ExtDef", $1->line, 3, $1, $2, $3); }
    ;

ExtDecList
    : VarDec { $$ = create_nonterminal("ExtDecList", $1->line, 1, $1); }
    | VarDec COMMA ExtDecList { $$ = create_nonterminal("ExtDecList", $1->line, 3, $1, $2, $3); }
    ;

Specifier
    : TYPE { $$ = create_nonterminal("Specifier", $1->line, 1, $1); }
    | StructSpecifier { $$ = create_nonterminal("Specifier", $1->line, 1, $1); }
    ;

StructSpecifier
    : STRUCT OptTag LC DefList RC {
        $$ = create_nonterminal("StructSpecifier", $1->line, 5, $1, $2, $3, $4, $5);
      }
    | STRUCT Tag {
        $$ = create_nonterminal("StructSpecifier", $1->line, 2, $1, $2);
      }
    ;

OptTag
    : ID { $$ = create_nonterminal("OptTag", $1->line, 1, $1); }
    | /* empty */ { $$ = NULL; }
    ;

Tag
    : ID { $$ = create_nonterminal("Tag", $1->line, 1, $1); }
    ;

VarDec
    : ID { $$ = create_nonterminal("VarDec", $1->line, 1, $1); }
    | VarDec LB INT RB { $$ = create_nonterminal("VarDec", $1->line, 4, $1, $2, $3, $4); }
    | VarDec LB FLOAT RB {
        report_syntax_error("Array dimension must be integer", $3->line);
        $$ = create_nonterminal("VarDec", $1->line, 4, $1, $2, $3, $4);
      }
    | VarDec LB INT RB RB {
        report_syntax_error("Extra closing bracket in array declaration", $5->line);
        $$ = create_nonterminal("VarDec", $1->line, 5, $1, $2, $3, $4, $5);
      }
    ;

FunDec
    : ID LP VarList RP { $$ = create_nonterminal("FunDec", $1->line, 4, $1, $2, $3, $4); }
    | ID LP RP { $$ = create_nonterminal("FunDec", $1->line, 3, $1, $2, $3); }
    ;

ParamDec
    : Specifier VarDec { $$ = create_nonterminal("ParamDec", $1->line, 2, $1, $2); }
    ;

CompSt
    : LC DefList StmtList RC { $$ = create_nonterminal("CompSt", $1->line, 4, $1, $2, $3, $4); }
    ;

Stmt
    : Exp SEMI { $$ = create_nonterminal("Stmt", $1->line, 2, $1, $2); }
    | Exp error {
        report_syntax_error("Missing \";\"", yylineno);
        yyerrok;
        $$ = create_nonterminal("Stmt", $1->line, 1, $1);
      }
    | CompSt { $$ = create_nonterminal("Stmt", $1->line, 1, $1); }
    | RETURN Exp SEMI { $$ = create_nonterminal("Stmt", $1->line, 3, $1, $2, $3); }
    | RETURN SEMI {
        report_syntax_error("Missing return value in non-void function", $1->line);
        $$ = create_nonterminal("Stmt", $1->line, 2, $1, $2);
      }
    | IF LP Exp RP Stmt %prec LOWER_THAN_ELSE {
        $$ = create_nonterminal("Stmt", $1->line, 5, $1, $2, $3, $4, $5);
      }
    | IF LP Exp RP Exp ELSE Stmt {
        TreeNode *then_stmt = create_nonterminal("Stmt", $5->line, 1, $5);
        report_syntax_error("Missing \";\"", $6->line);
        $$ = create_nonterminal("Stmt", $1->line, 7, $1, $2, $3, $4, then_stmt, $6, $7);
      }
    | IF LP Exp RP Stmt ELSE Stmt {
        $$ = create_nonterminal("Stmt", $1->line, 7, $1, $2, $3, $4, $5, $6, $7);
      }
    | WHILE LP Exp RP Stmt {
        $$ = create_nonterminal("Stmt", $1->line, 5, $1, $2, $3, $4, $5);
      }
    | WHILE LP Exp RP SEMI {
        report_syntax_error("Empty while statement body", $5->line);
        $$ = create_nonterminal("Stmt", $1->line, 5, $1, $2, $3, $4, $5);
      }
    ;

DefList
    : Def DefList { $$ = create_nonterminal("DefList", $1->line, 2, $1, $2); }
    | /* empty */ { $$ = NULL; }
    ;

Def
    : Specifier DecList SEMI { $$ = create_nonterminal("Def", $1->line, 3, $1, $2, $3); }
    | Specifier SEMI {
        if (is_anonymous_struct_specifier($1)) {
            report_syntax_error("Anonymous struct declaration", $2->line);
        }
        $$ = create_nonterminal("Def", $1->line, 2, $1, $2);
      }
    ;

DecList
    : Dec { $$ = create_nonterminal("DecList", $1->line, 1, $1); }
    | Dec COMMA DecList { $$ = create_nonterminal("DecList", $1->line, 3, $1, $2, $3); }
    ;

Dec
    : VarDec { $$ = create_nonterminal("Dec", $1->line, 1, $1); }
    | VarDec ASSIGNOP Exp { $$ = create_nonterminal("Dec", $1->line, 3, $1, $2, $3); }
    | VarDec ASSIGNOP LC Args RC {
        report_syntax_error("Array initialization not supported", $2->line);
        $$ = create_nonterminal("Dec", $1->line, 5, $1, $2, $3, $4, $5);
      }
    ;

Exp
    : Exp ASSIGNOP Exp { $$ = create_nonterminal("Exp", $1->line, 3, $1, $2, $3); }
    | Exp COMPASSIGN Exp {
        report_syntax_error("Unsupported compound assignment operator '+='", $2->line);
        $$ = create_nonterminal("Exp", $1->line, 3, $1, $2, $3);
      }
    | Exp AND Exp { $$ = create_nonterminal("Exp", $1->line, 3, $1, $2, $3); }
    | Exp OR Exp { $$ = create_nonterminal("Exp", $1->line, 3, $1, $2, $3); }
    | Exp RELOP Exp { $$ = create_nonterminal("Exp", $1->line, 3, $1, $2, $3); }
    | Exp PLUS Exp { $$ = create_nonterminal("Exp", $1->line, 3, $1, $2, $3); }
    | Exp MINUS Exp { $$ = create_nonterminal("Exp", $1->line, 3, $1, $2, $3); }
    | Exp STAR Exp { $$ = create_nonterminal("Exp", $1->line, 3, $1, $2, $3); }
    | Exp DIV Exp { $$ = create_nonterminal("Exp", $1->line, 3, $1, $2, $3); }
    | LP Exp RP { $$ = create_nonterminal("Exp", $1->line, 3, $1, $2, $3); }
    | MINUS Exp %prec UMINUS { $$ = create_nonterminal("Exp", $1->line, 2, $1, $2); }
    | NOT Exp { $$ = create_nonterminal("Exp", $1->line, 2, $1, $2); }
    | ID LP Args RP { $$ = create_nonterminal("Exp", $1->line, 4, $1, $2, $3, $4); }
    | ID LP RP { $$ = create_nonterminal("Exp", $1->line, 3, $1, $2, $3); }
    | Exp LB Exp COMMA Exp RB {
        report_syntax_error("Missing \"]\"", $4->line);
        $$ = create_nonterminal("Exp", $1->line, 6, $1, $2, $3, $4, $5, $6);
      }
    | Exp LB Exp RB { $$ = create_nonterminal("Exp", $1->line, 4, $1, $2, $3, $4); }
    | Exp LB Exp error RB {
        report_syntax_error("Missing \"]\"", yylineno);
        yyerrok;
        $$ = create_nonterminal("Exp", $1->line, 4, $1, $2, $3, $5);
      }
    | Exp DOT ID { $$ = create_nonterminal("Exp", $1->line, 3, $1, $2, $3); }
    | Exp DOT {
        report_syntax_error("Missing member name after '.'", $2->line);
        $$ = create_nonterminal("Exp", $1->line, 2, $1, $2);
      }
    | ID { $$ = create_nonterminal("Exp", $1->line, 1, $1); }
    | INT { $$ = create_nonterminal("Exp", $1->line, 1, $1); }
    | FLOAT { $$ = create_nonterminal("Exp", $1->line, 1, $1); }
    ;

Args
    : Exp COMMA Args { $$ = create_nonterminal("Args", $1->line, 3, $1, $2, $3); }
    | Exp COMMA {
        report_syntax_error("Trailing comma in function call arguments", $2->line);
        $$ = create_nonterminal("Args", $1->line, 2, $1, $2);
      }
    | Exp { $$ = create_nonterminal("Args", $1->line, 1, $1); }
    ;

StmtList
    : Stmt StmtList { $$ = create_nonterminal("StmtList", $1 ? $1->line : 0, 2, $1, $2); }
    | Def {
        report_syntax_error("Variable declaration after statements in block", $1->line);
      } StmtList {
        $$ = create_nonterminal("StmtList", $1->line, 2, $1, $3);
      }
    | /* empty */ { $$ = NULL; }
    ;

VarList
    : ParamDec COMMA VarList { $$ = create_nonterminal("VarList", $1->line, 3, $1, $2, $3); }
    | ParamDec COMMA {
        report_syntax_error("Trailing comma in function parameters", $2->line);
        $$ = create_nonterminal("VarList", $1->line, 2, $1, $2);
      }
    | ParamDec { $$ = create_nonterminal("VarList", $1->line, 1, $1); }
    ;

%%

#include "lex.yy.c"

static char *dup_string(const char *src) {
    size_t len = strlen(src) + 1;
    char *dst = (char *)malloc(len);
    if (dst == NULL) {
        fprintf(stderr, "out of memory\n");
        exit(1);
    }
    memcpy(dst, src, len);
    return dst;
}

TreeNode *create_nonterminal(const char *name, int line, int child_count, ...) {
    TreeNode *node = (TreeNode *)calloc(1, sizeof(TreeNode));
    va_list args;
    int i;

    if (node == NULL) {
        fprintf(stderr, "out of memory\n");
        exit(1);
    }

    node->name = dup_string(name);
    node->line = line;
    node->child_count = child_count;

    if (child_count > 0) {
        node->children = (TreeNode **)calloc((size_t)child_count, sizeof(TreeNode *));
        if (node->children == NULL) {
            fprintf(stderr, "out of memory\n");
            exit(1);
        }
    }

    va_start(args, child_count);
    for (i = 0; i < child_count; ++i) {
        node->children[i] = va_arg(args, TreeNode *);
        if (node->line == 0 && node->children[i] != NULL) {
            node->line = node->children[i]->line;
        }
    }
    va_end(args);

    return node;
}

TreeNode *create_token_node(const char *name, const char *value, int line) {
    TreeNode *node = (TreeNode *)calloc(1, sizeof(TreeNode));
    if (node == NULL) {
        fprintf(stderr, "out of memory\n");
        exit(1);
    }

    node->name = dup_string(name);
    node->value = dup_string(value);
    node->line = line;
    node->is_token = 1;
    return node;
}

void print_tree(TreeNode *root, int depth) {
    int i;

    if (root == NULL) {
        return;
    }

    for (i = 0; i < depth; ++i) {
        printf("  ");
    }

    if (root->is_token) {
        if (strcmp(root->name, "ID") == 0 ||
            strcmp(root->name, "TYPE") == 0 ||
            strcmp(root->name, "INT") == 0 ||
            strcmp(root->name, "FLOAT") == 0) {
            printf("%s: %s\n", root->name, root->value);
        } else {
            printf("%s\n", root->name);
        }
    } else {
        printf("%s (%d)\n", root->name, root->line);
        for (i = 0; i < root->child_count; ++i) {
            print_tree(root->children[i], depth + 1);
        }
    }
}

void free_tree(TreeNode *root) {
    int i;

    if (root == NULL) {
        return;
    }

    for (i = 0; i < root->child_count; ++i) {
        free_tree(root->children[i]);
    }

    free(root->children);
    free(root->name);
    free(root->value);
    free(root);
}

void report_syntax_error(const char *msg, int line) {
    if (suppress_eof_syntax_error) {
        suppress_eof_syntax_error = 0;
        return;
    }
    if (line == last_lexical_error_line) {
        return;
    }
    if (line == last_syntax_error_line) {
        return;
    }
    last_syntax_error_line = line;
    ++syntax_error_count;
    printf("Error type B at Line %d: %s.\n", line, msg);
}

void yyerror(const char *msg) {
    report_syntax_error(msg, yylineno);
}

int is_anonymous_struct_specifier(TreeNode *node) {
    TreeNode *struct_specifier;

    if (node == NULL || strcmp(node->name, "Specifier") != 0 || node->child_count == 0) {
        return 0;
    }

    struct_specifier = node->children[0];
    if (struct_specifier == NULL ||
        strcmp(struct_specifier->name, "StructSpecifier") != 0 ||
        struct_specifier->child_count != 5) {
        return 0;
    }

    return struct_specifier->children[1] == NULL;
}
