#include <stdio.h>
#include <stdlib.h>

#include "semantic.h"

extern FILE *yyin;
extern int yyparse(void);
extern void yyrestart(FILE *input_file);

extern TreeNode *syntax_tree_root;
extern int lexical_error_count;
extern int syntax_error_count;

void free_tree(TreeNode *root);

int main(int argc, char **argv) {
    FILE *input = NULL;

    if (argc != 2) {
        fprintf(stderr, "Usage: %s <source-file>\n", argv[0]);
        return 1;
    }

    input = fopen(argv[1], "r");
    if (input == NULL) {
        perror(argv[1]);
        return 1;
    }

    yyin = input;
    yyrestart(input);
    yyparse();
    fclose(input);

    if (lexical_error_count == 0 && syntax_error_count == 0) {
        semantic_analyze(syntax_tree_root);
    }

    free_tree(syntax_tree_root);
    return 0;
}
