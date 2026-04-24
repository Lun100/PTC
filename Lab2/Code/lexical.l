%option noyywrap nounput noinput yylineno
%x COMMENT

%{
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <limits.h>

#ifndef TREE_NODE_DECLARED
#define TREE_NODE_DECLARED
typedef struct TreeNode TreeNode;
#endif

TreeNode *create_token_node(const char *name, const char *value, int line);

#include "syntax.tab.h"

int lexical_error_count = 0;
int suppress_eof_syntax_error = 0;
int last_lexical_error_line = -1;
static int comment_start_line = 0;

static TreeNode *make_token(const char *name, const char *text) {
    return create_token_node(name, text, yylineno);
}

static TreeNode *make_int_token(const char *text) {
    char buffer[64];
    long value = strtol(text, NULL, 0);
    snprintf(buffer, sizeof(buffer), "%ld", value);
    return create_token_node("INT", buffer, yylineno);
}

static TreeNode *make_float_token(const char *text) {
    char buffer[64];
    float value = strtof(text, NULL);
    snprintf(buffer, sizeof(buffer), "%f", value);
    return create_token_node("FLOAT", buffer, yylineno);
}

static void report_lexical_error(const char *text) {
    ++lexical_error_count;
    last_lexical_error_line = yylineno;
    printf("Error type A at Line %d: Undefined character '%s'.\n", yylineno, text);
}

static void report_unterminated_comment(void) {
    ++lexical_error_count;
    last_lexical_error_line = comment_start_line;
    printf("Error type A at Line %d: Unterminated comment.\n", comment_start_line);
}

static void report_invalid_identifier(const char *text) {
    ++lexical_error_count;
    last_lexical_error_line = yylineno;
    printf("Error type A at Line %d: Invalid identifier starting with digit '%s'.\n", yylineno, text);
}

static void report_invalid_float(const char *text) {
    ++lexical_error_count;
    last_lexical_error_line = yylineno;
    printf("Error type A at Line %d: Invalid floating point literal '%s'.\n", yylineno, text);
}

static void report_nested_comment(void) {
    ++lexical_error_count;
    last_lexical_error_line = yylineno;
    printf("Error type A at Line %d: Nested comment is not allowed.\n", yylineno);
}
%}

DIGIT          [0-9]
LETTER         [A-Za-z_]
ID_BODY        [A-Za-z0-9_]
DEC_INT        0|([1-9]{DIGIT}*)
OCT_INT        0[0-7]+
HEX_INT        0[xX][0-9A-Fa-f]+
FLOAT_BASE     (({DIGIT}+\.{DIGIT}*)|({DIGIT}*\.{DIGIT}+))
EXP_PART       [eE][+-]?{DIGIT}+
FLOAT_NUM      ({DIGIT}+\.{DIGIT}+)|({FLOAT_BASE}{EXP_PART})
INVALID_ID     {DIGIT}+{LETTER}{ID_BODY}*
INVALID_FLOAT  ({DIGIT}+\.{DIGIT}+)(\.{DIGIT}+)+
WS             [ \t\r]+

%%

"int"          { yylval.node = make_token("TYPE", yytext); return TYPE; }
"float"        { yylval.node = make_token("TYPE", yytext); return TYPE; }
"struct"       { yylval.node = make_token("STRUCT", yytext); return STRUCT; }
"return"       { yylval.node = make_token("RETURN", yytext); return RETURN; }
"if"           { yylval.node = make_token("IF", yytext); return IF; }
"else"         { yylval.node = make_token("ELSE", yytext); return ELSE; }
"while"        { yylval.node = make_token("WHILE", yytext); return WHILE; }

"&&"           { yylval.node = make_token("AND", yytext); return AND; }
"||"           { yylval.node = make_token("OR", yytext); return OR; }
"+="           { yylval.node = make_token("COMPASSIGN", yytext); return COMPASSIGN; }
"!"            { yylval.node = make_token("NOT", yytext); return NOT; }
"=="           { yylval.node = make_token("RELOP", yytext); return RELOP; }
"!="           { yylval.node = make_token("RELOP", yytext); return RELOP; }
">="           { yylval.node = make_token("RELOP", yytext); return RELOP; }
"<="           { yylval.node = make_token("RELOP", yytext); return RELOP; }
">"            { yylval.node = make_token("RELOP", yytext); return RELOP; }
"<"            { yylval.node = make_token("RELOP", yytext); return RELOP; }
"="            { yylval.node = make_token("ASSIGNOP", yytext); return ASSIGNOP; }
"+"            { yylval.node = make_token("PLUS", yytext); return PLUS; }
"-"            { yylval.node = make_token("MINUS", yytext); return MINUS; }
"*"            { yylval.node = make_token("STAR", yytext); return STAR; }
"/"            { yylval.node = make_token("DIV", yytext); return DIV; }
"."            { yylval.node = make_token("DOT", yytext); return DOT; }
";"            { yylval.node = make_token("SEMI", yytext); return SEMI; }
","            { yylval.node = make_token("COMMA", yytext); return COMMA; }
"("            { yylval.node = make_token("LP", yytext); return LP; }
")"            { yylval.node = make_token("RP", yytext); return RP; }
"["            { yylval.node = make_token("LB", yytext); return LB; }
"]"            { yylval.node = make_token("RB", yytext); return RB; }
"{"            { yylval.node = make_token("LC", yytext); return LC; }
"}"            { yylval.node = make_token("RC", yytext); return RC; }

{INVALID_FLOAT} {
                  report_invalid_float(yytext);
                  yylval.node = create_token_node("FLOAT", "0.000000", yylineno);
                  return FLOAT;
                }
{FLOAT_NUM}    { yylval.node = make_float_token(yytext); return FLOAT; }
{HEX_INT}      { yylval.node = make_int_token(yytext); return INT; }
{OCT_INT}      { yylval.node = make_int_token(yytext); return INT; }
{DEC_INT}      { yylval.node = make_int_token(yytext); return INT; }
{INVALID_ID}   {
                  report_invalid_identifier(yytext);
                  yylval.node = make_token("ID", yytext);
                  return ID;
                }
{LETTER}{ID_BODY}* {
                  yylval.node = make_token("ID", yytext);
                  return ID;
                }

"//".*         { }
"/*"           { comment_start_line = yylineno; BEGIN(COMMENT); }
<COMMENT>"*/"  { BEGIN(INITIAL); }
<COMMENT>"/*"  { report_nested_comment(); }
<COMMENT>\n    { }
<COMMENT>.     { }
<COMMENT><<EOF>> {
                  BEGIN(INITIAL);
                  suppress_eof_syntax_error = 1;
                  report_unterminated_comment();
                  yyterminate();
                }
\n             { }
{WS}           { }

.              { report_lexical_error(yytext); }

%%
