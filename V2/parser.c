/*
Title: Parser Implementation
Name: parser.c
Author: Ishan Leung
Language: C23
Description: Implements parsing functions to convert tokens into an abstract syntax tree
*/

#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include "parser.h"

static AST* new_ast() {
    AST* ast = malloc(sizeof(AST));
    ast->count = 0;
    ast->capacity = 64;
    ast->items = malloc(sizeof(Instruction) * ast->capacity);
    return ast;
}

static void add_instr(AST* ast, Instruction instr) {
    if (ast->count >= ast->capacity) {
        ast->capacity *= 2;
        ast->items = realloc(ast->items, sizeof(Instruction) * ast->capacity);
    }
    ast->items[ast->count++] = instr;
}

static Token* peek(TokenList* tokens, int* i) {
    return &tokens->tokens[*i];
}

static Token* next(TokenList* tokens, int* i) {
    return &tokens->tokens[(*i)++];
}

AST* parse_tokens(TokenList* tokens) {
    AST* ast = new_ast();
    int i = 0;
    while (tokens->tokens[i].type != TOK_EOF) {
        Token* tok = next(tokens, &i);
        Instruction instr = {0};
        switch (tok->type) {
            case TOK_PUSH_INT:
                instr.type = AST_PUSH_INT;
                instr.int_val = atoi(next(tokens, &i)->lexeme);
                break;
            case TOK_PUSH_STR:
                instr.type = AST_PUSH_STR;
                instr.str_val = strdup(next(tokens, &i)->lexeme);
                break;
            case TOK_PUSH_DBL:
                instr.type = AST_PUSH_DBL;
                instr.dbl_val = atof(next(tokens, &i)->lexeme);
                break;
            case TOK_READ_INT:  instr.type = AST_READ_INT; break;
            case TOK_READ_STR:  instr.type = AST_READ_STR; break;
            case TOK_READ_DBL:  instr.type = AST_READ_DBL; break;
            case TOK_ADD:       instr.type = AST_ADD; break;
            case TOK_SUB:       instr.type = AST_SUB; break;
            case TOK_POP:       instr.type = AST_POP; break;
            case TOK_SWAP:      instr.type = AST_SWAP; break;
            case TOK_STORE:
                instr.type = AST_STORE;
                instr.str_val = strdup(next(tokens, &i)->lexeme);
                break;
            case TOK_LOAD:
                instr.type = AST_LOAD;
                instr.str_val = strdup(next(tokens, &i)->lexeme);
                break;
            case TOK_CONV:
                instr.type = AST_CONV;
                instr.str_val = strdup(tok->lexeme);
                break;
            case TOK_PULL:
                instr.type = AST_PULL;
                instr.arg1 = strdup(next(tokens, &i)->lexeme); // source
                instr.arg2 = strdup(next(tokens, &i)->lexeme); // destination
                break;
            case TOK_LABEL_DEF:
                instr.type = AST_LABEL;
                instr.str_val = strdup(tok->lexeme);
                break;
            case TOK_JMP_EQ0:
                instr.type = AST_JMP_EQ0;
                instr.str_val = strdup(next(tokens, &i)->lexeme);
                break;
            case TOK_JMP_GT0:
                instr.type = AST_JMP_GT0;
                instr.str_val = strdup(next(tokens, &i)->lexeme);
                break;
            case TOK_JMP:
                instr.type = AST_JMP;
                instr.str_val = strdup(next(tokens, &i)->lexeme);
                break;
            case TOK_PRINT:
                instr.type = AST_PRINT;
                break;
            case TOK_STR_CONCAT:
                instr.type = AST_STR_CONCAT;
                break;
            case TOK_INT:
            case TOK_DBL:
            case TOK_STR:
            case TOK_IDENT:
                // If token doesn't match any keyword, report error.
                printf("Unexpected token: %s\n", tok->lexeme);
                exit(1);
                break;
            default:
                printf("Unexpected token: %s\n", tok->lexeme);
                exit(1);
        }
        add_instr(ast, instr);
    }
    return ast;
}

void free_ast(AST* ast) {
    for (int i = 0; i < ast->count; ++i) {
        Instruction* in = &ast->items[i];
        if (in->str_val) free(in->str_val);
        if (in->arg1) free(in->arg1);
        if (in->arg2) free(in->arg2);
    }
    free(ast->items);
    free(ast);
}
