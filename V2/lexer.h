/*
Title: Header file for Lexer
Name: lexer.h
Author: Ishan Leung
Language: C23
Description: Defines token types, structures, and lexical analysis functions
*/

#pragma once

typedef enum {
    TOK_PUSH_INT, TOK_PUSH_STR, TOK_PUSH_DBL,
    TOK_READ_INT, TOK_READ_STR, TOK_READ_DBL,
    TOK_ADD, TOK_SUB, TOK_POP, TOK_SWAP,
    TOK_MUL, TOK_DIV, TOK_EXP, TOK_MOD,
    TOK_STORE, TOK_LOAD, TOK_PULL,
    TOK_CONV,
    TOK_PRINT,
    TOK_STR_CONCAT,
    TOK_LABEL_DEF,
    TOK_JMP_EQ0, TOK_JMP_GT0, TOK_JMP,
    TOK_INT, TOK_DBL, TOK_STR, TOK_IDENT,
    TOK_EOF
} TokenType;

typedef struct {
    TokenType type;
    char* lexeme;   // The string contents of the token
} Token;

typedef struct {
    Token* tokens;
    int count;
    int capacity;
} TokenList;

TokenList* tokenize(const char* source);
void free_tokens(TokenList* list);
