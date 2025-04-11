/*
Title: Parser Header File for IshanLang
Name: parser.h
Author: Ishan Leung
Language: C23
Description: Defines abstract syntax tree (AST) structures and parsing functions
*/

#pragma once
#include "lexer.h"

typedef enum {
    AST_PUSH_INT, AST_PUSH_STR, AST_PUSH_DBL,
    AST_READ_INT, AST_READ_STR, AST_READ_DBL,
    AST_ADD, AST_SUB, AST_POP, AST_SWAP,
    AST_STORE, AST_LOAD,
    AST_CONV,
    AST_PULL,
    AST_LABEL,
    AST_JMP_EQ0, AST_JMP_GT0, AST_JMP,
    AST_PRINT,
    AST_STR_CONCAT,
    AST_HALT
} InstrType;

typedef struct {
    InstrType type;
    char* str_val;      // For strings, variable names, conversion, label names or jump targets
    int int_val;        // For integer literals
    double dbl_val;     // For double literals
    char* arg1;         // For PULL (source)
    char* arg2;         // For PULL (destination)
} Instruction;

typedef struct {
    Instruction* items;
    int count;
    int capacity;
} AST;

AST* parse_tokens(TokenList* tokens);
void free_ast(AST* ast);
