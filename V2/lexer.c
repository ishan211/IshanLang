#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include "lexer.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include "lexer.h"

static Token make_token(TokenType type, const char* lex) {
    Token t;
    t.type = type;
    t.lexeme = strdup(lex);
    return t;
}

static void add_token(TokenList* list, Token t) {
    if (t.lexeme[0] == '\0') {
        free(t.lexeme);
        return;
    }
    if (list->count >= list->capacity) {
        list->capacity *= 2;
        list->tokens = realloc(list->tokens, sizeof(Token) * list->capacity);
    }
    list->tokens[list->count++] = t;
}

static TokenType match_keyword(const char* word) {
    if (strcmp(word, "PUSH.INT") == 0) return TOK_PUSH_INT;
    if (strcmp(word, "PUSH.STR") == 0) return TOK_PUSH_STR;
    if (strcmp(word, "PUSH.DBL") == 0) return TOK_PUSH_DBL;
    if (strcmp(word, "READ.INT") == 0) return TOK_READ_INT;
    if (strcmp(word, "READ.STR") == 0) return TOK_READ_STR;
    if (strcmp(word, "READ.DBL") == 0) return TOK_READ_DBL;
    if (strcmp(word, "ADD") == 0) return TOK_ADD;
    if (strcmp(word, "SUB") == 0) return TOK_SUB;
    if (strcmp(word, "MUL") == 0) return TOK_MUL;
    if (strcmp(word, "DIV") == 0) return TOK_DIV;
    if (strcmp(word, "EXP") == 0) return TOK_EXP;
    if (strcmp(word, "MOD") == 0) return TOK_MOD;
    if (strcmp(word, "POP") == 0) return TOK_POP;
    if (strcmp(word, "SWAP") == 0) return TOK_SWAP;
    if (strcmp(word, "STORE") == 0) return TOK_STORE;
    if (strcmp(word, "LOAD") == 0) return TOK_LOAD;
    if (strcmp(word, "PULL") == 0) return TOK_PULL;
    if (strcmp(word, "JMP.EQ.0") == 0) return TOK_JMP_EQ0;
    if (strcmp(word, "JMP.GT.0") == 0) return TOK_JMP_GT0;
    if (strcmp(word, "JMP") == 0) return TOK_JMP;
    if (strcmp(word, "PRINT") == 0) return TOK_PRINT;
    if (strcmp(word, "STR.CONCAT") == 0) return TOK_STR_CONCAT;
    return -1;
}

TokenList* tokenize(const char* source) {
    TokenList* list = malloc(sizeof(TokenList));
    list->count = 0;
    list->capacity = 64;
    list->tokens = malloc(sizeof(Token) * list->capacity);

    int len = strlen(source);
    int i = 0;

    while (i < len) {
        while (i < len && isspace(source[i])) i++;
        if (i >= len) break;

        char buffer[256];
        int j = 0;

        // Label handling: [LABEL]
        if (source[i] == '[') {
            i++;
            while (i < len && source[i] != ']') buffer[j++] = source[i++];
            buffer[j] = '\0';
            if (i < len && source[i] == ']') i++;
            add_token(list, make_token(TOK_LABEL_DEF, buffer));
            continue;
        }

        // String handling
        if (source[i] == '"') {
            i++;  // Skip the opening quote
            while (i < len && source[i] != '"') {
                buffer[j++] = source[i++];
            }
            buffer[j] = '\0';
            if (i < len && source[i] == '"') i++;  // Skip closing quote
            add_token(list, make_token(TOK_STR, buffer));
            continue;
        }

        // Regular tokens (no spaces)
        while (i < len && !isspace(source[i])) {
            buffer[j++] = source[i++];
        }
        buffer[j] = '\0';

        if (j == 0) continue;

        TokenType type = match_keyword(buffer);
        if (type != -1) {
            add_token(list, make_token(type, buffer));
        } else if (isdigit(buffer[0]) || (buffer[0] == '-' && isdigit(buffer[1]))) {
            if (strchr(buffer, '.')) {
                add_token(list, make_token(TOK_DBL, buffer));
            } else {
                add_token(list, make_token(TOK_INT, buffer));
            }
        } else {
            add_token(list, make_token(TOK_IDENT, buffer));
        }
    }

    add_token(list, make_token(TOK_EOF, "EOF"));
    return list;
}

void free_tokens(TokenList* list) {
    for (int i = 0; i < list->count; ++i) {
        free(list->tokens[i].lexeme);
    }
    free(list->tokens);
    free(list);
}
