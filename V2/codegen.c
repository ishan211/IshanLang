/*
Title: Bytecode Generator Implementation
Name: codegen.c
Author: Ishan Leung
Language: C23
Description: Implements bytecode generation from abstract syntax tree to executable format
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#include "codegen.h"
#include "bytecode.h"

// Label resolution structures
typedef struct {
    char* name;
    int offset;
} Label;

typedef struct {
    Label* items;
    int count;
    int capacity;
} LabelTable;

static void add_label(LabelTable* table, const char* name, int offset) {
    if (table->count >= table->capacity) {
        table->capacity *= 2;
        table->items = realloc(table->items, sizeof(Label) * table->capacity);
    }
    table->items[table->count++] = (Label){ strdup(name), offset };
}

static int lookup_label(LabelTable* table, const char* name) {
    for (int i = 0; i < table->count; ++i) {
        if (strcmp(table->items[i].name, name) == 0)
            return table->items[i].offset;
    }
    fprintf(stderr, "Undefined label: %s\n", name);
    exit(1);
}

static void write_string(FILE* f, const char* s) {
    uint32_t len = (uint32_t)strlen(s);
    fwrite(&len, sizeof(uint32_t), 1, f);
    fwrite(s, 1, len, f);
}

void generate_bytecode(AST* ast, const char* out_file) {
    FILE* f = fopen(out_file, "wb");
    if (!f) { perror("Unable to open bytecode output"); exit(1); }

    LabelTable labels = { malloc(sizeof(Label) * 16), 0, 16 };
    int offset = 0;

    // First pass: record label offsets
    for (int i = 0; i < ast->count; i++) {
        Instruction* in = &ast->items[i];
        if (in->type == AST_LABEL) {
            add_label(&labels, in->str_val, offset);
            continue;
        }
        offset += 1;
        if (in->type == AST_PUSH_INT) offset += sizeof(int);
        else if (in->type == AST_PUSH_DBL) offset += sizeof(double);
        else if (in->type == AST_PUSH_STR || in->type == AST_STORE || in->type == AST_LOAD)
            offset += sizeof(uint32_t) + (int)strlen(in->str_val);
        else if (in->type == AST_CONV) offset += 1;
        else if (in->type == AST_PULL)
            offset += sizeof(uint32_t) + strlen(in->arg1) + sizeof(uint32_t) + strlen(in->arg2);
        else if (in->type == AST_JMP_EQ0 || in->type == AST_JMP_GT0 || in->type == AST_JMP)
            offset += sizeof(int);
    }

    // Second pass: write bytecode
    for (int i = 0; i < ast->count; i++) {
        Instruction* in = &ast->items[i];
        switch (in->type) {
            case AST_PUSH_INT:
                fputc(OP_PUSH_INT, f);
                fwrite(&in->int_val, sizeof(int), 1, f);
                break;
            case AST_PUSH_STR:
                fputc(OP_PUSH_STR, f);
                write_string(f, in->str_val);
                break;
            case AST_PUSH_DBL:
                fputc(OP_PUSH_DBL, f);
                fwrite(&in->dbl_val, sizeof(double), 1, f);
                break;
            case AST_READ_INT:  fputc(OP_READ_INT, f); break;
            case AST_READ_STR:  fputc(OP_READ_STR, f); break;
            case AST_READ_DBL:  fputc(OP_READ_DBL, f); break;
            case AST_ADD:       fputc(OP_ADD, f); break;
            case AST_SUB:       fputc(OP_SUB, f); break;
            case AST_MUL:       fputc(OP_MUL, f); break;
            case AST_DIV:       fputc(OP_DIV, f); break;
            case AST_EXP:       fputc(OP_EXP, f); break;
            case AST_MOD:       fputc(OP_MOD, f); break;
            case AST_POP:       fputc(OP_POP, f); break;
            case AST_SWAP:      fputc(OP_SWAP, f); break;
            case AST_CONV:
                fputc(OP_CONV, f);
                if      (strcmp(in->str_val, "INT|STR") == 0) fputc(CONV_INT_TO_STR, f);
                else if (strcmp(in->str_val, "INT|DBL") == 0) fputc(CONV_INT_TO_DBL, f);
                else if (strcmp(in->str_val, "STR|INT") == 0) fputc(CONV_STR_TO_INT, f);
                else if (strcmp(in->str_val, "STR|DBL") == 0) fputc(CONV_STR_TO_DBL, f);
                else if (strcmp(in->str_val, "DBL|INT") == 0) fputc(CONV_DBL_TO_INT, f);
                else if (strcmp(in->str_val, "DBL|STR") == 0) fputc(CONV_DBL_TO_STR, f);
                else { fprintf(stderr, "Invalid conversion: %s\n", in->str_val); exit(1); }
                break;
            case AST_STORE:
                fputc(OP_STORE, f);
                write_string(f, in->str_val);
                break;
            case AST_LOAD:
                fputc(OP_LOAD, f);
                write_string(f, in->str_val);
                break;
            case AST_PULL:
                fputc(OP_PULL, f);
                write_string(f, in->arg1);
                write_string(f, in->arg2);
                break;
            case AST_JMP_EQ0: {
                fputc(OP_JMP_EQ0, f);
                int dest = lookup_label(&labels, in->str_val);
                fwrite(&dest, sizeof(int), 1, f);
                break;
            }
            case AST_JMP_GT0: {
                fputc(OP_JMP_GT0, f);
                int dest = lookup_label(&labels, in->str_val);
                fwrite(&dest, sizeof(int), 1, f);
                break;
            }
            case AST_JMP: {
                fputc(OP_JMP, f);
                int dest = lookup_label(&labels, in->str_val);
                fwrite(&dest, sizeof(int), 1, f);
                break;
            }
            case AST_PRINT:       fputc(OP_PRINT, f); break;
            case AST_STR_CONCAT:  fputc(OP_STR_CONCAT, f); break;
            case AST_HALT:        break; // HALT written at end
            case AST_LABEL:       break; // Labels not emitted
            default:
                fprintf(stderr, "Unhandled AST instruction in codegen\n");
                exit(1);
        }
    }

    fputc(OP_HALT, f);

    fclose(f);
    for (int i = 0; i < labels.count; i++) {
        free(labels.items[i].name);
    }
    free(labels.items);
}
