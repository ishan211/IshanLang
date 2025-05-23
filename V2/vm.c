#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#include <math.h>
#include "bytecode.h"

#define STACK_MAX 1024

typedef enum { VAL_INT, VAL_DBL, VAL_STR } ValueType;

typedef struct {
    ValueType type;
    union {
        int i;
        double d;
        char* s;
    };
} Value;

Value stack1[STACK_MAX];
int sp1 = 0;
Value stack2[STACK_MAX];
int sp2 = 0;

typedef struct {
    char* name;
    Value val;
} Variable;
Variable var_table[256];
int var_count = 0;

static void push(Value v, int stack_id) {
    if (stack_id == 1) {
        if (sp1 >= STACK_MAX) { fprintf(stderr, "Stack1 overflow\n"); exit(1); }
        stack1[sp1++] = v;
    } else if (stack_id == 2) {
        if (sp2 >= STACK_MAX) { fprintf(stderr, "Stack2 overflow\n"); exit(1); }
        stack2[sp2++] = v;
    }
}

static Value pop(int stack_id) {
    if (stack_id == 1) {
        if (sp1 <= 0) { fprintf(stderr, "Stack1 underflow\n"); exit(1); }
        return stack1[--sp1];
    } else if (stack_id == 2) {
        if (sp2 <= 0) { fprintf(stderr, "Stack2 underflow\n"); exit(1); }
        return stack2[--sp2];
    }
    fprintf(stderr, "Invalid stack id\n");
    exit(1);
}

static void swap() {
    if (sp1 < 2) { fprintf(stderr, "Not enough values to swap\n"); exit(1); }
    Value a = pop(1);
    Value b = pop(1);
    push(a, 1);
    push(b, 1);
}

static void store_var(const char* name, Value v) {
    for (int i = 0; i < var_count; i++) {
        if (strcmp(var_table[i].name, name) == 0) {
            var_table[i].val = v;
            return;
        }
    }
    var_table[var_count++] = (Variable){ strdup(name), v };
}

static Value load_var(const char* name) {
    for (int i = 0; i < var_count; i++) {
        if (strcmp(var_table[i].name, name) == 0)
            return var_table[i].val;
    }
    fprintf(stderr, "Undefined variable: %s\n", name);
    exit(1);
}

void run_vm(const char* file) {
    FILE* f = fopen(file, "rb");
    if (!f) { perror("Open bytecode in VM"); exit(1); }

    fseek(f, 0, SEEK_END);
    int size = ftell(f);
    fseek(f, 0, SEEK_SET);
    uint8_t* code = malloc(size);
    fread(code, 1, size, f);
    fclose(f);

    int pc = 0;
    while (pc < size) {
        uint8_t op = code[pc++];
        switch (op) {
            case OP_PUSH_INT: {
                int val = *(int*)(code + pc); pc += 4;
                push((Value){ VAL_INT, .i = val }, 1);
                break;
            }
            case OP_PUSH_STR: {
                uint32_t len = *(uint32_t*)(code + pc); pc += 4;
                char* s = strndup((char*)(code + pc), len); pc += len;
                push((Value){ VAL_STR, .s = s }, 1);
                break;
            }
            case OP_PUSH_DBL: {
                double d = *(double*)(code + pc); pc += 8;
                push((Value){ VAL_DBL, .d = d }, 1);
                break;
            }
            case OP_READ_INT: {
                int x;
                scanf("%d", &x);
                push((Value){ VAL_INT, .i = x }, 1);
                break;
            }
            case OP_READ_STR: {
                char buf[256];
                scanf("%255s", buf);
                push((Value){ VAL_STR, .s = strdup(buf) }, 1);
                break;
            }
            case OP_READ_DBL: {
                double d;
                scanf("%lf", &d);
                push((Value){ VAL_DBL, .d = d }, 1);
                break;
            }
            case OP_CONV: {
                uint8_t subtype = code[pc++];
                Value v = pop(1);
                Value res;
                switch (subtype) {
                    case CONV_INT_TO_STR: {
                        char buf[32]; snprintf(buf, sizeof(buf), "%d", v.i);
                        res = (Value){ VAL_STR, .s = strdup(buf) };
                        break;
                    }
                    case CONV_INT_TO_DBL:
                        res = (Value){ VAL_DBL, .d = (double)v.i };
                        break;
                    case CONV_STR_TO_INT:
                        res = (Value){ VAL_INT, .i = atoi(v.s) };
                        break;
                    case CONV_STR_TO_DBL:
                        res = (Value){ VAL_DBL, .d = atof(v.s) };
                        break;
                    case CONV_DBL_TO_INT:
                        res = (Value){ VAL_INT, .i = (int)v.d };
                        break;
                    case CONV_DBL_TO_STR: {
                        char buf[64]; snprintf(buf, sizeof(buf), "%.15g", v.d);
                        res = (Value){ VAL_STR, .s = strdup(buf) };
                        break;
                    }
                    default:
                        fprintf(stderr, "Unknown conversion subtype\n");
                        exit(1);
                }
                push(res, 1);
                break;
            }
            case OP_ADD: {
                Value b = pop(1), a = pop(1);
                if (a.type != b.type || (a.type != VAL_INT && a.type != VAL_DBL)) {
                    fprintf(stderr, "Type mismatch in ADD\n"); exit(1);
                }
                if (a.type == VAL_INT) push((Value){ VAL_INT, .i = a.i + b.i }, 1);
                else push((Value){ VAL_DBL, .d = a.d + b.d }, 1);
                break;
            }
            case OP_SUB: {
                Value b = pop(1), a = pop(1);
                if (a.type != b.type || (a.type != VAL_INT && a.type != VAL_DBL)) {
                    fprintf(stderr, "Type mismatch in SUB\n"); exit(1);
                }
                if (a.type == VAL_INT) push((Value){ VAL_INT, .i = a.i - b.i }, 1);
                else push((Value){ VAL_DBL, .d = a.d - b.d }, 1);
                break;
            }
            case OP_MUL: {
                Value b = pop(1), a = pop(1);
                if (a.type != b.type || (a.type != VAL_INT && a.type != VAL_DBL)) {
                    fprintf(stderr, "Type mismatch in MUL\n"); exit(1);
                }
                if (a.type == VAL_INT) push((Value){ VAL_INT, .i = a.i * b.i }, 1);
                else push((Value){ VAL_DBL, .d = a.d * b.d }, 1);
                break;
            }
            case OP_DIV: {
                Value b = pop(1), a = pop(1);
                if (a.type != b.type || (a.type != VAL_INT && a.type != VAL_DBL)) {
                    fprintf(stderr, "Type mismatch in DIV\n"); exit(1);
                }
                if ((b.type == VAL_INT && b.i == 0) || (b.type == VAL_DBL && b.d == 0.0)) {
                    fprintf(stderr, "Division by zero\n"); exit(1);
                }
                if (a.type == VAL_INT) push((Value){ VAL_INT, .i = a.i / b.i }, 1);
                else push((Value){ VAL_DBL, .d = a.d / b.d }, 1);
                break;
            }
            case OP_EXP: {
                Value b = pop(1), a = pop(1);
                if (a.type != b.type || (a.type != VAL_INT && a.type != VAL_DBL)) {
                    fprintf(stderr, "Type mismatch in EXP\n"); exit(1);
                }
                if (a.type == VAL_INT) {
                    int result = 1;
                    for (int i = 0; i < b.i; i++) result *= a.i;
                    push((Value){ VAL_INT, .i = result }, 1);
                } else {
                    push((Value){ VAL_DBL, .d = pow(a.d, b.d) }, 1);
                }
                break;
            }
            case OP_MOD: {
                Value b = pop(1), a = pop(1);
                if (a.type != b.type || (a.type != VAL_INT && a.type != VAL_DBL)) {
                    fprintf(stderr, "Type mismatch in MOD\n"); exit(1);
                }
                if (a.type == VAL_INT) push((Value){ VAL_INT, .i = a.i % b.i }, 1);
                else push((Value){ VAL_DBL, .d = fmod(a.d, b.d) }, 1);
                break;
            }
            case OP_POP: pop(1); break;
            case OP_SWAP: swap(); break;
            case OP_STORE: {
                uint32_t len = *(uint32_t*)(code + pc); pc += 4;
                char* name = strndup((char*)(code + pc), len); pc += len;
                store_var(name, pop(1));
                free(name);
                break;
            }
            case OP_LOAD: {
                uint32_t len = *(uint32_t*)(code + pc); pc += 4;
                char* name = strndup((char*)(code + pc), len); pc += len;
                push(load_var(name), 1);
                free(name);
                break;
            }
            case OP_PULL: {
                uint32_t len1 = *(uint32_t*)(code + pc); pc += 4;
                char* src = strndup((char*)(code + pc), len1); pc += len1;
                uint32_t len2 = *(uint32_t*)(code + pc); pc += 4;
                char* dst = strndup((char*)(code + pc), len2); pc += len2;
                int s = (strcmp(src, "1") == 0) ? 1 : 2;
                int d = (strcmp(dst, "1") == 0) ? 1 : 2;
                Value v = pop(s);
                push(v, d);
                free(src);
                free(dst);
                break;
            }
            case OP_JMP_EQ0: {
                int target = *(int*)(code + pc); pc += 4;
                Value v = pop(1);
                if ((v.type == VAL_INT && v.i == 0) || (v.type == VAL_DBL && v.d == 0.0))
                    pc = target;
                break;
            }
            case OP_JMP_GT0: {
                int target = *(int*)(code + pc); pc += 4;
                Value v = pop(1);
                if ((v.type == VAL_INT && v.i > 0) || (v.type == VAL_DBL && v.d > 0.0))
                    pc = target;
                break;
            }
            case OP_JMP: {
                int target = *(int*)(code + pc); pc = target;
                break;
            }
            case OP_PRINT: {
                if (sp1 <= 0) { fprintf(stderr, "Stack1 empty on PRINT\n"); exit(1); }
                Value v = stack1[sp1 - 1];
                if (v.type == VAL_INT) printf("%d\n", v.i);
                else if (v.type == VAL_DBL) printf("%.15g\n", v.d);
                else if (v.type == VAL_STR) printf("%s\n", v.s);
                break;
            }
            case OP_STR_CONCAT: {
                Value b = pop(1);
                Value a = pop(1);
                if (a.type != VAL_STR || b.type != VAL_STR) {
                    fprintf(stderr, "STR.CONCAT: operands must be strings\n");
                    exit(1);
                }
                int newlen = strlen(a.s) + strlen(b.s);
                char* res = malloc(newlen + 1);
                strcpy(res, a.s);
                strcat(res, b.s);
                push((Value){ VAL_STR, .s = res }, 1);
                break;
            }
            case OP_HALT:
                free(code);
                return;
            default:
                fprintf(stderr, "Unknown opcode: 0x%02X\n", op);
                exit(1);
        }
    }

    free(code);
}
