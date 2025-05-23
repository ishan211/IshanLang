#include <stdio.h>
#include <stdlib.h>
#include "lexer.h"
#include "parser.h"
#include "codegen.h"
#include "vm.h"

int main(int argc, char** argv) {
    if (argc < 2) {
        fprintf(stderr, "Usage: %s program.txt\n", argv[0]);
        return 1;
    }

    FILE* f = fopen(argv[1], "rb");
    if (!f) {
        perror("Failed to open source file");
        return 1;
    }

    fseek(f, 0, SEEK_END);
    long file_size = ftell(f);
    rewind(f);

    char* source = malloc(file_size + 1);
    fread(source, 1, file_size, f);
    source[file_size] = '\0';
    fclose(f);

    TokenList* tokens = tokenize(source);
    AST* ast = parse_tokens(tokens);
    generate_bytecode(ast, "program.bin");

    printf("Compiled successfully.\nRunning program:\n\n");
    run_vm("program.bin");

    free(source);
    free_tokens(tokens);
    free_ast(ast);

    return 0;
}
