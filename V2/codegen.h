/*
Title: Bytecode Generator Header
Name: codegen.h
Author: Ishan Leung
Language: C23
Description: Defines functions for generating bytecode from AST
*/

#pragma once
#include "parser.h"
void generate_bytecode(AST* ast, const char* out_file);