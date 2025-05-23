/*
Title: Bytecode Instruction Definitions
Name: bytecode.h
Author: Ishan Leung
Language: C23
Description: Defines bytecode operation codes and conversion types for the virtual machine
*/

#pragma once
// Bytecode instruction opcodes
typedef enum {
    OP_PUSH_INT    = 0x01,
    OP_PUSH_STR    = 0x02,
    OP_PUSH_DBL    = 0x03,

    OP_READ_INT    = 0x10,
    OP_READ_STR    = 0x11,
    OP_READ_DBL    = 0x12,

    OP_CONV        = 0x20,
    OP_ADD         = 0x21,
    OP_SUB         = 0x22,
    OP_POP         = 0x23,
    OP_SWAP        = 0x24,
    OP_MUL         = 0x25,
    OP_DIV         = 0x26,
    OP_EXP         = 0x27,
    OP_MOD         = 0x28,

    OP_STORE       = 0x30,
    OP_LOAD        = 0x31,
    OP_PULL        = 0x32,

    OP_JMP_EQ0     = 0x40,
    OP_JMP_GT0     = 0x41,
    OP_JMP         = 0x42,

    OP_LABEL       = 0x50, // Not emitted

    OP_PRINT       = 0x60,
    OP_STR_CONCAT  = 0x61,

    OP_HALT        = 0xFF
} OpCode;

// Conversion subtypes for OP_CONV
typedef enum {
    CONV_INT_TO_STR = 1,
    CONV_INT_TO_DBL,
    CONV_STR_TO_INT,
    CONV_STR_TO_DBL,
    CONV_DBL_TO_INT,
    CONV_DBL_TO_STR
} ConvCode;
