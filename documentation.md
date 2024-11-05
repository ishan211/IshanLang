# IshanLang |  Language and Interpreter Documentation [1.0]

## Overview
This documentation covers how to use the custom language and interpreter, which operates using a stack-based virtual machine with basic instructions and control flow capabilities. 

The language supports labels, conditional jumps, stack operations, arithmetic, and I/O instructions.

## Table of Contents
1. [Getting Started](#getting-started)
2. [Syntax](#syntax)
3. [Opcodes](#opcodes)
4. [Labels and Jumps](#labels-and-jumps)
5. [Running the Program](#running-the-program)

## Getting Started

To run a program written in this language, use the interpreter script `interpreter.py`. The command syntax is:
```bash
python interpreter.py <program_file> <print_stack>
```

- `<program_file>`: Path to the program file (e.g., `example1.il`).
- `<print_stack>`: Optional flag to print the call stack. Set `1` to print or `0` to disable.

Example:
```bash
python interpreter.py example1.il 1
```

## Syntax
Each line in the program file should contain a single instruction or label.

### Instruction Format
- An instruction typically consists of an opcode, followed by an optional argument.
- Opcodes are keywords that represent operations (e.g., `READ`, `PUSH`, `JUMP.EQ.0`).
- Arguments are separated by a space from the opcode.

### Label Format
- Labels are identifiers followed by a colon (`:`), such as `L1:`.
- Labels do not perform actions but serve as targets for jump instructions.

## Opcodes

### 1. Stack Operations
- **`PUSH <number>`**  
  Pushes `<number>` onto the stack.
  - Example: `PUSH 10` pushes `10` onto the stack.

- **`POP`**  
  Removes the top value from the stack.
  - Example: `POP`

### 2. Arithmetic Operations
- **`ADD`**  
  Pops the top two values from the stack, adds them, and pushes the result.
  - Example: `ADD` with stack `[2, 3]` results in `[5]`.

- **`SUB`**  
  Pops the top two values, subtracts the second popped value from the first, and pushes the result.
  - Example: `SUB` with stack `[5, 3]` results in `[2]`.

### 3. I/O Operations
- **`READ`**  
  Reads an integer from user input and pushes it onto the stack.
  - Example: `READ` prompts the user for input.

- **`PRINT <string>`**  
  Prints the provided string literal to the console.
  - Example: `PRINT "Hello, World!"` outputs `Hello, World!`.

### 4. Control Flow
- **`JUMP.EQ.0 <label>`**  
  Checks the top value of the stack. If it is `0`, jumps to the specified label.
  - Example: `JUMP.EQ.0 L1`

- **`JUMP.GT.0 <label>`**  
  Checks the top value of the stack. If it is greater than `0`, jumps to the specified label.
  - Example: `JUMP.GT.0 L1`

### 5. Program Control
- **`HALT`**  
  Terminates program execution.
  - Example: `HALT`

## Labels and Jumps

### Labels
Labels are markers in the code used as targets for conditional jumps. A label has the following format:
```plaintext
L1:
```
- Labels must be unique and end with a colon (`:`).
- Labels are not executed but serve as jump destinations for flow control.

### Jump Instructions
Jump instructions redirect the flow based on the top value of the stack.

1. **`JUMP.EQ.0 <label>`**  
   Jumps to `<label>` if the top stack value is `0`. If the condition is not met, it moves to the next instruction.

2. **`JUMP.GT.0 <label>`**  
   Jumps to `<label>` if the top stack value is greater than `0`. If the condition is not met, it moves to the next instruction.

## Example Program

Below is an example program, `example1.il`, which compares two input values and prints whether they are equal or not.

```plaintext
READ
READ
SUB
JUMP.EQ.0 L1
PRINT "not equal"
HALT

L1:
PRINT "equal"
HALT
```

### Explanation
1. **READ**: Prompts the user for two inputs, pushing them onto the stack.
2. **SUB**: Subtracts the second value from the first (checks if they are equal by resulting in `0`).
3. **JUMP.EQ.0 L1**: If the result of the subtraction is `0`, jumps to `L1`.
4. **PRINT "not equal"**: If not equal, it prints "not equal".
5. **L1**: Label that, if jumped to, skips the "not equal" message.
6. **PRINT "equal"**: Prints "equal" if the values are equal.
7. **HALT**: Stops execution.

## Running the Program

To run the above example with the interpreter, save it as `example1.il` and use the following command:

```bash
python interpreter.py example1.il 1
```

- The program will prompt for two inputs.
- It will output "equal" if the inputs are the same, or "not equal" otherwise.
- The `1` flag at the end will print the stack after each operation.

