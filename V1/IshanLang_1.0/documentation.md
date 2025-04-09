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
python interpreter.py <program_file> <show_program> <show_stack>
```

- `<program_file>`: Path to the program file (e.g., `example1.il`).
- `<show_program>`: Flag to print the program opcodes. Set `1` to print or `0` to disable.
- `<show_stack>`: Flag to print the call stack. Set `1` to print or `0` to disable.

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

# Example Programs

## Example 1: Equality Check
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

### Explanation:
1. **READ**: Prompts the user to input a value and pushes it onto the stack.
2. **READ**: Prompts for a second input and pushes this value onto the stack.
3. **SUB**: Pops the top two values, subtracts the second value from the first, and pushes the result.  
   - If the two inputs are equal, the result will be `0`, indicating equality.
4. **JUMP.EQ.0 L1**: Checks the top of the stack. If it is `0` (indicating equality), it jumps to label `L1`.
5. **PRINT "not equal"**: If the jump did not occur, this line prints "not equal," indicating the inputs were different.
6. **HALT**: Terminates the program.
7. **L1**: Label marking a jump point.
8. **PRINT "equal"**: If the inputs were equal, the program jumps here and prints "equal."
9. **HALT**: Terminates the program.

### Sample Outputs:
- **If inputs are equal**: 
  - User inputs: `5`, `5`
  - Output: `equal`
- **If inputs are not equal**: 
  - User inputs: `5`, `3`
  - Output: `not equal`

## Example 2: Check Even or Odd
```plaintext
READ
PUSH 1
ADD
JUMP.EQ.0 L1

LOOP:
PUSH 2
SUB
JUMP.EQ.0 L1
JUMP.GT.0 LOOP
PRINT "even"
HALT

L1:
PRINT "odd"
HALT
```


### Explanation:

1.  **READ**: Prompts the user to input a value and pushes it onto the stack.
    
2.  **PUSH 1**: Pushes `1` onto the stack.
    
3.  **ADD**: The program adds `1` to the previously read number (`3 + 1 = 4`). The stack now effectively has `4` (the updated input).
    
4.  **JUMP.EQ.0 L1**: This checks if the result (`4`) is equal to `0`. It is not, so it does not jump to `L1`.
    
5.  **LOOP**: The program enters the loop labeled `LOOP`.
    
6.  **PUSH 2**: Inside the loop, it pushes `2` onto the stack. The stack now has `[2]`.
    
7.  **SUB**: The program subtracts `2` from the current value (`4 - 2 = 2`). The value `2` is left on the stack.
    
8.  **JUMP.EQ.0 L1**: This checks if the result (`2`) is equal to `0`. It is not, so it does not jump.
    
9.  **JUMP.GT.0 LOOP**: This checks if the result (`2`) is greater than `0`. Since it is, the program jumps back to the start of the loop.
    
10.  The loop repeats:
    
    -   **PUSH 2**: Push `2` onto the stack again.
    -   **SUB**: Subtract `2` from `2`, resulting in `0`.
    -   **JUMP.EQ.0 L1**: This time, since the result is `0`, the program jumps to `L1`.
11.  **PRINT "odd"**: At label `L1`, the program prints "odd" because the original input was `3`, which is odd.
    
12.  **HALT**: The program terminates.
    

### Sample Outputs:

-   **Even Input Example**: For an input of `2`, the flow is as follows:
    
    -   Read `2`.
    -   Increment to `3` (thus checking the parity of `3`).
    -   Subtract `2` in the loop (resulting in `1`).
    -   Subtract again (resulting in `-1`, exiting the loop).
    -   It would end up printing "even".
-   **Odd Input Example**: For an input of `3`, the flow is as follows:
    
    -   Read `3`.
    -   Increment to `4` (thus checking the parity of `4`).
    -   Subtract `2` in the loop (resulting in `2`).
    -   Subtract again (resulting in `0`).
    -   Since the result is `0`, it jumps to label `L1` to print "odd".

 ### Credits:
 Created on: November 10, 2024 By Ishan Leung
 For similar projects and more cool stuff, check out my website at [https://ishanleung.netlify.app/](https://bit.ly/ishanleung) or my GitHub at [https://github.com/ishan211](https://github.com/ishan211).
