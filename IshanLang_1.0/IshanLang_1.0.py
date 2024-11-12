#####################################################################################
# IshanLang Interpreter 1.0
# Created by Ishan Leung
# Interpreter created Using Python 3.9.6 on 2024-11-06
#
# To Interpret IshanLang files (.il)
# Usage: python interpreter.py <file> <show_program: 1 or 0> <show_stack: 1 or 0>
#####################################################################################


import sys

# Check for the correct number of arguments
if len(sys.argv) != 4:
    print("Usage: python interpreter.py <file> <show_program: 1 or 0> <show_stack: 1 or 0>")
    sys.exit(1)

# Read arguments
program_filepath = sys.argv[1]
show_program = sys.argv[2]
show_stack = sys.argv[3]

# Ensure the show_program and show_stack arguments are valid
if show_program not in ("0", "1") or show_stack not in ("0", "1"):
    print("Error: show_program and show_stack must be 1 or 0.")
    sys.exit(1)

# Convert flags to booleans for easier use later
show_program = show_program == "1"
show_stack = show_stack == "1"

# Read file lines
program_lines = []
with open(program_filepath, "r") as program_file:
    program_lines = [line.strip() for line in program_file.readlines()]

program = []
token_counter = 0
label_tracker = {}
for line in program_lines:
    parts = line.split(" ")
    opcode = parts[0]

    # Check for empty line
    if opcode == "":
        continue

    # Check for label
    if opcode.endswith(":"):
        label_tracker[opcode[:-1]] = token_counter
        continue    

    # Store opcode token
    program.append(opcode)
    token_counter += 1

    # Handle each opcode
    if opcode == "PUSH":
        # Expecting number
        number = int(parts[1])
        program.append(number)
        token_counter += 1
    elif opcode == "PRINT":
        # Parse string literal
        string_literal = ' '.join(parts[1:])[1:-1]
        program.append(string_literal)
        token_counter += 1
    elif opcode == "JUMP.EQ.0":
        # Read label
        label = parts[1]
        program.append(label)
        token_counter += 1
    elif opcode == "JUMP.GT.0":
        # Read label
        label = parts[1]
        program.append(label)
        token_counter += 1

    # Print tokenized program if show_program flag is set
    if show_program:
        print("Tokenized program:", program)

class Stack:
    def __init__(self, size):
        self.buf = [0 for _ in range(size)]
        self.sp = -1
    def push(self, number):
        self.sp += 1
        self.buf[self.sp] = number
    def pop(self):
        number = self.buf[self.sp]
        self.sp -= 1
        return number
    def top(self):
        return self.buf[self.sp]
    def display(self):
        # Only display stack if show_stack flag is True
        if show_stack:
            print("Stack:", self.buf[:self.sp + 1])

pc = 0
stack = Stack(256)

while program[pc] != "HALT":
    opcode = program[pc]
    pc += 1

    if opcode == "PUSH":
        number = program[pc]
        pc += 1
        stack.push(number)
    elif opcode == "POP":
        stack.pop()
    elif opcode == "ADD":
        a = stack.pop()
        b = stack.pop()
        stack.push(a + b)
    elif opcode == "SUB":
        a = stack.pop()
        b = stack.pop()
        stack.push(b - a)
    elif opcode == "PRINT":
        string_literal = program[pc]
        pc += 1
        print(string_literal)
    elif opcode == "READ":
        number = int(input())
        stack.push(number)
    elif opcode == "JUMP.EQ.0":
        number = stack.top()
        if number == 0:
            pc = label_tracker[program[pc]]
        else:
            pc += 1
    elif opcode == "JUMP.GT.0":
        number = stack.top()
        if number > 0:
            pc = label_tracker[program[pc]]
        else:
            pc += 1
    else:
        print("Unexpected opcode received")
        exit(1)

    # Display stack after each instruction if show_stack flag is enabled
    stack.display()
