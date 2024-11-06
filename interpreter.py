#Add testing comments

import sys

# Check for the correct number of arguments
if len(sys.argv) != 3:
    print("Usage: python interpreter.py <file> <print_stack: 1 or 0>")
    sys.exit(1)

# Read arguments
program_filepath = sys.argv[1]
print_stack = sys.argv[2]

# Ensure the print_stack argument is valid
if print_stack not in ("0", "1"):
    print("Error: print_stack must be 1 or 0.")
    sys.exit(1)

# Read file line by line
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
        # Real label
        label = parts[1]
        program.append(label)
        token_counter += 1
    elif opcode == "JUMP.GT.0":
        # Real label
        label = parts[1]
        program.append(label)
        token_counter += 1
    
    # Print program if print_stack is set to 1
    if print_stack == "1":
        print(program)

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
        stack.push(a - b)
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