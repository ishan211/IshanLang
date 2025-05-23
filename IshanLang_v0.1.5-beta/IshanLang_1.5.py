#####################################################################################
# IshanLang Interpreter 1.5
# Created by Ishan Leung
#
# To Interpret IshanLang files (.il)
# Usage: python IshanLang_1.5.py <file> <show_program: 1 or 0> <show_stack: 1 or 0>
#####################################################################################


import sys

if len(sys.argv) != 4:
    print("Usage: python interpreter.py <file> <show_program: 1 or 0> <show_stack: 1 or 0>")
    sys.exit(1)

program_filepath = sys.argv[1]
show_program = sys.argv[2] == "1"
show_stack = sys.argv[3] == "1"

with open(program_filepath, "r") as f:
    program_lines = [line.strip() for line in f.readlines()]

program = []
token_counter = 0
label_tracker = {}

for line in program_lines:
    parts = line.split(" ")
    opcode = parts[0]

    if opcode == "":
        continue
    if opcode.endswith(":"):
        label_tracker[opcode[:-1]] = token_counter
        continue

    program.append(opcode)
    token_counter += 1

    if opcode == "PUSH.INT":
        number = int(parts[1])
        program.append(number)
        token_counter += 1
    elif opcode == "PUSH.DBL":
        number = round(float(parts[1]), 15)
        program.append(number)
        token_counter += 1
    elif opcode == "PRINT":
        string_literal = ' '.join(parts[1:])[1:-1]
        program.append(string_literal)
        token_counter += 1
    elif opcode in ("JUMP.EQ.0", "JUMP.GT.0"):
        program.append(parts[1])
        token_counter += 1

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
        val = self.buf[self.sp]
        self.sp -= 1
        return val
    def top(self):
        return self.buf[self.sp]
    def peek(self, offset=0):
        return self.buf[self.sp - offset]
    def display(self):
        if show_stack:
            print("Stack:", self.buf[:self.sp + 1])

pc = 0
stack = Stack(256)

while program[pc] != "HALT":
    opcode = program[pc]
    pc += 1

    if opcode == "PUSH.INT" or opcode == "PUSH.DBL":
        number = program[pc]
        pc += 1
        stack.push(number)
    elif opcode == "POP":
        stack.pop()
    elif opcode == "ADD":
        a = stack.pop()
        b = stack.pop()
        stack.push(b + a)
    elif opcode == "SUB":
        a = stack.pop()
        b = stack.pop()
        stack.push(b - a)
    elif opcode == "MUL":
        a = stack.peek(0)
        b = stack.peek(1)
        stack.push(b * a)
    elif opcode == "DIV":
        a = stack.peek(0)
        b = stack.peek(1)
        if a == 0:
            print("Error: Division by zero")
            sys.exit(1)
        stack.push(b / a)
    elif opcode == "EXP":
        a = stack.peek(0)
        b = stack.peek(1)
        stack.push(b ** a)
    elif opcode == "MOD":
        a = stack.peek(0)
        b = stack.peek(1)
        stack.push(b % a)
    elif opcode == "PRINT":
        string_literal = program[pc]
        pc += 1
        print(string_literal)
    elif opcode == "READ":
        val = input()
        number = float(val) if '.' in val else int(val)
        stack.push(round(number, 15))
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
        print("Unexpected opcode:", opcode)
        sys.exit(1)

    stack.display()
