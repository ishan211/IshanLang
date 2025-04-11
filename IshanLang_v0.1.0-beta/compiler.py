import sys
import os

program_filepath = sys.argv[1]

print("[CMD] Parsing")

program_lines = []
with open(program_filepath, "r") as program_file:
    program_lines = [
        line.strip()
            for line in program_file.readlines()]
    
program = []
for line in program_lines:
    parts = line.split(" ")
    opcode = parts[0]

    if opcode == "":
        continue
    program.append(opcode)

    if opcode == "PUSH":
        number = int(parts[1])
        program.append(number)
    elif opcode == "PRINT":
        string_literal = ' '.join(parts[1:])[1:-1]
        program.append(string_literal)
    elif opcode  == "JUMP.EQ.0":
        label = parts[1]
        program.append(label)
    elif opcode == "JUMP.GT.0":
        label = parts[1]
        program.append(label)

string_literals = []
for ip in range(len(program)):
    if program[ip] == "PRINT":
        string_literal = program[ip + 1]
        program[ip + 1] = len(string_literals)
        string_literals.append(string_literal)

asm_filepath = program_filepath[:-4] + ".asm"
out = open(asm_filepath, "w")

out.write("""; -- header --
bits 64
default rel
""")

out.write("""; -- variables --
section .bss
read_number resq 1 ; var to store read num, 64-bit int = 8 bytes
""")

out.write("""; -- constants --
section .data
read_format db "%ld", 0 ; format string for scanf
""")

for i, string_literal in enumerate(string_literals):
    out.write(f"string_literal_{i} db \"{string_literal}\", 0\n")

out.write("""; -- entry point --
section .text
global main
extern ExitProcess
extern printf
extern scanf
          
main:
\tPUSH rbp
\tMOV rbp, rsp
\tSUB rsp, 32
""")

ip = 0
while ip < len(program):
    opcode = program[ip]
    ip += 1

    if opcode.endswith(":"):
        out.write(f"; -- Label ---\n")
        out.write(f"{opcode}\n")
    elif opcode == "PUSH":
        number = program[ip]
        ip += 1

        out.write(f"; -- PUSH ---\n")
        out.write(f"\tPUSH {number}\n")
    elif opcode == "POP":
        out.write(f"; -- POP ---\n")
        out.write(f"\tPOP\n")
    elif opcode == "ADD":
        out.write(f"; -- ADD ---\n")
        out.write(f"\tPOP rax\n")
        out.write(f"\tADD qword [rsp], rax\n")
    elif opcode == "SUB":
        out.write(f"; -- SUB ---\n")
        out.write(f"\tPOP rax\n")
        out.write(f"\tSUB qword [rsp], rax\n")
    elif opcode == "PRINT":
        string_literal_index = program[ip]
        ip += 1

        out.write(f"; -- PRINT ---\n")
        out.write(f"\tLEA rcx, string_literal_{string_literal_index}\n")
        out.write(f"\tXOR eax, eax\n")
        out.write(f"\tCALL printf\n")
    elif opcode == "READ":
        out.write(f"; -- READ ---\n")
        out.write(f"\tLEA rcx, read_format\n")
        out.write(f"\tLEA rdx, read_number\n")
        out.write(f"\tXOR eax, eax\n")
        out.write(f"\tCALL scanf\n")
        out.write(f"\tPUSH qword [read_number]\n")
    elif opcode == "JUMP.EQ.0":
        label = program[ip]
        ip += 1

        out.write(f"; -- JUMP.EQ.0 ---\n")
        out.write(f"\tCMP qword [rsp], 0\n")
        out.write(f"\tJE {label}\n")
    elif opcode == "JUMP.GT.0":
        label = program[ip]
        ip += 1

        out.write(f"; -- JUMP.GT.0 ---\n")
        out.write(f"\tCMP qword [rsp], 0\n")
        out.write(f"\tJG {label}\n")
    elif opcode == "HALT":
        out.write(f"; -- HALT ---\n")
        out.write(f"\tJMP EXIT_LABEL\n")

out.write("EXIT_LABEL:\n")
out.write(f"\tXOR rax, rax\n")   
out.write(f'\tCALL ExitProcess\n')      

out.close()

print("[CMD] Assembling")
os.system(f"nasm -f win64 {asm_filepath}")
print("[CMD] Linking")
os.system(f"gcc -o {asm_filepath[:-4] + '.exe'} {asm_filepath[:-3] + 'obj'}")

print("[CMD] Running")
os.system(f"{asm_filepath[:-4] + '.exe'}")
