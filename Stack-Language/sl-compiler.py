# %%
import os
program_filepath = "program1.sl"

# %%
print("[CMD] Parsing")
with open(program_filepath, "r") as program_file:
    program_lines = [line.strip() for line in program_file.readlines()]

# %%
def handle_push(parts):
    return int(parts[1])

def handle_print(parts):
    return ' '.join(parts[1:])[1:-1]

def handle_jump(parts):
    return parts[1]


opcode_handlers = {
    "PUSH": handle_push,
    "PRINT": handle_print,
    "JUMP.EQ.0": handle_jump,
    "JUMP.GT.0": handle_jump
}

program = []

for line in program_lines:
    parts = line.split(" ")
    opcode = parts[0]
    if opcode == "":
        continue

    if not opcode.endswith(":") and opcode not in  ["HALT", "SUB", "READ", "ADD", "POP", "PUSH", "PRINT", "JUMP.EQ.0", "JUMP.GT.0"]:
        print(f"Error opcode {opcode}")

    program.append(opcode)

    if opcode in opcode_handlers:
        label = opcode_handlers[opcode](parts)
        program.append(label)


# %%
string_literals =[]
for instruction_pointer in range(len(program)):
    if program[instruction_pointer] == "PRINT":
        string_literal = program[instruction_pointer+1]
        program[instruction_pointer+1] = len(string_literals)
        string_literals.append(string_literal)


# %%
asm_filepath = program_filepath[:-3] + ".asm"
with open(asm_filepath, "w") as asm_file:
    asm_file.write("""; -- header --
bits 64
default rel
""")

    asm_file.write("""; -- variables --
section .bss
read_number resq 1 ; 64-bits integer = 8 bytes
""")

    asm_file.write("""; -- constants --
section .data
read_format db "%d", 0 ; the format string for scanf
""")
    
    for i, string_literal in enumerate(string_literals):
        asm_file.write(f"string_literal_{i} db \"{string_literal}\", 0\n")


    asm_file.write("""; -- entry point --
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

    instruction_pointer = 0
    while instruction_pointer < len(program):
        opcode = program[instruction_pointer]
        instruction_pointer += 1

        if opcode.endswith(':'):
            asm_file.write("; -- label --\n")
            asm_file.write(f"{opcode}\n")
        elif opcode == "PUSH":
            number = program[instruction_pointer]
            instruction_pointer += 1
            asm_file.write("; -- push --\n")
            asm_file.write(f"\tPUSH {number}\n")
        elif opcode == "POP":
            asm_file.write("; -- pop --\n")
            asm_file.write("\tPOP\n")
        elif opcode == "ADD":
            asm_file.write("; -- add --\n")
            asm_file.write("\tPOP rax\n")
            asm_file.write("\tADD qword [rsp], rax\n")
            # asm_file.write("\tPOP rbx\n")
            # asm_file.write("\tADD rbx, rax\n")
            # asm_file.write("\tPUSH rbx\n")
        elif opcode == "ADD":
            asm_file.write("; -- sub --\n")
            asm_file.write("\tPOP rax\n")
            asm_file.write("\tSUB qword [rsp], rax\n")
        elif opcode == "PRINT":
            string_index = program[instruction_pointer]
            instruction_pointer += 1
            asm_file.write("; -- print --\n")
            asm_file.write(f"\tLEA rcx, string_literal_{string_index}\n")
            asm_file.write("\tXOR eax, eax\n")
            asm_file.write("\tCALL printf\n")
        elif opcode == "READ":
            print_string = program[instruction_pointer]
            instruction_pointer += 1
            asm_file.write("; -- read --\n")
            asm_file.write("\tLEA rcx, read_format\n")
            asm_file.write("\tLEA rdx, read_number\n")
            asm_file.write("\tXOR eax, eax\n")
            asm_file.write("\tCALL scanf\n")
            asm_file.write("\tPUSH qword [read_number]\n")
        elif opcode == "JUMP.EQ.0":
            label = program[instruction_pointer]
            instruction_pointer += 1
            asm_file.write("; -- jump.eq.0 --\n")
            asm_file.write("\tCMP qword [rsp], 0\n")
            asm_file.write(f"\tJE {label}\n")
        elif opcode == "JUMP.EQ.0":
            label = program[instruction_pointer]
            instruction_pointer += 1
            asm_file.write("; -- jump.eq.0 --\n")
            asm_file.write("\tCMP qword [rsp], 0\n")
            asm_file.write(f"\tJG {label}\n")
        elif opcode == "HALT":
            asm_file.write("; -- halt --\n")
            asm_file.write("\tJMP EXIT_LABEL\n")
    asm_file.write("\tEXIT_LABEL:\n")
    asm_file.write("\tXOR rax, rax\n")
    asm_file.write("\tCALL ExitProcess\n")

        

# %%
print("[CMD] Assembling")
os.system(f'nasm -f elf64 {asm_filepath}')
print("[CMD] Linking")
os.system(f"gcc -o {asm_filepath[:-4] + '.exe'} {asm_filepath[:-4] + '.o'}")


