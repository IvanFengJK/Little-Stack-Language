import sys

program_filepath = sys.argv[1]

with open(program_filepath, "r") as program_file:
    program_lines = [line.strip() for line in program_file.readlines()]

# Tokenise Program

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

    if not opcode.endswith(":") and opcode not in  ["HALT", "SUB", "READ", "ADD", "POP", "PUSH", "PRINT", "JUMP.EQ.0", "JUMP.GT.0"]:
        print(f"Error opcode {opcode}")

    program.append(opcode)
    token_counter += 1

    if opcode in opcode_handlers:
        label = opcode_handlers[opcode](parts)
        program.append(label)
        token_counter += 1

# Stack Class
        
class Stack:

    def __init__(self, size) -> None:
        self.buf = [0] * size
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
    
# Interprete Program
    
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
        stack.push(a+b)
    elif opcode == "SUB":
        a = stack.pop()
        b = stack.pop()
        stack.push(b-a)
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