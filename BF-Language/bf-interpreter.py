import sys

program_filepath = sys.argv[1]

f = open(program_filepath, "r")
valid_chars = ['.', ',', '[', ']', '<', '>', '+', '-']
clean_text = ''.join(filter(lambda x: x in valid_chars, f.read()))

pos_mapping = {}
stack = []
for pos, char in enumerate(clean_text):
    if char == "[":
        stack.append(pos)
    elif char == "]":
        starting_pos = stack.pop()
        pos_mapping[starting_pos] = pos
        pos_mapping[pos] = starting_pos
if len(stack) > 0:
    raise ValueError('Mismatch brackets')

program_counter = 0
tap_pointer = 0
tap = [0]

if "," in clean_text:
    user_input = input()

while program_counter < len(clean_text):
    command = clean_text[program_counter]
    if command == "+":
        tap[tap_pointer] += 1
    elif command == "-":
        tap[tap_pointer] -= 1
    elif command == ">":
        tap_pointer += 1
        if tap_pointer == len(tap):
            tap.append(0)
    elif command == "<":
        tap_pointer -= 1
        tap_pointer = max(tap_pointer, 0)
    elif command == ",":
        if len(user_input) > 0:
            tap[tap_pointer] = ord(user_input[0])
            user_input = user_input[1:]
    elif command == ".":
        print(chr(tap[tap_pointer]), end="")
    elif command == "[" and tap[tap_pointer] == 0:
            program_counter = pos_mapping[program_counter]
    elif command == "]" and tap[tap_pointer] != 0:
            program_counter = pos_mapping[program_counter]

    program_counter += 1
    