# Interpreter
This is my holiday project to learn how to build a simple interpreter and compile for a stack-based language

I follow the tutorials from a YouTuber called [bvdl․io](https://www.youtube.com/@bvdlio). Specifically, I follow these two videos. [Video 1](https://www.youtube.com/watch?v=A3gTw1ZkeK0&t=30s) and [Video 2](https://www.youtube.com/watch?v=GsCWivTeFpY)

This stack-based language has the follow operations
```
PUSH <value>
POP
ADD
SUB
PRINT
READ
JUMP.EQ.0
JUMP.GT.0
```

I also tried to code an interpreter for an esoteric programming language. I reference some public repo and made a bf interpreter.

| Character | Meaning |
|-----------|---------|
| >         | Increment the data pointer by one (to point to the next cell to the right). |
| <         | Decrement the data pointer by one (to point to the next cell to the left). |
| +         | Increment the byte at the data pointer by one. |
| -         | Decrement the byte at the data pointer by one. |
| .         | Output the byte at the data pointer. |
| ,         | Accept one byte of input, storing its value in the byte at the data pointer. |
| [         | If the byte at the data pointer is zero, then instead of moving the instruction pointer forward to the next command, jump it forward to the command after the matching `]` command. |
| ]         | If the byte at the data pointer is nonzero, then instead of moving the instruction pointer forward to the next command, jump it back to the command after the matching `[` command. |
