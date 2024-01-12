; -- header --
bits 64
default rel
; -- variables --
section .bss
read_number resq 1 ; 64-bits integer = 8 bytes
; -- constants --
section .data
read_format db "%d", 0 ; the format string for scanf
string_literal_0 db "not equal", 0
string_literal_1 db "equal", 0
; -- entry point --
section .text
global main
extern ExitProcess
extern printf
extern scanf
    
main:
	PUSH rbp
	MOV rbp, rsp
	SUB rsp, 32
; -- read --
	LEA rcx, read_format
	LEA rdx, read_number
	XOR eax, eax
	CALL scanf
	PUSH qword [read_number]
; -- jump.eq.0 --
	CMP qword [rsp], 0
	JE L1
; -- print --
	LEA rcx, string_literal_0
	XOR eax, eax
	CALL printf
; -- halt --
	JMP EXIT_LABEL
; -- label --
L1:
; -- print --
	LEA rcx, string_literal_1
	XOR eax, eax
	CALL printf
; -- halt --
	JMP EXIT_LABEL
	EXIT_LABEL:
	XOR rax, rax
	CALL ExitProcess
