;; -*-NASM-*-
;; G-Machine runtime
global _main
extern _exit
extern _printf
extern _puts
default rel

%define NULL 0

%define TAG_NIX 0
%define TAG_APP 1
%define TAG_INT 2
%define TAG_FUN 3
%define TAG_FWD 4
%define TAG_CON 5

;; Stats stuff
%define step_ctr r13
%define claim_ctr r14

%define @@(n) add step_ctr, n

;; The heap pointer
%define rhp r12

;; Auxiliary macros
%macro g_pre_call_c 0
  @@(3)
  mov  rbx, rsp                 ; align stack ptr
  and  rbx, 0x0F                ; (dito)
  sub  rsp, rbx                 ; (dito)
%endmacro

%macro g_post_call_c 0
  @@(1)
  add  rsp, rbx                 ; restore stack ptr
%endmacro

%macro g_fail 1
  lea  rdi, [%1]
  jmp  _fail
%endmacro


;; CAF related stuff
%define CAF(label) __caf__ %+ label
%define G_INSTALL_CAFS __install__cafs__
%define G_INSTALL_MAIN __install__main__

%macro g_declare_cafs 1-*
  section .data
  align 8
  %rep %0
    CAF(%1): dq TAG_FUN, 0, 0
    %rotate 1
  %endrep

  section .text
G_INSTALL_CAFS:
  @@(3*%0+1)
  %rep %0
    lea  rax, [CAF(%1)+8]
    lea  rbx, [%1]
    mov  [rax], rbx
    %rotate 1
  %endrep
  ret
%endmacro

%macro g_install_cafs 0
  @@(1)
  call G_INSTALL_CAFS
%endmacro

%macro g_pushcaf 1
  @@(2)
  lea  rax, [CAF(%1)]
  push rax
%endmacro

%macro g_declare_main 1
  @@(2)
G_INSTALL_MAIN:
  pop  rbx
  g_pushcaf %1
  jmp  rbx
%endmacro

%macro g_install_main 0
  call G_INSTALL_MAIN
%endmacro


;; Heap operations
%macro g_claim 1
  @@(4)
  inc  claim_ctr
  lea  rax, [rhp+24*%1]
  lea  rcx, [hend]
  cmp  rax, rcx
  jle  %%ok
  g_fail HEAP_EXHAUSTED
%%ok:
%endmacro

%macro g_create 3
  @@(4)
  mov  qword [rhp   ], %1
  mov  qword [rhp+ 8], %2
  mov  qword [rhp+16], %3
  add  rhp, 24
%endmacro

;; TODO: make this faster
%macro g_alloc 1
  @@(1)
  g_claim %1
%rep %1
  push rhp
  g_create TAG_NIX, 0, 0
%endrep
%endmacro


;; Stack control
%macro g_globstart 2            ; %1 = name, % = arity
%1:
%ifdef DEBUG
  mov  rax, rbp
  sub  rax, rsp
  cmp  rax, 8*%2
  jge  %%ok
  g_fail MISSING_ARGUMENTS
%%ok:
%endif
  @@(2+3*%2)
  lea  rsi, [rsp+8]
  mov  rdi, rsp
%rep %2
  lodsq
  mov  rax, [rax+16]
  stosq
%endrep
%endmacro

%macro g_push 1
  @@(1)
  push qword [rsp+8*%1]
%endmacro

%macro g_pop 1
  @@(1)
  add rsp, 8*%1
%endmacro

%macro g_slide 1
  @@(3)
  pop  rax
  add  rsp, 8*%1
  push rax
%endmacro

%macro g_update 1
  @@(6)
  cld
  mov  rdi, [rsp+8*%1]
  pop  rsi
  movsq
  movsq
  movsq
%endmacro


;; Node constructors
%macro g_pushint 1
  @@(1)
  g_claim 1
  push rhp
  g_create TAG_INT, %1, NULL
%endmacro

%macro g_pushglobal 2           ; %1 = label, %2 = arity
  %if %2 = 0
    g_pushcaf %1
  %else
    @@(2)
    g_claim 1
    lea  rax, [%1]
    push rhp
    g_create TAG_FUN | %2 << 8, rax, 0
  %endif
%endmacro

;; TODO: use rdi and stosq here
%macro g_cons 2                 ; %1 = tag, %2 = arity
  @@(8+%2)
  g_claim 1
  mov  rbx, rhp                 ; save heap ptr for later push
  mov  qword [rhp], TAG_CON | %1 << 8
  add  rhp, 8
%rep %2
  pop  rax
  mov  [rhp], rax
  add  rhp, 8
%endrep
%rep 2 - %2
  mov  qword [rhp], 0
  add  rhp, 8
%endrep
  push rbx
%endmacro

%macro g_mkap 0
  @@(3)
  g_claim 1
  pop  rax
  pop  rbx
  push rhp
  g_create TAG_APP, rax, rbx
%endmacro


;; Selectors
%macro g_head 0
  @@(2)
  pop rax
  push qword [rax+8]
%endmacro

%macro g_tail 0
  @@(2)
  pop rax
  push qword [rax+16]
%endmacro


;; Arithmetic
%macro g_neg 0
  @@(4)
  g_claim 1
  pop rax
  mov rcx, [rax+8]
  neg rcx
  push rhp
  g_create TAG_INT, rcx, NULL
%endmacro

%macro g_binop 1
  @@(5)
  g_claim 1
  pop  rax
  mov  rcx, [rax+8]
  pop  rax
  %1   rcx, [rax+8]
  push rhp
  g_create TAG_INT, rcx, NULL
%endmacro

%define g_add g_binop add
%define g_sub g_binop sub
%define g_mul g_binop imul

%macro g_divop 1                ; %1: rax = quotient, rdx = remainder
  @@(7)
  g_claim 1
  pop  rdx
  mov  rax, [rdx+8]
  pop  rdx
  mov  rcx, [rdx+8]
  cqo
  idiv rcx                      ; TODO: use [...] as argument
  push rhp
  g_create TAG_INT, %1, NULL
%endmacro

%define g_div g_divop rax
%define g_mod g_divop rdx


;; Comparison relations
%macro g_relop 1                ; %1 is some setCC instruction
  @@(8)
  g_claim 1
  pop  rax
  mov  rbx, [rax+8]
  pop  rax
  mov  rcx, [rax+8]
  mov  rax, TAG_CON
  cmp  rbx, rcx
  %1   ah
  push rhp
  g_create rax, NULL, NULL
%endmacro

%define g_les g_relop setb
%define g_leq g_relop setbe
%define g_eqv g_relop sete
%define g_neq g_relop setne
%define g_geq g_relop setae
%define g_gtr g_relop seta

;; I/O
%macro g_print 0
  @@(5)
  pop  rax
  g_pre_call_c
  lea  rdi, [FORMAT_PRINT]
  mov  rsi, [rax+8]
  mov  rax, 0
  call _printf
  g_post_call_c
%endmacro

%macro g_debug 0
  g_pre_call_c
  lea  rdi, [FORMAT_DEBUG]
  mov  rsi, [rax]
  mov  rdx, [rax+ 8]
  mov  rcx, [rax+16]
  mov  rax, 0
  call _printf
  g_post_call_c
%endmacro


;; Jumps
%macro g_jump 1
  @@(1)
  jmp  %1
%endmacro

%macro g_jumpzero 1
  @@(3)
  pop  rax
  cmp  word [rax], TAG_CON       ; assumes little endian
  je   %1
%endmacro

%macro g_label 1
%1:
%endmacro

;; Evaluation control
%macro g_abort 0
  g_fail ABORT_CALLED
%endmacro

%macro g_eval 0
  @@(1)
  call _eval
%endmacro

%macro g_unwind 0
  @@(1)
  jmp  _unwind
%endmacro

%macro g_return 0
  @@(1)
  jmp _unwind.partial
%endmacro


section .text

;; "Main loop" of the evaluation
_main:
  push rbx
  lea  rhp, [hstart]
  g_install_cafs
  g_install_main
  g_eval
  lea  rdi, [STATS_MESSAGE]
  mov  rsi, step_ctr
  mov  rax, rhp
  lea  rbx, [hstart]
  sub  rax, rbx
  mov  rdx, 0
  mov  rbx, 24
  div  rbx
  mov  rdx, rax
  mov  rcx, claim_ctr
  mov  rax, 0
  g_pre_call_c
  call _printf
  g_post_call_c
  pop  rbx
  mov  rax, [rbx+8]
  pop  rbx
  ret

;; clean up this mess
_eval:
  @@(3)
  mov  rax, [rsp+8]
  cmp  byte [rax], TAG_APP      ; assumes little endian
  je   .app
  @@(2)
  cmp  word [rax], TAG_FUN      ; the lower tag word is TAG_FUN only for CAFs
  je   .caf
  @@(1)
  ret
.caf:
  @@(4)
  push rbp
  push rax
  mov  rbp, rsp
  jmp  [rax+8]
.app:
  @@(3)
  push rbp
  push rax
  mov  rbp, rsp
  ; jmp  unwind

_unwind:
  @@(3)
  mov  rax, [rsp]
  cmp  byte [rax], TAG_APP      ; assumes little endian
  jne  .done
  @@(2)
  push qword [rax+8]
  jmp _unwind
.done:
  @@(2)
  cmp  byte [rax], TAG_FUN     ; assumes little endian
  jne  .return
  @@(4)
  ; calc where rbp would need to be for a full application
  ; if rbp is smaller, we have a partial application
  movzx rbx, byte [rax+1]
  lea   rcx, [rsp+8*rbx]        ; TODO: test that this works correctly
  cmp   rbp, rcx
  jl    .partial
  @@(1)
  jmp   [rax+8]
.partial:
  @@(1)
  mov rsp, rbp
.return:
  @@(3)
  pop  rax
  pop  rbp
  ret

_fail:
  g_pre_call_c
  call _puts
  mov  rdi, 1
  call _exit


section .data
align 8
FORMAT_PRINT:
  db "%ld", 10, 0
FORMAT_DEBUG:
  db "TAG : 0x%016lx", 10, "ARG1: 0x%016lx", 10, "ARG2: 0x%016lx", 10, 0
STATS_MESSAGE:
  db 10, "Steps : %10ld", 10, "Allocs: %10ld", 10, "Checks: %10ld", 10, 0
HEAP_EXHAUSTED:
  db "HEAP EXHAUSTED", 10, 0
MISSING_ARGUMENTS:
  db "MISSING ARGUMENTS", 10, 0
ABORT_CALLED:
  db "ABORT CALLED", 10, 0

;; Heap
section .bss
align 8
hstart:
  resb HEAP_SIZE
hend:
  resq 1
