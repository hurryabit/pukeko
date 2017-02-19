;; -*-NASM-*-
;; G-Machine runtime
global _main
extern _exit
extern _printf
extern _puts
default rel

%define NULL 0

%define TAG_NIX 0
%define TAG_FWD 1
%define TAG_APP 2
%define TAG_INT 3
%define TAG_FUN 4
%define TAG_CON 5

%define TAG_MIN_GOOD TAG_APP
%define TAG_MAX_GOOD TAG_CON

;; Stats stuff
%define step_ctr r14
%define claim_ctr r15

%define @@(n) add step_ctr, n

;; The heap pointer and the heap limit
%define rhp r12
%define rhl r13

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


;; Debugging stuff
%macro g_assert_tag 3           ; %1 = reg with ptr, %2 = tag, %3 = error msg
%ifdef DEBUG
  cmp  byte [%1], %2
  je   %%ok
  g_fail %3
%%ok:
%endif
%endmacro

%macro g_assert_int 1
  g_assert_tag %1, TAG_INT, MSG_EXPECTED_INT
%endmacro

%macro g_assert_good_tag 2     ; %1 = reg with ptr, %2 = error msg
%ifdef DEBUG
  cmp  byte [%1], TAG_MIN_GOOD
  jl   %%bad
  cmp  byte [%1], TAG_MAX_GOOD
  jg   %%bad
  jmp  %%ok
%%bad:
  g_fail %2
%%ok:
%endif
%endmacro


;; CAF related stuff
%define CAF(label) __caf__ %+ label

%macro g_declare_cafs 1-*
section .data
  align 8
__cafs__start__:
%rep %0
  CAF(%1): dq TAG_FUN, 0, 0
%rotate 1
%endrep
__cafs__end__:

section .text
__install__cafs__:
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
  call __install__cafs__
%endmacro

%macro g_pushcaf 1
  @@(2)
  lea  rax, [CAF(%1)]
  push rax
%endmacro

%macro g_declare_main 1
  @@(2)
__install__main__:
  pop  rbx
  g_pushcaf %1
  jmp  rbx
%endmacro

%macro g_install_main 0
  call __install__main__
%endmacro


;; Heap operations
%macro g_claim 1
  @@(3)
  inc  claim_ctr
  lea  rax, [rhp+24*%1]
  cmp  rax, rhl
  jle  %%ok
  call _gc_collect
  lea  rax, [rhp+24*%1]
  cmp  rax, rhl
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
  @@(1)
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
%macro g_slct 1
  @@(2)
  pop rax
  g_assert_tag rax, TAG_CON, MSG_BAD_TAG_SELECT
  push qword [rax+8*%1]
%endmacro

%define g_head g_slct 1
%define g_tail g_slct 2


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
  g_assert_int rax
  mov  rcx, [rax+8]
  pop  rax
  g_assert_int rax
  %1   rcx, [rax+8]
  push rhp
  g_create TAG_INT, rcx, NULL
%endmacro

%define g_add g_binop add
%define g_sub g_binop sub
%define g_mul g_binop imul

%macro g_divop 1                ; %1: rax = quotient, rdx = remainder
  @@(6)
  g_claim 1
  pop  rcx
  g_assert_int rcx
  mov  rax, [rcx+8]
  cqo
  pop  rcx
  g_assert_int rcx
  idiv qword [rcx+8]
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
  g_assert_int rax
  mov  rbx, [rax+8]
  pop  rax
  g_assert_int rax
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


;; Jumps
%macro g_jump 1
  @@(1)
  jmp  %1
%endmacro

%macro g_jumpzero 1
  @@(3)
  pop  rax
  g_assert_tag rax, TAG_CON, MSG_BAD_TAG_JUMPZERO
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
  jmp _return
%endmacro


section .text

;; "Main loop" of the evaluation
_main:
  push rbx
  push rbp
  lea  rhp, [__heap__middle__]
  lea  rhl, [__heap__end__]
  ; initialize the gc stats
  mov  qword [rhl], 0
  sub  [rhl], rhp
  mov  qword [rhl+8], 0

  g_install_cafs
  g_install_main
  mov  rbp, rsp
  lea  rax, [__stack_start__]
  mov  [rax], rbp
  g_eval
  lea  rdi, [STATS_MESSAGE]
  mov  rsi, step_ctr
  lea  rax, [__heap__end__]
  mov  r8, [rax+8]
  mov  rax, [rax]
  add  rax, rhp
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
  g_assert_int rbx
  mov  rax, [rbx+8]
  pop  rbp
  pop  rbx
  ret

;; clean up this mess
_eval:
  @@(3)
  mov  rax, [rsp+8]
  g_assert_good_tag rax, MSG_BAD_TAG_EVAL
  cmp  byte [rax], TAG_APP      ; assumes little endian
  je   .app
  @@(2)
  cmp  word [rax], TAG_FUN      ; the tag *word* is TAG_FUN only for CAFs
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
  ; control goes on to _unwind intensionally

_unwind:
  @@(3)
  mov  rax, [rsp]
  g_assert_good_tag rax, MSG_BAD_TAG_UNWIND
  cmp  byte [rax], TAG_APP      ; assumes little endian
  jne  .done
  @@(2)
  push qword [rax+8]
  jmp _unwind
.done:
  @@(2)
  cmp  byte [rax], TAG_FUN     ; assumes little endian
  jne  _return.no_pop
  @@(4)
  ; calc where rbp would need to be for a full application
  ; if rbp is smaller, we have a partial application
  movzx rbx, byte [rax+1]
  lea   rcx, [rsp+8*rbx]        ; TODO: test that this works correctly
  cmp   rbp, rcx
  jl    _return
  @@(1)
  jmp   [rax+8]

_return:
  @@(1)
  mov  rsp, rbp
.no_pop:
  @@(3)
  pop  rax
  pop  rbp
  ret

_fail:
  g_pre_call_c
  call _puts
  mov  rdi, 1
  call _exit

;; TODO:
;; * change _gc_copy so that we take advantage of its only use case
%define rhs r8
%define rhe r9

;; copies a cell into the to-space
;; expectes a pointer to the cell to copy in rax
;; returns the pointer to the new cell also in rax
;; modifies rax and rsi
_gc_copy:
  ; if rax doesn't point into the heap, we don't need to do anything
  ; in particular, we return the null pointer unchanged
  ; TODO: we should check for the bounds of from-space, not the entire heap
  @@(2)
  cmp   rax, rhs
  jl    .return
  @@(2)
  cmp   rax, rhe
  jge   .return
  ; if rax points to a forwarding cell, we need to follow the redirection
  @@(2)
  cmp   byte [rax], TAG_FWD
  je    .forward
  ; in all other cases, we need to copy
  @@(8)
  mov   rsi, rax
  mov   rax, rdi
  movsq
  movsq
  movsq
  mov   qword [rsi-24], TAG_FWD
  mov   [rsi-16], rax
  ret
.forward:
  @@(1)
  mov rax, [rax+8]
.return:
  @@(1)
  ret

;; copies all cells the cell being pointed to by rax points to
;; into the to-space
;; modifies rbx and rsi; leaves rax unchanged
_gc_follow:
  g_assert_good_tag rax, MSG_BAD_TAG_GC
  @@(2)
  cmp  byte [rax], TAG_APP
  je   .follow
  @@(2)
  cmp  byte [rax], TAG_CON
  je   .follow
  @@(1)
  ret
.follow:
  @@(9)
  mov  rbx, rax
  mov  rax, [rbx+8]
  call _gc_copy
  mov  [rbx+8], rax
  mov  rax, [rbx+16]
  call _gc_copy
  mov  [rbx+16], rax
  mov  rax, rbx
  ret

;; assumes rcx <= rdx
;; modifies rax, rcx and rsi
;; leaves rdx unchanged
_gc_stack_frame:
  @@(6)
  mov  rax, [rcx]
  call _gc_copy
  mov  [rcx], rax
  add  rcx, 8
  cmp  rcx, rdx
  jle  _gc_stack_frame
  @@(1)
  ret


_gc_collect:
  @@(4)
  lea  rhs, [__heap__start__]
  lea  rhe, [__heap__end__]
  add  [rhe], rhp               ; update the number of allocations
  inc  qword [rhe+8]            ; update the number of gc runs

  cmp  rhl, rhe
  je   .move_back
  ; we need to move from first to second half
  @@(3)
  lea  rhp, [__heap__middle__]
  lea  rhl, [__heap__end__]
  jmp  .setup_done
.move_back:
  ; we need to move from second to first half
  @@(2)
  lea  rhp, [__heap__start__]
  lea  rhl, [__heap__middle__]
.setup_done:
  @@(4)
  cld
  mov  rdi, rhp

  ; follow caf cells (we assume there is always at least one, e.g., main)
  lea  rax, [__cafs__start__]
  lea  rdx, [__cafs__end__]
.cafs_loop:
  @@(4)
  call _gc_follow
  add  rax, 24
  cmp  rax, rdx
  jl   .cafs_loop

  ; copy everything immediately reachable from the current stack frame
  @@(3)
  lea  rcx, [rsp+8]             ; we need to skip the return address
  mov  rdx, rbp
  call _gc_stack_frame

  ; copy everything immediately reachable from old stack frames
.stack_loop:
  ; TODO: check if we can move this before the loop
  @@(3)
  lea  rbx, [__stack_start__]
  cmp  rdx, [rbx]
  jge  .stack_done
  @@(4)
  lea  rcx, [rdx+24]
  mov  rdx, [rdx+8]
  call _gc_stack_frame
  jmp .stack_loop
.stack_done:

  ; close the gap between rhp and rdi
  @@(1)
  mov  rax, rhp
.follow_loop:
  @@(2)
  cmp  rax, rdi
  jge  .finish
  @@(3)
  call _gc_follow
  add  rax, 24
  jmp  .follow_loop

.finish:
  @@(3)
  mov  rhp, rdi
  sub  [rhe], rhp
  ret


section .data
align 8
FORMAT_PRINT:
  db "%ld", 10, 0
FORMAT_DEBUG:
  db "TAG : 0x%016lx", 10, "ARG1: 0x%016lx", 10, "ARG2: 0x%016lx", 10, 0
STATS_MESSAGE:
  db 10, \
  "Steps  : %10ld", 10, \
  "Allocs : %10ld          (Checks: %10ld)", 10, \
  "GC runs: %10ld", 10, 0
HEAP_EXHAUSTED:
  db "HEAP EXHAUSTED", 10, 0
MISSING_ARGUMENTS:
  db "MISSING ARGUMENTS", 10, 0
MSG_BAD_TAG_SELECT:
  db "SELECT ON NON-CONS NODE", 10, 0
MSG_BAD_TAG_JUMPZERO:
  db "JUMPZERO ON NON-CONS NODE", 10, 0
MSG_BAD_TAG_EVAL:
  db "BAD TAG IN EVAL", 10, 0
MSG_BAD_TAG_UNWIND:
  db "BAD TAG IN UNWIND", 10, 0
MSG_BAD_TAG_GC:
  db "BAD TAG IN GC", 10, 0
MSG_EXPECTED_INT:
  db "EXPECTED INT TAG", 10, 0
ABORT_CALLED:
  db "ABORT CALLED", 10, 0

;; Heap
section .bss
align 8
__stack_start__:
  resq 1
__heap__start__:
  resq HEAP_SIZE / 16
__heap__middle__:
  resq HEAP_SIZE / 16
__heap__end__:
  ; the gc stores the number of allocations in the 1st qword and the number
  ; of gc runs in the 2nd qword
  resq 2
