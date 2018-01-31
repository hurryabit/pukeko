;; -*-NASM-*-
;; G-Machine runtime
global _main
extern _exit
extern _getchar
extern _printf
extern _putchar
extern _scanf
extern _gc_init
extern _gc_collect
extern _gc_stats
default rel

%define NULL 0

%define TAG_NIX 0
%define TAG_FWD 1
%define TAG_APP 2
%define TAG_INT 3
%define TAG_FUN 4
%define TAG_CON 128

%define TAG_MIN_GOOD TAG_APP
%define TAG_MAX_GOOD TAG_FUN

%define MAX_CON 32

%define CAF_SIZE 3

%define TAG_ARITY_SIZE(tag, arity, size) tag | (arity << 8) | (size << 32)

;; HEADER LAYOUT (little endian):
;;
;; +--------+--------+--------+--------+--------+--------+--------+--------+
;; |        |        |        |        |        |        |        |        |
;; |  TAG   | ARITY  |  ----  |  ----  |  SIZE  |  SIZE  |  SIZE  |  SIZE  |
;; |        |        |        |        |        |        |        |        |
;; +--------+--------+--------+--------+--------+--------+--------+--------+

;; Stats stuff
%define step_ctr r14
%define claim_ctr r15

%ifndef NOATAT
%define @@(n) add step_ctr, n
%else
%define @@(n)
%endif

;; The heap pointer and the heap limit
%define rhp r12
%define rhl r13

;; Auxiliary macros
%macro g_pre_call_c 0
  mov  rbx, rsp                 ; align stack ptr
  and  rbx, 0x0F                ; (dito)
  sub  rsp, rbx                 ; (dito)
%endmacro

%macro g_post_call_c 0
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
  mov  rsi, [%1]
  g_fail %3
%%ok:
%endif
%endmacro

%macro g_assert_int 1
  g_assert_tag %1, TAG_INT, MSG_EXPECTED_INT
%endmacro

%macro g_assert_con 3           ; %1 = reg with ptr, %2 = num of constructors, %3 = error msg
%ifdef DEBUG
  cmp  byte [%1], TAG_CON
  jb   %%bad
  cmp  byte [%1], TAG_CON + %2
  jae  %%bad
  jmp  %%ok
%%bad:
  mov  rsi, [%1]
  g_fail %3
%%ok:
%endif
%endmacro

%macro g_assert_good_tag 2     ; %1 = reg with ptr, %2 = error msg
%ifdef DEBUG
  cmp  byte [%1], TAG_MIN_GOOD
  jb   %%bad
  cmp  byte [%1], TAG_MAX_GOOD
  ja   %%bad
  jmp  %%ok
%%bad:
  g_assert_con %1, MAX_CON, %2
%%ok:
%endif
%endmacro

%macro g_assert_good_tag_or_fwd 2
%ifdef DEBUG
  cmp  byte [%1], TAG_FWD
  je   %%ok
  g_assert_good_tag %1, %2
%%ok:
%endif
%endmacro

;; Heap operations
%macro g_claim 1
  @@(4)
  inc  claim_ctr
  lea  rax, [rhp+8*%1]
  cmp  rax, rhl
  jbe  %%ok
  mov  rsi, %1
  call _gc
  %%ok:
%endmacro

;; TODO: do 'push rhp' here
%macro g_create 0-*             ; expects TAG_ARITY_SIZE(...) in rbx
  @@(3*%0+1)
  mov  qword [rhp], rbx
%assign idx 1
%rep %0
  mov  qword [rhp+8*idx], %1
%assign idx idx+1
%rotate 1
%endrep
  add  rhp, 8*(%0+1)
%endmacro

%macro g_create_int 1           ; %1 must not be rbx
  @@(1)
  mov  qword rbx, TAG_ARITY_SIZE(TAG_INT, 0, 2)
  g_create %1
%endmacro

;; TODO: make this faster
%macro g_alloc 1
  @@(4*%1)
  g_claim 3*%1
%rep %1
  push rhp
  mov  qword rbx, TAG_ARITY_SIZE(TAG_NIX, 0, 2)
  g_create 0, 0
%endrep
%endmacro

;; CAF related stuff
%define CAF(label) __caf__ %+ label

%macro g_declare_cafs 1-*
section .data
  align 8
__cafs__start__:
%rep %0
  CAF(%1):
  dq TAG_ARITY_SIZE(TAG_FUN, 0, CAF_SIZE)
  times (CAF_SIZE-1) dq 0
%rotate 1
%endrep
__cafs__end__:

section .text
__install__cafs__:
%rep %0
  lea  rax, [CAF(%1)+8]
  lea  rbx, [%1]
  mov  [rax], rbx
%rotate 1
%endrep
  ret
%endmacro

%macro g_install_cafs 0
  call __install__cafs__
%endmacro

%macro g_pushcaf 1
  @@(4)
  lea  rax, [CAF(%1)]
  push rax
%endmacro

%macro g_declare_main 1
__install__main__:
  pop  rbx
  g_pushcaf %1
  jmp  rbx
%endmacro

%macro g_install_main 0
  call __install__main__
%endmacro


;; Stack control
%macro g_globstart 2            ; %1 = name, % = arity
%1:
%ifdef DEBUG
  mov  rax, rbp
  sub  rax, rsp
  cmp  rax, 8*%2
  jae  %%ok
  g_fail MISSING_ARGUMENTS
%%ok:
%endif
  @@(%2*7)
%assign idx 0
%rep %2
  mov  rax, [rsp+8+8*idx]
  mov  rax, [rax+16]
  mov  [rsp+8*idx], rax
%assign idx idx+1
%endrep
%endmacro

%macro g_push 1
  @@(3)
  push qword [rsp+8*%1]
%endmacro

%macro g_pop 1
%if %1 = 0
%fatal "g_pop 0 is invalid"
%else
  @@(1)
  add rsp, 8*%1
%endif
%endmacro

%macro g_slide 1
  @@(6)
  pop  rax
  add  rsp, 8*%1
  push rax
%endmacro

;; TODO: Update inplace if space is sufficient.
%macro g_update 1
%if %1 = 0
%fatal "g_update 0 is invalid"
%else
  @@(13)
  pop  rax
  g_assert_good_tag_or_fwd rax, MSG_BAD_TAG_UPDATE
  mov  rbx, [rsp+8*(%1-1)]
  mov  [rsp+8*(%1-1)], rax
  mov  byte [rbx], TAG_FWD
  mov  [rbx+8], rax
%endif
%endmacro


;; Node constructors
%macro g_pushint 1
  @@(3)
  g_claim 2
  push rhp
  g_create_int %1
%endmacro

%macro g_pushglobal 2           ; %1 = label, %2 = arity
  %if %2 = 0
    g_pushcaf %1
  %else
    @@(5)
    g_claim 2
    lea  rax, [%1]
    push rhp
    mov  qword rbx, TAG_ARITY_SIZE(TAG_FUN, %2, 2)
    g_create rax
  %endif
%endmacro

%macro g_mkap 1
%if %1 = 0
%fatal "g_mkap 0 is invalid"
%else
  @@(11*%1+4)
  g_claim 3*%1
  mov  qword rax, TAG_ARITY_SIZE(TAG_APP, 0, 3)
  mov  qword [rhp], rax
  pop  qword [rhp+ 8]
  pop  qword [rhp+16]
%rep %1 - 1
  mov  qword rax, TAG_ARITY_SIZE(TAG_APP, 0, 3)
  mov  qword [rhp+24], rax
  mov  qword [rhp+32], rhp
  pop  qword [rhp+40]
  add  rhp, 24
%endrep
  push rhp
  add  rhp, 24
%endif
%endmacro

%macro g_updap 2                ; %1 = arity, %2 = offset
%if %1 = 0 || %2 = 0
%fatal "g_updap %1, %2 is invalid"
%elif %1 = 1
  @@(12)
  mov  rax, [rsp+8*(%2+1)]
  mov  qword rbx, TAG_ARITY_SIZE(TAG_APP, 0, 3)
  mov  qword [rax   ], rbx
  pop  qword [rax+ 8]
  pop  qword [rax+16]
%else
  @@(11*%1+3)
  g_claim 3*(%1-1)
  mov  qword rbx, TAG_ARITY_SIZE(TAG_APP, 0, 3)
  mov  qword [rhp   ], rbx
  pop  qword [rhp+ 8]
  pop  qword [rhp+16]
%rep %1 - 2
  mov  qword rbx, TAG_ARITY_SIZE(TAG_APP, 0, 3)
  mov  qword [rhp+24], rbx
  mov  qword [rhp+32], rhp
  pop  qword [rhp+40]
  add  rhp, 24
%endrep
  mov  rax, [rsp+8*%2]
  mov  qword rbx, TAG_ARITY_SIZE(TAG_APP, 0, 3)
  mov  qword [rax   ], rbx
  mov  qword [rax+ 8], rhp
  pop  qword [rax+16]
  add  rhp, 24
%endif
%endmacro

%macro g_cons 2                 ; %1 = tag, %2 = arity
  @@(4+3*%2+4)
%if %2 = 0
%assign size 2
%else
%assign size 1+%2
%endif
  g_claim size
  mov  qword rax, TAG_ARITY_SIZE(TAG_CON + %1, %2, size)
  mov  qword [rhp], rax
%assign idx 0
%rep %2
%assign idx idx+1
  pop  qword [rhp+8*idx]
%endrep
  push rhp
  add  rhp, 8*size
%endmacro

%macro g_updcons 3              ; %1 = tag, %2 = arity, %3 = offset
%if %3 = 0
  %fatal "g_updcons %1, %2, %3 is invalid"
%elif %2 > 2
  g_cons %1, %2
  g_update %3
%else
%if %2 = 0
%assign size 2
%else
%assign size 1+%2
%endif
  @@(6+3*%2)
  mov  rax, [rsp+8*(%2+%3-1)]
  mov  qword rbx, TAG_ARITY_SIZE(TAG_CON + %1, %2, size)
  mov  qword [rax], rbx
%assign idx 0
%rep %2
%assign idx idx+1
  pop  qword [rax+8*idx]
%endrep
%endif
%endmacro

;; TODO: Specialize case %1 = 0 if speed matters.
%macro g_uncons 1
  @@(2+3*%1)
  pop rax
  g_assert_con rax, MAX_CON, MSG_BAD_TAG_UNCONS
%assign idx %1
%rep %1
  push qword [rax+8*idx]
%assign idx idx - 1
%endrep
%endmacro


;; Arithmetic
%macro g_neg 0
  @@(8)
  g_claim 2
  pop rax
  g_assert_int rax
  mov rcx, [rax+8]
  neg rcx
  push rhp
  g_create_int rcx
%endmacro

%macro g_binop 1
  @@(11)
  g_claim 2
  pop  rax
  g_assert_int rax
  mov  rcx, [rax+8]
  pop  rax
  g_assert_int rax
  %1   rcx, [rax+8]
  push rhp
  g_create_int rcx
%endmacro

%define g_add g_binop add
%define g_sub g_binop sub
%define g_mul g_binop imul

%macro g_divop 1                ; %1: rax = quotient, rdx = remainder
  @@(12)
  g_claim 2
  pop  rcx
  g_assert_int rcx
  mov  rax, [rcx+8]
  cqo
  pop  rcx
  g_assert_int rcx
  idiv qword [rcx+8]
  push rhp
  g_create_int %1
%endmacro

%define g_div g_divop rax
%define g_mod g_divop rdx


;; Comparison relations
%macro g_relop 1                ; %1 is some setCC instruction
  @@(20)
  g_claim 2
  pop  rax
  g_assert_int rax
  mov  rbx, [rax+8]
  pop  rax
  g_assert_int rax
  mov  rcx, [rax+8]
  mov  rax, 0
  cmp  rbx, rcx
  %1   al
  mov  qword rbx, TAG_ARITY_SIZE(TAG_CON, 0, 2)
  add  rax, rbx
  push rhp
  mov  [rhp], rax
  add  rhp, 16
%endmacro

%define g_les g_relop setl
%define g_leq g_relop setle
%define g_eqv g_relop sete
%define g_neq g_relop setne
%define g_geq g_relop setge
%define g_gtr g_relop setg


;; Character conversion
%macro g_chr 0
  @@(8)
  g_claim 2
  pop rax
  g_assert_int rax
  mov rcx, [rax+8]
  and rcx, 0xFF
  push rhp
  g_create_int rcx
%endmacro

%macro g_ord 0
%endmacro


;; I/O
%macro g_print 0
  @@(5)
  pop  rax
  g_assert_int rax
  g_pre_call_c
  lea  rdi, [FORMAT_PRINT]
  mov  rsi, [rax+8]
  mov  rax, 0
  call _printf
  g_post_call_c
%endmacro

%macro g_input 0
  @@(5)
  g_claim 2
  g_pre_call_c
  lea  rdi, [FORMAT_INPUT]
  lea  rsi, [rhp+8]
  mov  rax, 0
  call _scanf
  g_post_call_c
  cmp  rax, 1
  je   %%ok
  g_fail MSG_BAD_INPUT
%%ok:
  @@(7)
  push rhp
  mov  qword rax, TAG_ARITY_SIZE(TAG_INT, 0, 2)
  mov  qword [rhp], rax
  add  rhp, 16
%endmacro

%macro g_putc 0
  @@(2)
  pop  rax
  g_assert_int rax
  g_pre_call_c
  mov  rdi, [rax+8]
  call _putchar
  g_post_call_c
%endmacro

%macro g_getc 0
  g_claim 2
  g_pre_call_c
  call _getchar
  g_post_call_c
  movsx rax, eax                 ; getchar returns an int32 and we care about the sign
  push rhp
  g_create_int rax
%endmacro

;; Jumps
%macro g_jump 1
  @@(1)
  jmp  %1
%endmacro

; %macro g_jumpzero 1
;   @@(4)
;   mov  rax, [rsp]
;   g_assert_con rax, 2, MSG_BAD_TAG_JUMPZERO
;   cmp  byte [rax], TAG_CON
;   je   %1
; %endmacro

;; TODO: Be more efficient for small numbers of cases
%macro g_jumpcase 2-MAX_CON
  @@(8)
  mov   rax, [rsp]
  g_assert_con rax, %0, MSG_BAD_TAG_JUMPCASE
  movzx rax, byte [rax]
  lea   rbx, [%%jump_table]
  jmp   [rbx+8*(rax-TAG_CON)]
section .data
align 8
%%jump_table:
%rep %0
  dq   %1
%rotate 1
%endrep
section .text
%endmacro

%macro g_label 1
%1:
%endmacro

;; Evaluation control
%macro g_abort 0
  g_fail ABORT_CALLED
%endmacro

%macro g_eval 0
  @@(3)
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
  ; set up the stack
  push rbx
  push rbp
  lea  rbp, [rsp-8]

  ; set up the gc info
  lea  rdi, [__gc_info__]
  mov  [rdi], rbp
  lea  rax, [__cafs__start__]
  mov  [rdi+24], rax
  lea  rax, [__cafs__end__]
  mov  [rdi+32], rax
  mov  qword [rdi+40], HEAP_SIZE
  lea  rax, [__heap_start__]
  mov  [rdi+48], rax
  mov  qword [rdi+88], 0
  mov  qword [rdi+96], 0
  g_pre_call_c
  call _gc_init
  g_post_call_c

  ; set up the heap ptr
  lea  rdi, [__gc_info__]
  mov  rhp, [rdi+72]
  mov  rhl, [rdi+80]

  g_install_cafs
  g_cons 0, 0
  g_install_main
  g_mkap 1
  g_eval
  g_uncons 2
  g_eval
  pop  rax
  g_assert_con rax, 1, MSG_BAD_TAG_EXIT
  pop  rax
  g_assert_con rax, 1, MSG_BAD_TAG_EXIT

  ; set up the gc info for the final stats
  g_pre_call_c
  lea  rdi, [__gc_info__]
  mov  [rdi+72], rhp
  mov  rsi, step_ctr
  mov  rdx, claim_ctr
  call _gc_stats
  g_post_call_c

  ; exit
  pop  rbp
  pop  rbx
  mov  rax, 0
  ret

%macro g_fwd_compress 1
  @@(5)
  mov  rax, %1
  mov  rbx, rax
  cmp  byte [rax], TAG_FWD
  jne  %%done
%%follow_loop:
  @@(4)
  mov  rax, [rax+8]
  cmp  byte [rax], TAG_FWD
  je   %%follow_loop
%%update_loop:
  @@(8)
  mov  rcx, [rbx+8]
  mov  [rbx+8], rax
  mov  rbx, rcx
  cmp  byte [rbx], TAG_FWD
  je   %%update_loop
  @@(3)
  mov  %1, rax
%%done:
%endmacro

_eval:
  g_fwd_compress [rsp+8]
  g_assert_good_tag rax, MSG_BAD_TAG_EVAL
  @@(2)
  cmp  byte [rax], TAG_APP
  je   .app
  @@(2)
  cmp  word [rax], TAG_FUN      ; the tag *word* is TAG_FUN only for CAFs
  je   .caf
  @@(2)
  ret
.caf:
  @@(9)
  push rbp
  push rax
  mov  rbp, rsp
  jmp  [rax+8]
.app:
  @@(7)
  push rbp
  push rax
  mov  rbp, rsp
  jmp  _unwind.no_fwd_compress

_unwind:
  g_fwd_compress [rsp]
  g_assert_good_tag rax, MSG_BAD_TAG_UNWIND
.no_fwd_compress:
  @@(2)
  cmp  byte [rax], TAG_APP
  jne  .done
  @@(4)
  push qword [rax+8]
  jmp  _unwind
.done:
  @@(2)
  cmp  byte [rax], TAG_FUN      ; assumes little endian
  jne  _return.no_pop
  @@(6)
  ; calc where rbp would need to be for a full application
  ; if rbp is smaller, we have a partial application
  movzx rbx, byte [rax+1]
  lea   rcx, [rsp+8*rbx]
  cmp   rbp, rcx
  jb    _return
  @@(2)
  jmp   [rax+8]

_return:
  @@(1)
  mov  rsp, rbp
.no_pop:
  @@(9)
  pop  rax
  g_assert_good_tag rax, MSG_BAD_TAG_RETURN
  pop  rbp
  mov  [rsp+8], rax
  ret

_fail:
  g_pre_call_c
  mov  rax, 0
  call _printf
  mov  rdi, 1
  call _exit

_gc:
  ; set up gc info
  lea  rdi, [__gc_info__]
  mov  [rdi+8], rbp
  lea  rax, [rsp+8]             ; skip the ret addr
  mov  [rdi+16], rax
  mov  [rdi+72], rhp
  g_pre_call_c
  call _gc_collect
  g_post_call_c

  ; set new heap ptr and limit
  lea  rdi, [__gc_info__]
  mov  rhp, [rdi+72]
  mov  rhl, [rdi+80]
  ret


section .data
align 8
FORMAT_PRINT:
  db "%lld", 10, 0
FORMAT_INPUT:
  db "%lld", 0
FORMAT_DEBUG:
  db "TAG : 0x%016llx", 10, "ARG1: 0x%016llx", 10, "ARG2: 0x%016llx", 10, 0
MISSING_ARGUMENTS:
  db "MISSING ARGUMENTS", 10, 0
MSG_BAD_INPUT:
  db "INVALID INPUT", 10, 0
MSG_BAD_TAG_UNCONS:
  db "BAD TAG IN UNCONS: #%016llx", 10, 0
MSG_BAD_TAG_JUMPCASE:
  db "BAD TAG IN JUMPCASE: #%016llx", 10, 0
MSG_BAD_TAG_EXIT:
  db "BAD TAG AT EXIT: #%016llx", 10, 0
MSG_BAD_TAG_UPDATE:
  db "BAD TAG IN UPDATE: #%016llx", 10, 0
MSG_BAD_TAG_EVAL:
  db "BAD TAG IN EVAL: #%016llx", 10, 0
MSG_BAD_TAG_UNWIND:
  db "BAD TAG IN UNWIND: #%016llx", 10, 0
MSG_BAD_TAG_RETURN:
  db "BAD TAG IN RETURN: #%016llx", 10, 0
MSG_EXPECTED_INT:
  db "EXPECTED INT TAG", 10, 0
ABORT_CALLED:
  db "ABORT CALLED", 10, 0

;; Heap
section .bss
align 8
__heap_start__:
  resb HEAP_SIZE

align 8
__gc_info__:
  resq 1                        ; stack start
  resq 1                        ; base ptr
  resq 1                        ; stack ptr
  resq 1                        ; cafs start
  resq 1                        ; cafs end
  resq 1                        ; heap size
  resq 1                        ; heap start
  resq 1                        ; heap middle
  resq 1                        ; heap end
  resq 1                        ; heap ptr
  resq 1                        ; heap limit
  resq 1                        ; num of allocs
  resq 1                        ; num of gc runs
