
.global main
main:
  push {fp, lr}
  mov fp, sp
  push {r0, r1, r2, r3}
  ldr r0, =1
  cmp r0, #0
  beq .L1
  ldr r0, =1
  cmp r0, #1
  moveq r0, #'.'
  movne r0, #'F'
  bl putchar
  b .L2
.L1:
  ldr r0, =0
  cmp r0, #1
  moveq r0, #'.'
  movne r0, #'F'
  bl putchar
.L2:
  ldr r0, =0
  cmp r0, #0
  beq .L3
  ldr r0, =0
  cmp r0, #1
  moveq r0, #'.'
  movne r0, #'F'
  bl putchar
  b .L4
.L3:
  ldr r0, =1
  cmp r0, #1
  moveq r0, #'.'
  movne r0, #'F'
  bl putchar
.L4:
  mov sp, fp
  mov r0, #0
  pop {fp, pc}
