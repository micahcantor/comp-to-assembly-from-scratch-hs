.global main
main:
  push {fp, lr}
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
  mov r0, #0
  pop {fp, pc}
