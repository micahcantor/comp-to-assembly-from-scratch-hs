.global main
main:
  push {fp, lr}
  ldr r0, =1
  cmp r0, #1
  moveq r0, #'.'
  movne r0, #'F'
  bl putchar
  ldr r0, =0
  cmp r0, #0
  moveq r0, #1
  movne r0, #0
  cmp r0, #1
  moveq r0, #'.'
  movne r0, #'F'
  bl putchar
  mov r0, #0
  pop {fp, pc}
