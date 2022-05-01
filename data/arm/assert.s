.global main
main:
  push {fp, lr}
  ldr r0, =1.0
  cmp r0, #1
  moveq r0, #'.'
  movne r0, #'F'
  bl putchar
  mov r0, #0
  pop {fp, pc}
