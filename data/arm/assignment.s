.global main
main:
  push {fp, lr}
  mov fp, sp
  push {r0, r1, r2, r3}
  ldr r0, =5
  push {r0, ip}
  ldr r0, =1
  push {r0, ip}
  ldr r0, [fp, #-24]
  pop {r1, ip}
  add r0, r0, r1
  push {r0, ip}
  ldr r0, =6
  pop {r1, ip}
  cmp r0, r1
  moveq r0, #1
  movne r0, #0
  cmp r0, #1
  moveq r0, #'.'
  movne r0, #'F'
  bl putchar
  ldr r0, =7
  str r0, [fp, #-24]
  ldr r0, =1
  push {r0, ip}
  ldr r0, [fp, #-24]
  pop {r1, ip}
  add r0, r0, r1
  push {r0, ip}
  ldr r0, =8
  pop {r1, ip}
  cmp r0, r1
  moveq r0, #1
  movne r0, #0
  cmp r0, #1
  moveq r0, #'.'
  movne r0, #'F'
  bl putchar
  mov sp, fp
  mov r0, #0
  pop {fp, pc}
