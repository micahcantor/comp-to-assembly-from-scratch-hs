.global main
main:
  push {fp, lr}
  mov fp, sp
  push {r0, r1, r2, r3}

.global factorial
factorial:
  push {fp, lr}
  mov fp, sp
  push {r0, r1, r2, r3}
  ldr r0, =1
  push {r0, ip}
.L1:
  ldr r0, [fp, #-16]
  push {r0, ip}
  ldr r0, =1
  pop {r1, ip}
  cmp r0, r1
  moveq r0, #0
  movne r0, #1
  cmp r0, #0
  beq .L2
  ldr r0, [fp, #-24]
  push {r0, ip}
  ldr r0, [fp, #-16]
  pop {r1, ip}
  mul r0, r0, r1
  str r0, [fp, #-24]
  ldr r0, [fp, #-16]
  push {r0, ip}
  ldr r0, =1
  pop {r1, ip}
  sub r0, r0, r1
  str r0, [fp, #-16]
  b .L1
.L2:
  ldr r0, [fp, #-24]
  mov sp, fp
  pop {fp, pc}
  mov sp, fp
  mov r0, #0
  pop {fp, pc}
  ldr r0, =5
  bl factorial
  push {r0, ip}
  ldr r0, =120
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
