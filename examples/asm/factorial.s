.global main
main:
  push {fp, lr}
  mov fp, sp
  push {r0, r1, r2, r3}
  ldr r0, =5
  bl factorial
  push {r0, ip}
  ldr r0, =120
  pop {r1, ip}
  cmp r1, r0
  moveq r0, #1
  movne r0, #0
  bl assert
  mov sp, fp
  mov r0, #0
  pop {fp, pc}

.global factorial
factorial:
  push {fp, lr}
  mov fp, sp
  push {r0, r1, r2, r3}
  ldr r0, [fp, #-16]
  push {r0, ip}
  ldr r0, =1
  pop {r1, ip}
  cmp r1, r0
  moveq r0, #1
  movne r0, #0
  cmp r0, #0
  beq .L1
  ldr r0, =1
  mov sp, fp
  pop {fp, pc}
  b .L2
.L1:
  ldr r0, [fp, #-16]
  push {r0, ip}
  ldr r0, [fp, #-16]
  push {r0, ip}
  ldr r0, =1
  pop {r1, ip}
  sub r0, r1, r0
  bl factorial
  pop {r1, ip}
  mul r0, r1, r0
  mov sp, fp
  pop {fp, pc}
.L2:
  mov sp, fp
  mov r0, #0
  pop {fp, pc}

.global assert
assert:
  push {fp, lr}
  mov fp, sp
  push {r0, r1, r2, r3}
  ldr r0, [fp, #-16]
  cmp r0, #0
  beq .L3
  ldr r0, =46
  bl putchar
  b .L4
.L3:
  ldr r0, =70
  bl putchar
.L4:
  mov sp, fp
  mov r0, #0
  pop {fp, pc}
