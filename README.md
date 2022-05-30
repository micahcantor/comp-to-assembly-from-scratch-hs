# Compiling to Assembly from Scratch in Haskell

This is the repository for the compiler I built following Vladamir Keleshev's book, [*Compiling to Assembly from Scratch*](http://keleshev.com/compiling-to-assembly-from-scratch).

You can compile an ARM-32 assembly file from a given JavaScript file using `run.sh`:

```shell
> ./run.sh examples/js/factorial.js examples/asm/factorial.s
```

Then you can run that assembly code using `run-asm.sh`:

```shell
> ./run-asm.sh examples/asm/factorial.s factorial
```

The only non-Haskell pre-requisites are a gcc cross compiler for 32-bit ARM, and the qemu emulator. On Debian/Ubuntu install the packages `gcc-arm-linux-gnueabihf` and `qemu-user`. Or you can run the assembly natively on a 32-bit Raspberry Pi device.

A small example of the compiler's output can be found in the `examples` directory. 