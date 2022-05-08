#!/bin/bash

mkdir -p out
arm-none-linux-gnueabihf-gcc -static $1 -o out/$2
qemu-arm-static ./out/$2