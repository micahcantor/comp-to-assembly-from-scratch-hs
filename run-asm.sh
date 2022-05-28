#!/bin/bash

mkdir -p out
arm-linux-gnueabihf-gcc -static $1 -o out/$2
qemu-arm ./out/$2