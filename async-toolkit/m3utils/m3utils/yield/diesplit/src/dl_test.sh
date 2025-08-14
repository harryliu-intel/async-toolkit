#/bin/sh -x
cc -std=c99 -O6 -shared -o liba.so a.c
cc -std=c99 testmain.c -ldl -lm
./a.out
