#/bin/sh -x
cc -O6 -shared -o liba.so a.c
cc testmain.c -ldl -lm
./a.out
