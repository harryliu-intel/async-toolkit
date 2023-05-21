#!/bin/sh -x 

/bin/rm -rf first_lvt
../AMD64_LINUX/varopt -T ../../varosc/src/circuit_lvt.sp -skip -firstonly -lvt -r first_lvt 
