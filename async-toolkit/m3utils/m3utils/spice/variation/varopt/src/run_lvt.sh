#!/bin/sh -x 

/bin/rm -rf run_lvt
../AMD64_LINUX/varopt -T ../../varosc/src/circuit_lvt.sp -skip -lvt -r run_lvt |& tee run_opt_lvt.0
