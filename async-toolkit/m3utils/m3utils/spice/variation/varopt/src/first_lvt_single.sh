#!/bin/sh -x
/bin/rm -rf first_lvt_single

../AMD64_LINUX/varopt -single -skip -firstonly -T ../../varosc/src/circuit_lvt.sp -lvt -r first_lvt_single
