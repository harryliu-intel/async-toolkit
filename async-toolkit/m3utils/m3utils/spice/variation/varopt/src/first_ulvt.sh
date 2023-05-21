#!/bin/sh -x
/bin/rm -rf first_ulvt

../AMD64_LINUX/varopt -skip -firstonly -T ../../varosc/src/circuit_ulvt.sp -r first_ulvt
