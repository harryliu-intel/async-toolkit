#!/bin/sh -x
/bin/rm -rf first_ulvt_single

../AMD64_LINUX/varopt -single -skip -firstonly -T ../../varosc/src/circuit_ulvt.sp -r first_ulvt_single
