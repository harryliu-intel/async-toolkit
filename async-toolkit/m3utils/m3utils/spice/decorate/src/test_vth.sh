#!/bin/sh -x
#
../AMD64_LINUX/spicedecorate -i test.sp -root adder_tb_Width32_MaxCarryChain31_AdderType2 -noprobe -S vth.scm -modify M '(modify-mos-vth -0.20 +0.20)' -o test_vth__out.sp
