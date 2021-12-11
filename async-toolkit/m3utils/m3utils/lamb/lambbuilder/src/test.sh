#! /bin/sh -x

../AMD64_LINUX/lambbuilder -w 30 -d 16 -clk 1e9 -prog read -f hspice -pm io -step 1e-11
