#!/bin/sh -x

rm -rf inv_out

time ../AMD64_LINUX/spiceanalyze -f inv.spi -p power.txt -o inv_out

scp -r inv_out mika@10.0.0.8:
