#!/bin/sh -x

DEPTH=128
WIDTH=144

sed -e s/depth/${DEPTH}/ -e s/width/${WIDTH}/ < cdp_lamb_1r1w1c.sv > cdp_lamb_1r1w1c_${DEPTH}d_${WIDTH}b.sv
sed -e s/depth/${DEPTH}/ -e s/width/${WIDTH}/ < cdp_lamb_1r1w2c.sv > cdp_lamb_1r1w2c_${DEPTH}d_${WIDTH}b.sv

#vcs -gui -o bench_1 -sverilog bench_1c.sv 
vcs -debug_access+r -o bench_2 -sverilog bench_2c.sv 

wait
