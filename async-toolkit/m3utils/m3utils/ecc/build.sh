#!/bin/sh

cm3 -x || exit

#DBITS=26 # ok
#DBITS=11 # ok
#DBITS=1  # ok
#DBITS=4  # ok
#DBITS=128
#DBITS=57 # ok
DBITS=1013 # ok

#MAXFANIN=2 
#MAXFANIN=6 
MAXFANIN=4 
OPTS=-syntactic
#OPTS=-structural

AMD64_LINUX/buildecc -w -m write_ecc -d ${DBITS} -maxfanin ${MAXFANIN} ${OPTS} -defs ecc_defs.v
AMD64_LINUX/buildecc -r -m read_ecc  -d ${DBITS} -maxfanin ${MAXFANIN} ${OPTS}  

fulcrum vcs vcs -full64 -timescale=1ns/1ps -sverilog -debug_all +access+rwc+notimingchecks+nospecify +define+PRS2VERILOG_TAU=0.050\
/nfs/site/home/mnystrox/p4/verilog/vendor/avago/stdlib/av28m_svt.v \
~/p4/verilog/vendor/avago/stdlib/av28m_svt.v \
write_ecc.sv \
read_ecc.sv \
ecc_tb.sv
