#!/bin/sh

fulcrum vcs vcs -full64 -timescale=1ns/1ps -sverilog -debug_all +access+rwc+notimingchecks+nospecify +define+PRS2VERILOG_TAU=0.050\
/nfs/site/home/mnystrox/p4/verilog/vendor/avago/stdlib/av28m_svt.v \
~/p4/verilog/vendor/avago/stdlib/av28m_svt.v \
eccs/single_file.sv \
ecc2_tb.sv
