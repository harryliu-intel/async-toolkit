#!/bin/sh

DEVROOT=~/p4

fulcrum vcs vcs -full64 -timescale=1ns/1ps -sverilog -debug_all +access+rwc+notimingchecks+nospecify +define+PRS2VERILOG_TAU=0.050\
${DEVROOT}/verilog/vendor/avago/stdlib/av28m_svt.v \
${DEVROOT}/verilog/vendor/avago/stdlib/av28m_svt.v \
${DEVROOT}/verilog/lib/sram/enable_new_ecc.v \
${DEVROOT}/verilog/lib/sram/new_ecc.v \
${DEVROOT}/verilog/unittest/lib/sram/ecc_range_tb.sv
