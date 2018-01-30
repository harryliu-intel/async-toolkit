#!/bin/sh -x
#vcs -sverilog sim/tsu_tb.sv -debug_all -top tsu_tb_mark -timescale=1fs/1fs
vcs -sverilog sim/tsu_tb_drift.sv -debug_all -top tsu_tb_mark -timescale=1fs/1fs
