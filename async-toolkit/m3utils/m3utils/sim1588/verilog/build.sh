#!/bin/sh -x
#vcs -sverilog sim/tsu_tb.sv -debug_all -top tsu_tb_marker -timescale=1fs/1fs
vcs -sverilog sim/tsu_tb_1.sv -debug_all -top tsu_tb_marker -timescale=1fs/1fs
