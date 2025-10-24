C Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
C SPDX-License-Identifier: Apache-2.0

+libverbose
+notimingcheck
+no_tchk_msg
+vcs+flush+dump
#+access+rwc
+libext+.v+.sv+.svh
+librescan
+lint=TFIPC-L
+lint=PCWM

//use ToolConfig;

//$SHRTL          = &ToolConfig::get_tool_path('ipconfig/shrtl');

+define+SHRTL_LIB_MODEL

+incdir+$SHRTL/src/common/rtl

../../shared/rtl/mby_msh_pkg.sv                
../../shared/rtl/shared_pkg.sv                                                                                  
../../shared/rtl/mby_egr_pkg.sv
$SHRTL/src/common/rtl/shrtl_nbits_pkg.vh                             
$SHRTL/src/fifo/rtl/shrtl_async_flop_fifo.sv
$SHRTL/src/fifo/rtl/shrtl_flop_fifo.sv
$SHRTL/src/common/rtl/shrtl_flop_ram.sv
$SHRTL/src/sync/rtl/shrtl_sync.sv
$SHRTL/src/sync/rtl/shrtl_lib_sync2.sv
$SHRTL/src/fifo/rtl/shrtl_async_fifo_ctrl.sv
../rtl/mby_mim_wr_cdc.sv   
../rtl/mby_mim_wr.sv   
../rtl/mby_mim_rdreq_cdc.sv   
../rtl/mby_mim_rdreq.sv   
../rtl/mby_mim_rdrsp_cdc.sv   
../rtl/mby_mim_rdrsp.sv   
../rtl/mby_mim.sv   
tb_mby_mim.sv   


