///
///  INTEL CONFIDENTIAL
///
///  Copyright 2018 Intel Corporation All Rights Reserved.
///
///  The source code contained or described herein and all documents related
///  to the source code ("Material") are owned by Intel Corporation or its
///  suppliers or licensors. Title to the Material remains with Intel
///  Corporation or its suppliers and licensors. The Material contains trade
///  secrets and proprietary and confidential information of Intel or its
///  suppliers and licensors. The Material is protected by worldwide copyright
///  and trade secret laws and treaty provisions. No part of the Material may
///  be used, copied, reproduced, modified, published, uploaded, posted,
///  transmitted, distributed, or disclosed in any way without Intel's prior
///  express written permission.
///
///  No license under any patent, copyright, trade secret or other intellectual
///  property right is granted to or conferred upon you by disclosure or
///  delivery of the Materials, either expressly, by implication, inducement,
///  estoppel or otherwise. Any license under such intellectual property rights
///  must be express and approved by Intel in writing.
///
// ---------------------------------------------------------------------------------------------------------------------
// -- Author : Jim McCormick <jim.mccormick@intel.com>
// -- Project Name : ??? 
// -- Description  : The top-level-module in this tesbench 
//
// -- Instantiation Hierarchy:                                  definition file
//                                                              ---------------
//          module  top                                         top.sv
//              msh_node_dut_if dut_if             i            msh_node_dut_if.sv
//              msh_node    dut                                 $(CLONE_ROOT)/src/msh/rtl/mby_msh_node.sv
//                  ...                                         (see mby_msh_node.sv)
//              testcase    test                                tests/<testcase>.sv
//                  env     env                                 env.sv
//                      system_driver   sys_drvr                system_driver.sv
//                      configuration   cfg                     configuration.sv
//                      id_generator    id_gen                  ig_generator.sv
//                      inp_driver      inp_driver              inp_driver.sv
//                          stimulus        stim                stimulus.sv
//                      monitor         mntr                    monitor.sv
//                      scoreboard      sb                      scoreboard.sv
//                  `include "test_loop_include"                test_loop_include.sv
//
//  FIXME:  change these to mesh assertions
//          bind tmpl_arb                                       $(CLONE_ROOT)/src/rtl/template/sva/template_binds.svh
//              tmpl_arb_assert a__tmpl_arb_assert              $(CLONE_ROOT)/src/rtl/template/sva/tmpl_arb_asert.sv
//
//  note:  verilog/msh_sim_pkg.sv is included on the compilation command line so that it can be used widely
//         without being included everywhere.
// ---------------------------------------------------------------------------------------------------------------------

// A module is a top-level-module if is defined but never instantiated

module top 
import mby_igr_pkg::*;
import igr_sim_pkg::*;
();

    logic clk = 0;                              // declare clock


    // define clock
    initial                                     // "initial" procedures are executed at the beginning of simulation
        forever                                 // "forever" is just a type of loop that executes forever 
            #(10/2)                             //  #<n> means wait <n> clocks
                clk = ~clk;                     //  invert the clock

    // instantiate DUT interface
    igr_dut_if dut_if (
        .clk(clk)                               // pass clock into this interface to make it part of the interface
    );


    // start firing reset immediately to protect reset guarded assertions
    initial dut_if.rst = 1'b1;

    // instantiate DUT (Design Under Test)

    igr_pre_post_wrap pre_post_wrap(

       // inputs
      .cclk                                    ( dut_if.clk ),
      .rst                                     ( dut_if.rst ),
      .i_shim_pb_data_p0                       ( dut_if.i_shim_pb_data_p0 ),
      .i_shim_pb_data_p1                       ( dut_if.i_shim_pb_data_p1 ),
      .i_shim_pb_data_p2                       ( dut_if.i_shim_pb_data_p2 ),
      .i_shim_pb_data_p3                       ( dut_if.i_shim_pb_data_p3 ),
      .i_shim_pb_md_p0                         ( dut_if.i_shim_pb_md_p0 ),
      .i_shim_pb_md_p1                         ( dut_if.i_shim_pb_md_p1 ),
      .i_shim_pb_md_p2                         ( dut_if.i_shim_pb_md_p2 ),
      .i_shim_pb_md_p3                         ( dut_if.i_shim_pb_md_p3 ),
      .i_shim_pb_v_p0                          ( dut_if.i_shim_pb_v_p0 ),
      .i_shim_pb_v_p1                          ( dut_if.i_shim_pb_v_p1 ),
      .i_shim_pb_v_p2                          ( dut_if.i_shim_pb_v_p2 ),
      .i_shim_pb_v_p3                          ( dut_if.i_shim_pb_v_p3 ),
      .egr_igr_wreq_mim_wreq_valid             ( dut_if.egr_igr_wreq_mim_wreq_valid ),
      .egr_igr_wreq_mim_wr_seg_ptr             ( dut_if.egr_igr_wreq_mim_wr_seg_ptr ),
      .egr_igr_wreq_mim_wr_sema                ( dut_if.egr_igr_wreq_mim_wr_sema ),
      .egr_igr_wreq_mim_wr_wd_sel              ( dut_if.egr_igr_wreq_mim_wr_wd_sel ),
      .egr_igr_wreq_mim_wreq_id                ( dut_if.egr_igr_wreq_mim_wreq_id ),
      .egr_igr_wreq_mim_wr_data                ( dut_if.egr_igr_wreq_mim_wr_data ),

      .igr_rx_ppe_intf0_ack                    ( dut_if.igr_rx_ppe_intf0_ack ),
      .igr_rx_ppe_intf1_ack                    ( dut_if.igr_rx_ppe_intf1_ack ),

      .mim_wreq_0_mim_wreq_credits             ( dut_if.mim_wreq_0_mim_wreq_credits ),
      .mim_wreq_1_mim_wreq_credits             ( dut_if.mim_wreq_1_mim_wreq_credits ),
      .mim_wreq_2_mim_wreq_credits             ( dut_if.mim_wreq_2_mim_wreq_credits ),
      .mim_wreq_3_mim_wreq_credits             ( dut_if.mim_wreq_3_mim_wreq_credits ),
      .mim_wreq_4_mim_wreq_credits             ( dut_if.mim_wreq_4_mim_wreq_credits ),
      .mim_wreq_5_mim_wreq_credits             ( dut_if.mim_wreq_5_mim_wreq_credits ),

      .rx_ppe_igr_intf0                        ( dut_if.rx_ppe_igr_intf1 ),
      .rx_ppe_igr_intf1                        ( dut_if.rx_ppe_igr_intf1 ),

              // outputs
      .igr_rx_ppe_intf0_tail                   ( dut_if.igr_rx_ppe_intf0_tail ),
      .igr_rx_ppe_intf0_head                   ( dut_if.igr_rx_ppe_intf0_head ),
      .igr_rx_ppe_intf1_tail                   ( dut_if.igr_rx_ppe_intf1_tail ),
      .igr_rx_ppe_intf1_head                   ( dut_if.igr_rx_ppe_intf1_head ),

      .egr_igr_wreq_mim_wreq_credits           ( dut_if.egr_igr_wreq_mim_wreq_credits ),

      .mim_wreq_0_mim_wreq_valid               ( dut_if.mim_wreq_0_mim_wreq_valid ),
      .mim_wreq_0_mim_wr_seg_ptr               ( dut_if.mim_wreq_0_mim_wr_seg_ptr ),
      .mim_wreq_0_mim_wr_sema                  ( dut_if.mim_wreq_0_mim_wr_sema ),
      .mim_wreq_0_mim_wr_wd_sel                ( dut_if.mim_wreq_0_mim_wr_wd_sel ),
      .mim_wreq_0_mim_wreq_id                  ( dut_if.mim_wreq_0_mim_wreq_id ),
      .mim_wreq_0_mim_wr_data                  ( dut_if.mim_wreq_0_mim_wr_data ),
      .mim_wreq_1_mim_wreq_valid               ( dut_if.mim_wreq_1_mim_wreq_valid ),
      .mim_wreq_1_mim_wr_seg_ptr               ( dut_if.mim_wreq_1_mim_wr_seg_ptr ),
      .mim_wreq_1_mim_wr_sema                  ( dut_if.mim_wreq_1_mim_wr_sema ),
      .mim_wreq_1_mim_wr_wd_sel                ( dut_if.mim_wreq_1_mim_wr_wd_sel ),
      .mim_wreq_1_mim_wreq_id                  ( dut_if.mim_wreq_1_mim_wreq_id ),
      .mim_wreq_1_mim_wr_data                  ( dut_if.mim_wreq_1_mim_wr_data ),
      .mim_wreq_2_mim_wreq_valid               ( dut_if.mim_wreq_2_mim_wreq_valid ),
      .mim_wreq_2_mim_wr_seg_ptr               ( dut_if.mim_wreq_2_mim_wr_seg_ptr ),
      .mim_wreq_2_mim_wr_sema                  ( dut_if.mim_wreq_2_mim_wr_sema ),
      .mim_wreq_2_mim_wr_wd_sel                ( dut_if.mim_wreq_2_mim_wr_wd_sel ),
      .mim_wreq_2_mim_wreq_id                  ( dut_if.mim_wreq_2_mim_wreq_id ),
      .mim_wreq_2_mim_wr_data                  ( dut_if.mim_wreq_2_mim_wr_data ),
      .mim_wreq_3_mim_wreq_valid               ( dut_if.mim_wreq_3_mim_wreq_valid ),
      .mim_wreq_3_mim_wr_seg_ptr               ( dut_if.mim_wreq_3_mim_wr_seg_ptr ),
      .mim_wreq_3_mim_wr_sema                  ( dut_if.mim_wreq_3_mim_wr_sema ),
      .mim_wreq_3_mim_wr_wd_sel                ( dut_if.mim_wreq_3_mim_wr_wd_sel ),
      .mim_wreq_3_mim_wreq_id                  ( dut_if.mim_wreq_3_mim_wreq_id ),
      .mim_wreq_3_mim_wr_data                  ( dut_if.mim_wreq_3_mim_wr_data ),
      .mim_wreq_4_mim_wreq_valid               ( dut_if.mim_wreq_4_mim_wreq_valid ),
      .mim_wreq_4_mim_wr_seg_ptr               ( dut_if.mim_wreq_4_mim_wr_seg_ptr ),
      .mim_wreq_4_mim_wr_sema                  ( dut_if.mim_wreq_4_mim_wr_sema ),
      .mim_wreq_4_mim_wr_wd_sel                ( dut_if.mim_wreq_4_mim_wr_wd_sel ),
      .mim_wreq_4_mim_wreq_id                  ( dut_if.mim_wreq_4_mim_wreq_id ),
      .mim_wreq_4_mim_wr_data                  ( dut_if.mim_wreq_4_mim_wr_data ),
      .mim_wreq_5_mim_wreq_valid               ( dut_if.mim_wreq_5_mim_wreq_valid ),
      .mim_wreq_5_mim_wr_seg_ptr               ( dut_if.mim_wreq_5_mim_wr_seg_ptr ),
      .mim_wreq_5_mim_wr_sema                  ( dut_if.mim_wreq_5_mim_wr_sema ),
      .mim_wreq_5_mim_wr_wd_sel                ( dut_if.mim_wreq_5_mim_wr_wd_sel ),
      .mim_wreq_5_mim_wreq_id                  ( dut_if.mim_wreq_5_mim_wreq_id ),
      .mim_wreq_5_mim_wr_data                  ( dut_if.mim_wreq_5_mim_wr_data ),
      .o_drop_seg_ptr                          ( dut_if.o_drop_seg_ptr ),
      .o_drop_seg_valid                        ( dut_if.o_drop_seg_valid ),
      .o_drop_sema                             ( dut_if.o_drop_sema ),
      .o_port_id                               ( dut_if.o_port_id ),
      .o_post_ppe_tag_at_rate0                 ( dut_if.o_post_ppe_tag_at_rate0 ),
      .o_post_ppe_tag_at_rate1                 ( dut_if.o_post_ppe_tag_at_rate1 ),
      .o_post_ppe_tag_set_aside0               ( dut_if.o_post_ppe_tag_set_aside0 ),
      .o_post_ppe_tag_set_aside1               ( dut_if.o_post_ppe_tag_set_aside1 )

    );

    // instantiate testcase
    testcase test(
        dut_if                                    // pass interface dut_if into testcase
    );

endmodule

