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

      .i_free_ptr_valid                        ( dut_if.i_free_ptr_valid ),              
      .i_free_seg_ptr                          ( dut_if.i_free_seg_ptr ),                
      .i_free_sema                             ( dut_if.i_free_sema ),                   
      .i_return_id                             ( dut_if.i_return_id ),                   
      .i_return_id_valid                       ( dut_if.i_return_id_valid ),             

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

      .rx_ppe_igr_intf0                        ( dut_if.rx_ppe_igr_intf0 ),
      .rx_ppe_igr_intf1                        ( dut_if.rx_ppe_igr_intf1 ),

              // outputs
      .o_free_ptr_req                          ( dut_if.o_free_ptr_req ),                

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
      .mim_wreq_5_mim_wr_data                  ( dut_if.mim_wreq_5_mim_wr_data )

    );

    rx_ppe_bfm  rx_ppe
      (
      .cclk                                    ( dut_if.clk ),
      .rst                                     ( dut_if.rst ),

      .igr_rx_ppe_intf0_ack                    ( dut_if.igr_rx_ppe_intf0_ack ),
      .igr_rx_ppe_intf1_ack                    ( dut_if.igr_rx_ppe_intf1_ack ),
      .igr_rx_ppe_intf0_tail                   ( dut_if.igr_rx_ppe_intf0_tail ),
      .igr_rx_ppe_intf0_head                   ( dut_if.igr_rx_ppe_intf0_head ),
      .igr_rx_ppe_intf1_tail                   ( dut_if.igr_rx_ppe_intf1_tail ),
      .igr_rx_ppe_intf1_head                   ( dut_if.igr_rx_ppe_intf1_head ),
      .rx_ppe_igr_intf0                        ( dut_if.rx_ppe_igr_intf0 ),
      .rx_ppe_igr_intf1                        ( dut_if.rx_ppe_igr_intf1 )

       );
    

    
    // instantiate testcase
    testcase test(
        dut_if                                    // pass interface dut_if into testcase
    );


    // Scott G -- this should go in monitor, but for quick hack, just putting it here...

    always @( negedge dut_if.clk ) begin

        for( int i=0; i<4; i++ ) begin
            if( dut_if.i_shim_pb_v_p0[i][0] ) begin
                $display("%0t: DEBUG MON: shim valid p0[%0d][0] sop %b eop %b data %0h ", 
                         $realtime, i,
                         dut_if.i_shim_pb_md_p0[i].md0.md.sop, 
                         dut_if.i_shim_pb_md_p0[i].md0.md.eop, 
                         dut_if.i_shim_pb_data_p0[i].seg0);
            end
            if( dut_if.i_shim_pb_v_p0[i][1] ) begin
                $display("%0t: DEBUG MON: shim valid p0[%0d][1] sop %b eop %b data %0h ", 
                         $realtime, i,
                         dut_if.i_shim_pb_md_p0[i].md1.md.sop, 
                         dut_if.i_shim_pb_md_p0[i].md1.md.eop, 
                         dut_if.i_shim_pb_data_p0[i].seg1);
            end
            if( dut_if.i_shim_pb_v_p0[i][2] ) begin
                $display("%0t: DEBUG MON: shim valid p0[%0d][2] sop %b eop %b data %0h ", 
                         $realtime, i,
                         dut_if.i_shim_pb_md_p0[i].md2.md.sop, 
                         dut_if.i_shim_pb_md_p0[i].md2.md.eop, 
                         dut_if.i_shim_pb_data_p0[i].seg2);
            end
        end

        `define LPP0_PATH top.pre_post_wrap.mby_igr_post_ppe.sop_mdata_lpp0_fpp
        if( `LPP0_PATH.valid ) begin
            $display("%0t: DEBUG MON: lpp0 mdata valid pkt_id %0h port %0h eop %b ", 
                     $realtime, 
                     `LPP0_PATH.pkt_id,
                     `LPP0_PATH.src_port,
                     `LPP0_PATH.md.eop
                     );
            
        end
  
        `define LPP1_PATH top.pre_post_wrap.mby_igr_post_ppe.sop_mdata_lpp1
        if( `LPP1_PATH.valid ) begin
            $display("%0t: DEBUG MON: lpp1 mdata valid pkt_id %0h port %0h eop %b ", 
                     $realtime, 
                     `LPP1_PATH.pkt_id,
                     `LPP1_PATH.src_port,
                     `LPP1_PATH.md.eop
                     );
            
        end


        `define EPL0_PATH top.pre_post_wrap.mby_igr_pre_ppe.tag_info_epl[0]
        if( `EPL0_PATH.valid ) begin
            $display("%0t: DEBUG MON: epl0 tag valid pkt_id %0h port %0h eop %b seg_ptr %0h sema %0b ", 
                     $realtime, 
                     `EPL0_PATH.pkt_id,
                     `EPL0_PATH.src_port,
                     `EPL0_PATH.md.valid,
                     `EPL0_PATH.wr_seg_ptr,
                     `EPL0_PATH.wr_sema
                     );
            // md .error, .wd_cnt, .eop_pos, .byte_pos
        end


        `define WRDATA0_PATH top.pre_post_wrap.mby_igr_pre_ppe.wr_data_epl[0]
        if( `WRDATA0_PATH.valid ) begin
            $display("%0t: DEBUG MON: wrdata valid seg_ptr %0h sema %0b, wd %0h, data ecc %0h %0h %0h %0h  %0h %0h %0h %0h ", 
                     $realtime, 
                     `WRDATA0_PATH.wr_seg_ptr,
                     `WRDATA0_PATH.wr_sema,
                     `WRDATA0_PATH.wd_sel,
                     `WRDATA0_PATH.data_ecc[0].data,
                     `WRDATA0_PATH.data_ecc[1].data,
                     `WRDATA0_PATH.data_ecc[2].data,
                     `WRDATA0_PATH.data_ecc[3].data,
                     `WRDATA0_PATH.data_ecc[4].data,
                     `WRDATA0_PATH.data_ecc[5].data,
                     `WRDATA0_PATH.data_ecc[6].data,
                     `WRDATA0_PATH.data_ecc[7].data
                     );
            
        end

    end
        
        
endmodule

