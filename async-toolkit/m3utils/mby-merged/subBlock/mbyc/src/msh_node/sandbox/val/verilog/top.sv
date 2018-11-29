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
import mby_msh_pkg::*;
import mby_msh_node_pkg::*;
import msh_node_sim_pkg::*;
();

    logic clk = 0;                              // declare clock


    // define clock
    initial                                     // "initial" procedures are executed at the beginning of simulation
        forever                                 // "forever" is just a type of loop that executes forever 
            #(10/2)                             //  #<n> means wait <n> clocks
                clk = ~clk;                     //  invert the clock

    // instantiate DUT interface
    msh_node_dut_if dut_if (
        .mclk(clk)                               // pass clock into this interface to make it part of the interface
    );


    // start firing reset immediately to protect reset guarded assertions
    initial dut_if.mhreset = 1'b1;
    initial dut_if.msreset = 1'b1;

    // instantiate DUT (Design Under Test)

    mby_msh_node msh_node(

    
        .mclk                           (clk),
        .mhreset                        (dut_if.mhreset),
        .msreset                        (dut_if.msreset),

        .i_eb_node_col                  (dut_if.i_eb_node_col),
        .i_sb_node_row                  (dut_if.i_sb_node_row),

        .i_nb_wr_req                    (dut_if.i_nb_wr_req),
        .i_sb_wr_req                    (dut_if.i_sb_wr_req),
        .i_eb_wr_req                    (dut_if.i_eb_wr_req),
        .i_wb_wr_req                    (dut_if.i_wb_wr_req),

        .i_nb_wr_dbus                   (dut_if.i_nb_wr_dbus),
        .i_sb_wr_dbus                   (dut_if.i_sb_wr_dbus),
        .i_eb_wr_dbus                   (dut_if.i_eb_wr_dbus),
        .i_wb_wr_dbus                   (dut_if.i_wb_wr_dbus),

        .i_nb_rd_req                    (dut_if.i_nb_rd_req),
        .i_sb_rd_req                    (dut_if.i_sb_rd_req),
        .i_eb_rd_req                    (dut_if.i_eb_rd_req),
        .i_wb_rd_req                    (dut_if.i_wb_rd_req),

        .i_nb_rd_rsp                    (dut_if.i_nb_rd_rsp),
        .i_sb_rd_rsp                    (dut_if.i_sb_rd_rsp),
        .i_eb_rd_rsp                    (dut_if.i_eb_rd_rsp),
        .i_wb_rd_rsp                    (dut_if.i_wb_rd_rsp),

        .i_nb_rd_dbus                   (dut_if.i_nb_rd_dbus),
        .i_sb_rd_dbus                   (dut_if.i_sb_rd_dbus),
        .i_eb_rd_dbus                   (dut_if.i_eb_rd_dbus),
        .i_wb_rd_dbus                   (dut_if.i_wb_rd_dbus),

        .i_nb_crdt_rtn_for_sb_wr_req    (dut_if.i_nb_crdt_rtn_for_sb_wr_req),
        .i_sb_crdt_rtn_for_nb_wr_req    (dut_if.i_sb_crdt_rtn_for_nb_wr_req),
        .i_eb_crdt_rtns_for_wb_wr_reqs  (dut_if.i_eb_crdt_rtns_for_wb_wr_reqs),
        .i_wb_crdt_rtns_for_eb_wr_reqs  (dut_if.i_wb_crdt_rtns_for_eb_wr_reqs),

        .i_nb_crdt_rtn_for_sb_rd_req    (dut_if.i_nb_crdt_rtn_for_sb_rd_req),
        .i_sb_crdt_rtn_for_nb_rd_req    (dut_if.i_sb_crdt_rtn_for_nb_rd_req),
        .i_eb_crdt_rtns_for_wb_rd_reqs  (dut_if.i_eb_crdt_rtns_for_wb_rd_reqs),
        .i_wb_crdt_rtns_for_eb_rd_reqs  (dut_if.i_wb_crdt_rtns_for_eb_rd_reqs),

        .i_nb_crdt_rtn_for_sb_rd_rsp    (dut_if.i_nb_crdt_rtn_for_sb_rd_rsp),
        .i_sb_crdt_rtn_for_nb_rd_rsp    (dut_if.i_sb_crdt_rtn_for_nb_rd_rsp),
        .i_eb_crdt_rtn_for_wb_rd_rsp    (dut_if.i_eb_crdt_rtn_for_wb_rd_rsp),
        .i_wb_crdt_rtn_for_eb_rd_rsp    (dut_if.i_wb_crdt_rtn_for_eb_rd_rsp),

        // outputs
   
        .o_nb_wr_req                    (dut_if.o_nb_wr_req),
        .o_sb_wr_req                    (dut_if.o_sb_wr_req),
        .o_eb_wr_req                    (dut_if.o_eb_wr_req),
        .o_wb_wr_req                    (dut_if.o_wb_wr_req),

        .o_nb_wr_dbus                   (dut_if.o_nb_wr_dbus),
        .o_sb_wr_dbus                   (dut_if.o_sb_wr_dbus),
        .o_eb_wr_dbus                   (dut_if.o_eb_wr_dbus),
        .o_wb_wr_dbus                   (dut_if.o_wb_wr_dbus),

        .o_nb_rd_req                    (dut_if.o_nb_rd_req),
        .o_sb_rd_req                    (dut_if.o_sb_rd_req),
        .o_eb_rd_req                    (dut_if.o_eb_rd_req),
        .o_wb_rd_req                    (dut_if.o_wb_rd_req),

        .o_nb_rd_rsp                    (dut_if.o_nb_rd_rsp),
        .o_sb_rd_rsp                    (dut_if.o_sb_rd_rsp),
        .o_eb_rd_rsp                    (dut_if.o_eb_rd_rsp),
        .o_wb_rd_rsp                    (dut_if.o_wb_rd_rsp),

        .o_nb_rd_dbus                   (dut_if.o_nb_rd_dbus),
        .o_sb_rd_dbus                   (dut_if.o_sb_rd_dbus),
        .o_eb_rd_dbus                   (dut_if.o_eb_rd_dbus),
        .o_wb_rd_dbus                   (dut_if.o_wb_rd_dbus),

        .o_nb_crdt_rtn_for_sb_wr_req    (dut_if.o_nb_crdt_rtn_for_sb_wr_req),
        .o_sb_crdt_rtn_for_nb_wr_req    (dut_if.o_sb_crdt_rtn_for_nb_wr_req),
        .o_eb_crdt_rtns_for_wb_wr_reqs  (dut_if.o_eb_crdt_rtns_for_wb_wr_reqs),
        .o_wb_crdt_rtns_for_eb_wr_reqs  (dut_if.o_wb_crdt_rtns_for_eb_wr_reqs),
                                                                                          
        .o_nb_crdt_rtn_for_sb_rd_req    (dut_if.o_nb_crdt_rtn_for_sb_rd_req),
        .o_sb_crdt_rtn_for_nb_rd_req    (dut_if.o_sb_crdt_rtn_for_nb_rd_req),
        .o_eb_crdt_rtns_for_wb_rd_reqs  (dut_if.o_eb_crdt_rtns_for_wb_rd_reqs),
        .o_wb_crdt_rtns_for_eb_rd_reqs  (dut_if.o_wb_crdt_rtns_for_eb_rd_reqs),
                                                                                          
        .o_nb_crdt_rtn_for_sb_rd_rsp    (dut_if.o_nb_crdt_rtn_for_sb_rd_rsp),
        .o_sb_crdt_rtn_for_nb_rd_rsp    (dut_if.o_sb_crdt_rtn_for_nb_rd_rsp),
        .o_eb_crdt_rtn_for_wb_rd_rsp    (dut_if.o_eb_crdt_rtn_for_wb_rd_rsp),
        .o_wb_crdt_rtn_for_eb_rd_rsp    (dut_if.o_wb_crdt_rtn_for_eb_rd_rsp)

    );

    // instantiate testcase
    testcase test(
        dut_if                                    // pass interface dut_if into testcase
    );

endmodule

