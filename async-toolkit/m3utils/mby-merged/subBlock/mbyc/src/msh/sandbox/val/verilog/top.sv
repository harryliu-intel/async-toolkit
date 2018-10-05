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
//              msh_dut_if dut_if                               msh_dut_if.sv
//              msh_node    dut                                 $(CLONE_ROOT)/src/msh/rtl/mby_msh_node.sv
//                  ...                                         (see mby_msh.sv)
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
();

    logic clk = 0;                              // declare clock


    // define clock
    initial                                     // "initial" procedures are executed at the beginning of simulation
        forever                                 // "forever" is just a type of loop that executes forever 
            #(10/2)                             //  #<n> means wait <n> clocks
                clk = ~clk;                     //  invert the clock

    // instantiate DUT interface
    msh_dut_if dut_if (
        .mclk(clk)                               // pass clock into this interface to make it part of the interface
    );

//**********************************************************************************************************************
// MESH NODE INTERFACE INSTANTIATIONS
//**********************************************************************************************************************

// north boundary interfaces

mby_msh_col_wr_if north_nb_wr_ifs[NUM_MSH_COLS-1:0]();
mby_msh_col_rd_if north_nb_rd_ifs[NUM_MSH_COLS-1:0]();
mby_msh_col_wr_if north_sb_wr_ifs[NUM_MSH_COLS-1:0]();
mby_msh_col_rd_if north_sb_rd_ifs[NUM_MSH_COLS-1:0]();

// south boundary interfaces

mby_msh_col_wr_if south_nb_wr_ifs[NUM_MSH_COLS-1:0]();
mby_msh_col_rd_if south_nb_rd_ifs[NUM_MSH_COLS-1:0]();
mby_msh_col_wr_if south_sb_wr_ifs[NUM_MSH_COLS-1:0]();
mby_msh_col_rd_if south_sb_rd_ifs[NUM_MSH_COLS-1:0]();

// east boundary interfaces

mby_msh_row_wr_if east_eb_wr_ifs[NUM_MSH_ROWS-1:0]();
mby_msh_row_rd_if east_eb_rd_ifs[NUM_MSH_ROWS-1:0]();
mby_msh_row_wr_if east_wb_wr_ifs[NUM_MSH_ROWS-1:0]();
mby_msh_row_rd_if east_wb_rd_ifs[NUM_MSH_ROWS-1:0]();

// west boundary interfaces

mby_msh_row_wr_if west_eb_wr_ifs[NUM_MSH_ROWS-1:0]();
mby_msh_row_rd_if west_eb_rd_ifs[NUM_MSH_ROWS-1:0]();
mby_msh_row_wr_if west_wb_wr_ifs[NUM_MSH_ROWS-1:0]();
mby_msh_row_rd_if west_wb_rd_ifs[NUM_MSH_ROWS-1:0]();

logic reset;

    // start firing reset immediately to protect reset guarded assertions
    initial dut_if.i_reset = 1'b1;
    initial reset = 1'b1;

    // instantiate DUT (Design Under Test)
//    mby_msh dut (
//
//        // DUT inputs
//        .mclk       (dut_if.mclk          ),      // The interface instantiated above is connected to the DUT 
//        .i_reset    (dut_if.i_reset       )      // Note the naming convention i_<???> for inputs.
//       
//    );

mby_msh msh(

    
    .mclk(clk),                                // mesh clock                                 
    .i_reset(reset),                            // reset

    // north boundary interfaces

    .o_north_nb_wr_ifs    (north_nb_wr_ifs),
    .o_north_nb_rd_ifs    (north_nb_rd_ifs),
    .i_north_sb_wr_ifs    (north_sb_wr_ifs),
    .i_north_sb_rd_ifs    (north_sb_rd_ifs),

    // south boundary interfaces

    .i_south_nb_wr_ifs    (south_nb_wr_ifs),
    .i_south_nb_rd_ifs    (south_nb_rd_ifs),
    .o_south_sb_wr_ifs    (south_sb_wr_ifs),
    .o_south_sb_rd_ifs    (south_sb_rd_ifs),

    // east boundary interfaces

    .o_east_eb_wr_ifs     (east_eb_wr_ifs),
    .o_east_eb_rd_ifs     (east_eb_rd_ifs),
    .i_east_wb_wr_ifs     (east_wb_wr_ifs),
    .i_east_wb_rd_ifs     (east_wb_rd_ifs),

    // west boundary interfaces

    .i_west_eb_wr_ifs     (west_eb_wr_ifs),
    .i_west_eb_rd_ifs     (west_eb_rd_ifs),
    .o_west_wb_wr_ifs     (west_wb_wr_ifs),
    .o_west_wb_rd_ifs     (west_wb_rd_ifs)

);

    // instantiate testcase
    testcase test(
        dut_if                                    // pass interface dut_if into testcase
    );

endmodule

