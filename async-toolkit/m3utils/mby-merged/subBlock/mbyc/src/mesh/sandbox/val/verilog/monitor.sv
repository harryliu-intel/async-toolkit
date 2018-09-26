///
///  INTEL CONFIDENTIAL
///
///  Copyright 2017 Intel Corporation All Rights Reserved.
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
// -- Description  : Monitor DUT behavior.  Pass messages to the scoreboard. Check for end of testcase.
// ---------------------------------------------------------------------------------------------------------------------

`ifndef MONITOR_SV
`define MONITOR_SV

`include "scoreboard.sv"
`include "configuration.sv"
`include "inp_driver.sv"

// simulation signals from DUT  (DUT signals can be accessed using . separate full path to signal) 
`define TEMPLATE_PATH   top.dut
`define OUTP_ARB_BIDS   `TEMPLATE_PATH.tmpl_arb.outp_arb_bids_arb
`define OUTP_ARB_GNTS   `TEMPLATE_PATH.tmpl_arb.outp_arb_gnts_arb
`define INP_SECTION     `TEMPLATE_PATH.inp_section
`define INP_FIFO_FULL   inp_flop_fifo.full 

class monitor;

    localparam FIFO_DEPTH  = tmpl_pkg::FIFO_DEPTH;
    localparam NUM_INPUTS  = tmpl_pkg::NUM_INPUTS;
    localparam NUM_OUTPUTS = tmpl_pkg::NUM_OUTPUTS;

    virtual tmpl_dut_if     dut_if;
    scoreboard              sb;
    configuration           cfg;
    inp_driver              inp_drvr        [NUM_INPUTS-1:0];

    bit                     sb_done;
    bit [NUM_INPUTS-1:0]    inp_drv_done;
    bit                     all_done;

    string                  name;

    integer                 clk_cnt;
    integer                 heartbeat;

    // statistics variables
    integer                 stat_num_arb_conflicts;

    function new(
        virtual tmpl_dut_if dut_if,
        scoreboard          sb,
        configuration       cfg,
        inp_driver          inp_drvr [NUM_INPUTS-1:0]
    );

        this.dut_if         = dut_if;
        this.sb             = sb;
        this.cfg            = cfg;
        this.inp_drvr       = inp_drvr;

        name                    = "monitor.sv";
        clk_cnt                 = 0;
        stat_num_arb_conflicts  = 0;
        all_done                = 1'b0;

        $value$plusargs("heartbeat=%d", heartbeat);     // heartbeat value comes from command line argument 

    endfunction

    task connect_to_DUT();
        forever begin
            @(negedge dut_if.clk) // sample on negedge

            // tell scoreboard about inputs driven to DUT
            foreach (dut_if.i_reqs[i])
                if (dut_if.i_reqs[i].vld)
                    sb.req_in_notification(i, dut_if.i_reqs[i]);

            // tell scoreboard about arbitration bids 
            foreach (`OUTP_ARB_BIDS[o])     // bids come from observation of DUT signals
                if (|`OUTP_ARB_BIDS[o])
                    sb.arb_bid_notification(o, `OUTP_ARB_BIDS[o], `OUTP_ARB_GNTS[o]);

            // count arbitration conflicts
            foreach (`OUTP_ARB_BIDS[o,i])
                if ((`OUTP_ARB_BIDS[o][i] == 1'b1) &&
                    (`OUTP_ARB_GNTS[o][i] != 1'b1)   )
                    stat_num_arb_conflicts++;

            // tell scoreboard about outputs driven by DUT
            foreach (dut_if.o_reqs[o])
                if (dut_if.o_reqs[o].vld)
                    sb.req_out_notification(o, dut_if.o_reqs[o]);


            clk_cnt++;

            sb_done = sb.all_empty();      // scoreboard has no unmatched qflits
            foreach (inp_drvr[i]) inp_drv_done[i] = inp_drvr[i].drv_done;
            if (&inp_drv_done && sb_done) all_done = 1;

`ifdef HEARTBEAT_ON
            if ((clk_cnt % heartbeat)==0) $display($time," %s  Heart Beat...", name);
`endif

        end
    endtask

    task reset();
        all_done = 1'b0;
    endtask

    task final_state_check();
        begin
            @(negedge dut_if.clk) // sample on negedge

            // check that FIFOs are empty
           
            // there seems to be a VCS bug that doesn't like variable indexing of inp_section
//            for (int i=0; i < NUM_INPUTS; i++) 
//                for (int e=1; e <= FIFO_DEPTH; e++) 
//                    if (top.dut.inp_section[i].rtinfo_fifo.full[e] != 0) begin
//                        $display("ERROR:  (time: %0d)  FINAL STATE CHECK FAILURE:  input FIFO %0d not empty", $time, i);
//                        $assertoff;
//                        $finish;
//                    end

                for (int e=1; e <= FIFO_DEPTH; e++) begin
                    if (top.dut.inp_section[0].rtinfo_fifo.full[e] != 0) begin
                        $display("ERROR:  (time: %0d)  FINAL STATE CHECK FAILURE:  input FIFO %0d not empty", $time, 0);
                        $assertoff;
                        $finish;
                    end
                    if (top.dut.inp_section[1].rtinfo_fifo.full[e] != 0) begin
                        $display("ERROR:  (time: %0d)  FINAL STATE CHECK FAILURE:  input FIFO %0d not empty", $time, 1);
                        $assertoff;
                        $finish;
                    end
                    if (top.dut.inp_section[2].rtinfo_fifo.full[e] != 0) begin
                        $display("ERROR:  (time: %0d)  FINAL STATE CHECK FAILURE:  input FIFO %0d not empty", $time, 2);
                        $assertoff;
                        $finish;
                    end
                    if (top.dut.inp_section[3].rtinfo_fifo.full[e] != 0) begin
                        $display("ERROR:  (time: %0d)  FINAL STATE CHECK FAILURE:  input FIFO %0d not empty", $time, 3);
                        $assertoff;
                        $finish;
                    end
                end

        end
    endtask

    task print_stats(string prepend);
        $display("%s %s: stat_num_arb_conflicts = %0d", prepend, name, stat_num_arb_conflicts);
    endtask

endclass

`endif // MONITOR_SV
