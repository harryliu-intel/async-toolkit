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
//-hz:
//`include "configuration.sv"
`include "inp_driver.sv"

// simulation signals from DUT  (DUT signals can be accessed using . separate full path to signal) 
`define NODE_PATH   top.msh_node
`define MEM_CTRL   `NODE_PATH.ctrl.mem_ctrl

class monitor;

//-hz:
//  localparam FIFO_DEPTH  = tmpl_pkg::FIFO_DEPTH;
//  localparam NUM_INPUTS  = tmpl_pkg::NUM_INPUTS;
//  localparam NUM_OUTPUTS = tmpl_pkg::NUM_OUTPUTS;
    localparam FIFO_DEPTH  = 80;
    localparam NUM_INPUTS  = 4;
    localparam NUM_OUTPUTS = 4;


    virtual msh_node_dut_if     dut_if;      
    // -hz: 12/7/2018:
    virtual mby_mem_msh_bank_ram_shell_4096x552_func_if mem_if_0;
    virtual mby_mem_msh_bank_ram_shell_4096x552_func_if mem_if_1;
    virtual mby_mem_msh_bank_ram_shell_4096x552_func_if mem_if_2;
    virtual mby_mem_msh_bank_ram_shell_4096x552_func_if mem_if_3;


    scoreboard              sb;
//-hz:
//  configuration           cfg;
    inp_driver              inp_drvr        [NUM_INPUTS-1:0];

    bit                     sb_done;
    bit [NUM_INPUTS-1:0]    inp_drv_done;
    bit                     all_done;

    string                  name;

    integer                 clk_cnt;
    integer                 heartbeat;

    integer		rd_rsp_cnt;
    integer		check_rd_rsp;
    integer		check_rd_data;

    integer		after_mem_wr;
    integer		mem_rd_cnt;

    integer		wr_req_cnt;
    integer		push_wr_data;

    // statistics variables
    integer                 stat_num_arb_conflicts;

    function new(
        virtual msh_node_dut_if dut_if,

        virtual mby_mem_msh_bank_ram_shell_4096x552_func_if mem_if_0,
        virtual mby_mem_msh_bank_ram_shell_4096x552_func_if mem_if_1,
        virtual mby_mem_msh_bank_ram_shell_4096x552_func_if mem_if_2,
        virtual mby_mem_msh_bank_ram_shell_4096x552_func_if mem_if_3,

        scoreboard          sb
//-hz:
//      configuration       cfg,
//      inp_driver          inp_drvr [NUM_INPUTS-1:0]
    );

        this.dut_if         = dut_if;
        this.sb             = sb;
//-hz:
//      this.cfg            = cfg;
//      this.inp_drvr       = inp_drvr;

	//-hz: 12/7/2018:
        this.mem_if_0	= mem_if_0;
        this.mem_if_1	= mem_if_1;
        this.mem_if_2	= mem_if_2;
        this.mem_if_3	= mem_if_3;

        name                    = "monitor.sv";
        clk_cnt                 = 0;
        stat_num_arb_conflicts  = 0;
        all_done                = 1'b0;

//-hz:
	rd_rsp_cnt		= 0;
	check_rd_rsp		= 0;
	check_rd_data		= 0;

	after_mem_wr		= 0;
        mem_rd_cnt		= 0;

	wr_req_cnt		= 0;
	push_wr_data		= 0;


        $value$plusargs("heartbeat=%d", heartbeat);     // heartbeat value comes from command line argument 

    endfunction

    task connect_to_DUT();
        forever begin
            @(negedge dut_if.mclk) // sample on negedge


	   // 12/6/2018: monitor wr req and data at mem_if:

           if (mem_if_0.wr_en)  begin 
            // $display("(time: %0d) %s: --------------------------------------------", $time, name);
               $display("(time: %0d) %s: mem_bank0: wr_en=%0d, adr='h%0h, wdata='h%0h", 
                         $time, name, mem_if_0.wr_en, mem_if_0.adr, mem_if_0.wr_data);
           end

           if (mem_if_1.wr_en)  begin
            // $display("(time: %0d) %s: --------------------------------------------", $time, name);
               $display("(time: %0d) %s: mem_bank1: wr_en=%0d, adr='h%0h, wdata='h%0h", 
                         $time, name, mem_if_1.wr_en, mem_if_1.adr, mem_if_1.wr_data);
           end

           if (mem_if_2.wr_en)  begin
            // $display("(time: %0d) %s: --------------------------------------------", $time, name);
               $display("(time: %0d) %s: mem_bank2: wr_en=%0d, adr='h%0h, wdata='h%0h", 
                         $time, name, mem_if_2.wr_en, mem_if_2.adr, mem_if_2.wr_data);
           end

           if (mem_if_3.wr_en)  begin
            // $display("(time: %0d) %s: --------------------------------------------", $time, name);
               $display("(time: %0d) %s: mem_bank3: wr_en=%0d, adr='h%0h, wdata='h%0h", 
                         $time, name, mem_if_3.wr_en, mem_if_3.adr, mem_if_3.wr_data);
           end



	   // 12/7/2018: check write at memory bank 0:
/*
	   if (mem_if_0.wr_en) begin
               sb.mem_bank0_wr_notification(mem_if_0.adr, mem_if_0.wr_data);
		after_mem_wr = 1;
	   end
*/


	   // 12/11/2018: push wr data at mem interface to fifo:
/*
	   if (mem_if_0.wr_en)
               sb.mem_bank0_req_in_notification(mem_if_0.adr, mem_if_0.wr_data);
*/


	   // 12/10/2018: monitor rd data at mem_if:

           // monitor rd req
/*
           if (mem_if_0.rd_en)  begin 
               $display("(time: %0d) %s: --------------------------------------------", $time, name);
		mem_rd_cnt++ ;
           //  $display("(time: %0d) %s: mem_bank0: mem_rd_cnt=%0d, rd_en=%0d, adr='h%0h", 
           //            $time, name, mem_rd_cnt, mem_if_0.rd_en, mem_if_0.adr);
               $display("(time: %0d) %s: mem_bank0: rd_en=%0d, adr='h%0h", 
                         $time, name, mem_if_0.rd_en, mem_if_0.adr);
           end

           if (mem_if_1.rd_en)  begin 
               $display("(time: %0d) %s: --------------------------------------------", $time, name);
		mem_rd_cnt++ ;
               $display("(time: %0d) %s: mem_bank1: mem_rd_cnt=%0d, rd_en=%0d, adr='h%0h", 
                         $time, name, mem_rd_cnt, mem_if_1.rd_en, mem_if_1.adr);
           end

           if (mem_if_2.rd_en)  begin 
               $display("(time: %0d) %s: --------------------------------------------", $time, name);
		mem_rd_cnt++ ;
               $display("(time: %0d) %s: mem_bank2: mem_rd_cnt=%0d, rd_en=%0d, adr='h%0h", 
                         $time, name, mem_rd_cnt, mem_if_2.rd_en, mem_if_2.adr);
           end

           if (mem_if_3.rd_en)  begin 
               $display("(time: %0d) %s: --------------------------------------------", $time, name);
		mem_rd_cnt++ ;
               $display("(time: %0d) %s: mem_bank3: mem_rd_cnt=%0d, rd_en=%0d, adr='h%0h", 
                         $time, name, mem_rd_cnt, mem_if_3.rd_en, mem_if_3.adr);
           end


           // monitor rd data which comes 1 cycle (should be 2 ???) after rd req

           if (mem_if_0.rd_valid)  begin 
               $display("(time: %0d) %s: --------------------------------------------", $time, name);
           //  $display("(time: %0d) %s: mem_bank0: mem_rd_cnt=%0d, rd_valid=%0d, rdata='h%0h", 
           //            $time, name, mem_rd_cnt, mem_if_0.rd_valid, mem_if_0.rd_data);
               $display("(time: %0d) %s: mem_bank0: rd_valid=%0d, rdata='h%0h", 
                         $time, name, mem_if_0.rd_valid, mem_if_0.rd_data);
           end

           if (mem_if_1.rd_valid)  begin 
               $display("(time: %0d) %s: --------------------------------------------", $time, name);
               $display("(time: %0d) %s: mem_bank1: mem_rd_cnt=%0d, rd_valid=%0d, rdata='h%0h", 
                         $time, name, mem_rd_cnt, mem_if_1.rd_valid, mem_if_1.rd_data);
           end

           if (mem_if_2.rd_valid)  begin 
               $display("(time: %0d) %s: --------------------------------------------", $time, name);
               $display("(time: %0d) %s: mem_bank2: mem_rd_cnt=%0d, rd_valid=%0d, rdata='h%0h", 
                         $time, name, mem_rd_cnt, mem_if_2.rd_valid, mem_if_2.rd_data);
           end

           if (mem_if_3.rd_valid)  begin 
               $display("(time: %0d) %s: --------------------------------------------", $time, name);
               $display("(time: %0d) %s: mem_bank3: mem_rd_cnt=%0d, rd_valid=%0d, rdata='h%0h", 
                         $time, name, mem_rd_cnt, mem_if_3.rd_valid, mem_if_3.rd_data);
           end
*/


	   // 12/10/2018: check 2nd read request at memory bank 0:
/*
  	   if (mem_if_0.rd_en && (mem_rd_cnt==2)) begin
               sb.mem_bank0_rd_req_notification(mem_if_0.adr);
  	   end
*/

	   // 12/10/2018: check 2nd read data at memory bank 0:
/*
  	   if (mem_if_0.rd_valid && (mem_rd_cnt==2)) begin
                 sb.mem_bank0_rd_data_notification(mem_if_0.rd_data);
  	   end
*/



// -----------------------------------------------------------------------------
	   // 12/12/2018: push wr data to fifo and pop data to check when read
// -----------------------------------------------------------------------------

	// expect wr data in next 2 cycles after req vld, 

	   if (dut_if.i_eb_wr_req[0].vld) 
	   begin
		wr_req_cnt++ ;
	   end
           else begin
                if (wr_req_cnt == 3) wr_req_cnt=0;
		else if (wr_req_cnt != 0) wr_req_cnt++;
  	   end

	   if (wr_req_cnt==3) push_wr_data = 1;
           else push_wr_data = 0;


	// push wr data:

	   if (push_wr_data)
               sb.eb_p0_wr_req_in_notification(dut_if.i_eb_wr_data[0]);



	// expect rd data in next 2 cycles after rd rsp

	   if (dut_if.o_wb_rd_rsp[0].vld) 
	   begin
                check_rd_rsp = 1;
		rd_rsp_cnt++ ;
	   end
           else begin
                check_rd_rsp = 0;
                if (rd_rsp_cnt == 3) rd_rsp_cnt=0;
		else if (rd_rsp_cnt != 0) rd_rsp_cnt++;
  	   end


	   if (rd_rsp_cnt==3) check_rd_data = 1;
           else check_rd_data = 0;


           // 12/11/2018: check rd data against wr fifo:

           if (check_rd_data) begin
                 $display("(time: %0d) %s: check wb rdata: rdata='h%0h",
                           $time, name, dut_if.o_wb_rd_data[0]);
                 sb.wb_p0_check_rd_data_notification(dut_if.o_wb_rd_data[0]);
           end



            // tell scoreboard about inputs driven to DUT
/*
	   if (check_rd_rsp) begin
               sb.wb_p0_rd_rsp_out_notification(dut_if.o_wb_rd_rsp[0]);
	   end

	   if (check_rd_data) begin
               sb.wb_p0_rd_data_out_notification(dut_if.o_wb_rd_data[0]);
	   end
*/

//         $display("-hz: monitor: (time: %0d)  check_rd_rsp = %0d  check_rd_data = %0d  rd_rsp_cnt = %0d ", 
//                                  $time,      check_rd_rsp,       check_rd_data,       rd_rsp_cnt);



//-hz:
//          foreach (dut_if.i_reqs[i])
//              if (dut_if.i_reqs[i].vld)
//                  sb.req_in_notification(i, dut_if.i_reqs[i]);
//
//          foreach (dut_if.i_eb_rd_req[i])
//              if (dut_if.i_eb_rd_req[i].vld)
//                  sb.req_in_notification(i, dut_if.i_eb_rd_req[i]);

            // tell scoreboard about arbitration bids 
//          foreach (`OUTP_ARB_BIDS[o])     // bids come from observation of DUT signals
//              if (|`OUTP_ARB_BIDS[o])
//                  sb.arb_bid_notification(o, `OUTP_ARB_BIDS[o], `OUTP_ARB_GNTS[o]);

            // count arbitration conflicts
//          foreach (`OUTP_ARB_BIDS[o,i])
//              if ((`OUTP_ARB_BIDS[o][i] == 1'b1) &&
//                  (`OUTP_ARB_GNTS[o][i] != 1'b1)   )
//                  stat_num_arb_conflicts++;

            // tell scoreboard about outputs driven by DUT
//          foreach (dut_if.o_reqs[o])
//              if (dut_if.o_reqs[o].vld)
//                  sb.req_out_notification(o, dut_if.o_reqs[o]);


            clk_cnt++;

//-hz:
//          sb_done = sb.all_empty();      // scoreboard has no unmatched qflits
            if (dut_if.o_wb_rd_rsp[0].vld) 
                sb_done = 1;

//          foreach (inp_drvr[i]) inp_drv_done[i] = inp_drvr[i].drv_done;
//          if (&inp_drv_done && sb_done) all_done = 1;

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
            @(negedge dut_if.mclk) // sample on negedge

            // check that FIFOs are empty
           
            // there seems to be a VCS bug that doesn't like variable indexing of inp_section
//            for (int i=0; i < NUM_INPUTS; i++) 
//                for (int e=1; e <= FIFO_DEPTH; e++) 
//                    if (top.dut.inp_section[i].rtinfo_fifo.full[e] != 0) begin
//                        $display("ERROR:  (time: %0d)  FINAL STATE CHECK FAILURE:  input FIFO %0d not empty", $time, i);
//                        $assertoff;
//                        $finish;
//                    end

//-hz:
/*
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
*/ // -hz:
            $display("-hz: inside monitor:  (time: %0d)  rsp.vld =  %0d ", $time, dut_if.o_wb_rd_rsp[0].vld);

        end
    endtask

    task print_stats(string prepend);
        $display("%s %s: stat_num_arb_conflicts = %0d", prepend, name, stat_num_arb_conflicts);
    endtask

endclass

`endif // MONITOR_SV
