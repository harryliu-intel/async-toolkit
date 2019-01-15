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
// -- Author : Jim McCormick <hess.hodge@intel.com>
// -- Project Name : ??? 
// -- Description  : This is a template for testbench code that drives
// --                the inputs of a DUT (Design Under Test). 
// ---------------------------------------------------------------------------------------------------------------------




`ifndef INP_DRIVER
`define INP_DRIVER

//`include "scoreboard.sv"
//`include "configuration.sv"
//`include "stimulus.sv"
import mby_igr_pkg::*, mby_egr_pkg::*, shared_pkg::*;

class inp_driver;

//    stimulus                stim;           // object for generating stimulus
//    configuration           cfg;            // config object in stim creation 
//
    virtual igr_dut_if      dut_if;           // input driver drives inputs in this interface
//
//    tmpl_pkg::req_in_t      req_fifo[$];    // a queue to hold incoming requests
//
//    tmpl_pkg::enc_inp_t     iport;          // input port number
    integer                 clk_cnt;        // counts clocks
//    integer                 num_reqs;       // number of input requests
    bit                     drv_done;       // set when driver is done
    string                  name;           // input driver name used in $display statements
    integer                 drove_reqs;

     
    shim_pb_data_t [3:0]       i_shim_pb_data_p0;
    shim_pb_data_t [3:0]       i_shim_pb_data_p1;
    shim_pb_data_t [3:0]       i_shim_pb_data_p2;
    shim_pb_data_t [3:0]       i_shim_pb_data_p3;
    shim_pb_md_t   [3:0]       i_shim_pb_md_p0;
    shim_pb_md_t   [3:0]       i_shim_pb_md_p1;
    shim_pb_md_t   [3:0]       i_shim_pb_md_p2;
    shim_pb_md_t   [3:0]       i_shim_pb_md_p3;
    logic [3:0][2:0]           i_shim_pb_v_p0;  
    logic [3:0][2:0]           i_shim_pb_v_p1;  
    logic [3:0][2:0]           i_shim_pb_v_p2;  
    logic [3:0][2:0]           i_shim_pb_v_p3;
    
    logic [3:0]                i_free_ptr_valid;
    logic [3:0]                o_free_ptr_req;
    seg_ptr_t [3:0]            i_free_seg_ptr;
    sema_t    [3:0]            i_free_sema;
    
    shim_pb_data_t [3:0]       shim_pb_data_p0_ff;
    shim_pb_data_t [3:0]       shim_pb_data_p1_ff;
    shim_pb_data_t [3:0]       shim_pb_data_p2_ff;
    shim_pb_data_t [3:0]       shim_pb_data_p3_ff;
    shim_pb_md_t   [3:0]       shim_pb_md_p0_ff;
    shim_pb_md_t   [3:0]       shim_pb_md_p1_ff;
    shim_pb_md_t   [3:0]       shim_pb_md_p2_ff;
    shim_pb_md_t   [3:0]       shim_pb_md_p3_ff;
    logic [3:0][2:0]           shim_pb_v_p0_ff;  
    logic [3:0][2:0]           shim_pb_v_p1_ff;  
    logic [3:0][2:0]           shim_pb_v_p2_ff;  
    logic [3:0][2:0]           shim_pb_v_p3_ff;
    
    logic [3:0]                free_ptr_valid_ff;
    seg_ptr_t [3:0]            free_seg_ptr_ff;
    sema_t    [3:0]            free_sema_ff;


    function new(

//        tmpl_pkg::enc_inp_t     iport, 
        virtual igr_dut_if     dut_if
//        configuration           cfg

    );

//        this.iport  = iport;
        this.dut_if = dut_if;
//        this.cfg    = cfg;

        name        = "inp_driver.sv";
//        stim = new(
//            .cfg    (cfg),
//            .iport  (iport)
//        );

        clk_cnt = 0;
        drove_reqs    = 0;
    endfunction

    // reset input driver
    task reset();
        drv_done        = 1'b0;
//        req_fifo        = {};           // initialize to empty queue
        for(int i=0; i<4; i++) begin
            i_shim_pb_data_p0[i] = shim_pb_data_t'('0); 
            i_shim_pb_data_p1[i] = shim_pb_data_t'('0); 
            i_shim_pb_data_p2[i] = shim_pb_data_t'('0); 
            i_shim_pb_data_p3[i] = shim_pb_data_t'('0); 
            i_shim_pb_md_p0[i]   = shim_pb_md_t'('0); 
            i_shim_pb_md_p1[i]   = shim_pb_md_t'('0); 
            i_shim_pb_md_p2[i]   = shim_pb_md_t'('0); 
            i_shim_pb_md_p3[i]   = shim_pb_md_t'('0); 
            i_shim_pb_v_p0[i]    = 3'h0; 
            i_shim_pb_v_p1[i]    = 3'h0; 
            i_shim_pb_v_p2[i]    = 3'h0; 
            i_shim_pb_v_p3[i]    = 3'h0; 
            i_free_ptr_valid[i]  = 1'b0;
            i_free_seg_ptr[i]    = seg_ptr_t'('0);
            i_free_sema[i]       = sema_t'('0);
            
        end
    endtask

    // connect signal defined in input driver to DUT interface
    task connect_to_DUT_inputs();
        forever begin
            @(posedge dut_if.clk);
            clk_cnt++;      // keep track of number of clocks
           
            // get inputs from drive_reqs() task
            shim_pb_data_p0_ff <= i_shim_pb_data_p0; 
            shim_pb_data_p1_ff <= i_shim_pb_data_p1; 
            shim_pb_data_p2_ff <= i_shim_pb_data_p2; 
            shim_pb_data_p3_ff <= i_shim_pb_data_p3; 
            shim_pb_md_p0_ff   <= i_shim_pb_md_p0  ; 
            shim_pb_md_p1_ff   <= i_shim_pb_md_p1  ; 
            shim_pb_md_p2_ff   <= i_shim_pb_md_p2  ; 
            shim_pb_md_p3_ff   <= i_shim_pb_md_p3  ; 
            shim_pb_v_p0_ff    <= i_shim_pb_v_p0   ; 
            shim_pb_v_p1_ff    <= i_shim_pb_v_p1   ; 
            shim_pb_v_p2_ff    <= i_shim_pb_v_p2   ; 
            shim_pb_v_p3_ff    <= i_shim_pb_v_p3   ; 
            free_ptr_valid_ff  <= i_free_ptr_valid;
            free_seg_ptr_ff    <= i_free_seg_ptr;
            free_sema_ff       <= i_free_sema;

          //  drvr_rd_req_to_dut_p1  <= drvr_rd_req_to_dut;
          //  drvr_wr_req_to_dut_p1  <= drvr_wr_req_to_dut;
          //  drvr_wr_dbus_to_dut_p1 <= drvr_wr_dbus_to_dut;

            // drive DUT inputs 
          //  dut_if.i_eb_rd_req[0]  = drvr_rd_req_to_dut_p1;
          //  dut_if.i_eb_wr_req[0]  = drvr_wr_req_to_dut_p1;
          //  dut_if.i_eb_wr_dbus[0] = drvr_wr_dbus_to_dut_p1;
            dut_if.i_shim_pb_data_p0 = shim_pb_data_p0_ff; 
            dut_if.i_shim_pb_data_p1 = shim_pb_data_p1_ff; 
            dut_if.i_shim_pb_data_p2 = shim_pb_data_p2_ff; 
            dut_if.i_shim_pb_data_p3 = shim_pb_data_p3_ff; 
            dut_if.i_shim_pb_md_p0   = shim_pb_md_p0_ff  ; 
            dut_if.i_shim_pb_md_p1   = shim_pb_md_p1_ff  ; 
            dut_if.i_shim_pb_md_p2   = shim_pb_md_p2_ff  ; 
            dut_if.i_shim_pb_md_p3   = shim_pb_md_p3_ff  ; 
            dut_if.i_shim_pb_v_p0    = shim_pb_v_p0_ff   ; 
            dut_if.i_shim_pb_v_p1    = shim_pb_v_p1_ff   ; 
            dut_if.i_shim_pb_v_p2    = shim_pb_v_p2_ff   ; 
            dut_if.i_shim_pb_v_p3    = shim_pb_v_p3_ff   ; 
            dut_if.i_free_ptr_valid  = free_ptr_valid_ff;
            dut_if.i_free_seg_ptr    = free_seg_ptr_ff;
            dut_if.i_free_sema       = free_sema_ff;
    
            dut_if.i_return_id_valid = '0;
            
        end
    endtask

    // generate a specified number of requests and load them into testbench request FIFOs
//    task load_stimulus(integer num_reqs);
//        this.num_reqs = num_reqs;
//
//        while (this.num_reqs > 0) begin
//            stim.randomize();               // create a random request 
//            req_fifo.push_back(stim.req);   // put random request into FIFO
//            this.num_reqs--;
//        end
//
//`ifdef INP_DRIVER_DEBUG
//        foreach (req_fifo[idx])
//            $display("(time: %0d)  INP_DRIVER_DEBUG: GENERATED REQUEST (iport %0d): vld=%d, outp=%d, data=%x", $time, iport, req_fifo[idx].vld, req_fifo[idx].outp, req_fifo[idx].data);
//`endif  // INP_DRIVER_DEBUG
//
//    endtask
//
    // Drive requests into DUT (template)
    task drive_reqs();
        if (!drove_reqs) begin
            repeat (10) @(posedge dut_if.clk);

            //--------------------------------------------------------
            // first packet -- port 0
            //   placeholder for free pointers from pointer cache
            i_free_ptr_valid          = 4'b0001;
            i_free_seg_ptr[0]         = 20'h11111;
            i_free_sema[0]            = 4'b1010;

            // 64B word valid
            i_shim_pb_v_p0[0][0]      = 1'b1;

            for( int i=0; i<8; i++ ) begin
                i_shim_pb_data_p0[0].seg0[i] = 72'(i+1);
            end

            i_shim_pb_md_p0[0].md0        = shim_md_t'('h123456);
            i_shim_pb_md_p0[0].md0.md.sop = 1'b1;
            i_shim_pb_md_p0[0].md0.md.eop = 1'b1;

            @(posedge dut_if.clk);
            i_shim_pb_v_p0[0][0]      = 1'b0;
            i_shim_pb_data_p0[0].seg0 = {8{72'h00}};
            i_shim_pb_md_p0[0]        = shim_pb_md_t'('h0);
  

            repeat (300) @(posedge dut_if.clk);
            //--------------------------------------------------------
            // packet #2 -- port 0
            //   placeholder for free pointers from pointer cache
            i_free_ptr_valid          = 4'b0001;
            i_free_seg_ptr[0]         = 20'h22222;
            i_free_sema[0]            = 4'b1010;

            // 64B word valid
            i_shim_pb_v_p0[0][1]      = 1'b1;

            for( int i=0; i<8; i++ ) begin
                i_shim_pb_data_p0[0].seg1[i] = 72'(i*2+1);
            end

            i_shim_pb_md_p0[0].md1        = shim_md_t'('h123422);
            i_shim_pb_md_p0[0].md1.md.sop = 1'b1;
            i_shim_pb_md_p0[0].md1.md.eop = 1'b1;

            @(posedge dut_if.clk);
            i_shim_pb_v_p0[0][1]      = 1'b0;
            i_shim_pb_data_p0[0].seg1 = {8{72'h00}};
            i_shim_pb_md_p0[0]        = shim_pb_md_t'('h0);
  

            repeat (300) @(posedge dut_if.clk);
            //--------------------------------------------------------
            // packet #3 -- port 0
            //   placeholder for free pointers from pointer cache
            i_free_ptr_valid          = 4'b0001;
            i_free_seg_ptr[0]         = 20'h33333;
            i_free_sema[0]            = 4'b1111;

            // 64B word valid
            i_shim_pb_v_p0[0][2]      = 1'b1;

            for( int i=0; i<8; i++ ) begin
                i_shim_pb_data_p0[0].seg2[i] = 72'(i*3+1);
            end

            i_shim_pb_md_p0[0].md2        = shim_md_t'('h123433);
            i_shim_pb_md_p0[0].md2.md.sop = 1'b1;
            i_shim_pb_md_p0[0].md2.md.eop = 1'b0;

            @(posedge dut_if.clk);
            // 2nd 64B of 3rd packet
            i_shim_pb_v_p0[0][2]      = 1'b0;
            i_shim_pb_data_p0[0].seg2 = {8{72'h00}};
  
            i_shim_pb_v_p0[0][0]      = 1'b1;

            for( int i=0; i<8; i++ ) begin
                i_shim_pb_data_p0[0].seg0[i] = 72'(i*4+1);
            end

            i_shim_pb_md_p0[0].md0        = shim_md_t'('haaaaa);
            i_shim_pb_md_p0[0].md0.md.sop = 1'b0;
            i_shim_pb_md_p0[0].md0.md.eop = 1'b0;

            @(posedge dut_if.clk);
            // 3rd 64B of 3rd packet
            i_shim_pb_v_p0[0][0]      = 1'b0;
            i_shim_pb_data_p0[0].seg0 = {8{72'h00}};
  
            i_shim_pb_v_p0[0][1]      = 1'b1;

            for( int i=0; i<8; i++ ) begin
                i_shim_pb_data_p0[0].seg1[i] = 72'(i*5+1);
            end

            i_shim_pb_md_p0[0].md1        = shim_md_t'('haaaaa);
            i_shim_pb_md_p0[0].md1.md.sop = 1'b0;
            i_shim_pb_md_p0[0].md1.md.eop = 1'b0;


            @(posedge dut_if.clk);
            // 4th 64B of 3rd packet, with EOP
            i_shim_pb_v_p0[0][1]      = 1'b0;
            i_shim_pb_data_p0[0].seg1 = {8{72'h00}};
  
            i_shim_pb_v_p0[0][2]      = 1'b1;

            for( int i=0; i<8; i++ ) begin
                i_shim_pb_data_p0[0].seg2[i] = 72'(i*6+1);
            end

            i_shim_pb_md_p0[0].md2        = shim_md_t'('haaaaa);
            i_shim_pb_md_p0[0].md2.md.sop = 1'b0;
            i_shim_pb_md_p0[0].md2.md.eop = 1'b1;

            @(posedge dut_if.clk);
            i_shim_pb_v_p0[0][2]      = 1'b0;
            i_shim_pb_data_p0[0].seg2 = {8{72'h00}};
            i_shim_pb_md_p0[0].md2        = shim_md_t'('hfffff);
            i_shim_pb_md_p0[0].md2.md.sop = 1'b0;
            i_shim_pb_md_p0[0].md2.md.eop = 1'b0;

            
            repeat (300) @(posedge dut_if.clk);

            drove_reqs = 1; 

        end

//        while (something_to_do()) begin
//            @(posedge dut_if.clk);
//
//            if (req_fifo.size() != 0) begin
//                drvr_req_to_dut = req_fifo.pop_front();
//            end else
//                drvr_req_to_dut = '0; 
//        end
//
//        $display("(time: %0d) %s: ** Done Driving Requests to Inputs (iport %0d) ** ", $time, name, iport);

        $display("(time: %0d) %s: ** Done Driving Requests to Inputs ** ", $time, name);

        @(posedge dut_if.clk);

        drv_done = 1'b1;
    endtask

    // figure out if input driver is done or not
    function bit something_to_do();
        something_to_do = 1'b0;
//        if (req_fifo.size() != 0) something_to_do = 1;
//        if (drvr_req_to_dut.vld || drvr_req_to_dut_p1.vld) something_to_do = 1;
//        return something_to_do;
    endfunction

endclass

`endif // INP_DRIVER
