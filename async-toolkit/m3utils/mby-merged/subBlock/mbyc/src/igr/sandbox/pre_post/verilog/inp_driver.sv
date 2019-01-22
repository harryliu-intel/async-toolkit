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

    logic [2:0] epl0_seg;
    logic [2:0] epl0_ptr_req_cnt;
    
    task drive_epl0;
        input [3:0] port;
        input       sop;
        input       eop;
        input int   data_mult;
        input [23:0] md;
        
        begin
            i_shim_pb_v_p0[port][epl0_seg]      = 1'b1;

            if( epl0_seg == 0 ) begin
                for( int i=0; i<8; i++ ) begin
                    i_shim_pb_data_p0[port].seg0[i] = 72'((i+1)*data_mult);
                end
                
                i_shim_pb_md_p0[port].md0        = shim_md_t'(md);
                i_shim_pb_md_p0[port].md0.md.sop = sop;
                i_shim_pb_md_p0[port].md0.md.eop = eop;
                
                @(posedge dut_if.clk);
                i_shim_pb_data_p0[port].seg0 = {8{72'h00}};
            end
            if( epl0_seg == 1 ) begin
                for( int i=0; i<8; i++ ) begin
                    i_shim_pb_data_p0[port].seg1[i] = 72'((i+1)*data_mult);
                end
                
                i_shim_pb_md_p0[port].md1        = shim_md_t'(md);
                i_shim_pb_md_p0[port].md1.md.sop = sop;
                i_shim_pb_md_p0[port].md1.md.eop = eop;
                
                @(posedge dut_if.clk);
                i_shim_pb_data_p0[port].seg1 = {8{72'h00}};
            end
            if( epl0_seg == 2 ) begin
                for( int i=0; i<8; i++ ) begin
                    i_shim_pb_data_p0[port].seg2[i] = 72'((i+1)*data_mult);
                end
                
                i_shim_pb_md_p0[port].md2        = shim_md_t'(md);
                i_shim_pb_md_p0[port].md2.md.sop = sop;
                i_shim_pb_md_p0[port].md2.md.eop = eop;
                
                @(posedge dut_if.clk);
                i_shim_pb_data_p0[port].seg2 = {8{72'h00}};
            end
            i_shim_pb_md_p0[port]        = shim_pb_md_t'('h0);
            i_shim_pb_v_p0[port][epl0_seg]    = 1'b0;

            epl0_seg = (epl0_seg) == 2 ? 0 : epl0_seg + 1;

        end
    endtask // drive_epl0

   `define PRE_PPE_PATH top.pre_post_wrap.mby_igr_pre_ppe
    task update_epl0_ptr;
        input [19:0] seg_ptr;
        input [3:0]  sema;
        static logic [19:0] seg_ptr_temp;
        static logic [3:0]  sema_temp;
        
        begin
            seg_ptr_temp = seg_ptr;
            sema_temp = sema;
            
            // forcing directly to pre_ppe to avoid dut_if flop delays
            //  this is a hack for first packet 2.0/3.0 testing
            //  priority to implementation of real pointer cache in post_ppe
            $display("%0t update ep0 ptr req cnt %0h ", $realtime,
                     epl0_ptr_req_cnt);
                     
            @(posedge dut_if.clk);
            wait( epl0_ptr_req_cnt > 0 ) begin
                @(posedge dut_if.clk);
            end
            epl0_ptr_req_cnt = epl0_ptr_req_cnt - 1;
            
            $display("%0t update ep0 ptr %0h %4b", $realtime, seg_ptr, sema);
            force `PRE_PPE_PATH.i_free_ptr_valid          = 4'b0001;
            force `PRE_PPE_PATH.i_free_seg_ptr[0]         = seg_ptr_temp;
            force `PRE_PPE_PATH.i_free_sema[0]            = sema_temp;
            @(posedge dut_if.clk);
            force `PRE_PPE_PATH.i_free_ptr_valid          = 4'b0000;
        end

    endtask // update_epl1_ptr
    
    // Drive requests into DUT (template)
    task drive_reqs();

        epl0_seg = 0;
        epl0_ptr_req_cnt = 0;
        
        
        if (!drove_reqs) begin
            repeat (10) @(posedge dut_if.clk);

            fork
                begin : ptr_cache
                    //  FIXME -- for first packet 3.0
                    //           this block models the ptr cache pushing free pointers to pre_ppe for EPL0
                    //           each cycle that pre_ppe asserts free_ptr_req indicates another pointer to be sent
                    //           epl0_ptr_req_cnt keeps track of outstanding requests
                    //           the real pointer cache interface may be nothing like this...
                    fork
                        begin
                            @(posedge dut_if.clk);
                            i_free_ptr_valid          = 4'b0001;
                            i_free_seg_ptr[0]         = 20'h11111;
                            i_free_sema[0]            = 4'b1010;
                            
                            @(posedge dut_if.clk);
                            i_free_ptr_valid          = 4'b0000;
                            @(posedge dut_if.clk);
                            
                            @(posedge dut_if.clk);
                            i_free_ptr_valid          = 4'b0001;
                            i_free_seg_ptr[0]         = 20'h22222;
                            i_free_sema[0]            = 4'b1010;
                            
                            @(posedge dut_if.clk);
                            i_free_ptr_valid          = 4'b0000;
                            @(posedge dut_if.clk);
                            
                            update_epl0_ptr( 20'h33333, 4'b1111);
                            update_epl0_ptr( 20'h44444, 4'b0011);
                            update_epl0_ptr( 20'h55555, 4'b0011);
                            update_epl0_ptr( 20'h66666, 4'b0011);
                            update_epl0_ptr( 20'h77777, 4'b0011);
                            update_epl0_ptr( 20'h88888, 4'b0011);
                            update_epl0_ptr( 20'h99999, 4'b0011);
                        end
                        forever begin
                            @(negedge dut_if.clk);
                            if( `PRE_PPE_PATH.o_free_ptr_req ) epl0_ptr_req_cnt = epl0_ptr_req_cnt + 1;
                        end
                    join
                    
                end

                begin : pbb
                    repeat (10) @(posedge dut_if.clk);
                    $display("%0t #1 --------------------------------------------------------", $realtime);
                    // first packet -- port 0
                    //   placeholder for free pointers from pointer cache
                    // 64B word valid
                    drive_epl0( 0, 1, 1, 'h1, 24'h123456);

                    repeat (300) @(posedge dut_if.clk);

                    $display("%0t #2 --------------------------------------------------------", $realtime);
                    // packet #2 -- port 0
                    // 64B word valid
                    drive_epl0( 0, 1, 1, 'h2, 24'h123422);

                    
                    repeat (300) @(posedge dut_if.clk);

                    $display("%0t #3 --------------------------------------------------------", $realtime);
                    // packet #3 -- port 0

                    drive_epl0( 0, 1, 0, 'h3, 24'h123433);
                    drive_epl0( 0, 0, 0, 'h4, 24'haaaaaa);
                    drive_epl0( 0, 0, 0, 'h5, 24'haaaaaa);

                    // 2nd segment starts here
                    drive_epl0( 0, 0, 1, 'h6, 24'haaaaaa);

                    repeat (300) @(posedge dut_if.clk);

                    $display("%0t #4 --------------------------------------------------------", $realtime);
                    // packet #4 -- port 0

                    drive_epl0( 0, 1, 0, 'h4, 24'h123444);
                     //  input [3:0] port;
                     //  input       sop;
                     //  input       eop;
                     //  input int   data_mult;
                     //  input [23:0] md;
                    drive_epl0( 0, 0, 0, 'h5, 24'h123555);
                    drive_epl0( 0, 0, 0, 'h6, 24'h123555);

                    drive_epl0( 0, 0, 0, 'h7, 24'h123555);
                    drive_epl0( 0, 0, 0, 'h8, 24'h123555);
                    drive_epl0( 0, 0, 0, 'h9, 24'h123555);
                    drive_epl0( 0, 0, 0, 'ha, 24'h123555);

                    drive_epl0( 0, 0, 1, 'hb, 24'h123555);

                    

                    repeat (300) @(posedge dut_if.clk);
                    disable ptr_cache;
                    
                    $display("%0t #end --------------------------------------------------------", $realtime);
                    
                    drove_reqs = 1; 
                end // block: pbb
            join
            
        end // if (!drove_reqs)
        

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
