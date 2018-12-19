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

import mby_msh_pkg::*;
import mby_egr_pkg::*;
class inp_driver;
//import mby_msh_pkg::*;

//    stimulus                stim;           // object for generating stimulus
//    configuration           cfg;            // config object in stim creation 
//
    virtual msh_dut_if     dut_if;           // input driver drives inputs in this interface
//
//    tmpl_pkg::req_in_t      req_fifo[$];    // a queue to hold incoming requests
//
//    tmpl_pkg::enc_inp_t     iport;          // input port number
    integer                 clk_cnt;        // counts clocks
//    integer                 num_reqs;       // number of input requests
    bit                     drv_done;       // set when driver is done
    string                  name;           // input driver name used in $display statements
    integer                 drove_reqs;

     
                 seg_ptr_t           drvr_rd_req_to_dut;
                 seg_ptr_t           drvr_rd_req_to_dut_p1;
                 seg_ptr_t           drvr_wr_req_to_dut;
                 seg_ptr_t           drvr_wr_req_to_dut_p1;

                 logic               drvr_rd_req_to_dut_vld;
                 logic               drvr_rd_req_to_dut_vld_p1;
                 logic               drvr_wr_req_to_dut_vld;
                 logic               drvr_wr_req_to_dut_vld_p1;

                 msh_data_t          drvr_wr_data_to_dut;
                 msh_data_t          drvr_wr_data_to_dut_p1;


    function new(

//        tmpl_pkg::enc_inp_t     iport, 
        virtual msh_dut_if     dut_if
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
//        drvr_req_to_dut = '0;
        drvr_wr_req_to_dut_vld    = '0;
        drvr_rd_req_to_dut_vld    = '0;
        drvr_wr_req_to_dut_vld_p1 = '0;
        drvr_rd_req_to_dut_vld_p1 = '0;
        drvr_rd_req_to_dut        = '0;
        drvr_rd_req_to_dut_p1     = '0;
        drvr_wr_req_to_dut        = '0;
        drvr_wr_req_to_dut_p1     = '0;
        drvr_wr_data_to_dut       = '0;
        drvr_wr_data_to_dut_p1    = '0;

        drove_reqs    = 0;

    endtask

    // connect signal defined in input driver to DUT interface
    task connect_to_DUT_inputs();
        forever begin
            @(posedge dut_if.mclk);
            clk_cnt++;      // keep track of number of clocks
          
            
            // get inputs from drive_reqs() task
            
            drvr_rd_req_to_dut_vld_p1  <= drvr_rd_req_to_dut_vld;
            drvr_wr_req_to_dut_vld_p1  <= drvr_wr_req_to_dut_vld;
            drvr_rd_req_to_dut_p1  <= drvr_rd_req_to_dut;
            drvr_wr_req_to_dut_p1  <= drvr_wr_req_to_dut;
            drvr_wr_data_to_dut_p1 <= drvr_wr_data_to_dut;


            // drive DUT inputs 

//
//
//
//
            dut_if.i_igr_eb_wreq_valid    [0][0]  = drvr_wr_req_to_dut_vld_p1;
            dut_if.i_igr_eb_wr_seg_ptr    [0][0]  = drvr_wr_req_to_dut_p1;
            dut_if.i_igr_eb_wr_sema       [0][0]  = '0;
            dut_if.i_igr_eb_wr_wd_sel     [0][0]  = '0;
            dut_if.i_igr_eb_wreq_id       [0][0]  = '0;
            dut_if.i_igr_eb_wr_data       [0][0]  = drvr_wr_data_to_dut_p1;
           
   
    // East Side Write Ports
            dut_if.i_igr_wb_wreq_valid    [0][0]  = '0;
            dut_if.i_igr_wb_wr_seg_ptr    [0][0]  = '0;
            dut_if.i_igr_wb_wr_sema       [0][0]  = '0;
            dut_if.i_igr_wb_wr_wd_sel     [0][0]  = '0;
            dut_if.i_igr_wb_wreq_id       [0][0]  = '0;
            dut_if.i_igr_wb_wr_data       [0][0]  = '0;

    
    //////////////////////////////
    // Mesh MGP Read Request Ports      FIXME:  add _rreq to signal names
    //////////////////////////////

    // West Side Read Request Ports 
            dut_if.i_egr_eb_rreq_valid    [0][0]  = drvr_rd_req_to_dut_vld_p1;
            dut_if.i_egr_eb_seg_ptr       [0][0]  = drvr_rd_req_to_dut_p1;
            dut_if.i_egr_eb_sema          [0][0]  = '0;
            dut_if.i_egr_eb_wd_sel        [0][0]  = '0;
            dut_if.i_egr_eb_req_id        [0][0]  = '0;
    
        
    // East Side Read Request Ports 
            dut_if.i_egr_wb_rreq_valid    [0][0]  = '0;
            dut_if.i_egr_wb_seg_ptr       [0][0]  = '0;
            dut_if.i_egr_wb_sema          [0][0]  = '0;
            dut_if.i_egr_wb_wd_sel        [0][0]  = '0;
            dut_if.i_egr_wb_req_id        [0][0]  = '0;
    
//
// Port 1
//
            dut_if.i_igr_eb_wreq_valid    [0][1]  = '0;
            dut_if.i_igr_eb_wr_seg_ptr    [0][1]  = '0;
            dut_if.i_igr_eb_wr_sema       [0][1]  = '0;
            dut_if.i_igr_eb_wr_wd_sel     [0][1]  = '0;
            dut_if.i_igr_eb_wreq_id       [0][1]  = '0;
            dut_if.i_igr_eb_wr_data       [0][1]  = '0;
           
   
    // East Side Write Ports
            dut_if.i_igr_wb_wreq_valid    [0][1]  = '0;
            dut_if.i_igr_wb_wr_seg_ptr    [0][1]  = '0;
            dut_if.i_igr_wb_wr_sema       [0][1]  = '0;
            dut_if.i_igr_wb_wr_wd_sel     [0][1]  = '0;
            dut_if.i_igr_wb_wreq_id       [0][1]  = '0;
            dut_if.i_igr_wb_wr_data       [0][1]  = '0;

    
    //////////////////////////////
    // Mesh MGP Read Request Ports      FIXME:  add _rreq to signal names
    //////////////////////////////

    // West Side Read Request Ports 
            dut_if.i_egr_eb_rreq_valid    [0][1]  = '0;
            dut_if.i_egr_eb_seg_ptr       [0][1]  = '0;
            dut_if.i_egr_eb_sema          [0][1]  = '0;
            dut_if.i_egr_eb_wd_sel        [0][1]  = '0;
            dut_if.i_egr_eb_req_id        [0][1]  = '0;
    
        
    // East Side Read Request Ports 
            dut_if.i_egr_wb_rreq_valid    [0][1]  = '0;
            dut_if.i_egr_wb_seg_ptr       [0][1]  = '0;
            dut_if.i_egr_wb_sema          [0][1]  = '0;
            dut_if.i_egr_wb_wd_sel        [0][1]  = '0;
            dut_if.i_egr_wb_req_id        [0][1]  = '0;
//
// Port 2
//
            dut_if.i_igr_eb_wreq_valid    [0][2]  = '0;
            dut_if.i_igr_eb_wr_seg_ptr    [0][2]  = '0;
            dut_if.i_igr_eb_wr_sema       [0][2]  = '0;
            dut_if.i_igr_eb_wr_wd_sel     [0][2]  = '0;
            dut_if.i_igr_eb_wreq_id       [0][2]  = '0;
            dut_if.i_igr_eb_wr_data       [0][2]  = '0;
           
   
    // East Side Write Ports
            dut_if.i_igr_wb_wreq_valid    [0][2]  = '0;
            dut_if.i_igr_wb_wr_seg_ptr    [0][2]  = '0;
            dut_if.i_igr_wb_wr_sema       [0][2]  = '0;
            dut_if.i_igr_wb_wr_wd_sel     [0][2]  = '0;
            dut_if.i_igr_wb_wreq_id       [0][2]  = '0;
            dut_if.i_igr_wb_wr_data       [0][2]  = '0;

    
    //////////////////////////////
    // Mesh MGP Read Request Ports      FIXME:  add _rreq to signal names
    //////////////////////////////

    // West Side Read Request Ports 
            dut_if.i_egr_eb_rreq_valid    [0][2]  = '0;
            dut_if.i_egr_eb_seg_ptr       [0][2]  = '0;
            dut_if.i_egr_eb_sema          [0][2]  = '0;
            dut_if.i_egr_eb_wd_sel        [0][2]  = '0;
            dut_if.i_egr_eb_req_id        [0][2]  = '0;
    
        
    // East Side Read Request Ports 
            dut_if.i_egr_wb_rreq_valid    [0][2]  = '0;
            dut_if.i_egr_wb_seg_ptr       [0][2]  = '0;
            dut_if.i_egr_wb_sema          [0][2]  = '0;
            dut_if.i_egr_wb_wd_sel        [0][2]  = '0;
            dut_if.i_egr_wb_req_id        [0][2]  = '0;
    
//generate
  for (int gv_row=1; gv_row < NUM_MSH_ROWS; gv_row++) begin : mim_rows
    for (int gv_port=0; gv_port < 3; gv_port++) begin : mim_ports
    // West Side Write Ports
            dut_if.i_igr_eb_wreq_valid    [gv_row][gv_port]  = '0;
            dut_if.i_igr_eb_wr_seg_ptr    [gv_row][gv_port]  = '0;
            dut_if.i_igr_eb_wr_sema       [gv_row][gv_port]  = '0;
            dut_if.i_igr_eb_wr_wd_sel     [gv_row][gv_port]  = '0;
            dut_if.i_igr_eb_wreq_id       [gv_row][gv_port]  = '0;
            dut_if.i_igr_eb_wr_data       [gv_row][gv_port]  = '0;
           
   
    // East Side Write Ports
            dut_if.i_igr_wb_wreq_valid    [gv_row][gv_port]  = '0;
            dut_if.i_igr_wb_wr_seg_ptr    [gv_row][gv_port]  = '0;
            dut_if.i_igr_wb_wr_sema       [gv_row][gv_port]  = '0;
            dut_if.i_igr_wb_wr_wd_sel     [gv_row][gv_port]  = '0;
            dut_if.i_igr_wb_wreq_id       [gv_row][gv_port]  = '0;
            dut_if.i_igr_wb_wr_data       [gv_row][gv_port]  = '0;

    
    //////////////////////////////
    // Mesh MGP Read Request Ports      FIXME:  add _rreq to signal names
    //////////////////////////////

    // West Side Read Request Ports 
            dut_if.i_egr_eb_rreq_valid    [gv_row][gv_port]  = '0;
            dut_if.i_egr_eb_seg_ptr       [gv_row][gv_port]  = '0;
            dut_if.i_egr_eb_sema          [gv_row][gv_port]  = '0;
            dut_if.i_egr_eb_wd_sel        [gv_row][gv_port]  = '0;
            dut_if.i_egr_eb_req_id        [gv_row][gv_port]  = '0;
    
        
    // East Side Read Request Ports 
            dut_if.i_egr_wb_rreq_valid    [gv_row][gv_port]  = '0;
            dut_if.i_egr_wb_seg_ptr       [gv_row][gv_port]  = '0;
            dut_if.i_egr_wb_sema          [gv_row][gv_port]  = '0;
            dut_if.i_egr_wb_wd_sel        [gv_row][gv_port]  = '0;
            dut_if.i_egr_wb_req_id        [gv_row][gv_port]  = '0;
    
  end : mim_ports
end : mim_rows
//endgenerate

    ////////////////////////
    // Mesh GMM Write Ports 
    ////////////////////////
   
    // North Side Write Port
            dut_if.i_sb_wreq_valid   = '0;
            dut_if.i_sb_wr_seg_ptr   = '0;
            dut_if.i_sb_wr_sema      = '0;
            dut_if.i_sb_wr_wd_sel    = '0;
            dut_if.i_sb_wreq_id      = '0;
            dut_if.i_sb_wr_data      = '0;
           
   
    // South Side Write Port
            dut_if.i_nb_wreq_valid   = '0;
            dut_if.i_nb_wr_seg_ptr   = '0;
            dut_if.i_nb_wr_sema      = '0;
            dut_if.i_nb_wr_wd_sel    = '0;
            dut_if.i_nb_wreq_id      = '0;
            dut_if.i_nb_wr_data      = '0;
    

    //////////////////////////////
    // Mesh GMM Read Request Ports
    //////////////////////////////

    // North Side Read Request Port 
            dut_if.i_sb_rreq_valid   = '0;
            dut_if.i_sb_rreq_seg_ptr = '0;
            dut_if.i_sb_rreq_sema    = '0;
            dut_if.i_sb_rreq_wd_sel  = '0;
            dut_if.i_sb_rreq_id      = '0;
    
        
    // South Side Read Request Port 
            dut_if.i_nb_rreq_valid   = '0;
            dut_if.i_nb_rreq_seg_ptr = '0;
            dut_if.i_nb_rreq_sema    = '0;
            dut_if.i_nb_rreq_wd_sel  = '0;
            dut_if.i_nb_rreq_id      = '0;
   
//
//
//
//
    
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
            @(posedge dut_if.mclk);

            drvr_wr_req_to_dut_vld      = 1'b1;
            // drvr_wr_req_to_dut.node_col = '0;
            // drvr_wr_req_to_dut.node_row = '0;
            // drvr_wr_req_to_dut.csr      = '0;
            drvr_wr_req_to_dut          = mby_msh_pkg::mshnd_addr_t'('h33);
            // drvr_wr_req_to_dut.age      = mby_msh_pkg::msh_trans_age_t'('h55);
            drvr_wr_data_to_dut    = mby_msh_pkg::msh_data_t'('ha5a5);

            @(posedge dut_if.mclk);

            drvr_wr_req_to_dut_vld      = 1'b0;
            // drvr_wr_req_to_dut.node_col = '0;
            // drvr_wr_req_to_dut.node_row = '0;
            // drvr_wr_req_to_dut.csr      = '0;
            drvr_wr_req_to_dut          = mby_msh_pkg::mshnd_addr_t'(0);
            // drvr_wr_req_to_dut.age      = mby_msh_pkg::msh_trans_age_t'(0);
            drvr_wr_data_to_dut    = mby_msh_pkg::msh_data_t'('h0000);


            repeat(25) @(posedge dut_if.mclk);

            drvr_rd_req_to_dut_vld      = 1'b1;
            // drvr_rd_req_to_dut.id       = mby_msh_pkg::msh_rd_id_t'(1);
            drvr_rd_req_to_dut          = mby_msh_pkg::mshnd_addr_t'('h33);

            @(posedge dut_if.mclk);

            drvr_rd_req_to_dut_vld      = 1'b0;
            // drvr_rd_req_to_dut.id       = mby_msh_pkg::msh_rd_id_t'(0);
            // drvr_rd_req_to_dut.node_col = '0;
            // drvr_rd_req_to_dut.node_row = '0;
            // drvr_rd_req_to_dut.csr      = '0;
            drvr_rd_req_to_dut          = mby_msh_pkg::mshnd_addr_t'(0);
            // drvr_rd_req_to_dut.sema_vld = 1'b0;
            // drvr_rd_req_to_dut.sema_val = 1'b0;
            // drvr_rd_req_to_dut.age      = mby_msh_pkg::msh_trans_age_t'(0);



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

        @(posedge dut_if.mclk);

        drv_done = 1'b1;
    endtask

    // figure out if input driver is done or not
    function bit something_to_do();
        something_to_do = 0;
//        if (req_fifo.size() != 0) something_to_do = 1;
//        if (drvr_req_to_dut.vld || drvr_req_to_dut_p1.vld) something_to_do = 1;
//        return something_to_do;
    endfunction

endclass

`endif // INP_DRIVER
