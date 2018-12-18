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

class inp_driver;

//    stimulus                stim;           // object for generating stimulus
//    configuration           cfg;            // config object in stim creation 
//
    virtual msh_node_dut_if     dut_if;           // input driver drives inputs in this interface
//
//    tmpl_pkg::req_in_t      req_fifo[$];    // a queue to hold incoming requests
//
//    tmpl_pkg::enc_inp_t     iport;          // input port number
    integer                 clk_cnt;        // counts clocks
//    integer                 num_reqs;       // number of input requests
    bit                     drv_done;       // set when driver is done
    string                  name;           // input driver name used in $display statements
    integer                 drove_reqs;

    mby_msh_pkg::msh_row_rd_req_t    drvr_rd_req_to_dut;
    mby_msh_pkg::msh_row_rd_req_t    drvr_rd_req_to_dut_p1;
    mby_msh_pkg::msh_row_wr_req_t    drvr_wr_req_to_dut;
    mby_msh_pkg::msh_row_wr_req_t    drvr_wr_req_to_dut_p1;
    mby_msh_pkg::msh_data_t          drvr_wr_data_to_dut;
    mby_msh_pkg::msh_data_t          drvr_wr_data_to_dut_p1;


    mby_msh_pkg::mshnd_addr_t	adr_1;
    mby_msh_pkg::mshnd_addr_t	adr_2;
    mby_msh_pkg::mshnd_addr_t	adr_3;
    mby_msh_pkg::mshnd_addr_t	adr_4;
    mby_msh_pkg::mshnd_addr_t	adr_5;
    mby_msh_pkg::mshnd_addr_t	adr_6;

    mby_msh_pkg::mshnd_addr_t	wadr;	// wr adr
    mby_msh_pkg::msh_data_t     wdata;	// wr data
    mby_msh_pkg::msh_rd_id_t	rid;	// rd req id

    integer  knob_inp_req_num;

    function new(

//        tmpl_pkg::enc_inp_t     iport, 
        virtual msh_node_dut_if     dut_if,
	integer  knob_inp_req_num
//        configuration           cfg

    );

        this.dut_if = dut_if;
//        this.cfg    = cfg;

	this.knob_inp_req_num = knob_inp_req_num;

        name        = "inp_driver.sv";
//        stim = new(
//            .cfg    (cfg),
//            .iport  (iport)
//        );

        clk_cnt = 0;
        drove_reqs    = 0;

        // $display("(time: %0d) %s: Display knob_inp_req_num = ", $time, name, knob_inp_req_num);

    endfunction

    // reset input driver
    task reset();
        drv_done        = 1'b0;
//        req_fifo        = {};           // initialize to empty queue
        drvr_rd_req_to_dut = '0;
        drvr_rd_req_to_dut_p1 = '0;
        drvr_wr_req_to_dut = '0;
        drvr_wr_req_to_dut_p1 = '0;
        drvr_wr_data_to_dut = '0;
        drvr_wr_data_to_dut_p1 = '0;

        drove_reqs    = 0;

	adr_1 = $urandom();
	adr_2 = $urandom();
	adr_3 = $urandom();
	adr_4 = $urandom();
	adr_5 = $urandom();
	adr_6 = $urandom();

    endtask

    // connect signal defined in input driver to DUT interface
    task connect_to_DUT_inputs();
        forever begin
            @(posedge dut_if.mclk);
            clk_cnt++;      // keep track of number of clocks
          
            
            // get inputs from drive_reqs() task
            
            drvr_rd_req_to_dut_p1  <= drvr_rd_req_to_dut;
            drvr_wr_req_to_dut_p1  <= drvr_wr_req_to_dut;
            drvr_wr_data_to_dut_p1 <= drvr_wr_data_to_dut;

            // drive DUT inputs 

            dut_if.i_nb_wr_req[0]   = '0;
            dut_if.i_nb_wr_data[0]  = '0;
            dut_if.i_nb_rd_req[0]   = '0;
            dut_if.i_nb_rd_rsp[0]   = '0;
            dut_if.i_nb_rd_data[0]  = '0;

            dut_if.i_sb_wr_req[0]   = '0;
            dut_if.i_sb_wr_data[0]  = '0;
            dut_if.i_sb_rd_req[0]   = '0;
            dut_if.i_sb_rd_rsp[0]   = '0;
            dut_if.i_sb_rd_data[0]  = '0;

            dut_if.i_eb_wr_req[0]   = drvr_wr_req_to_dut_p1;
            dut_if.i_eb_wr_data[0]  = drvr_wr_data_to_dut_p1;
            dut_if.i_eb_rd_req[0]   = drvr_rd_req_to_dut_p1;
            dut_if.i_eb_rd_rsp[0]   = '0;
            dut_if.i_eb_rd_data[0]  = '0;

            dut_if.i_wb_wr_req[0]   = '0;
            dut_if.i_wb_wr_data[0]  = '0;
            dut_if.i_wb_rd_req[0]   = '0;
            dut_if.i_wb_rd_rsp[0]   = '0;
            dut_if.i_wb_rd_data[0]  = '0;

            dut_if.i_nb_wr_req[1]   = '0;
            dut_if.i_nb_wr_data[1]  = '0;
            dut_if.i_nb_rd_req[1]   = '0;
            dut_if.i_nb_rd_rsp[1]   = '0;
            dut_if.i_nb_rd_data[1]  = '0;

            dut_if.i_sb_wr_req[1]   = '0;
            dut_if.i_sb_wr_data[1]  = '0;
            dut_if.i_sb_rd_req[1]   = '0;
            dut_if.i_sb_rd_rsp[1]   = '0;
            dut_if.i_sb_rd_data[1]  = '0;

            dut_if.i_eb_wr_req[1]   = '0;
            dut_if.i_eb_wr_data[1]  = '0;
            dut_if.i_eb_rd_req[1]   = '0;
            dut_if.i_eb_rd_rsp[1]   = '0;
            dut_if.i_eb_rd_data[1]  = '0;

            dut_if.i_wb_wr_req[1]   = '0;
            dut_if.i_wb_wr_data[1]  = '0;
            dut_if.i_wb_rd_req[1]   = '0;
            dut_if.i_wb_rd_rsp[1]   = '0;
            dut_if.i_wb_rd_data[1]  = '0;
    
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


/*
    // Drive requests into DUT (template)
    task drive_reqs();

        if (!drove_reqs) begin

            @(posedge dut_if.mclk);

	// 1st wr req
            drvr_wr_req_to_dut.vld      = 1'b1;
            drvr_wr_req_to_dut.node_col = '0;
            drvr_wr_req_to_dut.node_row = '0;
            drvr_wr_req_to_dut.csr      = '0;
         // drvr_wr_req_to_dut.addr     = mby_msh_pkg::mshnd_addr_t'('h10);
            drvr_wr_req_to_dut.addr     = adr_1;
            drvr_wr_req_to_dut.age      = mby_msh_pkg::msh_trans_age_t'('h51);

            @(posedge dut_if.mclk);

            drvr_wr_req_to_dut.vld      = 1'b0;
            drvr_wr_req_to_dut.node_col = '0;
            drvr_wr_req_to_dut.node_row = '0;
            drvr_wr_req_to_dut.csr      = '0;
            drvr_wr_req_to_dut.addr     = mby_msh_pkg::mshnd_addr_t'(0);
            drvr_wr_req_to_dut.age      = mby_msh_pkg::msh_trans_age_t'(0);

            @(posedge dut_if.mclk);

	// 1st wr data
         // drvr_wr_data_to_dut    = mby_msh_pkg::msh_data_t'('ha1a1);
            drvr_wr_data_to_dut    = $urandom();

            @(posedge dut_if.mclk);
            @(posedge dut_if.mclk);
            @(posedge dut_if.mclk);
            @(posedge dut_if.mclk);

	// 1st rd req
            drvr_rd_req_to_dut.vld      = 1'b1;
            drvr_rd_req_to_dut.id       = mby_msh_pkg::msh_rd_id_t'(1);
            drvr_rd_req_to_dut.node_col = '0;
            drvr_rd_req_to_dut.node_row = '0;
            drvr_rd_req_to_dut.csr      = '0;
         // drvr_rd_req_to_dut.addr     = mby_msh_pkg::mshnd_addr_t'('h10);
            drvr_rd_req_to_dut.addr     = adr_1;
            drvr_rd_req_to_dut.sema_vld = 1'b0;
            drvr_rd_req_to_dut.sema_val = 1'b0;
            drvr_rd_req_to_dut.age      = mby_msh_pkg::msh_trans_age_t'('h15);

            @(posedge dut_if.mclk);

            drvr_rd_req_to_dut.vld      = 1'b0;
            drvr_rd_req_to_dut.id       = mby_msh_pkg::msh_rd_id_t'(0);
            drvr_rd_req_to_dut.node_col = '0;
            drvr_rd_req_to_dut.node_row = '0;
            drvr_rd_req_to_dut.csr      = '0;
            drvr_rd_req_to_dut.addr     = mby_msh_pkg::mshnd_addr_t'(0);
            drvr_rd_req_to_dut.sema_vld = 1'b0;
            drvr_rd_req_to_dut.sema_val = 1'b0;
            drvr_rd_req_to_dut.age      = mby_msh_pkg::msh_trans_age_t'(0);

            @(posedge dut_if.mclk);
            @(posedge dut_if.mclk);
            @(posedge dut_if.mclk);
            @(posedge dut_if.mclk);
            @(posedge dut_if.mclk);
            @(posedge dut_if.mclk);
            @(posedge dut_if.mclk);
            @(posedge dut_if.mclk);
            @(posedge dut_if.mclk);
            @(posedge dut_if.mclk);
            @(posedge dut_if.mclk);
            @(posedge dut_if.mclk);
            @(posedge dut_if.mclk);
            @(posedge dut_if.mclk);
            @(posedge dut_if.mclk);
            @(posedge dut_if.mclk);
            @(posedge dut_if.mclk);
            @(posedge dut_if.mclk);
            @(posedge dut_if.mclk);
            @(posedge dut_if.mclk);

	// 2nd wr req, wr data and rd req:

            drvr_wr_req_to_dut.vld      = 1'b1;
            drvr_wr_req_to_dut.node_col = '0;
            drvr_wr_req_to_dut.node_row = '0;
            drvr_wr_req_to_dut.csr      = '0;
         // drvr_wr_req_to_dut.addr     = mby_msh_pkg::mshnd_addr_t'('h20);
            drvr_wr_req_to_dut.addr     = adr_2;
            drvr_wr_req_to_dut.age      = mby_msh_pkg::msh_trans_age_t'('h52);
            @(posedge dut_if.mclk);

            drvr_wr_req_to_dut.vld      = 1'b0;
            drvr_wr_req_to_dut.node_col = '0;
            drvr_wr_req_to_dut.node_row = '0;
            drvr_wr_req_to_dut.csr      = '0;
            drvr_wr_req_to_dut.addr     = mby_msh_pkg::mshnd_addr_t'(0);
            drvr_wr_req_to_dut.age      = mby_msh_pkg::msh_trans_age_t'(0);
            @(posedge dut_if.mclk);

         // drvr_wr_data_to_dut    = mby_msh_pkg::msh_data_t'('ha2a2);
            drvr_wr_data_to_dut    = $urandom();
            @(posedge dut_if.mclk);
            @(posedge dut_if.mclk);
            @(posedge dut_if.mclk);
            @(posedge dut_if.mclk);

            drvr_rd_req_to_dut.vld      = 1'b1;
            drvr_rd_req_to_dut.id       = mby_msh_pkg::msh_rd_id_t'(1);
            drvr_rd_req_to_dut.node_col = '0;
            drvr_rd_req_to_dut.node_row = '0;
            drvr_rd_req_to_dut.csr      = '0;
         // drvr_rd_req_to_dut.addr     = mby_msh_pkg::mshnd_addr_t'('h20);
            drvr_rd_req_to_dut.addr     = adr_2;
            drvr_rd_req_to_dut.sema_vld = 1'b0;
            drvr_rd_req_to_dut.sema_val = 1'b0;
            drvr_rd_req_to_dut.age      = mby_msh_pkg::msh_trans_age_t'('h25);
            @(posedge dut_if.mclk);

            drvr_rd_req_to_dut.vld      = 1'b0;
            drvr_rd_req_to_dut.id       = mby_msh_pkg::msh_rd_id_t'(0);
            drvr_rd_req_to_dut.node_col = '0;
            drvr_rd_req_to_dut.node_row = '0;
            drvr_rd_req_to_dut.csr      = '0;
            drvr_rd_req_to_dut.addr     = mby_msh_pkg::mshnd_addr_t'(0);
            drvr_rd_req_to_dut.sema_vld = 1'b0;
            drvr_rd_req_to_dut.sema_val = 1'b0;
            drvr_rd_req_to_dut.age      = mby_msh_pkg::msh_trans_age_t'(0);

            @(posedge dut_if.mclk);
            @(posedge dut_if.mclk);
            @(posedge dut_if.mclk);
            @(posedge dut_if.mclk);
            @(posedge dut_if.mclk);
            @(posedge dut_if.mclk);
            @(posedge dut_if.mclk);
            @(posedge dut_if.mclk);
            @(posedge dut_if.mclk);
            @(posedge dut_if.mclk);
            @(posedge dut_if.mclk);
            @(posedge dut_if.mclk);
            @(posedge dut_if.mclk);
            @(posedge dut_if.mclk);
            @(posedge dut_if.mclk);
            @(posedge dut_if.mclk);
            @(posedge dut_if.mclk);
            @(posedge dut_if.mclk);
            @(posedge dut_if.mclk);
            @(posedge dut_if.mclk);

	// 3rd wr req, wr data and rd req:

            drvr_wr_req_to_dut.vld      = 1'b1;
            drvr_wr_req_to_dut.node_col = '0;
            drvr_wr_req_to_dut.node_row = '0;
            drvr_wr_req_to_dut.csr      = '0;
         // drvr_wr_req_to_dut.addr     = mby_msh_pkg::mshnd_addr_t'('h30);
            drvr_wr_req_to_dut.addr     = adr_3;
            drvr_wr_req_to_dut.age      = mby_msh_pkg::msh_trans_age_t'('h53);
            @(posedge dut_if.mclk);

            drvr_wr_req_to_dut.vld      = 1'b0;
            drvr_wr_req_to_dut.node_col = '0;
            drvr_wr_req_to_dut.node_row = '0;
            drvr_wr_req_to_dut.csr      = '0;
            drvr_wr_req_to_dut.addr     = mby_msh_pkg::mshnd_addr_t'(0);
            drvr_wr_req_to_dut.age      = mby_msh_pkg::msh_trans_age_t'(0);
            @(posedge dut_if.mclk);

         // drvr_wr_data_to_dut    = mby_msh_pkg::msh_data_t'('ha3a3);
            drvr_wr_data_to_dut    = $urandom();
            @(posedge dut_if.mclk);
            @(posedge dut_if.mclk);
            @(posedge dut_if.mclk);
            @(posedge dut_if.mclk);

            drvr_rd_req_to_dut.vld      = 1'b1;
            drvr_rd_req_to_dut.id       = mby_msh_pkg::msh_rd_id_t'(1);
            drvr_rd_req_to_dut.node_col = '0;
            drvr_rd_req_to_dut.node_row = '0;
            drvr_rd_req_to_dut.csr      = '0;
         // drvr_rd_req_to_dut.addr     = mby_msh_pkg::mshnd_addr_t'('h30);
            drvr_rd_req_to_dut.addr     = adr_3;
            drvr_rd_req_to_dut.sema_vld = 1'b0;
            drvr_rd_req_to_dut.sema_val = 1'b0;
            drvr_rd_req_to_dut.age      = mby_msh_pkg::msh_trans_age_t'('h35);
            @(posedge dut_if.mclk);

            drvr_rd_req_to_dut.vld      = 1'b0;
            drvr_rd_req_to_dut.id       = mby_msh_pkg::msh_rd_id_t'(0);
            drvr_rd_req_to_dut.node_col = '0;
            drvr_rd_req_to_dut.node_row = '0;
            drvr_rd_req_to_dut.csr      = '0;
            drvr_rd_req_to_dut.addr     = mby_msh_pkg::mshnd_addr_t'(0);
            drvr_rd_req_to_dut.sema_vld = 1'b0;
            drvr_rd_req_to_dut.sema_val = 1'b0;
            drvr_rd_req_to_dut.age      = mby_msh_pkg::msh_trans_age_t'(0);

            @(posedge dut_if.mclk);
            @(posedge dut_if.mclk);
            @(posedge dut_if.mclk);
            @(posedge dut_if.mclk);
            @(posedge dut_if.mclk);
            @(posedge dut_if.mclk);
            @(posedge dut_if.mclk);
            @(posedge dut_if.mclk);
            @(posedge dut_if.mclk);
            @(posedge dut_if.mclk);
            @(posedge dut_if.mclk);
            @(posedge dut_if.mclk);
            @(posedge dut_if.mclk);
            @(posedge dut_if.mclk);
            @(posedge dut_if.mclk);
            @(posedge dut_if.mclk);
            @(posedge dut_if.mclk);
            @(posedge dut_if.mclk);
            @(posedge dut_if.mclk);
            @(posedge dut_if.mclk);

	// 4th & 5th: 2 wr reqs and wr data first, then 2 read followed

	    // 4th wr
            drvr_wr_req_to_dut.vld      = 1'b1;
            drvr_wr_req_to_dut.node_col = '0;
            drvr_wr_req_to_dut.node_row = '0;
            drvr_wr_req_to_dut.csr      = '0;
         // drvr_wr_req_to_dut.addr     = mby_msh_pkg::mshnd_addr_t'('h40);
            drvr_wr_req_to_dut.addr     = adr_4;
            drvr_wr_req_to_dut.age      = mby_msh_pkg::msh_trans_age_t'('h54);
            @(posedge dut_if.mclk);

            drvr_wr_req_to_dut.vld      = 1'b0;
            @(posedge dut_if.mclk);

         // drvr_wr_data_to_dut    = mby_msh_pkg::msh_data_t'('ha4a4);
            drvr_wr_data_to_dut    = $urandom();
            @(posedge dut_if.mclk);
            @(posedge dut_if.mclk);
            @(posedge dut_if.mclk);

	    // 5th wr
            drvr_wr_req_to_dut.vld      = 1'b1;
         // drvr_wr_req_to_dut.addr     = mby_msh_pkg::mshnd_addr_t'('h50);
            drvr_wr_req_to_dut.addr     = adr_5;
            drvr_wr_req_to_dut.age      = mby_msh_pkg::msh_trans_age_t'('h55);
            @(posedge dut_if.mclk);

            drvr_wr_req_to_dut.vld      = 1'b0;
            @(posedge dut_if.mclk);

         // drvr_wr_data_to_dut    = mby_msh_pkg::msh_data_t'('ha5a5);
            drvr_wr_data_to_dut    = $urandom();

            @(posedge dut_if.mclk);
            @(posedge dut_if.mclk);
            @(posedge dut_if.mclk);
            @(posedge dut_if.mclk);
            @(posedge dut_if.mclk);
            @(posedge dut_if.mclk);
            @(posedge dut_if.mclk);
            @(posedge dut_if.mclk);
            @(posedge dut_if.mclk);
            @(posedge dut_if.mclk);
            @(posedge dut_if.mclk);
            @(posedge dut_if.mclk);
            @(posedge dut_if.mclk);
            @(posedge dut_if.mclk);
            @(posedge dut_if.mclk);
            @(posedge dut_if.mclk);
            @(posedge dut_if.mclk);
            @(posedge dut_if.mclk);
            @(posedge dut_if.mclk);
            @(posedge dut_if.mclk);

	    // 4th rd
            drvr_rd_req_to_dut.vld      = 1'b1;
            drvr_rd_req_to_dut.id       = mby_msh_pkg::msh_rd_id_t'(1);
         // drvr_rd_req_to_dut.addr     = mby_msh_pkg::mshnd_addr_t'('h40);
            drvr_rd_req_to_dut.addr     = adr_4;
            drvr_rd_req_to_dut.age      = mby_msh_pkg::msh_trans_age_t'('h45);
            @(posedge dut_if.mclk);

            drvr_rd_req_to_dut.vld      = 1'b0;
            @(posedge dut_if.mclk);

            @(posedge dut_if.mclk);
            @(posedge dut_if.mclk);
            @(posedge dut_if.mclk);
            @(posedge dut_if.mclk);

	    // 5th rd
            drvr_rd_req_to_dut.vld      = 1'b1;
            drvr_rd_req_to_dut.id       = mby_msh_pkg::msh_rd_id_t'(1);
         // drvr_rd_req_to_dut.addr     = mby_msh_pkg::mshnd_addr_t'('h50);
            drvr_rd_req_to_dut.addr     = adr_5;
            drvr_rd_req_to_dut.age      = mby_msh_pkg::msh_trans_age_t'('h55);
            @(posedge dut_if.mclk);

            drvr_rd_req_to_dut.vld      = 1'b0;
            @(posedge dut_if.mclk);

            @(posedge dut_if.mclk);
            @(posedge dut_if.mclk);
            @(posedge dut_if.mclk);
            @(posedge dut_if.mclk);
            @(posedge dut_if.mclk);
            @(posedge dut_if.mclk);
            @(posedge dut_if.mclk);
            @(posedge dut_if.mclk);
            @(posedge dut_if.mclk);
            @(posedge dut_if.mclk);
            @(posedge dut_if.mclk);
            @(posedge dut_if.mclk);
            @(posedge dut_if.mclk);
            @(posedge dut_if.mclk);
            @(posedge dut_if.mclk);
            @(posedge dut_if.mclk);
            @(posedge dut_if.mclk);
            @(posedge dut_if.mclk);
            @(posedge dut_if.mclk);
            @(posedge dut_if.mclk);

        // 6th wr req, wr data and rd req:

            drvr_wr_req_to_dut.vld      = 1'b1;
            drvr_wr_req_to_dut.node_col = '0;
            drvr_wr_req_to_dut.node_row = '0;
            drvr_wr_req_to_dut.csr      = '0;
         // drvr_wr_req_to_dut.addr     = mby_msh_pkg::mshnd_addr_t'('h60);
            drvr_wr_req_to_dut.addr     = adr_6;
            drvr_wr_req_to_dut.age      = mby_msh_pkg::msh_trans_age_t'('h63);
            @(posedge dut_if.mclk);

            drvr_wr_req_to_dut.vld      = 1'b0;
            drvr_wr_req_to_dut.node_col = '0;
            drvr_wr_req_to_dut.node_row = '0;
            drvr_wr_req_to_dut.csr      = '0;
            drvr_wr_req_to_dut.addr     = mby_msh_pkg::mshnd_addr_t'(0);
            drvr_wr_req_to_dut.age      = mby_msh_pkg::msh_trans_age_t'(0);
            @(posedge dut_if.mclk);

         // drvr_wr_data_to_dut    = mby_msh_pkg::msh_data_t'('ha6a6);
            drvr_wr_data_to_dut    = $urandom();
            @(posedge dut_if.mclk);
            @(posedge dut_if.mclk);
            @(posedge dut_if.mclk);
            @(posedge dut_if.mclk);

            drvr_rd_req_to_dut.vld      = 1'b1;
            drvr_rd_req_to_dut.id       = mby_msh_pkg::msh_rd_id_t'(1);
            drvr_rd_req_to_dut.node_col = '0;
            drvr_rd_req_to_dut.node_row = '0;
            drvr_rd_req_to_dut.csr      = '0;
         // drvr_rd_req_to_dut.addr     = mby_msh_pkg::mshnd_addr_t'('h60);
            drvr_rd_req_to_dut.addr     = adr_6;
            drvr_rd_req_to_dut.sema_vld = 1'b0;
            drvr_rd_req_to_dut.sema_val = 1'b0;
            drvr_rd_req_to_dut.age      = mby_msh_pkg::msh_trans_age_t'('h65);
            @(posedge dut_if.mclk);

            drvr_rd_req_to_dut.vld      = 1'b0;
            drvr_rd_req_to_dut.id       = mby_msh_pkg::msh_rd_id_t'(0);
            drvr_rd_req_to_dut.node_col = '0;
            drvr_rd_req_to_dut.node_row = '0;
            drvr_rd_req_to_dut.csr      = '0;
            drvr_rd_req_to_dut.addr     = mby_msh_pkg::mshnd_addr_t'(0);
            drvr_rd_req_to_dut.sema_vld = 1'b0;
            drvr_rd_req_to_dut.sema_val = 1'b0;
            drvr_rd_req_to_dut.age      = mby_msh_pkg::msh_trans_age_t'(0);

            @(posedge dut_if.mclk);
            @(posedge dut_if.mclk);
            @(posedge dut_if.mclk);
            @(posedge dut_if.mclk);
            @(posedge dut_if.mclk);

            @(posedge dut_if.mclk);
            @(posedge dut_if.mclk);
            @(posedge dut_if.mclk);
            @(posedge dut_if.mclk);
            @(posedge dut_if.mclk);

            @(posedge dut_if.mclk);
            @(posedge dut_if.mclk);
            @(posedge dut_if.mclk);
            @(posedge dut_if.mclk);
            @(posedge dut_if.mclk);

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
*/


    // Drive requests into DUT (template)
    task drive_reqs();

        if (!drove_reqs) begin

            @(posedge dut_if.mclk);


            drvr_wr_req_to_dut.node_col = '0;
            drvr_wr_req_to_dut.node_row = '0;
            drvr_wr_req_to_dut.csr      = '0;
            drvr_rd_req_to_dut.sema_vld = 1'b0;
            drvr_rd_req_to_dut.sema_val = 1'b0;


            for (integer req_loop = 0; req_loop < knob_inp_req_num; req_loop++) begin

	       wadr = $urandom();
	       wdata = $urandom();
 	       rid = $urandom();

               // wr req, wr data:

               @(posedge dut_if.mclk);
               drvr_wr_req_to_dut.vld      = 1'b1;
               drvr_wr_req_to_dut.addr     = wadr;
               drvr_wr_req_to_dut.age      = $urandom();

               @(posedge dut_if.mclk);
               drvr_wr_req_to_dut.vld      = 1'b0;

               @(posedge dut_if.mclk);
               drvr_wr_data_to_dut    = wdata;

               @(posedge dut_if.mclk);
               @(posedge dut_if.mclk);
               @(posedge dut_if.mclk);
               @(posedge dut_if.mclk);
            
               // rd req
               @(posedge dut_if.mclk);
               drvr_rd_req_to_dut.vld      = 1'b1;
               drvr_rd_req_to_dut.id       = rid;
               drvr_rd_req_to_dut.addr     = wadr;
               drvr_rd_req_to_dut.age      = $urandom();

               @(posedge dut_if.mclk);
               drvr_rd_req_to_dut.vld      = 1'b0;

               @(posedge dut_if.mclk);
               @(posedge dut_if.mclk);
               @(posedge dut_if.mclk);
               @(posedge dut_if.mclk);
               @(posedge dut_if.mclk);
               @(posedge dut_if.mclk);
               @(posedge dut_if.mclk);
               @(posedge dut_if.mclk);
               @(posedge dut_if.mclk);
               @(posedge dut_if.mclk);
               @(posedge dut_if.mclk);
               @(posedge dut_if.mclk);
               @(posedge dut_if.mclk);
               @(posedge dut_if.mclk);
               @(posedge dut_if.mclk);
               @(posedge dut_if.mclk);
               @(posedge dut_if.mclk);
               @(posedge dut_if.mclk);
               @(posedge dut_if.mclk);
               @(posedge dut_if.mclk);

               $display("(time: %0d) %s: ** (req num = %0d) ** ", $time, name, req_loop);

            end		// end loop 
 
            drove_reqs = 1;

        end

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
