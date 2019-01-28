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
    integer                 drv_corr;       // set when driver is done

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


    mby_msh_pkg::msh_row_wr_req_t    drvr_p0_eb_wr_req_to_dut;
    mby_msh_pkg::msh_row_wr_req_t    drvr_p0_eb_wr_req_to_dut_q1;
    mby_msh_pkg::msh_data_t          drvr_p0_eb_wr_data_to_dut;
    mby_msh_pkg::msh_data_t          drvr_p0_eb_wr_data_to_dut_q1;

    mby_msh_pkg::msh_row_wr_req_t    drvr_p0_wb_wr_req_to_dut;
    mby_msh_pkg::msh_row_wr_req_t    drvr_p0_wb_wr_req_to_dut_q1;
    mby_msh_pkg::msh_data_t          drvr_p0_wb_wr_data_to_dut;
    mby_msh_pkg::msh_data_t          drvr_p0_wb_wr_data_to_dut_q1;

    mby_msh_pkg::msh_col_wr_req_t    drvr_p0_nb_wr_req_to_dut;
    mby_msh_pkg::msh_col_wr_req_t    drvr_p0_nb_wr_req_to_dut_q1;
    mby_msh_pkg::msh_data_t          drvr_p0_nb_wr_data_to_dut;
    mby_msh_pkg::msh_data_t          drvr_p0_nb_wr_data_to_dut_q1;

    mby_msh_pkg::msh_col_wr_req_t    drvr_p0_sb_wr_req_to_dut;
    mby_msh_pkg::msh_col_wr_req_t    drvr_p0_sb_wr_req_to_dut_q1;
    mby_msh_pkg::msh_data_t          drvr_p0_sb_wr_data_to_dut;
    mby_msh_pkg::msh_data_t          drvr_p0_sb_wr_data_to_dut_q1;


    mby_msh_pkg::msh_row_rd_req_t    drvr_p0_eb_rd_req_to_dut;
    mby_msh_pkg::msh_row_rd_req_t    drvr_p0_eb_rd_req_to_dut_q1;
    mby_msh_pkg::msh_row_rd_req_t    drvr_p0_wb_rd_req_to_dut;
    mby_msh_pkg::msh_row_rd_req_t    drvr_p0_wb_rd_req_to_dut_q1;

    mby_msh_pkg::msh_col_rd_req_t    drvr_p0_nb_rd_req_to_dut;
    mby_msh_pkg::msh_col_rd_req_t    drvr_p0_nb_rd_req_to_dut_q1;
    mby_msh_pkg::msh_col_rd_req_t    drvr_p0_sb_rd_req_to_dut;
    mby_msh_pkg::msh_col_rd_req_t    drvr_p0_sb_rd_req_to_dut_q1;


    mby_msh_pkg::msh_row_rd_rsp_t    drvr_p0_eb_rd_rsp_to_dut;
    mby_msh_pkg::msh_row_rd_rsp_t    drvr_p0_eb_rd_rsp_to_dut_q1;
    mby_msh_pkg::msh_data_t          drvr_p0_eb_rd_data_to_dut;
    mby_msh_pkg::msh_data_t          drvr_p0_eb_rd_data_to_dut_q1;



    mby_msh_pkg::msh_row_rd_rsp_t    drvr_p0_wb_rd_rsp_to_dut;
    mby_msh_pkg::msh_row_rd_rsp_t    drvr_p0_wb_rd_rsp_to_dut_q1;
    mby_msh_pkg::msh_data_t          drvr_p0_wb_rd_data_to_dut;
    mby_msh_pkg::msh_data_t          drvr_p0_wb_rd_data_to_dut_q1;

    mby_msh_pkg::msh_col_rd_rsp_t    drvr_p0_nb_rd_rsp_to_dut;
    mby_msh_pkg::msh_col_rd_rsp_t    drvr_p0_nb_rd_rsp_to_dut_q1;
    mby_msh_pkg::msh_data_t          drvr_p0_nb_rd_data_to_dut;
    mby_msh_pkg::msh_data_t          drvr_p0_nb_rd_data_to_dut_q1;

    mby_msh_pkg::msh_col_rd_rsp_t    drvr_p0_sb_rd_rsp_to_dut;
    mby_msh_pkg::msh_col_rd_rsp_t    drvr_p0_sb_rd_rsp_to_dut_q1;
    mby_msh_pkg::msh_data_t          drvr_p0_sb_rd_data_to_dut;
    mby_msh_pkg::msh_data_t          drvr_p0_sb_rd_data_to_dut_q1;

// plane 1

    mby_msh_pkg::msh_row_wr_req_t    drvr_p1_eb_wr_req_to_dut;
    mby_msh_pkg::msh_row_wr_req_t    drvr_p1_eb_wr_req_to_dut_q1;
    mby_msh_pkg::msh_data_t          drvr_p1_eb_wr_data_to_dut;
    mby_msh_pkg::msh_data_t          drvr_p1_eb_wr_data_to_dut_q1;

    mby_msh_pkg::msh_row_wr_req_t    drvr_p1_wb_wr_req_to_dut;
    mby_msh_pkg::msh_row_wr_req_t    drvr_p1_wb_wr_req_to_dut_q1;
    mby_msh_pkg::msh_data_t          drvr_p1_wb_wr_data_to_dut;
    mby_msh_pkg::msh_data_t          drvr_p1_wb_wr_data_to_dut_q1;

    mby_msh_pkg::msh_col_wr_req_t    drvr_p1_nb_wr_req_to_dut;
    mby_msh_pkg::msh_col_wr_req_t    drvr_p1_nb_wr_req_to_dut_q1;
    mby_msh_pkg::msh_data_t          drvr_p1_nb_wr_data_to_dut;
    mby_msh_pkg::msh_data_t          drvr_p1_nb_wr_data_to_dut_q1;

    mby_msh_pkg::msh_col_wr_req_t    drvr_p1_sb_wr_req_to_dut;
    mby_msh_pkg::msh_col_wr_req_t    drvr_p1_sb_wr_req_to_dut_q1;
    mby_msh_pkg::msh_data_t          drvr_p1_sb_wr_data_to_dut;
    mby_msh_pkg::msh_data_t          drvr_p1_sb_wr_data_to_dut_q1;


    mby_msh_pkg::msh_row_rd_req_t    drvr_p1_eb_rd_req_to_dut;
    mby_msh_pkg::msh_row_rd_req_t    drvr_p1_eb_rd_req_to_dut_q1;
    mby_msh_pkg::msh_row_rd_req_t    drvr_p1_wb_rd_req_to_dut;
    mby_msh_pkg::msh_row_rd_req_t    drvr_p1_wb_rd_req_to_dut_q1;
    mby_msh_pkg::msh_col_rd_req_t    drvr_p1_nb_rd_req_to_dut;
    mby_msh_pkg::msh_col_rd_req_t    drvr_p1_nb_rd_req_to_dut_q1;
    mby_msh_pkg::msh_col_rd_req_t    drvr_p1_sb_rd_req_to_dut;
    mby_msh_pkg::msh_col_rd_req_t    drvr_p1_sb_rd_req_to_dut_q1;


    mby_msh_pkg::msh_row_rd_rsp_t    drvr_p1_eb_rd_rsp_to_dut;
    mby_msh_pkg::msh_row_rd_rsp_t    drvr_p1_eb_rd_rsp_to_dut_q1;
    mby_msh_pkg::msh_data_t          drvr_p1_eb_rd_data_to_dut;
    mby_msh_pkg::msh_data_t          drvr_p1_eb_rd_data_to_dut_q1;

    mby_msh_pkg::msh_row_rd_rsp_t    drvr_p1_wb_rd_rsp_to_dut;
    mby_msh_pkg::msh_row_rd_rsp_t    drvr_p1_wb_rd_rsp_to_dut_q1;
    mby_msh_pkg::msh_data_t          drvr_p1_wb_rd_data_to_dut;
    mby_msh_pkg::msh_data_t          drvr_p1_wb_rd_data_to_dut_q1;

    mby_msh_pkg::msh_col_rd_rsp_t    drvr_p1_nb_rd_rsp_to_dut;
    mby_msh_pkg::msh_col_rd_rsp_t    drvr_p1_nb_rd_rsp_to_dut_q1;
    mby_msh_pkg::msh_data_t          drvr_p1_nb_rd_data_to_dut;
    mby_msh_pkg::msh_data_t          drvr_p1_nb_rd_data_to_dut_q1;

    mby_msh_pkg::msh_col_rd_rsp_t    drvr_p1_sb_rd_rsp_to_dut;
    mby_msh_pkg::msh_col_rd_rsp_t    drvr_p1_sb_rd_rsp_to_dut_q1;
    mby_msh_pkg::msh_data_t          drvr_p1_sb_rd_data_to_dut;
    mby_msh_pkg::msh_data_t          drvr_p1_sb_rd_data_to_dut_q1;


    mby_msh_pkg::msh_row_wr_req_t    drvr_row_wreq;
    mby_msh_pkg::msh_col_wr_req_t    drvr_col_wreq;
    mby_msh_pkg::msh_row_rd_req_t    drvr_row_rreq;
    mby_msh_pkg::msh_col_rd_req_t    drvr_col_rreq;

    mby_msh_pkg::msh_data_t          drvr_wdata;        // wr data
    mby_msh_pkg::msh_data_t          drvr_rdata;        // wr data

    mby_msh_pkg::msh_row_rd_rsp_t    drvr_row_rsp;
    mby_msh_pkg::msh_col_rd_rsp_t    drvr_col_rsp;


    integer     which_plane;
    integer     drv_toward;
    integer     node_col;
    integer     node_row;
    integer	rreq_port_row;
    integer	rreq_port_side;
    integer	rsp_port_row;
    integer	rsp_port_side;
    
    integer  knob_inp_req_num;
    integer  knob_plane;
    integer  knob_drv_toward;
    integer  knob_legal_only;
    integer  knob_req_row;
    integer  knob_req_col;
    integer  knob_rreq_port_row;
    integer  knob_rreq_port_side;
    integer  knob_rsp_port_row;
    integer  knob_rsp_port_side;



    function new(

//        tmpl_pkg::enc_inp_t     iport, 
//        configuration           cfg
        virtual msh_node_dut_if     dut_if,
	integer  knob_inp_req_num,
	integer  knob_plane,
	integer  knob_drv_toward,
        integer  knob_legal_only,
	integer  knob_req_row,
	integer  knob_req_col,
	integer  knob_rreq_port_row,
	integer  knob_rreq_port_side,
	integer  knob_rsp_port_row,
	integer  knob_rsp_port_side
    );

        this.dut_if = dut_if;
//        this.cfg    = cfg;

	this.knob_inp_req_num = knob_inp_req_num;
	this.knob_plane = knob_plane;
	this.knob_drv_toward = knob_drv_toward;
	this.knob_legal_only = knob_legal_only;
	this.knob_req_row = knob_req_row;
	this.knob_req_col = knob_req_col;
	this.knob_rreq_port_row = knob_rreq_port_row;
	this.knob_rreq_port_side = knob_rreq_port_side;
	this.knob_rsp_port_row = knob_rsp_port_row;
	this.knob_rsp_port_side = knob_rsp_port_side;

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
        drv_corr        = 1'b0;
//      req_fifo        = {};           // initialize to empty queue

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


// plane 0
        drvr_p0_eb_wr_req_to_dut = '0;
        drvr_p0_eb_wr_req_to_dut_q1 = '0;
        drvr_p0_eb_wr_data_to_dut = '0;
        drvr_p0_eb_wr_data_to_dut_q1 = '0;

        drvr_p0_wb_wr_req_to_dut = '0;
        drvr_p0_wb_wr_req_to_dut_q1 = '0;
        drvr_p0_wb_wr_data_to_dut = '0;
        drvr_p0_wb_wr_data_to_dut_q1 = '0;

        drvr_p0_nb_wr_req_to_dut = '0;
        drvr_p0_nb_wr_req_to_dut_q1 = '0;
        drvr_p0_nb_wr_data_to_dut = '0;
        drvr_p0_nb_wr_data_to_dut_q1 = '0;

        drvr_p0_sb_wr_req_to_dut = '0;
        drvr_p0_sb_wr_req_to_dut_q1 = '0;
        drvr_p0_sb_wr_data_to_dut = '0;
        drvr_p0_sb_wr_data_to_dut_q1 = '0;

        drvr_p0_eb_rd_req_to_dut = '0;
        drvr_p0_eb_rd_req_to_dut_q1 = '0;
        drvr_p0_wb_rd_req_to_dut = '0;
        drvr_p0_wb_rd_req_to_dut_q1 = '0;
        drvr_p0_nb_rd_req_to_dut = '0;
        drvr_p0_nb_rd_req_to_dut_q1 = '0;
        drvr_p0_sb_rd_req_to_dut = '0;
        drvr_p0_sb_rd_req_to_dut_q1 = '0;

        drvr_p0_eb_rd_rsp_to_dut = '0;
        drvr_p0_eb_rd_rsp_to_dut_q1 = '0;
        drvr_p0_eb_rd_data_to_dut = '0;
        drvr_p0_eb_rd_data_to_dut_q1 = '0;

        drvr_p0_wb_rd_rsp_to_dut = '0;
        drvr_p0_wb_rd_rsp_to_dut_q1 = '0;
        drvr_p0_wb_rd_data_to_dut = '0;
        drvr_p0_wb_rd_data_to_dut_q1 = '0;

        drvr_p0_nb_rd_rsp_to_dut = '0;
        drvr_p0_nb_rd_rsp_to_dut_q1 = '0;
        drvr_p0_nb_rd_data_to_dut = '0;
        drvr_p0_nb_rd_data_to_dut_q1 = '0;

        drvr_p0_sb_rd_rsp_to_dut = '0;
        drvr_p0_sb_rd_rsp_to_dut_q1 = '0;
        drvr_p0_sb_rd_data_to_dut = '0;
        drvr_p0_sb_rd_data_to_dut_q1 = '0;

// plane 1
        drvr_p1_eb_wr_req_to_dut = '0;
        drvr_p1_eb_wr_req_to_dut_q1 = '0;
        drvr_p1_eb_wr_data_to_dut = '0;
        drvr_p1_eb_wr_data_to_dut_q1 = '0;

        drvr_p1_wb_wr_req_to_dut = '0;
        drvr_p1_wb_wr_req_to_dut_q1 = '0;
        drvr_p1_wb_wr_data_to_dut = '0;
        drvr_p1_wb_wr_data_to_dut_q1 = '0;

        drvr_p1_nb_wr_req_to_dut = '0;
        drvr_p1_nb_wr_req_to_dut_q1 = '0;
        drvr_p1_nb_wr_data_to_dut = '0;
        drvr_p1_nb_wr_data_to_dut_q1 = '0;

        drvr_p1_sb_wr_req_to_dut = '0;
        drvr_p1_sb_wr_req_to_dut_q1 = '0;
        drvr_p1_sb_wr_data_to_dut = '0;
        drvr_p1_sb_wr_data_to_dut_q1 = '0;

        drvr_p1_eb_rd_req_to_dut = '0;
        drvr_p1_eb_rd_req_to_dut_q1 = '0;
        drvr_p1_wb_rd_req_to_dut = '0;
        drvr_p1_wb_rd_req_to_dut_q1 = '0;
        drvr_p1_nb_rd_req_to_dut = '0;
        drvr_p1_nb_rd_req_to_dut_q1 = '0;
        drvr_p1_sb_rd_req_to_dut = '0;
        drvr_p1_sb_rd_req_to_dut_q1 = '0;

        drvr_p1_eb_rd_rsp_to_dut = '0;
        drvr_p1_eb_rd_rsp_to_dut_q1 = '0;
        drvr_p1_eb_rd_data_to_dut = '0;
        drvr_p1_eb_rd_data_to_dut_q1 = '0;

        drvr_p1_wb_rd_rsp_to_dut = '0;
        drvr_p1_wb_rd_rsp_to_dut_q1 = '0;
        drvr_p1_wb_rd_data_to_dut = '0;
        drvr_p1_wb_rd_data_to_dut_q1 = '0;

        drvr_p1_nb_rd_rsp_to_dut = '0;
        drvr_p1_nb_rd_rsp_to_dut_q1 = '0;
        drvr_p1_nb_rd_data_to_dut = '0;
        drvr_p1_nb_rd_data_to_dut_q1 = '0;

        drvr_p1_sb_rd_rsp_to_dut = '0;
        drvr_p1_sb_rd_rsp_to_dut_q1 = '0;
        drvr_p1_sb_rd_data_to_dut = '0;
        drvr_p1_sb_rd_data_to_dut_q1 = '0;

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


        // plane0
            drvr_p0_eb_wr_req_to_dut_q1  <= drvr_p0_eb_wr_req_to_dut;
            drvr_p0_wb_wr_req_to_dut_q1  <= drvr_p0_wb_wr_req_to_dut;
            drvr_p0_nb_wr_req_to_dut_q1  <= drvr_p0_nb_wr_req_to_dut;
            drvr_p0_sb_wr_req_to_dut_q1  <= drvr_p0_sb_wr_req_to_dut;

            drvr_p0_eb_wr_data_to_dut_q1 <= drvr_p0_eb_wr_data_to_dut;
            drvr_p0_wb_wr_data_to_dut_q1 <= drvr_p0_wb_wr_data_to_dut;
            drvr_p0_nb_wr_data_to_dut_q1 <= drvr_p0_nb_wr_data_to_dut;
            drvr_p0_sb_wr_data_to_dut_q1 <= drvr_p0_sb_wr_data_to_dut;

            drvr_p0_eb_rd_req_to_dut_q1  <= drvr_p0_eb_rd_req_to_dut;
            drvr_p0_wb_rd_req_to_dut_q1  <= drvr_p0_wb_rd_req_to_dut;
            drvr_p0_nb_rd_req_to_dut_q1  <= drvr_p0_nb_rd_req_to_dut;
            drvr_p0_sb_rd_req_to_dut_q1  <= drvr_p0_sb_rd_req_to_dut;

            drvr_p0_eb_rd_rsp_to_dut_q1  <= drvr_p0_eb_rd_rsp_to_dut;
            drvr_p0_wb_rd_rsp_to_dut_q1  <= drvr_p0_wb_rd_rsp_to_dut;
            drvr_p0_nb_rd_rsp_to_dut_q1  <= drvr_p0_nb_rd_rsp_to_dut;
            drvr_p0_sb_rd_rsp_to_dut_q1  <= drvr_p0_sb_rd_rsp_to_dut;

            drvr_p0_eb_rd_data_to_dut_q1 <= drvr_p0_eb_rd_data_to_dut;
            drvr_p0_wb_rd_data_to_dut_q1 <= drvr_p0_wb_rd_data_to_dut;
            drvr_p0_nb_rd_data_to_dut_q1 <= drvr_p0_nb_rd_data_to_dut;
            drvr_p0_sb_rd_data_to_dut_q1 <= drvr_p0_sb_rd_data_to_dut;

        // plane1
            drvr_p1_eb_wr_req_to_dut_q1  <= drvr_p1_eb_wr_req_to_dut;
            drvr_p1_wb_wr_req_to_dut_q1  <= drvr_p1_wb_wr_req_to_dut;
            drvr_p1_nb_wr_req_to_dut_q1  <= drvr_p1_nb_wr_req_to_dut;
            drvr_p1_sb_wr_req_to_dut_q1  <= drvr_p1_sb_wr_req_to_dut;

            drvr_p1_eb_wr_data_to_dut_q1 <= drvr_p1_eb_wr_data_to_dut;
            drvr_p1_wb_wr_data_to_dut_q1 <= drvr_p1_wb_wr_data_to_dut;
            drvr_p1_nb_wr_data_to_dut_q1 <= drvr_p1_nb_wr_data_to_dut;
            drvr_p1_sb_wr_data_to_dut_q1 <= drvr_p1_sb_wr_data_to_dut;

            drvr_p1_eb_rd_req_to_dut_q1  <= drvr_p1_eb_rd_req_to_dut;
            drvr_p1_wb_rd_req_to_dut_q1  <= drvr_p1_wb_rd_req_to_dut;
            drvr_p1_nb_rd_req_to_dut_q1  <= drvr_p1_nb_rd_req_to_dut;
            drvr_p1_sb_rd_req_to_dut_q1  <= drvr_p1_sb_rd_req_to_dut;


            drvr_p1_eb_rd_rsp_to_dut_q1  <= drvr_p1_eb_rd_rsp_to_dut;
            drvr_p1_wb_rd_rsp_to_dut_q1  <= drvr_p1_wb_rd_rsp_to_dut;
            drvr_p1_nb_rd_rsp_to_dut_q1  <= drvr_p1_nb_rd_rsp_to_dut;
            drvr_p1_sb_rd_rsp_to_dut_q1  <= drvr_p1_sb_rd_rsp_to_dut;

            drvr_p1_eb_rd_data_to_dut_q1 <= drvr_p1_eb_rd_data_to_dut;
            drvr_p1_wb_rd_data_to_dut_q1 <= drvr_p1_wb_rd_data_to_dut;
            drvr_p1_nb_rd_data_to_dut_q1 <= drvr_p1_nb_rd_data_to_dut;
            drvr_p1_sb_rd_data_to_dut_q1 <= drvr_p1_sb_rd_data_to_dut;



            // drive DUT inputs 


            dut_if.i_nb_wr_req[0]   = drvr_p0_nb_wr_req_to_dut_q1;
            dut_if.i_nb_wr_data[0]  = drvr_p0_nb_wr_data_to_dut_q1;
            dut_if.i_nb_rd_req[0]   = drvr_p0_nb_rd_req_to_dut_q1;

//          $display("(time: %0d) %s: ** inside connect drv_corr = (%0d) ** ", $time, name, drv_corr);

	    if (drv_corr == 0) begin
              dut_if.i_nb_rd_rsp[0]   = drvr_p0_nb_rd_rsp_to_dut_q1;
              dut_if.i_nb_rd_data[0]  = drvr_p1_nb_rd_data_to_dut_q1;
	    end

            dut_if.i_sb_wr_req[0]   = drvr_p0_sb_wr_req_to_dut_q1;
            dut_if.i_sb_wr_data[0]  = drvr_p0_sb_wr_data_to_dut_q1;
            dut_if.i_sb_rd_req[0]   = drvr_p0_sb_rd_req_to_dut_q1;
	    if (drv_corr == 0) begin
              dut_if.i_sb_rd_rsp[0]   = drvr_p0_sb_rd_rsp_to_dut_q1;
              dut_if.i_sb_rd_data[0]  = drvr_p0_sb_rd_data_to_dut_q1;
	    end

            dut_if.i_eb_wr_req[0]   = drvr_p0_eb_wr_req_to_dut_q1;
            dut_if.i_eb_wr_data[0]  = drvr_p0_eb_wr_data_to_dut_q1;
            dut_if.i_eb_rd_req[0]   = drvr_p0_eb_rd_req_to_dut_q1;
	    if (drv_corr == 0) begin
              dut_if.i_eb_rd_rsp[0]   = drvr_p0_eb_rd_rsp_to_dut_q1;
              dut_if.i_eb_rd_data[0]  = drvr_p0_eb_rd_data_to_dut_q1;
	    end

            dut_if.i_wb_wr_req[0]   = drvr_p0_wb_wr_req_to_dut_q1;
            dut_if.i_wb_wr_data[0]  = drvr_p0_wb_wr_data_to_dut_q1;
            dut_if.i_wb_rd_req[0]   = drvr_p0_wb_rd_req_to_dut_q1;
	    if (drv_corr == 0) begin
              dut_if.i_wb_rd_rsp[0]   = drvr_p0_wb_rd_rsp_to_dut_q1;
              dut_if.i_wb_rd_data[0]  = drvr_p0_wb_rd_data_to_dut_q1;
	    end

        // plane1
            dut_if.i_nb_wr_req[1]   = drvr_p1_nb_wr_req_to_dut_q1;
            dut_if.i_nb_wr_data[1]  = drvr_p1_nb_wr_data_to_dut_q1;
            dut_if.i_nb_rd_req[1]   = drvr_p1_nb_rd_req_to_dut_q1;
	    if (drv_corr == 0) begin
              dut_if.i_nb_rd_rsp[1]   = drvr_p1_nb_rd_rsp_to_dut_q1;
              dut_if.i_nb_rd_data[1]  = drvr_p1_nb_rd_data_to_dut_q1;
	    end

            dut_if.i_sb_wr_req[1]   = drvr_p1_sb_wr_req_to_dut_q1;
            dut_if.i_sb_wr_data[1]  = drvr_p1_sb_wr_data_to_dut_q1;
            dut_if.i_sb_rd_req[1]   = drvr_p1_sb_rd_req_to_dut_q1;
	    if (drv_corr == 0) begin
              dut_if.i_sb_rd_rsp[1]   = drvr_p1_sb_rd_rsp_to_dut_q1;
              dut_if.i_sb_rd_data[1]  = drvr_p1_sb_rd_data_to_dut_q1;
	    end

            dut_if.i_eb_wr_req[1]   = drvr_p1_eb_wr_req_to_dut_q1;
            dut_if.i_eb_wr_data[1]  = drvr_p1_eb_wr_data_to_dut_q1;
            dut_if.i_eb_rd_req[1]   = drvr_p1_eb_rd_req_to_dut_q1;
	    if (drv_corr == 0) begin
              dut_if.i_eb_rd_rsp[1]   = drvr_p1_eb_rd_rsp_to_dut_q1;
              dut_if.i_eb_rd_data[1]  = drvr_p1_eb_rd_data_to_dut_q1;
	    end


            dut_if.i_wb_wr_req[1]   = drvr_p1_wb_wr_req_to_dut_q1;
            dut_if.i_wb_wr_data[1]  = drvr_p1_wb_wr_data_to_dut_q1;
            dut_if.i_wb_rd_req[1]   = drvr_p1_wb_rd_req_to_dut_q1;
	    if (drv_corr == 0) begin
              dut_if.i_wb_rd_rsp[1]   = drvr_p1_wb_rd_rsp_to_dut_q1;
              dut_if.i_wb_rd_data[1]  = drvr_p1_wb_rd_data_to_dut_q1;
	    end
    
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




    // Drive requests into DUT
    task drive_reqs();

        if (!drove_reqs) begin

            @(posedge dut_if.mclk);


            //drvr_rd_req_to_dut.sema_val = 1'b0;


	    // drv_corr = 1;


            for (integer req_loop = 0; req_loop < knob_inp_req_num; req_loop++) begin

	       $display("(time: %0d) %s: ****************************", $time, name);
               $display("(time: %0d) %s: ** (current req loop = %0d) ** ", $time, name, req_loop);
	       $display("(time: %0d) %s: ****************************", $time, name);


                // randomize target node for incoming request
                // but
                // because the dut.col is randomized in the test thru a knob,
                // should we add constrain so that it should be (req.col >= dut.col) ???
                // what will happen if req.col < dut.col ???

                if (knob_plane == 0)        which_plane = 0;
                else if (knob_plane == 1)   which_plane = 1;
                else                        which_plane = $urandom_range(0, 1) ;

                // which_plane = 0;

                // drv_toward: 0 -> EB
                //             1 -> WB
                //             2 -> NB
                //             3 -> SB

		if (knob_drv_toward == 0) 	drv_toward = 0;
		else if (knob_drv_toward == 1) 	drv_toward = 1;
		else if (knob_drv_toward == 2) 	drv_toward = 2;
		else if (knob_drv_toward == 3) 	drv_toward = 3;
		else 		drv_toward = $urandom_range(0, 3) ;

                // drv_toward = 2;


            //  $display("(time: %0d) %s: !!! knob_drv_toward=%0d, drv_toward=%0d !!! ", $time, name, knob_drv_toward, drv_toward);


		if (knob_req_row < 16)  node_row = knob_req_row;
		else  node_row = $urandom_range(0, 15) ;

		if (knob_req_col < 8)  node_col = knob_req_col;
		else  node_col = $urandom_range(0, 7) ;

               // node_row = 2;
               // node_col = 4;



                // $display("(time: %0d) %s: knob_legal_only=%0d ", $time, name, knob_legal_only);
                // $display("(time: %0d) %s: req_node_row=%0d, col=%0d ", $time, name, node_row, node_col);
                // $display("(time: %0d) %s: dut_node_row=%0d, col=%0d ", $time, name, dut_if.i_sb_node_row, dut_if.i_eb_node_col);



		// make sure req_row and req_col are legal

	 	if (knob_legal_only == 1) begin

		   if ((knob_drv_toward != 0) && (drv_toward == 0) && (node_col < dut_if.i_eb_node_col)) begin
		      node_col = $urandom_range(dut_if.i_eb_node_col, 7);
                   // $display("(time: %0d) %s: Legalized EB wreq, req_node_col=%0d ", $time, name, node_col);
	  	   end

		   else if ((knob_drv_toward != 1) && (drv_toward == 1) && (node_col > dut_if.i_eb_node_col)) begin
		      node_col = $urandom_range(0, dut_if.i_eb_node_col);
                   // $display("(time: %0d) %s: Legalized WB wreq, req_node_col=%0d ", $time, name, node_col);
	 	   end

		   else if ((knob_drv_toward != 2) && (drv_toward == 2) && (node_row > dut_if.i_sb_node_row)) begin
		      node_row = $urandom_range(0, dut_if.i_sb_node_row);
                   // $display("(time: %0d) %s: Legalized NB wreq, req_node_row=%0d ", $time, name, node_row);
		   end

		   else if ((knob_drv_toward != 3) && (drv_toward == 3) && (node_row < dut_if.i_sb_node_row)) begin
		      node_row = $urandom_range(dut_if.i_sb_node_row, 15);
                   // $display("(time: %0d) %s: Legalized SB wreq, req_node_row=%0d ", $time, name, node_row);
		   end
		end



		if (knob_rreq_port_row < 16) 	rreq_port_row = knob_rreq_port_row;
	        else	rreq_port_row = $urandom_range(0, 15);

		if (knob_rreq_port_side < 4) 	rreq_port_side = knob_rreq_port_side;
	        else	rreq_port_side = $urandom_range(0, 3);



                // $display("(time: %0d) %s: rreq_port_row =%0d", $time, name, rreq_port_row);
                // $display("(time: %0d) %s: rreq_port_side=%0d", $time, name, rreq_port_side);



		// make sure rreq port_row is legal

	 	if (knob_legal_only == 1) begin
		   if ((drv_toward == 2) && (knob_rreq_port_row >= 16) // indicating knob selects random
					 && (rreq_port_row <= dut_if.i_sb_node_row)) // impossible case
		   begin
		      if (dut_if.i_sb_node_row == 15)
		         rreq_port_row = 15;
		      else
		         rreq_port_row = $urandom_range(dut_if.i_sb_node_row+1, 15);

                   // $display("(time: %0d) %s: Legalized NB rreq, rreq_port_row=%0d", $time, name, rreq_port_row);
	  	   end
	 	   else if ((drv_toward == 3) && (knob_rreq_port_row >= 16) && (rreq_port_row >= dut_if.i_sb_node_row)) begin
		      if (dut_if.i_sb_node_row == 0)
		         rreq_port_row = 0;
		      else
		         rreq_port_row = $urandom_range(0, dut_if.i_sb_node_row-1);

                    //$display("(time: %0d) %s: Legalized SB rreq, rreq_port_row=%0d", $time, name, rreq_port_row);
		   end
	   	end


		// make sure rreq port_side is legal

	 	if (knob_legal_only == 1) begin
		   if ((drv_toward == 2) && (knob_rreq_port_side >= 4) && (rreq_port_side == 0)) begin
		      rreq_port_side = 1; // south, east or west are ok
                   // $display("(time: %0d) %s: Legalized NB rreq, rreq_port_side=%0d", $time, name, rreq_port_side);
	 	   end
	 	   else if ((drv_toward == 3) && (knob_rreq_port_side >= 4) && (rreq_port_side == 1)) begin
		      rreq_port_side = 0; // north, east or west are ok
                   // $display("(time: %0d) %s: Legalized SB rreq, rreq_port_side=%0d", $time, name, rreq_port_side);
		   end
		end




		// rsp:


                if (knob_rsp_port_row < 16)    rsp_port_row = knob_rsp_port_row;
                else    rsp_port_row = $urandom_range(0, 15);

                if (knob_rsp_port_side < 4)    rsp_port_side = knob_rsp_port_side;
                else    rsp_port_side = $urandom_range(0, 3);



		// make sure rsp port_row is legal


                if (knob_legal_only == 1) begin
                   if ((drv_toward == 2) && (knob_rsp_port_row >= 16) && (rsp_port_row >= dut_if.i_sb_node_row)) begin
                      if (dut_if.i_sb_node_row == 0)
                         rsp_port_row = 0;
                      else
                         rsp_port_row = $urandom_range(0, dut_if.i_sb_node_row-1);

                   // $display("(time: %0d) %s: Legalized NB rsp, rsp_port_row=%0d", $time, name, rsp_port_row);
                   end
                   else if ((drv_toward == 3) && (knob_rsp_port_row >= 16) && (rsp_port_row <= dut_if.i_sb_node_row)) begin
                      if (dut_if.i_sb_node_row == 15)
                         rsp_port_row = 15;
                      else
                         rsp_port_row = $urandom_range(dut_if.i_sb_node_row+1, 15);

                    //$display("(time: %0d) %s: Legalized SB rsp, rsp_port_row=%0d", $time, name, rsp_port_row);
                   end
                end


                // make sure rsp port_side is legal

                if (knob_legal_only == 1) begin
                   if ((drv_toward == 2) && (knob_rsp_port_side >= 4) && (rsp_port_side == 1)) begin
                      rsp_port_side = 0; // north, east or west are ok
                   // $display("(time: %0d) %s: Legalized NB rsp, rsp_port_side=%0d", $time, name, rsp_port_side);
                   end
                   else if ((drv_toward == 3) && (knob_rsp_port_side >= 4) && (rsp_port_side == 0)) begin
                      rsp_port_side = 1; // south, east or west are ok
                   // $display("(time: %0d) %s: Legalized SB rsp, rsp_port_side=%0d", $time, name, rsp_port_side);
                   end
                end






                // display target of request

		if (which_plane == 0) 
                   if (drv_toward == 0)
                     $display("(time: %0d) %s: *** P0 EB req to (row, col) = (%0d, %0d) *** ", $time, name, node_row, node_col);

                   else if (drv_toward == 1)
                     $display("(time: %0d) %s: *** P0 WB req to (row, col) = (%0d, %0d) *** ", $time, name, node_row, node_col);

                   else if (drv_toward == 2) begin
                     $display("(time: %0d) %s: *** P0 NB req to row = (%0d) *** ", $time, name, node_row);
                     $display("(time: %0d) %s: req port_row=(%0d), port_side=(%0d)", $time,name,rreq_port_row,rreq_port_side);
                     $display("(time: %0d) %s: rsp port_row=(%0d), port_side=(%0d)", $time,name,rsp_port_row ,rsp_port_side);
		   end
                   else begin
                     $display("(time: %0d) %s: *** P0 SB req to row = (%0d) *** ", $time, name, node_row);
                     $display("(time: %0d) %s: req port_row=(%0d), port_side=(%0d)", $time,name,rreq_port_row,rreq_port_side);
                     $display("(time: %0d) %s: rsp port_row=(%0d), port_side=(%0d)", $time,name,rsp_port_row ,rsp_port_side);
		   end
		else
                   if (drv_toward == 0)
                     $display("(time: %0d) %s: *** P1 EB req to (row, col) = (%0d, %0d) *** ", $time, name, node_row, node_col);

                   else if (drv_toward == 1)
                     $display("(time: %0d) %s: *** P1 WB req to (row, col) = (%0d, %0d) *** ", $time, name, node_row, node_col);

                   else if (drv_toward == 2) begin
                     $display("(time: %0d) %s: *** P1 NB req to row = (%0d) *** ", $time, name, node_row);
                     $display("(time: %0d) %s: req port_row=(%0d), port_side=(%0d)", $time,name,rreq_port_row,rreq_port_side);
                     $display("(time: %0d) %s: rsp port_row=(%0d), port_side=(%0d)", $time,name,rsp_port_row ,rsp_port_side);
		   end
                   else begin
                     $display("(time: %0d) %s: *** P1 SB req to row = (%0d) *** ", $time, name, node_row);
                     $display("(time: %0d) %s: req port_row=(%0d), port_side=(%0d)", $time,name,rreq_port_row,rreq_port_side);
                     $display("(time: %0d) %s: rsp port_row=(%0d), port_side=(%0d)", $time,name,rsp_port_row ,rsp_port_side);
		   end



                // wr_req:

        drvr_row_wreq.node_col  = node_col;
        drvr_row_wreq.node_row  = node_row;
        drvr_row_wreq.csr       = $urandom_range(0, 1);
        drvr_row_wreq.addr      = $urandom();
        drvr_row_wreq.sema_val  = $urandom_range(0, 1);
        drvr_row_wreq.age       = $urandom();

        drvr_col_wreq.node_row  = node_row;
        drvr_col_wreq.csr       = $urandom_range(0, 1);
        drvr_col_wreq.addr      = $urandom();
        drvr_col_wreq.sema_val  = $urandom_range(0, 1);
        drvr_col_wreq.age       = $urandom();

               drvr_wdata       = $urandom();

                // rd_req:

        drvr_row_rreq.id        = $urandom();
        drvr_row_rreq.node_col  = node_col;
        drvr_row_rreq.node_row  = node_row;
        drvr_row_rreq.csr       = $urandom_range(0, 1);
        drvr_row_rreq.addr      = $urandom();
        drvr_row_rreq.sema_val  = $urandom_range(0, 1);
        drvr_row_rreq.age       = $urandom();

        drvr_col_rreq.id        = $urandom();
    //  drvr_col_rreq.port_row  = $urandom();
    //  drvr_col_rreq.port_side = $urandom();
        drvr_col_rreq.port_row  = rreq_port_row;
        drvr_col_rreq.port_side = rreq_port_side;
        drvr_col_rreq.node_row  = node_row;
        drvr_col_rreq.csr       = $urandom_range(0, 1);
        drvr_col_rreq.addr      = $urandom();
        drvr_col_rreq.sema_val  = $urandom_range(0, 1);
        drvr_col_rreq.age       = $urandom();


	drvr_row_rsp.id		= $urandom();

	drvr_col_rsp.id		= $urandom();
	drvr_col_rsp.port_side	= rsp_port_side;
	drvr_col_rsp.port_row	= rsp_port_row;

               drvr_rdata       = $urandom();


               // p0 wr req, wr data:

               @(posedge dut_if.mclk);

               if (which_plane == 0) // plane 0
                   if (drv_toward == 0) begin  // EB
                        drvr_p0_eb_wr_req_to_dut        = drvr_row_wreq;
                        drvr_p0_eb_wr_req_to_dut.vld    = 1'b1;
                   end
                   else if (drv_toward == 1) begin // WB
                        drvr_p0_wb_wr_req_to_dut        = drvr_row_wreq;
                        drvr_p0_wb_wr_req_to_dut.vld    = 1'b1;
                   end
                   else if (drv_toward == 2) begin // NB
                        drvr_p0_nb_wr_req_to_dut        = drvr_col_wreq;
                        drvr_p0_nb_wr_req_to_dut.vld    = 1'b1;
                   end
                   else begin  // SB
                        drvr_p0_sb_wr_req_to_dut        = drvr_col_wreq;
                        drvr_p0_sb_wr_req_to_dut.vld    = 1'b1;
                   end
               else
                   if (drv_toward == 0) begin      
                        drvr_p1_eb_wr_req_to_dut        = drvr_row_wreq;
                        drvr_p1_eb_wr_req_to_dut.vld    = 1'b1;
                   end
                   else if (drv_toward == 1) begin
                        drvr_p1_wb_wr_req_to_dut        = drvr_row_wreq;
                        drvr_p1_wb_wr_req_to_dut.vld    = 1'b1;
                   end
                   else if (drv_toward == 2) begin
                        drvr_p1_nb_wr_req_to_dut        = drvr_col_wreq;
                        drvr_p1_nb_wr_req_to_dut.vld    = 1'b1;
                   end
                   else begin                     
                        drvr_p1_sb_wr_req_to_dut        = drvr_col_wreq;
                        drvr_p1_sb_wr_req_to_dut.vld    = 1'b1;
                   end

               @(posedge dut_if.mclk);

               drvr_p0_eb_wr_req_to_dut = '0;
               drvr_p0_wb_wr_req_to_dut = '0;
               drvr_p0_nb_wr_req_to_dut = '0;
               drvr_p0_sb_wr_req_to_dut = '0;
               drvr_p1_eb_wr_req_to_dut = '0;
               drvr_p1_wb_wr_req_to_dut = '0;
               drvr_p1_nb_wr_req_to_dut = '0;
               drvr_p1_sb_wr_req_to_dut = '0;


               @(posedge dut_if.mclk);

               if (which_plane == 0) // plane 0
                   if (drv_toward == 0)
                        drvr_p0_eb_wr_data_to_dut    = drvr_wdata;
                   else if (drv_toward == 1)
                        drvr_p0_wb_wr_data_to_dut    = drvr_wdata;
                   else if (drv_toward == 2)
                        drvr_p0_nb_wr_data_to_dut    = drvr_wdata;
                   else
                        drvr_p0_sb_wr_data_to_dut    = drvr_wdata;
               else 
                   if (drv_toward == 0)
                        drvr_p1_eb_wr_data_to_dut    = drvr_wdata;
                   else if (drv_toward == 1)
                        drvr_p1_wb_wr_data_to_dut    = drvr_wdata;
                   else if (drv_toward == 2)
                        drvr_p1_nb_wr_data_to_dut    = drvr_wdata;
                   else
                        drvr_p1_sb_wr_data_to_dut    = drvr_wdata;

               @(posedge dut_if.mclk);
               drvr_p0_eb_wr_data_to_dut    = '0;
               drvr_p0_wb_wr_data_to_dut    = '0;
               drvr_p0_nb_wr_data_to_dut    = '0;
               drvr_p0_sb_wr_data_to_dut    = '0;
               drvr_p1_eb_wr_data_to_dut    = '0;
               drvr_p1_wb_wr_data_to_dut    = '0;
               drvr_p1_nb_wr_data_to_dut    = '0;
               drvr_p1_sb_wr_data_to_dut    = '0;

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
            

               // rd req

               @(posedge dut_if.mclk);

               if (which_plane == 0)            // plane 0
                  if (drv_toward == 0) begin    // EB
                     drvr_p0_eb_rd_req_to_dut           = drvr_row_rreq;
                     drvr_p0_eb_rd_req_to_dut.vld       = 1'b1;
                     drvr_p0_eb_rd_req_to_dut.csr       = drvr_row_wreq.csr;  // same as wr_req
                     drvr_p0_eb_rd_req_to_dut.addr      = drvr_row_wreq.addr; // same as wr_req
                  end
                  else if (drv_toward == 1) begin       // WB
                     drvr_p0_wb_rd_req_to_dut           = drvr_row_rreq;
                     drvr_p0_wb_rd_req_to_dut.vld       = 1'b1;
                     drvr_p0_wb_rd_req_to_dut.csr       = drvr_row_wreq.csr;
                     drvr_p0_wb_rd_req_to_dut.addr      = drvr_row_wreq.addr;
                  end
                  else if (drv_toward == 2) begin       // NB
                     drvr_p0_nb_rd_req_to_dut           = drvr_col_rreq;
                     drvr_p0_nb_rd_req_to_dut.vld       = 1'b1;
                     drvr_p0_nb_rd_req_to_dut.csr       = drvr_col_wreq.csr;
                     drvr_p0_nb_rd_req_to_dut.addr      = drvr_col_wreq.addr;
                  end
                  else begin
                     drvr_p0_sb_rd_req_to_dut           = drvr_col_rreq;
                     drvr_p0_sb_rd_req_to_dut.vld       = 1'b1;
                     drvr_p0_sb_rd_req_to_dut.csr       = drvr_col_wreq.csr;
                     drvr_p0_sb_rd_req_to_dut.addr      = drvr_col_wreq.addr;
                  end
               else // plane 1
                  if (drv_toward == 0) begin    // EB
                     drvr_p1_eb_rd_req_to_dut           = drvr_row_rreq;
                     drvr_p1_eb_rd_req_to_dut.vld       = 1'b1;
                     drvr_p1_eb_rd_req_to_dut.csr       = drvr_row_wreq.csr;
                     drvr_p1_eb_rd_req_to_dut.addr      = drvr_row_wreq.addr;
                  end
                  else if (drv_toward == 1) begin       // WB
                     drvr_p1_wb_rd_req_to_dut           = drvr_row_rreq;
                     drvr_p1_wb_rd_req_to_dut.vld       = 1'b1;
                     drvr_p1_wb_rd_req_to_dut.csr       = drvr_row_wreq.csr;
                     drvr_p1_wb_rd_req_to_dut.addr      = drvr_row_wreq.addr;
                  end
                  else if (drv_toward == 2) begin       // NB
                     drvr_p1_nb_rd_req_to_dut           = drvr_col_rreq;
                     drvr_p1_nb_rd_req_to_dut.vld       = 1'b1;
                     drvr_p1_nb_rd_req_to_dut.csr       = drvr_col_wreq.csr;
                     drvr_p1_nb_rd_req_to_dut.addr      = drvr_col_wreq.addr;
                  end
                  else begin
                     drvr_p1_sb_rd_req_to_dut           = drvr_col_rreq;
                     drvr_p1_sb_rd_req_to_dut.vld       = 1'b1;
                     drvr_p1_sb_rd_req_to_dut.csr       = drvr_col_wreq.csr;
                     drvr_p1_sb_rd_req_to_dut.addr      = drvr_col_wreq.addr;
                  end

               @(posedge dut_if.mclk);

               drvr_p0_eb_rd_req_to_dut      = '0;
               drvr_p0_wb_rd_req_to_dut      = '0;
               drvr_p0_nb_rd_req_to_dut      = '0;
               drvr_p0_sb_rd_req_to_dut      = '0;
               drvr_p1_eb_rd_req_to_dut      = '0;
               drvr_p1_wb_rd_req_to_dut      = '0;
               drvr_p1_nb_rd_req_to_dut      = '0;
               drvr_p1_sb_rd_req_to_dut      = '0;


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


               // rd rsp

               @(posedge dut_if.mclk);

	       if (drv_corr == 0) begin
                if (which_plane == 0)            // plane 0
                  if (drv_toward == 0) begin    // EB
                     drvr_p0_eb_rd_rsp_to_dut           = drvr_row_rsp;
                     drvr_p0_eb_rd_rsp_to_dut.vld       = 1'b1;
                  end
                  else if (drv_toward == 1) begin       // WB
                     drvr_p0_wb_rd_rsp_to_dut           = drvr_row_rsp;
                     drvr_p0_wb_rd_rsp_to_dut.vld       = 1'b1;
                  end
                  else if (drv_toward == 2) begin       // NB
                     drvr_p0_nb_rd_rsp_to_dut           = drvr_col_rsp;
                     drvr_p0_nb_rd_rsp_to_dut.vld       = 1'b1;
                  end
                  else begin
                     drvr_p0_sb_rd_rsp_to_dut           = drvr_col_rsp;
                     drvr_p0_sb_rd_rsp_to_dut.vld       = 1'b1;
                  end
                else // plane 1
                  if (drv_toward == 0) begin    // EB
                     drvr_p1_eb_rd_rsp_to_dut           = drvr_row_rsp;
                     drvr_p1_eb_rd_rsp_to_dut.vld       = 1'b1;
                  end
                  else if (drv_toward == 1) begin       // WB
                     drvr_p1_wb_rd_rsp_to_dut           = drvr_row_rsp;
                     drvr_p1_wb_rd_rsp_to_dut.vld       = 1'b1;
                  end
                  else if (drv_toward == 2) begin       // NB
                     drvr_p1_nb_rd_rsp_to_dut           = drvr_col_rsp;
                     drvr_p1_nb_rd_rsp_to_dut.vld       = 1'b1;
                  end
                  else begin
                     drvr_p1_sb_rd_rsp_to_dut           = drvr_col_rsp;
                     drvr_p1_sb_rd_rsp_to_dut.vld       = 1'b1;
                  end
	       end


               @(posedge dut_if.mclk);

	       if (drv_corr == 0) begin
                  drvr_p0_eb_rd_rsp_to_dut      = '0;
                  drvr_p0_wb_rd_rsp_to_dut      = '0;
                  drvr_p0_nb_rd_rsp_to_dut      = '0;
                  drvr_p0_sb_rd_rsp_to_dut      = '0;
                  drvr_p1_eb_rd_rsp_to_dut      = '0;
                  drvr_p1_wb_rd_rsp_to_dut      = '0;
                  drvr_p1_nb_rd_rsp_to_dut      = '0;
                  drvr_p1_sb_rd_rsp_to_dut      = '0;
	       end


               @(posedge dut_if.mclk);

	       if (drv_corr == 0) begin
                 if (which_plane == 0) // plane 0
                   if (drv_toward == 0) 	drvr_p0_eb_rd_data_to_dut    = drvr_rdata;
                   else if (drv_toward == 1) 	drvr_p0_wb_rd_data_to_dut    = drvr_rdata;
                   else if (drv_toward == 2) 	drvr_p0_nb_rd_data_to_dut    = drvr_rdata;
                   else 			drvr_p0_sb_rd_data_to_dut    = drvr_rdata;
                 else
                   if (drv_toward == 0) 	drvr_p1_eb_rd_data_to_dut    = drvr_rdata;
                   else if (drv_toward == 1) 	drvr_p1_wb_rd_data_to_dut    = drvr_rdata;
                   else if (drv_toward == 2) 	drvr_p1_nb_rd_data_to_dut    = drvr_rdata;
                   else 			drvr_p1_sb_rd_data_to_dut    = drvr_rdata;
	       end

               @(posedge dut_if.mclk);

	       if (drv_corr == 0) begin
                  drvr_p0_eb_rd_data_to_dut    = '0;
                  drvr_p0_wb_rd_data_to_dut    = '0;
                  drvr_p0_nb_rd_data_to_dut    = '0;
                  drvr_p0_sb_rd_data_to_dut    = '0;
                  drvr_p1_eb_rd_data_to_dut    = '0;
                  drvr_p1_wb_rd_data_to_dut    = '0;
                  drvr_p1_nb_rd_data_to_dut    = '0;
                  drvr_p1_sb_rd_data_to_dut    = '0;
	       end


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



            // $display("(time: %0d) %s: ** (current req loop = %0d) ** ", $time, name, req_loop);

            end         // end loop 
 
            drove_reqs = 1;

        end

        $display("(time: %0d) %s: ** Done Driving Requests to Inputs ** ", $time, name);

        @(posedge dut_if.mclk);


/*
	// give enough time before assigning dvr_done to 1 to allow scoreboard empty, otherwise ERROR
	// let's say 30 cycles should be enough

        for (integer drv_done_delay = 0; drv_done_delay < 60; drv_done_delay++) begin
           @(posedge dut_if.mclk);
           $display("(time: %0d) %s: drv_done_delay ", $time, name, drv_done_delay);
	end
*/

        drv_done = 1'b1;


        $display("(time: %0d) %s: drv_done = (%0d)", $time, name, drv_done);

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
