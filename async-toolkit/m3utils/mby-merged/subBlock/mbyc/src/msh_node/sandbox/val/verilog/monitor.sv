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
    virtual mby_mem_msh_bank_ram_shell_4096x532_func_if mem_if_0;
    virtual mby_mem_msh_bank_ram_shell_4096x532_func_if mem_if_1;
    virtual mby_mem_msh_bank_ram_shell_4096x532_func_if mem_if_2;
    virtual mby_mem_msh_bank_ram_shell_4096x532_func_if mem_if_3;


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

    integer		after_mem_wr;
    integer		mem_rd_cnt;

        // expected wr output by driving wr input:

    integer     exp_o_p0_eb_wr_valid;
    integer     exp_o_p0_eb_wr_valid_q1;
    integer     exp_o_p0_eb_wr_valid_q2;

    integer     exp_o_p0_wb_wr_valid;
    integer     exp_o_p0_wb_wr_valid_q1;
    integer     exp_o_p0_wb_wr_valid_q2;

    integer     exp_o_p0_nb_wr_valid;
    integer     exp_o_p0_nb_wr_valid_q1;
    integer     exp_o_p0_nb_wr_valid_q2;
    integer     exp_o_p0_nb_wr_valid_from_eb;
    integer     exp_o_p0_nb_wr_valid_from_eb_q1;
    integer     exp_o_p0_nb_wr_valid_from_eb_q2;
    integer     exp_o_p0_nb_wr_valid_from_wb;
    integer     exp_o_p0_nb_wr_valid_from_wb_q1;
    integer     exp_o_p0_nb_wr_valid_from_wb_q2;

    integer     exp_o_p0_sb_wr_valid;
    integer     exp_o_p0_sb_wr_valid_q1;
    integer     exp_o_p0_sb_wr_valid_q2;
    integer     exp_o_p0_sb_wr_valid_from_eb;
    integer     exp_o_p0_sb_wr_valid_from_eb_q1;
    integer     exp_o_p0_sb_wr_valid_from_eb_q2;
    integer     exp_o_p0_sb_wr_valid_from_wb;
    integer     exp_o_p0_sb_wr_valid_from_wb_q1;
    integer     exp_o_p0_sb_wr_valid_from_wb_q2;

    integer     exp_o_p1_eb_wr_valid;
    integer     exp_o_p1_eb_wr_valid_q1;
    integer     exp_o_p1_eb_wr_valid_q2;

    integer     exp_o_p1_wb_wr_valid;
    integer     exp_o_p1_wb_wr_valid_q1;
    integer     exp_o_p1_wb_wr_valid_q2;

    integer     exp_o_p1_nb_wr_valid;
    integer     exp_o_p1_nb_wr_valid_q1;
    integer     exp_o_p1_nb_wr_valid_q2;
    integer     exp_o_p1_nb_wr_valid_from_eb;
    integer     exp_o_p1_nb_wr_valid_from_eb_q1;
    integer     exp_o_p1_nb_wr_valid_from_eb_q2;
    integer     exp_o_p1_nb_wr_valid_from_wb;
    integer     exp_o_p1_nb_wr_valid_from_wb_q1;
    integer     exp_o_p1_nb_wr_valid_from_wb_q2;

    integer     exp_o_p1_sb_wr_valid;
    integer     exp_o_p1_sb_wr_valid_q1;
    integer     exp_o_p1_sb_wr_valid_q2;
    integer     exp_o_p1_sb_wr_valid_from_eb;
    integer     exp_o_p1_sb_wr_valid_from_eb_q1;
    integer     exp_o_p1_sb_wr_valid_from_eb_q2;
    integer     exp_o_p1_sb_wr_valid_from_wb;
    integer     exp_o_p1_sb_wr_valid_from_wb_q1;
    integer     exp_o_p1_sb_wr_valid_from_wb_q2;

        // expected wr to mem by driving wr input:

    integer     exp_p0_eb_wr_to_mem_valid;
    integer     exp_p0_wb_wr_to_mem_valid;
    integer     exp_p0_nb_wr_to_mem_valid;
    integer     exp_p0_sb_wr_to_mem_valid;
    integer     exp_p1_eb_wr_to_mem_valid;
    integer     exp_p1_wb_wr_to_mem_valid;
    integer     exp_p1_nb_wr_to_mem_valid;
    integer     exp_p1_sb_wr_to_mem_valid;

    integer     exp_p0_eb_wr_to_mem_valid_q1;
    integer     exp_p0_wb_wr_to_mem_valid_q1;
    integer     exp_p0_nb_wr_to_mem_valid_q1;
    integer     exp_p0_sb_wr_to_mem_valid_q1;
    integer     exp_p1_eb_wr_to_mem_valid_q1;
    integer     exp_p1_wb_wr_to_mem_valid_q1;
    integer     exp_p1_nb_wr_to_mem_valid_q1;
    integer     exp_p1_sb_wr_to_mem_valid_q1;

    integer     exp_p0_eb_wr_to_mem_valid_q2;
    integer     exp_p0_wb_wr_to_mem_valid_q2;
    integer     exp_p0_nb_wr_to_mem_valid_q2;
    integer     exp_p0_sb_wr_to_mem_valid_q2;
    integer     exp_p1_eb_wr_to_mem_valid_q2;
    integer     exp_p1_wb_wr_to_mem_valid_q2;
    integer     exp_p1_nb_wr_to_mem_valid_q2;
    integer     exp_p1_sb_wr_to_mem_valid_q2;

	// detect wr output

    integer     o_p0_eb_wr_valid;
    integer     o_p0_eb_wr_valid_q1;
    integer     o_p0_eb_wr_valid_q2;

    integer     o_p0_wb_wr_valid;
    integer     o_p0_wb_wr_valid_q1;
    integer     o_p0_wb_wr_valid_q2;

    integer     o_p0_nb_wr_valid;
    integer     o_p0_nb_wr_valid_q1;
    integer     o_p0_nb_wr_valid_q2;

    integer     o_p0_sb_wr_valid;
    integer     o_p0_sb_wr_valid_q1;
    integer     o_p0_sb_wr_valid_q2;

    integer     o_p1_eb_wr_valid;
    integer     o_p1_eb_wr_valid_q1;
    integer     o_p1_eb_wr_valid_q2;

    integer     o_p1_wb_wr_valid;
    integer     o_p1_wb_wr_valid_q1;
    integer     o_p1_wb_wr_valid_q2;

    integer     o_p1_nb_wr_valid;
    integer     o_p1_nb_wr_valid_q1;
    integer     o_p1_nb_wr_valid_q2;

    integer     o_p1_sb_wr_valid;
    integer     o_p1_sb_wr_valid_q1;
    integer     o_p1_sb_wr_valid_q2;

    mby_msh_pkg::msh_col_wr_req_t  exp_row_to_col_wreq;


	// expected rreq output by driving rreq input

    integer     exp_o_p0_eb_rreq_valid;
    integer     exp_o_p0_wb_rreq_valid;
    integer     exp_o_p0_nb_rreq_valid;
    integer     exp_o_p0_nb_rreq_valid_from_eb;
    integer     exp_o_p0_nb_rreq_valid_from_wb;
    integer     exp_o_p0_sb_rreq_valid;
    integer     exp_o_p0_sb_rreq_valid_from_eb;
    integer     exp_o_p0_sb_rreq_valid_from_wb;

    integer     exp_o_p1_eb_rreq_valid;
    integer     exp_o_p1_wb_rreq_valid;
    integer     exp_o_p1_nb_rreq_valid;
    integer     exp_o_p1_nb_rreq_valid_from_eb;
    integer     exp_o_p1_nb_rreq_valid_from_wb;
    integer     exp_o_p1_sb_rreq_valid;
    integer     exp_o_p1_sb_rreq_valid_from_eb;
    integer     exp_o_p1_sb_rreq_valid_from_wb;

    integer     exp_p0_eb_rreq_to_mem_valid;
    integer     exp_p0_wb_rreq_to_mem_valid;
    integer     exp_p0_nb_rreq_to_mem_valid;
    integer     exp_p0_sb_rreq_to_mem_valid;
    integer     exp_p1_eb_rreq_to_mem_valid;
    integer     exp_p1_wb_rreq_to_mem_valid;
    integer     exp_p1_nb_rreq_to_mem_valid;
    integer     exp_p1_sb_rreq_to_mem_valid;

	// detect rreq output
    integer     o_p0_eb_rreq_valid;
    integer     o_p0_wb_rreq_valid;
    integer     o_p0_nb_rreq_valid;
    integer     o_p0_sb_rreq_valid;

    integer     o_p1_eb_rreq_valid;
    integer     o_p1_wb_rreq_valid;
    integer     o_p1_nb_rreq_valid;
    integer     o_p1_sb_rreq_valid;

    mby_msh_pkg::msh_col_rd_req_t  exp_row_to_col_rreq;


	// rsp

    integer     o_p0_eb_rsp_valid;	// to check rsp
    integer     o_p0_eb_rsp_valid_q1;
    integer     o_p0_eb_rsp_valid_q2;	// to check rdata

    integer     o_p0_wb_rsp_valid;	
    integer     o_p0_wb_rsp_valid_q1;
    integer     o_p0_wb_rsp_valid_q2;

    integer     o_p0_nb_rsp_valid;	
    integer     o_p0_nb_rsp_valid_q1;
    integer     o_p0_nb_rsp_valid_q2;

    integer     o_p0_sb_rsp_valid;	
    integer     o_p0_sb_rsp_valid_q1;
    integer     o_p0_sb_rsp_valid_q2;

    integer     o_p1_eb_rsp_valid;	// to check rsp
    integer     o_p1_eb_rsp_valid_q1;
    integer     o_p1_eb_rsp_valid_q2;	// to check rdata

    integer     o_p1_wb_rsp_valid;	
    integer     o_p1_wb_rsp_valid_q1;
    integer     o_p1_wb_rsp_valid_q2;

    integer     o_p1_nb_rsp_valid;	
    integer     o_p1_nb_rsp_valid_q1;
    integer     o_p1_nb_rsp_valid_q2;

    integer     o_p1_sb_rsp_valid;	
    integer     o_p1_sb_rsp_valid_q1;
    integer     o_p1_sb_rsp_valid_q2;


    mby_msh_pkg::msh_row_rd_rsp_t  exp_o_p0_wb_rsp_from_i_eb_rreq;
    mby_msh_pkg::msh_row_rd_rsp_t  exp_o_p0_eb_rsp_from_i_wb_rreq;

    mby_msh_pkg::msh_row_rd_rsp_t  exp_o_p0_eb_rsp_from_i_nb_rreq;
    mby_msh_pkg::msh_row_rd_rsp_t  exp_o_p0_wb_rsp_from_i_nb_rreq;
    mby_msh_pkg::msh_col_rd_rsp_t  exp_o_p0_nb_rsp_from_i_nb_rreq;
    mby_msh_pkg::msh_col_rd_rsp_t  exp_o_p0_sb_rsp_from_i_nb_rreq;

    mby_msh_pkg::msh_row_rd_rsp_t  exp_o_p0_eb_rsp_from_i_sb_rreq;
    mby_msh_pkg::msh_row_rd_rsp_t  exp_o_p0_wb_rsp_from_i_sb_rreq;
    mby_msh_pkg::msh_col_rd_rsp_t  exp_o_p0_nb_rsp_from_i_sb_rreq;
    mby_msh_pkg::msh_col_rd_rsp_t  exp_o_p0_sb_rsp_from_i_sb_rreq;


    mby_msh_pkg::msh_row_rd_rsp_t  exp_o_p1_wb_rsp_from_i_eb_rreq;
    mby_msh_pkg::msh_row_rd_rsp_t  exp_o_p1_eb_rsp_from_i_wb_rreq;

    mby_msh_pkg::msh_row_rd_rsp_t  exp_o_p1_eb_rsp_from_i_nb_rreq;
    mby_msh_pkg::msh_row_rd_rsp_t  exp_o_p1_wb_rsp_from_i_nb_rreq;
    mby_msh_pkg::msh_col_rd_rsp_t  exp_o_p1_nb_rsp_from_i_nb_rreq;
    mby_msh_pkg::msh_col_rd_rsp_t  exp_o_p1_sb_rsp_from_i_nb_rreq;

    mby_msh_pkg::msh_row_rd_rsp_t  exp_o_p1_eb_rsp_from_i_sb_rreq;
    mby_msh_pkg::msh_row_rd_rsp_t  exp_o_p1_wb_rsp_from_i_sb_rreq;
    mby_msh_pkg::msh_col_rd_rsp_t  exp_o_p1_nb_rsp_from_i_sb_rreq;
    mby_msh_pkg::msh_col_rd_rsp_t  exp_o_p1_sb_rsp_from_i_sb_rreq;


	// rd data

    integer 	i_p0_eb_rreq_to_exp_o_wb_rdata;
    integer 	i_p0_wb_rreq_to_exp_o_eb_rdata;

    integer 	i_p0_nb_rreq_to_exp_o_eb_rdata;
    integer 	i_p0_nb_rreq_to_exp_o_wb_rdata;
    integer 	i_p0_nb_rreq_to_exp_o_nb_rdata;
    integer 	i_p0_nb_rreq_to_exp_o_sb_rdata;

    integer 	i_p0_sb_rreq_to_exp_o_eb_rdata;
    integer 	i_p0_sb_rreq_to_exp_o_wb_rdata;
    integer 	i_p0_sb_rreq_to_exp_o_nb_rdata;
    integer 	i_p0_sb_rreq_to_exp_o_sb_rdata;

    integer 	i_p1_eb_rreq_to_exp_o_wb_rdata;
    integer 	i_p1_wb_rreq_to_exp_o_eb_rdata;

    integer 	i_p1_nb_rreq_to_exp_o_eb_rdata;
    integer 	i_p1_nb_rreq_to_exp_o_wb_rdata;
    integer 	i_p1_nb_rreq_to_exp_o_nb_rdata;
    integer 	i_p1_nb_rreq_to_exp_o_sb_rdata;

    integer 	i_p1_sb_rreq_to_exp_o_eb_rdata;
    integer 	i_p1_sb_rreq_to_exp_o_wb_rdata;
    integer 	i_p1_sb_rreq_to_exp_o_nb_rdata;
    integer 	i_p1_sb_rreq_to_exp_o_sb_rdata;



    mby_msh_pkg::msh_data_t	last_i_p0_eb_wdata;
    mby_msh_pkg::msh_data_t	last_i_p0_wb_wdata;
    mby_msh_pkg::msh_data_t	last_i_p0_nb_wdata;
    mby_msh_pkg::msh_data_t	last_i_p0_sb_wdata;

    mby_msh_pkg::msh_data_t	last_i_p1_eb_wdata;
    mby_msh_pkg::msh_data_t	last_i_p1_wb_wdata;
    mby_msh_pkg::msh_data_t	last_i_p1_nb_wdata;
    mby_msh_pkg::msh_data_t	last_i_p1_sb_wdata;



    // statistics variables
    integer                 stat_num_arb_conflicts;

    function new(
        virtual msh_node_dut_if dut_if,

        virtual mby_mem_msh_bank_ram_shell_4096x532_func_if mem_if_0,
        virtual mby_mem_msh_bank_ram_shell_4096x532_func_if mem_if_1,
        virtual mby_mem_msh_bank_ram_shell_4096x532_func_if mem_if_2,
        virtual mby_mem_msh_bank_ram_shell_4096x532_func_if mem_if_3,

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


	after_mem_wr		= 0;
        mem_rd_cnt		= 0;


        exp_o_p0_eb_wr_valid       = 0;
        exp_o_p0_eb_wr_valid_q1    = 0;
        exp_o_p0_eb_wr_valid_q2    = 0;

        exp_o_p0_wb_wr_valid               = 0;
        exp_o_p0_wb_wr_valid_q1    = 0;
        exp_o_p0_wb_wr_valid_q2    = 0;

        exp_o_p0_nb_wr_valid       = 0;
        exp_o_p0_nb_wr_valid_q1    = 0;
        exp_o_p0_nb_wr_valid_q2    = 0;
        exp_o_p0_nb_wr_valid_from_eb       = 0;
        exp_o_p0_nb_wr_valid_from_eb_q1    = 0;
        exp_o_p0_nb_wr_valid_from_eb_q2    = 0;
        exp_o_p0_nb_wr_valid_from_wb       = 0;
        exp_o_p0_nb_wr_valid_from_wb_q1    = 0;
        exp_o_p0_nb_wr_valid_from_wb_q2    = 0;

        exp_o_p0_sb_wr_valid       = 0;
        exp_o_p0_sb_wr_valid_q1    = 0;
        exp_o_p0_sb_wr_valid_q2    = 0;
        exp_o_p0_sb_wr_valid_from_eb       = 0;
        exp_o_p0_sb_wr_valid_from_eb_q1    = 0;
        exp_o_p0_sb_wr_valid_from_eb_q2    = 0;
        exp_o_p0_sb_wr_valid_from_wb       = 0;
        exp_o_p0_sb_wr_valid_from_wb_q1    = 0;
        exp_o_p0_sb_wr_valid_from_wb_q2    = 0;


        exp_o_p1_eb_wr_valid               = 0;
        exp_o_p1_eb_wr_valid_q1    = 0;
        exp_o_p1_eb_wr_valid_q2    = 0;

        exp_o_p1_wb_wr_valid               = 0;
        exp_o_p1_wb_wr_valid_q1    = 0;
        exp_o_p1_wb_wr_valid_q2    = 0;

        exp_o_p1_nb_wr_valid       = 0;
        exp_o_p1_nb_wr_valid_q1    = 0;
        exp_o_p1_nb_wr_valid_q2    = 0;
        exp_o_p1_nb_wr_valid_from_eb       = 0;
        exp_o_p1_nb_wr_valid_from_eb_q1    = 0;
        exp_o_p1_nb_wr_valid_from_eb_q2    = 0;
        exp_o_p1_nb_wr_valid_from_wb       = 0;
        exp_o_p1_nb_wr_valid_from_wb_q1    = 0;
        exp_o_p1_nb_wr_valid_from_wb_q2    = 0;

        exp_o_p1_sb_wr_valid       = 0;
        exp_o_p1_sb_wr_valid_q1    = 0;
        exp_o_p1_sb_wr_valid_q2    = 0;
        exp_o_p1_sb_wr_valid_from_eb       = 0;
        exp_o_p1_sb_wr_valid_from_eb_q1    = 0;
        exp_o_p1_sb_wr_valid_from_eb_q2    = 0;
        exp_o_p1_sb_wr_valid_from_wb       = 0;
        exp_o_p1_sb_wr_valid_from_wb_q1    = 0;
        exp_o_p1_sb_wr_valid_from_wb_q2    = 0;

        exp_p0_eb_wr_to_mem_valid  = 0;
        exp_p0_wb_wr_to_mem_valid  = 0;
        exp_p0_nb_wr_to_mem_valid  = 0;
        exp_p0_sb_wr_to_mem_valid  = 0;
        exp_p1_eb_wr_to_mem_valid  = 0;
        exp_p1_wb_wr_to_mem_valid  = 0;
        exp_p1_nb_wr_to_mem_valid  = 0;
        exp_p1_sb_wr_to_mem_valid  = 0;

        exp_p0_eb_wr_to_mem_valid_q1       = 0;
        exp_p0_wb_wr_to_mem_valid_q1       = 0;
        exp_p0_nb_wr_to_mem_valid_q1       = 0;
        exp_p0_sb_wr_to_mem_valid_q1       = 0;
        exp_p1_eb_wr_to_mem_valid_q1       = 0;
        exp_p1_wb_wr_to_mem_valid_q1       = 0;
        exp_p1_nb_wr_to_mem_valid_q1       = 0;
        exp_p1_sb_wr_to_mem_valid_q1       = 0;

        exp_p0_eb_wr_to_mem_valid_q2       = 0;
        exp_p0_wb_wr_to_mem_valid_q2       = 0;
        exp_p0_nb_wr_to_mem_valid_q2       = 0;
        exp_p0_sb_wr_to_mem_valid_q2       = 0;
        exp_p1_eb_wr_to_mem_valid_q2       = 0;
        exp_p1_wb_wr_to_mem_valid_q2       = 0;
        exp_p1_nb_wr_to_mem_valid_q2       = 0;
        exp_p1_sb_wr_to_mem_valid_q2       = 0;

	o_p0_eb_wr_valid	= 0;
        o_p0_eb_wr_valid_q1     = 0;
        o_p0_eb_wr_valid_q2        = 0;

        o_p0_wb_wr_valid        = 0;
        o_p0_wb_wr_valid_q1        = 0;
        o_p0_wb_wr_valid_q2        = 0;

        o_p0_nb_wr_valid        = 0;
        o_p0_nb_wr_valid_q1        = 0;
        o_p0_nb_wr_valid_q2        = 0;

        o_p0_sb_wr_valid        = 0;
        o_p0_sb_wr_valid_q1        = 0;
        o_p0_sb_wr_valid_q2        = 0;

        o_p1_eb_wr_valid        = 0;
        o_p1_eb_wr_valid_q1        = 0;
        o_p1_eb_wr_valid_q2        = 0;

        o_p1_wb_wr_valid        = 0;
        o_p1_wb_wr_valid_q1        = 0;
        o_p1_wb_wr_valid_q2        = 0;

        o_p1_nb_wr_valid        = 0;
        o_p1_nb_wr_valid_q1        = 0;
        o_p1_nb_wr_valid_q2        = 0;

        o_p1_sb_wr_valid        = 0;
        o_p1_sb_wr_valid_q1        = 0;
        o_p1_sb_wr_valid_q2        = 0;

        exp_row_to_col_wreq      = '0;


	// initialize expected rreq output

   	exp_o_p0_eb_rreq_valid		= 0;
    	exp_o_p0_wb_rreq_valid		= 0;
    	exp_o_p0_nb_rreq_valid		= 0;
    	exp_o_p0_nb_rreq_valid_from_eb	= 0;
    	exp_o_p0_nb_rreq_valid_from_wb	= 0;
    	exp_o_p0_sb_rreq_valid		= 0;
    	exp_o_p0_sb_rreq_valid_from_eb	= 0;
    	exp_o_p0_sb_rreq_valid_from_wb	= 0;

    	exp_o_p1_eb_rreq_valid		= 0;
    	exp_o_p1_wb_rreq_valid		= 0;
    	exp_o_p1_nb_rreq_valid		= 0;
    	exp_o_p1_nb_rreq_valid_from_eb	= 0;
    	exp_o_p1_nb_rreq_valid_from_wb	= 0;
    	exp_o_p1_sb_rreq_valid		= 0;
    	exp_o_p1_sb_rreq_valid_from_eb	= 0;
    	exp_o_p1_sb_rreq_valid_from_wb	= 0;

     	exp_p0_eb_rreq_to_mem_valid	=0;
     	exp_p0_wb_rreq_to_mem_valid	=0;
     	exp_p0_nb_rreq_to_mem_valid	=0;
     	exp_p0_sb_rreq_to_mem_valid	=0;
    	exp_p1_eb_rreq_to_mem_valid	=0;
    	exp_p1_wb_rreq_to_mem_valid	=0;
    	exp_p1_nb_rreq_to_mem_valid	=0;
    	exp_p1_sb_rreq_to_mem_valid	=0;

        // detect rreq output

    	o_p0_eb_wr_valid	= 0;
    	o_p0_wb_wr_valid	= 0;
    	o_p0_nb_wr_valid	= 0;
    	o_p0_sb_wr_valid	= 0;

    	o_p1_eb_wr_valid	= 0;
    	o_p1_wb_wr_valid	= 0;
    	o_p1_nb_wr_valid	= 0;
    	o_p1_sb_wr_valid	= 0;

    	exp_row_to_col_rreq	= '0;

	// check rsp

	o_p0_eb_rsp_valid	= 0;
	o_p0_eb_rsp_valid_q1	= 0;
	o_p0_eb_rsp_valid_q2	= 0;

	o_p0_wb_rsp_valid	= 0;
	o_p0_wb_rsp_valid_q1	= 0;
	o_p0_wb_rsp_valid_q2	= 0;

	o_p0_nb_rsp_valid	= 0;
	o_p0_nb_rsp_valid_q1	= 0;
	o_p0_nb_rsp_valid_q2	= 0;

	o_p0_sb_rsp_valid	= 0;
	o_p0_sb_rsp_valid_q1	= 0;
	o_p0_sb_rsp_valid_q2	= 0;

	o_p1_eb_rsp_valid	= 0;
	o_p1_eb_rsp_valid_q1	= 0;
	o_p1_eb_rsp_valid_q2	= 0;

	o_p1_wb_rsp_valid	= 0;
	o_p1_wb_rsp_valid_q1	= 0;
	o_p1_wb_rsp_valid_q2	= 0;

	o_p1_nb_rsp_valid	= 0;
	o_p1_nb_rsp_valid_q1	= 0;
	o_p1_nb_rsp_valid_q2	= 0;

	o_p1_sb_rsp_valid	= 0;
	o_p1_sb_rsp_valid_q1	= 0;
	o_p1_sb_rsp_valid_q2	= 0;


	exp_o_p0_wb_rsp_from_i_eb_rreq	= '0;
	exp_o_p0_eb_rsp_from_i_wb_rreq	= '0;

	exp_o_p0_eb_rsp_from_i_nb_rreq	= '0;
	exp_o_p0_wb_rsp_from_i_nb_rreq	= '0;
	exp_o_p0_nb_rsp_from_i_nb_rreq	= '0;
	exp_o_p0_sb_rsp_from_i_nb_rreq	= '0;

	exp_o_p0_eb_rsp_from_i_sb_rreq	= '0;
	exp_o_p0_wb_rsp_from_i_sb_rreq	= '0;
	exp_o_p0_nb_rsp_from_i_sb_rreq	= '0;
	exp_o_p0_sb_rsp_from_i_sb_rreq	= '0;


	exp_o_p1_wb_rsp_from_i_eb_rreq	= '0;
	exp_o_p1_eb_rsp_from_i_wb_rreq	= '0;

	exp_o_p1_eb_rsp_from_i_nb_rreq	= '0;
	exp_o_p1_wb_rsp_from_i_nb_rreq	= '0;
	exp_o_p1_nb_rsp_from_i_nb_rreq	= '0;
	exp_o_p1_sb_rsp_from_i_nb_rreq	= '0;

	exp_o_p1_eb_rsp_from_i_sb_rreq	= '0;
	exp_o_p1_wb_rsp_from_i_sb_rreq	= '0;
	exp_o_p1_nb_rsp_from_i_sb_rreq	= '0;
	exp_o_p1_sb_rsp_from_i_sb_rreq	= '0;

	i_p0_eb_rreq_to_exp_o_wb_rdata	= 0;
	i_p0_wb_rreq_to_exp_o_eb_rdata	= 0;

	i_p0_nb_rreq_to_exp_o_eb_rdata	= 0;
	i_p0_nb_rreq_to_exp_o_wb_rdata	= 0;
	i_p0_nb_rreq_to_exp_o_nb_rdata	= 0;
	i_p0_nb_rreq_to_exp_o_sb_rdata	= 0;

	i_p0_sb_rreq_to_exp_o_eb_rdata	= 0;
	i_p0_sb_rreq_to_exp_o_wb_rdata	= 0;
	i_p0_sb_rreq_to_exp_o_nb_rdata	= 0;
	i_p0_sb_rreq_to_exp_o_sb_rdata	= 0;


	i_p1_eb_rreq_to_exp_o_wb_rdata	= 0;
	i_p1_wb_rreq_to_exp_o_eb_rdata	= 0;

	i_p1_nb_rreq_to_exp_o_eb_rdata	= 0;
	i_p1_nb_rreq_to_exp_o_wb_rdata	= 0;
	i_p1_nb_rreq_to_exp_o_nb_rdata	= 0;
	i_p1_nb_rreq_to_exp_o_sb_rdata	= 0;

	i_p1_sb_rreq_to_exp_o_eb_rdata	= 0;
	i_p1_sb_rreq_to_exp_o_wb_rdata	= 0;
	i_p1_sb_rreq_to_exp_o_nb_rdata	= 0;
	i_p1_sb_rreq_to_exp_o_sb_rdata	= 0;


	last_i_p0_eb_wdata	= '0;
	last_i_p0_wb_wdata	= '0;
	last_i_p0_nb_wdata	= '0;
	last_i_p0_sb_wdata	= '0;

	last_i_p1_eb_wdata	= '0;
	last_i_p1_wb_wdata	= '0;
	last_i_p1_nb_wdata	= '0;
	last_i_p1_sb_wdata	= '0;



        $value$plusargs("heartbeat=%d", heartbeat);     // heartbeat value comes from command line argument 

    endfunction

    task connect_to_DUT();
        forever begin
            @(negedge dut_if.mclk) // sample on negedge

/*
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

	   if (mem_if_0.wr_en) begin
               sb.mem_bank0_wr_notification(mem_if_0.adr, mem_if_0.wr_data);
		after_mem_wr = 1;
	   end



	   // 12/11/2018: push wr data at mem interface to fifo:

	   if (mem_if_0.wr_en)
               sb.mem_bank0_req_in_notification(mem_if_0.adr, mem_if_0.wr_data);



	   // 12/10/2018: monitor rd data at mem_if:

           // monitor rd req

           if (mem_if_0.rd_en)  begin 
               $display("(time: %0d) %s: --------------------------------------------", $time, name);
		mem_rd_cnt++ ;
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



	   // 12/10/2018: check 2nd read request at memory bank 0:

  	   if (mem_if_0.rd_en && (mem_rd_cnt==2)) begin
               sb.mem_bank0_rd_req_notification(mem_if_0.adr);
  	   end


	   // 12/10/2018: check 2nd read data at memory bank 0:

  	   if (mem_if_0.rd_valid && (mem_rd_cnt==2)) begin
                 sb.mem_bank0_rd_data_notification(mem_if_0.rd_data);
  	   end

*/



// -----------------------------------------------------------------------------
// 12/12/2018: push wrreq and data to fifo and pop to check when req goes out node or at memory
// -----------------------------------------------------------------------------


        // p0:

            // 1/7/2019: push expected o_eb_wr_data[0], and wrreq

           exp_o_p0_eb_wr_valid_q2 = exp_o_p0_eb_wr_valid_q1;
           exp_o_p0_eb_wr_valid_q1 = exp_o_p0_eb_wr_valid;
           if (dut_if.i_eb_wr_req[0].vld && (dut_if.i_eb_wr_req[0].node_col > dut_if.i_eb_node_col))
                exp_o_p0_eb_wr_valid = 1;
           else if (dut_if.i_eb_wr_req[0].vld && (dut_if.i_eb_wr_req[0].node_col < dut_if.i_eb_node_col))
           begin
                exp_o_p0_eb_wr_valid = 0;
                $display("(time: %0d) %s: WARNING: illegal P0 EB WR req gets dropped  !!! ", $time, name);
           end
           else  exp_o_p0_eb_wr_valid = 0;

           // push wrreq:
           if (exp_o_p0_eb_wr_valid) begin
               $display("(time: %0d) %s: --------", $time, name);
               $display("(time: %0d) %s: A P0 EB WR req is expected to go thru to EB output", $time, name);
               $display("(time: %0d) %s: i_p0_eb_wreq=(%0h)", $time, name, dut_if.i_eb_wr_req[0]);
               sb.exp_o_p0_eb_wreq_notification(dut_if.i_eb_wr_req[0]);
           end

           // push wdata:
           if (exp_o_p0_eb_wr_valid_q2) begin
               $display("(time: %0d) %s: i_p0_eb_wdata = (%0h)", $time, name, dut_if.i_eb_wr_data[0]);
               sb.exp_o_p0_eb_wdata_notification(dut_if.i_eb_wr_data[0]);
           end


            // 1/7/2019: push expected o_wb_wr_data[0] 

           exp_o_p0_wb_wr_valid_q2 = exp_o_p0_wb_wr_valid_q1;
           exp_o_p0_wb_wr_valid_q1 = exp_o_p0_wb_wr_valid;
           if (dut_if.i_wb_wr_req[0].vld && (dut_if.i_wb_wr_req[0].node_col < dut_if.i_eb_node_col))
                exp_o_p0_wb_wr_valid = 1;
           else if (dut_if.i_wb_wr_req[0].vld && (dut_if.i_wb_wr_req[0].node_col > dut_if.i_eb_node_col))
           begin
                exp_o_p0_wb_wr_valid = 0;
                $display("(time: %0d) %s: WARNING: illegal P0 WB WR req gets dropped  !!! ", $time, name);
           end
           else  exp_o_p0_wb_wr_valid = 0;

           if (exp_o_p0_wb_wr_valid) begin
               $display("(time: %0d) %s: --------", $time, name);
               $display("(time: %0d) %s: A P0 WB WR req is expected to go thru to WB output", $time, name);
               $display("(time: %0d) %s: i_p0_wb_wreq=(%0h)", $time, name, dut_if.i_wb_wr_req[0]);
               sb.exp_o_p0_wb_wreq_notification(dut_if.i_wb_wr_req[0]);
	   end

           if (exp_o_p0_wb_wr_valid_q2) begin
               $display("(time: %0d) %s: i_p0_wb_wdata = (%0h)", $time, name, dut_if.i_wb_wr_data[0]);
               sb.exp_o_p0_wb_wdata_notification(dut_if.i_wb_wr_data[0]);
	   end



            // 1/7/2019: push expected o_nb_wr_data[0] : from input nb, eb or wb

           exp_o_p0_nb_wr_valid_q2 = exp_o_p0_nb_wr_valid_q1;
           exp_o_p0_nb_wr_valid_q1 = exp_o_p0_nb_wr_valid;
           if (dut_if.i_nb_wr_req[0].vld && (dut_if.i_nb_wr_req[0].node_row < dut_if.i_sb_node_row))
                exp_o_p0_nb_wr_valid = 1;
           else if (dut_if.i_nb_wr_req[0].vld && (dut_if.i_nb_wr_req[0].node_row > dut_if.i_sb_node_row))
           begin
                exp_o_p0_nb_wr_valid = 0;
                $display("(time: %0d) %s: WARNING: illegal P0 NB WR req gets dropped  !!! ", $time, name);
           end
           else  exp_o_p0_nb_wr_valid = 0;


           exp_o_p0_nb_wr_valid_from_eb_q2 = exp_o_p0_nb_wr_valid_from_eb_q1;
           exp_o_p0_nb_wr_valid_from_eb_q1 = exp_o_p0_nb_wr_valid_from_eb;
           if (dut_if.i_eb_wr_req[0].vld && (dut_if.i_eb_wr_req[0].node_col == dut_if.i_eb_node_col)
                                         && (dut_if.i_eb_wr_req[0].node_row < dut_if.i_sb_node_row) )
                exp_o_p0_nb_wr_valid_from_eb = 1;
	   else exp_o_p0_nb_wr_valid_from_eb = 0;

           exp_o_p0_nb_wr_valid_from_wb_q2 = exp_o_p0_nb_wr_valid_from_wb_q1;
           exp_o_p0_nb_wr_valid_from_wb_q1 = exp_o_p0_nb_wr_valid_from_wb;
           if (dut_if.i_wb_wr_req[0].vld && (dut_if.i_wb_wr_req[0].node_col == dut_if.i_eb_node_col)
                                         && (dut_if.i_wb_wr_req[0].node_row < dut_if.i_sb_node_row) )
                exp_o_p0_nb_wr_valid_from_wb = 1;
	   else exp_o_p0_nb_wr_valid_from_wb = 0;


           if (exp_o_p0_nb_wr_valid) begin
               $display("(time: %0d) %s: --------", $time, name);
               $display("(time: %0d) %s: A P0 NB WR req is expected to go thru to NB output", $time, name);
               $display("(time: %0d) %s: i_p0_nb_wreq=(%0h)", $time, name, dut_if.i_nb_wr_req[0]);
               sb.exp_o_p0_nb_wreq_notification(dut_if.i_nb_wr_req[0]);
	   end
           else if (exp_o_p0_nb_wr_valid_from_eb) begin
                 exp_row_to_col_wreq.vld         = dut_if.i_eb_wr_req[0].vld;
                 exp_row_to_col_wreq.node_row    = dut_if.i_eb_wr_req[0].node_row;
                 exp_row_to_col_wreq.csr         = dut_if.i_eb_wr_req[0].csr;
                 exp_row_to_col_wreq.addr        = dut_if.i_eb_wr_req[0].addr;
                 exp_row_to_col_wreq.sema_val    = dut_if.i_eb_wr_req[0].sema_val;
                 exp_row_to_col_wreq.age         = dut_if.i_eb_wr_req[0].age;
               $display("(time: %0d) %s: --------", $time, name);
               $display("(time: %0d) %s: A P0 EB WR req is expected to go to NB output !!!", $time, name); 
               $display("(time: %0d) %s: i_p0_eb_wreq=(%0h)", $time, name, dut_if.i_eb_wr_req[0]);
               sb.exp_o_p0_nb_wreq_notification(exp_row_to_col_wreq); 
           end
           else if (exp_o_p0_nb_wr_valid_from_wb) begin
                 exp_row_to_col_wreq.vld         = dut_if.i_wb_wr_req[0].vld;
                 exp_row_to_col_wreq.node_row    = dut_if.i_wb_wr_req[0].node_row;
                 exp_row_to_col_wreq.csr         = dut_if.i_wb_wr_req[0].csr;
                 exp_row_to_col_wreq.addr        = dut_if.i_wb_wr_req[0].addr;
                 exp_row_to_col_wreq.sema_val    = dut_if.i_wb_wr_req[0].sema_val;
                 exp_row_to_col_wreq.age         = dut_if.i_wb_wr_req[0].age;
               $display("(time: %0d) %s: --------", $time, name);
               $display("(time: %0d) %s: A P0 WB WR req is expected to go to NB output !!!", $time, name); 
               $display("(time: %0d) %s: i_p0_wb_wreq=(%0h)", $time, name, dut_if.i_wb_wr_req[0]);
               sb.exp_o_p0_nb_wreq_notification(exp_row_to_col_wreq); 
           end


           if (exp_o_p0_nb_wr_valid_q2) begin
               $display("(time: %0d) %s: i_p0_nb_wdata = (%0h)", $time, name, dut_if.i_nb_wr_data[0]);
               sb.exp_o_p0_nb_wdata_notification(dut_if.i_nb_wr_data[0]);
	   end
           else if (exp_o_p0_nb_wr_valid_from_eb_q2) begin
               $display("(time: %0d) %s: i_p0_eb_wdata = (%0h)", $time, name, dut_if.i_eb_wr_data[0]);
               sb.exp_o_p0_nb_wdata_notification(dut_if.i_eb_wr_data[0]);
	   end
           else if (exp_o_p0_nb_wr_valid_from_wb_q2) begin
               $display("(time: %0d) %s: i_p0_wb_wdata = (%0h)", $time, name, dut_if.i_wb_wr_data[0]);
               sb.exp_o_p0_nb_wdata_notification(dut_if.i_wb_wr_data[0]);
	   end



            // 1/7/2019: push expected o_sb_wr_data[0] 

           exp_o_p0_sb_wr_valid_q2 = exp_o_p0_sb_wr_valid_q1;
           exp_o_p0_sb_wr_valid_q1 = exp_o_p0_sb_wr_valid;
           if (dut_if.i_sb_wr_req[0].vld && (dut_if.i_sb_wr_req[0].node_row > dut_if.i_sb_node_row))
                exp_o_p0_sb_wr_valid = 1;
           else if (dut_if.i_sb_wr_req[0].vld && (dut_if.i_sb_wr_req[0].node_row < dut_if.i_sb_node_row))
           begin
                exp_o_p0_sb_wr_valid = 0;
                $display("(time: %0d) %s: WARNING: illegal P0 SB WR req gets dropped  !!! ", $time, name);
           end
           else  exp_o_p0_sb_wr_valid = 0;

           exp_o_p0_sb_wr_valid_from_eb_q2 = exp_o_p0_sb_wr_valid_from_eb_q1;
           exp_o_p0_sb_wr_valid_from_eb_q1 = exp_o_p0_sb_wr_valid_from_eb;
           if (dut_if.i_eb_wr_req[0].vld && (dut_if.i_eb_wr_req[0].node_col == dut_if.i_eb_node_col)
                                         && (dut_if.i_eb_wr_req[0].node_row > dut_if.i_sb_node_row) )
                exp_o_p0_sb_wr_valid_from_eb = 1;
	   else exp_o_p0_sb_wr_valid_from_eb = 0;

           exp_o_p0_sb_wr_valid_from_wb_q2 = exp_o_p0_sb_wr_valid_from_wb_q1;
           exp_o_p0_sb_wr_valid_from_wb_q1 = exp_o_p0_sb_wr_valid_from_wb;
           if (dut_if.i_wb_wr_req[0].vld && (dut_if.i_wb_wr_req[0].node_col == dut_if.i_eb_node_col)
                                         && (dut_if.i_wb_wr_req[0].node_row > dut_if.i_sb_node_row) )
                exp_o_p0_sb_wr_valid_from_wb = 1;
	   else exp_o_p0_sb_wr_valid_from_wb = 0;


           if (exp_o_p0_sb_wr_valid) begin
               $display("(time: %0d) %s: --------", $time, name);
               $display("(time: %0d) %s: A P0 SB WR req is expected to go thru to SB output", $time, name);
               $display("(time: %0d) %s: i_p0_sb_wreq=(%0h)", $time, name, dut_if.i_sb_wr_req[0]);
               sb.exp_o_p0_sb_wreq_notification(dut_if.i_sb_wr_req[0]);
	   end
           else if (exp_o_p0_sb_wr_valid_from_eb) begin
               $display("(time: %0d) %s: --------", $time, name);
               $display("(time: %0d) %s: A P0 EB WR req is expected to go to SB output !!!", $time, name); 
               $display("(time: %0d) %s: i_p0_eb_wreq=(%0h)", $time, name, dut_if.i_eb_wr_req[0]);
                 exp_row_to_col_wreq.vld         = dut_if.i_eb_wr_req[0].vld;
                 exp_row_to_col_wreq.node_row    = dut_if.i_eb_wr_req[0].node_row;
                 exp_row_to_col_wreq.csr         = dut_if.i_eb_wr_req[0].csr;
                 exp_row_to_col_wreq.addr        = dut_if.i_eb_wr_req[0].addr;
                 exp_row_to_col_wreq.sema_val    = dut_if.i_eb_wr_req[0].sema_val;
                 exp_row_to_col_wreq.age         = dut_if.i_eb_wr_req[0].age;
               sb.exp_o_p0_sb_wreq_notification(exp_row_to_col_wreq); // (dut_if.i_eb_wr_erq[0]);
           end
           else if (exp_o_p0_sb_wr_valid_from_wb) begin
               $display("(time: %0d) %s: --------", $time, name);
               $display("(time: %0d) %s: A P0 WB WR req is expected to go to SB output !!!", $time, name); 
               $display("(time: %0d) %s: i_p0_wb_wreq=(%0h)", $time, name, dut_if.i_wb_wr_req[0]);
                 exp_row_to_col_wreq.vld         = dut_if.i_wb_wr_req[0].vld;
                 exp_row_to_col_wreq.node_row    = dut_if.i_wb_wr_req[0].node_row;
                 exp_row_to_col_wreq.csr         = dut_if.i_wb_wr_req[0].csr;
                 exp_row_to_col_wreq.addr        = dut_if.i_wb_wr_req[0].addr;
                 exp_row_to_col_wreq.sema_val    = dut_if.i_wb_wr_req[0].sema_val;
                 exp_row_to_col_wreq.age         = dut_if.i_wb_wr_req[0].age;
               sb.exp_o_p0_sb_wreq_notification(exp_row_to_col_wreq); // (dut_if.i_wb_wr_data[0]);
           end


           if (exp_o_p0_sb_wr_valid_q2) begin
               $display("(time: %0d) %s: i_p0_sb_wdata = (%0h)", $time, name, dut_if.i_sb_wr_data[0]);
               sb.exp_o_p0_sb_wdata_notification(dut_if.i_sb_wr_data[0]);
	   end
           else if (exp_o_p0_sb_wr_valid_from_eb_q2) begin
               $display("(time: %0d) %s: i_p0_eb_wdata = (%0h)", $time, name, dut_if.i_eb_wr_data[0]);
               sb.exp_o_p0_sb_wdata_notification(dut_if.i_eb_wr_data[0]);
	   end
           else if (exp_o_p0_sb_wr_valid_from_wb_q2) begin
               $display("(time: %0d) %s: i_p0_wb_wdata = (%0h)", $time, name, dut_if.i_eb_wr_data[0]);
               sb.exp_o_p0_sb_wdata_notification(dut_if.i_wb_wr_data[0]);
	   end


        // p1:

            // 1/7/2019: push expected o_eb_wr_data[1]

           exp_o_p1_eb_wr_valid_q2 = exp_o_p1_eb_wr_valid_q1;
           exp_o_p1_eb_wr_valid_q1 = exp_o_p1_eb_wr_valid;
           if (dut_if.i_eb_wr_req[1].vld && (dut_if.i_eb_wr_req[1].node_col > dut_if.i_eb_node_col))
                exp_o_p1_eb_wr_valid = 1;
           else if (dut_if.i_eb_wr_req[1].vld && (dut_if.i_eb_wr_req[1].node_col < dut_if.i_eb_node_col))
           begin
                exp_o_p1_eb_wr_valid = 0;
                $display("(time: %0d) %s: WARNING: illegal P1 EB WR req gets dropped  !!! ", $time, name);
           end
           else  exp_o_p1_eb_wr_valid = 0;


           if (exp_o_p1_eb_wr_valid) begin
               $display("(time: %0d) %s: --------", $time, name);
               $display("(time: %0d) %s: A P1 EB WR req is expected to go thru to EB output", $time, name);
               $display("(time: %0d) %s: i_p1_eb_wreq=(%0h)", $time, name, dut_if.i_eb_wr_req[1]);
               sb.exp_o_p1_eb_wreq_notification(dut_if.i_eb_wr_req[1]);
	   end

           if (exp_o_p1_eb_wr_valid_q2) begin
               $display("(time: %0d) %s: i_p1_eb_wdata = %0h", $time, name, dut_if.i_eb_wr_data[1]);
               sb.exp_o_p1_eb_wdata_notification(dut_if.i_eb_wr_data[1]);
	   end



            // 1/7/2019: push expected o_wb_wr_data[1] 

           exp_o_p1_wb_wr_valid_q2 = exp_o_p1_wb_wr_valid_q1;
           exp_o_p1_wb_wr_valid_q1 = exp_o_p1_wb_wr_valid;
           if (dut_if.i_wb_wr_req[1].vld && (dut_if.i_wb_wr_req[1].node_col < dut_if.i_eb_node_col))
                exp_o_p1_wb_wr_valid = 1;
           else if (dut_if.i_wb_wr_req[1].vld && (dut_if.i_wb_wr_req[1].node_col > dut_if.i_eb_node_col))
           begin 
                exp_o_p1_wb_wr_valid = 0;
                $display("(time: %0d) %s: WARNING: illegal P1 WB WR req gets dropped  !!! ", $time, name);
           end
           else  exp_o_p1_wb_wr_valid = 0;


           if (exp_o_p1_wb_wr_valid) begin
               $display("(time: %0d) %s: --------", $time, name);
               $display("(time: %0d) %s: A P1 WB WR req is expected to go thru to WB output", $time, name);
               $display("(time: %0d) %s: i_p1_wb_wreq=(%0h)", $time, name, dut_if.i_wb_wr_req[1]);
               sb.exp_o_p1_wb_wreq_notification(dut_if.i_wb_wr_req[1]);
	   end

           if (exp_o_p1_wb_wr_valid_q2) begin
               $display("(time: %0d) %s: i_p1_wb_wdata = %0h", $time, name, dut_if.i_wb_wr_data[1]);
               sb.exp_o_p1_wb_wdata_notification(dut_if.i_wb_wr_data[1]);
	   end



            // 1/7/2019: push expected o_nb_wr_data[1] 

           exp_o_p1_nb_wr_valid_q2 = exp_o_p1_nb_wr_valid_q1;
           exp_o_p1_nb_wr_valid_q1 = exp_o_p1_nb_wr_valid;
           if (dut_if.i_nb_wr_req[1].vld && (dut_if.i_nb_wr_req[1].node_row < dut_if.i_sb_node_row))
                exp_o_p1_nb_wr_valid = 1;
           else if (dut_if.i_nb_wr_req[1].vld && (dut_if.i_nb_wr_req[1].node_row > dut_if.i_sb_node_row))
           begin
                exp_o_p1_nb_wr_valid = 0;
                $display("(time: %0d) %s: WARNING: illegal P1 NB WR req gets dropped  !!! ", $time, name);
           end
           else  exp_o_p1_nb_wr_valid = 0;

           exp_o_p1_nb_wr_valid_from_eb_q2 = exp_o_p1_nb_wr_valid_from_eb_q1;
           exp_o_p1_nb_wr_valid_from_eb_q1 = exp_o_p1_nb_wr_valid_from_eb;
           if (dut_if.i_eb_wr_req[1].vld && (dut_if.i_eb_wr_req[1].node_col == dut_if.i_eb_node_col)
                                         && (dut_if.i_eb_wr_req[1].node_row < dut_if.i_sb_node_row) )
                exp_o_p1_nb_wr_valid_from_eb = 1;
	   else exp_o_p1_nb_wr_valid_from_eb = 0;

           exp_o_p1_nb_wr_valid_from_wb_q2 = exp_o_p1_nb_wr_valid_from_wb_q1;
           exp_o_p1_nb_wr_valid_from_wb_q1 = exp_o_p1_nb_wr_valid_from_wb;
           if (dut_if.i_wb_wr_req[1].vld && (dut_if.i_wb_wr_req[1].node_col == dut_if.i_eb_node_col)
                                         && (dut_if.i_wb_wr_req[1].node_row < dut_if.i_sb_node_row) )
                exp_o_p1_nb_wr_valid_from_wb = 1;
	   else exp_o_p1_nb_wr_valid_from_wb = 0;


           if (exp_o_p1_nb_wr_valid) begin
               $display("(time: %0d) %s: --------", $time, name);
               $display("(time: %0d) %s: A P1 NB WR req is expected to go thru to NB output", $time, name);
               $display("(time: %0d) %s: i_p1_nb_wreq=(%0h)", $time, name, dut_if.i_nb_wr_req[1]);
               sb.exp_o_p1_nb_wreq_notification(dut_if.i_nb_wr_req[1]);
	   end
           else if (exp_o_p1_nb_wr_valid_from_eb) begin
               $display("(time: %0d) %s: --------", $time, name);
               $display("(time: %0d) %s: A P1 EB WR req is expected to go to NB output !!!", $time, name); 
               $display("(time: %0d) %s: i_p0_eb_wreq=(%0h)", $time, name, dut_if.i_eb_wr_req[0]);
                 exp_row_to_col_wreq.vld         = dut_if.i_eb_wr_req[1].vld;
                 exp_row_to_col_wreq.node_row    = dut_if.i_eb_wr_req[1].node_row;
                 exp_row_to_col_wreq.csr         = dut_if.i_eb_wr_req[1].csr;
                 exp_row_to_col_wreq.addr        = dut_if.i_eb_wr_req[1].addr;
                 exp_row_to_col_wreq.sema_val    = dut_if.i_eb_wr_req[1].sema_val;
                 exp_row_to_col_wreq.age         = dut_if.i_eb_wr_req[1].age;
               sb.exp_o_p1_nb_wreq_notification(exp_row_to_col_wreq); // (dut_if.i_eb_wr_erq[1]);
           end
           else if (exp_o_p1_nb_wr_valid_from_wb) begin
               $display("(time: %0d) %s: --------", $time, name);
               $display("(time: %0d) %s: A P1 WB WR req is expected to go to NB output !!!", $time, name); 
               $display("(time: %0d) %s: i_p0_wb_wreq=(%0h)", $time, name, dut_if.i_wb_wr_req[0]);
                 exp_row_to_col_wreq.vld         = dut_if.i_wb_wr_req[1].vld;
                 exp_row_to_col_wreq.node_row    = dut_if.i_wb_wr_req[1].node_row;
                 exp_row_to_col_wreq.csr         = dut_if.i_wb_wr_req[1].csr;
                 exp_row_to_col_wreq.addr        = dut_if.i_wb_wr_req[1].addr;
                 exp_row_to_col_wreq.sema_val    = dut_if.i_wb_wr_req[1].sema_val;
                 exp_row_to_col_wreq.age         = dut_if.i_wb_wr_req[1].age;
               sb.exp_o_p1_nb_wreq_notification(exp_row_to_col_wreq); // (dut_if.i_wb_wr_data[1]);
           end


           if (exp_o_p1_nb_wr_valid_q2) begin
               $display("(time: %0d) %s: i_p1_nb_wdata = %0h", $time, name, dut_if.i_nb_wr_data[1]);
               sb.exp_o_p1_nb_wdata_notification(dut_if.i_nb_wr_data[1]);
	   end
           else if (exp_o_p1_nb_wr_valid_from_eb_q2) begin
               $display("(time: %0d) %s: i_p1_eb_wdata = %0h", $time, name, dut_if.i_eb_wr_data[1]);
               sb.exp_o_p1_nb_wdata_notification(dut_if.i_eb_wr_data[1]);
	   end
           else if (exp_o_p1_nb_wr_valid_from_wb_q2) begin
               $display("(time: %0d) %s: i_p1_wb_wdata = %0h", $time, name, dut_if.i_wb_wr_data[1]);
               sb.exp_o_p1_nb_wdata_notification(dut_if.i_wb_wr_data[1]);
	   end



            // 1/7/2019: push expected o_sb_wr_data[1] 

           exp_o_p1_sb_wr_valid_q2 = exp_o_p1_sb_wr_valid_q1;
           exp_o_p1_sb_wr_valid_q1 = exp_o_p1_sb_wr_valid;
           if (dut_if.i_sb_wr_req[1].vld && (dut_if.i_sb_wr_req[1].node_row > dut_if.i_sb_node_row))
                exp_o_p1_sb_wr_valid = 1;
           else if (dut_if.i_sb_wr_req[1].vld && (dut_if.i_sb_wr_req[1].node_row < dut_if.i_sb_node_row))
           begin
                exp_o_p1_sb_wr_valid = 0;
                $display("(time: %0d) %s: WARNING: illegal P1 SB WR req gets dropped  !!! ", $time, name);
           end
           else  exp_o_p1_sb_wr_valid = 0;

           exp_o_p1_sb_wr_valid_from_eb_q2 = exp_o_p1_sb_wr_valid_from_eb_q1;
           exp_o_p1_sb_wr_valid_from_eb_q1 = exp_o_p1_sb_wr_valid_from_eb;
           if (dut_if.i_eb_wr_req[1].vld && (dut_if.i_eb_wr_req[1].node_col == dut_if.i_eb_node_col)
                                         && (dut_if.i_eb_wr_req[1].node_row > dut_if.i_sb_node_row) )
                exp_o_p1_sb_wr_valid_from_eb = 1;
	   else exp_o_p1_sb_wr_valid_from_eb = 0;

           exp_o_p1_sb_wr_valid_from_wb_q2 = exp_o_p1_sb_wr_valid_from_wb_q1;
           exp_o_p1_sb_wr_valid_from_wb_q1 = exp_o_p1_sb_wr_valid_from_wb;
           if (dut_if.i_wb_wr_req[1].vld && (dut_if.i_wb_wr_req[1].node_col == dut_if.i_eb_node_col)
                                         && (dut_if.i_wb_wr_req[1].node_row > dut_if.i_sb_node_row) )
                exp_o_p1_sb_wr_valid_from_wb = 1;
	   else exp_o_p1_sb_wr_valid_from_wb = 0;


           if (exp_o_p1_sb_wr_valid) begin
               $display("(time: %0d) %s: --------", $time, name);
               $display("(time: %0d) %s: A P1 SB WR req is expected to go thru to SB output", $time, name);
               $display("(time: %0d) %s: i_p1_sb_wreq=(%0h)", $time, name, dut_if.i_sb_wr_req[1]);
               sb.exp_o_p1_sb_wreq_notification(dut_if.i_sb_wr_req[1]);
	   end
           else if (exp_o_p1_sb_wr_valid_from_eb) begin
               $display("(time: %0d) %s: --------", $time, name);
               $display("(time: %0d) %s: A P1 EB ER req is expected to go to SB output !!!", $time, name); 
               $display("(time: %0d) %s: i_p0_eb_wreq=(%0h)", $time, name, dut_if.i_eb_wr_req[0]);
                 exp_row_to_col_wreq.vld         = dut_if.i_eb_wr_req[1].vld;
                 exp_row_to_col_wreq.node_row    = dut_if.i_eb_wr_req[1].node_row;
                 exp_row_to_col_wreq.csr         = dut_if.i_eb_wr_req[1].csr;
                 exp_row_to_col_wreq.addr        = dut_if.i_eb_wr_req[1].addr;
                 exp_row_to_col_wreq.sema_val    = dut_if.i_eb_wr_req[1].sema_val;
                 exp_row_to_col_wreq.age         = dut_if.i_eb_wr_req[1].age;
               sb.exp_o_p1_sb_wreq_notification(exp_row_to_col_wreq); // (dut_if.i_eb_wr_erq[1]);
           end
           else if (exp_o_p1_sb_wr_valid_from_wb) begin
               $display("(time: %0d) %s: --------", $time, name);
               $display("(time: %0d) %s: A P1 WB ER req is expected to go to SB output !!!", $time, name); 
               $display("(time: %0d) %s: i_p0_wb_wreq=(%0h)", $time, name, dut_if.i_wb_wr_req[0]);
                 exp_row_to_col_wreq.vld         = dut_if.i_wb_wr_req[1].vld;
                 exp_row_to_col_wreq.node_row    = dut_if.i_wb_wr_req[1].node_row;
                 exp_row_to_col_wreq.csr         = dut_if.i_wb_wr_req[1].csr;
                 exp_row_to_col_wreq.addr        = dut_if.i_wb_wr_req[1].addr;
                 exp_row_to_col_wreq.sema_val    = dut_if.i_wb_wr_req[1].sema_val;
                 exp_row_to_col_wreq.age         = dut_if.i_wb_wr_req[1].age;
               sb.exp_o_p1_sb_wreq_notification(exp_row_to_col_wreq); // (dut_if.i_wb_wr_data[1]);
           end


           if (exp_o_p1_sb_wr_valid_q2) begin
               $display("(time: %0d) %s: i_p1_sb_wdata = %0h", $time, name, dut_if.i_sb_wr_data[1]);
               sb.exp_o_p1_sb_wdata_notification(dut_if.i_sb_wr_data[1]);
	   end
           else if (exp_o_p1_sb_wr_valid_from_eb_q2) begin
               $display("(time: %0d) %s: i_p1_eb_wdata = %0h", $time, name, dut_if.i_eb_wr_data[1]);
               sb.exp_o_p1_sb_wdata_notification(dut_if.i_eb_wr_data[1]);
	   end
           else if (exp_o_p1_sb_wr_valid_from_wb_q2) begin
               $display("(time: %0d) %s: i_p1_wb_wdata = %0h", $time, name, dut_if.i_wb_wr_data[1]);
               sb.exp_o_p1_sb_wdata_notification(dut_if.i_wb_wr_data[1]);
	   end



        // wr to mem

            // 1/7/2019: push expected node_memory req 
            
           exp_p0_eb_wr_to_mem_valid_q2 = exp_p0_eb_wr_to_mem_valid_q1;
           exp_p0_wb_wr_to_mem_valid_q2 = exp_p0_wb_wr_to_mem_valid_q1;
           exp_p0_nb_wr_to_mem_valid_q2 = exp_p0_nb_wr_to_mem_valid_q1;
           exp_p0_sb_wr_to_mem_valid_q2 = exp_p0_sb_wr_to_mem_valid_q1;
           exp_p1_eb_wr_to_mem_valid_q2 = exp_p1_eb_wr_to_mem_valid_q1;
           exp_p1_wb_wr_to_mem_valid_q2 = exp_p1_wb_wr_to_mem_valid_q1;
           exp_p1_nb_wr_to_mem_valid_q2 = exp_p1_nb_wr_to_mem_valid_q1;
           exp_p1_sb_wr_to_mem_valid_q2 = exp_p1_sb_wr_to_mem_valid_q1;


           exp_p0_eb_wr_to_mem_valid_q1 = exp_p0_eb_wr_to_mem_valid;
           exp_p0_wb_wr_to_mem_valid_q1 = exp_p0_wb_wr_to_mem_valid;
           exp_p0_nb_wr_to_mem_valid_q1 = exp_p0_nb_wr_to_mem_valid;
           exp_p0_sb_wr_to_mem_valid_q1 = exp_p0_sb_wr_to_mem_valid;
           exp_p1_eb_wr_to_mem_valid_q1 = exp_p1_eb_wr_to_mem_valid;
           exp_p1_wb_wr_to_mem_valid_q1 = exp_p1_wb_wr_to_mem_valid;
           exp_p1_nb_wr_to_mem_valid_q1 = exp_p1_nb_wr_to_mem_valid;
           exp_p1_sb_wr_to_mem_valid_q1 = exp_p1_sb_wr_to_mem_valid;

           if (dut_if.i_eb_wr_req[0].vld && (dut_if.i_eb_wr_req[0].node_col == dut_if.i_eb_node_col)
                                         && (dut_if.i_eb_wr_req[0].node_row == dut_if.i_sb_node_row))
                exp_p0_eb_wr_to_mem_valid = 1;
	   else exp_p0_eb_wr_to_mem_valid = 0;

           if (dut_if.i_wb_wr_req[0].vld && (dut_if.i_wb_wr_req[0].node_col == dut_if.i_eb_node_col)
                                         && (dut_if.i_wb_wr_req[0].node_row == dut_if.i_sb_node_row))
                exp_p0_wb_wr_to_mem_valid = 1;
	   else exp_p0_wb_wr_to_mem_valid = 0;

           if (dut_if.i_nb_wr_req[0].vld && (dut_if.i_nb_wr_req[0].node_row == dut_if.i_sb_node_row))
                exp_p0_nb_wr_to_mem_valid = 1;
	   else exp_p0_nb_wr_to_mem_valid = 0;

           if (dut_if.i_sb_wr_req[0].vld && (dut_if.i_sb_wr_req[0].node_row == dut_if.i_sb_node_row))
                exp_p0_sb_wr_to_mem_valid = 1;
	   else exp_p0_sb_wr_to_mem_valid = 0;

           if (dut_if.i_eb_wr_req[1].vld && (dut_if.i_eb_wr_req[1].node_col == dut_if.i_eb_node_col)
                                         && (dut_if.i_eb_wr_req[1].node_row == dut_if.i_sb_node_row))
                exp_p1_eb_wr_to_mem_valid = 1;
	   else exp_p1_eb_wr_to_mem_valid = 0;

           if (dut_if.i_wb_wr_req[1].vld && (dut_if.i_wb_wr_req[1].node_col == dut_if.i_eb_node_col)
                                         && (dut_if.i_wb_wr_req[1].node_row == dut_if.i_sb_node_row))
                exp_p1_wb_wr_to_mem_valid = 1;
	   else exp_p1_wb_wr_to_mem_valid = 0;

           if (dut_if.i_nb_wr_req[1].vld && (dut_if.i_nb_wr_req[1].node_row == dut_if.i_sb_node_row))
                exp_p1_nb_wr_to_mem_valid = 1;
	   else exp_p1_nb_wr_to_mem_valid = 0;

           if (dut_if.i_sb_wr_req[1].vld && (dut_if.i_sb_wr_req[1].node_row == dut_if.i_sb_node_row))
                exp_p1_sb_wr_to_mem_valid = 1;
	   else exp_p1_sb_wr_to_mem_valid = 0;


                // push mem addr:
           if (exp_p0_eb_wr_to_mem_valid) begin
               $display("(time: %0d) %s: --------", $time, name);
               $display("(time: %0d) %s: A P0 EB WR req is expected to go to dut memory !!!", $time, name); 
               $display("(time: %0d) %s: i_p0_eb_wreq=(%0h)", $time, name, dut_if.i_eb_wr_req[0]);
               sb.exp_mem_wreq_notification(dut_if.i_eb_wr_req[0].addr);
	   end

           else if (exp_p0_wb_wr_to_mem_valid) begin
               $display("(time: %0d) %s: --------", $time, name);
               $display("(time: %0d) %s: A P0 WB WR req is expected to go to dut memory !!!", $time, name); 
               $display("(time: %0d) %s: i_p0_wb_wreq=(%0h)", $time, name, dut_if.i_wb_wr_req[0]);
               sb.exp_mem_wreq_notification(dut_if.i_wb_wr_req[0].addr);
	   end

           else if (exp_p0_nb_wr_to_mem_valid) begin
               $display("(time: %0d) %s: --------", $time, name);
               $display("(time: %0d) %s: A P0 NB WR req is expected to go to dut memory !!!", $time, name); 
               $display("(time: %0d) %s: i_p0_nb_wreq=(%0h)", $time, name, dut_if.i_nb_wr_req[0]);
               sb.exp_mem_wreq_notification(dut_if.i_nb_wr_req[0].addr);
	   end

           else if (exp_p0_sb_wr_to_mem_valid) begin
               $display("(time: %0d) %s: --------", $time, name);
               $display("(time: %0d) %s: A P0 SB WR req is expected to go to dut memory !!!", $time, name); 
               $display("(time: %0d) %s: i_p0_sb_wreq=(%0h)", $time, name, dut_if.i_sb_wr_req[0]);
               sb.exp_mem_wreq_notification(dut_if.i_sb_wr_req[0].addr);
	   end

           else if (exp_p1_eb_wr_to_mem_valid) begin
               $display("(time: %0d) %s: --------", $time, name);
               $display("(time: %0d) %s: A P1 EB WR req is expected to go to dut memory !!!", $time, name); 
               $display("(time: %0d) %s: i_p1_eb_wreq=(%0h)", $time, name, dut_if.i_eb_wr_req[1]);
               sb.exp_mem_wreq_notification(dut_if.i_eb_wr_req[1].addr);
	   end

           else if (exp_p1_wb_wr_to_mem_valid) begin
               $display("(time: %0d) %s: --------", $time, name);
               $display("(time: %0d) %s: A P1 WB WR req is expected to go to dut memory !!!", $time, name); 
               $display("(time: %0d) %s: i_p1_wb_wreq=(%0h)", $time, name, dut_if.i_wb_wr_req[1]);
               sb.exp_mem_wreq_notification(dut_if.i_wb_wr_req[1].addr);
	   end

           else if (exp_p1_nb_wr_to_mem_valid) begin
               $display("(time: %0d) %s: --------", $time, name);
               $display("(time: %0d) %s: A P1 NB WR req is expected to go to dut memory !!!", $time, name); 
               $display("(time: %0d) %s: i_p1_nb_wreq=(%0h)", $time, name, dut_if.i_nb_wr_req[1]);
               sb.exp_mem_wreq_notification(dut_if.i_nb_wr_req[1].addr);
	   end

           else if (exp_p1_sb_wr_to_mem_valid) begin
               $display("(time: %0d) %s: --------", $time, name);
               $display("(time: %0d) %s: A P1 SB WR req is expected to go to dut memory !!!", $time, name); 
               $display("(time: %0d) %s: i_p1_sb_wreq=(%0h)", $time, name, dut_if.i_sb_wr_req[1]);
               sb.exp_mem_wreq_notification(dut_if.i_sb_wr_req[1].addr);
	   end


           if      (exp_p0_eb_wr_to_mem_valid_q2) begin
               $display("(time: %0d) %s: i_p0_eb_wdata=(%0h)", $time, name, dut_if.i_eb_wr_data[0]);
               sb.exp_mem_wdata_notification(dut_if.i_eb_wr_data[0]);
	   end

           else if (exp_p0_wb_wr_to_mem_valid_q2) begin
               $display("(time: %0d) %s: i_p0_wb_wdata=(%0h)", $time, name, dut_if.i_wb_wr_data[0]);
               sb.exp_mem_wdata_notification(dut_if.i_wb_wr_data[0]);
	   end

           else if (exp_p0_nb_wr_to_mem_valid_q2) begin
               $display("(time: %0d) %s: i_p0_nb_wdata=(%0h)", $time, name, dut_if.i_nb_wr_data[0]);
               sb.exp_mem_wdata_notification(dut_if.i_nb_wr_data[0]);
	   end

           else if (exp_p0_sb_wr_to_mem_valid_q2) begin
               $display("(time: %0d) %s: i_p0_sb_wdata=(%0h)", $time, name, dut_if.i_sb_wr_data[0]);
               sb.exp_mem_wdata_notification(dut_if.i_sb_wr_data[0]);
	   end

           else if (exp_p1_eb_wr_to_mem_valid_q2) begin
               $display("(time: %0d) %s: i_p1_eb_wdata=(%0h)", $time, name, dut_if.i_eb_wr_data[1]);
               sb.exp_mem_wdata_notification(dut_if.i_eb_wr_data[1]);
	   end

           else if (exp_p1_wb_wr_to_mem_valid_q2) begin
               $display("(time: %0d) %s: i_p1_wb_wdata=(%0h)", $time, name, dut_if.i_wb_wr_data[1]);
               sb.exp_mem_wdata_notification(dut_if.i_wb_wr_data[1]);
	   end

           else if (exp_p1_nb_wr_to_mem_valid_q2) begin
               $display("(time: %0d) %s: i_p1_nb_wdata=(%0h)", $time, name, dut_if.i_nb_wr_data[1]);
               sb.exp_mem_wdata_notification(dut_if.i_nb_wr_data[1]);
	   end

           else if (exp_p1_sb_wr_to_mem_valid_q2) begin
               $display("(time: %0d) %s: i_p1_sb_wdata=(%0h)", $time, name, dut_if.i_sb_wr_data[1]);
               sb.exp_mem_wdata_notification(dut_if.i_sb_wr_data[1]);
	   end



        // check mem wr_data:

           if (mem_if_0.wr_en) begin
               $display("(time: %0d) %s: --------", $time, name);
               $display("(time: %0d) %s: A WR req to memory bank_0 is detected", $time, name);
            // $display("(time: %0d) %s: mem_if_0.wdata=(%0h)", $time, name, mem_if_0.wr_data);
               sb.check_mem_wr_notification({2'h0, mem_if_0.adr}, mem_if_0.wr_data);
	   end
           else if (mem_if_1.wr_en) begin
               $display("(time: %0d) %s: --------", $time, name);
               $display("(time: %0d) %s: A WR req to memory bank_1 is detected", $time, name);
            // $display("(time: %0d) %s: mem_if_1.wdata=(%0h)", $time, name, mem_if_1.wr_data);
               sb.check_mem_wr_notification({2'h1, mem_if_1.adr}, mem_if_1.wr_data);
	   end
           else if (mem_if_2.wr_en) begin
               $display("(time: %0d) %s: --------", $time, name);
               $display("(time: %0d) %s: A WR req to memory bank_2 is detected", $time, name);
            // $display("(time: %0d) %s: mem_if_2.wdata=(%0h)", $time, name, mem_if_2.wr_data);
               sb.check_mem_wr_notification({2'h2, mem_if_2.adr}, mem_if_2.wr_data);
	   end
           else if (mem_if_3.wr_en) begin
               $display("(time: %0d) %s: --------", $time, name);
               $display("(time: %0d) %s: A WR req to memory bank_3 is detected", $time, name);
            // $display("(time: %0d) %s: mem_if_3.wdata=(%0h)", $time, name, mem_if_3.wr_data);
               sb.check_mem_wr_notification({2'h3, mem_if_3.adr}, mem_if_3.wr_data);
	   end


        // check p0 wr data:

           o_p0_eb_wr_valid_q2 = o_p0_eb_wr_valid_q1;
           o_p0_eb_wr_valid_q1 = o_p0_eb_wr_valid;
           o_p0_eb_wr_valid    = dut_if.o_eb_wr_req[0].vld;

           if (o_p0_eb_wr_valid) begin
               $display("(time: %0d) %s: --------", $time, name);
               $display("(time: %0d) %s: A P0 EB WR req output is detected", $time, name);
               sb.check_p0_eb_wreq_notification(dut_if.o_eb_wr_req[0]);
	   end

           if (o_p0_eb_wr_valid_q2) begin
               $display("(time: %0d) %s: --------", $time, name);
               $display("(time: %0d) %s: A P0 EB WR data output is detected", $time, name);
               sb.check_p0_eb_wdata_notification(dut_if.o_eb_wr_data[0]);
	   end


           o_p0_wb_wr_valid_q2 = o_p0_wb_wr_valid_q1;
           o_p0_wb_wr_valid_q1 = o_p0_wb_wr_valid;
           o_p0_wb_wr_valid    = dut_if.o_wb_wr_req[0].vld;

           if (o_p0_wb_wr_valid) begin
               $display("(time: %0d) %s: --------", $time, name);
               $display("(time: %0d) %s: A P0 WB WR req output is detected", $time, name);
               sb.check_p0_wb_wreq_notification(dut_if.o_wb_wr_req[0]);
	   end

           if (o_p0_wb_wr_valid_q2) begin
               $display("(time: %0d) %s: --------", $time, name);
               $display("(time: %0d) %s: A P0 WB WR data output is detected", $time, name);
               sb.check_p0_wb_wdata_notification(dut_if.o_wb_wr_data[0]);
	   end


           o_p0_nb_wr_valid_q2 = o_p0_nb_wr_valid_q1;
           o_p0_nb_wr_valid_q1 = o_p0_nb_wr_valid;
           o_p0_nb_wr_valid    = dut_if.o_nb_wr_req[0].vld;

           if (o_p0_nb_wr_valid) begin
               $display("(time: %0d) %s: --------", $time, name);
               $display("(time: %0d) %s: A P0 NB WR req output is detected", $time, name);
               sb.check_p0_nb_wreq_notification(dut_if.o_nb_wr_req[0]);
	   end

           if (o_p0_nb_wr_valid_q2) begin
               $display("(time: %0d) %s: --------", $time, name);
               $display("(time: %0d) %s: A P0 NB WR data output is detected", $time, name);
               sb.check_p0_nb_wdata_notification(dut_if.o_nb_wr_data[0]);
	   end


           o_p0_sb_wr_valid_q2 = o_p0_sb_wr_valid_q1;
           o_p0_sb_wr_valid_q1 = o_p0_sb_wr_valid;
           o_p0_sb_wr_valid    = dut_if.o_sb_wr_req[0].vld;

           if (o_p0_sb_wr_valid) begin
               $display("(time: %0d) %s: --------", $time, name);
               $display("(time: %0d) %s: A P0 SB WR req output is detected", $time, name);
               sb.check_p0_sb_wreq_notification(dut_if.o_sb_wr_req[0]);
	   end

           if (o_p0_sb_wr_valid_q2) begin
               $display("(time: %0d) %s: --------", $time, name);
               $display("(time: %0d) %s: A P0 SB WR data output is detected", $time, name);
               sb.check_p0_sb_wdata_notification(dut_if.o_sb_wr_data[0]);
	   end


        // check p1 wr data:

           o_p1_eb_wr_valid_q2 = o_p1_eb_wr_valid_q1;
           o_p1_eb_wr_valid_q1 = o_p1_eb_wr_valid;
           o_p1_eb_wr_valid    = dut_if.o_eb_wr_req[1].vld;

           if (o_p1_eb_wr_valid) begin
               $display("(time: %0d) %s: --------", $time, name);
               $display("(time: %0d) %s: A P1 EB WR req output is detected", $time, name);
               sb.check_p1_eb_wreq_notification(dut_if.o_eb_wr_req[1]);
	   end

           if (o_p1_eb_wr_valid_q2) begin
               $display("(time: %0d) %s: --------", $time, name);
               $display("(time: %0d) %s: A P1 EB WR data output is detected", $time, name);
               sb.check_p1_eb_wdata_notification(dut_if.o_eb_wr_data[1]);
 	   end


           o_p1_wb_wr_valid_q2 = o_p1_wb_wr_valid_q1;
           o_p1_wb_wr_valid_q1 = o_p1_wb_wr_valid;
           o_p1_wb_wr_valid    = dut_if.o_wb_wr_req[1].vld;

           if (o_p1_wb_wr_valid) begin
               $display("(time: %0d) %s: --------", $time, name);
               $display("(time: %0d) %s: A P1 WB WR req output is detected", $time, name);
               sb.check_p1_wb_wreq_notification(dut_if.o_wb_wr_req[1]);
	   end

           if (o_p1_wb_wr_valid_q2) begin
               $display("(time: %0d) %s: --------", $time, name);
               $display("(time: %0d) %s: A P1 WB WR data output is detected", $time, name);
               sb.check_p1_wb_wdata_notification(dut_if.o_wb_wr_data[1]);
	   end


           o_p1_nb_wr_valid_q2 = o_p1_nb_wr_valid_q1;
           o_p1_nb_wr_valid_q1 = o_p1_nb_wr_valid;
           o_p1_nb_wr_valid    = dut_if.o_nb_wr_req[1].vld;

           if (o_p1_nb_wr_valid) begin
               $display("(time: %0d) %s: --------", $time, name);
               $display("(time: %0d) %s: A P1 NB WR req output is detected", $time, name);
               sb.check_p1_nb_wreq_notification(dut_if.o_nb_wr_req[1]);
	   end

           if (o_p1_nb_wr_valid_q2) begin
               $display("(time: %0d) %s: --------", $time, name);
               $display("(time: %0d) %s: A P1 NB WR data output is detected", $time, name);
               sb.check_p1_nb_wdata_notification(dut_if.o_nb_wr_data[1]);
  	   end


           o_p1_sb_wr_valid_q2 = o_p1_sb_wr_valid_q1;
           o_p1_sb_wr_valid_q1 = o_p1_sb_wr_valid;
           o_p1_sb_wr_valid    = dut_if.o_sb_wr_req[1].vld;


           if (o_p1_sb_wr_valid) begin
               $display("(time: %0d) %s: --------", $time, name);
               $display("(time: %0d) %s: A P1 SB WR req output is detected", $time, name);
               sb.check_p1_sb_wreq_notification(dut_if.o_sb_wr_req[1]);
	   end

           if (o_p1_sb_wr_valid_q2) begin
               $display("(time: %0d) %s: --------", $time, name);
               $display("(time: %0d) %s: A P1 SB WR data output is detected", $time, name);
               sb.check_p1_sb_wdata_notification(dut_if.o_sb_wr_data[1]);
	   end



// -----------------------------------------------------------------------------------
// 1/10/2019: push rreq to fifo and pop to check when rreq goes out node or to memory
// -----------------------------------------------------------------------------------


	// p0

           // push expected o_eb_rd_req[0]

           if (dut_if.i_eb_rd_req[0].vld && (dut_if.i_eb_rd_req[0].node_col > dut_if.i_eb_node_col))
                exp_o_p0_eb_rreq_valid = 1;
           else if (dut_if.i_eb_rd_req[0].vld && (dut_if.i_eb_rd_req[0].node_col < dut_if.i_eb_node_col))
           begin
                exp_o_p0_eb_rreq_valid = 0;
                $display("(time: %0d) %s: WARNING: illegal P0 EB RD req gets dropped  !!! ", $time, name);
           end
           else  exp_o_p0_eb_rreq_valid = 0;

           if (exp_o_p0_eb_rreq_valid) begin
               $display("(time: %0d) %s: --------", $time, name);
               $display("(time: %0d) %s: A P0 EB RD req is expected to go thru to EB output", $time, name);
               $display("(time: %0d) %s: i_p0_eb_rreq=(%0h)", $time, name, dut_if.i_eb_rd_req[0]);
               sb.exp_o_p0_eb_rreq_notification(dut_if.i_eb_rd_req[0]);
           end


           // push expected o_wb_rd_req[0]

           if (dut_if.i_wb_rd_req[0].vld && (dut_if.i_wb_rd_req[0].node_col < dut_if.i_eb_node_col))
                exp_o_p0_wb_rreq_valid = 1;
           else if (dut_if.i_wb_rd_req[0].vld && (dut_if.i_wb_rd_req[0].node_col > dut_if.i_eb_node_col))
           begin
                exp_o_p0_wb_rreq_valid = 0;
                $display("(time: %0d) %s: WARNING: illegal P0 WB RD req gets dropped  !!! ", $time, name);
           end
           else  exp_o_p0_wb_rreq_valid = 0;

           if (exp_o_p0_wb_rreq_valid) begin
               $display("(time: %0d) %s: --------", $time, name);
               $display("(time: %0d) %s: A P0 WB RD req is expected to go thru to WB output", $time, name);
               $display("(time: %0d) %s: i_p0_wb_rreq=(%0h)", $time, name, dut_if.i_wb_rd_req[0]);
               sb.exp_o_p0_wb_rreq_notification(dut_if.i_wb_rd_req[0]);
	   end


           // push expected o_nb_rd_req[0]
	
           if (dut_if.i_nb_rd_req[0].vld && (dut_if.i_nb_rd_req[0].node_row < dut_if.i_sb_node_row))
                exp_o_p0_nb_rreq_valid = 1;
           else if (dut_if.i_nb_rd_req[0].vld && (dut_if.i_nb_rd_req[0].node_row > dut_if.i_sb_node_row))
           begin
                exp_o_p0_nb_rreq_valid = 0;
                $display("(time: %0d) %s: WARNING: illegal P0 NB RD req gets dropped  !!! ", $time, name);
           end
           else  exp_o_p0_nb_rreq_valid = 0;

           if (dut_if.i_eb_rd_req[0].vld && (dut_if.i_eb_rd_req[0].node_col == dut_if.i_eb_node_col)
                                         && (dut_if.i_eb_rd_req[0].node_row < dut_if.i_sb_node_row) )
                exp_o_p0_nb_rreq_valid_from_eb = 1;
	   else exp_o_p0_nb_rreq_valid_from_eb = 0;

           if (dut_if.i_wb_rd_req[0].vld && (dut_if.i_wb_rd_req[0].node_col == dut_if.i_eb_node_col)
                                         && (dut_if.i_wb_rd_req[0].node_row < dut_if.i_sb_node_row) )
                exp_o_p0_nb_rreq_valid_from_wb = 1;
	   else exp_o_p0_nb_rreq_valid_from_wb = 0;


           if (exp_o_p0_nb_rreq_valid) begin
               $display("(time: %0d) %s: --------", $time, name);
               $display("(time: %0d) %s: A P0 NB RD req is expected to go thru to NB output", $time, name);
               $display("(time: %0d) %s: i_p0_nb_rreq=(%0h)", $time, name, dut_if.i_nb_rd_req[0]);
               sb.exp_o_p0_nb_rreq_notification(dut_if.i_nb_rd_req[0]);
	   end
           else if (exp_o_p0_nb_rreq_valid_from_eb) begin
                 exp_row_to_col_rreq.vld         = dut_if.i_eb_rd_req[0].vld;
                 exp_row_to_col_rreq.id          = dut_if.i_eb_rd_req[0].id;
                 exp_row_to_col_rreq.port_row    = dut_if.i_sb_node_row;  // source row = dut node row
                 exp_row_to_col_rreq.port_side   = 2'd3	;		  // source side: 0=north,1=s,2=e,3=w
                 exp_row_to_col_rreq.node_row    = dut_if.i_eb_rd_req[0].node_row;
                 exp_row_to_col_rreq.csr         = dut_if.i_eb_rd_req[0].csr;
                 exp_row_to_col_rreq.addr        = dut_if.i_eb_rd_req[0].addr;
                 exp_row_to_col_rreq.sema_val    = dut_if.i_eb_rd_req[0].sema_val;
                 exp_row_to_col_rreq.age         = dut_if.i_eb_rd_req[0].age;
               $display("(time: %0d) %s: --------", $time, name);
               $display("(time: %0d) %s: A P0 EB RD req is expected to go to NB output", $time, name);
               $display("(time: %0d) %s: i_p0_eb_rreq=(%0h)", $time, name, dut_if.i_eb_rd_req[0]);
               sb.exp_o_p0_nb_rreq_notification(exp_row_to_col_rreq); // (dut_if.i_eb_rd_req[0]);
           end
           else if (exp_o_p0_nb_rreq_valid_from_wb) begin
                 exp_row_to_col_rreq.vld         = dut_if.i_wb_rd_req[0].vld;
                 exp_row_to_col_rreq.id          = dut_if.i_wb_rd_req[0].id;
                 exp_row_to_col_rreq.port_row    = dut_if.i_sb_node_row;
                 exp_row_to_col_rreq.port_side   = 2'd2;	// source side: 0=north,1=s,2=e,3=w
                 exp_row_to_col_rreq.node_row    = dut_if.i_wb_rd_req[0].node_row;
                 exp_row_to_col_rreq.csr         = dut_if.i_wb_rd_req[0].csr;
                 exp_row_to_col_rreq.addr        = dut_if.i_wb_rd_req[0].addr;
                 exp_row_to_col_rreq.sema_val    = dut_if.i_wb_rd_req[0].sema_val;
                 exp_row_to_col_rreq.age         = dut_if.i_wb_rd_req[0].age;
               $display("(time: %0d) %s: --------", $time, name);
               $display("(time: %0d) %s: A P0 WB RD req is expected to go to NB output", $time, name);
               $display("(time: %0d) %s: i_p0_wb_rreq=(%0h)", $time, name, dut_if.i_wb_rd_req[0]);
               sb.exp_o_p0_nb_rreq_notification(exp_row_to_col_rreq); // (dut_if.i_wb_rd_req[0]);
           end


           // push expected o_sb_rd_req[0]

           if (dut_if.i_sb_rd_req[0].vld && (dut_if.i_sb_rd_req[0].node_row > dut_if.i_sb_node_row))
                exp_o_p0_sb_rreq_valid = 1;
           else if (dut_if.i_sb_rd_req[0].vld && (dut_if.i_sb_rd_req[0].node_row < dut_if.i_sb_node_row))
           begin
                exp_o_p0_sb_rreq_valid = 0;
                $display("(time: %0d) %s: WARNING: illegal P0 SB RD req gets dropped  !!! ", $time, name);
           end
           else  exp_o_p0_sb_rreq_valid = 0;

           if (dut_if.i_eb_rd_req[0].vld && (dut_if.i_eb_rd_req[0].node_col == dut_if.i_eb_node_col)
                                         && (dut_if.i_eb_rd_req[0].node_row > dut_if.i_sb_node_row) )
                exp_o_p0_sb_rreq_valid_from_eb = 1;
	   else exp_o_p0_sb_rreq_valid_from_eb = 0;

           if (dut_if.i_wb_rd_req[0].vld && (dut_if.i_wb_rd_req[0].node_col == dut_if.i_eb_node_col)
                                         && (dut_if.i_wb_rd_req[0].node_row > dut_if.i_sb_node_row) )
                exp_o_p0_sb_rreq_valid_from_wb = 1;
	   else exp_o_p0_sb_rreq_valid_from_wb = 0;


           if (exp_o_p0_sb_rreq_valid) begin
               $display("(time: %0d) %s: --------", $time, name);
               $display("(time: %0d) %s: A P0 SB RD req is expected to go thru to SB output", $time, name);
               $display("(time: %0d) %s: i_p0_sb_rreq=(%0h)", $time, name, dut_if.i_sb_rd_req[0]);
               sb.exp_o_p0_sb_rreq_notification(dut_if.i_sb_rd_req[0]);
	   end
           else if (exp_o_p0_sb_rreq_valid_from_eb) begin
                 exp_row_to_col_rreq.vld         = dut_if.i_eb_rd_req[0].vld;
                 exp_row_to_col_rreq.id          = dut_if.i_eb_rd_req[0].id;
                 exp_row_to_col_rreq.port_row    = dut_if.i_sb_node_row;  // source row
                 exp_row_to_col_rreq.port_side   = 2'd3 ;                 // source side: 0=north,1=s,2=e,3=w
                 exp_row_to_col_rreq.node_row    = dut_if.i_eb_rd_req[0].node_row;
                 exp_row_to_col_rreq.csr         = dut_if.i_eb_rd_req[0].csr;
                 exp_row_to_col_rreq.addr        = dut_if.i_eb_rd_req[0].addr;
                 exp_row_to_col_rreq.sema_val    = dut_if.i_eb_rd_req[0].sema_val;
                 exp_row_to_col_rreq.age         = dut_if.i_eb_rd_req[0].age;
               $display("(time: %0d) %s: --------", $time, name);
               $display("(time: %0d) %s: A P0 EB RD req is expected to go to SB output", $time, name);
               $display("(time: %0d) %s: i_p0_eb_rreq=(%0h)", $time, name, dut_if.i_eb_rd_req[0]);
               sb.exp_o_p0_sb_rreq_notification(exp_row_to_col_rreq); // (dut_if.i_eb_rd_erq[0]);
           end
           else if (exp_o_p0_sb_rreq_valid_from_wb) begin
                 exp_row_to_col_rreq.vld         = dut_if.i_wb_rd_req[0].vld;
                 exp_row_to_col_rreq.id          = dut_if.i_wb_rd_req[0].id;
                 exp_row_to_col_rreq.port_row    = dut_if.i_sb_node_row;  // source row
                 exp_row_to_col_rreq.port_side   = 2'd2 ;                 // source side: 0=north,1=s,2=e,3=w
                 exp_row_to_col_rreq.node_row    = dut_if.i_wb_rd_req[0].node_row;
                 exp_row_to_col_rreq.csr         = dut_if.i_wb_rd_req[0].csr;
                 exp_row_to_col_rreq.addr        = dut_if.i_wb_rd_req[0].addr;
                 exp_row_to_col_rreq.sema_val    = dut_if.i_wb_rd_req[0].sema_val;
                 exp_row_to_col_rreq.age         = dut_if.i_wb_rd_req[0].age;
               $display("(time: %0d) %s: --------", $time, name);
               $display("(time: %0d) %s: A P0 WB RD req is expected to go to SB output", $time, name);
               $display("(time: %0d) %s: i_p0_wb_rreq=(%0h)", $time, name, dut_if.i_wb_rd_req[0]);
               sb.exp_o_p0_sb_rreq_notification(exp_row_to_col_rreq); // (dut_if.i_wb_rd_req[0]);
           end


	// p1:

            // push expected o_eb_rd_req[1]

           if (dut_if.i_eb_rd_req[1].vld && (dut_if.i_eb_rd_req[1].node_col > dut_if.i_eb_node_col))
                exp_o_p1_eb_rreq_valid = 1;
           else if (dut_if.i_eb_rd_req[1].vld && (dut_if.i_eb_rd_req[1].node_col < dut_if.i_eb_node_col))
           begin
                exp_o_p1_eb_rreq_valid = 0;
                $display("(time: %0d) %s: WARNING: illegal P1 EB RD req gets dropped  !!! ", $time, name);
           end
           else  exp_o_p1_eb_rreq_valid = 0;

           if (exp_o_p1_eb_rreq_valid) begin
               $display("(time: %0d) %s: --------", $time, name);
               $display("(time: %0d) %s: A P1 EB RD req is expected to go thru to EB output", $time, name);
               $display("(time: %0d) %s: i_p1_eb_rreq=(%0h)", $time, name, dut_if.i_eb_rd_req[1]);
               sb.exp_o_p1_eb_rreq_notification(dut_if.i_eb_rd_req[1]);
	   end


            // push expected o_wb_rd_req[1]

           if (dut_if.i_wb_rd_req[1].vld && (dut_if.i_wb_rd_req[1].node_col < dut_if.i_eb_node_col))
                exp_o_p1_wb_rreq_valid = 1;
           else if (dut_if.i_wb_rd_req[1].vld && (dut_if.i_wb_rd_req[1].node_col > dut_if.i_eb_node_col))
           begin
                exp_o_p1_wb_rreq_valid = 0;
                $display("(time: %0d) %s: WARNING: illegal P1 WB RD req gets dropped  !!! ", $time, name);
           end
           else  exp_o_p1_wb_rreq_valid = 0;

           if (exp_o_p1_wb_rreq_valid) begin
               $display("(time: %0d) %s: --------", $time, name);
               $display("(time: %0d) %s: A P1 WB RD req is expected to go thru to WB output", $time, name);
               $display("(time: %0d) %s: i_p1_wb_rreq=(%0h)", $time, name, dut_if.i_wb_rd_req[1]);
               sb.exp_o_p1_wb_rreq_notification(dut_if.i_wb_rd_req[1]);
	   end


            // push expected o_nb_rd_req[1]

           if (dut_if.i_nb_rd_req[1].vld && (dut_if.i_nb_rd_req[1].node_row < dut_if.i_sb_node_row))
                exp_o_p1_nb_rreq_valid = 1;
           else if (dut_if.i_nb_rd_req[1].vld && (dut_if.i_nb_rd_req[1].node_row > dut_if.i_sb_node_row))
           begin
                exp_o_p1_nb_rreq_valid = 0;
                $display("(time: %0d) %s: WARNING: illegal P1 NB RD req gets dropped  !!! ", $time, name);
           end
           else  exp_o_p1_nb_rreq_valid = 0;

           if (dut_if.i_eb_rd_req[1].vld && (dut_if.i_eb_rd_req[1].node_col == dut_if.i_eb_node_col)
                                         && (dut_if.i_eb_rd_req[1].node_row < dut_if.i_sb_node_row) )
                exp_o_p1_nb_rreq_valid_from_eb = 1;
	   else exp_o_p1_nb_rreq_valid_from_eb = 0;

           if (dut_if.i_wb_rd_req[1].vld && (dut_if.i_wb_rd_req[1].node_col == dut_if.i_eb_node_col)
                                         && (dut_if.i_wb_rd_req[1].node_row < dut_if.i_sb_node_row) )
                exp_o_p1_nb_rreq_valid_from_wb = 1;
	   else exp_o_p1_nb_rreq_valid_from_wb = 0;


           if (exp_o_p1_nb_rreq_valid) begin
               $display("(time: %0d) %s: --------", $time, name);
               $display("(time: %0d) %s: A P1 NB RD req is expected to go thru to NB output", $time, name);
               $display("(time: %0d) %s: i_p1_nb_rreq=(%0h)", $time, name, dut_if.i_nb_rd_req[1]);
               sb.exp_o_p1_nb_rreq_notification(dut_if.i_nb_rd_req[1]);
	   end
           else if (exp_o_p1_nb_rreq_valid_from_eb) begin
                 exp_row_to_col_rreq.vld         = dut_if.i_eb_rd_req[1].vld;
                 exp_row_to_col_rreq.id          = dut_if.i_eb_rd_req[1].vld;
                 exp_row_to_col_rreq.port_row    = dut_if.i_sb_node_row;  // source row
                 exp_row_to_col_rreq.port_side   = 2'd3 ;                 // source side: 0=north,1=s,2=e,3=w
                 exp_row_to_col_rreq.node_row    = dut_if.i_eb_rd_req[1].node_row;
                 exp_row_to_col_rreq.csr         = dut_if.i_eb_rd_req[1].csr;
                 exp_row_to_col_rreq.addr        = dut_if.i_eb_rd_req[1].addr;
                 exp_row_to_col_rreq.sema_val    = dut_if.i_eb_rd_req[1].sema_val;
                 exp_row_to_col_rreq.age         = dut_if.i_eb_rd_req[1].age;
               $display("(time: %0d) %s: --------", $time, name);
               $display("(time: %0d) %s: A P1 EB RD req is expected to go to NB output", $time, name);
               $display("(time: %0d) %s: i_p1_eb_rreq=(%0h)", $time, name, dut_if.i_eb_rd_req[1]);
               sb.exp_o_p1_nb_rreq_notification(exp_row_to_col_rreq); // (dut_if.i_eb_rd_req[1]);
           end
           else if (exp_o_p1_nb_rreq_valid_from_wb) begin
                 exp_row_to_col_rreq.vld         = dut_if.i_wb_rd_req[1].vld;
                 exp_row_to_col_rreq.id          = dut_if.i_wb_rd_req[1].vld;
                 exp_row_to_col_rreq.port_row    = dut_if.i_sb_node_row;  // source row
                 exp_row_to_col_rreq.port_side   = 2'd2 ;                 // source side: 0=north,1=s,2=e,3=w
                 exp_row_to_col_rreq.node_row    = dut_if.i_wb_rd_req[1].node_row;
                 exp_row_to_col_rreq.csr         = dut_if.i_wb_rd_req[1].csr;
                 exp_row_to_col_rreq.addr        = dut_if.i_wb_rd_req[1].addr;
                 exp_row_to_col_rreq.sema_val    = dut_if.i_wb_rd_req[1].sema_val;
                 exp_row_to_col_rreq.age         = dut_if.i_wb_rd_req[1].age;
               $display("(time: %0d) %s: --------", $time, name);
               $display("(time: %0d) %s: A P1 WB RD req is expected to go to NB output", $time, name);
               $display("(time: %0d) %s: i_p1_wb_rreq=(%0h)", $time, name, dut_if.i_wb_rd_req[1]);
               sb.exp_o_p1_nb_rreq_notification(exp_row_to_col_rreq); // (dut_if.i_wb_wr_req[1]);
           end


            // push expected o_sb_rd_req[1]

           if (dut_if.i_sb_rd_req[1].vld && (dut_if.i_sb_rd_req[1].node_row > dut_if.i_sb_node_row))
                exp_o_p1_sb_rreq_valid = 1;
           else if (dut_if.i_sb_rd_req[1].vld && (dut_if.i_sb_rd_req[1].node_row < dut_if.i_sb_node_row))
           begin
                exp_o_p1_sb_rreq_valid = 0;
                $display("(time: %0d) %s: WARNING: illegal P1 SB RD req gets dropped  !!! ", $time, name);
           end
           else  exp_o_p1_sb_rreq_valid = 0;

           if (dut_if.i_eb_rd_req[1].vld && (dut_if.i_eb_rd_req[1].node_col == dut_if.i_eb_node_col)
                                         && (dut_if.i_eb_rd_req[1].node_row > dut_if.i_sb_node_row) )
                exp_o_p1_sb_rreq_valid_from_eb = 1;
	   else exp_o_p1_sb_rreq_valid_from_eb = 0;

           if (dut_if.i_wb_rd_req[1].vld && (dut_if.i_wb_rd_req[1].node_col == dut_if.i_eb_node_col)
                                         && (dut_if.i_wb_rd_req[1].node_row > dut_if.i_sb_node_row) )
                exp_o_p1_sb_rreq_valid_from_wb = 1;
	   else exp_o_p1_sb_rreq_valid_from_wb = 0;


           if (exp_o_p1_sb_rreq_valid) begin
               $display("(time: %0d) %s: --------", $time, name);
               $display("(time: %0d) %s: A P1 SB RD req is expected to go thru to SB output", $time, name);
               $display("(time: %0d) %s: i_p1_sb_rreq=(%0h)", $time, name, dut_if.i_sb_rd_req[1]);
               sb.exp_o_p1_sb_rreq_notification(dut_if.i_sb_rd_req[1]);
 	   end
           else if (exp_o_p1_sb_rreq_valid_from_eb) begin
                 exp_row_to_col_rreq.vld         = dut_if.i_eb_rd_req[1].vld;
                 exp_row_to_col_rreq.id          = dut_if.i_eb_rd_req[1].id;
                 exp_row_to_col_rreq.port_row    = dut_if.i_sb_node_row;  // source row
                 exp_row_to_col_rreq.port_side   = 2'd3 ;                 // source side: 0=north,1=s,2=e,3=w
                 exp_row_to_col_rreq.node_row    = dut_if.i_eb_rd_req[1].node_row;
                 exp_row_to_col_rreq.csr         = dut_if.i_eb_rd_req[1].csr;
                 exp_row_to_col_rreq.addr        = dut_if.i_eb_rd_req[1].addr;
                 exp_row_to_col_rreq.sema_val    = dut_if.i_eb_rd_req[1].sema_val;
                 exp_row_to_col_rreq.age         = dut_if.i_eb_rd_req[1].age;
               $display("(time: %0d) %s: --------", $time, name);
               $display("(time: %0d) %s: A P1 EB RD req is expected to go to SB output", $time, name);
               $display("(time: %0d) %s: i_p1_eb_rreq=(%0h)", $time, name, dut_if.i_eb_rd_req[1]);
               sb.exp_o_p1_sb_rreq_notification(exp_row_to_col_rreq); // (dut_if.i_eb_rd_req[1]);
           end
           else if (exp_o_p1_sb_rreq_valid_from_wb) begin
                 exp_row_to_col_rreq.vld         = dut_if.i_wb_rd_req[1].vld;
                 exp_row_to_col_rreq.id          = dut_if.i_wb_rd_req[1].id;
                 exp_row_to_col_rreq.port_row    = dut_if.i_sb_node_row;  // source row
                 exp_row_to_col_rreq.port_side   = 2'd2 ;                 // source side: 0=north,1=s,2=e,3=w
                 exp_row_to_col_rreq.node_row    = dut_if.i_wb_rd_req[1].node_row;
                 exp_row_to_col_rreq.csr         = dut_if.i_wb_rd_req[1].csr;
                 exp_row_to_col_rreq.addr        = dut_if.i_wb_rd_req[1].addr;
                 exp_row_to_col_rreq.sema_val    = dut_if.i_wb_rd_req[1].sema_val;
                 exp_row_to_col_rreq.age         = dut_if.i_wb_rd_req[1].age;
               $display("(time: %0d) %s: --------", $time, name);
               $display("(time: %0d) %s: A P1 WB RD req is expected to go to SB output", $time, name);
               $display("(time: %0d) %s: i_p1_wb_rreq=(%0h)", $time, name, dut_if.i_wb_rd_req[1]);
               sb.exp_o_p1_sb_rreq_notification(exp_row_to_col_rreq); // (dut_if.i_wb_rd_req[1]);
           end


	// rreq to mem

           if (dut_if.i_eb_rd_req[0].vld && (dut_if.i_eb_rd_req[0].node_col == dut_if.i_eb_node_col)
                                         && (dut_if.i_eb_rd_req[0].node_row == dut_if.i_sb_node_row))
                exp_p0_eb_rreq_to_mem_valid = 1;
	   else exp_p0_eb_rreq_to_mem_valid = 0;

           if (dut_if.i_wb_rd_req[0].vld && (dut_if.i_wb_rd_req[0].node_col == dut_if.i_eb_node_col)
                                         && (dut_if.i_wb_rd_req[0].node_row == dut_if.i_sb_node_row))
                exp_p0_wb_rreq_to_mem_valid = 1;
	   else exp_p0_wb_rreq_to_mem_valid = 0;

           if (dut_if.i_nb_rd_req[0].vld && (dut_if.i_nb_rd_req[0].node_row == dut_if.i_sb_node_row))
                exp_p0_nb_rreq_to_mem_valid = 1;
	   else exp_p0_nb_rreq_to_mem_valid = 0;

           if (dut_if.i_sb_rd_req[0].vld && (dut_if.i_sb_rd_req[0].node_row == dut_if.i_sb_node_row))
                exp_p0_sb_rreq_to_mem_valid = 1;
	   else exp_p0_sb_rreq_to_mem_valid = 0;

           if (dut_if.i_eb_rd_req[1].vld && (dut_if.i_eb_rd_req[1].node_col == dut_if.i_eb_node_col)
                                         && (dut_if.i_eb_rd_req[1].node_row == dut_if.i_sb_node_row))
                exp_p1_eb_rreq_to_mem_valid = 1;
	   else exp_p1_eb_rreq_to_mem_valid = 0;

           if (dut_if.i_wb_rd_req[1].vld && (dut_if.i_wb_rd_req[1].node_col == dut_if.i_eb_node_col)
                                         && (dut_if.i_wb_rd_req[1].node_row == dut_if.i_sb_node_row))
                exp_p1_wb_rreq_to_mem_valid = 1;
	   else exp_p1_wb_rreq_to_mem_valid = 0;

           if (dut_if.i_nb_rd_req[1].vld && (dut_if.i_nb_rd_req[1].node_row == dut_if.i_sb_node_row))
                exp_p1_nb_rreq_to_mem_valid = 1;
	   else exp_p1_nb_rreq_to_mem_valid = 0;

           if (dut_if.i_sb_rd_req[1].vld && (dut_if.i_sb_rd_req[1].node_row == dut_if.i_sb_node_row))
                exp_p1_sb_rreq_to_mem_valid = 1;
	   else exp_p1_sb_rreq_to_mem_valid = 0;


                // push rreq addr to mem;
           if (exp_p0_eb_rreq_to_mem_valid)
               sb.exp_mem_rreq_notification(dut_if.i_eb_rd_req[0].addr);
           else if (exp_p0_wb_rreq_to_mem_valid)
               sb.exp_mem_rreq_notification(dut_if.i_wb_rd_req[0].addr);
           else if (exp_p0_nb_rreq_to_mem_valid)
               sb.exp_mem_rreq_notification(dut_if.i_nb_rd_req[0].addr);
           else if (exp_p0_sb_rreq_to_mem_valid)
               sb.exp_mem_rreq_notification(dut_if.i_sb_rd_req[0].addr);
           else if (exp_p1_eb_rreq_to_mem_valid)
               sb.exp_mem_rreq_notification(dut_if.i_eb_rd_req[1].addr);
           else if (exp_p1_wb_rreq_to_mem_valid)
               sb.exp_mem_rreq_notification(dut_if.i_wb_rd_req[1].addr);
           else if (exp_p1_nb_rreq_to_mem_valid)
               sb.exp_mem_rreq_notification(dut_if.i_nb_rd_req[1].addr);
           else if (exp_p1_sb_rreq_to_mem_valid)
               sb.exp_mem_rreq_notification(dut_if.i_sb_rd_req[1].addr);



        // display rd request prediction

/*
           if (exp_o_p0_nb_rreq_valid_from_eb) begin
                $display("(time: %0d) %s: A P0 EB RD REQ goes to NB output !!!", $time, name);
                $display(": the EB input REQ id = (%0h)", dut_if.i_eb_rd_req[0].id);
                $display(": the EB input REQ addr = (%0h)", dut_if.i_eb_rd_req[0].addr);
                $display(": the NB output expected port_row = (%0d)", dut_if.i_sb_node_row);
                $display(": the NB output expected port_side = (%0d)", mby_msh_pkg::msh_west_side);
	   end
           else if (exp_o_p0_nb_rreq_valid_from_wb) begin
                $display("(time: %0d) %s: A P0 WB RD REQ goes to NB output !!!", $time, name);
                $display(": the WB input REQ id = (%0h)", dut_if.i_wb_rd_req[0].id);
                $display(": the WB input REQ addr = (%0h)", dut_if.i_wb_rd_req[0].addr);
                $display(": the NB output expected port_row = (%0d)", dut_if.i_sb_node_row);
                $display(": the NB output expected port_side = (%0d)", mby_msh_pkg::msh_east_side);
	   end
           else if (exp_o_p0_sb_rreq_valid_from_eb) begin
                $display("(time: %0d) %s: A P0 EB RD REQ goes to SB output !!!", $time, name);
                $display(": the EB input REQ id = (%0h)", dut_if.i_eb_rd_req[0].id);
                $display(": the EB input REQ addr = (%0h)", dut_if.i_eb_rd_req[0].addr);
                $display(": the SB output expected port_row = (%0d)", dut_if.i_sb_node_row);
                $display(": the SB output expected port_side = (%0d)", mby_msh_pkg::msh_west_side);
	   end
           else if (exp_o_p0_sb_rreq_valid_from_wb) begin
                $display("(time: %0d) %s: A P0 WB RD REQ goes to SB output !!!", $time, name);
                $display(": the WB input REQ id = (%0h)", dut_if.i_wb_rd_req[0].id);
                $display(": the WB input REQ addr = (%0h)", dut_if.i_wb_rd_req[0].addr);
                $display(": the SB output expected port_row = (%0d)", dut_if.i_sb_node_row);
                $display(": the SB output expected port_side = (%0d)", mby_msh_pkg::msh_east_side);
	   end

           else if (exp_o_p1_nb_rreq_valid_from_eb) begin
                $display("(time: %0d) %s: A P1 EB RD REQ goes to NB output !!!", $time, name);
                $display(": the EB input REQ id = (%0h)", dut_if.i_eb_rd_req[1].id);
                $display(": the EB input REQ addr = (%0h)", dut_if.i_eb_rd_req[1].addr);
                $display(": the NB output expected port_row = (%0d)", dut_if.i_sb_node_row);
                $display(": the NB output expected port_side = (%0d)", mby_msh_pkg::msh_west_side);
	   end
           else if (exp_o_p1_nb_rreq_valid_from_wb) begin
                $display("(time: %0d) %s: A P0 WB RD REQ goes to NB output !!!", $time, name);
                $display(": the WB input REQ id = (%0h)", dut_if.i_wb_rd_req[1].id);
                $display(": the WB input REQ addr = (%0h)", dut_if.i_wb_rd_req[1].addr);
                $display(": the NB output expected port_row = (%0d)", dut_if.i_sb_node_row);
                $display(": the NB output expected port_side = (%0d)", mby_msh_pkg::msh_east_side);
	   end
           else if (exp_o_p1_sb_rreq_valid_from_eb) begin
                $display("(time: %0d) %s: A P0 EB RD REQ goes to SB output !!!", $time, name);
                $display(": the EB input REQ id = (%0h)", dut_if.i_eb_rd_req[1].id);
                $display(": the EB input REQ addr = (%0h)", dut_if.i_eb_rd_req[1].addr);
                $display(": the SB output expected port_row = (%0d)", dut_if.i_sb_node_row);
                $display(": the SB output expected port_side = (%0d)", mby_msh_pkg::msh_west_side);
	   end
           else if (exp_o_p1_sb_rreq_valid_from_wb) begin
                $display("(time: %0d) %s: A P0 WB RD REQ goes to SB output !!!", $time, name);
                $display(": the WB input REQ id = (%0h)", dut_if.i_wb_rd_req[1].id);
                $display(": the WB input REQ addr = (%0h)", dut_if.i_wb_rd_req[1].addr);
                $display(": the SB output expected port_row = (%0d)", dut_if.i_sb_node_row);
                $display(": the SB output expected port_side = (%0d)", mby_msh_pkg::msh_east_side);
	   end

           else if (exp_p0_eb_rreq_to_mem_valid)
                $display("(time: %0d) %s: A P0 EB RD REQ goes to NODE memory, input id = (%0h), addr = (%0h)",
                          $time, name, dut_if.i_eb_rd_req[0].id, dut_if.i_eb_rd_req[0].addr);
           else if (exp_p0_wb_rreq_to_mem_valid)
                $display("(time: %0d) %s: A P0 WB RD REQ goes to NODE memory, input id = (%0h), addr = (%0h)",
                          $time, name, dut_if.i_wb_rd_req[0].id, dut_if.i_wb_rd_req[0].addr);
           else if (exp_p0_nb_rreq_to_mem_valid)
                $display("(time: %0d) %s: A P0 NB RD REQ goes to NODE memory, input id = (%0h), addr = (%0h)",
                          $time, name, dut_if.i_nb_rd_req[0].id, dut_if.i_nb_rd_req[0].addr);
           else if (exp_p0_sb_rreq_to_mem_valid)
                $display("(time: %0d) %s: A P0 SB RD REQ goes to NODE memory, input id = (%0h), addr = (%0h)",
                          $time, name, dut_if.i_sb_rd_req[0].id, dut_if.i_sb_rd_req[0].addr);

           else if (exp_p1_eb_rreq_to_mem_valid)
                $display("(time: %0d) %s: A P1 EB RD REQ goes to NODE memory, input id = (%0h), addr = (%0h)",
                          $time, name, dut_if.i_eb_rd_req[1].id, dut_if.i_eb_rd_req[1].addr);
           else if (exp_p1_wb_rreq_to_mem_valid)
                $display("(time: %0d) %s: A P1 WB RD REQ goes to NODE memory, input id = (%0h), addr = (%0h)",
                          $time, name, dut_if.i_wb_rd_req[1].id, dut_if.i_wb_rd_req[1].addr);
           else if (exp_p1_nb_rreq_to_mem_valid)
                $display("(time: %0d) %s: A P1 NB RD REQ goes to NODE memory, input id = (%0h), addr = (%0h)",
                          $time, name, dut_if.i_nb_rd_req[1].id, dut_if.i_nb_rd_req[1].addr);
           else if (exp_p1_sb_rreq_to_mem_valid)
                $display("(time: %0d) %s: A P1 SB RD REQ goes to NODE memory, input id = (%0h), addr = (%0h)",
                          $time, name, dut_if.i_sb_rd_req[1].id, dut_if.i_sb_rd_req[1].addr);

           else if (exp_o_p0_eb_rreq_valid)
                $display("(time: %0d) %s: A P0 EB RD REQ is expected to go thru to EB output, id = (%0h), addr = (%0h)",
                          $time, name, dut_if.i_eb_rd_req[0].id, dut_if.i_eb_rd_req[0].addr);
           else if (exp_o_p0_wb_rreq_valid)
                $display("(time: %0d) %s: A P0 WB RD REQ is expected to go thru to WB output, id = (%0h), addr = (%0h)",
                          $time, name, dut_if.i_wb_rd_req[0].id, dut_if.i_wb_rd_req[0].addr);

           else if (exp_o_p0_nb_rreq_valid) begin
                $display("(time: %0d) %s: A P0 NB RD REQ is expected to go thru to NB output", $time, name);
                $display(": the P0 NB input REQ id = (%0h)", dut_if.i_nb_rd_req[0].id);
                $display(": the P0 NB input REQ addr = (%0h)", dut_if.i_nb_rd_req[0].addr);
                $display(": the P0 NB input REQ port_row = (%0d)", dut_if.i_nb_rd_req[0].port_row);
                $display(": the P0 NB input REQ port_side = (%0d)", dut_if.i_nb_rd_req[0].port_side);
	   end

           else if (exp_o_p0_sb_rreq_valid) begin
                $display("(time: %0d) %s: A P0 SB RD REQ is expected to go thru to SB output", $time, name);
                $display(": the P0 SB input REQ id = (%0h)", dut_if.i_sb_rd_req[0].id);
                $display(": the P0 SB input REQ addr = (%0h)", dut_if.i_sb_rd_req[0].addr);
                $display(": the P0 SB input REQ port_row = (%0d)", dut_if.i_sb_rd_req[0].port_row);
                $display(": the P0 SB input REQ port_side = (%0d)", dut_if.i_sb_rd_req[0].port_side);
	   end

           else if (exp_o_p1_eb_rreq_valid)
                $display("(time: %0d) %s: A P1 EB RD REQ is expected to go thru to EB output, id = (%0h), addr = (%0h)",
                          $time, name, dut_if.i_eb_rd_req[1].id, dut_if.i_eb_rd_req[1].addr);
           else if (exp_o_p1_wb_rreq_valid)
                $display("(time: %0d) %s: A P1 WB RD REQ is expected to go thru to WB output, id = (%0h), addr = (%0h)",
                          $time, name, dut_if.i_wb_rd_req[1].id, dut_if.i_wb_rd_req[1].addr);

           else if (exp_o_p1_nb_rreq_valid) begin
                $display("(time: %0d) %s: A P1 NB RD REQ is expected to go thru to NB output", $time, name);
                $display(": the P1 NB input REQ id = (%0h)", dut_if.i_nb_rd_req[1].id);
                $display(": the P1 NB input REQ addr = (%0h)", dut_if.i_nb_rd_req[1].addr);
                $display(": the P1 NB input REQ port_row = (%0d)", dut_if.i_nb_rd_req[1].port_row);
                $display(": the P1 NB input REQ port_side = (%0d)", dut_if.i_nb_rd_req[1].port_side);
	   end

           else if (exp_o_p1_sb_rreq_valid) begin
                $display("(time: %0d) %s: A P1 SB RD REQ is expected to go thru to SB output", $time, name);
                $display(": the P1 SB input REQ id = (%0h)", dut_if.i_sb_rd_req[1].id);
                $display(": the P1 SB input REQ addr = (%0h)", dut_if.i_sb_rd_req[1].addr);
                $display(": the P1 SB input REQ port_row = (%0d)", dut_if.i_sb_rd_req[1].port_row);
                $display(": the P1 SB input REQ port_side = (%0d)", dut_if.i_sb_rd_req[1].port_side);
	   end

*/


	// check rreq.addr at mem interface

           if (mem_if_0.rd_en) begin
               $display("(time: %0d) %s: --------", $time, name);
               $display("(time: %0d) %s: A RD req to memory bank_0 is detected", $time, name);
               sb.check_mem_rreq_notification(mem_if_0.adr);
	   end
           else if (mem_if_1.rd_en) begin
               $display("(time: %0d) %s: --------", $time, name);
               $display("(time: %0d) %s: A RD req to memory bank_1 is detected", $time, name);
               sb.check_mem_rreq_notification(mem_if_1.adr);
	   end
           else if (mem_if_2.rd_en) begin
               $display("(time: %0d) %s: --------", $time, name);
               $display("(time: %0d) %s: A RD req to memory bank_2 is detected", $time, name);
               sb.check_mem_rreq_notification(mem_if_2.adr);
	   end
           else if (mem_if_3.rd_en) begin
               $display("(time: %0d) %s: --------", $time, name);
               $display("(time: %0d) %s: A RD req to memory bank_3 is detected", $time, name);
               sb.check_mem_rreq_notification(mem_if_3.adr);
	   end



        // check rreq at output

           o_p0_eb_rreq_valid    = dut_if.o_eb_rd_req[0].vld;
           if (o_p0_eb_rreq_valid) begin
               $display("(time: %0d) %s: --------", $time, name);
               $display("(time: %0d) %s: A P0 EB RD req OUTPUT is detected", $time, name);
               sb.check_p0_eb_rreq_notification(dut_if.o_eb_rd_req[0]);
	   end

           o_p0_wb_rreq_valid    = dut_if.o_wb_rd_req[0].vld;
           if (o_p0_wb_rreq_valid) begin
               $display("(time: %0d) %s: --------", $time, name);
               $display("(time: %0d) %s: A P0 WB RD req OUTPUT is detected", $time, name);
               sb.check_p0_wb_rreq_notification(dut_if.o_wb_rd_req[0]);
	   end

           o_p0_nb_rreq_valid    = dut_if.o_nb_rd_req[0].vld;
           if (o_p0_nb_rreq_valid) begin
               $display("(time: %0d) %s: --------", $time, name);
               $display("(time: %0d) %s: A P0 NB RD req OUTPUT is detected", $time, name);
               sb.check_p0_nb_rreq_notification(dut_if.o_nb_rd_req[0]);
	   end

           o_p0_sb_rreq_valid    = dut_if.o_sb_rd_req[0].vld;
           if (o_p0_sb_rreq_valid) begin
               $display("(time: %0d) %s: --------", $time, name);
               $display("(time: %0d) %s: A P0 SB RD req OUTPUT is detected", $time, name);
               sb.check_p0_sb_rreq_notification(dut_if.o_sb_rd_req[0]);
	   end


           o_p1_eb_rreq_valid    = dut_if.o_eb_rd_req[1].vld;
           if (o_p1_eb_rreq_valid) begin
               $display("(time: %0d) %s: --------", $time, name);
               $display("(time: %0d) %s: A P1 EB RD req OUTPUT is detected", $time, name);
               sb.check_p1_eb_rreq_notification(dut_if.o_eb_rd_req[1]);
	   end

           o_p1_wb_rreq_valid    = dut_if.o_wb_rd_req[1].vld;
           if (o_p1_wb_rreq_valid) begin
               $display("(time: %0d) %s: --------", $time, name);
               $display("(time: %0d) %s: A P1 WB RD req OUTPUT is detected", $time, name);
               sb.check_p1_wb_rreq_notification(dut_if.o_wb_rd_req[1]);
	   end

           o_p1_nb_rreq_valid    = dut_if.o_nb_rd_req[1].vld;
           if (o_p1_nb_rreq_valid) begin
               $display("(time: %0d) %s: --------", $time, name);
               $display("(time: %0d) %s: A P1 NB RD req OUTPUT is detected", $time, name);
               sb.check_p1_nb_rreq_notification(dut_if.o_nb_rd_req[1]);
	   end

           o_p1_sb_rreq_valid    = dut_if.o_sb_rd_req[1].vld;
           if (o_p1_sb_rreq_valid) begin
               $display("(time: %0d) %s: --------", $time, name);
               $display("(time: %0d) %s: A P1 SB RD req OUTPUT is detected", $time, name);
               sb.check_p1_sb_rreq_notification(dut_if.o_sb_rd_req[1]);
	   end



	// monitor rsp: 
	//
	// for an EB rreq with (req.col >= dut.col), get a WB rsp
	// i_eb_rreq can go to mem, or o_nb_rreq, or o_sb_rreq, but expect a o_wb_rsp at the end
	//
	// for a  WB rreq with (req.col <= dut.col), get an EB rsp
	//
	//               |------|    |------|
	// i_EB_rreq --> | node |    | node | <-- i_WB_rreq
	// o_WB_rsp  <-- |      |    |      | --> o_EB_rsp
	//               |------|    |------|
	//

  	// for a NB rreq with (req.row <= dut.row) or
	// for a SB rreq with (req.row >= dut.row),
	// may get a rsp to any side depending on port_row and port_side
	// port_row and port_side may be generated randomly by the testbench if no restriction applied
	//
	// if (rreq.port_row <  node.row), o_NB_rsp = 1
	// if (rreq.port_row >  node.row), o_SB_rsp = 1
	// if (rreq.port_row == node.row and rreq.port_side == N),  o_NB_rsp = 1
	// if (rreq.port_row == node.row and rreq.port_side == S),  o_SB_rsp = 1
	// if (rreq.port_row == node.row and rreq.port_side == E),  o_EB_rsp = 1
	// if (rreq.port_row == node.row and rreq.port_side == W),  o_WB_rsp = 1
	// 
  	//        i_SB_rreq      o_NB_rsp
  	//                 |    ^
  	//                 v    |
	//               |--------|
	// o_WE_rsp  <-- |  node  | --> o_EB_rsp
	//               |--------|
     	//                 ^    |
     	//                 |    v
     	//        i_NB_rreq    o_SB_rsp




	// push expected rsp //////////////////////////////////////


	// p0:

	// for EB rreq to expect a WB rsp output:

           if (dut_if.i_eb_rd_req[0].vld && (dut_if.i_eb_rd_req[0].node_col >= dut_if.i_eb_node_col)) begin
               $display("(time: %0d) %s: --------", $time, name);
               $display("(time: %0d) %s: A P0 EB rreq to expect a WB rsp output", $time, name);

                exp_o_p0_wb_rsp_from_i_eb_rreq.vld	= dut_if.i_eb_rd_req[0].vld;
                exp_o_p0_wb_rsp_from_i_eb_rreq.id	= dut_if.i_eb_rd_req[0].id;
               sb.exp_o_p0_wb_rsp_notification(exp_o_p0_wb_rsp_from_i_eb_rreq);

		i_p0_eb_rreq_to_exp_o_wb_rdata = 1;
	   end
	   else i_p0_eb_rreq_to_exp_o_wb_rdata = 0;


	// for WB rreq to expect an EB rsp output:

           if (dut_if.i_wb_rd_req[0].vld && (dut_if.i_wb_rd_req[0].node_col <= dut_if.i_eb_node_col)) begin
               $display("(time: %0d) %s: --------", $time, name);
               $display("(time: %0d) %s: A P0 WB rreq to expect an EB rsp output", $time, name);

                exp_o_p0_eb_rsp_from_i_wb_rreq.vld	= dut_if.i_wb_rd_req[0].vld;
                exp_o_p0_eb_rsp_from_i_wb_rreq.id	= dut_if.i_wb_rd_req[0].id;
               sb.exp_o_p0_eb_rsp_notification(exp_o_p0_eb_rsp_from_i_wb_rreq);

		i_p0_wb_rreq_to_exp_o_eb_rdata = 1;
	   end
	   else i_p0_wb_rreq_to_exp_o_eb_rdata = 0;


	// for NB rreq, to EB/WB/NB/SB rsp output

	// NB to EB
           if (dut_if.i_nb_rd_req[0].vld && (dut_if.i_nb_rd_req[0].node_row  <= dut_if.i_sb_node_row)
                                         && (dut_if.i_nb_rd_req[0].port_row  == dut_if.i_sb_node_row)
                                         && (dut_if.i_nb_rd_req[0].port_side == mby_msh_pkg::msh_east_side))
	   begin
               $display("(time: %0d) %s: --------", $time, name);
               $display("(time: %0d) %s: A P0 NB rreq to expect an EB rsp output", $time, name);

                exp_o_p0_eb_rsp_from_i_nb_rreq.vld	= dut_if.i_nb_rd_req[0].vld;
                exp_o_p0_eb_rsp_from_i_nb_rreq.id	= dut_if.i_nb_rd_req[0].id;
               sb.exp_o_p0_eb_rsp_notification(exp_o_p0_eb_rsp_from_i_nb_rreq);

		i_p0_nb_rreq_to_exp_o_eb_rdata = 1;
	   end
	   else i_p0_nb_rreq_to_exp_o_eb_rdata = 0;


	// NB to WB
           if (dut_if.i_nb_rd_req[0].vld && (dut_if.i_nb_rd_req[0].node_row  <= dut_if.i_sb_node_row)
                                         && (dut_if.i_nb_rd_req[0].port_row  == dut_if.i_sb_node_row)
                                         && (dut_if.i_nb_rd_req[0].port_side == mby_msh_pkg::msh_west_side))
	   begin
               $display("(time: %0d) %s: --------", $time, name);
               $display("(time: %0d) %s: A P0 NB rreq to expect an WB rsp output", $time, name);

                exp_o_p0_wb_rsp_from_i_nb_rreq.vld	= dut_if.i_nb_rd_req[0].vld;
                exp_o_p0_wb_rsp_from_i_nb_rreq.id	= dut_if.i_nb_rd_req[0].id;
               sb.exp_o_p0_wb_rsp_notification(exp_o_p0_wb_rsp_from_i_nb_rreq);

	        i_p0_nb_rreq_to_exp_o_wb_rdata = 1;
	   end
	   else i_p0_nb_rreq_to_exp_o_wb_rdata = 0;


	// NB to NB
           if ( (dut_if.i_nb_rd_req[0].vld && (dut_if.i_nb_rd_req[0].node_row  <= dut_if.i_sb_node_row)
                                           && (dut_if.i_nb_rd_req[0].port_row  == dut_if.i_sb_node_row)
                                           && (dut_if.i_nb_rd_req[0].port_side == mby_msh_pkg::msh_north_side))
	      ||
                (dut_if.i_nb_rd_req[0].vld && (dut_if.i_nb_rd_req[0].node_row  <= dut_if.i_sb_node_row)
                                           && (dut_if.i_nb_rd_req[0].port_row  <  dut_if.i_sb_node_row))
	      )
	   begin
               $display("(time: %0d) %s: --------", $time, name);
              $display("(time: %0d) %s: A P0 NB rreq to expect a NB rsp output due to port_row and side", $time, name);
           // $display("exp_o_nb_rsp_id = (%0h)", dut_if.i_nb_rd_req[0].id);
           // $display("exp_o_nb_rsp_port_row = (%0h)", dut_if.i_nb_rd_req[0].port_row);
           // $display("exp_o_nb_rsp_port_side = (%0h)", dut_if.i_nb_rd_req[0].port_side);

                exp_o_p0_nb_rsp_from_i_nb_rreq.vld		= dut_if.i_nb_rd_req[0].vld;
                exp_o_p0_nb_rsp_from_i_nb_rreq.id		= dut_if.i_nb_rd_req[0].id;
                exp_o_p0_nb_rsp_from_i_nb_rreq.port_side	= dut_if.i_nb_rd_req[0].port_side;
                exp_o_p0_nb_rsp_from_i_nb_rreq.port_row	= dut_if.i_nb_rd_req[0].port_row;
               sb.exp_o_p0_nb_rsp_notification(exp_o_p0_nb_rsp_from_i_nb_rreq);

	        i_p0_nb_rreq_to_exp_o_nb_rdata = 1;
	   end
	   else i_p0_nb_rreq_to_exp_o_nb_rdata = 0;


	// NB to SB
           if ( (dut_if.i_nb_rd_req[0].vld && (dut_if.i_nb_rd_req[0].node_row  <= dut_if.i_sb_node_row)
                                           && (dut_if.i_nb_rd_req[0].port_row  == dut_if.i_sb_node_row)
                                           && (dut_if.i_nb_rd_req[0].port_side == mby_msh_pkg::msh_south_side))
	      ||
                (dut_if.i_nb_rd_req[0].vld && (dut_if.i_nb_rd_req[0].node_row  <= dut_if.i_sb_node_row)
                                           && (dut_if.i_nb_rd_req[0].port_row  >  dut_if.i_sb_node_row))
	      )
	   begin
               $display("(time: %0d) %s: --------", $time, name);
               $display("(time: %0d) %s: A P0 NB rreq to expect a SB rsp output due to port row and side", $time, name);
           //  $display("exp_o_sb_rsp_id = (%0h)", dut_if.i_nb_rd_req[0].id);
           //  $display("exp_o_sb_rsp_port_row = (%0h)", dut_if.i_nb_rd_req[0].port_row);
           //  $display("exp_o_sb_rsp_port_side = (%0h)", dut_if.i_nb_rd_req[0].port_side);

                exp_o_p0_sb_rsp_from_i_nb_rreq.vld		= dut_if.i_nb_rd_req[0].vld;
                exp_o_p0_sb_rsp_from_i_nb_rreq.id		= dut_if.i_nb_rd_req[0].id;
                exp_o_p0_sb_rsp_from_i_nb_rreq.port_side	= dut_if.i_nb_rd_req[0].port_side;
                exp_o_p0_sb_rsp_from_i_nb_rreq.port_row	= dut_if.i_nb_rd_req[0].port_row;
               sb.exp_o_p0_sb_rsp_notification(exp_o_p0_sb_rsp_from_i_nb_rreq);

	        i_p0_nb_rreq_to_exp_o_sb_rdata = 1;
	   end	
	   else i_p0_nb_rreq_to_exp_o_sb_rdata = 0;


	// for SB rreq, to EB/WB/NB/SB rsp output

	// SB to EB:
           if (dut_if.i_sb_rd_req[0].vld && (dut_if.i_sb_rd_req[0].node_row  >= dut_if.i_sb_node_row)
                                         && (dut_if.i_sb_rd_req[0].port_row  == dut_if.i_sb_node_row)
                                         && (dut_if.i_sb_rd_req[0].port_side == mby_msh_pkg::msh_east_side))
	   begin
               $display("(time: %0d) %s: --------", $time, name);
               $display("(time: %0d) %s: A P0 SB rreq to expect an EB rsp output", $time, name);

                exp_o_p0_eb_rsp_from_i_sb_rreq.vld	= dut_if.i_sb_rd_req[0].vld;
                exp_o_p0_eb_rsp_from_i_sb_rreq.id	= dut_if.i_sb_rd_req[0].id;
               sb.exp_o_p0_eb_rsp_notification(exp_o_p0_eb_rsp_from_i_sb_rreq);

	        i_p0_sb_rreq_to_exp_o_eb_rdata = 1;
	   end
	   else i_p0_sb_rreq_to_exp_o_eb_rdata = 0;


	// SB to WB
           if (dut_if.i_sb_rd_req[0].vld && (dut_if.i_sb_rd_req[0].node_row  >= dut_if.i_sb_node_row)
                                         && (dut_if.i_sb_rd_req[0].port_row  == dut_if.i_sb_node_row)
                                         && (dut_if.i_sb_rd_req[0].port_side == mby_msh_pkg::msh_west_side))
	   begin
               $display("(time: %0d) %s: --------", $time, name);
               $display("(time: %0d) %s: A P0 SB rreq to expect an WB rsp output)", $time, name);

                exp_o_p0_wb_rsp_from_i_sb_rreq.vld	= dut_if.i_sb_rd_req[0].vld;
                exp_o_p0_wb_rsp_from_i_sb_rreq.id	= dut_if.i_sb_rd_req[0].id;
               sb.exp_o_p0_wb_rsp_notification(exp_o_p0_wb_rsp_from_i_sb_rreq);

	        i_p0_sb_rreq_to_exp_o_wb_rdata = 1;
	   end
	   else i_p0_sb_rreq_to_exp_o_wb_rdata = 0;


	// SB to NB
           if ( (dut_if.i_sb_rd_req[0].vld && (dut_if.i_sb_rd_req[0].node_row  >= dut_if.i_sb_node_row)
                                           && (dut_if.i_sb_rd_req[0].port_row  == dut_if.i_sb_node_row)
                                           && (dut_if.i_sb_rd_req[0].port_side == mby_msh_pkg::msh_north_side))
	      ||
                (dut_if.i_sb_rd_req[0].vld && (dut_if.i_sb_rd_req[0].node_row  >= dut_if.i_sb_node_row)
                                           && (dut_if.i_sb_rd_req[0].port_row  <  dut_if.i_sb_node_row))
	      )
	   begin
               $display("(time: %0d) %s: --------", $time, name);
               $display("(time: %0d) %s: A P0 SB rreq to expect a NB rsp output", $time, name);
            // $display("exp_o_nb_rsp_id = (%0h)", dut_if.i_sb_rd_req[0].id);
            // $display("exp_o_nb_rsp_port_row = (%0h)", dut_if.i_sb_rd_req[0].port_row);
            // $display("exp_o_nb_rsp_port_side = (%0h)", dut_if.i_sb_rd_req[0].port_side);

                exp_o_p0_nb_rsp_from_i_sb_rreq.vld		= dut_if.i_sb_rd_req[0].vld;
                exp_o_p0_nb_rsp_from_i_sb_rreq.id		= dut_if.i_sb_rd_req[0].id;
                exp_o_p0_nb_rsp_from_i_sb_rreq.port_side	= dut_if.i_sb_rd_req[0].port_side;
                exp_o_p0_nb_rsp_from_i_sb_rreq.port_row	= dut_if.i_sb_rd_req[0].port_row;
               sb.exp_o_p0_nb_rsp_notification(exp_o_p0_nb_rsp_from_i_sb_rreq);

	        i_p0_sb_rreq_to_exp_o_nb_rdata = 1;
	   end
	   else i_p0_sb_rreq_to_exp_o_nb_rdata = 0;


	// SB to SB
           if ( (dut_if.i_sb_rd_req[0].vld && (dut_if.i_sb_rd_req[0].node_row  >= dut_if.i_sb_node_row)
                                           && (dut_if.i_sb_rd_req[0].port_row  == dut_if.i_sb_node_row)
                                           && (dut_if.i_sb_rd_req[0].port_side == mby_msh_pkg::msh_south_side))
	      ||
                (dut_if.i_sb_rd_req[0].vld && (dut_if.i_nb_rd_req[0].node_row  >= dut_if.i_sb_node_row)
                                           && (dut_if.i_nb_rd_req[0].port_row  >  dut_if.i_sb_node_row))
	      )
	   begin
               $display("(time: %0d) %s: --------", $time, name);
               $display("(time: %0d) %s: A P0 SB rreq to expect a SB rsp output", $time, name);
            // $display("exp_o_sb_rsp_id = (%0h)", dut_if.i_sb_rd_req[0].id);
            // $display("exp_o_sb_rsp_port_row = (%0h)", dut_if.i_sb_rd_req[0].port_row);
            // $display("exp_o_sb_rsp_port_side = (%0h)", dut_if.i_sb_rd_req[0].port_side);

                exp_o_p0_sb_rsp_from_i_sb_rreq.vld		= dut_if.i_sb_rd_req[0].vld;
                exp_o_p0_sb_rsp_from_i_sb_rreq.id		= dut_if.i_sb_rd_req[0].id;
                exp_o_p0_sb_rsp_from_i_sb_rreq.port_side	= dut_if.i_sb_rd_req[0].port_side;
                exp_o_p0_sb_rsp_from_i_sb_rreq.port_row	= dut_if.i_sb_rd_req[0].port_row;
               sb.exp_o_p0_sb_rsp_notification(exp_o_p0_sb_rsp_from_i_sb_rreq);

	        i_p0_sb_rreq_to_exp_o_sb_rdata = 1;
	   end
	   else i_p0_sb_rreq_to_exp_o_sb_rdata = 0;


	// p1


        // for EB rreq to expect a WB rsp output:

           if (dut_if.i_eb_rd_req[1].vld && (dut_if.i_eb_rd_req[1].node_col >= dut_if.i_eb_node_col)) begin
               $display("(time: %0d) %s: --------", $time, name);
               $display("(time: %0d) %s: A P1 EB rreq to expect a WB rsp output", $time, name);
             
                exp_o_p1_wb_rsp_from_i_eb_rreq.vld		= dut_if.i_eb_rd_req[1].vld;
                exp_o_p1_wb_rsp_from_i_eb_rreq.id		= dut_if.i_eb_rd_req[1].id;
               sb.exp_o_p1_wb_rsp_notification(exp_o_p1_wb_rsp_from_i_eb_rreq);

	        i_p1_eb_rreq_to_exp_o_wb_rdata = 1;
	   end
	   else i_p1_eb_rreq_to_exp_o_wb_rdata = 0;


        // for WB rreq to expect an EB rsp output:

           if (dut_if.i_wb_rd_req[1].vld && (dut_if.i_wb_rd_req[1].node_col <= dut_if.i_eb_node_col)) begin
               $display("(time: %0d) %s: --------", $time, name);
               $display("(time: %0d) %s: A P1 WB rreq to expect an EB rsp output", $time, name);

                exp_o_p1_eb_rsp_from_i_wb_rreq.vld		= dut_if.i_wb_rd_req[1].vld;
                exp_o_p1_eb_rsp_from_i_wb_rreq.id		= dut_if.i_wb_rd_req[1].id;
               sb.exp_o_p1_eb_rsp_notification(exp_o_p1_eb_rsp_from_i_wb_rreq);

	        i_p1_wb_rreq_to_exp_o_eb_rdata = 1;
	   end
	   else i_p1_wb_rreq_to_exp_o_eb_rdata = 0;


        // for NB rreq, to EB/WB/NB/SB rsp output

        // NB to EB
           if (dut_if.i_nb_rd_req[1].vld && (dut_if.i_nb_rd_req[1].node_row  <= dut_if.i_sb_node_row)
                                         && (dut_if.i_nb_rd_req[1].port_row  == dut_if.i_sb_node_row)
                                         && (dut_if.i_nb_rd_req[1].port_side == mby_msh_pkg::msh_east_side))
	   begin
               $display("(time: %0d) %s: --------", $time, name);
               $display("(time: %0d) %s: A P1 NB rreq to expect an EB rsp output", $time, name);

                exp_o_p1_eb_rsp_from_i_nb_rreq.vld		= dut_if.i_nb_rd_req[1].vld;
                exp_o_p1_eb_rsp_from_i_nb_rreq.id		= dut_if.i_nb_rd_req[1].id;
               sb.exp_o_p1_eb_rsp_notification(exp_o_p1_eb_rsp_from_i_nb_rreq);

	        i_p1_nb_rreq_to_exp_o_eb_rdata = 1;
	   end
	   else i_p1_nb_rreq_to_exp_o_eb_rdata = 0;


        // NB to WB
           if (dut_if.i_nb_rd_req[1].vld && (dut_if.i_nb_rd_req[1].node_row  <= dut_if.i_sb_node_row)
                                         && (dut_if.i_nb_rd_req[1].port_row  == dut_if.i_sb_node_row)
                                         && (dut_if.i_nb_rd_req[1].port_side == mby_msh_pkg::msh_west_side))
	   begin
               $display("(time: %0d) %s: --------", $time, name);
               $display("(time: %0d) %s: A P1 NB rreq to expect a WB rsp output", $time, name);

                exp_o_p1_wb_rsp_from_i_nb_rreq.vld		= dut_if.i_nb_rd_req[1].vld;
                exp_o_p1_wb_rsp_from_i_nb_rreq.id		= dut_if.i_nb_rd_req[1].id;
               sb.exp_o_p1_wb_rsp_notification(exp_o_p1_wb_rsp_from_i_nb_rreq);

	        i_p1_nb_rreq_to_exp_o_wb_rdata = 1;
	   end
	   else i_p1_nb_rreq_to_exp_o_wb_rdata = 0;


        // NB to NB
           if ( (dut_if.i_nb_rd_req[1].vld && (dut_if.i_nb_rd_req[1].node_row  <= dut_if.i_sb_node_row)
                                           && (dut_if.i_nb_rd_req[1].port_row  == dut_if.i_sb_node_row)
                                           && (dut_if.i_nb_rd_req[1].port_side == mby_msh_pkg::msh_north_side))
              ||
                (dut_if.i_nb_rd_req[1].vld && (dut_if.i_nb_rd_req[1].node_row  <= dut_if.i_sb_node_row)
                                           && (dut_if.i_nb_rd_req[1].port_row  <  dut_if.i_sb_node_row))
              )
	   begin
               $display("(time: %0d) %s: --------", $time, name);
               $display("(time: %0d) %s: A P1 NB rreq to expect a NB rsp output:", $time, name);
            // $display("exp_o_nb_rsp_id = (%0h)", dut_if.i_nb_rd_req[1].id);
            // $display("exp_o_nb_rsp_port_row = (%0h)", dut_if.i_nb_rd_req[1].port_row);
            // $display("exp_o_nb_rsp_port_side = (%0h)", dut_if.i_nb_rd_req[1].port_side);

                exp_o_p1_nb_rsp_from_i_sb_rreq.vld		= dut_if.i_nb_rd_req[1].vld;
                exp_o_p1_nb_rsp_from_i_sb_rreq.id		= dut_if.i_nb_rd_req[1].id;
                exp_o_p1_nb_rsp_from_i_sb_rreq.port_side	= dut_if.i_nb_rd_req[1].port_side;
                exp_o_p1_nb_rsp_from_i_sb_rreq.port_row	= dut_if.i_nb_rd_req[1].port_row;
               sb.exp_o_p1_nb_rsp_notification(exp_o_p1_nb_rsp_from_i_nb_rreq);

	        i_p1_nb_rreq_to_exp_o_nb_rdata = 1;
	   end
	   else i_p1_nb_rreq_to_exp_o_nb_rdata = 0;


        // NB to SB
           if ( (dut_if.i_nb_rd_req[1].vld && (dut_if.i_nb_rd_req[1].node_row  <= dut_if.i_sb_node_row)
                                           && (dut_if.i_nb_rd_req[1].port_row  == dut_if.i_sb_node_row)
                                           && (dut_if.i_nb_rd_req[1].port_side == mby_msh_pkg::msh_south_side))
              ||
                (dut_if.i_nb_rd_req[1].vld && (dut_if.i_nb_rd_req[1].node_row  <= dut_if.i_sb_node_row)
                                           && (dut_if.i_nb_rd_req[1].port_row  >  dut_if.i_sb_node_row))
              )
	   begin
               $display("(time: %0d) %s: --------", $time, name);
               $display("(time: %0d) %s: A P1 NB rreq to expect a SB rsp output:", $time, name);
            // $display("exp_o_sb_rsp_id = (%0h)", dut_if.i_nb_rd_req[1].id);
            // $display("exp_o_sb_rsp_port_row = (%0h)", dut_if.i_nb_rd_req[1].port_row);
            // $display("exp_o_sb_rsp_port_side = (%0h)", dut_if.i_nb_rd_req[1].port_side);

                exp_o_p1_sb_rsp_from_i_nb_rreq.vld		= dut_if.i_nb_rd_req[1].vld;
                exp_o_p1_sb_rsp_from_i_nb_rreq.id		= dut_if.i_nb_rd_req[1].id;
                exp_o_p1_sb_rsp_from_i_nb_rreq.port_side	= dut_if.i_nb_rd_req[1].port_side;
                exp_o_p1_sb_rsp_from_i_nb_rreq.port_row	= dut_if.i_nb_rd_req[1].port_row;
               sb.exp_o_p1_sb_rsp_notification(exp_o_p1_sb_rsp_from_i_nb_rreq);

	        i_p1_nb_rreq_to_exp_o_sb_rdata = 1;
	   end
	   else i_p1_nb_rreq_to_exp_o_sb_rdata = 0;


        // for SB rreq, to EB/WB/NB/SB rsp output

        // SB to EB:
           if (dut_if.i_sb_rd_req[1].vld && (dut_if.i_sb_rd_req[1].node_row  >= dut_if.i_sb_node_row)
                                         && (dut_if.i_sb_rd_req[1].port_row  == dut_if.i_sb_node_row)
                                         && (dut_if.i_sb_rd_req[1].port_side == mby_msh_pkg::msh_east_side))
	   begin
               $display("(time: %0d) %s: --------", $time, name);
               $display("(time: %0d) %s: A P1 SB rreq to expect an EB rsp output", $time, name);

                exp_o_p1_eb_rsp_from_i_sb_rreq.vld		= dut_if.i_sb_rd_req[1].vld;
                exp_o_p1_eb_rsp_from_i_sb_rreq.id		= dut_if.i_sb_rd_req[1].id;
               sb.exp_o_p1_eb_rsp_notification(exp_o_p1_eb_rsp_from_i_sb_rreq);
	
		i_p1_sb_rreq_to_exp_o_eb_rdata = 1;
	   end
	   else i_p1_sb_rreq_to_exp_o_eb_rdata = 0;


        // SB to WB
           if (dut_if.i_sb_rd_req[1].vld && (dut_if.i_sb_rd_req[1].node_row  >= dut_if.i_sb_node_row)
                                         && (dut_if.i_sb_rd_req[1].port_row  == dut_if.i_sb_node_row)
                                         && (dut_if.i_sb_rd_req[1].port_side == mby_msh_pkg::msh_west_side))
	   begin
               $display("(time: %0d) %s: --------", $time, name);
               $display("(time: %0d) %s: A P1 SB rreq to expect an WB rsp output", $time, name);

                exp_o_p1_wb_rsp_from_i_sb_rreq.vld		= dut_if.i_sb_rd_req[1].vld;
                exp_o_p1_wb_rsp_from_i_sb_rreq.id		= dut_if.i_sb_rd_req[1].id;
               sb.exp_o_p1_wb_rsp_notification(exp_o_p1_wb_rsp_from_i_sb_rreq);
	
	 	i_p1_sb_rreq_to_exp_o_wb_rdata = 1;
	   end
	   else i_p1_sb_rreq_to_exp_o_wb_rdata = 0;


        // SB to NB
           if ( (dut_if.i_sb_rd_req[1].vld && (dut_if.i_sb_rd_req[1].node_row  >= dut_if.i_sb_node_row)
                                           && (dut_if.i_sb_rd_req[1].port_row  == dut_if.i_sb_node_row)
                                           && (dut_if.i_sb_rd_req[1].port_side == mby_msh_pkg::msh_north_side))
              ||
                (dut_if.i_sb_rd_req[1].vld && (dut_if.i_sb_rd_req[1].node_row  >= dut_if.i_sb_node_row)
                                           && (dut_if.i_sb_rd_req[1].port_row  <  dut_if.i_sb_node_row))
              )
	   begin
               $display("(time: %0d) %s: --------", $time, name);
               $display("(time: %0d) %s: A P1 SB rreq to expect a NB rsp output:", $time, name);
            // $display("exp_o_nb_rsp_id = (%0h)", dut_if.i_sb_rd_req[1].id);
            // $display("exp_o_nb_rsp_port_row = (%0h)", dut_if.i_sb_rd_req[1].port_row);
            // $display("exp_o_nb_rsp_port_side = (%0h)", dut_if.i_sb_rd_req[1].port_side);

                exp_o_p1_nb_rsp_from_i_sb_rreq.vld		= dut_if.i_sb_rd_req[1].vld;
                exp_o_p1_nb_rsp_from_i_sb_rreq.id		= dut_if.i_sb_rd_req[1].id;
                exp_o_p1_nb_rsp_from_i_sb_rreq.port_side	= dut_if.i_sb_rd_req[1].port_side;
                exp_o_p1_nb_rsp_from_i_sb_rreq.port_row	= dut_if.i_sb_rd_req[1].port_row;
               sb.exp_o_p1_nb_rsp_notification(exp_o_p1_nb_rsp_from_i_sb_rreq);

		i_p1_sb_rreq_to_exp_o_nb_rdata = 1;
	   end
	   else i_p1_sb_rreq_to_exp_o_nb_rdata = 0;


        // SB to SB
           if ( (dut_if.i_sb_rd_req[1].vld && (dut_if.i_sb_rd_req[1].node_row  >= dut_if.i_sb_node_row)
                                           && (dut_if.i_sb_rd_req[1].port_row  == dut_if.i_sb_node_row)
                                           && (dut_if.i_sb_rd_req[1].port_side == mby_msh_pkg::msh_south_side))
              ||
                (dut_if.i_sb_rd_req[1].vld && (dut_if.i_nb_rd_req[1].node_row  >= dut_if.i_sb_node_row)
                                           && (dut_if.i_nb_rd_req[1].port_row  >  dut_if.i_sb_node_row))
              )
	   begin
               $display("(time: %0d) %s: --------", $time, name);
               $display("(time: %0d) %s: for A P1 SB rreq to expect a SB rsp output:", $time, name);
            // $display("exp_o_sb_rsp_id = (%0h)", dut_if.i_sb_rd_req[1].id);
            // $display("exp_o_sb_rsp_port_row = (%0h)", dut_if.i_sb_rd_req[1].port_row);
            // $display("exp_o_sb_rsp_port_side = (%0h)", dut_if.i_sb_rd_req[1].port_side);

                exp_o_p1_sb_rsp_from_i_sb_rreq.vld		= dut_if.i_sb_rd_req[1].vld;
                exp_o_p1_sb_rsp_from_i_sb_rreq.id		= dut_if.i_sb_rd_req[1].id;
                exp_o_p1_sb_rsp_from_i_sb_rreq.port_side	= dut_if.i_sb_rd_req[1].port_side;
                exp_o_p1_sb_rsp_from_i_sb_rreq.port_row	= dut_if.i_sb_rd_req[1].port_row;
               sb.exp_o_p1_sb_rsp_notification(exp_o_p1_sb_rsp_from_i_sb_rreq);

		i_p1_sb_rreq_to_exp_o_sb_rdata = 1;
	   end
	   else i_p1_sb_rreq_to_exp_o_sb_rdata = 0;
	



	// what is last wdata for a valid write request ? //////////////////////////////


	   if (exp_o_p0_eb_wr_valid_q2 		||	// eb_req.col > dut.col
	       exp_o_p0_nb_wr_valid_from_eb_q2 	||	// eb_req.col = dut.col && eb_req.row < dut.row
	       exp_o_p0_sb_wr_valid_from_eb_q2 	||	// eb_req.col = dut.col && eb_req.row > dut.row
	       exp_p0_eb_wr_to_mem_valid_q2 	) 	// eb_req.col = dut.col && eb_req.row = dut.row
		last_i_p0_eb_wdata = dut_if.i_eb_wr_data[0];

	   if (exp_o_p0_wb_wr_valid_q2 		||	// wb_req.col > dut.col
	       exp_o_p0_nb_wr_valid_from_wb_q2 	||	// wb_req.col = dut.col && wb_req.row < dut.row
	       exp_o_p0_sb_wr_valid_from_wb_q2 	||	// wb_req.col = dut.col && wb_req.row > dut.row
	       exp_p0_wb_wr_to_mem_valid_q2 	) 	// wb_req.col = dut.col && wb_req.row = dut.row
		last_i_p0_wb_wdata = dut_if.i_wb_wr_data[0];

	   if (exp_o_p0_nb_wr_valid_q2 		|| 	// nb_req.row < dut.row
	       exp_p0_nb_wr_to_mem_valid_q2	)	// nb_req.row = dut.row
		last_i_p0_nb_wdata = dut_if.i_nb_wr_data[0];

	   if (exp_o_p0_sb_wr_valid_q2 		|| 	// sb_req.row > dut.row
	       exp_p0_sb_wr_to_mem_valid_q2	)	// sb_req.row = dut.row
		last_i_p0_sb_wdata = dut_if.i_sb_wr_data[0];


	   if (exp_o_p1_eb_wr_valid_q2 		||	// eb_req.col > dut.col
	       exp_o_p1_nb_wr_valid_from_eb_q2 	||	// eb_req.col = dut.col && eb_req.row < dut.row
	       exp_o_p1_sb_wr_valid_from_eb_q2 	||	// eb_req.col = dut.col && eb_req.row > dut.row
	       exp_p1_eb_wr_to_mem_valid_q2 	) 	// eb_req.col = dut.col && eb_req.row = dut.row
		last_i_p1_eb_wdata = dut_if.i_eb_wr_data[1];

	   if (exp_o_p1_wb_wr_valid_q2 		||	// wb_req.col > dut.col
	       exp_o_p1_nb_wr_valid_from_wb_q2 	||	// wb_req.col = dut.col && wb_req.row < dut.row
	       exp_o_p1_sb_wr_valid_from_wb_q2 	||	// wb_req.col = dut.col && wb_req.row > dut.row
	       exp_p1_wb_wr_to_mem_valid_q2 	) 	// wb_req.col = dut.col && wb_req.row = dut.row
		last_i_p1_wb_wdata = dut_if.i_wb_wr_data[1];

	   if (exp_o_p1_nb_wr_valid_q2 		|| 	// nb_req.row < dut.row
	       exp_p1_nb_wr_to_mem_valid_q2	)	// nb_req.row = dut.row
		last_i_p1_nb_wdata = dut_if.i_nb_wr_data[1];

	   if (exp_o_p1_sb_wr_valid_q2 		|| 	// sb_req.row > dut.row
	       exp_p1_sb_wr_to_mem_valid_q2	)	// sb_req.row = dut.row
		last_i_p1_sb_wdata = dut_if.i_sb_wr_data[1];




	// push expected rd_data: /////////////////////////////////////////////////////


           if (i_p0_wb_rreq_to_exp_o_eb_rdata) 
		sb.exp_o_p0_eb_rdata_notification(last_i_p0_wb_wdata);

           if (i_p0_eb_rreq_to_exp_o_wb_rdata) begin
		sb.exp_o_p0_wb_rdata_notification(last_i_p0_eb_wdata);
	   end


 	   if (i_p0_nb_rreq_to_exp_o_eb_rdata)
               sb.exp_o_p0_eb_rdata_notification(last_i_p0_nb_wdata);

 	   if (i_p0_nb_rreq_to_exp_o_wb_rdata) begin
               sb.exp_o_p0_wb_rdata_notification(last_i_p0_nb_wdata);
	   end

 	   if (i_p0_nb_rreq_to_exp_o_nb_rdata)
               sb.exp_o_p0_nb_rdata_notification(last_i_p0_nb_wdata);

 	   if (i_p0_nb_rreq_to_exp_o_sb_rdata)
               sb.exp_o_p0_sb_rdata_notification(last_i_p0_nb_wdata);


 	   if (i_p0_sb_rreq_to_exp_o_eb_rdata)
               sb.exp_o_p0_eb_rdata_notification(last_i_p0_sb_wdata);

 	   if (i_p0_sb_rreq_to_exp_o_wb_rdata) begin
               sb.exp_o_p0_wb_rdata_notification(last_i_p0_sb_wdata);
	   end

 	   if (i_p0_sb_rreq_to_exp_o_nb_rdata)
               sb.exp_o_p0_nb_rdata_notification(last_i_p0_sb_wdata);

 	   if (i_p0_sb_rreq_to_exp_o_sb_rdata)
               sb.exp_o_p0_sb_rdata_notification(last_i_p0_sb_wdata);



           if (i_p1_wb_rreq_to_exp_o_eb_rdata)
                sb.exp_o_p1_eb_rdata_notification(last_i_p1_wb_wdata);

           if (i_p1_eb_rreq_to_exp_o_wb_rdata)
                sb.exp_o_p1_wb_rdata_notification(last_i_p1_eb_wdata);


           if (i_p1_nb_rreq_to_exp_o_eb_rdata)
               sb.exp_o_p1_eb_rdata_notification(last_i_p1_nb_wdata);

           if (i_p1_nb_rreq_to_exp_o_wb_rdata)
               sb.exp_o_p1_wb_rdata_notification(last_i_p1_nb_wdata);

           if (i_p1_nb_rreq_to_exp_o_nb_rdata)
               sb.exp_o_p1_nb_rdata_notification(last_i_p1_nb_wdata);

           if (i_p1_nb_rreq_to_exp_o_sb_rdata)
               sb.exp_o_p1_sb_rdata_notification(last_i_p1_nb_wdata);


           if (i_p1_sb_rreq_to_exp_o_eb_rdata)
               sb.exp_o_p1_eb_rdata_notification(last_i_p1_sb_wdata);

           if (i_p1_sb_rreq_to_exp_o_wb_rdata)
               sb.exp_o_p1_wb_rdata_notification(last_i_p1_sb_wdata);

           if (i_p1_sb_rreq_to_exp_o_nb_rdata)
               sb.exp_o_p1_nb_rdata_notification(last_i_p1_sb_wdata);

           if (i_p1_sb_rreq_to_exp_o_sb_rdata)
               sb.exp_o_p1_sb_rdata_notification(last_i_p1_sb_wdata);




	// check rsp output against expected rsp //////////////////////////////////////


           o_p0_eb_rsp_valid_q2 = o_p0_eb_rsp_valid_q1;
           o_p0_eb_rsp_valid_q1 = o_p0_eb_rsp_valid;
           o_p0_eb_rsp_valid    = dut_if.o_eb_rd_rsp[0].vld;

           if (o_p0_eb_rsp_valid) begin
               $display("(time: %0d) %s: --------", $time, name);
               $display("(time: %0d) %s: A P0 EB RD rsp OUTPUT is detected", $time, name);
               sb.check_p0_eb_rsp_notification(dut_if.o_eb_rd_rsp[0]);
	   end

           if (o_p0_eb_rsp_valid_q2) begin
               $display("(time: %0d) %s: --------", $time, name);
               $display("(time: %0d) %s: A P0 EB RD data OUTPUT is detected", $time, name);
               sb.check_p0_eb_rdata_notification(dut_if.o_eb_rd_data[0]);
	   end


           o_p0_wb_rsp_valid_q2 = o_p0_wb_rsp_valid_q1;
           o_p0_wb_rsp_valid_q1 = o_p0_wb_rsp_valid;
           o_p0_wb_rsp_valid    = dut_if.o_wb_rd_rsp[0].vld;

           if (o_p0_wb_rsp_valid) begin
               $display("(time: %0d) %s: --------", $time, name);
               $display("(time: %0d) %s: A P0 WB RD rsp OUTPUT is detected", $time, name);
               sb.check_p0_wb_rsp_notification(dut_if.o_wb_rd_rsp[0]);
	   end

           if (o_p0_wb_rsp_valid_q2) begin
               $display("(time: %0d) %s: --------", $time, name);
               $display("(time: %0d) %s: A P0 WB RD data OUTPUT is detected", $time, name);
               sb.check_p0_wb_rdata_notification(dut_if.o_wb_rd_data[0]);
	   end


           o_p0_nb_rsp_valid_q2 = o_p0_nb_rsp_valid_q1;
           o_p0_nb_rsp_valid_q1 = o_p0_nb_rsp_valid;
           o_p0_nb_rsp_valid    = dut_if.o_nb_rd_rsp[0].vld;

           if (o_p0_nb_rsp_valid) begin
               $display("(time: %0d) %s: --------", $time, name);
               $display("(time: %0d) %s: A P0 NB RD rsp OUTPUT is detected", $time, name);
               sb.check_p0_nb_rsp_notification(dut_if.o_nb_rd_rsp[0]);
	   end


           if (o_p0_nb_rsp_valid_q2) begin
               $display("(time: %0d) %s: --------", $time, name);
               $display("(time: %0d) %s: A P0 NB RD data OUTPUT is detected", $time, name);
               sb.check_p0_nb_rdata_notification(dut_if.o_nb_rd_data[0]);
	   end


           o_p0_sb_rsp_valid_q2 = o_p0_sb_rsp_valid_q1;
           o_p0_sb_rsp_valid_q1 = o_p0_sb_rsp_valid;
           o_p0_sb_rsp_valid    = dut_if.o_sb_rd_rsp[0].vld;

           if (o_p0_sb_rsp_valid) begin
               $display("(time: %0d) %s: --------", $time, name);
               $display("(time: %0d) %s: A P0 SB RD rsp OUTPUT is detected", $time, name);
               sb.check_p0_sb_rsp_notification(dut_if.o_sb_rd_rsp[0]);
	   end

           if (o_p0_sb_rsp_valid_q2) begin
               $display("(time: %0d) %s: --------", $time, name);
               $display("(time: %0d) %s: A P0 SB RD data OUTPUT is detected", $time, name);
               sb.check_p0_sb_rdata_notification(dut_if.o_sb_rd_data[0]);
	   end



           o_p1_eb_rsp_valid_q2 = o_p1_eb_rsp_valid_q1;
           o_p1_eb_rsp_valid_q1 = o_p1_eb_rsp_valid;
           o_p1_eb_rsp_valid    = dut_if.o_eb_rd_rsp[1].vld;

           if (o_p1_eb_rsp_valid) begin
               $display("(time: %0d) %s: --------", $time, name);
               $display("(time: %0d) %s: A P1 EB RD rsp OUTPUT is detected", $time, name);
               sb.check_p1_eb_rsp_notification(dut_if.o_eb_rd_rsp[1]);
	   end

           if (o_p1_eb_rsp_valid_q2) begin
               $display("(time: %0d) %s: --------", $time, name);
               $display("(time: %0d) %s: A P1 EB RD data OUTPUT is detected", $time, name);
               sb.check_p1_eb_rdata_notification(dut_if.o_eb_rd_data[1]);
	   end


           o_p1_wb_rsp_valid_q2 = o_p1_wb_rsp_valid_q1;
           o_p1_wb_rsp_valid_q1 = o_p1_wb_rsp_valid;
           o_p1_wb_rsp_valid    = dut_if.o_wb_rd_rsp[1].vld;

           if (o_p1_wb_rsp_valid) begin
               $display("(time: %0d) %s: --------", $time, name);
               $display("(time: %0d) %s: A P1 WB RD rsp OUTPUT is detected", $time, name);
               sb.check_p1_wb_rsp_notification(dut_if.o_wb_rd_rsp[1]);
	   end

           if (o_p1_wb_rsp_valid_q2) begin
               $display("(time: %0d) %s: --------", $time, name);
               $display("(time: %0d) %s: A P1 WB RD data OUTPUT is detected", $time, name);
               sb.check_p1_wb_rdata_notification(dut_if.o_wb_rd_data[1]);
	   end


           o_p1_nb_rsp_valid_q2 = o_p1_nb_rsp_valid_q1;
           o_p1_nb_rsp_valid_q1 = o_p1_nb_rsp_valid;
           o_p1_nb_rsp_valid    = dut_if.o_nb_rd_rsp[1].vld;

           if (o_p1_nb_rsp_valid) begin
               $display("(time: %0d) %s: --------", $time, name);
               $display("(time: %0d) %s: A P1 NB RD rsp OUTPUT is detected", $time, name);
               sb.check_p1_nb_rsp_notification(dut_if.o_nb_rd_rsp[1]);
	   end

           if (o_p1_nb_rsp_valid_q2) begin
               $display("(time: %0d) %s: --------", $time, name);
               $display("(time: %0d) %s: A P1 NB RD data OUTPUT is detected", $time, name);
               sb.check_p1_nb_rdata_notification(dut_if.o_nb_rd_data[1]);
	   end


           o_p1_sb_rsp_valid_q2 = o_p1_sb_rsp_valid_q1;
           o_p1_sb_rsp_valid_q1 = o_p1_sb_rsp_valid;
           o_p1_sb_rsp_valid    = dut_if.o_sb_rd_rsp[1].vld;

           if (o_p1_sb_rsp_valid) begin
               $display("(time: %0d) %s: --------", $time, name);
               $display("(time: %0d) %s: A P1 SB RD rsp OUTPUT is detected", $time, name);
               sb.check_p1_sb_rsp_notification(dut_if.o_sb_rd_rsp[1]);
	   end

           if (o_p1_sb_rsp_valid_q2) begin
               $display("(time: %0d) %s: --------", $time, name);
               $display("(time: %0d) %s: A P1 SB RD data OUTPUT is detected", $time, name);
               sb.check_p1_sb_rdata_notification(dut_if.o_sb_rd_data[1]);
	   end



// ----------------------------------------------------------------------------








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


/*
    task final_state_check();
        begin
            @(negedge dut_if.mclk) // sample on negedge

        end
    endtask
*/

    task print_stats(string prepend);
        $display("%s %s: stat_num_arb_conflicts = %0d", prepend, name, stat_num_arb_conflicts);
    endtask

endclass

`endif // MONITOR_SV
