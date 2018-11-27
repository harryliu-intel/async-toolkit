// -----------------------------------------------------------------------------
// Copyright(C) 2011 Intel Corporation, Confidential Information
// -----------------------------------------------------------------------------
//
// Filename:    fc_bug_workaround_forces.sv
// Revision:    0
// Revised By:  mmdhanif
// Revised On:  Date: 8/12/2016
// Created By:  mmdhanif
// Created On:  8/12/2016
// Description: This file contains the bug workaround forces 
//              Forces in this file must have a bug/bugtool id associated with it
//              Once the bug is closed/released, the forces must be removed 
//------------------------------------------------------------------------------

// -------------------------------------------
// Owner: mmdhanif
// Date : 8/12/2016
// HSD  : bXXXXXX
// Info : Dummy force - template/reference for force enabling
// -------------------------------------------
`INITIAL begin
    if (apply_forces.patch_bXXXXXX) begin
        $display("Apply forces for bXXXXXX Simulation Workaround");
    end      
end      

// --------------------------------------------------------
// Owner: ssnanal
// Date : 11/12/2018
// HSD  : bXXXXXX
// Info : Placeholder for IMC driven clocks and resets 
// --------------------------------------------------------
`INITIAL begin
    if (apply_forces.imc_hack) begin
        $display("Apply forces for IMC Workaround");

        //Clocks
        force `mby_ec_top_0.cclk = `HVL_TOP.fc_sig_if.tmp_cclk;
        force `mby_ec_top_0.clk  = `HVL_TOP.fc_sig_if.tmp_clk;

        force `mby_ec_top_1.cclk = `HVL_TOP.fc_sig_if.tmp_cclk;
        force `mby_ec_top_1.clk  = `HVL_TOP.fc_sig_if.tmp_clk;

        `ifdef FC_64
            force `mby_ec_top_2.cclk = `HVL_TOP.fc_sig_if.tmp_cclk;
            force `mby_ec_top_2.clk  = `HVL_TOP.fc_sig_if.tmp_clk;
            force `mby_ec_top_3.cclk = `HVL_TOP.fc_sig_if.tmp_cclk;
            force `mby_ec_top_3.clk  = `HVL_TOP.fc_sig_if.tmp_clk;
            force `mby_ec_top_4.cclk = `HVL_TOP.fc_sig_if.tmp_cclk;
            force `mby_ec_top_4.clk  = `HVL_TOP.fc_sig_if.tmp_clk;
            force `mby_ec_top_5.cclk = `HVL_TOP.fc_sig_if.tmp_cclk;
            force `mby_ec_top_5.clk  = `HVL_TOP.fc_sig_if.tmp_clk;
            force `mby_ec_top_6.cclk = `HVL_TOP.fc_sig_if.tmp_cclk;
            force `mby_ec_top_6.clk  = `HVL_TOP.fc_sig_if.tmp_clk;
            force `mby_ec_top_7.cclk = `HVL_TOP.fc_sig_if.tmp_cclk;
            force `mby_ec_top_7.clk  = `HVL_TOP.fc_sig_if.tmp_clk;
        `endif

        force `mby_mpp_0.cclk  = `HVL_TOP.fc_sig_if.tmp_cclk;

        force `mby_mpp_1.cclk  = `HVL_TOP.fc_sig_if.tmp_cclk;

        `ifdef FC_64
            force `mby_mpp_2.cclk  = `HVL_TOP.fc_sig_if.tmp_cclk;
            force `mby_mpp_3.cclk  = `HVL_TOP.fc_sig_if.tmp_cclk;
            force `mby_mpp_4.cclk  = `HVL_TOP.fc_sig_if.tmp_cclk;
            force `mby_mpp_5.cclk  = `HVL_TOP.fc_sig_if.tmp_cclk;
            force `mby_mpp_6.cclk  = `HVL_TOP.fc_sig_if.tmp_cclk;
            force `mby_mpp_7.cclk  = `HVL_TOP.fc_sig_if.tmp_cclk;
        `endif

        force `mby_gmn_0.cclk  = `HVL_TOP.fc_sig_if.tmp_cclk;
        force `mby_gms_0.cclk  = `HVL_TOP.fc_sig_if.tmp_cclk;
        force `mby_msh_0.mclk  = `HVL_TOP.fc_sig_if.tmp_mclk;

        //Reset
        #100ns; //TODO

        force `mby_ec_top_0.powergood_rst_n  = 1;
        force `mby_ec_top_0.rst_n  = 1;
        force `mby_ec_top_0.sreset_n  = 1;
        force `mby_ec_top_0.hreset_n  = 1;
        force `mby_ec_top_0.reset  = 8'h0; //TODO

        force `mby_ec_top_1.powergood_rst_n  = 1;
        force `mby_ec_top_1.rst_n  = 1;
        force `mby_ec_top_1.sreset_n  = 1;
        force `mby_ec_top_1.hreset_n  = 1;
        force `mby_ec_top_1.reset  = 8'h0; //TODO

        `ifdef FC_64
            force `mby_ec_top_2.powergood_rst_n  = 1;
            force `mby_ec_top_2.rst_n  = 1;
            force `mby_ec_top_2.sreset_n  = 1;
            force `mby_ec_top_2.hreset_n  = 1;
            force `mby_ec_top_2.reset  = 8'h0; //TODO

            force `mby_ec_top_3.powergood_rst_n  = 1;
            force `mby_ec_top_3.rst_n  = 1;
            force `mby_ec_top_3.sreset_n  = 1;
            force `mby_ec_top_3.hreset_n  = 1;
            force `mby_ec_top_3.reset  = 8'h0; //TODO

            force `mby_ec_top_4.powergood_rst_n  = 1;
            force `mby_ec_top_4.rst_n  = 1;
            force `mby_ec_top_4.sreset_n  = 1;
            force `mby_ec_top_4.hreset_n  = 1;
            force `mby_ec_top_4.reset  = 8'h0; //TODO

            force `mby_ec_top_5.powergood_rst_n  = 1;
            force `mby_ec_top_5.rst_n  = 1;
            force `mby_ec_top_5.sreset_n  = 1;
            force `mby_ec_top_5.hreset_n  = 1;
            force `mby_ec_top_5.reset  = 8'h0; //TODO

            force `mby_ec_top_6.powergood_rst_n  = 1;
            force `mby_ec_top_6.rst_n  = 1;
            force `mby_ec_top_6.sreset_n  = 1;
            force `mby_ec_top_6.hreset_n  = 1;
            force `mby_ec_top_6.reset  = 8'h0; //TODO

            force `mby_ec_top_7.powergood_rst_n  = 1;
            force `mby_ec_top_7.rst_n  = 1;
            force `mby_ec_top_7.sreset_n  = 1;
            force `mby_ec_top_7.hreset_n  = 1;
            force `mby_ec_top_7.reset  = 8'h0; //TODO
        `endif
       
        force `mby_mpp_0.sreset  = 0; //TODO
        force `mby_mpp_0.arst_n  = 1;

        force `mby_mpp_1.sreset  = 0; //TODO
        force `mby_mpp_1.arst_n  = 1;

        `ifdef FC_64
            force `mby_mpp_2.sreset  = 0; //TODO
            force `mby_mpp_2.arst_n  = 1;

            force `mby_mpp_3.sreset  = 0; //TODO
            force `mby_mpp_3.arst_n  = 1;

            force `mby_mpp_4.sreset  = 0; //TODO
            force `mby_mpp_4.arst_n  = 1;

            force `mby_mpp_5.sreset  = 0; //TODO
            force `mby_mpp_5.arst_n  = 1;

            force `mby_mpp_6.sreset  = 0; //TODO
            force `mby_mpp_6.arst_n  = 1;

            force `mby_mpp_7.sreset  = 0; //TODO
            force `mby_mpp_7.arst_n  = 1;
        `endif

        force `mby_gmn_0.reset_n  = 1;
        force `mby_gms_0.reset_n  = 1;
// JMCCORMI:  reset signal names changed in mesh.  There are now 4 active high
// reset signals:  
//      chreset - core hard reset
//      csreset - core soft reset
//      mhreset - mesh hard reset
//      msreset - mesh soft reset
// From the looks of this file, we are all over the place right now with
// reset.  Hopefully someone will come up with a plan for reset that makes
// this all more consistent.  
//
//        force `mby_msh_0.i_reset  = 1;   
    end
end      

