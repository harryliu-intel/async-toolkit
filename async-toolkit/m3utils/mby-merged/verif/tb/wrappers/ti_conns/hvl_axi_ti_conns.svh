// -----------------------------------------------------------------------------
// INTEL CONFIDENTIAL
// Copyright(c) 2018, Intel Corporation. All Rights Reserved
// -----------------------------------------------------------------------------
//
// Created By   :  Raghu P Gudla
// Created On   :  09/22/2018
// Description  :  AXI test island instantiation and connectivity
//------------------------------------------------------------------------------

`ifdef AXI_ENV_ENABLE

    svt_axi_if    axi_vif();
    AxiResetIf  axi_reset_vif();

    //Typedef of the reset modport to simplify access
    typedef virtual AxiResetIf.axi_reset_modport AXI_RESET_MP;

    AXI_RESET_MP axi_reset_mp;


    // -----------------------------------------------------------------------------
    // Test Island
    // -----------------------------------------------------------------------------
    //axi_ti  #() axi0_ti ();

    clkgen_if #(.PERIOD(20000), .DELAY(0), .DUTYCYCLE(50)) axi_tb_clk(); // 50MHz AXI clock

    assign axi_vif.common_aclk = axi_tb_clk.clk;
    assign axi_reset_vif.clk = axi_tb_clk.clk;

    /**
    * Assign the reset pin from the reset interface to the reset pins from the VIP interface.
    */
    assign axi_vif.master_if[0].aresetn = axi_reset_vif.reset;
    assign axi_vif.slave_if[0].aresetn  = axi_reset_vif.reset;

    initial begin
        axi_reset_mp = axi_reset_vif.axi_reset_modport;

        //uvm_config_db#(virtual svt_axi_if)::set(null, $sformatf("%s.axi_bfm","uvm_test_top.tb_env"), "axi_vif", axi_vif);
        uvm_config_db#(virtual svt_axi_if)::set(uvm_root::get(), "*", "axi_vif", axi_vif);
        uvm_config_db#(virtual AxiResetIf)::set(uvm_root::get(), "*", "axi_reset_vif", axi_reset_vif);
        //uvm_config_db#(virtual axi_reset_vif.axi_reset_modport)::set(uvm_root::get(), $sformatf("%s.axi_bfm*",TB_ENV_PATH), "reset_mp", axi_reset_vif.axi_reset_modport);
        uvm_config_db#(virtual AxiResetIf.axi_reset_modport)::set(uvm_root::get(), "*", "reset_mp", axi_reset_vif.axi_reset_modport);

        // Resetting the AXI Reset Interface Mod port/Reset 
        axi_reset_mp.reset  = 1'b0;

        repeat(10) @(posedge axi_reset_mp.clk);
        #2;
        axi_reset_mp.reset = 1'b0;

        repeat(10) @(posedge axi_reset_mp.clk);
        axi_reset_mp.reset = 1'b1;

    end

`endif

// <<< VIM SETTINGS
// vim: ts=4 et
// >>>

