// -----------------------------------------------------------------------------
// INTEL CONFIDENTIAL
// Copyright(c) 2018, Intel Corporation. All Rights Reserved
// -----------------------------------------------------------------------------
//
// Created By   :  Raghu P Gudla
// Created On   :  09/22/2018
// Description  :  APB test island instantiation and connectivity
//------------------------------------------------------------------------------

`ifdef APB_ENV_ENABLE

    svt_apb_if    apb_master_vif();
    svt_apb_if    apb_slave_vif();

    ApbResetIf  apb_reset_vif();

    //Typedef of the reset modport to simplify access
    typedef virtual ApbResetIf.apb_reset_modport APB_RESET_MP;

    APB_RESET_MP apb_reset_mp;


    // -----------------------------------------------------------------------------
    // Test Island
    // -----------------------------------------------------------------------------
    //apb_ti  #() apb0_ti ();

    clkgen_if #(.PERIOD(20000), .DELAY(0), .DUTYCYCLE(50)) apb_tb_clk(); // 50MHz APB clock

    assign apb_master_vif.pclk = apb_tb_clk.clk;
    assign apb_slave_vif.pclk = apb_tb_clk.clk;
    assign apb_reset_vif.pclk = apb_tb_clk.clk;

    /**
    * Assign the reset pin from the reset interface to the reset pins from the VIP interface.
    */
    assign apb_master_vif.presetn = apb_reset_vif.presetn;
    assign apb_slave_vif.presetn  = apb_reset_vif.presetn;

    initial begin
        apb_reset_mp = apb_reset_vif.apb_reset_modport;

        uvm_config_db#(virtual svt_apb_if)::set(uvm_root::get(), "uvm_test_top.tb_env.apb_subenv.apb_bfm.apb_master_env", "vif", apb_master_vif);
        uvm_config_db#(virtual svt_apb_if)::set(uvm_root::get(), "uvm_test_top.tb_env.apb_subenv.apb_bfm.apb_slave_env", "vif", apb_slave_vif);
        //uvm_config_db#(virtual svt_apb_if)::set(uvm_root::get(), "*", "apb_master_vif", apb_master_vif);
        //uvm_config_db#(virtual svt_apb_if)::set(uvm_root::get(), "*", "apb_slave_vif", apb_slave_vif);
        uvm_config_db#(virtual ApbResetIf)::set(uvm_root::get(), "*", "apb_reset_vif", apb_reset_vif);
        //uvm_config_db#(virtual apb_reset_vif.apb_reset_modport)::set(uvm_root::get(), $sformatf("%s.apb_bfm*",TB_ENV_PATH), "reset_mp", apb_reset_vif.apb_reset_modport);
        uvm_config_db#(virtual ApbResetIf.apb_reset_modport)::set(uvm_root::get(), "*", "reset_mp", apb_reset_vif.apb_reset_modport);

        // Resetting the APB Reset Interface Mod port/Reset 
        apb_reset_mp.presetn  = 1'b0;

        repeat(10) @(posedge apb_reset_mp.pclk);
        #2;
        apb_reset_mp.presetn = 1'b0;

        repeat(10) @(posedge apb_reset_mp.pclk);
        apb_reset_mp.presetn = 1'b1;

    end

`endif

// <<< VIM SETTINGS
// vim: ts=4 et
// >>>

