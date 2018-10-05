// -----------------------------------------------------------------------------
// INTEL CONFIDENTIAL
// Copyright(c) 2018, Intel Corporation. All Rights Reserved
// -----------------------------------------------------------------------------
//
// Created By   :  Raghu P Gudla
// Created On   :  09/22/2018
// Description  :  CHI test island instantiation and connectivity
//------------------------------------------------------------------------------

`ifdef CHI_ENV_ENABLE

    ChiResetIf    chi_reset_vif();

    parameter               simulation_cycle = 20000;
    bit SystemClock;

    `ifdef SVT_CHI_ENABLE_MULTI_CLOCK
      bit                     rn_clk[`SVT_CHI_MAX_NUM_RNS-1:0];
      bit                     sn_clk[`SVT_CHI_MAX_NUM_SNS-1:0];
    `endif  
    `ifdef SVT_CHI_ENABLE_MULTI_RESET
      logic                     rn_resetn[`SVT_CHI_MAX_NUM_RNS-1:0];
      logic                     sn_resetn[`SVT_CHI_MAX_NUM_SNS-1:0];
    `endif

    bit SystemReset;

    int rn_txrsp_lcrdv_counter = 0;

    bit is_reset_done = 0;

    /** VIP Interface instance representing the CHI system */
   svt_chi_if chi_vif (
  `ifndef SVT_CHI_ENABLE_MULTI_CLOCK
                        SystemClock,
  `else
                        rn_clk,
                        sn_clk,
  `endif
  `ifndef SVT_CHI_ENABLE_MULTI_RESET
                        SystemReset
  `else
                        rn_resetn,
                        sn_resetn
  `endif
                        );

    //Typedef of the reset modport to simplify access
    typedef virtual ChiResetIf.chi_reset_modport CHI_RESET_MP;

    CHI_RESET_MP chi_reset_mp;

    // -----------------------------------------------------------------------------
    // Test Island
    // -----------------------------------------------------------------------------
    //axi_ti  #() axi0_ti ();

    clkgen_if #(.PERIOD(20000), .DELAY(0), .DUTYCYCLE(50)) chi_tb_clk(); // 50MHz AXI clock

    assign chi_reset_vif.clk = chi_tb_clk.clk;
    assign SystemReset = chi_reset_vif.rstn;

    `ifdef SVT_AMBA_INTERFACE_METHOD_DISABLE
    `ifndef SVT_CHI_ENABLE_MULTI_CLOCK
    assign chi_vif.common_aclk = chi_tb_clk.clk;
    `endif
    `endif


    /**
    * Assign the reset pin from the reset interface to the reset pins from the VIP interface.
    */
    `ifdef SVT_CHI_ENABLE_MULTI_RESET
    assign rn_resetn = chi_reset_vif.rn_resetn;
    assign sn_resetn = chi_reset_vif.sn_resetn;
    `endif

    initial begin
        #(simulation_cycle/2); // No clock edge at T=0
        SystemClock = 0;

        forever begin
        `ifdef SVT_CHI_ENABLE_MULTI_CLOCK         
          // These separate clocks are generated with a #1
          // skew for every alternate index. This is just
          // for demonstration purpose.
          foreach (rn_clk[i]) begin
            fork
              automatic int _rn_idx = i;
              #(_rn_idx%2) rn_clk[_rn_idx] = chi_tb_clk.clk;//SystemClock;
            join_none
          end
      
          foreach (sn_clk[i]) begin
            fork
              automatic int _sn_idx = i;
              #(_sn_idx%2) sn_clk[_sn_idx] = chi_tb_clk.clk;//SystemClock;
            join_none
          end
         `endif
         #(simulation_cycle/2) SystemClock = ~SystemClock;
        end
    end

      
   //   TB logic to drive FLIT signals
  initial
    begin
    `ifndef SVT_CHI_ENABLE_MULTI_RESET      
      // Wait for reset to come then go away
      wait (chi_reset_vif.rstn === 0);
      @ (posedge chi_reset_vif.rstn);
      // Wait for link to be active
      repeat (5) @ (posedge chi_tb_clk.clk);
    `else
      fork
        begin
          // Wait for RN reset to come then go away
          wait (rn_resetn[0] === 0);
          @ (posedge chi_reset_vif.rstn);
          // Wait for link to be active
          repeat (5) @ (posedge chi_tb_clk.clk);
        end
        begin
          // Wait for SN reset to come then go away
          wait (sn_resetn[0] === 0);
          @ (posedge chi_reset_vif.rstn);
          // Wait for link to be active
          repeat (5) @ (posedge chi_tb_clk.clk);
        end        
      join
      
`endif // !`ifndef SVT_CHI_ENABLE_MULTI_RESET
      is_reset_done = 1;      
  end

  // TXRSPLCRDV is driven by the interconnect. Since we do not have a real
  // interconnect, it is driven from the testbench.  Check if the number of
  // outstanding credits is less than 4, if so, send a credit. The number of
  // outstanding credits is decremented each time we get a flit on TXRSPFLITV
  always @ (chi_vif.rn_if[0].rn_cb)
    begin
      if ((is_reset_done == 1) && 
          (chi_vif.rn_if[0].TXLINKACTIVEREQ === 1'b1) && 
          (chi_vif.rn_if[0].TXLINKACTIVEACK === 1'b1))
        begin
          if (chi_vif.rn_if[0].TXRSPFLITV === 1'b1)
            rn_txrsp_lcrdv_counter--;
          
          if (rn_txrsp_lcrdv_counter < 4)
            begin
              `HDL_TOP.chi_svt_dut.TXRSPLCRDV_RN1 <= 1'b1;
              rn_txrsp_lcrdv_counter++;
            end
          else 
            `HDL_TOP.chi_svt_dut.TXRSPLCRDV_RN1 <= 1'b0;
        end
    end


    initial begin
        chi_reset_mp = chi_reset_vif.chi_reset_modport;

        uvm_config_db#(virtual svt_chi_if)::set(uvm_root::get(), "*", "vif", chi_vif);
        uvm_config_db#(virtual svt_chi_if)::set(uvm_root::get(), "*", "chi_vif", chi_vif);
        uvm_config_db#(virtual ChiResetIf)::set(uvm_root::get(), "*", "chi_reset_vif", chi_reset_vif);
        uvm_config_db#(virtual ChiResetIf.chi_reset_modport)::set(uvm_root::get(), "*", "chi_reset_mp", chi_reset_mp);

        chi_reset_vif.rstn = 1'b0;
        `ifdef SVT_CHI_ENABLE_MULTI_RESET
        // For demo purpose, RN and SN reset signals are generated with #1 skew
        foreach (chi_reset_vif.rn_resetn[i]) chi_reset_vif.rn_resetn[i] = 1'b0;
        #1;
        foreach (chi_reset_vif.sn_resetn[i]) chi_reset_vif.sn_resetn[i] = 1'b0;
        `endif    
        repeat(10) @(posedge chi_reset_vif.clk);
        chi_reset_vif.rstn = 1'b1;
        `ifdef SVT_CHI_ENABLE_MULTI_RESET
        // For demo purpose, RN and SN reset signals are generated with #1 skew
        foreach (chi_reset_vif.rn_resetn[i]) chi_reset_vif.rn_resetn[i] = 1'b1;
        #1;
        foreach (chi_reset_vif.sn_resetn[i]) chi_reset_vif.sn_resetn[i] = 1'b1;
        `endif

    end

`endif

// <<< VIM SETTINGS
// vim: ts=4 et
// >>>

