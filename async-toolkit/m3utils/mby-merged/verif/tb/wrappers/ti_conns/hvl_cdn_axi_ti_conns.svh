// -----------------------------------------------------------------------------
// INTEL CONFIDENTIAL
// Copyright(c) 2018, Intel Corporation. All Rights Reserved
// -----------------------------------------------------------------------------
//
// Created By   :  Raghu P Gudla
// Created On   :  09/22/2018
// Description  :  CDN AXI test island instantiation and connectivity
//------------------------------------------------------------------------------

`ifdef CDN_AXI_ENV_ENABLE

  reg aclk;
  reg aresetn;
    
   cdnAxi4Interface#(.DATA_WIDTH(128)) userDutInterface (aclk,aresetn);

  //Toggling the clock
  always #50 aclk = ~aclk;

  //Controlling the reset
  initial
  begin
    aclk = 1'b1;
    aresetn = 1'b1;
    #100;
    aresetn = 1'b0;   
    #5000;
    aresetn = 1'b1;
  end
  
    //setting the virtual interface to the sve and starting uvm.
  initial
  begin
`ifndef CDN_AXI_USING_CLOCKING_BLOCK
    uvm_config_db#(virtual interface cdnAxi4Interface#(.DATA_WIDTH(128)))::set(null,"*", "vif", userDutInterface);
`else        
    uvm_config_db#(virtual interface cdnAxi4ActiveMasterInterface#(.DATA_WIDTH(128)))::set(null,"*active_master", "vif", userDutInterface.activeMaster);
    uvm_config_db#(virtual interface cdnAxi4ActiveSlaveInterface#(.DATA_WIDTH(128)))::set(null,"*active_slave", "vif", userDutInterface.activeSlave);
    uvm_config_db#(virtual interface cdnAxi4PassiveInterface#(.DATA_WIDTH(128)))::set(null,"*passiveSlave", "vif", userDutInterface.passiveSlave);
    uvm_config_db#(virtual interface cdnAxi4PassiveInterface#(.DATA_WIDTH(128)))::set(null,"*passiveMaster", "vif", userDutInterface.passiveMaster);
`endif
  end

`endif

// <<< VIM SETTINGS
// vim: ts=4 et
// >>>

