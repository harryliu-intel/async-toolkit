
//config tlm;
//  design tlm_tb_lib.tlm_tb;
//endconfig

// this configuration should be regarded as a list of overrides for emulation on top of 'config soc_rtl;'
config tlm_emu;
  //moved to DUT.cfg: design tlm_rtl_lib.tlm_top;
  default liblist tlm_rtl_lib sip_shared_lib;
endconfig
