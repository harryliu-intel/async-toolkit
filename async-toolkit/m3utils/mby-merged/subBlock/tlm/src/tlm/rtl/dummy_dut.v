module dummy_dut();
 
`ifdef UVM 
  import uvm_pkg::*;
`else
  import ovm_pkg::*;
`endif

  
  // Parameters for interfaces
`include "tlm_params.sv"
`ifdef UVM
`include "uvm_macros.svh"
`else
`include "ovm_macros.svh"
`endif
  

  
  // IOSF SideBand interface
  // ===============================================

  
  initial begin
    force    `TLM1_TOP.psf_tlm_tat = 0;
    force    `TLM1_TOP.psf_tlm_tep = 0;
    force    `TLM1_TOP.psf_tlm_ttag = 0;
    force    `TLM1_TOP.psf_tlm_trqid = 0;
    force    `TLM1_TOP.psf_tlm_tsai = 0;
    
    
  end
  

  
   initial begin
     @(posedge `TLM1_TOP_PATH.tlm_if.secondary_reset) ;
     
  end
  
  
  // Intruupt mimic
  initial  begin
    force `TLM1_TOP.tlm_int = 0;
    #10us;
    force `TLM1_TOP.tlm_int = 1;
    
  end
  
endmodule
