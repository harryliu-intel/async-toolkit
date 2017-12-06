


import uvm_pkg::*;

`ifdef XVM
   import ovm_pkg::*;
   import xvm_pkg::*;
   `include "ovm_macros.svh"
   `include "sla_macros.svh"
`endif

import sla_pkg::*;
import mby_env_pkg::*;

`include "uvm_macros.svh"
`include "slu_macros.svh"

class mby_jasper_cov_all_test extends uvm_test;
  
  `uvm_component_utils(mby_jasper_cov_all_test)
    
  mby_env env;
  
  function new (string name="mby_jasper_cov_all_test", uvm_component parent=null);
    super.new (name, parent);
  endfunction // new

   function void build_phase(uvm_phase phase);
    uvm_report_info(get_full_name(),"Build", UVM_LOG);
    env = mby_env::type_id::create("env",this);
     
   endfunction // void

   function void connect_phase(uvm_phase phase);
     super.connect_phase(phase);

// START IOSF_NOT_PRESENT
     env.set_test_phase_type("env", "HARD_RESET_PHASE", "mby_hard_reset_seq");
     env.set_test_phase_type("env", "CONFIG_PHASE", "mby_pci_config_seq");
// END IOSF_NOT_PRESENT
   endfunction // void
  
  
endclass // my_test

module mby_jasper_cov_all_test ();

  string testname;

  initial begin

  run_test("");

end
endmodule // mby_jasper_cov_all_test

