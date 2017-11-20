


import ovm_pkg::*;
import sla_pkg::*;
import mby_env_pkg::*;

`include "ovm_macros.svh"
`include "sla_macros.svh"

class mby_jasper_cov_all_test extends ovm_test;
  
  `ovm_component_utils(mby_jasper_cov_all_test)
    
  mby_env env;
  
  function new (string name="mby_jasper_cov_all_test", ovm_component parent=null);
    super.new (name, parent);
  endfunction // new

   function void build();
    ovm_report_info(get_full_name(),"Build", OVM_LOG);
    env = mby_env::type_id::create("env",this);
     
   endfunction // void

   function void connect();
     super.connect();

// START IOSF_NOT_PRESENT
     env.set_test_phase_type("env", "HARD_RESET_PHASE", "mby_hard_reset_seq");
     env.set_test_phase_type("env", "CONFIG_PHASE", "mby_pci_config_seq");
// END IOSF_NOT_PRESENT
   endfunction // void
  
  
endclass // my_test

module mby_jasper_cov_all_test ();

initial begin
  run_test("");

end
endmodule // mby_jasper_cov_all_test

