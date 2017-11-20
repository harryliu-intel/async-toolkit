


import uvm_pkg::*;

`ifdef XVM
   import ovm_pkg::*;
   import xvm_pkg::*;
   `include "ovm_macros.svh"
   `include "sla_macros.svh"
`endif

import sla_pkg::*;
import tlm_env_pkg::*;

`include "uvm_macros.svh"
`include "slu_macros.svh"

class tlm_alive_test extends uvm_test;
  
  `uvm_component_utils(tlm_alive_test)
    
  tlm_env env;
  
  function new (string name="tlm_alive_test", uvm_component parent=null);
    super.new (name, parent);
  endfunction // new

   function void build_phase(uvm_phase phase);
    uvm_report_info(get_full_name(),"Build", UVM_LOG);
    env = tlm_env::type_id::create("env",this);
     
   endfunction // void

   function void connect_phase(uvm_phase phase);
     super.connect_phase(phase);

   endfunction // void
  
  
endclass // my_test

module tlm_alive_test ();

import xvm_pkg::*;

  string testname;

  initial begin

if ($value$plusargs("UVM_TESTNAME=%s", testname  ))
    xvm_pkg::run_test("","", xvm::EOP_UVM);
  else
    run_test("");

end
endmodule // tlm_alive_test

