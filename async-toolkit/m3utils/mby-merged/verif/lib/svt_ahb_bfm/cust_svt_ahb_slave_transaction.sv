
class cust_svt_ahb_slave_transaction extends svt_ahb_slave_transaction;

   int response_type_okay_wt   = 1000;
   int response_type_error_wt  = 1;
  
   int num_wait_cycles_zero_wt = 50;
   int num_wait_cycles_non_zero_wt = 1;

   // Declare user-defined constraints
   constraint slave_constraints {

      response_type dist {svt_ahb_transaction::OKAY:=response_type_okay_wt,
                        svt_ahb_transaction::ERROR:=response_type_error_wt};
  
      num_wait_cycles dist { 0 := num_wait_cycles_zero_wt,
                          [1:16] := num_wait_cycles_non_zero_wt };

   }

   /** UVM Object Utility macro */
   `uvm_object_utils_begin(cust_svt_ahb_slave_transaction)
      `uvm_field_int(response_type_okay_wt,UVM_ALL_ON)
      `uvm_field_int(response_type_error_wt ,UVM_ALL_ON)
   `uvm_object_utils_end

   /** Class Constructor */
   function new (string name = "cust_svt_ahb_slave_transaction");
      super.new(name);
   endfunction: new

endclass