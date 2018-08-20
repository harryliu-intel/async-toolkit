class cust_svt_ahb_master_transaction extends svt_ahb_master_transaction;
   int burst_type_single_wt = 1;
   int burst_type_incr4_wt  = 2;
   int burst_type_incr_wt   = 3;

   int num_busy_cycles_zero_wt = 500;
   int num_busy_cycles_non_zero_wt = 1;

   // Declare user-defined constraints
   constraint master_constraints {
      burst_type dist { svt_ahb_transaction::SINGLE := burst_type_single_wt,
                        svt_ahb_transaction::INCR   := burst_type_incr_wt,
                        svt_ahb_transaction::INCR4  := burst_type_incr4_wt }; 
  
      foreach (num_busy_cycles[i]) {
         num_busy_cycles[i] dist { 0 := num_busy_cycles_zero_wt, 
                                   [1:16] := num_busy_cycles_non_zero_wt};  
      }  

      (addr >=0 && addr <= 'h500);
   }

   /** UVM Object Utility macro */
   `uvm_object_utils_begin(cust_svt_ahb_master_transaction)
      `uvm_field_int(burst_type_single_wt,UVM_ALL_ON)
      `uvm_field_int(burst_type_incr_wt ,UVM_ALL_ON)
      `uvm_field_int(burst_type_incr4_wt ,UVM_ALL_ON)
   `uvm_object_utils_end

   /** Class Constructor */
   function new (string name = "cust_svt_ahb_master_transaction");
      super.new(name);
   endfunction: new

endclass