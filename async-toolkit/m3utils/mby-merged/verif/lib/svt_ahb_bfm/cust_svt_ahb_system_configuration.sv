
class cust_svt_ahb_system_configuration extends svt_ahb_system_configuration;

   /** UVM Object Utility macro */
   `uvm_object_utils_begin (cust_svt_ahb_system_configuration)
      `uvm_field_int(num_masters,    UVM_DEFAULT)
      `uvm_field_int(num_slaves,    UVM_DEFAULT)
    `uvm_object_utils_end

   /** Class Constructor */
   function new (string name = "cust_svt_ahb_system_configuration");

      super.new(name);
      
   endfunction 
 
   

endclass