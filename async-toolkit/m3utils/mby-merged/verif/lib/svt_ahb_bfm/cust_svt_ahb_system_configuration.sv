
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
 
   
   function void setup_ahb(int num_mst, int num_slv, bit is_active, int dwidth);
             
      this.num_masters = num_mst;
      this.num_slaves = num_slv;
      /** Create port configurations */
      this.create_sub_cfgs(num_mst,num_slv);
      if (num_mst) begin
         this.master_cfg[0].is_active  = is_active;
         this.master_cfg[0].data_width = dwidth;
      end
      else if (num_slv) begin
         this.slave_cfg[0].is_active  = is_active;
         this.slave_cfg[0].data_width = dwidth;
      end  
   endfunction 

   

endclass