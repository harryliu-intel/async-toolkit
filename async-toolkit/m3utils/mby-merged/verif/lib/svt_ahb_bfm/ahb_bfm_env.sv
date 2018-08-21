
typedef class cust_svt_ahb_system_configuration;
   
class ahb_bfm_env extends uvm_env;

   /** AHB System ENV */
   svt_ahb_system_env   ahb_system_env;

   virtual svt_ahb_if ahb_if;
   
   /** Virtual Sequencer */
   ahb_virtual_sequencer sequencer;

   /** AHB System Configuration */
   cust_svt_ahb_system_configuration cfg;

   /** UVM Component Utility macro */
   `uvm_component_utils(ahb_bfm_env)

   /** Class Constructor */
   function new (string name="ahb_bfm_env", uvm_component parent=null);
      super.new (name, parent);
   endfunction

   virtual function void set_vif(virtual svt_ahb_if ahb_vif);
      ahb_if = ahb_vif;
   endfunction 
   
   /** Build the AHB System ENV */
   virtual function void build_phase(uvm_phase phase);
      `uvm_info("build_phase", "Entered...",UVM_LOW)

      super.build_phase(phase);

      uvm_config_db#(svt_ahb_vif)::set(this, "ahb_system_env", "vif", ahb_if);

      
      /**
      * Check if the configuration is passed to the environment.
      * If not then create the configuration and pass it to the agent.
      */

      if (cfg == null) begin
         cfg = cust_svt_ahb_system_configuration::type_id::create("cfg");
	 `uvm_info ("build_phase" , $sformatf("ahb_cfg : %s", cfg.sprint()), UVM_NONE) 
      end
      
      uvm_config_db#(svt_ahb_system_configuration)::set(this, "ahb_system_env", "cfg", cfg);
      
      /** Construct the system agent */
      ahb_system_env = svt_ahb_system_env::type_id::create("ahb_system_env", this);
      /** Construct the virtual sequencer */
      sequencer = ahb_virtual_sequencer::type_id::create("sequencer", this);

      `uvm_info("build_phase", "Exiting...", UVM_LOW)
   endfunction

   virtual function void connect_phase(uvm_phase phase);
      `uvm_info("connect_phase", "Entered...",UVM_LOW)

      super.connect_phase(phase);
      
   endfunction

   function void setup_bfm(int num_mst, int num_slv, bit is_active, int data_width );

      cfg.num_masters = num_mst;
      cfg.num_slaves = num_slv;
      /** Create port configurations */
      cfg.create_sub_cfgs(num_mst,num_slv);
      if (num_mst) begin
         cfg.master_cfg[0].is_active  = is_active;
         cfg.master_cfg[0].data_width = data_width;
      end
      else if (num_slv) begin
         cfg.slave_cfg[0].is_active  = is_active;
         cfg.slave_cfg[0].data_width = data_width;
      end
      
  endfunction: setup_bfm

endclass 

