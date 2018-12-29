//------------------------------------------------------------------------------
//   Author        : Harshal Singh (hdsingh)
//   Project       : Madison Bay
//------------------------------------------------------------------------------
`ifndef MBY_MESH_FIRST_PKT_TEST__SV
 `define MBY_MESH_FIRST_PKT_TEST__SV

typedef class mby_mesh_first_pkt_seq;
//------------------------------------------------------------------------------
// Class : mby_mesh_first_pkt_test
// Test class to send first pkt 
//------------------------------------------------------------------------------
class mby_mesh_first_pkt_test extends mby_mesh_base_test;

    `uvm_component_utils(mby_mesh_first_pkt_test)
    //------------------------------------------------------------------------------
    // Constructor: new
    //  Arguments:
    //  name   - Mesh random test object name.
    //  parent - Component parent object.
    //------------------------------------------------------------------------------
    function new (string name="mby_mesh_first_pkt_test", uvm_component parent=null);
        super.new (name, parent);
    endfunction :  new

    //------------------------------------------------------------------------------
    // Function: build_phase
    //  Arguments:
    //  phase - uvm_phase object.
    //------------------------------------------------------------------------------
    function void build_phase(uvm_phase phase);
        super.build_phase(phase);
    endfunction : build_phase

    //------------------------------------------------------------------------------
    // Function: connect_phase
    // Sets USER_DATA_PHASE sequence.
    //
    //  Arguments:
    //  phase - uvm_phase object.
    //------------------------------------------------------------------------------
    function void connect_phase(uvm_phase phase);
       super.connect_phase(phase);

    endfunction : connect_phase

   //---------------------------------------------------------------------------
   // Function: set_default_sequences()
   //---------------------------------------------------------------------------
   function void set_default_sequences();
      super.set_default_sequences();
      `uvm_info("::set_default_sequences", "Setting phase sequences", UVM_NONE)

      uvm_config_db#(uvm_object_wrapper)::set(this,
         "env.tb_seqr.main_phase",
         "default_sequence",
         mby_mesh_first_pkt_seq::type_id::get());
   endfunction : set_default_sequences

    //---------------------------------------------------------------------------
   // Function: start_of_simulation_phase()
   //
   // Updates the uvm report server to use MBY's report server
   //
   //---------------------------------------------------------------------------
   function void start_of_simulation_phase( uvm_phase phase );
      super.start_of_simulation_phase( phase );
 
      `uvm_info("start_of_simulation_phase()", "Exiting start_of_sim phase", UVM_NONE)
   endfunction: start_of_simulation_phase
   
   //---------------------------------------------------------------------------
   // Task: run_phase()
   //
   // Prints out a message to identify start of run phase
   //---------------------------------------------------------------------------
   task run_phase(uvm_phase phase);
      phase.raise_objection(this);
      `uvm_info("::run_phase()", "Starting run phase", UVM_NONE)
      
      phase.drop_objection(this);
   endtask : run_phase

endclass : mby_mesh_first_pkt_test

//------------------------------------------------------------------------------
// Class : mby_mesh_first_pkt_seq
// This is test sequence clasee
//------------------------------------------------------------------------------
class mby_mesh_first_pkt_seq extends mby_mesh_seq_lib::mby_mesh_env_base_seq;

   `uvm_object_utils(mby_mesh_first_pkt_seq)

   mby_mesh_req_seq eb_req_seq;

   //------------------------------------------------------------------------------
   //  Constructor: new
   //  Arguments:
   //  name   - Mesh random test  seq object name.
   //------------------------------------------------------------------------------
   function new (string name="mby_mesh_first_pkt_seq");
      super.new (name);
      this.set_automatic_phase_objection(1);
      `uvm_info(this.get_name(), ("mby_mesh_first_pkt_seq::new"), UVM_NONE)
        
   endfunction :  new
   
   //------------------------------------------------------------------------------
   //  Task:  body
   //  Counts 50 clocks and then completes.
   //------------------------------------------------------------------------------
   task body ();

      this.set_name("mby_mesh_first_pkt_seq");

      eb_req_seq = mby_mesh_req_seq::type_id::create("eb_req_seq");

      `uvm_info(this.get_name(), ("First pkt seq starting"), UVM_LOW)

      //
      // Sending 1 write request.
      //
      eb_req_seq.start(env.eb_mgp_bfm[0].wrreq_agent[0].req_seqr);
      repeat (200) @(vif.fab_clk);

      //
      // Sending 1 read request.
      //
      eb_req_seq.start(env.eb_mgp_bfm[0].rdreq_agent[0].req_seqr);
      repeat (200) @(vif.fab_clk);

   endtask

endclass : mby_mesh_first_pkt_seq
`endif