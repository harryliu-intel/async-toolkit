//------------------------------------------------------------------------------
//   Author        : Harshal Singh (hdsingh)
//   Project       : Madison Bay
//------------------------------------------------------------------------------

typedef class mby_mesh_first_pkt_seq;
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
       // Specifying main phase sequence
       uvm_config_db#(uvm_object_wrapper)::set(this,
        "env.mby_mesh_tb_sequencer.main_phase",
         "default_sequence", mby_mesh_first_pkt_seq::type_id::get());
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
   // Task: run_phase()
   //
   // Prints out a message to identify start of run phase
   //---------------------------------------------------------------------------
   task run_phase(uvm_phase phase);
      phase.raise_objection(this);
      `uvm_info("::run_phase()", "Starting run phase", UVM_NONE)
      //
      phase.drop_objection(this);
   endtask : run_phase

endclass : mby_mesh_first_pkt_test


class mby_mesh_first_pkt_seq extends mby_mesh_seq_lib::mby_mesh_env_base_seq;

    `uvm_object_utils(mby_mesh_first_pkt_seq)

    //------------------------------------------------------------------------------
    //  Constructor: new
    //  Arguments:
    //  name   - Mesh random test  seq object name.
    //------------------------------------------------------------------------------
    function new (string name="mby_mesh_first_pkt_seq");
        super.new (name);
        this.set_automatic_phase_objection(1);
      `uvm_info(this.get_name(), ("mby_mesh_first_pkt_seq::new"), UVM_LOW)
        
    endfunction :  new

    //------------------------------------------------------------------------------
    //  Task:  body
    //  Counts 50 clocks and then completes.
    //------------------------------------------------------------------------------
    task body ();

    this.set_name("mby_mesh_first_pkt_seq");


    endtask

endclass : mby_mesh_first_pkt_seq
