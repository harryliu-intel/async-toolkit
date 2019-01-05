// -----------------------------------------------------------------------------
// INTEL CONFIDENTIAL
// Copyright(c) 2018, Intel Corporation. All Rights Reserved
// -----------------------------------------------------------------------------
//
// Created By   :  Subodh Nanal 
// Created On   :  12/14/2018
// Description  :  Basic packet test 
// -----------------------------------------------------------------------------

class frame_test_eth_frame extends eth_bfm_pkg::eth_frame;

   `uvm_object_utils(frame_test_eth_frame)

   //-------------------------------------------------------------------------------------------------------------------
   // Function: new
   //
   // Constructor.
   //
   // Arguments:
   //    string name - object name
   //-------------------------------------------------------------------------------------------------------------------
   function new(string name = "frame_test_eth_frame");
      super.new(name);
   endfunction

   //-------------------------------------------------------------------------------------------------------------------
   // Constraint: test_constraints
   //
   // Constraints used for frames started from this test.
   //-------------------------------------------------------------------------------------------------------------------
   constraint constraints {
      payload.size == 64;  
      kind           inside {eth_bfm_pkg::BASIC_FRAME,
                             eth_bfm_pkg::IPV4_FRAME,
                             eth_bfm_pkg::IPV6_FRAME};
      vlan.size == 0;    
      mpls.size == 0;   
      corrupt == 0;    
      bubble == 0;      
      dmac            == 'h000102030405;
      smac            == 'h060708090a0b;
   };
endclass


class fc_pkt_basic_seq extends fc_seq_pkg::fc_base_seq;
    `uvm_object_utils(fc_pkt_basic_seq) 
    `uvm_declare_p_sequencer(slu_sequencer)

    sla_pkg::slu_sequencer virtual_seqr;

    function new(string name = "fc_pkt_basic_seq");
        super.new(name);
    endfunction

    virtual task body();
        eth_port_env_pkg::eth_port_frame_seq frame_seq;
        //TODO eth_bfm_pkg::eth_frame               frame;
        frame_test_eth_frame               frame;

        uvm_sequencer_base frame_seqr;


        `uvm_info(get_name(), "enter fc_pkt_basic_seq", UVM_LOW);

        //TEMP: EPC 0 - EPL0 single port transmit and receive


        //TODO frame = eth_bfm_pkg::eth_frame::type_id::create("frame");
        //TODO assert(frame.randomize() with {
        //TODO          bubble         == 0;
        //TODO          kind           inside {eth_bfm_pkg::BASIC_FRAME,
        //TODO                                 eth_bfm_pkg::IPV4_FRAME,
        //TODO                                 eth_bfm_pkg::IPV6_FRAME};
        //TODO          payload.size()  == 64;
        //TODO          dmac            == 'h000102030405;
        //TODO          smac            == 'h060708090a0b;
        //TODO          (kind == eth_bfm_pkg::BASIC_FRAME) ->
        //TODO             foreach (payload[idx])
        //TODO               payload[idx] == idx;
        //TODO          })
        //TODO        else begin
        //TODO              `uvm_error(get_name(), "Unable to randomize eth_pkt");
        //TODO        end

        frame = frame_test_eth_frame::type_id::create("frame");
        frame_seq = eth_port_env_pkg::eth_port_frame_seq::type_id::create($sformatf("frame_seq_%0d_%0d", 0, 0));
        //TODO NOT REQD frame_seq.env = tb_env.epc_subenv.epc_env_inst[0].part_env[0];
        frame_seq.frame = frame;
        frame_seq.num_frames = 1;

        frame_seqr = tb_env.epc_subenv.epc_env_inst[0].part_env[0].active_frame_seqrs[0];
        frame_seq.start(frame_seqr);

        #10us;

        `uvm_info(get_name(), "exit fc_pkt_basic_seq", UVM_LOW);
    endtask : body
endclass : fc_pkt_basic_seq

class fc_pkt_basic extends fc_base_test;
    `uvm_component_utils(fc_pkt_basic)

   function new(string name = "fc_pkt_basic", uvm_component parent = null);
        super.new(name, parent);
    endfunction

    virtual function void build_phase(uvm_phase phase);
        set_timeout(1000us, "USER_DATA_PHASE");
        super.build_phase(phase);
        tb_env.skip_test_phase("CONFIG_PHASE");
    endfunction

    virtual function void connect_phase(uvm_phase phase);
        super.connect_phase(phase);
        tb_env.set_test_phase_type( "tb_env", "USER_DATA_PHASE", "fc_pkt_basic_seq");

    endfunction

    // UVM method -> end_of_elaboration function                             
    function void end_of_elaboration_phase(uvm_phase phase);                                       
        super.end_of_elaboration_phase (phase);                                            
    endfunction : end_of_elaboration_phase                                       
                                                                          
    // UVM method -> start_of_simulation function                      
    function void start_of_simulation_phase(uvm_phase phase); 
        super.start_of_simulation_phase (phase);   
        if($test$plusargs("SIMPROFILING")) begin
            set_timeout(10000us, "USER_DATA_PHASE");
        end      
    endfunction : start_of_simulation_phase              

endclass

