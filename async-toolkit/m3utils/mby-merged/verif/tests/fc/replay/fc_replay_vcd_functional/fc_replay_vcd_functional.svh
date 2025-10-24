// -----------------------------------------------------------------------------
// Copyright(C) 2013 Intel Corporation, Confidential Information
// -----------------------------------------------------------------------------
//
// <test_header>
//     <title>functional replay VCD test(basic)</title>
//     <team>presi val</team>
//     <environment value="fullchip">
//         <project value="emmitsburg">
//         </project>
//     </environment>
// </test_header>
//
// -----------------------------------------------------------------------------
// dft_sim -m fc -x fc_replay_vcd_functional:VCD_CONFIG_FILE=vcd_config.csv 

`ifdef FC_VCD_REPLAY
import sigaccess_pkg::*;
class fc_replay_vcd_sequence extends fc_seq_pkg::fc_base_seq;
    `uvm_object_utils(fc_replay_vcd_sequence) 
    `uvm_declare_p_sequencer(slu_sequencer)
    vcd_player_seq vcd_player;
    
    function new(string name = "fc_replay_vcd_sequence");
        super.new(name);
    endfunction

    virtual task body();
        `uvm_info(get_name(), "start fc_replay_vcd_sequence", UVM_LOW);
        fork begin
            reset_fuses();
        end      
        join_none
        `uvm_do(vcd_player);
        `uvm_info(get_name(), "exit fc_replay_vcd_sequence", UVM_LOW);
    endtask    

    virtual task reset_fuses();
        wait(dut_if.rsmrstb === 1);
        tb_env.fuse_env.reset_fuse_ram();
    endtask    
endclass
// -----------------------------------------------------------------------------
class fc_replay_vcd_functional extends fc_base_test;
    `uvm_component_utils(fc_test_pkg::fc_replay_vcd_functional)

    sigaccess_wrapper sigs;
   function new(string name = "fc_replay_vcd_functional", uvm_component parent = null);
        super.new(name, parent);
        $display("ACE_ERR_FILTER: filter fc_replay_vcd_functional");
    endfunction

    virtual function void build_phase(uvm_phase phase);
        super.build_phase(phase);
        tb_env.skip_test_phase("POWER_GOOD_PHASE"); 
        tb_env.skip_test_phase("TRAINING_PHASE"); 
        tb_env.skip_test_phase("CONFIG_PHASE"); 
        tb_env.skip_test_phase("FLUSH_PHASE");         
        sigs = new("sigaccess", this);
    endfunction

    virtual function void connect_phase(uvm_phase phase);
        super.connect_phase(phase);
        uvm_top.set_config_object(.inst_name("*"), .field_name("sigaccess"), .value(sigs), .clone(0));
        tb_env.set_test_phase_type( "tb_env", "USER_DATA_PHASE", "fc_replay_vcd_sequence");      
        set_timeout(10000us, "USER_DATA_PHASE");     
    endfunction

endclass
`endif

