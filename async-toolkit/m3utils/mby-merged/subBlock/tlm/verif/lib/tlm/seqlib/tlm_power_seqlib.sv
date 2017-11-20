

/* ----------------------------------------------------------------------


   ----------------------------------------------------------------------
   file:     tlm_power_seqlib.sv
   Date Created  : 25/7/2016
   Author        : dbenita
   Project       : TLM1 IP
   ----------------------------------------------------------------------
   Section: TLM1 Power seq

  This file contain all of the TLM1 power sequences.
 

*/


/*
 Class: tlm_hard_reset_seq
 
 This sequence power up the IP
 
 _This seq shouldn't be reuse by SOC!_
 
 */

class tlm_hard_reset_seq extends slu_sequence_base;
  `uvm_object_utils(tlm_hard_reset_seq) 
  `uvm_declare_p_sequencer(slu_sequencer)



    task body(); 
      virtual tlm_env_if env_if;
      tlm_env tlm_env_ptr;
      uvm_event tlm_fusepull_comp_e;
      uvm_event_pool    TLM1evPool;

      ;

      
      uvm_report_warning (get_name(), "INTEG - TLM1_hard_reset should be IMP ");

      `slu_assert($cast(tlm_env_ptr, tlm_env::get_tlm_env()), ("Unable to get handle to tlm_env."))
      
	//Pointer to env IF
	env_if = tlm_env_ptr.tlm_if;
      //pointer to event pool and fuse event
      TLM1evPool = TLM1evPool.get_global_pool();
      tlm_fusepull_comp_e = TLM1evPool.get("TLM1_DETECT_FUSEPULL_COMP_SB_MSG");
	
      //Reset and power up flow
      env_if.power_good_reset = 0;
      env_if.primary_reset = 0;
      env_if.secondary_reset = 0;
      env_if.enable_secondary_clock = 0;
      env_if.enable_primary_clock = 0;

      //power good
      #1us;
      env_if.power_good_reset = 1;

      // Secondeary interface
      #1us;
      env_if.enable_secondary_clock = 1;
      repeat (10) begin
	@(posedge env_if.secondary_clock);
      end
      env_if.secondary_reset = 1;

      tlm_fusepull_comp_e.wait_trigger();
      
      //Primary interface
      env_if.enable_primary_clock = 1;
      repeat (10) begin
	@(posedge env_if.primary_clock);
      end	
      env_if.primary_reset = 1;
      
      
      
      
    endtask // body
endclass // tlm_hard_reset_seq

class tlm_dummy_seq extends slu_sequence_base;
  `uvm_object_utils(tlm_dummy_seq) 
  `uvm_declare_p_sequencer(slu_sequencer)

    task body(); 
      #100ns; 
    endtask // body
endclass // tlm_dummy_seq

