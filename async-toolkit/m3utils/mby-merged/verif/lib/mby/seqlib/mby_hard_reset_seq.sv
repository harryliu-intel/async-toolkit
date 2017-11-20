

/* ----------------------------------------------------------------------


   ----------------------------------------------------------------------
   file:     mby_power_seqlib.sv
   Date Created  : 25/7/2016
   Author        : dbenita
   Project       : MBY IP
   ----------------------------------------------------------------------
   Section: MBY Power seq

  This file contain all of the MBY power sequences.
 

*/


/*
 Class: mby_hard_reset_seq
 
 This sequence power up the IP
 
 _This seq shouldn't be reuse by SOC!_
 
 */

class mby_hard_reset_seq extends mby_extended_base_seq;
  `ovm_sequence_utils(mby_hard_reset_seq,sla_sequencer)



    task body(); 
      virtual mby_env_if env_if;
      mby_env mby_env_ptr;
      ovm_event mby_fusepull_comp_e;
      ovm_event_pool    MBYevPool;

      ;

      
      ovm_report_warning (get_name(), "INTEG - MBY_hard_reset should be IMP ");

      `sla_assert($cast(mby_env_ptr, mby_env::get_mby_env()), ("Unable to get handle to mby_env."))
      
	//Pointer to env IF
	env_if = mby_env_ptr.mby_if;
      //pointer to event pool and fuse event
      MBYevPool = MBYevPool.get_global_pool();
      mby_fusepull_comp_e = MBYevPool.get("MBY_DETECT_FUSEPULL_COMP_SB_MSG");
	
      //Reset and power up flow
      env_if.power_good_reset = 0;
      env_if.primary_reset = 0;
      env_if.secondary_reset = 0;
      env_if.enable_secondary_clock = 0;
      env_if.enable_primary_clock = 0;

      //power good
      #1;
      env_if.power_good_reset = 1;

      // Secondeary interface
      #1;
      env_if.enable_secondary_clock = 1;
      repeat (10) begin
	@(posedge env_if.secondary_clock);
      end
      env_if.secondary_reset = 1;

      mby_fusepull_comp_e.wait_trigger();
      
      //Primary interface
      env_if.enable_primary_clock = 1;
      repeat (10) begin
	@(posedge env_if.primary_clock);
      end	
      env_if.primary_reset = 1;
      
      
      
      
    endtask // body
endclass // mby_hard_reset_seq

