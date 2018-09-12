
/* ----------------------------------------------------------------------


   ----------------------------------------------------------------------
   file:     mby_training_seqlib.sv
   Date Created  : 25/7/2016
   Author        : dbenita
   Project       : MBY IP
   ----------------------------------------------------------------------
   Section: MBY training seq

  This file contain all of the MBY training sequences.
 
  
 
 

*/



/*
 Class: mby_training_phase_seq
 
 INTEG need to imp if needed
  
 */


class mby_training_phase_seq extends mby_env_base_seq;
  

  
  
  `uvm_object_utils(mby_training_phase_seq) 
  

  
  
  `uvm_declare_p_sequencer(slu_sequencer)
    
    task body();
      uvm_report_warning (get_name(), "INTEG - MBY training seq is not IMP ");
            
    endtask
endclass // mby_training_phase_seq
