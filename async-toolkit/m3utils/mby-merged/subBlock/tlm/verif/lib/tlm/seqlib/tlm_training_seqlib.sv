
/* ----------------------------------------------------------------------


   ----------------------------------------------------------------------
   file:     tlm_training_seqlib.sv
   Date Created  : 25/7/2016
   Author        : dbenita
   Project       : TLM1 IP
   ----------------------------------------------------------------------
   Section: TLM1 training seq

  This file contain all of the TLM1 training sequences.
 
  
 
 

*/



/*
 Class: tlm_training_phase_seq
 
 INTEG need to imp if needed
  
 */


class tlm_training_phase_seq extends slu_sequence_base;
  

  
  
  `uvm_object_utils(tlm_training_phase_seq) 
  

  
  
  `uvm_declare_p_sequencer(slu_sequencer)
    
    task body();
      uvm_report_warning (get_name(), "INTEG - TLM1 training seq is not IMP ");
            
    endtask
endclass // tlm_training_phase_seq
