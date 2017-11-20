// ----------------------------------------------------------------------
//
//
//   ----------------------------------------------------------------------
//   file:     mby_extended_base_seq.sv
//   Date Created  : 25/7/2016
//   Author        : dbenita
//   Project       : MBY IP
//   ----------------------------------------------------------------------
//   Section: MBY Config seq
//
//  This file contain all of the MBY base sequences.
//
// Class: mby_extended_base_seq
// 
// Base sequence for all the sequences.
// 
// This base sequence setup the MBY RAL reg file pointer in the sequence which will be used by all the config seq
// to access MBY registers
  

class mby_extended_base_seq extends mby_base_seq;
  `ovm_sequence_utils(mby_extended_base_seq,sla_sequencer)
  
   /*
    Function: new
    
    Constractor, set up the MBY RAL pointer.
    */
  function new(input string name = "mby_extended_base_seq",
               ovm_sequencer_base sequencer=null, ovm_sequence parent_seq=null);
    super.new(name, sequencer, parent_seq);
    
  endfunction

endclass



