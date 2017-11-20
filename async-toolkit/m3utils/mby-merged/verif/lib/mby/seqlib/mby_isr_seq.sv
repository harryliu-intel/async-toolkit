/* ----------------------------------------------------------------------


   ----------------------------------------------------------------------
   file:     mby_isr_seq.sv
   Date Created  : 25/7/2016
   Author        : dbenita
   Project       : MBY IP
   ----------------------------------------------------------------------
   Section: MBY Config seq

  This file contain all of the MBY configuration sequences.
*/

/*
 Class: mby_isr_seq
 
 This sequence set up the MBY config space:
 */
class mby_isr_seq extends mby_base_seq;
  `ovm_sequence_utils(mby_isr_seq,sla_sequencer)

    task body();
      ovm_report_warning (get_name(), "INTEG - mby_isr_seq should be imp ");

    endtask // body
endclass // mby_isr_seq
