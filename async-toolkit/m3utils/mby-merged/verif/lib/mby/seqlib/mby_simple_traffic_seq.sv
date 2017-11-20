
/* ----------------------------------------------------------------------


   ----------------------------------------------------------------------
   file:     mby_traffic_seqlib.sv
   Date Created  : 25/7/2016
   Author        : dbenita
   Project       : MBY IP
   ----------------------------------------------------------------------
   Section: MBY traffics seq
 
 This file include all the MBY traffic sequences.
 
 The sequences use RAL and SM to generate traffic to the DUT
 
  

*/


/*
 Class: mby_simple_traffic_seq
 
 MBY simple traffic class seq.
 
 */

class mby_simple_traffic_seq extends mby_extended_base_seq;
  
  `ovm_sequence_utils(mby_simple_traffic_seq,sla_sequencer)
    
    task body();
      ovm_report_warning (get_name(), "INTEG - MBY training seq is not IMP ");
      
    endtask

  
  
endclass // mby_simple_traffic_seq
