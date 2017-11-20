
/* ----------------------------------------------------------------------


   ----------------------------------------------------------------------
   file:     tlm_traffic_seqlib.sv
   Date Created  : 25/7/2016
   Author        : dbenita
   Project       : TLM1 IP
   ----------------------------------------------------------------------
   Section: TLM1 traffics seq
 
 This file include all the TLM1 traffic sequences.
 
 The sequences use RAL and SM to generate traffic to the DUT
 
  

*/


/*
 Class: sc_tlm_simple_traffic
 
 TLM1 simple traffic class seq.
 
 */

class sc_tlm_simple_traffic extends tlm_config_base_seq;
  
  `uvm_object_utils(sc_tlm_simple_traffic) 
  
  `uvm_declare_p_sequencer(slu_sequencer)
    
    task body();
      uvm_report_warning (get_name(), "INTEG - TLM1 training seq is not IMP ");
      
    endtask

  
  
endclass // sc_tlm_simple_traffic
