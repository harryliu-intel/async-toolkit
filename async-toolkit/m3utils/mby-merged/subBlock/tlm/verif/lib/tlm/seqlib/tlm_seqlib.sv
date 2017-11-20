/* ----------------------------------------------------------------------


   ----------------------------------------------------------------------
   file:     tlm_seqlib.sv
   Date Created  : 25/7/2016
   Author        : dbenita
   Project       : TLM1 IP
   ----------------------------------------------------------------------
   Section: TLM1  sequences library
 
 include all TLM1 sequences
 
 <tlm_training_seqlib.sv> include the initial training that need to enable the TLM1.
 
 <tlm_config_seqlib.sv> include all the TLM1 configuration sequences.
 
 <tlm_traffic_seqlib.sv> include the TLM1 traffic  sequences
 
 <tlm_power_seqlib.sv> include the TLM1 power sequences

 *Note:* _You must run first the training seq followed by config sequences before enable TLM1 traffic_
 
 

*/



`include "tlm_training_seqlib.sv"
`include "tlm_config_seqlib.sv"
`include "tlm_traffic_seqlib.sv"
`include "tlm_power_seqlib.sv"
  