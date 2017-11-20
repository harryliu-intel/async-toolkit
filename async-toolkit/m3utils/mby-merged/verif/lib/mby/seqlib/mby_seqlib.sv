/* ----------------------------------------------------------------------


   ----------------------------------------------------------------------
   file:     mby_seqlib.sv
   Date Created  : 25/7/2016
   Author        : dbenita
   Project       : MBY IP
   ----------------------------------------------------------------------
   Section: MBY  sequences library
 
 include all MBY sequences
 
 <mby_training_seqlib.sv> include the initial training that need to enable the MBY.
 
 <mby_config_seqlib.sv> include all the MBY configuration sequences.
 
 <mby_traffic_seqlib.sv> include the MBY traffic  sequences
 
 <mby_power_seqlib.sv> include the MBY power sequences

 *Note:* _You must run first the training seq followed by config sequences before enable MBY traffic_
 
 

*/



`include "mby_base_seq.sv"
`include "mby_extended_base_seq.sv"
// START IOSF_NOT_PRESENT
`include "mby_iosf_pri_basic_trans.sv"
// END IOSF_NOT_PRESENT
`include "mby_hard_reset_seq.sv"
`include "mby_pci_config_seq.sv"
`include "mby_simple_traffic_seq.sv"
`include "mby_training_phase_seq.sv"
`include "mby_isr_seq.sv"
  
