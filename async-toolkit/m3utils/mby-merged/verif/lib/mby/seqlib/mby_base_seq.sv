// ----------------------------------------------------------------------
//
//
//   ----------------------------------------------------------------------
//   file:     mby_base_seq.sv
//   Date Created  : 25/7/2016
//   Author        : dbenita
//   Project       : MBY IP
//   ----------------------------------------------------------------------
//   Section: MBY Config seq
//
//  This file contain all of the MBY base sequences.
//
// Class: mby_base_seq
// 
// Base sequence for all the sequences.
// 
// This base sequence setup the MBY RAL reg file pointer in the sequence which will be used by all the config seq
// to access MBY registers
  

class mby_base_seq extends sla_sequence_base;
  `ovm_sequence_utils(mby_base_seq,sla_sequencer)
  
  sla_status_t status;
  sla_ral_data_t rd_val, wr_val;
  mby_regs_file mby;
  sla_ral_env ral;

   /*
    Function: new
    
    Constractor, set up the MBY RAL pointer.
    */
  function new(input string name = "mby_base_seq",
               ovm_sequencer_base sequencer=null, ovm_sequence parent_seq=null);
    super.new(name, sequencer, parent_seq);

	 `sla_assert($cast(ral, sla_ral_env::get_ptr()), ("Unable to get handle to RAL."))
    
      `sla_assert($cast(mby, ral.find_file("mby_regs")), ("Unable to get handle to mby"))
  endfunction

  virtual function void sm_config();
    sm.ag.allocate_mem(ag_result, "MMIO_LOW", 32'h2_0000, "GBE_MEM_LOW",32'h1_FFFF);
  endfunction

endclass



