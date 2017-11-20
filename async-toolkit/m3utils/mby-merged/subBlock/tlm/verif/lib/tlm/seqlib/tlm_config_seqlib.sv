


/* ----------------------------------------------------------------------


   ----------------------------------------------------------------------
   file:     tlm_config_seqlib.sv
   Date Created  : 25/7/2016
   Author        : dbenita
   Project       : TLM1 IP
   ----------------------------------------------------------------------
   Section: TLM1 Config seq

  This file contain all of the TLM1 configuration sequences.
 

*/



/*
 Class: tlm_config_base_seq
 
 Base sequence for all the config sequences.
 
 This base sequence setup the TLM1 RAL reg file pointer in the sequence which will be used by all the config seq
 to access TLM1 registers
 
  
 */
class tlm_config_base_seq extends slu_sequence_base;
  `uvm_object_utils(tlm_config_base_seq) 
  `uvm_declare_p_sequencer(slu_sequencer)
  
  slu_status_t status;
  slu_ral_data_t rd_val, wr_val;
  sla_ral_env ral;
  tlm_regs_file tlm;

   /*
    Function: new
    
    Constractor, set up the TLM1 RAL pointer.
    */
  function new(input string name = "tlm_config_base_seq",
               uvm_sequencer_base sequencer=null, uvm_sequence parent_seq=null);
    super.new(name, sequencer, parent_seq);
    
    
      `slu_assert($cast(ral, sla_ral_env::get_ptr()), ("Unable to get handle to RAL."))
      `slu_assert($cast(tlm, ral.find_file("tlm_regs")), ("Unable to get handle to tlm"))
  endfunction
endclass

/*
 Class: tlm_pci_config_seq
 
 This sequence set up the TLM1 config space:
 */
class tlm_pci_config_seq extends tlm_config_base_seq;
  `uvm_object_utils(tlm_pci_config_seq) 
  `uvm_declare_p_sequencer(slu_sequencer)
    
    task body();
      uvm_report_warning (get_name(), "INTEG - TLM1 _config should be IMP ");
      
      // This example of using SM and RAL to configure IP BASE address
      begin	
	slu_sm_ag_result     tlm_mem_bar;
	sm.ag.allocate_mem(tlm_mem_bar, "MMIO_LOW", 32'h2_0000, "GBE_MEM_LOW",32'h1_FFFF);
	
	tlm.BASE_ADDR_0.write(status,tlm_mem_bar.addr,"primary",this);

      end
    endtask // body
endclass // tlm_pci_config_seq


/*
 Class: tlm_isr_seq
 
 TLM1 interrupt service routine
 */
class tlm_isr_seq extends tlm_config_base_seq;
  `uvm_object_utils(tlm_isr_seq) 
  `uvm_declare_p_sequencer(slu_sequencer)
    
    task body();
      uvm_report_warning (get_name(), "INTEG - tlm_isr_seq should be imp ");
      
    endtask // body
endclass // tlm_isr_seq

