/* ----------------------------------------------------------------------


   ----------------------------------------------------------------------
   file:     mby_pci_config_seq.sv
   Date Created  : 25/7/2016
   Author        : dbenita
   Project       : MBY IP
   ----------------------------------------------------------------------
   Section: MBY Config seq

  This file contain all of the MBY configuration sequences.
*/

/*
 Class: mby_pci_config_seq
 
 This sequence set up the MBY config space:
 */
class mby_pci_config_seq extends mby_base_seq;
  `uvm_object_utils(mby_pci_config_seq) 
  `uvm_declare_p_sequencer(slu_sequencer)
    
    task body();
      uvm_report_warning (get_name(), "INTEG - MBY _config should be IMP ");
      
      // This example of using SM and RAL to configure IP BASE address
      begin	
		 // sm_config();
	     //mby.BASE_ADDR_0.write(status,ag_result.addr,"primary",this);
      end
    endtask // body
endclass // mby_pci_config_seq


