// -------------------------------------------------------------------
// -- Intel Proprietary
// -- Copyright (C) 2015 Intel Corporation
// -- All Rights Reserved
// -------------------------------------------------------------------
// -- Author : Nelson Crumbaker
// -- Project Name : Madison Bay
// -- Description :
// --
// -------------------------------------------------------------------

`ifndef __INSIDE_CDN_PCIE_PKG__
** ERROR: This file is meant to be used only through cdn_pcie_pkg.sv.  Do not include it individually.;
`endif  /* __INSIDE_CDN_PCIE_PKG__ */

`ifndef __PCIE_VIRTUAL_SEQ_LIB_SV__
`define __PCIE_VIRTUAL_SEQ_LIB_SV__

class pcie_virtual_sequence extends uvm_sequence;
  
   `uvm_object_utils(pcie_virtual_sequence)
   `uvm_declare_p_sequencer(pcie_virtual_sequencer)

   function new(string name="pcie_virtual_sequence");
      super.new(name);
   endfunction : new

   virtual task pre_body();
      if (starting_phase != null) 
         starting_phase.raise_objection(this, get_type_name() );
   endtask : pre_body

   virtual task post_body();
      if (starting_phase != null) 
         starting_phase.drop_objection(this, get_type_name() );
   endtask : post_body
endclass : pcie_virtual_sequence

// Simple Read After Write sequence
class parallelRdAfterWrSeq extends pcie_virtual_sequence;
  
   pcie_TLReadAfterWriteSeq RdAfterWrSeq;

   `uvm_object_utils_begin(parallelRdAfterWrSeq)
      `uvm_field_object(RdAfterWrSeq, UVM_ALL_ON)
   `uvm_object_utils_end
   `uvm_declare_p_sequencer(pcie_virtual_sequencer)

   function new(string name="parallelRdAfterWrSeq");
      super.new(name);
   endfunction // new

   virtual task body();
      // #50000;
      #500;
      `uvm_info(get_type_name(), "Virtual sequence parallelRdAfterWrSeq started", UVM_MEDIUM);
      `uvm_do_on(RdAfterWrSeq, p_sequencer.pSeqr0)
      #20000;
      `uvm_info(get_type_name(), "Finished body of parallelRdAfterWrSeq", UVM_MEDIUM);
   endtask : body 
endclass : parallelRdAfterWrSeq

// Error Injection
/* class pcie_errinj_vseq extends pcie_virtual_sequence;
  
   pcie_ECRCerrorInj eiSeq;
   pcie_FundReset    rstSeq;

   `uvm_object_utils_begin(pcie_errinj_vseq)
      `uvm_field_object(eiSeq, UVM_ALL_ON)
      `uvm_field_object(rstSeq, UVM_ALL_ON)
   `uvm_object_utils_end
   `uvm_declare_p_sequencer(pcie_virtual_sequencer)

   function new(string name="pcie_errinj_vseq");
      super.new(name);
   endfunction : new

   virtual task body();
      #50000;
      `uvm_info(get_type_name(), "Virtual sequence pcie_errinj_vseq started", UVM_MEDIUM);
      `uvm_do_on(eiSeq, p_sequencer.pSeqr0)

      #10000;
      `uvm_info(get_type_name(), "Finished body of pcie_errinj_vseq", UVM_MEDIUM);
   endtask : body
endclass : pcie_errinj_vseq */

// Send a Message to DUT
class pcie_vdm_vseq extends pcie_virtual_sequence;
  
   pcie_VDMSequence msgSeq;

   `uvm_object_utils_begin(pcie_vdm_vseq)
      `uvm_field_object(msgSeq, UVM_ALL_ON)
   `uvm_object_utils_end
   `uvm_declare_p_sequencer(pcie_virtual_sequencer)

   function new(string name="pcie_vdm_vseq");
      super.new(name);
   endfunction : new

   virtual task body();
      #50000;
      `uvm_info(get_type_name(), "Virtual sequence pcie_vdm_vseq started", UVM_MEDIUM);
      `uvm_do_on(msgSeq, p_sequencer.pSeqr0)

      #10000;
      `uvm_info(get_type_name(), "Finished body of pcie_vdm_vseq", UVM_MEDIUM);
      
   endtask : body
endclass : pcie_vdm_vseq

`endif /* __PCIE_VIRTUAL_SEQ_LIB_SV__ */

// -------------------------------------------------------------------
//
