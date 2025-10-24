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

`ifndef __PCIE_COVER_SV__
`define __PCIE_COVER_SV__

class pcie_cover extends uvm_component;

   // Pointer to the packet to be covered
   DenaliSvPcie::denaliPcieTlpPacket coverTlp;

   `uvm_component_utils(pcie_cover)

   // Analysis imports which connect to analysis ports
   uvm_analysis_imp #(DenaliSvPcie::denaliPciePacket, pcie_cover) CoverEndedCbPortImp;

   // Example coverage group
   covergroup covTlp; 
      option.per_instance = 1;
    
      TlpType : coverpoint coverTlp.tlpType 
      {
         ignore_bins ignoreTypes = { 
                                      DenaliSvPcie::DENALI_PCIE_TL_UNKNOWN,
                                      DenaliSvPcie::DENALI_PCIE_TL_TCfgRd,
                                      DenaliSvPcie::DENALI_PCIE_TL_TCfgWr,
                                      DenaliSvPcie::DENALI_PCIE_TL_TYPE_TOTAL
                                   };
      }

      TrafficClass : coverpoint coverTlp.trafficClass
      {
         bins tc [8] = { [ 0 : 7 ] };
      }

      Length : coverpoint coverTlp.length 
      { 
         bins len_0        = {            0   }; 
         bins len_1_31     = { [   1 :   31 ] }; 
         bins len_32_63    = { [  32 :   63 ] }; 
         bins len_64_127   = { [  64 :  127 ] }; 
         bins len_128_255  = { [ 128 :  255 ] }; 
         bins len_256_511  = { [ 256 :  511 ] }; 
         bins len_512_1023 = { [ 512 : 1023 ] };
      }
 
      MemTlpLength : cross Length, TlpType
      {
         ignore_bins ignoreTlpTypes = ! binsof(TlpType) intersect { 
                                                                     DenaliSvPcie::DENALI_PCIE_TL_MRd_32, 
                                                                     DenaliSvPcie::DENALI_PCIE_TL_MRd_64,
                                                                     DenaliSvPcie::DENALI_PCIE_TL_MWr_32, 
                                                                     DenaliSvPcie::DENALI_PCIE_TL_MWr_64 
                                                                  };
      }
    
   endgroup : covTlp

   function new(string name = "pcie_cover", uvm_component parent = null);
      super.new(name, parent);

      covTlp = new();
      covTlp.set_inst_name({get_full_name(), ".covTlp"});

      CoverEndedCbPortImp = new("CoverEndedCbPortImp", this);
   endfunction : new

   virtual function void write(DenaliSvPcie::denaliPciePacket packet);
      if ($cast(coverTlp, packet))
         collectTlpCoverage();
   endfunction : write 

   
   virtual function void collectTlpCoverage();
      covTlp.sample();
      `uvm_info(get_type_name(), {"TLP Coverage Collected:\n"}, UVM_HIGH)
   endfunction : collectTlpCoverage

endclass : pcie_cover

`endif /* __PCIE_COVER_SV__ */

// -------------------------------------------------------------------
//
