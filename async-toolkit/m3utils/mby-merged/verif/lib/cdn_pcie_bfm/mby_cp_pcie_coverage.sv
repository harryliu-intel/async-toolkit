// -------------------------------------------------------------------
// -- Intel Proprietary
// -- Copyright (C) 2015 Intel Corporation
// -- All Rights Reserved
// -------------------------------------------------------------------
// -- Author : Nelson Crumbaker
// -- Project Name : Madison Bay
// -- Description : Functional Coverage for PCIe interface
// --   
// -------------------------------------------------------------------

`ifndef __INSIDE_CDN_PCIE_PKG__
** ERROR: This file is meant to be used only through cdn_tb_pkg.sv.  Do not include it individually.;
`endif  /* __INSIDE_CDN_PCIE_PKG__ */

`ifndef __MBY_CP_PCIE_COVERAGE_SV__
`define __MBY_CP_PCIE_COVERAGE_SV__

typedef enum {TO_DUT=0, FROM_DUT=1} tlp_direction_type;

class mby_cp_pcie_coverage extends uvm_component;

   bit coverage_enable = 0;
   mby_cp_pcie_cfg pcie_cfg;

   uvm_analysis_imp #(DenaliSvPcie::denaliPciePacket, mby_cp_pcie_coverage) pcie_cov_port;

   DenaliSvPcie::denaliPcieTlpPacket pcie_tlp_pkt;
   tlp_direction_type  tlp_dir;

   `uvm_component_utils_begin(mby_cp_pcie_coverage)
      `uvm_field_int(coverage_enable, UVM_DEFAULT)
   `uvm_component_utils_end

   virtual function void build();
      super.build();
   endfunction : build

   covergroup pcie_cg;
      // option.per_instance = 1;
      
      tlp_dir  : coverpoint tlp_dir
      {
         bins to_dut   = {TO_DUT};
         bins from_dut = {FROM_DUT};
      }

      tlp_type : coverpoint pcie_tlp_pkt.tlpType
      {
         bins cfg_rd    = {DenaliSvPcie::DENALI_PCIE_TL_CfgRd0};
         bins cfg_wr    = {DenaliSvPcie::DENALI_PCIE_TL_CfgWr0};
         bins mem_rd_32 = {DenaliSvPcie::DENALI_PCIE_TL_MRd_32};
         bins mem_wr_32 = {DenaliSvPcie::DENALI_PCIE_TL_MWr_32};
         bins mem_rd_64 = {DenaliSvPcie::DENALI_PCIE_TL_MRd_64};
         bins mem_wr_64 = {DenaliSvPcie::DENALI_PCIE_TL_MWr_64};
         bins mem_cpl   = {DenaliSvPcie::DENALI_PCIE_TL_Cpl};
         bins mem_cpld  = {DenaliSvPcie::DENALI_PCIE_TL_CplD};
      }

      length   : coverpoint pcie_tlp_pkt.length
      {
         // Max Payload size handled by PCIe Controller is 512 bytes = 128 dwords
         bins one_dword   = {'h1};
         bins small_pkt   = {['h2:'h1f]};
         bins med_pkt     = {['h20:'h3f]};
         bins large_pkt   = {['h40:'h7f]};
         bins max_size    = {'h80};
      }

      tlp_dir_X_tlp_type_X_length : cross tlp_dir, tlp_type, length
      {
         // Config Read TLPs to DUT
         bins cfg_rd_tlp_to_dut = binsof (tlp_dir.to_dut) && binsof (tlp_type.cfg_rd) && 
                                  binsof (length.one_dword);

         // Config Write TLPs to DUT
         bins cfg_wr_tlp_to_dut = binsof (tlp_dir.to_dut) && binsof (tlp_type.cfg_wr) && 
                                  binsof (length.one_dword);

         // Memory Reads to DUT to access DMA CSRs
         bins mem_rd_32_tlp_to_dut = binsof (tlp_dir.to_dut) && binsof (tlp_type.mem_rd_32) && 
                                     binsof (length.one_dword);
         bins mem_rd_64_tlp_to_dut = binsof (tlp_dir.to_dut) && binsof (tlp_type.mem_rd_64) && 
                                     binsof (length.one_dword);

         // Memory Writes to DUT to access DMA CSRs
         bins mem_wr_32_tlp_to_dut = binsof (tlp_dir.to_dut) && binsof (tlp_type.mem_wr_32) && 
                                     binsof (length.one_dword);
         bins mem_wr_64_tlp_to_dut = binsof (tlp_dir.to_dut) && binsof (tlp_type.mem_wr_64) && 
                                     binsof (length.one_dword);

         // Completions with Data to DUT for Send DMA transfers
         bins cpld_tlp_to_dut_ond_dword = binsof (tlp_dir.to_dut) && binsof (tlp_type.mem_cpld) && 
                                          binsof (length.one_dword);
         bins cpld_tlp_to_dut_small_pkt = binsof (tlp_dir.to_dut) && binsof (tlp_type.mem_cpld) && 
                                          binsof (length.small_pkt);
         bins cpld_tlp_to_dut_med_pkt   = binsof (tlp_dir.to_dut) && binsof (tlp_type.mem_cpld) && 
                                          binsof (length.med_pkt);
         bins cpld_tlp_to_dut_large_pkt = binsof (tlp_dir.to_dut) && binsof (tlp_type.mem_cpld) && 
                                          binsof (length.large_pkt);
         bins cpld_tlp_to_dut_max_size  = binsof (tlp_dir.to_dut) && binsof (tlp_type.mem_cpld) && 
                                          binsof (length.max_size);

         // Memory Reads from DUT for Send DMA transfers
         bins mem_rd_32_tlp_from_dut_one_dword = binsof (tlp_dir.from_dut) && binsof (tlp_type.mem_rd_32) && 
                                                 binsof (length.one_dword);
         bins mem_rd_32_tlp_from_dut_small_pkt = binsof (tlp_dir.from_dut) && binsof (tlp_type.mem_rd_32) && 
                                                 binsof (length.small_pkt);
         bins mem_rd_32_tlp_from_dut_med_pkt   = binsof (tlp_dir.from_dut) && binsof (tlp_type.mem_rd_32) && 
                                                 binsof (length.med_pkt);
         bins mem_rd_32_tlp_from_dut_large_pkt = binsof (tlp_dir.from_dut) && binsof (tlp_type.mem_rd_32) && 
                                                 binsof (length.large_pkt);
         bins mem_rd_32_tlp_from_dut_max_size  = binsof (tlp_dir.from_dut) && binsof (tlp_type.mem_rd_32) && 
                                                 binsof (length.max_size);
         bins mem_rd_64_tlp_from_dut_one_dword = binsof (tlp_dir.from_dut) && binsof (tlp_type.mem_rd_64) && 
                                                 binsof (length.one_dword);
         bins mem_rd_64_tlp_from_dut_small_pkt = binsof (tlp_dir.from_dut) && binsof (tlp_type.mem_rd_64) && 
                                                 binsof (length.small_pkt);
         bins mem_rd_64_tlp_from_dut_med_pkt   = binsof (tlp_dir.from_dut) && binsof (tlp_type.mem_rd_64) && 
                                                 binsof (length.med_pkt);
         bins mem_rd_64_tlp_from_dut_large_pkt = binsof (tlp_dir.from_dut) && binsof (tlp_type.mem_rd_64) && 
                                                 binsof (length.large_pkt);
         bins mem_rd_64_tlp_from_dut_max_size  = binsof (tlp_dir.from_dut) && binsof (tlp_type.mem_rd_64) && 
                                                 binsof (length.max_size);

         // Memory Writes from DUT for Recv DMA transfers
         bins mem_wr_32_tlp_from_dut_one_dword = binsof (tlp_dir.from_dut) && binsof (tlp_type.mem_wr_32) && 
                                                 binsof (length.one_dword);
         bins mem_wr_32_tlp_from_dut_small_pkt = binsof (tlp_dir.from_dut) && binsof (tlp_type.mem_wr_32) && 
                                                 binsof (length.small_pkt);
         bins mem_wr_32_tlp_from_dut_med_pkt   = binsof (tlp_dir.from_dut) && binsof (tlp_type.mem_wr_32) && 
                                                 binsof (length.med_pkt);
         bins mem_wr_32_tlp_from_dut_large_pkt = binsof (tlp_dir.from_dut) && binsof (tlp_type.mem_wr_32) && 
                                                 binsof (length.large_pkt);
         bins mem_wr_32_tlp_from_dut_max_size  = binsof (tlp_dir.from_dut) && binsof (tlp_type.mem_wr_32) && 
                                                 binsof (length.max_size);
         bins mem_wr_64_tlp_from_dut_one_dword = binsof (tlp_dir.from_dut) && binsof (tlp_type.mem_wr_64) && 
                                                 binsof (length.one_dword);
         bins mem_wr_64_tlp_from_dut_small_pkt = binsof (tlp_dir.from_dut) && binsof (tlp_type.mem_wr_64) && 
                                                 binsof (length.small_pkt);
         bins mem_wr_64_tlp_from_dut_med_pkt   = binsof (tlp_dir.from_dut) && binsof (tlp_type.mem_wr_64) && 
                                                 binsof (length.med_pkt);
         bins mem_wr_64_tlp_from_dut_large_pkt = binsof (tlp_dir.from_dut) && binsof (tlp_type.mem_wr_64) && 
                                                 binsof (length.large_pkt);
         bins mem_wr_64_tlp_from_dut_max_size  = binsof (tlp_dir.from_dut) && binsof (tlp_type.mem_wr_64) && 
                                                 binsof (length.max_size);

         // Completions from DUT for Config accesses
         bins cpl_tlp_from_dut = binsof (tlp_dir.from_dut) && binsof (tlp_type.mem_cpl);

         // Completions with Data from DUT for Memory Read Accesses (DMA CSRs)
         bins cpld_tlp_from_dut = binsof (tlp_dir.from_dut) && binsof (tlp_type.mem_cpld) && 
                                  binsof (length.one_dword);

         // Cross coverage bins to ignore.
         // DUT will not generate Config TLPs. Ignore those
         ignore_bins cfg_tlp_from_dut = binsof (tlp_dir.from_dut) &&
                                        (binsof (tlp_type.cfg_rd) || binsof (tlp_type.cfg_wr)); 

         // BFM will not generate Completions without Data. Ignore those
         ignore_bins cpl_tlp_to_dut = binsof (tlp_dir.to_dut) && binsof (tlp_type.mem_cpl);

         // DUT will only generate CplDs for CSR accesses. So, they will always be one dword.
         // Ignore the rest
         ignore_bins cpld_tlp_from_dut_not_one_dword = binsof (tlp_dir.from_dut) && binsof (tlp_type.mem_cpld) &&
                                                       !binsof (length.one_dword);

         // BFM will only generate larger than one dword packets for CplDs, Ignore the rest.
         ignore_bins non_cpld_tlp_to_dut_not_one_dword = binsof (tlp_dir.to_dut) && !binsof (tlp_type.mem_cpld) &&
                                                         !binsof (length.one_dword);
      }
   endgroup : pcie_cg

   covergroup pcie_dma_cg;
      // option.per_instance = 1;
      pcie_payload_8dw  : coverpoint pcie_cfg.pcie_payload_8dw
      {
         bins max_length_gt_8dw = {'h0};
         bins max_length_eq_8dw = {'h1};
      }
      pcie_ctrl0_init  : coverpoint pcie_cfg.pcie_ctrl0_init
      {
         bins enabled  = {'h1};
         bins disabled = {'h0};
      }
      pcie_ctrl0_target  : coverpoint pcie_cfg.pcie_ctrl0_target
      {
         bins enabled  = {'h1};
         bins disabled = {'h0};
      }
      pcie_ctrl0_tag  : coverpoint pcie_cfg.pcie_ctrl0_tag
      {
         bins enabled  = {'h1};
         bins disabled = {'h0};
      }
   endgroup : pcie_dma_cg

   covergroup pcie_reset_cg;
      pcie_mid_test_reset : coverpoint pcie_cfg.pcie_mid_test_reset
      {
         bins power_on_reset           = {POWER_ON_RESET};
         bins pcie_cold_reset          = {PCIE_COLD_RESET};
         bins pcie_hot_reset           = {PCIE_HOT_RESET};
         bins dmaq_soft_reset_by_risc  = {DMAQ_SOFT_RESET_BY_RISC};
         bins dmaq_soft_reset_by_pcie  = {DMAQ_SOFT_RESET_BY_PCIE};
      }
   endgroup : pcie_reset_cg

   virtual function void configure();
      super.configure();
      pcie_dma_cg.sample();
      pcie_reset_cg.sample();
   endfunction : configure

   function new (string name, uvm_component parent);
      super.new(name, parent);
      pcie_cov_port = new("pcie_cov_port", this);
      pcie_cg = new();
      pcie_cg.set_inst_name({get_full_name(), ".pcie_cg"});
      pcie_dma_cg = new();
      pcie_dma_cg.set_inst_name({get_full_name(), ".pcie_dma_cg"});
      pcie_reset_cg = new();
      pcie_reset_cg.set_inst_name({get_full_name(), ".pcie_reset_cg"});
   endfunction : new

   virtual function void write(uvm_sequence_item pkt);
      DenaliSvPcie::denaliPciePacket       pcie_pkt;
      // DenaliSvPcie::denaliPcieTrainingSet  pcie_training_set;

      if (coverage_enable) begin
         if ($cast(pcie_pkt, pkt)) begin
            // Only track TLPs
            if ($cast(pcie_tlp_pkt, pcie_pkt)) begin
               // TLPs generated by BFM (received by DUT)
               if (pcie_tlp_pkt.cbReason == DenaliSvPcie::PCIE_CB_TL_user_queue_exit)
                  tlp_dir = TO_DUT;
               // TLPs received by the BFM (generated by DUT)
               else if (pcie_tlp_pkt.cbReason == DenaliSvPcie::PCIE_CB_TL_RX_packet)
                  tlp_dir = FROM_DUT;
               pcie_cg.sample();
            end
            /* else if ($cast(pcie_training_set, pcie_pkt)) begin
               if (pcie_training_set.cbReason == PCIE_CB_PL_TX_end_packet &&
                   (pcie_training_set.osType == DenaliSvPcie::DENALI_PCIE_PL__ts1_set ||
                    pcie_training_set.osType == DenaliSvPcie::DENALI_PCIE_PL__ts2_set) &&
                   pcie_training_set.trainingControl.size() > 0) begin
                  if (pcie_training_set.trainingControl[0] == 'h1)
                     pcie_hot_reset = 1;
                  else
                     pcie_hot_reset = 0;
                  pcie_reset_cg.sample();
               end
            end */
         end
         else
            `uvm_fatal("mby_cp_pcie_coverage", $sformatf("\nReceived Unknown Packet Type"));
      end
   endfunction : write
endclass


`endif /* __MBY_CP_PCIE_COVERAGE_SV__ */
