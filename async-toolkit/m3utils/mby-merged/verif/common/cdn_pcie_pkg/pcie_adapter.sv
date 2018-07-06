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

`ifndef __PCIE_ADAPTER_SV__
`define __PCIE_ADAPTER_SV__

class pcie_adapter extends uvm_reg_adapter;

   DenaliSvPcie::denaliPcieConfig srcConfig;
   DenaliSvPcie::denaliPcieConfig dstConfig;
   bit byte_swap_enable = 1;

   `uvm_object_utils(pcie_adapter)

   function new(string name = "pcie_adapter");
      super.new(name);
   endfunction : new

   virtual function reset_adapter();
      byte_swap_enable = 1;
   endfunction : reset_adapter

   virtual function uvm_sequence_item reg2bus (const ref uvm_reg_bus_op rw);
      bit[63:0] bar0_start_address;
      DenaliSvPcie::denaliPcieTlpMemPacket pkt = DenaliSvPcie::denaliPcieTlpMemPacket::type_id::create("pkt");

      bar0_start_address = dstConfig.mem64Bars[0].startAddress;
      pkt.pktType = DenaliSvPcie::DENALI_PCIE__Tlp;
      pkt.tlpType = (rw.kind == UVM_READ && bar0_start_address >= 'h1_0000_0000) ? DenaliSvPcie::DENALI_PCIE_TL_MRd_64 :
                    (rw.kind == UVM_READ && bar0_start_address < 'h1_0000_0000) ? DenaliSvPcie::DENALI_PCIE_TL_MRd_32 : 
                    (rw.kind == UVM_WRITE && bar0_start_address >= 'h1_0000_0000) ? DenaliSvPcie::DENALI_PCIE_TL_MWr_64 :
                    DenaliSvPcie::DENALI_PCIE_TL_MWr_32;
      pkt.easyModeTlpConstraint.constraint_mode(1);
      pkt.srcConfig = srcConfig;
      pkt.dstConfig = dstConfig;

      pkt.randomize() with {
      length == 1;
      firstBe == 'hf;
      //What should this be?
      fullAddress == bar0_start_address + rw.addr; };

      if (rw.kind == UVM_WRITE) begin
         if (byte_swap_enable) begin
            pkt.payload[3] = rw.data[7:0];
            pkt.payload[2] = rw.data[15:8];
            pkt.payload[1] = rw.data[23:16];
            pkt.payload[0] = rw.data[31:24];
         end
         else begin
            pkt.payload[0] = rw.data[7:0];
            pkt.payload[1] = rw.data[15:8];
            pkt.payload[2] = rw.data[23:16];
            pkt.payload[3] = rw.data[31:24];
         end
      end
      return pkt;
   endfunction : reg2bus

   virtual function void bus2reg (uvm_sequence_item bus_item, 
                                  ref uvm_reg_bus_op rw);
      DenaliSvPcie::denaliPcieTlpMemPacket tlp_mem_pkt;
      DenaliSvPcie::denaliPcieTlpCplPacket tlp_cpl_pkt;
      // Responses for Write requests are clones of the request
      if ($cast(tlp_mem_pkt, bus_item)) begin
         rw.kind = (tlp_mem_pkt.tlpType == DenaliSvPcie::DENALI_PCIE_TL_MRd_32 ||
                    tlp_mem_pkt.tlpType == DenaliSvPcie::DENALI_PCIE_TL_MRd_64) ? UVM_READ : UVM_WRITE;
         // What should the address be?
         rw.addr = tlp_mem_pkt.fullAddress;
         if (byte_swap_enable)
            rw.data = {tlp_mem_pkt.payload[0], tlp_mem_pkt.payload[1], tlp_mem_pkt.payload[2], tlp_mem_pkt.payload[3]};
         else
            rw.data = {tlp_mem_pkt.payload[3], tlp_mem_pkt.payload[2], tlp_mem_pkt.payload[1], tlp_mem_pkt.payload[0]};
         rw.status = UVM_IS_OK;
      end
      // Responses for Read requests are Completion TLPs
      else if ($cast(tlp_cpl_pkt, bus_item)) begin
         rw.kind = UVM_READ;
         // What should the address be?
         rw.addr = tlp_cpl_pkt.lowAddress;
         if (byte_swap_enable)
            rw.data = {tlp_cpl_pkt.payload[0], tlp_cpl_pkt.payload[1], tlp_cpl_pkt.payload[2], tlp_cpl_pkt.payload[3]};
         else
            rw.data = {tlp_cpl_pkt.payload[3], tlp_cpl_pkt.payload[2], tlp_cpl_pkt.payload[1], tlp_cpl_pkt.payload[0]};
         rw.status = UVM_IS_OK;
      end
      else begin
         `uvm_fatal("cdn_pcie_pkg::pcie_adapter", 
                    "\nProvided bus_item is not of type denaliPcieTlpMemPacket or denaliPcieTlpCplPacket")
         return;
      end
   endfunction : bus2reg

endclass : pcie_adapter

`endif /* __PCIE_ADAPTER_SV__ */

// -------------------------------------------------------------------
//
