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

`ifndef __PCIE_DRIVER_SV__
`define __PCIE_DRIVER_SV__

class pcie_driver extends cdnPcieUvm::cdnPcieUvmDriver;

   bit tr_exited_tl_user_queue;

   // Keep track of outstanding read requests
   DenaliSvPcie::denaliPcieTlpMemPacket tlp_mem_pkt_q[$];

   `uvm_component_utils_begin(pcie_driver)
      `uvm_field_int(tr_exited_tl_user_queue, UVM_ALL_ON)
   `uvm_component_utils_end

   uvm_analysis_imp #(DenaliSvPcie::denaliPciePacket, pcie_driver) TL_user_queue_exitImp;
   uvm_analysis_imp #(DenaliSvPcie::denaliPciePacket, pcie_driver) TL_transmit_queue_enterImp;
   uvm_analysis_imp #(DenaliSvPcie::denaliPciePacket, pcie_driver) TL_RX_packetImp;

   function new(string name = "pcie_driver", uvm_component parent = null);
      super.new(name, parent); 
   endfunction : new

   function void build();
      super.build();
      TL_user_queue_exitImp = new("TL_user_queue_exitImp", this); 
      TL_transmit_queue_enterImp = new("TL_transmit_queue_enterImp", this); 
      TL_RX_packetImp = new("TL_RX_packetImp", this); 
      tr_exited_tl_user_queue = 0;
      tlp_mem_pkt_q.delete();
   endfunction : build

   virtual function void connect();
      super.connect();
      pAgent.monitor.TL_user_queue_exitCbPort.connect(TL_user_queue_exitImp);
      pAgent.monitor.TL_transmit_queue_enterCbPort.connect(TL_transmit_queue_enterImp);
      pAgent.monitor.TL_RX_packetCbPort.connect(TL_RX_packetImp);
   endfunction : connect

   function void write(DenaliSvPcie::denaliPciePacket tr);

      DenaliSvPcie::denaliPcieTlpCplPacket tlp_cpl_pkt;
      DenaliSvPcie::denaliPcieTlpMemPacket tlp_mem_pkt;    
      int matching_idx[$];
       
      if (tr.cbReason == DenaliSvPcie::PCIE_CB_TL_user_queue_exit) begin
         tr_exited_tl_user_queue = 1;
      end
      else if (tr.cbReason == DenaliSvPcie::PCIE_CB_TL_transmit_queue_enter) begin
         // If it is a Memory Read, get the transactionIdTag. Since the
         // VIP adds the transactionIdTag, the value is known only at
         // TL_transmit_queue_enter time and not at TL_user_queue_exit
         // time.
         if ($cast(tlp_mem_pkt, tr))
            if (tlp_mem_pkt.tlpType == DenaliSvPcie::DENALI_PCIE_TL_MRd_32 ||
                tlp_mem_pkt.tlpType == DenaliSvPcie::DENALI_PCIE_TL_MRd_64) begin
               matching_idx = tlp_mem_pkt_q.find_first_index with (
                                 item.fullAddress == tlp_mem_pkt.fullAddress);
               if (!matching_idx.empty())
                  tlp_mem_pkt_q[matching_idx[0]].transactionIdTag = tlp_mem_pkt.transactionIdTag;
            end
      end
      else if (tr.cbReason == DenaliSvPcie::PCIE_CB_TL_RX_packet)
         // Ignore received TLPs if there are no
         // outstanding Memory Read Requests
         if (!tlp_mem_pkt_q.empty())
            // Check that the received TLP is a completion
            if ($cast(tlp_cpl_pkt, tr)) begin
               // Find the first read request that has the same tag and requester id
               // Ignoring lowAddress for now. I think it only matters when there are
               // multiple completions for a read request
               matching_idx = tlp_mem_pkt_q.find_first_index with (
                                 item.transactionIdTag == tlp_cpl_pkt.transactionIdTag &&
                                 item.requesterId.getBusNumber() == tlp_cpl_pkt.requesterId.getBusNumber() &&
                                 item.requesterId.getDeviceNumber() == tlp_cpl_pkt.requesterId.getDeviceNumber() &&
                                 item.requesterId.getFunctionNumber() == tlp_cpl_pkt.requesterId.getFunctionNumber()
                                 // && item.fullAddress[6:0] == tlp_cpl_pkt.lowAddress
                              );
               if (!matching_idx.empty()) begin
                  tlp_mem_pkt = tlp_mem_pkt_q[matching_idx[0]];
                  $cast(rsp, tlp_cpl_pkt.clone());
                  // This is important because set_id_info sets
                  // the sequence_id and transaction_id for the
                  // sequence_item. The sequence receiving the
                  // response will try to match the sequence_id
                  // to determine whether it should use the
                  // response.
                  rsp.set_id_info(tlp_mem_pkt);
                  seq_item_port.put_response(rsp);
                  tlp_mem_pkt_q.delete(matching_idx[0]);
                  matching_idx.delete();
               end
            end
   endfunction : write

   virtual task waitForTransactionEnd (DenaliSvPcie::denaliPciePacket tr);

      DenaliSvPcie::denaliPcieTlpMemPacket tlp_mem_pkt;
       
      //`uvm_info("pcie_driver", $sformatf("\nInside waitForTransactionEnd for denaliPciePacket:\n%s", tr.sprintInfo()), UVM_HIGH);
      wait (tr_exited_tl_user_queue == 1);
      tr_exited_tl_user_queue = 0;
      if ($cast(tlp_mem_pkt, tr)) begin
         // Writes are posted, so send a response back to
         // the sequence right away
         if (tlp_mem_pkt.tlpType == DenaliSvPcie::DENALI_PCIE_TL_MWr_32 ||
             tlp_mem_pkt.tlpType == DenaliSvPcie::DENALI_PCIE_TL_MWr_64) begin
            $cast(rsp, tlp_mem_pkt.clone());
            rsp.set_id_info(tlp_mem_pkt);
            seq_item_port.put_response(rsp);
         end

         // Reads are non-posted. Do item_done(), but don't
         // send a response. This will block the sequence,
         // but not block other sequences from using the driver
         // When a completion is received for the read request,
         // send a response to the sequence so that it can
         // proceed. Store read request until completion is
         // received. Need to match completion with read request.
         // Note: Push onto queue here instead of at TL_user_queue_exit,
         // because we have sequence_id and transaction_id here,
         // and we need it for the response
         else if (tlp_mem_pkt.tlpType == DenaliSvPcie::DENALI_PCIE_TL_MRd_32 ||
                  tlp_mem_pkt.tlpType == DenaliSvPcie::DENALI_PCIE_TL_MRd_64)
            tlp_mem_pkt_q.push_back(tlp_mem_pkt);
      end
   endtask : waitForTransactionEnd

  
endclass : pcie_driver

`endif /* __PCIE_DRIVER_SV__ */

// -------------------------------------------------------------------
//
