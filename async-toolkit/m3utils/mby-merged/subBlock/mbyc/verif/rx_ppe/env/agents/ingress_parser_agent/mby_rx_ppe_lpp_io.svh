// Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
// SPDX-License-Identifier: Apache-2.0

//------------------------------------------------------------------------------
//
// INTEL CONFIDENTIAL
//
// Copyright 2018 Intel Corporation All Rights Reserved.
//
// The source code contained or described herein and all documents related to
// the source code ("Material") are owned by Intel Corporation or its suppliers
// or licensors. Title to the Material remains with Intel Corporation or its
// suppliers and licensors. The Material contains trade secrets and proprietary
// and confidential information of Intel or its suppliers and licensors.  The
// Material is protected by worldwide copyright and trade secret laws and
// treaty provisions. No part of the Material may be used, copied, reproduced,
// modified, published, uploaded, posted, transmitted, distributed, or
// disclosed in any way without Intel's prior express written permission.
//
// No license under any patent, copyright, trade secret or other intellectual
// property right is granted to or conferred upon you by disclosure or delivery
// of the Materials, either expressly, by implication, inducement, estoppel or
// otherwise. Any license under such intellectual property rights must be
// express and approved by Intel in writing.
//
//------------------------------------------------------------------------------
//   Author        : Nathan Mai
//            
//   Project       : Madison Bay
//------------------------------------------------------------------------------

//`timescale 1ps/1fs

//`include "uvm_macros.svh"

//import uvm_pkg::*;
//import eth_bfm_pkg::*;

//------------------------------------------------------------------------------
// IO policy for LPP mode.  Create 2 for a complete interface.
// ASSUMPTIONS:
//    IGR aligns headers with the first byte in bits [7:0]
//    IGR delivers only 1 header per clock maximum
//------------------------------------------------------------------------------
class ppe_lpp_io extends cli_io;
   parameter MAX_DATA = 128;
   parameter TAILBUF_SIZE = 16384 / MAX_DATA;

   virtual igr_rx_ppe_if vintf;
   int intf_num;
   igr_rx_ppe_tail_t tailbuf[TAILBUF_SIZE];
   int tailptr = 0;

   `uvm_object_utils(ppe_lpp_io)

   function new(string name = "ppe_lpp_io");
      super.new(name);
      foreach (tailbuf[i])
         tailbuf[i] = 0;
   endfunction

   function void set_vintf(virtual igr_rx_ppe_if vintf);
      this.vintf = vintf;
   endfunction

   // In LPP mode, the interface is split into 2 interfaces.  We'll create 2 io
   // policies and give each one an unique number (0 or 1).  The io policy uses
   // this number to know which interface to drive/observe.
   virtual function void set_intf_num(int intf_num);
      intf_num = intf_num;
   endfunction

   virtual function int max_data();
      return MAX_DATA;
   endfunction

   virtual function int max_tcs();
      return 9;
   endfunction

   virtual function int max_ports();
      return 8;
   endfunction

   virtual task wait_for_clock(int idx = 0);
      @(vintf.clkb);
   endtask

   virtual function bit get_reset();
      return vintf.clkb.hreset;
   endfunction

   virtual function void set_atom(int bus_pos, int frame_pos, atom_t atom);
      igr_rx_ppe_head_t head;
      igr_rx_ppe_tail_t tail;
      case (intf_num)
         1:       vintf.clkb.intf1_head.data[bus_pos*8+:8] <= atom.data;
         default: vintf.clkb.intf0_head.data[bus_pos*8+:8] <= atom.data;
      endcase
      if (bus_pos == 0) begin
         head.md.cpp_md = 0;
         head.md.ts = atom.timestamp;
         head.md.tc = atom.tc;
         head.md.id = atom.id;
         case (intf_num)
            1: begin
               vintf.clkb.intf1_head.md.cpp_md <= head.md.cpp_md;
               vintf.clkb.intf1_head.md.ts <= head.md.ts;
               vintf.clkb.intf1_head.md.tc <= head.md.tc;
               vintf.clkb.intf1_head.md.id <= head.md.id;
               end
            default: begin
               vintf.clkb.intf0_head.md.cpp_md <= head.md.cpp_md;
               vintf.clkb.intf0_head.md.ts <= head.md.ts;
               vintf.clkb.intf0_head.md.tc <= head.md.tc;
               vintf.clkb.intf0_head.md.id <= head.md.id;
               end
         endcase
      end
      if (bus_pos == 0 && atom.kind == SOP) begin
         tail.err = 0;
         tail.len = atom.len;
         tail.id = atom.id;
         tail.valid = 1;
         if (atom.len <= MAX_DATA) begin
            // back up tailptr to the previous empty slot (because our tail buffer
            // is big enough, it's guaranteed to be empty)
            tailptr = (tailptr - 1) % MAX_DATA;
            tailbuf[tailptr] = tail;
         end
         else begin
            // save tail info for later, assuming it arrives when the real packet
            // tail would be, find an empty slot
            int ptr = (tailptr + ((atom.len - 1) / TAILBUF_SIZE)) % TAILBUF_SIZE;
            while (tailbuf[ptr].valid) // find nearest unused slot
               ptr = (ptr + 1) % TAILBUF_SIZE;
            tailbuf[ptr] = tail;
         end
      end
      // drive tail info
      if (bus_pos == 0) begin
         tail = tailbuf[tailptr];
         tailbuf[tailptr] = 0;
         tailptr = (tailptr + 1) % TAILBUF_SIZE;
         case (intf_num)
            1:       vintf.clkb.intf1_tail <= tail;
            default: vintf.clkb.intf0_tail <= tail;
         endcase
      end
   endfunction

   virtual function atom_t get_atom(int bus_pos, int frame_pos);
      igr_rx_ppe_head_t head;
      igr_rx_ppe_tail_t tail;
      atom_t atom;
      case (intf_num)
         1: begin
            head = vintf.clkb.intf1_head;
            tail = vintf.clkb.intf1_tail;
            end
         default: begin
            head = vintf.clkb.intf0_head;
            tail = vintf.clkb.intf0_tail;
            end
      endcase

      atom.id = head.md.id;
      atom.data = head.data[bus_pos*8+:8];
      if (head.valid) begin
         if (bus_pos == 0) begin
            atom.kind = SOP;
            atom.eoh = 0;
         end
         else if (tail.valid && (tail.id == head.md.id) && (bus_pos == (tail.len - 1))) begin
            atom.kind = EOP;
            atom.eoh = 1;
         end
         else if ((tail.valid && (tail.id == head.md.id) && (bus_pos < (tail.len - 1))) ||
                  (!(tail.valid && (tail.id == head.md.id)))) begin
            atom.kind = MID;
            atom.eoh = (bus_pos == MAX_DATA - 1);
         end
         else
            atom.kind = IDLE;
      end
      return atom;
   endfunction

   virtual function int get_terminate_len(int id);
      igr_rx_ppe_tail_t tail;
      case (intf_num)
         1:       tail = vintf.clkb.intf1_tail;
         default: tail = vintf.clkb.intf0_tail;
      endcase
      if (tail.valid && tail.id == id)
         return tail.len;
      return 0;
   endfunction

   virtual function void set_valid_port(int port);
      igr_rx_ppe_head_t head;
      if (port < 0) begin
         head.valid = 0;
         head.md.port = 0;
      end
      else begin
         head.valid = 1;
         head.md.port = port;
      end
      case (intf_num)
         1: begin
            vintf.clkb.intf1_head.md.port <= head.md.port;
            vintf.clkb.intf1_head.valid <= head.valid;
            end
         default: begin
            vintf.clkb.intf0_head.md.port <= head.md.port;
            vintf.clkb.intf0_head.valid <= head.valid;
            end
      endcase
   endfunction

   virtual function int get_valid_port();
      igr_rx_ppe_head_t head;
      int port = -1;
      case (intf_num)
         1:       head = vintf.clkb.intf1_head;
         default: head = vintf.clkb.intf0_head;
      endcase
      if (head.valid)
         port = head.md.port;
      return port;
   endfunction

   virtual function void set_credit_port(int port);
      logic ack = (port >= 0);
      case (intf_num)
         1:       vintf.clkb.intf1_ack <= ack;
         default: vintf.clkb.intf0_ack <= ack;
      endcase
   endfunction

   virtual function int get_credit_port();
      int port = -1;
      logic ack;
      case (intf_num)
         1:       ack = vintf.clkb.intf1_ack;
         default: ack = vintf.clkb.intf0_ack;
      endcase
      if (ack === 1)
         port = 0;
      return port;
   endfunction

   virtual function void set_extensions_sop(int bus_pos, eth_frame frame);
      // set extra_data here
   endfunction

   virtual function void get_extensions_sop(int bus_pos, eth_frame frame);
      // get extra_data here
   endfunction

   virtual function void set_extensions_eop(int bus_pos, eth_frame frame);
   endfunction

   virtual function void get_extensions_eop(int bus_pos, eth_frame frame);
   endfunction

endclass
