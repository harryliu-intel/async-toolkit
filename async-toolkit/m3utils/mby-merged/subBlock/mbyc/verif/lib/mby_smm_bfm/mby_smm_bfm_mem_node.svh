//-----------------------------------------------------------------------------
// Title         : Madison Bay Mesh Memory Node Class
// Project       : Madison Bay
//-----------------------------------------------------------------------------
// File          : mby_smm_bfm_mem_node.svh
// Author        : Roman Bernal <r.bernal@intel.com>
// Created       : 01.11.2018
// Last modified : 01.11.2018
//-----------------------------------------------------------------------------
// Description :
// This is the Mesh BFM Memory Node Class for Madison Bay
//-----------------------------------------------------------------------------
// Copyright (c) 2018 by Intel Corporation This model is the confidential and
// proprietary property of Intel Corporation and the possession or use of this
// file requires a written license from Intel Corporation.
//------------------------------------------------------------------------------
// Modification history :
// 01.11.2018 : created
//-----------------------------------------------------------------------------
`ifndef __MBY_SMM_BFM_MEM_NODE__
`define __MBY_SMM_BFM_MEM_NODE__

//-----------------------------------------------------------------------------
// CLASS: mby_smm_bfm_mem_node
//
// This is a parameterized class which declares an associative-dynamic 1MB MEMORY node.
// MEM is 64B addressable (i.e., needs 14-bit for addressing). 
//
// PARAMETERS::
//     int ADDR_WIDTH - 14-bit Address Width
//     int DATA_WIDTH - 64 Bytes Data Width
//-----------------------------------------------------------------------------
class mby_smm_bfm_mem_node 
#(
   int ADDR_WIDTH = SMM_BFM_ADDR_WIDTH, 
   int DATA_WIDTH = SMM_BFM_DATA_WIDTH 
) extends uvm_component;
   
   `uvm_component_utils(mby_smm_bfm_mem_node#(ADDR_WIDTH, DATA_WIDTH))
   
   // VARIABLE: MEMORY_DEPTH
   //    TODO : what is this? Not used anywhere else
   localparam MEMORY_DEPTH = 1 << ADDR_WIDTH;
   
   // VARIABLE: MEM
   //    Associative array representing the memory of the SMM node. Assignations require the memory
   //    address as key, which points to the data stored in it.
   reg [DATA_WIDTH-1:0] MEM [logic];
   
   // -------------------------------------------------------------------------
   // CONSTRUCTOR: new
   //
   // Constructor
   //
   // ARGUMENTS:
   //    string name          - An identifier for this configuration object.
   //    uvm_component parent - The translator's parent component.
   // -------------------------------------------------------------------------
   function new (string name = "mby_smm_bfm_mem_node", uvm_component parent);
      super.new(name,parent);
   endfunction : new

   // -------------------------------------------------------------------------
   // FUNCTION: mwr()
   //
   // Issues memory writes made to this SMM node.
   //
   // ARGUMENTS:
   //     logic [ADDR_WIDTH-1:0] address - Location in Memory where Data will be stored.
   //     logic [ADDR_WIDTH-1:0] wr_data - Data being stored in Memory.
   // -------------------------------------------------------------------------
   function mwr(logic [ADDR_WIDTH-1:0] address, logic [DATA_WIDTH-1:0] wr_data);
      string msg_str;
      
      //TODO : Look for Mrd Req in Cache. If there, then service request otherwise do Data MWr into MEM 
      if(!MEM.exists(address)) begin
         MEM[address] = wr_data;
         
         msg_str = $sformatf("mwr(): SMM BFM node memory write issued at Address = 0x%04x, Data = 0x%0128x", address, wr_data);
      end else begin
         // TODO : Should we worry about this?
         msg_str = $sformatf("mwr(): Attempted to write SMM BFM node Address = 0x%04x with Data = 0x%0128x, ", address, wr_data);
         msg_str = {msg_str, "but the write wasn't completed due to memory address already contains data."};
      end
      
      `uvm_info(get_type_name(), msg_str, UVM_DEBUG)
   endfunction : mwr

   // -------------------------------------------------------------------------
   // FUNCTION: mrd()
   //
   // Returns memory reads made to this SMM node.
   //
   // ARGUMENTS:
   //    logic [ADDR_WIDTH-1:0] address  - Location in Memory where Data will be read..
   //
   // RETURNS:
   //    logic [DATA_WIDTH-1:0]          - Data read from Memory.
   // -------------------------------------------------------------------------
   function logic [DATA_WIDTH-1:0] mrd(logic [ADDR_WIDTH-1:0] address);
      string msg_str;
      logic    [DATA_WIDTH-1:0] rd_data;
      
      if(MEM.exists(address)) begin
         rd_data = MEM[address];
         MEM.delete(address);
         
         msg_str = $sformatf("mrd(): SMM BFM node memory read at Address = 0x%04x, Data = 0x%0128x", address, rd_data);
      end else begin
         // TODO : Should we worry about this?
         msg_str = $sformatf("mrd(): Attempted to read SMM BFM node memory Address = 0x%04x, ", address, rd_data);
         msg_str = {msg_str, "but that memory location hasn't been set."};
      end
      //TODO: If Data Write has not arrived, store MRd Req in Cache - Not yet defined.
      `uvm_info(get_type_name(), msg_str, UVM_DEBUG)
      
      return rd_data;
   endfunction : mrd
endclass : mby_smm_bfm_mem_node

`endif
