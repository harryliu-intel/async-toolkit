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
// PARAMETERS:
//     ADDR_WIDTH - 14-bit Address Width
//     DATA_WIDTH - 64 Bytes Data Width
//-----------------------------------------------------------------------------
class mby_smm_bfm_mem_node 
#(
   int ADDR_WIDTH = 14, 
   int DATA_WIDTH = 64*8 
) extends uvm_component;
   
   `uvm_component_utils(mby_smm_bfm_mem_node#(ADDR_WIDTH,DATA_WIDTH))
   
   // VARIABLE: MEMORY_DEPTH
   // 
   localparam MEMORY_DEPTH = 1 << ADDR_WIDTH;
     
   // Dynamic/Associative MEM Node indexed by logic (address)
   reg [DATA_WIDTH-1:0] MEM [logic];
   
   function new (string name = "mby_smm_bfm_mem_node", uvm_component parent);
      super.new(name,parent);
   endfunction : new

   // -------------------------------------------------------------------------
   // FUNCTION: mwr()
   //
   // This mem_node.mwr() function is targeted/called in mem_mesh class based on 
   // the Row & Col information coming in the request
   //
   // ARGUMENTS:
   //     address - Location in Memory where Data will be stored
   //     wr_data - Data being stored in Memory
   // -------------------------------------------------------------------------
   function mwr(logic [ADDR_WIDTH-1:0]address, logic [DATA_WIDTH-1:0] wr_data);
      `uvm_info("mby_smm_bfm_mem_node()::mwr(): Address=", $sformatf("%014h",address), UVM_DEBUG);
      `uvm_info("mby_smm_bfm_mem_node()::mwr(): wr_data=", $sformatf("%0128h",wr_data), UVM_DEBUG);

      //TODO: Look for Mrd Req in Cache. If there, then service request otherwise do Data MWr into MEM 
      if(!MEM.exists(address)) begin
         MEM[address] = wr_data;
      end
   endfunction : mwr


   function logic [DATA_WIDTH-1:0] mrd(logic [ADDR_WIDTH-1:0] address);
      logic [DATA_WIDTH-1:0] rd_data;
      `uvm_info("mby_smm_bfm_mem_node()::mrd(): Address=", $sformatf("%014h",address), UVM_DEBUG);
      if(MEM.exists(address)) begin
         rd_data = MEM[address];
         MEM.delete(address);
         `uvm_info("mby_smm_bfm_mem_node()::mrd(): rd_data=", $sformatf("%0128h",rd_data), UVM_DEBUG);
         return rd_data;
      end
      //TODO: If Data Write has not arrived, store MRd Req in Cache - Not yet defined.
   endfunction : mrd
      
endclass : mby_smm_bfm_mem_node

`endif
