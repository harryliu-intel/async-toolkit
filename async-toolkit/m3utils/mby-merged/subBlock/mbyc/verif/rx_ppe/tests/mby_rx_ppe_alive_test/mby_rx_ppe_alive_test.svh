// Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
// SPDX-License-Identifier: Apache-2.0

// vim: noai : ts=3 : sw=3 : expandtab : ft=systemverilog

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
//   Author        : Akshay Kotian
//   Project       : Madison Bay
//------------------------------------------------------------------------------

//   Class:    mby_rx_ppe_alive_test
//

`ifndef MBY_RX_PPE_ALIVE_TEST__SV
`define MBY_RX_PPE_ALIVE_TEST__SV

`ifndef __INSIDE_MBY_RX_PPE_TEST_LIB
`error "Attempt to include file outside of mby_rx_ppe_test_lib."
`endif

typedef class mby_rx_ppe_alive_seq;
class mby_rx_ppe_alive_test extends mby_rx_ppe_base_test;

   `uvm_component_utils(mby_rx_ppe_alive_test)
   //------------------------------------------------------------------------------
   // Constructor: new
   //  Arguments:
   //  name   - rx_ppe alive test object name.
   //  parent - Component parent object.
   //------------------------------------------------------------------------------
   function new (string name="mby_rx_ppe_alive_test", uvm_component parent=null);
      super.new (name, parent);
   endfunction :  new

   //------------------------------------------------------------------------------
   // Function: build_phase
   //  Arguments:
   //  phase - uvm_phase object.
   //------------------------------------------------------------------------------
   function void build_phase(uvm_phase phase);
      super.build_phase(phase);
   endfunction : build_phase

   //------------------------------------------------------------------------------
   // Function: connect_phase
   // Sets USER_DATA_PHASE sequence.
   //
   //  Arguments:
   //  phase - uvm_phase object.
   //------------------------------------------------------------------------------
   function void connect_phase(uvm_phase phase);
      super.connect_phase(phase);
      set_default_sequences();

   endfunction : connect_phase
   
   //---------------------------------------------------------------------------
   // Function: set_default_sequences()
   //---------------------------------------------------------------------------
   function void set_default_sequences();
      super.set_default_sequences();
      `uvm_info("::set_default_sequences", "Setting phase sequences", UVM_NONE)

      // Specifying main phase sequence
      uvm_config_db#(uvm_object_wrapper)::set(this,
         "env.tb_seqr.main_phase",
         "default_sequence",
         mby_rx_ppe_alive_seq::type_id::get());
   endfunction : set_default_sequences   

endclass : mby_rx_ppe_alive_test

class mby_rx_ppe_alive_seq extends mby_rx_ppe_seq_lib::mby_rx_ppe_env_base_seq;

   `uvm_object_utils(mby_rx_ppe_alive_seq)

   //------------------------------------------------------------------------------
   //  Constructor: new
   //  Arguments:
   //  name   - rx_ppe alive test  seq object name.
   //------------------------------------------------------------------------------
   function new (string name="mby_rx_ppe_alive_seq");
      super.new (name);
   endfunction :  new

   //------------------------------------------------------------------------------
   //  Task:  body
   //  Count 20 clocks.
   //------------------------------------------------------------------------------
   virtual task body();
      int count = 0;
   
      `uvm_info(this.get_name(), ("Phase::main_phase:mby_rx_ppe_alive_seq::Starting"), UVM_LOW)
      repeat(20) begin
         @(posedge vif.fab_clk);
         count++;
         `uvm_info(get_name(), $sformatf("mby_rx_ppe_alive_seq: clock edge %0d",count), UVM_LOW);
      end

/*{           
      addr = 'h20000;
      wr_data = 'haaaa;
      wm_reg_write(addr,wr_data);
      wm_reg_read(addr,rd_data);
      if (rd_data != wr_data) begin
         `uvm_error(get_name(),$sformatf("mby_rx_ppe_alive_seq: Parser Data Mismatch: addr=%0h data=%0h",addr, rd_data));
      end else begin
         `uvm_info(get_name(), $sformatf("mby_rx_ppe_alive_seq: Parser: addr=%0h data=%0h",addr, rd_data), UVM_LOW);      
      end
      
      addr = 'h2e808;
      wr_data = 'hbbbb;
      wm_reg_write(addr,wr_data);
      wm_reg_read(addr,rd_data);
      if (rd_data != wr_data) begin
         `uvm_error(get_name(),$sformatf("mby_rx_ppe_alive_seq: Parser Data Mismatch: addr=%0h data=%0h",addr, rd_data));
      end else begin
         `uvm_info(get_name(), $sformatf("mby_rx_ppe_alive_seq: Mapper: addr=%0h data=%0h",addr, rd_data), UVM_LOW);
      end
            
      addr = 'h6a000;
      wr_data = 'hcccc;
      wm_reg_write(addr,wr_data);
      wm_reg_read(addr,rd_data);
      if (rd_data != wr_data) begin
         `uvm_error(get_name(),$sformatf("mby_rx_ppe_alive_seq: Parser Data Mismatch: addr=%0h data=%0h",addr, rd_data));
      end else begin
         `uvm_info(get_name(), $sformatf("mby_rx_ppe_alive_seq: CGRPA: addr=%0h data=%0h",addr, rd_data), UVM_LOW); 
      end      
      
      addr = 'h387000;
      wr_data = 'hdddd;
      wm_reg_write(addr,wr_data);
      wm_reg_read(addr,rd_data);
      if (rd_data != wr_data) begin
         `uvm_error(get_name(),$sformatf("mby_rx_ppe_alive_seq: Parser Data Mismatch: addr=%0h data=%0h",addr, rd_data));
      end else begin
         `uvm_info(get_name(), $sformatf("mby_rx_ppe_alive_seq: CGRPB: addr=%0h data=%0h",addr, rd_data), UVM_LOW);  
      end      
      
      addr = 'h4b3800;
      wr_data = 'heeee;
      wm_reg_write(addr,wr_data);
      wm_reg_read(addr,rd_data);
      if (rd_data != wr_data) begin
         `uvm_error(get_name(),$sformatf("mby_rx_ppe_alive_seq: Parser Data Mismatch: addr=%0h data=%0h",addr, rd_data));
      end else begin
         `uvm_info(get_name(), $sformatf("mby_rx_ppe_alive_seq: Entropy: addr=%0h data=%0h",addr, rd_data), UVM_LOW);  
      end      
      
      addr = 'h628a08;
      wr_data = 'h2222;
      wm_reg_write(addr,wr_data);
      wm_reg_read(addr,rd_data);
      if (rd_data != wr_data) begin
         `uvm_error(get_name(),$sformatf("mby_rx_ppe_alive_seq: Parser Data Mismatch: addr=%0h data=%0h",addr, rd_data));
      end else begin
         `uvm_info(get_name(), $sformatf("mby_rx_ppe_alive_seq: Policers: addr=%0h data=%0h",addr, rd_data), UVM_LOW);        
      end
}*/
      
   endtask

endclass : mby_rx_ppe_alive_seq

`endif // MBY_RX_PPE_ALIVE_TEST__SV
