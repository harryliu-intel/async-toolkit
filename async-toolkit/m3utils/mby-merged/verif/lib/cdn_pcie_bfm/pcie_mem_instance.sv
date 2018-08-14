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

`ifndef __PCIE_MEM_INSTANCE_SV__
`define __PCIE_MEM_INSTANCE_SV__

typedef class pcie_agent;
   
class pcie_mem_instance extends cdnPcieUvm::cdnPcieUvmMemInstance;

   uvm_event      dev_active_ev;
   uvm_event      dev_inactive_ev;
   uvm_event_pool pool;
      
   `uvm_component_utils(pcie_mem_instance)

   function new(string name, uvm_component parent);
      super.new(name, parent);
      dev_active_ev = new("Device_Active");
      dev_active_ev.reset();
      dev_inactive_ev = new("Device_Active");
      dev_inactive_ev.reset();
   endfunction : new

   virtual task run();
      super.run();
      void'(this.setCallback(DenaliSvMem::DENALI_CB_Write));
      void'(this.setCallback(DenaliSvMem::DENALI_CB_Read));
   endtask : run

   virtual function void writeReg(DenaliSvPcie::denaliPcieRegNumT addr, reg [31:0] data);
     `uvm_info(get_type_name(), $sformatf("pcie_mem_instance::writeReg - ADDR:%x, DATA:%x",addr, data), UVM_HIGH);
      super.writeReg(addr, data);
   endfunction : writeReg

   virtual function reg [31:0] readReg(DenaliSvPcie::denaliPcieRegNumT addr);
      readReg = super.readReg(addr);
     `uvm_info(get_type_name(), $sformatf("pcie_mem_instance::readReg - ADDR:%x, DATA:%x",addr,readReg), UVM_HIGH);
   endfunction : readReg
   
   task wait_device_active();
      wait_device_state(1);
   endtask

   task wait_device_inactive();
      wait_device_state(0);
   endtask

   task wait_device_state(input bit active_state = 1);
      `uvm_info(get_type_name(),
                $sformatf("Waiting for RC BFM to reach Device %0s State ...",
                          active_state ? "Active" : "Inactive"), UVM_LOW);
      if (active_state)
         dev_active_ev.wait_trigger();
      else
         dev_inactive_ev.wait_trigger();

      `uvm_info(get_type_name(),
                $sformatf("RC BFM reached Device %0s State!",
                          active_state ? "Active" : "Inactive"), UVM_LOW);
   endtask : wait_device_state

   virtual function void build();
      super.build();
      pool = new();
      pool = pool.get_global_pool();
   endfunction : build

   virtual function void connect();
      super.connect();
   endfunction : connect
   
   // For every memory write callback, if the PCIe Device
   // State changes, trigger an event in the global event
   // pool.
   virtual function int WriteCbF(ref DenaliSvMem::denaliMemTransaction trans);
      DenaliSvPcie::denaliPcieDeviceStateT st;
      DenaliSvPcie::denaliPcieLtssmStateT  ltssm_state;
      uvm_event              ev;
      reg [7:0]              data[];
      reg [15:0]             state;

      if (trans.Address == DenaliSvPcie::PCIE_REG_DEN_DEV_ST) begin
         // for (int i = 0; i < PCIE_DEVICE_STATE_COUNT; i++) begin
         //    st = DenaliSvPcie::denaliPcieDeviceStateT'(i);
         //    ev = this.pool.get(st.name());
         //    ev.reset();
         // end

         // st = DenaliSvPcie::denaliPcieDeviceStateT'(trans.Data[3][3:0]);
         if (!$cast(st, trans.Data[3][3:0]))
            `uvm_fatal("pcie_mem_instance","Can't cast device state");
         ev = this.pool.get(st.name());
         ev.trigger();

         // Detect transition to Device Active state
         if(st == DenaliSvPcie::PCIE_DEVICE_STATE_Active)
            this.dev_active_ev.trigger();
         else
            this.dev_inactive_ev.trigger();
      end
      else if (trans.Address == DenaliSvPcie::PCIE_REG_DEN_LTSSM_STATE) begin
         if (!$cast(ltssm_state, {trans.Data[2], trans.Data[3]}))
            `uvm_fatal("pcie_mem_instance","Can't cast device state");
         //`uvm_info(get_full_name(), $sformatf("WriteCbF: LTSSM state is %0s", ltssm_state), UVM_HIGH);
      end
      return super.WriteCbF(trans);
   endfunction : WriteCbF

   virtual function int ReadCbF(ref DenaliSvMem::denaliMemTransaction trans);
      DenaliSvPcie::denaliPcieDeviceStateT st;
      DenaliSvPcie::denaliPcieLtssmStateT  ltssm_state;
      uvm_event              ev;
      reg [7:0]              data[];
      reg [15:0]             state;

      if (trans.Address == DenaliSvPcie::PCIE_REG_DEN_DEV_ST) begin
         if (!$cast(st, trans.Data[3][3:0]))
            `uvm_fatal("pcie_mem_instance","Can't cast device state");

         // Detect transition to Device Active state
         if(st == DenaliSvPcie::PCIE_DEVICE_STATE_Active)
            this.dev_active_ev.trigger();
         else
            this.dev_inactive_ev.trigger();
      end
      else if (trans.Address == DenaliSvPcie::PCIE_REG_DEN_LTSSM_STATE) begin
         if (!$cast(ltssm_state, {trans.Data[2], trans.Data[3]}))
            `uvm_fatal("pcie_mem_instance","Can't cast device state");
         `uvm_info(get_full_name(), $sformatf("ReadCbF: LTSSM state is %0s", ltssm_state), UVM_HIGH);
      end
      return super.ReadCbF(trans);
   endfunction : ReadCbF
  
endclass : pcie_mem_instance

`endif /* __PCIE_MEM_INSTANCE_SV__ */

// -------------------------------------------------------------------
//
