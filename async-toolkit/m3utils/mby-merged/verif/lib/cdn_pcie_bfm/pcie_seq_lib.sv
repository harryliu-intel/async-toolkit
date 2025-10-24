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

`ifndef __PCIE_SEQ_LIB_SV__
`define __PCIE_SEQ_LIB_SV__

class pcie_sequence extends uvm_sequence #(DenaliSvPcie::denaliPciePacket);
   pcie_agent         rc_bfm_agent;
   pcie_sequencer     pcie_seqr;
   pcie_mem_instance  pcie_mem_inst;

   `uvm_object_utils_begin(pcie_sequence)
      `uvm_field_object(rc_bfm_agent, UVM_ALL_ON)
      `uvm_field_object(pcie_seqr, UVM_ALL_ON)
      `uvm_field_object(pcie_mem_inst, UVM_ALL_ON)
   `uvm_object_utils_end
   `uvm_declare_p_sequencer(pcie_sequencer)

   function new(string name = "pcie_sequence");
      super.new(name);
   endfunction : new

   virtual task pre_body();
      if (!$cast(pcie_seqr, get_sequencer())) 
         `uvm_fatal("pcie_sequence", "pcie_seqr is of wrong type");
      if (!$cast(rc_bfm_agent, pcie_seqr.pEnv.rc_bfm)) 
         `uvm_fatal("pcie_sequence", "rc_bfm_agent is of wrong type");
      if (!$cast(pcie_mem_inst, rc_bfm_agent.regInst))
         `uvm_fatal("pcie_sequence", "Failed to cast pcie_mem_inst");
      //if (starting_phase != null) 
      //   starting_phase.raise_objection(this,"Sequence started...");
   endtask : pre_body

   task wait_for_RC_active();
      reg [31:0] device_state;

      if (!$cast(pcie_seqr, get_sequencer())) 
         `uvm_fatal("pcie_sequence","pcie_seqr is of wrong type");
      if (!$cast(rc_bfm_agent, pcie_seqr.pAgent) ) 
         `uvm_fatal("pcie_sequence", "rc_bfm_agent is of wrong type");
      if (!$cast(pcie_mem_inst, rc_bfm_agent.regInst))
         `uvm_fatal("pcie_sequence", "Failed to cast pcie_mem_inst");
      device_state = pcie_mem_inst.readReg(DenaliSvPcie::PCIE_REG_DEN_DEV_ST);

      if ( DenaliSvPcie::denaliPcieDeviceStateT'(device_state[3:0]) == DenaliSvPcie::PCIE_DEVICE_STATE_Active) begin
         //AK:7/18/17 -- When "Wait for SPICO code download" is enabled, RC
         //BFM will already be ACTIVE. Skipping the wait for it to go Inactive   
          `uvm_info("pcie_sequence", "Device is already Active. skipping the wait", UVM_LOW);
         //pcie_mem_inst.wait_device_inactive();
         device_state = pcie_mem_inst.readReg(DenaliSvPcie::PCIE_REG_DEN_DEV_ST);
      end

      if ( DenaliSvPcie::denaliPcieDeviceStateT'(device_state[3:0]) != DenaliSvPcie::PCIE_DEVICE_STATE_Active) begin
         `uvm_info("pcie_sequence", "Wait for RC Active State", UVM_LOW);
         pcie_mem_inst.wait_device_active();
      end
   endtask : wait_for_RC_active

   virtual task post_body();
      //if (starting_phase != null) 
      //   starting_phase.drop_objection(this,"Sequence finished!");
   endtask : post_body

endclass : pcie_sequence

// This implements a TLP sequence.
class pcie_TLReadAfterWriteSeq extends pcie_sequence;

   DenaliSvPcie::denaliPcieTlpMemPacket rdTlp;
   DenaliSvPcie::denaliPcieTlpMemPacket wrTlp;

   `uvm_object_utils_begin(pcie_TLReadAfterWriteSeq)
      `uvm_field_object(wrTlp, UVM_ALL_ON)
      `uvm_field_object(rdTlp, UVM_ALL_ON)
   `uvm_object_utils_end
  
   `uvm_declare_p_sequencer(pcie_sequencer)

   function new(string name = "pcie_TLReadAfterWriteSeq");
      super.new(name);
   endfunction : new

   // Set the source and destination configuration spaces, and the packet
   // and TLP types. Also enable the Easy Mode constraint.
   virtual task pre_do(bit is_item);
      pcie_sequencer pcie_seqr;

      if (rdTlp) begin
         rdTlp.pktType = DenaliSvPcie::DENALI_PCIE__Tlp;
         rdTlp.tlpType = DenaliSvPcie::DENALI_PCIE_TL_MRd_32;
         rdTlp.easyModeTlpConstraint.constraint_mode(1);
         if ($cast(pcie_seqr,get_sequencer())) begin
            rdTlp.srcConfig = pcie_seqr.srcConfig;
            rdTlp.dstConfig = pcie_seqr.dstConfig;
         end
      end else if (wrTlp) begin
         wrTlp.pktType = DenaliSvPcie::DENALI_PCIE__Tlp;
         wrTlp.tlpType = DenaliSvPcie::DENALI_PCIE_TL_MWr_32;
         wrTlp.easyModeTlpConstraint.constraint_mode(1);
         if ($cast(pcie_seqr,get_sequencer())) begin
            wrTlp.srcConfig = pcie_seqr.srcConfig;
            wrTlp.dstConfig = pcie_seqr.dstConfig;
         end
      end
   endtask : pre_do

   // Perform Memory Read After Memory Write
   virtual task body();
      `uvm_info("pcie_TLReadAfterWriteSeq", "Starting pcie_TLReadAfterWriteSeq sequence", UVM_LOW);
      wait_for_RC_active();
      `uvm_info("pcie_TLReadAfterWriteSeq", "Drive MWr_32", UVM_LOW);
      `uvm_do_with(wrTlp,{wrTlp.length <= 32;})
      `uvm_info("pcie_TLReadAfterWriteSeq", $sformatf("\nDriving TLP:\n%s", wrTlp.sprintInfo()), UVM_LOW);
      `uvm_info("pcie_TLReadAfterWriteSeq", "Drive MRd_32", UVM_LOW);
      `uvm_do_with(rdTlp,{rdTlp.length <= 32;})
      `uvm_info("pcie_TLReadAfterWriteSeq", $sformatf("\nDriving TLP:\n%s", rdTlp.sprintInfo()), UVM_LOW);
     
      #20000ns;
   endtask : body
endclass : pcie_TLReadAfterWriteSeq

// This sequence will send a TLP from the BFM and will damage it's ECRC.
class pcie_ECRCerrorInj extends pcie_sequence;

   DenaliSvPcie::denaliPcieTlpMemPacket pkt;
   int unsigned                              seqNum;

   `uvm_object_utils_begin(pcie_ECRCerrorInj)
      `uvm_field_object(pkt, UVM_ALL_ON)
   `uvm_object_utils_end
   `uvm_declare_p_sequencer(pcie_sequencer)

   function new(string name = "pcie_ECRCerrorInj");
      super.new(name);
   endfunction : new

   virtual task pre_do(bit is_item);
      pcie_sequencer pcie_seqr;

      pkt.pktType = DenaliSvPcie::DENALI_PCIE__Tlp;
      pkt.tlpType = DenaliSvPcie::DENALI_PCIE_TL_MRd_32;
      pkt.easyModeTlpConstraint.constraint_mode(1);
      pkt.errInjects = new[3];
      // To ease BFM checkings on the Tx packet.
      pkt.errInjects[2] = DenaliSvPcie::PCIE_EI_RELAX_CHK;
      if ($cast(pcie_seqr,get_sequencer())) begin
         pkt.srcConfig = pcie_seqr.srcConfig;
         pkt.dstConfig = pcie_seqr.dstConfig;
      end
   endtask : pre_do

   virtual task body();
      `uvm_info(get_type_name(),"Starting Sequence pcie_ECRCerrorInj",UVM_LOW);
      wait_for_RC_active();

      // Send a pkt with bad-ECRC
      `uvm_do_with(pkt, {pkt.errInjects[0] == DenaliSvPcie::PCIE_EI_TLP_ENABLE_ECRC;
                         pkt.errInjects[1] == DenaliSvPcie::PCIE_EI_TLP_ECRC;
                        } );

      `uvm_info(get_type_name(),"Finished Sequence pcie_ECRCerrorInj",UVM_LOW);
   endtask : body

endclass : pcie_ECRCerrorInj

// This sequence implements Fundamental Reset
/* class pcie_FundReset extends pcie_sequence;

   pcie_TLReadAfterWriteSeq memSeq;
   int unsigned                              delay;
   DenaliSvPcie::denaliPcieTlpMemPacket wrTlp;

   `uvm_object_utils_begin(pcie_FundReset)
      `uvm_field_object(wrTlp, UVM_ALL_ON)
      `uvm_field_int(delay, UVM_ALL_ON)
     `uvm_object_utils_end
   `uvm_declare_p_sequencer(pcie_sequencer)

   function new(string name = "pcie_FundReset");
      super.new(name);
   endfunction : new

   task do_fundamental_reset();
      `uvm_info(get_type_name(),"Starting to Assert PERST_ Fundamental Reset",UVM_LOW);
      tb.PERST_n = 0;
      #500
      tb.PERST_n = 1;
      `uvm_info(get_type_name(),"Finished to Assert PERST_ Fundamental Reset",UVM_LOW);
   endtask : do_fundamental_reset

   virtual task body();
      `uvm_info(get_type_name(),"Starting Sequence pcie_FundReset",UVM_LOW);
      fork 
         begin
            `uvm_do(memSeq)
         end
         begin
            // Wait a random time before pulling reset
            assert(randomize(delay) with {delay > 10; delay < 50;} );
            `uvm_info(get_type_name(), $sformatf("Wait %d before initiating reset..",delay), UVM_LOW);
            #delay;
            do_fundamental_reset();
            `uvm_info(get_type_name(),"Booting from the beginning..", UVM_LOW);
            `uvm_do(memSeq)
         end
      join_any
      `uvm_info(get_type_name(),"Finished Sequence pcie_FundReset", UVM_LOW);
   endtask : body

endclass : pcie_FundReset */

// This sequence implements Vendor Defined Message
class pcie_VDMSequence extends pcie_sequence;

   DenaliSvPcie::denaliPcieTlpMsgPacket Msg;
   rand integer                         regAddr;
   rand integer                         sizeInBytes;

   `uvm_object_utils_begin(pcie_VDMSequence)
      `uvm_field_int(regAddr, UVM_ALL_ON)
      `uvm_field_int(sizeInBytes, UVM_ALL_ON)
   `uvm_object_utils_end
   `uvm_declare_p_sequencer(pcie_sequencer)

   function new(string name = "pcie_VDMSequence");
      super.new(name);
   endfunction : new

   virtual task pre_do(bit is_item);
      Msg.srcConfig = p_sequencer.srcConfig;
      Msg.dstConfig = p_sequencer.dstConfig;

      // Set fields and enable constraints in the packet.
      Msg.pktType = DenaliSvPcie::DENALI_PCIE__Tlp;
      Msg.tlpType = DenaliSvPcie::DENALI_PCIE_TL_MsgD;
      Msg.messageType = DenaliSvPcie::DENALI_PCIE_TL_MSG_VD_Type0;
      Msg.vdMsgRouting = DenaliSvPcie::DENALI_PCIE_TL_VDMSG_ROUT_local_terminate;
      Msg.length = 0;
      Msg.easyModeTlpConstraint.constraint_mode(1);
   endtask : pre_do

   virtual task body();
      wait_for_RC_active();
      `uvm_info(get_type_name(),"Send a Vendor Defined Message from BFM", UVM_LOW);
      `uvm_do(Msg)
      Msg.print();
     
      #10000ns;
   endtask : body
endclass : pcie_VDMSequence

//THis Base Sequence provides Handle for both EP and RC registers
class apr_pcie_base_seq extends pcie_sequence;
   pcie_agent         ep_mon_agent;
   pcie_mem_instance  ep_mem_inst;

   `uvm_object_utils_begin(apr_pcie_base_seq)
      `uvm_field_object(ep_mon_agent, UVM_ALL_ON)
      `uvm_field_object(ep_mem_inst, UVM_ALL_ON)
   `uvm_object_utils_end

   function new(string name = "apr_pcie_base_seq");
      super.new(name);
   endfunction : new

   //virtual task pre_body();
   virtual task body();
	    if (!$cast(pcie_seqr, get_sequencer())) 
               `uvm_fatal("apr_pcie_base_seq","pcie_seqr is of wrong type");
            if (!$cast(rc_bfm_agent, pcie_seqr.pAgent) ) 
               `uvm_fatal("apr_pcie_base_seq", "rc_bfm_agent is of wrong type");
            if (!$cast(pcie_mem_inst, rc_bfm_agent.regInst))
               `uvm_fatal("apr_pcie_base_seq", "Failed to cast pcie_mem_inst");

	    if (!$cast(ep_mon_agent, pcie_seqr.pEnv.ep_mon) ) 
               `uvm_fatal("apr_pcie_base_seq", "ep_mon_agent is of wrong type");
            if (!$cast(ep_mem_inst, ep_mon_agent.regInst))
               `uvm_fatal("apr_pcie_base_seq", "Failed to cast pcie_mem_inst");
   endtask : body
   //endtask : pre_body
endclass : apr_pcie_base_seq

//Basic LinkUp Sequence
class apr_pcie_linkup_seq extends apr_pcie_base_seq;

   `uvm_object_utils(apr_pcie_linkup_seq)


   function new(string name = "apr_pcie_linkup_seq");
      super.new(name);
   endfunction : new

   virtual task body();
               super.body();
            `uvm_info("apr_pcie_linkup_seq", "Waiting on Linkup and Device Enumeration to Finish", UVM_NONE);
             wait_for_RC_active();
             `uvm_info("apr_pcie_linkup_seq", "Auto_Enumeration Completed", UVM_NONE);
   endtask : body
endclass : apr_pcie_linkup_seq

//This implements VIP back-door for both EP and RC register space
class apr_pcievip_bkdoor_wrseq extends apr_pcie_base_seq;
     reg [31:0] den_sim_st, rd_val;

   `uvm_object_utils(apr_pcievip_bkdoor_wrseq)


   function new(string name = "apr_pcievip_bkdoor_wrseq");
      super.new(name);
   endfunction : new

   virtual task body();
	    super.body();
           `uvm_info("apr_pcievip_bkdoor_wrseq", "Performing Read_Modify_write on EP_MON Registers!!", UVM_NONE);
      // Converting known denali errors to Warning.
	    rd_val = ep_mem_inst.readReg(DenaliSvPcie::PCIE_REG_DEN_ERROR_CTRL);
      `uvm_info(get_full_name(), $sformatf("PCIE_REG_DEN_ERROR_CTRL reads : %0h",rd_val), UVM_HIGH);
	    ep_mem_inst.writeReg(DenaliSvPcie::PCIE_REG_DEN_ERROR_CTRL, ((DenaliSvPcie::PCIE_TL_NONFATAL_MON_MEM << 8) + (DenaliSvPcie::PCIE_ERR_CONFIG_DIRECTION_TRX << 4) + DenaliSvPcie::PCIE_ERR_CONFIG_FORMAT_WARN));
   endtask : body
endclass : apr_pcievip_bkdoor_wrseq

//cfgWr0 Generation
class gen_cfgwr extends apr_pcie_base_seq;
      DenaliSvPcie::denaliPcieTlpCfgPacket wrTlp;
       pcie_sequencer pcie_seqr;
      reg[7:0]      wr_data[4];
      rand reg[31:0] address;
      rand reg [31:0] reg_data;
   `uvm_object_utils(gen_cfgwr);
   `uvm_declare_p_sequencer(pcie_sequencer);

   function new (string name="gen_cfgwr");
      super.new(name);
      wrTlp = DenaliSvPcie::denaliPcieTlpCfgPacket::type_id::create("wrTlp");
   endfunction : new

   virtual task body();
               super.body();
        if ($cast(pcie_seqr,get_sequencer())) begin
		wrTlp.srcConfig = pcie_seqr.srcConfig;
		wrTlp.dstConfig = pcie_seqr.dstConfig;
        end
    `uvm_info(get_type_name(), "Starting Cfg_WR item on PCIe-VIP RC sequencer", UVM_HIGH);
	//reg_num = 6'h25; //ext_reg_num = 4'h8;
	wrTlp.easyModeTlpConstraint.constraint_mode(1);
        wrTlp.randomize() with {length == 'h1;};
	assert (wrTlp.randomize() with {
          registerNumber  ==  address[7:2];   
          extRegisterNumber  ==  address[11:8];   
          trafficClass   == 0;  
          length == 1;              
          firstBe == 'hf;
          lastBe  == 'h0;
          ipg == 0;
          errCtrlCount == 0;
          pktDelay == 0;
          hasDigest == 0;
          isPoisoned == 0;
          attr ==0;  
         });
	wr_data[3] = reg_data[7:0];
	wr_data[2] = reg_data[15:8];
	wr_data[1] = reg_data[23:16];
	wr_data[0] = reg_data[31:24];
	wrTlp.tlpType = DenaliSvPcie::DENALI_PCIE_TL_CfgWr0;
	wrTlp.setPayload(wr_data);

	start_item(wrTlp);
        finish_item(wrTlp);
    endtask : body
endclass : gen_cfgwr

class gen_cfgrd extends apr_pcie_base_seq;
      DenaliSvPcie::denaliPcieTlpCfgPacket rdTlp;
      DenaliSvPcie::denaliPcieTlpCplPacket rsptlp;
      pcie_sequencer pcie_seqr;
      reg[7:0]      wr_data[4];
      rand reg[31:0] address;
      reg [31:0] reg_data;
   `uvm_object_utils(gen_cfgrd);
   `uvm_declare_p_sequencer(pcie_sequencer);

   function new (string name="gen_cfgrd");
      super.new(name);
      rdTlp = DenaliSvPcie::denaliPcieTlpCfgPacket::type_id::create("rdTlp");
      rsptlp = DenaliSvPcie::denaliPcieTlpCplPacket::type_id::create("rsptlp");
   endfunction : new

   virtual task body();
               super.body();
        if ($cast(pcie_seqr,get_sequencer())) begin
		rdTlp.srcConfig = pcie_seqr.srcConfig;
		rdTlp.dstConfig = pcie_seqr.dstConfig;
        end
        `uvm_info(get_type_name(), "Starting Cfg_RD item on PCIe-VIP RC sequencer", UVM_HIGH);
	reg_data = 32'h0;
	rdTlp.easyModeTlpConstraint.constraint_mode(1);
        rdTlp.randomize() with {length == 'h1;};
	assert (rdTlp.randomize() with {
          registerNumber  ==  address[7:2];   
          extRegisterNumber  ==  address[11:8];   
          trafficClass   == 0;  
          length == 1;              
          firstBe == 'hf;
          lastBe  == 'h0;
          ipg == 0;
          errCtrlCount == 0;
          pktDelay == 0;
          hasDigest == 0;
          isPoisoned == 0;
          attr ==0;  
         });
	wr_data[3] = reg_data[7:0];
	wr_data[2] = reg_data[15:8];
	wr_data[1] = reg_data[23:16];
	wr_data[0] = reg_data[31:24];
	rdTlp.tlpType = DenaliSvPcie::DENALI_PCIE_TL_CfgRd0;
	rdTlp.setPayload(wr_data);

	start_item(rdTlp);
        finish_item(rdTlp);
	get_response(rsp);
	`uvm_info(get_type_name(), $sformatf( "got response from VIP sequencer-\n %s",rsp.sprint()), UVM_HIGH);
	if ( !$cast(rsptlp, rsp) ) `uvm_fatal("CFGRD_SEQ", "Failed to cast cfg_rsp");
	reg_data[7:0]= rsptlp.payload[3]; 
	reg_data[15:8]= rsptlp.payload[2];
	reg_data[23:16] = rsptlp.payload[1];
	reg_data[31:24] = rsptlp.payload[0];
	`uvm_info(get_full_name(), $psprintf("RD_DATA :: %0h",reg_data), UVM_HIGH);
    endtask : body
endclass : gen_cfgrd


class gen_memwr32 extends apr_pcie_base_seq;
      DenaliSvPcie::denaliPcieTlpMemPacket wrTlp;
       pcie_sequencer pcie_seqr;
      reg[7:0]      wr_data[4];
      rand reg[31:0] address;
      rand reg [31:0] reg_data;
      bit[63:0] bar0_start_address;
      bit[63:0] target_address;
   `uvm_object_utils_begin(gen_memwr32)
      `uvm_field_object(wrTlp, UVM_ALL_ON)
   `uvm_object_utils_end
   `uvm_declare_p_sequencer(pcie_sequencer);

   function new (string name="gen_memwr32");
      super.new(name);
      wrTlp = DenaliSvPcie::denaliPcieTlpMemPacket::type_id::create("wrTlp");
   endfunction : new

   virtual task body();
               super.body();
        if ($cast(pcie_seqr,get_sequencer())) begin
		wrTlp.srcConfig = pcie_seqr.srcConfig;
		wrTlp.dstConfig = pcie_seqr.dstConfig;
        end
       bar0_start_address = wrTlp.dstConfig.mem64Bars[0].startAddress;
       target_address = bar0_start_address + address;
       wrTlp.pktType = DenaliSvPcie::DENALI_PCIE__Tlp;
       wrTlp.tlpType = DenaliSvPcie::DENALI_PCIE_TL_MWr_32;
       `uvm_info(get_type_name(), "Starting MWR_32 item on PCIe-VIP RC sequencer", UVM_HIGH);
       `uvm_info(get_full_name(), $sformatf("bar0_start_address= :: %0h",bar0_start_address), UVM_HIGH);
	//reg_num = 6'h25; //ext_reg_num = 4'h8;
	wrTlp.easyModeTlpConstraint.constraint_mode(1);
	 wrTlp.randomize() with {
         length == 1;
         firstBe == 'hf;
	 lastBe == 'h0;
         address == target_address[31:2]; };
	wr_data[3] = reg_data[7:0];
	wr_data[2] = reg_data[15:8];
	wr_data[1] = reg_data[23:16];
	wr_data[0] = reg_data[31:24];
	wrTlp.setPayload(wr_data);
        `uvm_info("gen_memwr32", $sformatf("\nDriving MWr_32 TLP:\n%s", wrTlp.sprintInfo()), UVM_HIGH);
	start_item(wrTlp);
        finish_item(wrTlp);
    endtask : body
endclass : gen_memwr32


class gen_memrd32 extends apr_pcie_base_seq;
      DenaliSvPcie::denaliPcieTlpMemPacket rdTlp;
      DenaliSvPcie::denaliPcieTlpCplPacket rsptlp;
      pcie_sequencer pcie_seqr;
      reg[7:0]      wr_data[4];
      rand reg[31:0] address;
      reg [31:0] reg_data;
      bit[63:0] bar0_start_address;
      bit[63:0] target_address;
   `uvm_object_utils(gen_memrd32);
   `uvm_declare_p_sequencer(pcie_sequencer);

   function new (string name="gen_memrd32");
      super.new(name);
      rdTlp = DenaliSvPcie::denaliPcieTlpMemPacket::type_id::create("rdTlp");
      rsptlp = DenaliSvPcie::denaliPcieTlpCplPacket::type_id::create("rsptlp");
   endfunction : new

   virtual task body();
               super.body();
        if ($cast(pcie_seqr,get_sequencer())) begin
		rdTlp.srcConfig = pcie_seqr.srcConfig;
		rdTlp.dstConfig = pcie_seqr.dstConfig;
        end
        `uvm_info(get_type_name(), "Starting MRd item on PCIe-VIP RC sequencer", UVM_HIGH);
	bar0_start_address = rdTlp.dstConfig.mem64Bars[0].startAddress;
	target_address = bar0_start_address + address;
        rdTlp.pktType = DenaliSvPcie::DENALI_PCIE__Tlp;
        rdTlp.tlpType = DenaliSvPcie::DENALI_PCIE_TL_MRd_32;
	reg_data = 32'h0;
	rdTlp.easyModeTlpConstraint.constraint_mode(1);
	`uvm_info(get_full_name(), $sformatf("MRD:: bar0_start_address= :: %0h",bar0_start_address), UVM_HIGH);
	rdTlp.randomize() with {
         length == 1;
         firstBe == 'hf;
	 lastBe == 'h0;
         address == target_address[31:2]; };
	wr_data[3] = reg_data[7:0];
	wr_data[2] = reg_data[15:8];
	wr_data[1] = reg_data[23:16];
	wr_data[0] = reg_data[31:24];
	rdTlp.setPayload(wr_data);

	start_item(rdTlp);
        finish_item(rdTlp);
	get_response(rsp);
	`uvm_info(get_type_name(), $sformatf( "got response from VIP sequencer-\n %s",rsp.sprint()), UVM_HIGH);
	if ( !$cast(rsptlp, rsp) ) `uvm_fatal("gen_memrd32", "Failed to cast cpld rsp");
	reg_data[7:0]= rsptlp.payload[3]; 
	reg_data[15:8]= rsptlp.payload[2];
	reg_data[23:16] = rsptlp.payload[1];
	reg_data[31:24] = rsptlp.payload[0];
	`uvm_info(get_full_name(), $psprintf("RD_DATA :: %0h",reg_data), UVM_HIGH);
    endtask : body
endclass : gen_memrd32

class apr_pcie_tlp_seq extends apr_pcie_base_seq;
	gen_cfgwr             cfgwr_Seq;
	gen_cfgrd             cfgrd_Seq;
	gen_memwr32           memwr_Seq;
	gen_memrd32           memrd_Seq;
        `uvm_object_utils_begin(apr_pcie_tlp_seq)
		`uvm_field_object(cfgwr_Seq, UVM_ALL_ON)
		`uvm_field_object(cfgrd_Seq, UVM_ALL_ON)
		`uvm_field_object(memwr_Seq, UVM_ALL_ON)
		`uvm_field_object(memrd_Seq, UVM_ALL_ON)
	`uvm_object_utils_end
		

	function new(string name="apr_pcie_tlp_seq");
		super.new(name);
	endfunction // new

	virtual task body();
                     super.body();
	       	endtask : body 

       virtual task pciecfgrd(input reg [31:0] addr, output reg [31:0] reg_out);
                     cfgrd_Seq = gen_cfgrd::type_id::create("cfgrd_Seq");
		     cfgrd_Seq.randomize() with {address == addr;};
		     cfgrd_Seq.start(pcie_seqr);
		     reg_out = cfgrd_Seq.reg_data;
		`uvm_info(get_full_name(), $sformatf("RD Addr= :: %0h and Resp Data = %h",addr, reg_out), UVM_HIGH);
       endtask

       virtual task pciecfgwr(input reg [31:0] addr, reg [31:0] data);
	        cfgwr_Seq = gen_cfgwr::type_id::create("cfgwr_Seq");
		cfgwr_Seq.randomize() with {address == addr; reg_data == data;};
		cfgwr_Seq.start(pcie_seqr);
		`uvm_info(get_full_name(), $sformatf("Write addr = %h DATA :: %0h",addr, data), UVM_HIGH);
       endtask

       virtual task pciememwr(input reg [31:0] addr, reg [31:0] data);
                memwr_Seq = gen_memwr32::type_id::create("memwr_Seq");
        	memwr_Seq.randomize() with {address == addr; reg_data == data;};
        	memwr_Seq.start(pcie_seqr);
        	`uvm_info(get_full_name(), $sformatf("MEM_WR :: Write addr = %h DATA :: %0h",addr, data), UVM_HIGH);
       endtask

       virtual task pciememrd(input reg [31:0] addr, output reg [31:0] reg_out);
                     memrd_Seq = gen_memrd32::type_id::create("memrd_Seq");
		     memrd_Seq.randomize() with {address == addr;};
		     memrd_Seq.start(pcie_seqr);
		     reg_out = memrd_Seq.reg_data;
		`uvm_info(get_full_name(), $sformatf("RD Addr= :: %0h and Resp Data = %h",addr, reg_out), UVM_HIGH);
       endtask

endclass : apr_pcie_tlp_seq
//APR BAR Programming Sequence
class apr_bar_prog_seq extends apr_pcie_tlp_seq;
      reg [31:0] reg_data;
      reg [31:0] rd_val;
   `uvm_object_utils(apr_bar_prog_seq)


   function new(string name = "apr_bar_prog_seq");
      super.new(name);
   endfunction : new

   virtual task body();
               super.body();
              `uvm_info("apr_bar_prog_seq", "body::Over-Writing BAR Settings!!", UVM_NONE);
	      reg_data = 32'h0000_0000;
	      pciecfgwr(32'h0014, reg_data);//BAR_1

	      //reg_data = 32'h0000_4004;
	      reg_data = 32'h4000_0004;
	      pciecfgwr(32'h0010, reg_data);//BAR_0
              `uvm_info("apr_bar_prog_seq", "body::Quiese Period !!", UVM_HIGH);
	      #5000ns;
	      `uvm_info(get_full_name(), $sformatf("Ready For Register Access :: %0h",rd_val), UVM_HIGH);
   endtask : body
endclass : apr_bar_prog_seq

//APR random MTU max_payload_size programming seq
class apr_rand_mtu_prog_seq extends apr_pcie_tlp_seq;
      reg [31:0] reg_data;
      reg [31:0] rd_val;
      rand int MaxRdReqSize, MaxPayloadSize; 
      bit [2:0] Rd_size, Wr_size;
      bit msi_64_en = 1'b0;
      
   //cdn_pcie_pkg::apr_cp_pcie_cfg pcie_cfg;
   `uvm_object_utils_begin(apr_rand_mtu_prog_seq)
       `uvm_field_int(MaxRdReqSize, UVM_ALL_ON | UVM_DEC);
       `uvm_field_int(MaxPayloadSize, UVM_ALL_ON | UVM_DEC);
   `uvm_object_utils_end


   function new(string name = "apr_rand_mtu_prog_seq");
      super.new(name);
   endfunction : new

   virtual task body();
          super.body();
        case (MaxRdReqSize)
	   128      : Rd_size = 3'b000;
	   256      : Rd_size = 3'b001;
	   512      : Rd_size = 3'b010; //DUT Default
	   1024     : Rd_size = 3'b011;
	   2048     : Rd_size = 3'b100;
	   4096     : Rd_size = 3'b101;
	   default  : Rd_size = 3'b010;
	endcase

        case (MaxPayloadSize)
	   128      : Wr_size = 3'b000; //DUT Default
	   256      : Wr_size = 3'b001;
	   512      : Wr_size = 3'b010;
	   1024     : Wr_size = 3'b011;
	   2048     : Wr_size = 3'b100;
	   4096     : Wr_size = 3'b101;
	   default  : Wr_size = 3'b001;
	endcase
	    rd_val = pcie_mem_inst.readReg(DenaliSvPcie::PCIE_REG_EXP_DEV_CTRL);
            `uvm_info(get_full_name(), $sformatf("RC_BFM :: DEV_CTRL (Default) reads : %0h,Def_MaxRdReqSize = %0h, Def_MaxPayloadSize %0h",rd_val, rd_val[14:12], rd_val[7:5]), UVM_HIGH);
            rd_val[14:12] = Rd_size;
	    rd_val[7:5] = Wr_size;
	    pcie_mem_inst.writeReg(DenaliSvPcie::PCIE_REG_EXP_DEV_CTRL, rd_val);

	    rd_val = ep_mem_inst.readReg(DenaliSvPcie::PCIE_REG_EXP_DEV_CTRL);
            `uvm_info(get_full_name(), $sformatf("EP_MON :: DEV_CTRL (Default) reads : %0h,Def_MaxRdReqSize = %0h, Def_MaxPayloadSize %0h",rd_val, rd_val[14:12], rd_val[7:5]), UVM_HIGH);
            rd_val[14:12] = Rd_size;
	    rd_val[7:5] = Wr_size;
	    ep_mem_inst.writeReg(DenaliSvPcie::PCIE_REG_EXP_DEV_CTRL, rd_val);
	    pciecfgwr(32'h0078, rd_val);//PCIE_DEVICE_CONTROL_2

	    msi_64_en = $urandom();

	    if(msi_64_en) begin
	      // read Msg_ctrl_reg (bit 7) to check suppport then programm some valid random value 
	       reg_data = 'h00000000; //Message_Addr[63:32] for 64 bit addressing
	       pciecfgwr(32'h0054, reg_data);//Message_Addr[63:32]  ('h15 in translog)
	       reg_data = 'h00008000; //Message_Addr[63:32] for 64 bit addressing
	       pciecfgwr(32'h0058, reg_data);//Message_Addr[63:32]  ('h16 in translog)
	    end
	    reg_data = $urandom(); //MSI_DATA
	    pciecfgwr(32'h005C, reg_data);//Message_Data_Register ('h17 in translog)

	    reg_data = 'h00000406;
	    pciecfgwr(32'h0004, reg_data);//PCIE_COMMAND_REG
      `uvm_info(get_full_name(), $sformatf("PCIE_DEVICE_CONTROL_2 REG Modified Data : %0h, Mod_MaxRdReqSize = %0h, Mod_MaxPayloadSize %0h",rd_val, rd_val[14:12], rd_val[7:5]), UVM_HIGH);
   endtask : body
endclass : apr_rand_mtu_prog_seq

class apr_pcie_shared_csr_rw_seq extends apr_pcie_tlp_seq;
      reg [31:0] reg_data;
      reg [31:0] rd_val;
   `uvm_object_utils(apr_pcie_shared_csr_rw_seq)


   function new(string name = "apr_pcie_shared_csr_rw_seq");
      super.new(name);
   endfunction : new

   virtual task body();
               super.body();
              `uvm_info("apr_pcie_shared_csr_rw_seq", "body::Default Read and compare!!", UVM_NONE);
	      pciememrd(32'h0000, rd_val);//CSR_0 :CP_CAP_REG
	      if(rd_val !== 32'h0) 
                    `uvm_error(get_name(),$sformatf("Default Read mismatch for CP_CAP_REG, Exp_Data = 32'h0 and Data Read=%0h", rd_val));
	      pciememrd(32'h0004, rd_val);//CSR_1 :CP_PCIE_CTRL
	      if(rd_val !== 32'hE0E0_E0E0) 
                    `uvm_error(get_name(),$sformatf("Default Read mismatch for CP_PCIE_CTRL, Exp_Data = 32'hE0E0_E0E0 and Data Read=%0h", rd_val));
	     pciememrd(32'h0008, rd_val);//CSR_2 :CP_LED_GP_STRAP_PINS
	     if(rd_val !== 32'h0000_0000) 
                   `uvm_error(get_name(),$sformatf("Default Read mismatch for CP_LED_GP_STRAP_PINS, Exp_Data = 32'h0000_0000 and Data Read=%0h", rd_val));
	     pciememrd(32'h0010, rd_val);//CSR_3 :CP_PCIE_STATUS
	     if(rd_val[5:0] !== 6'h11) 
                   `uvm_error(get_name(),$sformatf("Default Read mismatch for CP_PCIE_STATUS, Exp_Data = 6'h11 and Data Read=%0h", rd_val[5:0]));
	     pciememrd(32'h0014, rd_val);//CSR_4 :CP_PCIE_DBI_CTRL
	     if(rd_val !== 32'h00)// bit 7 has default value '0' for pcie and '1' for processor 
	    // if(rd_val !== 32'h80)// Temporary fix till DBI state_machine starts working 
                   `uvm_error(get_name(),$sformatf("Default Read mismatch for CP_PCIE_DBI_CTRL, Exp_Data = 32'h00 and Data Read=%0h", rd_val));
	     pciememrd(32'h0018, rd_val);//CSR_5 :CP_PCIE_RST_STATUS
	     pciememrd(32'h001c, rd_val);//CSR_6 :CP_HOST_INT_VECTOR
	     if(rd_val !== 32'h0) 
                   `uvm_error(get_name(),$sformatf("Default Read mismatch for CP_HOST_INT_VECTOR, Exp_Data = 32'h0 and Data Read=%0h", rd_val));
	     pciememrd(32'h0020, rd_val);//CSR_7 :CP_HOST_INT_PENDING
	     if(rd_val !== 32'h0) 
                   `uvm_error(get_name(),$sformatf("Default Read mismatch for CP_HOST_INT_PENDING, Exp_Data = 32'h0 and Data Read=%0h", rd_val));
	     pciememrd(32'h0024, rd_val);//CSR_8 :CP_DMA_ERR_GEN
	     if(rd_val !== 32'h0) 
                   `uvm_error(get_name(),$sformatf("Default Read mismatch for CP_DMA_ERR_GEN, Exp_Data = 32'h0 and Data Read=%0h", rd_val));
	     pciememrd(32'h002c, rd_val);//CSR_10:CP_Host_RISC_Ctrl
	     if(rd_val !== 32'h0) 
                   `uvm_error(get_name(),$sformatf("Default Read mismatch for CP_Host_RISC_Ctrl, Exp_Data = 32'h0 and Data Read=%0h", rd_val));
	     pciememrd(32'h0030, rd_val);//CSR_11:CP_RISC_GP
	     if(rd_val !== 32'h0) 
                   `uvm_error(get_name(),$sformatf("Default Read mismatch for CP_RISC_GP, Exp_Data = 32'h0 and Data Read=%0h", rd_val));
	     pciememrd(32'h0034, rd_val);//CSR_12:CP_GP_IO_Reg
	     if(rd_val !== 32'h0000) 
                   `uvm_error(get_name(),$sformatf("Default Read mismatch for CP_GP_IO_Reg, Exp_Data = 32'h0000 and Data Read=%0h", rd_val));

	      pciememrd(32'h0038, rd_val);//CSR_13:CP_Queue_Reg01
	      if(rd_val !== 32'h0) 
                   `uvm_error(get_name(),$sformatf("Default Read mismatch for CP_Queue_Reg01, Exp_Data = 32'h0 and Data Read=%0h", rd_val));
	      pciememrd(32'h0040, rd_val);//CSR_14:CP_Queue_Reg02
	      if(rd_val !== 32'h0) 
                   `uvm_error(get_name(),$sformatf("Default Read mismatch for CP_Queue_Reg02, Exp_Data = 32'h0 and Data Read=%0h", rd_val));
	      pciememrd(32'h0048, rd_val);//CSR_15:CP_Queue_Reg03
	      if(rd_val !== 32'h0) 
                   `uvm_error(get_name(),$sformatf("Default Read mismatch for CP_Queue_Reg03, Exp_Data = 32'h0 and Data Read=%0h", rd_val));
	      pciememrd(32'h0050, rd_val);//CSR_16:CP_Queue_Reg04
	      if(rd_val !== 32'h0) 
                   `uvm_error(get_name(),$sformatf("Default Read mismatch for CP_Queue_Reg04, Exp_Data = 32'h0 and Data Read=%0h", rd_val));

	      pciememrd(32'h003c, rd_val);//CSR_17:CP_Queue_Reg11
	      if(rd_val !== 32'h0) 
                   `uvm_error(get_name(),$sformatf("Default Read mismatch for CP_Queue_Reg11, Exp_Data = 32'h0 and Data Read=%0h", rd_val));
	      pciememrd(32'h0044, rd_val);//CSR_18:CP_Queue_Reg12
	      if(rd_val !== 32'h0) 
                   `uvm_error(get_name(),$sformatf("Default Read mismatch for CP_Queue_Reg12, Exp_Data = 32'h0 and Data Read=%0h", rd_val));
	      pciememrd(32'h004c, rd_val);//CSR_19:CP_Queue_Reg13
	      if(rd_val !== 32'h0) 
                   `uvm_error(get_name(),$sformatf("Default Read mismatch for CP_Queue_Reg13, Exp_Data = 32'h0 and Data Read=%0h", rd_val));
	      pciememrd(32'h0054, rd_val);//CSR_20:CP_Queue_Reg14
	      if(rd_val !== 32'h0) 
                   `uvm_error(get_name(),$sformatf("Default Read mismatch for CP_Queue_Reg14, Exp_Data = 32'h0 and Data Read=%0h", rd_val));

	      pciememrd(32'h0068, rd_val);//CSR_21:CP_Queue_Status_0
	      if(rd_val !== 32'h0) 
                   `uvm_error(get_name(),$sformatf("Default Read mismatch for CP_Queue_Status_0, Exp_Data = 32'h0 and Data Read=%0h", rd_val));
	      pciememrd(32'h006c, rd_val);//CSR_22:CP_Queue_Status_1
	      if(rd_val !== 32'h0) 
                   `uvm_error(get_name(),$sformatf("Default Read mismatch for CP_Queue_Status_1, Exp_Data = 32'h0 and Data Read=%0h", rd_val));
	      pciememrd(32'h0070, rd_val);//CSR_23:CP_Queue_Status_2
	      if(rd_val !== 32'h0) 
                   `uvm_error(get_name(),$sformatf("Default Read mismatch for CP_Queue_Status_2, Exp_Data = 32'h0 and Data Read=%0h", rd_val));
	      pciememrd(32'h0074, rd_val);//CSR_24:CP_Queue_Status_3
	      if(rd_val !== 32'h0) 
                   `uvm_error(get_name(),$sformatf("Default Read mismatch for CP_Queue_Status_3, Exp_Data = 32'h0 and Data Read=%0h", rd_val));

	      pciememrd(32'h0078, rd_val);//CSR_21:CP_DMA_ERR_CNT_0
	      if(rd_val !== 32'h0) 
                   `uvm_error(get_name(),$sformatf("Default Read mismatch for CP_DMA_ERR_CNT_0, Exp_Data = 32'h0 and Data Read=%0h", rd_val));
	      pciememrd(32'h007c, rd_val);//CSR_22:CP_DMA_ERR_CNT_1
	      if(rd_val !== 32'h0) 
                   `uvm_error(get_name(),$sformatf("Default Read mismatch for CP_DMA_ERR_CNT_1, Exp_Data = 32'h0 and Data Read=%0h", rd_val));
	      pciememrd(32'h0080, rd_val);//CSR_23:CP_DMA_ERR_CNT_2
	      if(rd_val !== 32'h0) 
                   `uvm_error(get_name(),$sformatf("Default Read mismatch for CP_DMA_ERR_CNT_2, Exp_Data = 32'h0 and Data Read=%0h", rd_val));
	      pciememrd(32'h0084, rd_val);//CSR_24:CP_DMA_ERR_CNT_3
	      if(rd_val !== 32'h0) 
                   `uvm_error(get_name(),$sformatf("Default Read mismatch for CP_DMA_ERR_CNT_3, Exp_Data = 32'h0 and Data Read=%0h", rd_val));
	      pciememrd(32'h0088, rd_val);//CSR_25:CP_DMA_ERR_CNT_4
	      if(rd_val !== 32'h0) 
                   `uvm_error(get_name(),$sformatf("Default Read mismatch for CP_DMA_ERR_CNT_4, Exp_Data = 32'h0 and Data Read=%0h", rd_val));
	      pciememrd(32'h008c, rd_val);//CSR_26:CP_DMA_ERR_CNT_5
	      if(rd_val !== 32'h0) 
                   `uvm_error(get_name(),$sformatf("Default Read mismatch for CP_DMA_ERR_CNT_5, Exp_Data = 32'h0 and Data Read=%0h", rd_val));

	      pciememrd(32'h0098, rd_val);//CSR_27:CP_RISC_HOST_GP_REG_0
	      if(rd_val !== 32'h0) 
                   `uvm_error(get_name(),$sformatf("Default Read mismatch for CP_RISC_HOST_GP_REG_0, Exp_Data = 32'h0 and Data Read=%0h", rd_val));
	      pciememrd(32'h009c, rd_val);//CSR_28:CP_RISC_HOST_GP_REG_1
	      if(rd_val !== 32'h0) 
                   `uvm_error(get_name(),$sformatf("Default Read mismatch for CP_RISC_HOST_GP_REG_1, Exp_Data = 32'h0 and Data Read=%0h", rd_val));
	      pciememrd(32'h00a0, rd_val);//CSR_29:CP_RISC_HOST_GP_REG_2
	      if(rd_val !== 32'h0) 
                   `uvm_error(get_name(),$sformatf("Default Read mismatch for CP_RISC_HOST_GP_REG_2, Exp_Data = 32'h0 and Data Read=%0h", rd_val));
	      pciememrd(32'h00a4, rd_val);//CSR_30:CP_RISC_HOST_GP_REG_3
	      if(rd_val !== 32'h0) 
                   `uvm_error(get_name(),$sformatf("Default Read mismatch for CP_RISC_HOST_GP_REG_3, Exp_Data = 32'h0 and Data Read=%0h", rd_val));
	      pciememrd(32'h00a8, rd_val);//CSR_31:CP_RISC_HOST_GP_REG_4
	      if(rd_val !== 32'h0) 
                   `uvm_error(get_name(),$sformatf("Default Read mismatch for CP_RISC_HOST_GP_REG_4, Exp_Data = 32'h0 and Data Read=%0h", rd_val));
	      pciememrd(32'h00ac, rd_val);//CSR_32:CP_RISC_HOST_GP_REG_5
	      if(rd_val !== 32'h0) 
                   `uvm_error(get_name(),$sformatf("Default Read mismatch for CP_RISC_HOST_GP_REG_5, Exp_Data = 32'h0 and Data Read=%0h", rd_val));
	      pciememrd(32'h00b0, rd_val);//CSR_33:CP_RISC_HOST_GP_REG_6
	      if(rd_val !== 32'h0) 
                   `uvm_error(get_name(),$sformatf("Default Read mismatch for CP_RISC_HOST_GP_REG_6, Exp_Data = 32'h0 and Data Read=%0h", rd_val));
	      pciememrd(32'h00b4, rd_val);//CSR_34:CP_RISC_HOST_GP_REG_7
	      if(rd_val !== 32'h0) 
                   `uvm_error(get_name(),$sformatf("Default Read mismatch for CP_RISC_HOST_GP_REG_7, Exp_Data = 32'h0 and Data Read=%0h", rd_val));
	      #1000ns;
              `uvm_info("apr_pcie_shared_csr_rw_seq", "Write and then Read!!", UVM_NONE);
	      reg_data = 32'hFFFF_FFFF;
	      pciememwr(32'h0000, reg_data);//CSR_0 :CP_CAP_REG
	      pciememwr(32'h0004, reg_data);//CSR_1 :CP_PCIE_CTRL
	      pciememwr(32'h0010, reg_data);//CSR_3 :CP_PCIE_STATUS
	      pciememwr(32'h0014, reg_data);//CSR_4 :CP_PCIE_DBI_CTRL
	      pciememwr(32'h0018, reg_data);//CSR_5 :CP_PCIE_RST_STATUS
	      pciememwr(32'h001c, reg_data);//CSR_6 :CP_HOST_INT_VECTOR
	      pciememwr(32'h0020, reg_data);//CSR_7 :CP_HOST_INT_PENDING
	      pciememwr(32'h0024, reg_data);//CSR_8 :CP_DMA_ERR_GEN
	      pciememwr(32'h002c, reg_data);//CSR_10:CP_Host_RISC_Ctrl
	      pciememwr(32'h0030, reg_data);//CSR_11:CP_RISC_GP
	      reg_data = 32'hFFFF_7FFF;
	      pciememwr(32'h0034, reg_data);//CSR_12:CP_GP_IO_Reg
	      reg_data = 32'hFFA5_FFFF;
	      pciememwr(32'h0008, reg_data);//CSR_2 :CP_LED_GP_STRAP_PINS
	      reg_data = 32'hFFFF_FFFF;
	      pciememwr(32'h0038, reg_data);//CSR_13:CP_Queue_Reg01
	      pciememwr(32'h0040, reg_data);//CSR_14:CP_Queue_Reg02
	      pciememwr(32'h0048, reg_data);//CSR_15:CP_Queue_Reg03
	      pciememwr(32'h0050, reg_data);//CSR_16:CP_Queue_Reg04
	      pciememwr(32'h003c, reg_data);//CSR_17:CP_Queue_Reg11
	      pciememwr(32'h0044, reg_data);//CSR_18:CP_Queue_Reg12
	      pciememwr(32'h004c, reg_data);//CSR_19:CP_Queue_Reg13
	      pciememwr(32'h0054, reg_data);//CSR_20:CP_Queue_Reg14
	      pciememwr(32'h0068, reg_data);//CSR_21:CP_Queue_Status_0
	      pciememwr(32'h006c, reg_data);//CSR_22:CP_Queue_Status_1
	      pciememwr(32'h0070, reg_data);//CSR_23:CP_Queue_Status_2
	      pciememwr(32'h0074, reg_data);//CSR_24:CP_Queue_Status_3
	      pciememwr(32'h0078, reg_data);//CSR_21:CP_DMA_ERR_CNT_0
	      pciememwr(32'h007c, reg_data);//CSR_22:CP_DMA_ERR_CNT_1
	      pciememwr(32'h0080, reg_data);//CSR_23:CP_DMA_ERR_CNT_2
	      pciememwr(32'h0084, reg_data);//CSR_24:CP_DMA_ERR_CNT_3
	      pciememwr(32'h0088, reg_data);//CSR_25:CP_DMA_ERR_CNT_4
	      pciememwr(32'h008c, reg_data);//CSR_26:CP_DMA_ERR_CNT_5
	      pciememwr(32'h0098, reg_data);//CSR_27:CP_RISC_HOST_GP_REG_0
	      pciememwr(32'h009c, reg_data);//CSR_28:CP_RISC_HOST_GP_REG_1
	      pciememwr(32'h00a0, reg_data);//CSR_29:CP_RISC_HOST_GP_REG_2
	      pciememwr(32'h00a4, reg_data);//CSR_30:CP_RISC_HOST_GP_REG_3
	      pciememwr(32'h00a8, reg_data);//CSR_31:CP_RISC_HOST_GP_REG_4
	      pciememwr(32'h00ac, reg_data);//CSR_32:CP_RISC_HOST_GP_REG_5
	      pciememwr(32'h00b0, reg_data);//CSR_33:CP_RISC_HOST_GP_REG_6
	      pciememwr(32'h00b4, reg_data);//CSR_34:CP_RISC_HOST_GP_REG_7
	      #1000ns;
	      `uvm_info(get_full_name(), $sformatf("Generating MRd_32 for Modified Register Access :: "), UVM_NONE);
	      pciememrd(32'h0000, rd_val);//CSR_0 :CP_CAP_REG
	      if(rd_val !== 32'h2) 
                    `uvm_error(get_name(),$sformatf("Modified Read mismatch for CP_CAP_REG, Exp_Data = 32'h2 and Data Read=%0h", rd_val));
	      pciememrd(32'h0004, rd_val);//CSR_1 :CP_PCIE_CTRL
	      if(rd_val !== 32'hE0E0_E0E0) 
                    `uvm_error(get_name(),$sformatf("Modified Read mismatch for CP_PCIE_CTRL, Exp_Data = 32'hE0E0_E0E0 and Data Read=%0h", rd_val));
	      pciememrd(32'h0008, rd_val);//CSR_2 :CP_LED_GP_STRAP_PINS
	      if(rd_val !== 32'h0000_0000) 
                    `uvm_error(get_name(),$sformatf("Modified Read mismatch for CP_LED_GP_STRAP_PINS, Exp_Data = 32'h0025_0000 and Data Read=%0h", rd_val));
	      pciememrd(32'h0010, rd_val);//CSR_3 :CP_PCIE_STATUS
	      if(rd_val[5:0] !== 6'h11) 
                    `uvm_error(get_name(),$sformatf("Modified Read mismatch for CP_PCIE_STATUS, Exp_Data = 6'h11 and Data Read=%0h", rd_val[5:0]));
	      pciememrd(32'h0014, rd_val);//CSR_4 :CP_PCIE_DBI_CTRL
	     // if(rd_val !== 32'h80)// Temporary fix till DBI state_machine starts working 
	     if(rd_val !== 32'h00)// bit 7 has default value '0' for pcie and '1' for processor 
                    `uvm_error(get_name(),$sformatf("Modified Read mismatch for CP_PCIE_DBI_CTRL, Exp_Data = 32'h00 and Data Read=%0h", rd_val));
	      pciememrd(32'h0018, rd_val);//CSR_5 :CP_PCIE_RST_STATUS
	      pciememrd(32'h001c, rd_val);//CSR_6 :CP_HOST_INT_VECTOR
	      if(rd_val !== 32'h7F) 
                    `uvm_error(get_name(),$sformatf("Modified Read mismatch for CP_HOST_INT_VECTOR, Exp_Data = 32'h0 and Data Read=%0h", rd_val));
	      pciememrd(32'h0020, rd_val);//CSR_7 :CP_HOST_INT_PENDING
	      pciememrd(32'h0024, rd_val);//CSR_8 :CP_DMA_ERR_GEN
	      if(rd_val !== 32'h0) 
                    `uvm_error(get_name(),$sformatf("Modified Read mismatch for CP_DMA_ERR_GEN, Exp_Data = 32'h0 and Data Read=%0h", rd_val));
	      pciememrd(32'h002c, rd_val);//CSR_10:CP_Host_RISC_Ctrl
	      if(rd_val !== 32'h0) //Reset status CSR with RW/1C field
                    `uvm_error(get_name(),$sformatf("Modified Read mismatch for CP_Host_RISC_Ctrl, Exp_Data = 32'h0 and Data Read=%0h", rd_val));
	      pciememrd(32'h0030, rd_val);//CSR_11:CP_RISC_GP
	      if(rd_val !== 32'hFFFF_FFFF)
                    `uvm_error(get_name(),$sformatf("Modified Read mismatch for CP_RISC_GP, Exp_Data = 32'h0 and Data Read=%0h", rd_val));
	      pciememrd(32'h0034, rd_val);//CSR_12:CP_GP_IO_Reg
	      if(rd_val !== 32'h1F00_7F7F) //bit 28:24 are RW for pcie 
                    `uvm_error(get_name(),$sformatf("Modified Read mismatch for CP_GP_IO_Reg, Exp_Data = 32'h1F00_7F7F and Data Read=%0h", rd_val));

	      pciememrd(32'h0038, rd_val);//CSR_13:CP_Queue_Reg01
	      pciememrd(32'h0040, rd_val);//CSR_14:CP_Queue_Reg02
	      pciememrd(32'h0048, rd_val);//CSR_15:CP_Queue_Reg03
	      pciememrd(32'h0050, rd_val);//CSR_16:CP_Queue_Reg04

	      pciememrd(32'h003c, rd_val);//CSR_17:CP_Queue_Reg11
	      pciememrd(32'h0044, rd_val);//CSR_18:CP_Queue_Reg12
	      pciememrd(32'h004c, rd_val);//CSR_19:CP_Queue_Reg13
	      pciememrd(32'h0054, rd_val);//CSR_20:CP_Queue_Reg14

	      pciememrd(32'h0068, rd_val);//CSR_21:CP_Queue_Status_0
	      pciememrd(32'h006c, rd_val);//CSR_22:CP_Queue_Status_1
	      pciememrd(32'h0070, rd_val);//CSR_23:CP_Queue_Status_2
	      pciememrd(32'h0074, rd_val);//CSR_24:CP_Queue_Status_3

	      pciememrd(32'h0078, rd_val);//CSR_21:CP_DMA_ERR_CNT_0
	      if(rd_val !== 32'h000F_FFFF) 
                   `uvm_error(get_name(),$sformatf("Read mismatch for CP_DMA_ERR_CNT_0, Exp_Data = 32'h000F_FFFF and Data Read=%0h", rd_val));
	      pciememrd(32'h007c, rd_val);//CSR_22:CP_DMA_ERR_CNT_1
	      if(rd_val !== 32'h000F_FFFF) 
                   `uvm_error(get_name(),$sformatf("Read mismatch for CP_DMA_ERR_CNT_1, Exp_Data = 32'h000F_FFFF and Data Read=%0h", rd_val));
	      pciememrd(32'h0080, rd_val);//CSR_23:CP_DMA_ERR_CNT_2
	      if(rd_val !== 32'h000F_FFFF) 
                   `uvm_error(get_name(),$sformatf("Read mismatch for CP_DMA_ERR_CNT_2, Exp_Data = 32'h000F_FFFF and Data Read=%0h", rd_val));
	      pciememrd(32'h0084, rd_val);//CSR_24:CP_DMA_ERR_CNT_3
	      if(rd_val !== 32'h000F_FFFF) 
                   `uvm_error(get_name(),$sformatf("Read mismatch for CP_DMA_ERR_CNT_3, Exp_Data = 32'h000F_FFFF and Data Read=%0h", rd_val));
	      pciememrd(32'h0088, rd_val);//CSR_25:CP_DMA_ERR_CNT_4
	      if(rd_val !== 32'h000F_FFFF) 
                   `uvm_error(get_name(),$sformatf("Read mismatch for CP_DMA_ERR_CNT_4, Exp_Data = 32'h000F_FFFF and Data Read=%0h", rd_val));
	      pciememrd(32'h008c, rd_val);//CSR_26:CP_DMA_ERR_CNT_5
	      if(rd_val !== 32'h000F_FFFF) 
                   `uvm_error(get_name(),$sformatf("Read mismatch for CP_DMA_ERR_CNT_5, Exp_Data = 32'h000F_FFFF and Data Read=%0h", rd_val));

	      pciememrd(32'h0098, rd_val);//CSR_27:CP_RISC_HOST_GP_REG_0
	      if(rd_val !== 32'hFFFF_FFFF) 
                   `uvm_error(get_name(),$sformatf("Read mismatch for CP_RISC_HOST_GP_REG_0, Exp_Data = 32'hFFFF_FFFF and Data Read=%0h", rd_val));
	      pciememrd(32'h009c, rd_val);//CSR_28:CP_RISC_HOST_GP_REG_1
	      if(rd_val !== 32'hFFFF_FFFF) 
                   `uvm_error(get_name(),$sformatf("Read mismatch for CP_RISC_HOST_GP_REG_1, Exp_Data = 32'hFFFF_FFFF and Data Read=%0h", rd_val));
	      pciememrd(32'h00a0, rd_val);//CSR_29:CP_RISC_HOST_GP_REG_2
	      if(rd_val !== 32'hFFFF_FFFF) 
                   `uvm_error(get_name(),$sformatf("Read mismatch for CP_RISC_HOST_GP_REG_2, Exp_Data = 32'hFFFF_FFFF and Data Read=%0h", rd_val));
	      pciememrd(32'h00a4, rd_val);//CSR_30:CP_RISC_HOST_GP_REG_3
	      if(rd_val !== 32'hFFFF_FFFF) 
                   `uvm_error(get_name(),$sformatf("Read mismatch for CP_RISC_HOST_GP_REG_3, Exp_Data = 32'hFFFF_FFFF and Data Read=%0h", rd_val));
	      pciememrd(32'h00a8, rd_val);//CSR_31:CP_RISC_HOST_GP_REG_4
	      if(rd_val !== 32'hFFFF_FFFF) 
                   `uvm_error(get_name(),$sformatf("Read mismatch for CP_RISC_HOST_GP_REG_4, Exp_Data = 32'hFFFF_FFFF and Data Read=%0h", rd_val));
	      pciememrd(32'h00ac, rd_val);//CSR_32:CP_RISC_HOST_GP_REG_5
	      if(rd_val !== 32'hFFFF_FFFF) 
                   `uvm_error(get_name(),$sformatf("Read mismatch for CP_RISC_HOST_GP_REG_5, Exp_Data = 32'hFFFF_FFFF and Data Read=%0h", rd_val));
	      pciememrd(32'h00b0, rd_val);//CSR_33:CP_RISC_HOST_GP_REG_6
	      if(rd_val !== 32'hFFFF_FFFF) 
                   `uvm_error(get_name(),$sformatf("Read mismatch for CP_RISC_HOST_GP_REG_6, Exp_Data = 32'hFFFF_FFFF and Data Read=%0h", rd_val));
	      pciememrd(32'h00b4, rd_val);//CSR_34:CP_RISC_HOST_GP_REG_7
	      if(rd_val !== 32'hFFFF_FFFF) 
                   `uvm_error(get_name(),$sformatf("Read mismatch for CP_RISC_HOST_GP_REG_7, Exp_Data = 32'hFFFF_FFFF and Data Read=%0h", rd_val));
	      #1000ns;
   endtask : body
endclass : apr_pcie_shared_csr_rw_seq

class apr_pcie_bit_bash_csr_rw_seq extends apr_pcie_tlp_seq;
      rand reg [31:0] reg_data;
      reg [31:0] exp_data, exp_data_1;
      reg [31:0] rd_val;
   `uvm_object_utils(apr_pcie_bit_bash_csr_rw_seq)
`define MASK_CP_CAP_REG 32'h2
`define MASK_CP_PCIE_CTRL 32'hE0E0_E0E0
`define MASK_CP_LED_GP_STRAP_PINS 32'h0
`define MASK_CP_PCIE_STATUS 32'h0 //READ_ONLY
`define MASK_CP_PCIE_DBI_CTRL 32'h0 //READ_ONLY
`define MASK_CP_PCIE_RST_STATUS 32'h0 //READ_ONLY
`define MASK_CP_HOST_INT_VECTOR 32'h7F
`define MASK_CP_HOST_INT_PENDING 32'h0 //READ_ONLY
`define MASK_CP_DMA_ERR_GEN 32'h0  //RW1C_RSVD
`define MASK_CP_HOST_RISC_CTRL 32'h0  //RW1C_RSVD
`define MASK_CP_RISC_GP 32'hFFFF_FFFF
`define MASK_CP_GP_IO_REG 32'h1F00_FFFF
`define MASK_CP_QX_REG_1 32'hFFFC_FFFF
`define MASK_CP_QX_REG_2 32'hFFFF_FFFF
`define MASK_CP_Q_STS_X 32'h3000_0000
`define MASK_CP_DMA_ERR_CNT_X 32'hF_FFFF
`define MASK_CP_RISC_HOST_GP_REG_X 32'hFFFF_FFFF

     string reg_name;
     reg[31:0] reg_mask;
constraint pattern_data {
           reg_data inside {32'hFFFF_FFFF, 32'hAAAA_AAAA, 32'h5555_5555, 32'h6666_6666, 32'h9999_9999, 32'h0000_0000};
	   };


   function new(string name = "apr_pcie_bit_bash_csr_rw_seq");
      super.new(name);
   endfunction : new

   //Default value check is already covered in some other sequence so here focus is only of RW attribute
   virtual task body();
               super.body();
              `uvm_info("apr_pcie_bit_bash_csr_rw_seq", "body::Bit_bash Read Write and compare!!", UVM_NONE);
	      check_dma_queue_reg_pairs();
	      check_reg_attrib(32'h0098);//CSR_27:CP_RISC_HOST_GP_REG_0
	      check_reg_attrib(32'h009c);//CSR_28:CP_RISC_HOST_GP_REG_1
	      check_reg_attrib(32'h00a0);//CSR_29:CP_RISC_HOST_GP_REG_2
	      check_reg_attrib(32'h00a4);//CSR_30:CP_RISC_HOST_GP_REG_3
	      check_reg_attrib(32'h00a8);//CSR_31:CP_RISC_HOST_GP_REG_4
	      check_reg_attrib(32'h00ac);//CSR_32:CP_RISC_HOST_GP_REG_5
	      check_reg_attrib(32'h00b0);//CSR_33:CP_RISC_HOST_GP_REG_6
	      check_reg_attrib(32'h00b4);//CSR_34:CP_RISC_HOST_GP_REG_7
	      check_reg_attrib(32'h0078);//CSR_21:CP_DMA_ERR_CNT_0
	      check_reg_attrib(32'h007c);//CSR_22:CP_DMA_ERR_CNT_1
	      check_reg_attrib(32'h0080);//CSR_23:CP_DMA_ERR_CNT_2
	      check_reg_attrib(32'h0084);//CSR_24:CP_DMA_ERR_CNT_3
	      check_reg_attrib(32'h0088);//CSR_25:CP_DMA_ERR_CNT_4
	      check_reg_attrib(32'h008c);//CSR_26:CP_DMA_ERR_CNT_5
	      check_reg_attrib(32'h0068);//CSR_21:CP_Queue_Status_0 //BUG
	      check_reg_attrib(32'h006c);//CSR_22:CP_Queue_Status_1
	      check_reg_attrib(32'h0070);//CSR_23:CP_Queue_Status_2
	      check_reg_attrib(32'h0074);//CSR_24:CP_Queue_Status_3
	      check_reg_attrib(32'h0034);//CSR_12:CP_GP_IO_Reg
	      check_reg_attrib(32'h0030);//CSR_11:CP_RISC_GP_Reg
	      check_reg_attrib(32'h001C);//CSR_6 :CP_HOST_INT_VECTOR_Reg
	      check_reg_attrib(32'h0020);//CSR_7 :CP_HOST_INT_PENDIN_Reg
	      check_reg_attrib(32'h0008);//CSR_2 :CP_LED_GP_STRAP_PINS
	      check_reg_attrib(32'h0010);//CSR_3 :CP_PCIE_STATUS_Reg
	      check_reg_attrib(32'h0014);//CSR_4 :CP_PCIE_DBI_CTRL_Reg
	      check_reg_attrib(32'h0018);//CSR_5 :CP_PCIE_RST_STATUS_Reg
	      check_reg_attrib(32'h0024);//CSR_8 :CP_DMA_ERR_GEN_Reg
	      check_reg_attrib(32'h002C);//CSR_10:CP_Host_RISC_Ctrl_Reg
	      check_reg_attrib(32'h0000);//CSR_0 :CP_CAP_REG
	      check_reg_attrib(32'h0004);//CSR_1 :CP_PCIE_CTRL_Reg
   endtask : body


  virtual task check_reg_attrib(input reg [31:0] addr);
      reg [31:0] def_val;
     check_reg_name_n_mask(addr);
     pciememrd(addr, def_val);
     randomize(reg_data);
     pciememwr(addr, reg_data);
     #500ns;
     exp_data_1 = reg_data & reg_mask;
     exp_data = exp_data_1 | (def_val & ~reg_mask);
     pciememrd(addr, rd_val);
     if(((addr == 32'h68) | (addr == 32'h6C) | (addr == 32'h70) | (addr == 32'h74)) & reg_data[29:28] == 2'b11) exp_data = 32'h0; //Reset the queues
     if((addr == 32'h1c) & (reg_data[6] == 'b0)) exp_data = 32'h0; // Clear all pending msi interrupt
     if(rd_val !== exp_data) 
           `uvm_error(get_name(),$sformatf("Read mismatch for %s, Def_data = %0h, Data_pattern = %0h, Exp_Data = %0h and Data Read=%0h", reg_name,def_val,reg_data,exp_data, rd_val));
     if(rd_val !== def_val) 
     pciememwr(addr, def_val); //restoring old value
  endtask : check_reg_attrib

  function void check_reg_name_n_mask(input reg [31:0] reg_addr);

   case (reg_addr) 
         32'h0000	: begin 
	                      reg_name = "CP_CAP_REG_Reg"; 
			      reg_mask = `MASK_CP_CAP_REG;
			  end
         32'h0004	: begin 
	                      reg_name = "CP_PCIE_CTRL_Reg"; 
			      reg_mask = `MASK_CP_PCIE_CTRL;
			  end
         32'h0008	: begin 
	                      reg_name = "CP_LED_GP_STRAP_PINS_Reg"; 
			      reg_mask = `MASK_CP_LED_GP_STRAP_PINS;
			  end
         32'h0010	: begin 
	                      reg_name = "CP_PCIE_STATUS_Reg"; 
			      reg_mask = `MASK_CP_PCIE_STATUS;
			  end
         32'h0014	: begin 
	                      reg_name = "CP_PCIE_DBI_CTRL_Reg"; 
			      reg_mask = `MASK_CP_PCIE_DBI_CTRL;
			  end
         32'h0018	: begin 
	                      reg_name = "CP_PCIE_RST_STATUS_Reg"; 
			      reg_mask = `MASK_CP_PCIE_RST_STATUS;
			  end
         32'h001C	: begin 
	                      reg_name = "CP_HOST_INT_VECTOR_Reg"; 
			      reg_mask = `MASK_CP_HOST_INT_VECTOR;
			  end
         32'h0020	: begin 
	                      reg_name = "CP_HOST_INT_PENDING_Reg"; 
			      reg_mask = `MASK_CP_HOST_INT_PENDING;
			  end
         32'h0024	: begin 
	                      reg_name = "CP_DMA_ERR_GEN_Reg"; 
			      reg_mask = `MASK_CP_DMA_ERR_GEN;
			  end
         32'h002C	: begin 
	                      reg_name = "CP_Host_RISC_Ctrl_Reg"; 
			      reg_mask = `MASK_CP_HOST_RISC_CTRL;
			  end
         32'h0030	: begin 
	                      reg_name = "CP_RISC_GP_Reg"; 
			      reg_mask = `MASK_CP_RISC_GP;
			  end
         32'h0034	: begin 
	                      reg_name = "CP_GP_IO_Reg"; 
			      reg_mask = `MASK_CP_GP_IO_REG;
			  end
         32'h0098	: begin 
	                      reg_name = "CP_RISC_HOST_GP_REG_0"; 
			      reg_mask = `MASK_CP_RISC_HOST_GP_REG_X;
			  end
         32'h009C	: begin 
	                      reg_name = "CP_RISC_HOST_GP_REG_1"; 
			      reg_mask = `MASK_CP_RISC_HOST_GP_REG_X;
			  end
         32'h00A0	: begin 
	                      reg_name = "CP_RISC_HOST_GP_REG_2"; 
			      reg_mask = `MASK_CP_RISC_HOST_GP_REG_X;
			  end
         32'h00A4	: begin 
	                      reg_name = "CP_RISC_HOST_GP_REG_3"; 
			      reg_mask = `MASK_CP_RISC_HOST_GP_REG_X;
			  end
         32'h00A8	: begin 
	                      reg_name = "CP_RISC_HOST_GP_REG_4"; 
			      reg_mask = `MASK_CP_RISC_HOST_GP_REG_X;
			  end
         32'h00AC	: begin 
	                      reg_name = "CP_RISC_HOST_GP_REG_5"; 
			      reg_mask = `MASK_CP_RISC_HOST_GP_REG_X;
			  end
         32'h00B0	: begin 
	                      reg_name = "CP_RISC_HOST_GP_REG_6"; 
			      reg_mask = `MASK_CP_RISC_HOST_GP_REG_X;
			  end
         32'h00B4	: begin 
	                      reg_name = "CP_RISC_HOST_GP_REG_7"; 
			      reg_mask = `MASK_CP_RISC_HOST_GP_REG_X;
			  end
         32'h0078	: begin 
	                      reg_name = "CP_DMA_ERR_CNT_0"; 
			      reg_mask = `MASK_CP_DMA_ERR_CNT_X;
			  end
         32'h007C	: begin 
	                      reg_name = "CP_DMA_ERR_CNT_1"; 
			      reg_mask = `MASK_CP_DMA_ERR_CNT_X;
			  end
         32'h0080	: begin 
	                      reg_name = "CP_DMA_ERR_CNT_2"; 
			      reg_mask = `MASK_CP_DMA_ERR_CNT_X;
			  end
         32'h0084	: begin 
	                      reg_name = "CP_DMA_ERR_CNT_3"; 
			      reg_mask = `MASK_CP_DMA_ERR_CNT_X;
			  end
         32'h0088	: begin 
	                      reg_name = "CP_DMA_ERR_CNT_4"; 
			      reg_mask = `MASK_CP_DMA_ERR_CNT_X;
			  end
         32'h008C	: begin 
	                      reg_name = "CP_DMA_ERR_CNT_5"; 
			      reg_mask = `MASK_CP_DMA_ERR_CNT_X;
			  end
         32'h0068	: begin 
	                      reg_name = "CP_Queue_Status_0"; 
			      reg_mask = `MASK_CP_Q_STS_X;
			  end
         32'h006C	: begin 
	                      reg_name = "CP_Queue_Status_1"; 
			      reg_mask = `MASK_CP_Q_STS_X;
			  end
         32'h0070	: begin 
	                      reg_name = "CP_Queue_Status_2"; 
			      reg_mask = `MASK_CP_Q_STS_X;
			  end
         32'h0074	: begin 
	                      reg_name = "CP_Queue_Status_3"; 
			      reg_mask = `MASK_CP_Q_STS_X;
			  end
         default 	: begin 
                          `uvm_error(get_name(),$sformatf("No Register Name and Mask avialble for Address =%0h", reg_addr));
			  end
     endcase
  endfunction : check_reg_name_n_mask

  virtual task check_dma_queue_reg_pairs();
              randomize(reg_data);
	      pciememwr(32'h0038, reg_data);//CSR_13:CP_Queue_Reg01
	      pciememwr(32'h0040, reg_data);//CSR_14:CP_Queue_Reg02
	      pciememwr(32'h0048, reg_data);//CSR_15:CP_Queue_Reg03
	      pciememwr(32'h0050, reg_data);//CSR_16:CP_Queue_Reg04
	      pciememwr(32'h003c, reg_data);//CSR_17:CP_Queue_Reg11
	      pciememwr(32'h0044, reg_data);//CSR_18:CP_Queue_Reg12
	      pciememwr(32'h004c, reg_data);//CSR_19:CP_Queue_Reg13
	      pciememwr(32'h0054, reg_data);//CSR_20:CP_Queue_Reg14
	      exp_data = reg_data & `MASK_CP_QX_REG_1;
	      pciememrd(32'h0038, rd_val);//CSR_13:CP_Queue_Reg01
	      if(rd_val !== exp_data) 
                    `uvm_error(get_name(),$sformatf("Read mismatch for CP_Queue_Reg01, Exp_Data = 32'h0 and Data Read=%0h", rd_val));
	      pciememrd(32'h0040, rd_val);//CSR_13:CP_Queue_Reg02
	      if(rd_val !== exp_data) 
                    `uvm_error(get_name(),$sformatf("Read mismatch for CP_Queue_Reg02, Exp_Data = 32'h0 and Data Read=%0h", rd_val));
	      pciememrd(32'h0048, rd_val);//CSR_13:CP_Queue_Reg03
	      if(rd_val !== exp_data) 
                    `uvm_error(get_name(),$sformatf("Read mismatch for CP_Queue_Reg03, Exp_Data = 32'h0 and Data Read=%0h", rd_val));
	      pciememrd(32'h0050, rd_val);//CSR_13:CP_Queue_Reg04
	      if(rd_val !== exp_data) 
                    `uvm_error(get_name(),$sformatf("Read mismatch for CP_Queue_Reg04, Exp_Data = 32'h0 and Data Read=%0h", rd_val));
   
              exp_data = reg_data & `MASK_CP_QX_REG_2;
	      pciememrd(32'h003c, rd_val);//CSR_17:CP_Queue_Reg11
	      if(rd_val !== exp_data) 
                    `uvm_error(get_name(),$sformatf("Read mismatch for CP_Queue_Reg11, Exp_Data = 32'h0 and Data Read=%0h", rd_val));
	      pciememrd(32'h0044, rd_val);//CSR_17:CP_Queue_Reg12
	      if(rd_val !== exp_data) 
                    `uvm_error(get_name(),$sformatf("Read mismatch for CP_Queue_Reg12, Exp_Data = 32'h0 and Data Read=%0h", rd_val));
	      pciememrd(32'h004c, rd_val);//CSR_17:CP_Queue_Reg13
	      if(rd_val !== exp_data) 
                    `uvm_error(get_name(),$sformatf("Read mismatch for CP_Queue_Reg13, Exp_Data = 32'h0 and Data Read=%0h", rd_val));
	      pciememrd(32'h0054, rd_val);//CSR_17:CP_Queue_Reg14
	      if(rd_val !== exp_data) 
                    `uvm_error(get_name(),$sformatf("Read mismatch for CP_Queue_Reg14, Exp_Data = 32'h0 and Data Read=%0h", rd_val));

	      reg_data = 32'h0; //restoring default values
              pciememwr(32'h0038, reg_data);//CSR_13:CP_Queue_Reg01
	      pciememwr(32'h0040, reg_data);//CSR_14:CP_Queue_Reg02
	      pciememwr(32'h0048, reg_data);//CSR_15:CP_Queue_Reg03
	      pciememwr(32'h0050, reg_data);//CSR_16:CP_Queue_Reg04
	      pciememwr(32'h003c, reg_data);//CSR_17:CP_Queue_Reg11
	      pciememwr(32'h0044, reg_data);//CSR_18:CP_Queue_Reg12
	      pciememwr(32'h004c, reg_data);//CSR_19:CP_Queue_Reg13
	      pciememwr(32'h0054, reg_data);//CSR_20:CP_Queue_Reg14
  endtask : check_dma_queue_reg_pairs 
endclass : apr_pcie_bit_bash_csr_rw_seq

// This implements a TLP sequence.
class apr_pcie_rd_wr_tlp_seq extends pcie_sequence;

   DenaliSvPcie::denaliPcieTlpMemPacket rdTlp;
   DenaliSvPcie::denaliPcieTlpMemPacket wrTlp;

   `uvm_object_utils_begin(apr_pcie_rd_wr_tlp_seq)
      `uvm_field_object(wrTlp, UVM_ALL_ON)
      `uvm_field_object(rdTlp, UVM_ALL_ON)
   `uvm_object_utils_end
  
   `uvm_declare_p_sequencer(pcie_sequencer)

   function new(string name = "apr_pcie_rd_wr_tlp_seq");
      super.new(name);
   endfunction : new

   // Set the source and destination configuration spaces, and the packet
   // and TLP types. Also enable the Easy Mode constraint.
   virtual task pre_do(bit is_item);
      pcie_sequencer pcie_seqr;

      if (rdTlp) begin
         rdTlp.pktType = DenaliSvPcie::DENALI_PCIE__Tlp;
         rdTlp.tlpType = DenaliSvPcie::DENALI_PCIE_TL_MRd_32;
         rdTlp.easyModeTlpConstraint.constraint_mode(1);
         if ($cast(pcie_seqr,get_sequencer())) begin
            rdTlp.srcConfig = pcie_seqr.srcConfig;
            rdTlp.dstConfig = pcie_seqr.dstConfig;
         end
      end else if (wrTlp) begin
         wrTlp.pktType = DenaliSvPcie::DENALI_PCIE__Tlp;
         wrTlp.tlpType = DenaliSvPcie::DENALI_PCIE_TL_MWr_32;
         wrTlp.easyModeTlpConstraint.constraint_mode(1);
         if ($cast(pcie_seqr,get_sequencer())) begin
            wrTlp.srcConfig = pcie_seqr.srcConfig;
            wrTlp.dstConfig = pcie_seqr.dstConfig;
         end
      end
   endtask : pre_do

   // Perform Memory Read After Memory Write
   virtual task body();
      `uvm_info("apr_pcie_rd_wr_tlp_seq", "Starting pcie_TLWriteReadSeq sequence", UVM_HIGH);
      //`uvm_info("apr_pcie_rd_wr_tlp_seq", "Default Read : Drive MRd_32", UVM_HIGH);
      //`uvm_do_with(rdTlp,{rdTlp.address == 29'h1000_0000;rdTlp.length == 1;rdTlp.firstBe == 4'hf;rdTlp.lastBe == 4'h0;})
      //`uvm_info("apr_pcie_rd_wr_tlp_seq", $sformatf("\nDefault Driving TLP:\n%s", rdTlp.sprintInfo()), UVM_HIGH);
      //#300ns;
      `uvm_info("apr_pcie_rd_wr_tlp_seq", "Drive MWr_32", UVM_HIGH);
      `uvm_do_with(wrTlp,{wrTlp.address == 29'h1000_0000;wrTlp.length == 1;wrTlp.firstBe == 4'hf;wrTlp.lastBe == 4'h0;})
      `uvm_info("apr_pcie_rd_wr_tlp_seq", $sformatf("\nDriving TLP:\n%s", wrTlp.sprintInfo()), UVM_HIGH);
      #100ns;
      `uvm_info("apr_pcie_rd_wr_tlp_seq", "Drive MRd_32", UVM_HIGH);
      `uvm_do_with(rdTlp,{rdTlp.address == 29'h1000_0000;rdTlp.length == 1;rdTlp.firstBe == 4'hf;rdTlp.lastBe == 4'h0;})
      `uvm_info("apr_pcie_rd_wr_tlp_seq", $sformatf("\nDriving TLP:\n%s", rdTlp.sprintInfo()), UVM_HIGH);
      //DO: Remove later
      #20000ns;
   endtask : body
endclass : apr_pcie_rd_wr_tlp_seq


`endif /* __PCIE_SEQ_LIB_SV__ */

// -------------------------------------------------------------------
//
