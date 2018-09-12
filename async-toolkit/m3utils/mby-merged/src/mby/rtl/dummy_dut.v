module dummy_dut();
 
`ifdef UVM 
  import uvm_pkg::*;
`else
  import ovm_pkg::*;
`endif
// START IOSF_NOT_PRESENT
//  import IosfPkg::*;
// END IOSF_NOT_PRESENT

  
  // Parameters for interfaces
`include "mby_params.sv"
`ifdef UVM
`include "uvm_macros.svh"
`else
`include "ovm_macros.svh"
`endif
  

// START IOSF_NOT_PRESENT
//  iosf_primary_intf #(`MBY_IOSF_PRI_PARAMS) iosf_dut_pri_if();
// END IOSF_NOT_PRESENT
  
  // IOSF SideBand interface
  // ===============================================
// START IOSF_NOT_PRESENT
//`ifdef IOSF_SB_PH2
//  iosf_sbc_intf  iosf_dut_sb_if(.side_clk(`MBY_TOP_PATH.secondary_clock), 
//                                .side_rst_b(`MBY_TOP_PATH.mby_if.secondary_reset),
//                                .gated_side_clk(),
//                                .agent_rst_b(`MBY_TOP_PATH.mby_if.secondary_reset));
//`else
//  
//  iosf_sbc_intf  #(.PAYLOAD_WIDTH(8),
//                               .AGENT_MASTERING_SB_IF(1)) iosf_dut_sb_if(.side_clk(`MBY_TOP_PATH.secondary_clock), 
//									      .side_rst_b(`MBY_TOP_PATH.mby_if.secondary_reset),
//									      .gated_side_clk(),
//									      .agent_rst_b(`MBY_TOP_PATH.mby_if.secondary_reset));
//`endif
// END IOSF_NOT_PRESENT

  
// START IOSF_NOT_PRESENT
//    assign iosf_dut_pri_if.cmd_chid         = `MBY_TOP.psf_mby_cmd_chid;
//    assign iosf_dut_pri_if.cmd_put          = `MBY_TOP.psf_mby_cmd_put;
//    assign iosf_dut_pri_if.cmd_rtype        = `MBY_TOP.psf_mby_cmd_rtype;
//    assign iosf_dut_pri_if.gnt              = `MBY_TOP.psf_mby_gnt;
//    assign iosf_dut_pri_if.gnt_chid         = `MBY_TOP.psf_mby_gnt_chid;
//    assign iosf_dut_pri_if.gnt_rtype        = `MBY_TOP.psf_mby_gnt_rtype;
//    assign iosf_dut_pri_if.gnt_type         = `MBY_TOP.psf_mby_gnt_type;
//    assign iosf_dut_pri_if.prim_clkack      = `MBY_TOP.psf_mby_prim_clkack;
//    assign iosf_dut_pri_if.prim_ism_fabric  = `MBY_TOP.psf_mby_prim_ism_fabric;
//    assign iosf_dut_pri_if.taddress         = `MBY_TOP.psf_mby_taddress;
//    assign iosf_dut_pri_if.tdata            = `MBY_TOP.psf_mby_tdata;
//    assign iosf_dut_pri_if.tdest_id         = `MBY_TOP.psf_mby_tdest_id;
//    assign iosf_dut_pri_if.tfbe             = `MBY_TOP.psf_mby_tfbe;
//    assign iosf_dut_pri_if.tfmt             = `MBY_TOP.psf_mby_tfmt;
//    assign iosf_dut_pri_if.tlbe             = `MBY_TOP.psf_mby_tlbe;
//    assign iosf_dut_pri_if.tlength          = `MBY_TOP.psf_mby_tlength;
//    assign iosf_dut_pri_if.tns              = `MBY_TOP.psf_mby_tns;
//    assign iosf_dut_pri_if.tro              = `MBY_TOP.psf_mby_tro;
//    assign iosf_dut_pri_if.trqid            = `MBY_TOP.psf_mby_trqid;
//    assign iosf_dut_pri_if.ttag             = `MBY_TOP.psf_mby_ttag;
//    assign iosf_dut_pri_if.ttc              = `MBY_TOP.psf_mby_ttc;
//    assign iosf_dut_pri_if.ttype            = `MBY_TOP.psf_mby_ttype;
//    assign iosf_dut_pri_if.tsai             = `MBY_TOP.psf_mby_tsai;
//    assign   iosf_dut_pri_if.tat              = `MBY_TOP.psf_mby_tat; 
//    assign   iosf_dut_pri_if.tep              = `MBY_TOP.psf_mby_tep; 
//
//
//  assign   iosf_dut_pri_if.prim_clk               = `MBY_TOP_PATH.primary_clock;
//  assign   iosf_dut_pri_if.powergood_rst_b        = `MBY_TOP_PATH.mby_if.power_good_reset;
//  assign   iosf_dut_pri_if.prim_rst_b             = `MBY_TOP_PATH.mby_if.primary_reset;
//  assign   iosf_dut_pri_if.msecondary_bus_rst_b   = `MBY_TOP_PATH.mby_if.primary_reset;
//  assign   iosf_dut_pri_if.tsecondary_bus_rst_b   = `MBY_TOP_PATH.mby_if.primary_reset;
//
// //outputs
// always @(`MBY_TOP_PATH.primary_clock) begin
//
//    force `MBY_TOP.mby_psf_credit_chid     = iosf_dut_pri_if.credit_chid ;
//    force `MBY_TOP.mby_psf_credit_cmd      = iosf_dut_pri_if.credit_cmd ;
//    force `MBY_TOP.mby_psf_credit_data     = iosf_dut_pri_if.credit_data ;
//    force `MBY_TOP.mby_psf_credit_put      = iosf_dut_pri_if.credit_put ;
//    force `MBY_TOP.mby_psf_credit_rtype    = iosf_dut_pri_if.credit_rtype ;
//    force `MBY_TOP.mby_psf_maddress        = iosf_dut_pri_if.maddress ;
//    force `MBY_TOP.mby_psf_mdata           = iosf_dut_pri_if.mdata ;
//    force `MBY_TOP.mby_psf_mfbe            = iosf_dut_pri_if.mfbe ;
//    force `MBY_TOP.mby_psf_mfmt            = iosf_dut_pri_if.mfmt ;
//    force `MBY_TOP.mby_psf_mlbe            = iosf_dut_pri_if.mlbe ;
//    force `MBY_TOP.mby_psf_mlength         = iosf_dut_pri_if.mlength ;
//    force `MBY_TOP.mby_psf_mns             = iosf_dut_pri_if.mns ;
//    force `MBY_TOP.mby_psf_mro             = iosf_dut_pri_if.mro ;
//    force `MBY_TOP.mby_psf_mrqid           = iosf_dut_pri_if.mrqid ;
//    force `MBY_TOP.mby_psf_mtag            = iosf_dut_pri_if.mtag ;
//    force `MBY_TOP.mby_psf_mtc             = iosf_dut_pri_if.mtc ;
//    force `MBY_TOP.mby_psf_mtype           = iosf_dut_pri_if.mtype ;
//    force `MBY_TOP.mby_psf_msai            = iosf_dut_pri_if.msai ;
//    force `MBY_TOP.mby_psf_mep            =  iosf_dut_pri_if.mep ;
//    force `MBY_TOP.mby_psf_mat            =  iosf_dut_pri_if.mat ;
//    force `MBY_TOP.mby_psf_prim_clkreq     = iosf_dut_pri_if.prim_clkreq ;
//    force `MBY_TOP.mby_psf_prim_ism_agent  = iosf_dut_pri_if.prim_ism_agent ;
//    force `MBY_TOP.mby_psf_req_cdata       = iosf_dut_pri_if.req_cdata ;
//    force `MBY_TOP.mby_psf_req_chid        = iosf_dut_pri_if.req_chid ;
//    force `MBY_TOP.mby_psf_req_dlen        = iosf_dut_pri_if.req_dlen ;
//    force `MBY_TOP.mby_psf_req_locked      = iosf_dut_pri_if.req_locked ;
//    force `MBY_TOP.mby_psf_req_ns          = iosf_dut_pri_if.req_ns ;
//    force `MBY_TOP.mby_psf_req_put         = iosf_dut_pri_if.req_put ;
//    force `MBY_TOP.mby_psf_req_ro          = iosf_dut_pri_if.req_ro ;
//    force `MBY_TOP.mby_psf_req_rtype       = iosf_dut_pri_if.req_rtype ;
//    force `MBY_TOP.mby_psf_req_tc          = iosf_dut_pri_if.req_tc ;
//
//  end // always @ (`MBY_TOP_PATH.primary_clock)
//
//   initial begin
//     @(posedge iosf_pri_if.prim_rst_b) ;
//     @(posedge iosf_dut_pri_if.powergood_rst_b);
//     force iosf_dut_pri_if.prim_pok = 1; 
//  end
//
//
//
//
//    assign iosf_dut_pri_if.tth = 0;
//   // assign iosf_dut_pri_if.tep = 0;
//    assign iosf_dut_pri_if.tido = 0;
//   // assign iosf_dut_pri_if.tat =0;
//    assign iosf_dut_pri_if.ttd   =0;
//    assign iosf_dut_pri_if.trs   =0;
//    assign iosf_dut_pri_if.tecrc =0;
//    assign iosf_dut_pri_if.cmd_nfs_err =0;
//    assign iosf_dut_pri_if.tchain =0;
//
//    assign iosf_dut_pri_if.trsvd1_7     =0;
//    assign iosf_dut_pri_if.trsvd1_3     =0;
//    assign iosf_dut_pri_if.trsvd1_1     =0;
//    assign iosf_dut_pri_if.trsvd0_7     =0;
//    assign iosf_dut_pri_if.tcparity     =0;
//    assign iosf_dut_pri_if.tdparity     =0;
//    assign iosf_dut_pri_if.tdec =0;
//    assign iosf_dut_pri_if.tdbe =0;
//    assign iosf_dut_pri_if.tbewd =0;
//    assign iosf_dut_pri_if.obff =0;
//    assign iosf_dut_pri_if.tsrc_id =0;
//    assign iosf_dut_pri_if.tecrc_error =0;
//    assign iosf_dut_pri_if.tecrc_generate =0;
//    assign   iosf_dut_pri_if.mem_closed = 0;
// END IOSF_NOT_PRESENT
  initial begin
    force    `MBY_TOP.psf_mby_tat = 0;
    force    `MBY_TOP.psf_mby_tep = 0;
    force    `MBY_TOP.psf_mby_ttag = 0;
    force    `MBY_TOP.psf_mby_trqid = 0;
    force    `MBY_TOP.psf_mby_tsai = 0;
  end

// START IOSF_NOT_PRESENT
//  //  assign iosf_dut_pri_if.tsai =0;
//
//    // Secondary IOSF connection to MBY
//    // ===============================================
//
//  assign   iosf_dut_sb_if.teom = `MBY_TOP.sb2_mby_eom ;
//  assign   iosf_dut_sb_if.mnpcup = `MBY_TOP.sb2_mby_npcup;
//  assign   iosf_dut_sb_if.tnpput = `MBY_TOP.sb2_mby_npput;
//  assign   iosf_dut_sb_if.tpayload = `MBY_TOP.sb2_mby_payload;
//  assign   iosf_dut_sb_if.mpccup = `MBY_TOP.sb2_mby_pccup;
//  assign   iosf_dut_sb_if.tpcput = `MBY_TOP.sb2_mby_pcput;
// // assign   iosf_dut_sb_if.side_clkack = `MBY_TOP.sb2_mby_side_clkack;
//  assign   iosf_dut_sb_if.side_ism_fabric = `MBY_TOP.sb2_mby_side_ism_fabric;
//  assign   iosf_dut_sb_if.side_clkack = 1;
//  always @(`MBY_TOP_PATH.secondary_clock) begin
//    force `MBY_TOP.mby_sb2_eom = iosf_dut_sb_if.meom;
//    force `MBY_TOP.mby_sb2_npcup = iosf_dut_sb_if.tnpcup;
//    force `MBY_TOP.mby_sb2_npput = iosf_dut_sb_if.mnpput;
//    force `MBY_TOP.mby_sb2_payload = iosf_dut_sb_if.mpayload;
//    force `MBY_TOP.mby_sb2_pccup = iosf_dut_sb_if.tpccup;
//    force `MBY_TOP.mby_sb2_pcput = iosf_dut_sb_if.mpcput;
//    force `MBY_TOP.mby_sb2_side_clkreq = iosf_dut_sb_if.side_clkreq;
//    force `MBY_TOP.mby_sb2_side_ism_agent = iosf_dut_sb_if.side_ism_agent;
//  end
// END IOSF_NOT_PRESENT

// START IOSF_NOT_PRESENT
//   initial begin
//     @(posedge `MBY_TOP_PATH.mby_if.secondary_reset) ;
//     @(posedge `MBY_TOP_PATH.secondary_clock)
//     force iosf_dut_sb_if.side_pok= 1;
//  end
// END IOSF_NOT_PRESENT

// START IOSF_NOT_PRESENT
//
//  iosf_primary_ti #(`MBY_IOSF_PRI_PARAMS,  // signal width parameters
//                     .IS_COMPMON (1),
//                    .IS_FABRIC  (0))
//   iosf_primary_ti  (.iosf_primary_intf (iosf_dut_pri_if));
//
//
//  `ifdef IOSF_SB_PH2
//  iosf_sb_ti #( .PAYLOAD_WIDTH(8),
//    .AGENT_MASTERING_SB_IF(1)
//  )    // signal width parameters
//    iosf_sb_ti  (.iosf_sbc_intf (iosf_dut_sb_if));
//  `else
//
//  iosf_sb_ti #( .PAYLOAD_WIDTH(8),
//    .AGENT_MASTERING_SB_IF(1),
//    .INTF_NAME("dummy_dut_sb_if")
//  )    // signal width parameters
//    iosf_sb_ti  (.iosf_sbc_intf (iosf_dut_sb_if));
//  `endif
//  IosfAgentVc    iosfAgtVc;
//
//  // Configuration descriptors
//  IosfAgtCfg   iosfAgtCfg;
//
//
//  initial begin
//    #0;
//
//   iosfAgtCfg           = new ("iosfAgtCfg");
//   // Populate configuration
//
//  iosfAgtCfg.MAX_DATA_LEN     = `MBY_MAX_DATA_LEN;  
//  iosfAgtCfg.MMAX_ADDR        = `MBY_MMAX_ADDR; 
//  iosfAgtCfg.TMAX_ADDR        = `MBY_TMAX_ADDR; 
//  iosfAgtCfg.AGENT_WIDTH      = `MBY_AGENT_WIDTH; 
//  iosfAgtCfg.MNUMCHAN         = `MBY_MNUMCHAN;  
//  iosfAgtCfg.TNUMCHAN         = `MBY_TNUMCHAN;   
//  iosfAgtCfg.MNUMCHANL2       = `MBY_MNUMCHANL2;
//  iosfAgtCfg.TNUMCHANL2       = `MBY_TNUMCHANL2; 
//  iosfAgtCfg.MD_WIDTH         = `MBY_MD_WIDTH;
//  iosfAgtCfg.TD_WIDTH         = `MBY_TD_WIDTH;
//  iosfAgtCfg.DEST_ID_WIDTH    = `MBY_DEST_ID_WIDTH;
//  iosfAgtCfg.SRC_ID_WIDTH     = `MBY_SRC_ID_WIDTH;
//  iosfAgtCfg.MSAI_WIDTH       = `MBY_MSAI_WIDTH;
//  iosfAgtCfg.TSAI_WIDTH       = `MBY_TSAI_WIDTH;
//  iosfAgtCfg.Max_Payload_Size = Iosf::SIZE_64_BYTES; 
//  iosfAgtCfg.intfName     = "mby_tb.dummy_dut_.iosf_primary_ti";
//  iosfAgtCfg.POK_Supported = 1;
//
//   iosfAgtVc            = new ("iosfAgtVc",     null);
//   // Configure children
//   set_config_object ("iosfAgtVc",    "iosfAgtCfg", iosfAgtCfg, 0);
//   // Select functionality of IOSF version
//    iosfAgtVc.iosfRevision =  Iosf::REV110;
//
//  end // initial begin
//
//  iosfsbm_agent::iosfsbm_agtvc AgtSbVc;
//  iosfsbm_agent::agtvc_cfg     AgtSbVcCfg;
//
//  initial begin
//    iosfsbm_cm::pid_t    fab_my_ports[$];
//    iosfsbm_cm::pid_t    fab_other_ports[$]; 
//    iosfsbm_cm::opcode_t fab_supp_opcodes[$];
//    // Integ - If the IP supports fuse pull
//    iosfsbm_cm::opcode_t         fuses_opcodes[$];
//    // Integ - If the IP supports PME  
//    iosfsbm_cm::opcode_t         pme_opcodes[$];
//    #0;
//    AgtSbVcCfg = new ("AgtSbVcCfg");
//
//    //$cast (AgtSbVcCfg, create_object ("iosfsbm_agent::iosfsbm_agtvc", 
//    //"AgtSbVcCfg"));
//
//    AgtSbVcCfg.ext_header_support     = 1;
//    AgtSbVcCfg.ext_headers_per_txn    = 0;
//
//     fuses_opcodes = {8'hB8}; //`MBY_FUSE_PULL_OPCODE_G0
//
//     pme_opcodes = {`MBY_PME_OPCODE};
//    fab_my_ports = '{`MBY_SB_PORT_ID };
//    fab_other_ports = '{`MBY_FUSE_PULL_EP_ID,`MBY_PME_OPCODE,`MBY_DFX_EP_ID,
//       `MBY_HOST_EP_ID};
//    fab_supp_opcodes = {iosfsbm_cm::DEFAULT_OPCODES, fuses_opcodes, pme_opcodes};
//     assert (AgtSbVcCfg.randomize with 
//            { num_tx_ext_headers       == 1;
//              ext_headers[0]           == 32'h00000000;
//              payload_width            == 8;
//              compl_delay              == 1;
//              np_crd_buffer            == 3;
//              pc_crd_buffer            == 3;
//              my_ports.size()          == fab_my_ports.size();
//              other_ports.size()       == fab_other_ports.size();
//              foreach (fab_my_ports[i])
//              my_ports[i]           == fab_my_ports[i];
//              foreach (fab_other_ports[i])
//              other_ports[i]        == fab_other_ports[i];
//              supported_opcodes.size() == fab_supp_opcodes.size();
//              foreach (fab_supp_opcodes[i])
//              supported_opcodes[i]  == fab_supp_opcodes[i];
//              }) else
//      ovm_report_error ("Duumy DUT", "AgtSbVcCfg Randomization failed");
//
//
//    `ifdef IOSF_SB_PH2
//    //ddaftary set instance name required for phase 2
//    AgtSbVcCfg.inst_name = "mby_tb.dummy_dut_.iosf_sb_ti";
//    `else
//    AgtSbVcCfg.intf_name = "dummy_dut_sb_if";
//    `endif
//    //To disable compliance monitor set this to 1
//    AgtSbVcCfg.disable_compmon = 0;
//
//    // Use interanl memeory model
//    AgtSbVcCfg.use_mem = 1;
//     //AgtSbVcCfg.disable_driving_clkack = 1;
//    //set IOSF Spec Version
//    AgtSbVcCfg.iosfsb_spec_rev  = iosfsbm_cm::IOSF_11;
//    AgtSbVcCfg.disable_compmon_assertion("ISMPM_061_AGENTMUSTENTER_IDLE_REQ"); //IA - removed - fails due to pgcb sync {TODO}
//
//    set_config_string ("AgtSbVc.*", "default_sequence", "fuse_pull_seq");
//    set_config_int ("AgtSbVc.*", "count", 1);
//    set_config_object ("AgtSbVc", "agent_cfg", AgtSbVcCfg , 0);
//    AgtSbVc = new ("AgtSbVc",null);
//
//
//    //    $cast (AgtSbVcCfg, create_component ("iosfsbm_agent::iosfsbm_agtvc",
//    //                                       "AgtSbVcCfg"));
//  end
//
//
//class fuse_pull_seq extends ovm_sequence;
//  `ovm_sequence_utils(fuse_pull_seq, iosfsbm_cm::iosfsbc_sequencer);
//  iosfsbm_seq::iosf_sb_seq sb_seq;
//
//  task body();
//    `ovm_do_with(sb_seq, {xaction_class_i == iosfsbm_cm::NON_POSTED;
//       src_pid_i == `MBY_SB_PORT_ID; 
//       dest_pid_i == `MBY_FUSE_PULL_EP_ID;
//       opcode_i == 8'hB8;//MBY_types::FUSE_GROUP0_REQ ;
//       data_i.size() == 0;
//       addr_i.size() == 0;
//       fbe_i == 0;
//       sbe_i == 0;
//       bar_i == 0;
//       fid_i == 0;
//       xaction_delay_i == 0;
//       exp_rsp_i == 0;
//       compare_completion_i == 0;
//     });
//  endtask // body
//
//endclass // fuse_pull_seq
// END IOSF_NOT_PRESENT

  // Intruupt mimic
  initial  begin
    force `MBY_TOP.mby_int = 0;
    #10us;
    force `MBY_TOP.mby_int = 1;
  end

endmodule
