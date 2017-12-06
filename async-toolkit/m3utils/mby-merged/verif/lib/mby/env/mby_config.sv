
/* ----------------------------------------------------------------------


   ----------------------------------------------------------------------
   file:    mby_config.sv
   Date Created  : 25/7/2016
   Author        : dbenita
   Project       : MBY IP
   ----------------------------------------------------------------------
 
  MBY config object

 This is the configuration object to control the MBY env.
 
 This Class contain all the switches to control the ENV seeting.
 
 By default it contain Saola config oobject:
 
 checkers_enabled - default 1
 
 monitors_enabled - default 1
 
 trackers_enabled  - default 1
 
 coverage_enabled  - default 1
 
 By default Saola will be build this object unless Upper env 
 or a test override it using set_config ...
 
 

*/

class mby_config extends slu_config_object;

    string mby_primary_access = "primary";
    string mby_sideband_access = "sideband";
    bit mby_chassis_rst_verbose_dbg;
    //Variable: mby_has_reset_pkg
    // This bit control the build of the reset pkg
    // It gets it value fromthe TI MBY_HAS_RESET_PKG parameter
    bit mby_has_reset_pkg = 0;

// START IOSF_NOT_PRESENT 
//  IosfFabCfg   MBYIosfPriVcCfg;
//
//  iosfsbm_fbrc::fbrcvc_cfg     MBYIosfSbVcCfg;
//
//  // Integ - If the IP supports fuse pull
//  iosfsbm_cm::opcode_t         fuses_opcodes[$];
//  // Integ - If the IP supports PME
//  iosfsbm_cm::opcode_t         pme_opcodes[$];
// END IOSF_NOT_PRESENT 

  `uvm_object_utils_begin(mby_config)
     `uvm_field_string(mby_primary_access, UVM_ALL_ON)
     `uvm_field_string(mby_sideband_access, UVM_ALL_ON)
     `uvm_field_string(mby_chassis_rst_verbose_dbg, UVM_ALL_ON)
   `uvm_object_utils_end

  //function: new 
  function new( string         name = "");
    super.new(name);
    set_parent_name(name);
// START CHASSIS_NOT_PRESENT
//    mby_has_reset_pkg = 1;
// END CHASSIS_NOT_PRESENT

// START IOSF_NOT_PRESENT
//    configure_iosf_primary();
//    configure_iosf_sideband();
// END IOSF_NOT_PRESENT

  endfunction

// START IOSF_NOT_PRESENT
//  function void configure_iosf_sideband();
//    iosfsbm_cm::pid_t    fab_my_ports[$];
//    iosfsbm_cm::pid_t    fab_other_ports[$]; 
//    iosfsbm_cm::opcode_t fab_supp_opcodes[$];
//    iosfsbm_cm::pid_t    fab_mcast_ports[$]; //for spt_chassis_rst_pkg
//
//    fuses_opcodes = {MBY_types::FUSE_GROUP0_REQ,
//                     MBY_types::FUSE_GROUP1_REQ,
//                     MBY_types::FUSE_GROUP2_REQ,
//                     MBY_types::FUSE_GROUP3_REQ};
//    `uvm_info (get_name (), "INTEG - Currently setup IP to support FUSE PULL ", UVM_HIGH);
//    // INTEG If the IP support PME
//    `uvm_info (get_name(),	"INTEG - Currently setup IP to support PME ", UVM_HIGH);
//    pme_opcodes = {`MBY_PME_OPCODE};
//    //opcode supportted byth the IP
//    fab_supp_opcodes = {iosfsbm_cm::DEFAULT_OPCODES, fuses_opcodes, pme_opcodes};
//    // MBY IP port ID
//    fab_other_ports = '{`MBY_SB_PORT_ID };
//    `uvm_info (get_name(),	$psprintf ("INTEG - Currently setup IP SB ID to 0x%x ", `MBY_SB_PORT_ID), UVM_HIGH);
//    // Port that the IP recieving MSGs from
//    fab_my_ports = '{`MBY_FUSE_PULL_EP_ID,`MBY_PMC_EP_ID,`MBY_DFX_EP_ID, `MBY_HOST_EP_ID}; 
//
//    
//     MBYIosfSbVcCfg = iosfsbm_fbrc::fbrcvc_cfg::type_id::create("MBYIosfSbVcCfg");
//    
//    // INTEG If the IP support fuse pull
//    // Support of SAI
//    MBYIosfSbVcCfg.ext_header_support     = 1;
//    MBYIosfSbVcCfg.agt_ext_header_support = 1;
//    MBYIosfSbVcCfg.ext_headers_per_txn     = 1;
//    
//    assert (MBYIosfSbVcCfg.randomize with 
//            { num_tx_ext_headers       == 1;
//              ext_headers[0]           == 32'h00000000;
//              payload_width            == 8;
//              compl_delay              == 1;
//              np_crd_buffer            == 1;
//              pc_crd_buffer            == 1;
//              my_ports.size()          == fab_my_ports.size();
//              other_ports.size()       == fab_other_ports.size();
//	      mcast_ports.size()       == fab_mcast_ports.size();
//              foreach (fab_my_ports[i])
//              my_ports[i]           == fab_my_ports[i];
//              foreach (fab_other_ports[i])
//              other_ports[i]        == fab_other_ports[i];
//	      foreach (fab_mcast_ports[i])
//              mcast_ports[i]           == fab_mcast_ports[i];
//              supported_opcodes.size() == fab_supp_opcodes.size();
//              foreach (fab_supp_opcodes[i])
//              supported_opcodes[i]  == fab_supp_opcodes[i];
//              }) else
//      `uvm_error (get_name (), "MBYIosfSbVcCfg Randomization failed"); 
//    
//    `ifdef IOSF_SB_PH2
//    //ddaftary set instance name required for phase 2
//
//    `else
//    MBYIosfSbVcCfg.intf_name = "mby_sb_intf";
//    `endif
//        
//    //To disable compliance monitor set this to 1
//    MBYIosfSbVcCfg.disable_compmon = 0; 
//    
//    //set IOSF Spec Version      
//    MBYIosfSbVcCfg.set_iosfspec_ver(iosfsbm_cm::IOSF_11);
//    
//    //configure SVC to not drive clkack since CCU will drive clkack
//    MBYIosfSbVcCfg.disable_driving_clkack = 1;
//  endfunction : configure_iosf_sideband
//
//  function void configure_iosf_primary();
//    MBYIosfPriVcCfg = IosfPkg::IosfFabCfg::type_id::create("MBYIosfPriVcCfg");
//    
//    // QuestaSim fails b2b/test04 (TGTDECODE) without next 3 lines)
//    MBYIosfPriVcCfg.iosfAgtCfg[0] = IosfPkg::IosfAgtCfg::type_id::create("MBYIosfPriVcCfg.iosfAgtCfg[0]");
//    MBYIosfPriVcCfg.iosfAgtCfg[1] = IosfPkg::IosfAgtCfg::type_id::create("MBYIosfPriVcCfg.iosfAgtCfg[1]");
//  
// // Randomize Fabric Cfg
//    if (!MBYIosfPriVcCfg.randomize () with
//	     {MBYIosfPriVcCfg.upNode == 0;
//         MBYIosfPriVcCfg.nodes  == 2;
//         MBYIosfPriVcCfg.decode == Iosf::FABDECODE;
//         // Fabric VC has embedded north agent that must have same
//         // channels as MBY DUT.  ID widths and Max sizes make constraints
//         // easier.  The defines are in mby_params.sv
//         MBYIosfPriVcCfg.iosfAgtCfg[0].MNUMCHAN         == `MBY_MNUMCHAN;  
//         MBYIosfPriVcCfg.iosfAgtCfg[0].TNUMCHAN         == `MBY_TNUMCHAN;   
//         MBYIosfPriVcCfg.iosfAgtCfg[0].MNUMCHANL2       == `MBY_MNUMCHANL2;
//         MBYIosfPriVcCfg.iosfAgtCfg[0].TNUMCHANL2       == `MBY_TNUMCHANL2; 
//         MBYIosfPriVcCfg.iosfAgtCfg[0].SRC_ID_WIDTH     == `MBY_SRC_ID_WIDTH;
//         MBYIosfPriVcCfg.iosfAgtCfg[0].DEST_ID_WIDTH    == `MBY_DEST_ID_WIDTH;
//         MBYIosfPriVcCfg.iosfAgtCfg[0].Max_Payload_Size == Iosf::SIZE_64_BYTES; 
//         MBYIosfPriVcCfg.iosfAgtCfg[0].Max_Read_Request_Size 
//         == Iosf::SIZE_64_BYTES; 
//         // iosfAgtCfg [1] connects to MBY DUT
//         MBYIosfPriVcCfg.iosfAgtCfg[1].MAX_DATA_LEN     == `MBY_MAX_DATA_LEN;  
//         MBYIosfPriVcCfg.iosfAgtCfg[1].MMAX_ADDR        == `MBY_MMAX_ADDR; 
//         MBYIosfPriVcCfg.iosfAgtCfg[1].TMAX_ADDR        == `MBY_TMAX_ADDR; 
//         MBYIosfPriVcCfg.iosfAgtCfg[1].AGENT_WIDTH      == `MBY_AGENT_WIDTH; 
//         MBYIosfPriVcCfg.iosfAgtCfg[1].MNUMCHAN         == `MBY_MNUMCHAN;  
//         MBYIosfPriVcCfg.iosfAgtCfg[1].TNUMCHAN         == `MBY_TNUMCHAN;   
//         MBYIosfPriVcCfg.iosfAgtCfg[1].MNUMCHANL2       == `MBY_MNUMCHANL2;
//         MBYIosfPriVcCfg.iosfAgtCfg[1].TNUMCHANL2       == `MBY_TNUMCHANL2; 
//         MBYIosfPriVcCfg.iosfAgtCfg[1].MD_WIDTH         == `MBY_MD_WIDTH;
//         MBYIosfPriVcCfg.iosfAgtCfg[1].TD_WIDTH         == `MBY_TD_WIDTH;
//         MBYIosfPriVcCfg.iosfAgtCfg[1].SRC_ID_WIDTH     == `MBY_SRC_ID_WIDTH;
//         MBYIosfPriVcCfg.iosfAgtCfg[1].DEST_ID_WIDTH    == `MBY_DEST_ID_WIDTH;
//         MBYIosfPriVcCfg.iosfAgtCfg[1].MSAI_WIDTH       == `MBY_MSAI_WIDTH;
//         MBYIosfPriVcCfg.iosfAgtCfg[1].TSAI_WIDTH       == `MBY_TSAI_WIDTH;
//         MBYIosfPriVcCfg.iosfAgtCfg[1].Max_Payload_Size == Iosf::SIZE_64_BYTES; 
//         MBYIosfPriVcCfg.iosfAgtCfg[1].Max_Read_Request_Size 
//         == Iosf::SIZE_64_BYTES;
//	 })
//      
//      `uvm_error (get_name (), "MBYIosfPriVcCfg randomize failed");
//
//    //credit config according to SCS HAS 
//    // setTxnCredits (Iosf::channel_t channel , 
//    //                int npcmd,  int npdata  , 
//    //                int pcmd,   int pdata   , 
//    //                int cplcmd, int cpldata );
//    //                            CH npC npD pC pD cC cD
//    MBYIosfPriVcCfg.iosfAgtCfg[1].setTxnCredits(0 ,1  ,1  ,1 ,4 ,1 ,1);
//    
//    //credit thresholds ( randomized accordint to number of credits )
//  MBYIosfPriVcCfg.iosfAgtCfg[1].setCredUpdThreshold(0, 
//				$urandom_range(1,MBYIosfPriVcCfg.iosfAgtCfg[1].npCmdCrd[0]), 
//				$urandom_range(1,MBYIosfPriVcCfg.iosfAgtCfg[1].npDataCrd[0]), 
//				$urandom_range(1,MBYIosfPriVcCfg.iosfAgtCfg[1].pCmdCrd[0]),   
//			//	$urandom_range(1,MBYIosfPriVcCfg.iosfAgtCfg[1].pDataCrd[0]), 
//				4,
//				$urandom_range(1,MBYIosfPriVcCfg.iosfAgtCfg[1].cCmdCrd[0]), 
//				$urandom_range(1,MBYIosfPriVcCfg.iosfAgtCfg[1].cDataCrd[0]));
//    MBYIosfPriVcCfg.iosfAgtCfg[1].POK_Supported = 1;
//
//
//  endfunction : configure_iosf_primary
//
// END IOSF_NOT_PRESENT

  
endclass
