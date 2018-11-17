

module egr_top_tb 
  import mby_egr_pkg::*, shared_pkg::* 
  `ifndef NO_SLA 
  ,sla_pkg::*
  `endif
  ; 
();
    logic clk;
    logic arst_n;
  
  mim_wr_if         igr_dirtypod_if(); //IGR Dirty Pod Write Interface //TODO Modify port names to similar of IGR
  mim_rd_if         igr_cleanpod_if(); //IGR Clean Pod Read Req/Rsp Interface

  mim_rd_if            mim_rd_if0_0(); //MIM Read Request Row0 Plane 0
  mim_rd_if            mim_rd_if0_1(); //MIM Read Request Row0 Plane 1
  mim_rd_if            mim_rd_if0_2(); //MIM Read Request Row0 Plane 2
  mim_rd_if            mim_rd_if1_0(); //MIM Read Request Row1 Plane 0
  mim_rd_if            mim_rd_if1_1(); //MIM Read Request Row1 Plane 1
  mim_rd_if            mim_rd_if1_2(); //MIM Read Request Row1 Plane 2
    
    egr_igr_if                   igr_if(); //EGR-IGR Pointer Interface
    egr_pod_if                   pod_if(); //EGR-Pod Ring Interface
  egr_mc_mirror_pod_if mc_mirror_pod_if(); //EGR-MC Mirror Pod Ring Interface
    egr_ahb_if                   ahb_if(); //EGR-AHB Interface
    egr_smm_writearb_if smm_writearb_if(); //EGR-SMM_Write_Arb Interface
    egr_smm_readarb_if  smm_readarb_if0(); //EGR-SMM_Read_Arb Interface
    egr_smm_readarb_if  smm_readarb_if1(); //EGR-SMM_Read_Arb Interface

  egr_tagring_if          tagring_if0_0(); //EGR-Tag Ring Interface MGP  0 0
  egr_tagring_if          tagring_if0_1(); //EGR-Tag Ring Interface MGP  0 1
  egr_tagring_if          tagring_if1_0(); //EGR-Tag Ring Interface MGP  1 0
  egr_tagring_if          tagring_if1_1(); //EGR-Tag Ring Interface MGP  1 1
  egr_tagring_if          tagring_if2_0(); //EGR-Tag Ring Interface MGP  2 0
  egr_tagring_if          tagring_if2_1(); //EGR-Tag Ring Interface MGP  2 1
  egr_tagring_if          tagring_if3_0(); //EGR-Tag Ring Interface MGP  3 0
  egr_tagring_if          tagring_if3_1(); //EGR-Tag Ring Interface MGP  3 1
  egr_tagring_if          tagring_if4_0(); //EGR-Tag Ring Interface MGP  4 0
  egr_tagring_if          tagring_if4_1(); //EGR-Tag Ring Interface MGP  4 1
  egr_tagring_if          tagring_if5_0(); //EGR-Tag Ring Interface MGP  5 0
  egr_tagring_if          tagring_if5_1(); //EGR-Tag Ring Interface MGP  5 1
  egr_tagring_if          tagring_if6_0(); //EGR-Tag Ring Interface MGP  6 0
  egr_tagring_if          tagring_if6_1(); //EGR-Tag Ring Interface MGP  6 1
  egr_tagring_if          tagring_if7_0(); //EGR-Tag Ring Interface MGP  7 0
  egr_tagring_if          tagring_if7_1(); //EGR-Tag Ring Interface MGP  7 1
  egr_tagring_if          tagring_if8_0(); //EGR-Tag Ring Interface MGP  8 0
  egr_tagring_if          tagring_if8_1(); //EGR-Tag Ring Interface MGP  8 1
  egr_tagring_if          tagring_if9_0(); //EGR-Tag Ring Interface MGP  9 0
  egr_tagring_if          tagring_if9_1(); //EGR-Tag Ring Interface MGP  9 1
  egr_tagring_if         tagring_if10_0(); //EGR-Tag Ring Interface MGP 10 0
  egr_tagring_if         tagring_if10_1(); //EGR-Tag Ring Interface MGP 10 1
  egr_tagring_if         tagring_if11_0(); //EGR-Tag Ring Interface MGP 11 0
  egr_tagring_if         tagring_if11_1(); //EGR-Tag Ring Interface MGP 11 1
  egr_tagring_if         tagring_if12_0(); //EGR-Tag Ring Interface MGP 12 0
  egr_tagring_if         tagring_if12_1(); //EGR-Tag Ring Interface MGP 12 1
  egr_tagring_if         tagring_if13_0(); //EGR-Tag Ring Interface MGP 13 0
  egr_tagring_if         tagring_if13_1(); //EGR-Tag Ring Interface MGP 13 1
  egr_tagring_if         tagring_if14_0(); //EGR-Tag Ring Interface MGP 14 0
  egr_tagring_if         tagring_if14_1(); //EGR-Tag Ring Interface MGP 14 1
  egr_tagring_if         tagring_if15_0(); //EGR-Tag Ring Interface MGP 15 0
  egr_tagring_if         tagring_if15_1(); //EGR-Tag Ring Interface MGP 15 1
                       
    egr_mce_tagring_if  mce_tagring_if0();  //EGR-Tag Ring Interface 
    egr_mce_tagring_if  mce_tagring_if1();  //EGR-Tag Ring Interface 
    egr_mce_tagring_if  mce_tagring_if2();  //EGR-Tag Ring Interface 
    egr_mce_tagring_if  mce_tagring_if3();  //EGR-Tag Ring Interface 
    egr_statsmgmt_if       statsmgmt_if(); //EGR-Stats Management Interface
    egr_tx_ppe_if            tx_ppe_if0(); //EGR-TxPPE 0 Interface
    egr_tx_ppe_if            tx_ppe_if1(); //EGR-TxPPE 1 Interface
    egr_ppe_stm_if           ppe_stm_if(); //EGR-PPE Shared Table Memory Interface 
    egr_epl_if                  epl_if0(); //EGR-EPL 0 Interface
    egr_epl_if                  epl_if1(); //EGR-EPL 1 Interface
    egr_epl_if                  epl_if2(); //EGR-EPL 2 Interface
    egr_epl_if                  epl_if3(); //EGR-EPL 3 Interface
    egr_mc_table_if        mc_table_if0(); //EGR-MultiCast Shared Table 0 Interface
    egr_mc_table_if        mc_table_if1(); //EGR-MultiCast Shared Table 1 Interface 
  egr_cmring_if               cmring_if(); //EGR-Congestion Management Ring Interface

    egr_top dut(.*);
    egr_top_tb_tagring tagring_bfm(.*);
    egr_top_tb_app app_bfm(.*);
    egr_top_tb_runtime runtime(.*);

always_comb ahb_if.dfxsignal_in = 0;
always_comb ahb_if.ahb_req_p = 0;
always_comb ahb_if.ahb_addr = 0;
always_comb ahb_if.ahb_wr = 0;
always_comb ahb_if.ahb_wr_data = 0;

    egr_dummy_if dummy_if(
        .igr_if(igr_if.igr),
        .pod_if(pod_if.pod_ring),
        .smm_writearb_if(smm_writearb_if.smm_writearb),
        .smm_readarb_if0           (smm_readarb_if0.smm_readarb),                 
        .smm_readarb_if1           (smm_readarb_if1.smm_readarb),                 
        .mce_tagring_if0           (mce_tagring_if0.tagring),                 
        .mce_tagring_if1           (mce_tagring_if1.tagring),                 
        .mce_tagring_if2           (mce_tagring_if2.tagring),                 
        .mce_tagring_if3           (mce_tagring_if3.tagring),        
        .statsmgmt_if(statsmgmt_if.statsmgmt),
        .tx_ppe_if0(tx_ppe_if0.ppe),
        .tx_ppe_if1(tx_ppe_if1.ppe),
        .ppe_stm_if(ppe_stm_if.stm),
        .mc_table_if0(mc_table_if0.mc_table),
        .mc_table_if1(mc_table_if1.mc_table)
        );

endmodule : egr_top_tb
