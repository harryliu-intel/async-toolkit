

module egr_top_tb 
  import mby_egr_pkg::*, shared_pkg::* 
  `ifndef NO_SLA 
  ,sla_pkg::*
  `endif
  ; 
();
    logic clk;
    logic arst_n;
    
    egr_igr_if                   igr_if(); //EGR-IGR Pointer Interface
    egr_pod_if                   pod_if(); //EGR-Pod Ring Interface
    egr_ahb_if                   ahb_if(); //EGR-AHB Interface
    egr_ahb_if                   dfx_if(); //EGR-DFx Interface
    egr_smm_writearb_if smm_writearb_if(); //EGR-SMM_Write_Arb Interface
    egr_smm_readarb_if  smm_readarb_if0(); //EGR-SMM_Read_Arb Interface
    egr_smm_readarb_if  smm_readarb_if1(); //EGR-SMM_Read_Arb Interface
    egr_tagring_if          tagring_if0();  //EGR-Tag Ring Interface 
    egr_tagring_if          tagring_if1();  //EGR-Tag Ring Interface 
    egr_tagring_if         tagring_if10();  //EGR-Tag Ring Interface 
    egr_tagring_if         tagring_if11();  //EGR-Tag Ring Interface 
    egr_tagring_if         tagring_if12();  //EGR-Tag Ring Interface 
    egr_tagring_if         tagring_if13();  //EGR-Tag Ring Interface 
    egr_tagring_if         tagring_if14();  //EGR-Tag Ring Interface 
    egr_tagring_if         tagring_if15();  //EGR-Tag Ring Interface 
    egr_tagring_if         tagring_if16();  //EGR-Tag Ring Interface 
    egr_tagring_if         tagring_if17();  //EGR-Tag Ring Interface 
    egr_tagring_if         tagring_if18();  //EGR-Tag Ring Interface 
    egr_tagring_if         tagring_if19();  //EGR-Tag Ring Interface 
    egr_tagring_if          tagring_if2();  //EGR-Tag Ring Interface 
    egr_tagring_if         tagring_if20();  //EGR-Tag Ring Interface 
    egr_tagring_if         tagring_if21();  //EGR-Tag Ring Interface 
    egr_tagring_if         tagring_if22();  //EGR-Tag Ring Interface 
    egr_tagring_if         tagring_if23();  //EGR-Tag Ring Interface 
    egr_tagring_if         tagring_if24();  //EGR-Tag Ring Interface 
    egr_tagring_if         tagring_if25();  //EGR-Tag Ring Interface 
    egr_tagring_if         tagring_if26();  //EGR-Tag Ring Interface 
    egr_tagring_if         tagring_if27();  //EGR-Tag Ring Interface 
    egr_tagring_if         tagring_if28();  //EGR-Tag Ring Interface 
    egr_tagring_if         tagring_if29();  //EGR-Tag Ring Interface 
    egr_tagring_if          tagring_if3();  //EGR-Tag Ring Interface 
    egr_tagring_if         tagring_if30();  //EGR-Tag Ring Interface 
    egr_tagring_if         tagring_if31();  //EGR-Tag Ring Interface 
    egr_tagring_if          tagring_if4();  //EGR-Tag Ring Interface 
    egr_tagring_if          tagring_if5();  //EGR-Tag Ring Interface 
    egr_tagring_if          tagring_if6();  //EGR-Tag Ring Interface 
    egr_tagring_if          tagring_if7();  //EGR-Tag Ring Interface 
    egr_tagring_if          tagring_if8();  //EGR-Tag Ring Interface 
    egr_tagring_if          tagring_if9();  //EGR-Tag Ring Interface
                       
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

    egr_top dut(
        .clk(clk),
        .arst_n(arst_n),
        .igr_if(igr_if.egr),
        .pod_if(pod_if.egr),
        .ahb_if(ahb_if.egr),
        .smm_writearb_if(smm_writearb_if.egr),
        .smm_readarb_if0           (smm_readarb_if0.egr),                 
        .smm_readarb_if1           (smm_readarb_if1.egr),                 
        .tagring_if0               (tagring_if0.egr),                     
        .tagring_if1               (tagring_if1.egr),                     
        .tagring_if2               (tagring_if2.egr),                     
        .tagring_if3               (tagring_if3.egr),                     
        .tagring_if4               (tagring_if4.egr),                     
        .tagring_if5               (tagring_if5.egr),                     
        .tagring_if6               (tagring_if6.egr),                     
        .tagring_if7               (tagring_if7.egr),                     
        .tagring_if8               (tagring_if8.egr),                     
        .tagring_if9               (tagring_if9.egr),                     
        .tagring_if10              (tagring_if10.egr),                    
        .tagring_if11              (tagring_if11.egr),                    
        .tagring_if12              (tagring_if12.egr),                    
        .tagring_if13              (tagring_if13.egr),                    
        .tagring_if14              (tagring_if14.egr),                    
        .tagring_if15              (tagring_if15.egr),                    
        .tagring_if16              (tagring_if16.egr),                    
        .tagring_if17              (tagring_if17.egr),                    
        .tagring_if18              (tagring_if18.egr),                    
        .tagring_if19              (tagring_if19.egr),                    
        .tagring_if20              (tagring_if20.egr),                    
        .tagring_if21              (tagring_if21.egr),                    
        .tagring_if22              (tagring_if22.egr),                    
        .tagring_if23              (tagring_if23.egr),                    
        .tagring_if24              (tagring_if24.egr),                    
        .tagring_if25              (tagring_if25.egr),                    
        .tagring_if26              (tagring_if26.egr),                    
        .tagring_if27              (tagring_if27.egr),                    
        .tagring_if28              (tagring_if28.egr),                    
        .tagring_if29              (tagring_if29.egr),                    
        .tagring_if30              (tagring_if30.egr),                    
        .tagring_if31              (tagring_if31.egr),                    
        .mce_tagring_if0           (mce_tagring_if0.egr),                 
        .mce_tagring_if1           (mce_tagring_if1.egr),                 
        .mce_tagring_if2           (mce_tagring_if2.egr),                 
        .mce_tagring_if3           (mce_tagring_if3.egr),        
        .statsmgmt_if(statsmgmt_if.egr),
        .tx_ppe_if0(tx_ppe_if0.egr),
        .tx_ppe_if1(tx_ppe_if1.egr),
        .ppe_stm_if(ppe_stm_if.egr),
        .epl_if0(epl_if0.egr),
        .epl_if1(epl_if1.egr),
        .epl_if2(epl_if2.egr),
        .epl_if3(epl_if3.egr),
        .mc_table_if0(mc_table_if0.egr),
        .mc_table_if1(mc_table_if1.egr)
    );
    
    egr_dummy_if dummy_if(
        .clk(clk),
        .arst_n(arst_n),
        .igr_if(igr_if.igr),
        .pod_if(pod_if.pod_ring),
        .ahb_if(ahb_if.ahb),
        .dfx_if(ahb_if.dfx),
        .smm_writearb_if(smm_writearb_if.smm_writearb),
        .smm_readarb_if0           (smm_readarb_if0.smm_readarb),                 
        .smm_readarb_if1           (smm_readarb_if1.smm_readarb),                 
        .tagring_if0               (tagring_if0.tagring),                     
        .tagring_if1               (tagring_if1.tagring),                     
        .tagring_if2               (tagring_if2.tagring),                     
        .tagring_if3               (tagring_if3.tagring),                     
        .tagring_if4               (tagring_if4.tagring),                     
        .tagring_if5               (tagring_if5.tagring),                     
        .tagring_if6               (tagring_if6.tagring),                     
        .tagring_if7               (tagring_if7.tagring),                     
        .tagring_if8               (tagring_if8.tagring),                     
        .tagring_if9               (tagring_if9.tagring),                     
        .tagring_if10              (tagring_if10.tagring),                    
        .tagring_if11              (tagring_if11.tagring),                    
        .tagring_if12              (tagring_if12.tagring),                    
        .tagring_if13              (tagring_if13.tagring),                    
        .tagring_if14              (tagring_if14.tagring),                    
        .tagring_if15              (tagring_if15.tagring),                    
        .tagring_if16              (tagring_if16.tagring),                    
        .tagring_if17              (tagring_if17.tagring),                    
        .tagring_if18              (tagring_if18.tagring),                    
        .tagring_if19              (tagring_if19.tagring),                    
        .tagring_if20              (tagring_if20.tagring),                    
        .tagring_if21              (tagring_if21.tagring),                    
        .tagring_if22              (tagring_if22.tagring),                    
        .tagring_if23              (tagring_if23.tagring),                    
        .tagring_if24              (tagring_if24.tagring),                    
        .tagring_if25              (tagring_if25.tagring),                    
        .tagring_if26              (tagring_if26.tagring),                    
        .tagring_if27              (tagring_if27.tagring),                    
        .tagring_if28              (tagring_if28.tagring),                    
        .tagring_if29              (tagring_if29.tagring),                    
        .tagring_if30              (tagring_if30.tagring),                    
        .tagring_if31              (tagring_if31.tagring),                    
        .mce_tagring_if0           (mce_tagring_if0.tagring),                 
        .mce_tagring_if1           (mce_tagring_if1.tagring),                 
        .mce_tagring_if2           (mce_tagring_if2.tagring),                 
        .mce_tagring_if3           (mce_tagring_if3.tagring),        
        .statsmgmt_if(statsmgmt_if.statsmgmt),
        .tx_ppe_if0(tx_ppe_if0.ppe),
        .tx_ppe_if1(tx_ppe_if1.ppe),
        .ppe_stm_if(ppe_stm_if.stm),
        .epl_if0(epl_if0.epl),
        .epl_if1(epl_if1.epl),
        .epl_if2(epl_if2.epl),
        .epl_if3(epl_if3.epl),
        .mc_table_if0(mc_table_if0.mc_table),
        .mc_table_if1(mc_table_if1.mc_table)
        );
    
initial
begin
    arst_n = 1; #5;
    arst_n = 0; #5;
    arst_n = 1; #5;
    #100;
end


initial
begin
    for(int i=0; i<10;i++)
    begin
        clk = 1; #5;
        clk = 0; #5;
    end
end
    

endmodule : egr_top_tb
