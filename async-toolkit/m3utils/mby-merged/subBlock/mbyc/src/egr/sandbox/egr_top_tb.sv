

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
    egr_smm_readarb_if   smm_readarb_if(); //EGR-SMM_Read_Arb Interface
    egr_tagring_if           tagring_if(); //EGR-Tag Ring Interface
    egr_statsmgmt_if       statsmgmt_if(); //EGR-Stats Management Interface
    egr_tx_ppe_if            tx_ppe_if0(); //EGR-TxPPE 0 Interface
    egr_tx_ppe_if            tx_ppe_if1(); //EGR-TxPPE 1 Interface
    egr_ppe_stm_if           ppe_stm_if(); //EGR-PPE Shared Table Memory Interface 
    egr_epl_if                  epl_if0(); //EGR-EPL 0 Interface
    egr_epl_if                  epl_if1(); //EGR-EPL 1 Interface
    egr_epl_if                  epl_if2(); //EGR-EPL 2 Interface
    egr_epl_if                  epl_if3(); //EGR-EPL 3 Interface
    egr_vp_if                    vp_if0(); //EGR-VP0 Interface
    egr_vp_if                    vp_if1(); //EGR-VP1 Interface
    egr_mc_table_if        mc_table_if0(); //EGR-MultiCast Shared Table 0 Interface
    egr_mc_table_if        mc_table_if1(); //EGR-MultiCast Shared Table 1 Interface 

    egr_top dut(
        .clk(clk),
        .arst_n(arst_n),
        .igr_if(igr_if.egr),
        .pod_if(pod_if.egr),
        .ahb_if(ahb_if.egr),
        .smm_writearb_if(smm_writearb_if.egr),
        .smm_readarb_if(smm_readarb_if.egr),
        .tagring_if(tagring_if.egr),
        .statsmgmt_if(statsmgmt_if.egr),
        .tx_ppe_if0(tx_ppe_if0.egr),
        .tx_ppe_if1(tx_ppe_if1.egr),
        .ppe_stm_if(ppe_stm_if.egr),
        .epl_if0(epl_if0.egr),
        .epl_if1(epl_if1.egr),
        .epl_if2(epl_if2.egr),
        .epl_if3(epl_if3.egr),
        .vp_if0(vp_if0.egr),
        .vp_if1(vp_if1.egr),
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
        .smm_readarb_if(smm_readarb_if.smm_readarb),
        .tagring_if(tagring_if.tagring),
        .statsmgmt_if(statsmgmt_if.statsmgmt),
        .tx_ppe_if0(tx_ppe_if0.ppe),
        .tx_ppe_if1(tx_ppe_if1.ppe),
        .ppe_stm_if(ppe_stm_if.stm),
        .epl_if0(epl_if0.epl),
        .epl_if1(epl_if1.epl),
        .epl_if2(epl_if2.epl),
        .epl_if3(epl_if3.epl),
        .vp_if0(vp_if0.vp),
        .vp_if1(vp_if1.vp),
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
