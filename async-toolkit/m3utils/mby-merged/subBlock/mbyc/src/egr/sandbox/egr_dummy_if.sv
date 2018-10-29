
// -- Description  : This is a dummy block that 
//                   instantiates the modports to connect to the EGR interfaces
//------------------------------------------------------------------------------

module egr_dummy_if 
  import mby_egr_pkg::*, shared_pkg::*; 
(
//TODO change width of interfaces to parameters
  input logic                                  clk,
  input logic                               arst_n, //Asynchronous negedge reset   
  
  egr_igr_if.igr                            igr_if, //EGR-IGR Pointer Interface
  egr_pod_if.pod_ring                       pod_if, //EGR-Pod Ring Interface
  egr_ahb_if.ahb                            ahb_if, //EGR-AHB Interface
  egr_ahb_if.dfx                            dfx_if, //EGR-DFx Interface
  egr_smm_writearb_if.smm_writearb smm_writearb_if, //EGR-SMM_Write_Arb Interface
  egr_smm_readarb_if.smm_readarb   smm_readarb_if0, //EGR-SMM_Read_Arb Interface
  egr_smm_readarb_if.smm_readarb   smm_readarb_if1, //EGR-SMM_Read_Arb Interface
  egr_tagring_if.tagring               tagring_if0, //EGR-Tag Ring Interface
  egr_tagring_if.tagring               tagring_if1, //EGR-Tag Ring Interface
  egr_tagring_if.tagring               tagring_if2, //EGR-Tag Ring Interface
  egr_tagring_if.tagring               tagring_if3, //EGR-Tag Ring Interface
  egr_tagring_if.tagring               tagring_if4, //EGR-Tag Ring Interface
  egr_tagring_if.tagring               tagring_if5, //EGR-Tag Ring Interface
  egr_tagring_if.tagring               tagring_if6, //EGR-Tag Ring Interface
  egr_tagring_if.tagring               tagring_if7, //EGR-Tag Ring Interface
  egr_tagring_if.tagring               tagring_if8, //EGR-Tag Ring Interface
  egr_tagring_if.tagring               tagring_if9, //EGR-Tag Ring Interface
  egr_tagring_if.tagring              tagring_if10, //EGR-Tag Ring Interface
  egr_tagring_if.tagring              tagring_if11, //EGR-Tag Ring Interface
  egr_tagring_if.tagring              tagring_if12, //EGR-Tag Ring Interface
  egr_tagring_if.tagring              tagring_if13, //EGR-Tag Ring Interface
  egr_tagring_if.tagring              tagring_if14, //EGR-Tag Ring Interface
  egr_tagring_if.tagring              tagring_if15, //EGR-Tag Ring Interface
  egr_tagring_if.tagring              tagring_if16, //EGR-Tag Ring Interface
  egr_tagring_if.tagring              tagring_if17, //EGR-Tag Ring Interface
  egr_tagring_if.tagring              tagring_if18, //EGR-Tag Ring Interface
  egr_tagring_if.tagring              tagring_if19, //EGR-Tag Ring Interface
  egr_tagring_if.tagring              tagring_if20, //EGR-Tag Ring Interface
  egr_tagring_if.tagring              tagring_if21, //EGR-Tag Ring Interface
  egr_tagring_if.tagring              tagring_if22, //EGR-Tag Ring Interface
  egr_tagring_if.tagring              tagring_if23, //EGR-Tag Ring Interface
  egr_tagring_if.tagring              tagring_if24, //EGR-Tag Ring Interface
  egr_tagring_if.tagring              tagring_if25, //EGR-Tag Ring Interface
  egr_tagring_if.tagring              tagring_if26, //EGR-Tag Ring Interface
  egr_tagring_if.tagring              tagring_if27, //EGR-Tag Ring Interface
  egr_tagring_if.tagring              tagring_if28, //EGR-Tag Ring Interface
  egr_tagring_if.tagring              tagring_if29, //EGR-Tag Ring Interface
  egr_tagring_if.tagring              tagring_if30, //EGR-Tag Ring Interface
  egr_tagring_if.tagring              tagring_if31, //EGR-Tag Ring Interface
  egr_mce_tagring_if.tagring       mce_tagring_if0, //EGR-Tag Ring Interface
  egr_mce_tagring_if.tagring       mce_tagring_if1, //EGR-Tag Ring Interface
  egr_mce_tagring_if.tagring       mce_tagring_if2, //EGR-Tag Ring Interface
  egr_mce_tagring_if.tagring       mce_tagring_if3, //EGR-Tag Ring Interface

  egr_statsmgmt_if.statsmgmt          statsmgmt_if, //EGR-Stats Management Interface
  egr_tx_ppe_if.ppe                     tx_ppe_if0, //EGR-TxPPE 0 Interface
  egr_tx_ppe_if.ppe                     tx_ppe_if1, //EGR-TxPPE 1 Interface
  egr_ppe_stm_if.stm                    ppe_stm_if, //EGR-PPE Shared Table Memory Interface 
  egr_epl_if.epl                           epl_if0, //EGR-EPL 0 Interface
  egr_epl_if.epl                           epl_if1, //EGR-EPL 1 Interface
  egr_epl_if.epl                           epl_if2, //EGR-EPL 2 Interface
  egr_epl_if.epl                           epl_if3, //EGR-EPL 3 Interface
  egr_mc_table_if.mc_table            mc_table_if0, //EGR-MultiCast Shared Table 0 Interface
  egr_mc_table_if.mc_table            mc_table_if1  //EGR-MultiCast Shared Table 1 Interface
 
);


endmodule : egr_dummy_if
