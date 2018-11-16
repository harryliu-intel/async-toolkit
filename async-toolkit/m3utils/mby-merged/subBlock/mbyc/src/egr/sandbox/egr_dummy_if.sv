
// -- Description  : This is a dummy block that 
//                   instantiates the modports to connect to the EGR interfaces
//------------------------------------------------------------------------------

module egr_dummy_if 
  import mby_egr_pkg::*, shared_pkg::*; 
(
  egr_igr_if.igr                            igr_if, //EGR-IGR Pointer Interface
  egr_pod_if.pod_ring                       pod_if, //EGR-Pod Ring Interface
  egr_smm_writearb_if.smm_writearb smm_writearb_if, //EGR-SMM_Write_Arb Interface
  egr_smm_readarb_if.smm_readarb   smm_readarb_if0, //EGR-SMM_Read_Arb Interface
  egr_smm_readarb_if.smm_readarb   smm_readarb_if1, //EGR-SMM_Read_Arb Interface
  egr_mce_tagring_if.tagring       mce_tagring_if0, //EGR-Tag Ring Interface
  egr_mce_tagring_if.tagring       mce_tagring_if1, //EGR-Tag Ring Interface
  egr_mce_tagring_if.tagring       mce_tagring_if2, //EGR-Tag Ring Interface
  egr_mce_tagring_if.tagring       mce_tagring_if3, //EGR-Tag Ring Interface

  egr_statsmgmt_if.statsmgmt          statsmgmt_if, //EGR-Stats Management Interface
  egr_tx_ppe_if.ppe                     tx_ppe_if0, //EGR-TxPPE 0 Interface
  egr_tx_ppe_if.ppe                     tx_ppe_if1, //EGR-TxPPE 1 Interface
  egr_ppe_stm_if.stm                    ppe_stm_if, //EGR-PPE Shared Table Memory Interface 
  egr_mc_table_if.mc_table            mc_table_if0, //EGR-MultiCast Shared Table 0 Interface
  egr_mc_table_if.mc_table            mc_table_if1  //EGR-MultiCast Shared Table 1 Interface
 
);


endmodule : egr_dummy_if
