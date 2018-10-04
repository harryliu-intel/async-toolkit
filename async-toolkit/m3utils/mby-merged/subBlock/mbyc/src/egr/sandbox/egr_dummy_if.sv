
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
  egr_smm_readarb_if.smm_readarb    smm_readarb_if, //EGR-SMM_Read_Arb Interface
  egr_tagring_if.tagring                tagring_if, //EGR-Tag Ring Interface
  egr_statsmgmt_if.statsmgmt          statsmgmt_if, //EGR-Stats Management Interface
  egr_tx_ppe_if.ppe                     tx_ppe_if0, //EGR-TxPPE 0 Interface
  egr_tx_ppe_if.ppe                     tx_ppe_if1, //EGR-TxPPE 1 Interface
  egr_ppe_stm_if.stm                    ppe_stm_if, //EGR-PPE Shared Table Memory Interface 
  egr_epl_if.epl                           epl_if0, //EGR-EPL 0 Interface
  egr_epl_if.epl                           epl_if1, //EGR-EPL 1 Interface
  egr_epl_if.epl                           epl_if2, //EGR-EPL 2 Interface
  egr_epl_if.epl                           epl_if3, //EGR-EPL 3 Interface
  egr_vp_if.vp                              vp_if0, //EGR-VP0 Interface
  egr_vp_if.vp                              vp_if1, //EGR-VP1 Interface
  egr_mc_table_if.mc_table            mc_table_if0, //EGR-MultiCast Shared Table 0 Interface
  egr_mc_table_if.mc_table            mc_table_if1  //EGR-MultiCast Shared Table 1 Interface  
);


endmodule : egr_dummy_if
