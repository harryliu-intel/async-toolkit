
module tlm_top(
	       //resets
	       input logic tlm_power_good_reset,
	       input logic tlm_primary_reset,
	       input logic tlm_secondary_reset,
	       //clocks
	       input logic tlm_primary_clock,
	       input logic tlm_secondary_clock,
	       
   input logic psf_tlm_cmd_chid,
   input logic psf_tlm_cmd_put,
   input logic [1:0] psf_tlm_cmd_rtype,
   input logic psf_tlm_gnt,
   input logic psf_tlm_gnt_chid,
   input logic [1:0] psf_tlm_gnt_rtype,
   input logic [1:0] psf_tlm_gnt_type,
   input logic psf_tlm_prim_clkack,
   input logic [2:0] psf_tlm_prim_ism_fabric,
   input logic [35:0] psf_tlm_taddress,
   input logic [63:0] psf_tlm_tdata,
   input logic [7:0] psf_tlm_tdest_id,
   input logic [3:0] psf_tlm_tfbe,
   input logic [1:0] psf_tlm_tfmt,
   input logic [3:0] psf_tlm_tlbe,
   input logic [9:0] psf_tlm_tlength,
   input logic psf_tlm_tns,
   input logic psf_tlm_tro,
   input logic [15:0] psf_tlm_trqid,
   input logic [7:0] psf_tlm_ttag,
   input logic [4:0] psf_tlm_ttype,
   input logic [6:0] psf_tlm_tsai,
   input logic psf_tlm_tep,
   input logic [1:0] psf_tlm_tat,
   input logic psf_tlm_ttc ,
	       
	       //outputs
   output logic tlm_psf_credit_chid ,
   output logic tlm_psf_credit_cmd ,
   output logic tlm_psf_credit_data ,
   output logic tlm_psf_credit_put ,
   output logic [1:0] tlm_psf_credit_rtype ,
   output logic [35:0] tlm_psf_maddress ,
   output logic [63:0] tlm_psf_mdata ,
   output logic [3:0] tlm_psf_mfbe ,
   output logic [1:0] tlm_psf_mfmt ,
   output logic [3:0] tlm_psf_mlbe ,
   output logic [9:0] tlm_psf_mlength ,
   output logic tlm_psf_mns ,
   output logic tlm_psf_mro ,
   output logic [15:0] tlm_psf_mrqid ,
   output logic [7:0] tlm_psf_mtag ,
   output logic tlm_psf_mtc ,
   output logic tlm_psf_mep,
   output logic [1:0] tlm_psf_mat,
   output logic [4:0] tlm_psf_mtype ,
   output logic tlm_psf_prim_clkreq ,
   output logic [2:0] tlm_psf_prim_ism_agent ,
   output logic tlm_psf_req_cdata ,
   output logic tlm_psf_req_chid ,
   output logic [9:0] tlm_psf_req_dlen ,
   output logic tlm_psf_req_locked ,
   output logic tlm_psf_req_ns ,
   output logic tlm_psf_req_put ,
   output logic tlm_psf_req_ro ,
   output logic [1:0] tlm_psf_req_rtype ,
   output logic tlm_psf_req_tc ,
   output logic [6:0] 		tlm_psf_msai,
   output [13:0] tlm_psf_msrc_id,
  output [13:0] tlm_psf_req_dest_id,	       
	       
	       // Secondary IOSF connection to TLM1
	       // ===============================================
	       
	       // Inputs
	       
   input  logic sb2_tlm_eom,
   input  logic sb2_tlm_npcup,
   input  logic sb2_tlm_npput,
   input  logic [7:0] sb2_tlm_payload,
   input  logic sb2_tlm_pccup,
   input  logic sb2_tlm_pcput,
   input  logic sb2_tlm_side_clkack,
   input  logic [2:0] sb2_tlm_side_ism_fabric,
	       
	       //outputs
   output logic tlm_sb2_eom ,
   output logic tlm_sb2_npcup ,
   output logic tlm_sb2_npput ,
   output logic [7:0] tlm_sb2_payload ,
   output logic tlm_sb2_pccup ,
   output logic tlm_sb2_pcput ,
   output logic tlm_sb2_side_clkreq ,
   output logic [2:0] tlm_sb2_side_ism_agent ,

	       //int
	       output logic tlm_int
	       
	       );
  
endmodule