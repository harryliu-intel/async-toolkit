
module mby_top(
	       //resets
	       input logic mby_power_good_reset,
	       input logic mby_primary_reset,
	       input logic mby_secondary_reset,
	       //clocks
	       input logic mby_primary_clock,
	       input logic mby_secondary_clock,
	       
   input logic psf_mby_cmd_chid,
   input logic psf_mby_cmd_put,
   input logic [1:0] psf_mby_cmd_rtype,
   input logic psf_mby_gnt,
   input logic psf_mby_gnt_chid,
   input logic [1:0] psf_mby_gnt_rtype,
   input logic [1:0] psf_mby_gnt_type,
   input logic psf_mby_prim_clkack,
   input logic [2:0] psf_mby_prim_ism_fabric,
   input logic [35:0] psf_mby_taddress,
   input logic [63:0] psf_mby_tdata,
   input logic [7:0] psf_mby_tdest_id,
   input logic [3:0] psf_mby_tfbe,
   input logic [1:0] psf_mby_tfmt,
   input logic [3:0] psf_mby_tlbe,
   input logic [9:0] psf_mby_tlength,
   input logic psf_mby_tns,
   input logic psf_mby_tro,
   input logic [15:0] psf_mby_trqid,
   input logic [7:0] psf_mby_ttag,
   input logic [4:0] psf_mby_ttype,
   input logic [6:0] psf_mby_tsai,
   input logic psf_mby_tep,
   input logic [1:0] psf_mby_tat,
   input logic psf_mby_ttc ,
	       
	       //outputs
   output logic mby_psf_credit_chid ,
   output logic mby_psf_credit_cmd ,
   output logic mby_psf_credit_data ,
   output logic mby_psf_credit_put ,
   output logic [1:0] mby_psf_credit_rtype ,
   output logic [35:0] mby_psf_maddress ,
   output logic [63:0] mby_psf_mdata ,
   output logic [3:0] mby_psf_mfbe ,
   output logic [1:0] mby_psf_mfmt ,
   output logic [3:0] mby_psf_mlbe ,
   output logic [9:0] mby_psf_mlength ,
   output logic mby_psf_mns ,
   output logic mby_psf_mro ,
   output logic [15:0] mby_psf_mrqid ,
   output logic [7:0] mby_psf_mtag ,
   output logic mby_psf_mtc ,
   output logic mby_psf_mep,
   output logic [1:0] mby_psf_mat,
   output logic [4:0] mby_psf_mtype ,
   output logic mby_psf_prim_clkreq ,
   output logic [2:0] mby_psf_prim_ism_agent ,
   output logic mby_psf_req_cdata ,
   output logic mby_psf_req_chid ,
   output logic [9:0] mby_psf_req_dlen ,
   output logic mby_psf_req_locked ,
   output logic mby_psf_req_ns ,
   output logic mby_psf_req_put ,
   output logic mby_psf_req_ro ,
   output logic [1:0] mby_psf_req_rtype ,
   output logic mby_psf_req_tc ,
   output logic [6:0] 		mby_psf_msai,
   output [13:0] mby_psf_msrc_id,
   output [13:0] mby_psf_req_dest_id,	       
	       
	       // Secondary IOSF connection to MBY
	       // ===============================================
	       
	       // Inputs
	       
   input  logic sb2_mby_eom,
   input  logic sb2_mby_npcup,
   input  logic sb2_mby_npput,
   input  logic [7:0] sb2_mby_payload,
   input  logic sb2_mby_pccup,
   input  logic sb2_mby_pcput,
   input  logic sb2_mby_side_clkack,
   input  logic [2:0] sb2_mby_side_ism_fabric,
	       
	       //outputs
   output logic mby_sb2_eom ,
   output logic mby_sb2_npcup ,
   output logic mby_sb2_npput ,
   output logic [7:0] mby_sb2_payload ,
   output logic mby_sb2_pccup ,
   output logic mby_sb2_pcput ,
   output logic mby_sb2_side_clkreq ,
   output logic [2:0] mby_sb2_side_ism_agent ,

	       //int
	       output logic mby_int
	       
	       );
  
endmodule