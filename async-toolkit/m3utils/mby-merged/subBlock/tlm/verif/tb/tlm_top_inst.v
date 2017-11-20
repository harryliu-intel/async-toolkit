//++++++++++++++++++++++++++++++++++++++++++++++++++++++
// HOW to use this file :
// every time the RTL is updated just run auto instance
// Ctrl c + ctrl A in emacs
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++


`ifdef TLM1_DUMMY_DEF
module dummy();
`endif

/*AUTOREGINPUT*/
// Beginning of automatic reg inputs (for undeclared instantiated-module inputs)
reg			psf_tlm_cmd_chid;	// To tlm_top of tlm_top.v
reg			psf_tlm_cmd_put;	// To tlm_top of tlm_top.v
reg [1:0]		psf_tlm_cmd_rtype;	// To tlm_top of tlm_top.v
reg			psf_tlm_gnt;		// To tlm_top of tlm_top.v
reg			psf_tlm_gnt_chid;	// To tlm_top of tlm_top.v
reg [1:0]		psf_tlm_gnt_rtype;	// To tlm_top of tlm_top.v
reg [1:0]		psf_tlm_gnt_type;	// To tlm_top of tlm_top.v
reg			psf_tlm_prim_clkack;	// To tlm_top of tlm_top.v
reg [2:0]		psf_tlm_prim_ism_fabric;// To tlm_top of tlm_top.v
reg [35:0]		psf_tlm_taddress;	// To tlm_top of tlm_top.v
reg [63:0]		psf_tlm_tdata;		// To tlm_top of tlm_top.v
reg [7:0]		psf_tlm_tdest_id;	// To tlm_top of tlm_top.v
reg [3:0]		psf_tlm_tfbe;		// To tlm_top of tlm_top.v
reg [1:0]		psf_tlm_tfmt;		// To tlm_top of tlm_top.v
reg [3:0]		psf_tlm_tlbe;		// To tlm_top of tlm_top.v
reg [9:0]		psf_tlm_tlength;	// To tlm_top of tlm_top.v
reg			psf_tlm_tns;		// To tlm_top of tlm_top.v
reg			psf_tlm_tro;		// To tlm_top of tlm_top.v
reg [15:0]		psf_tlm_trqid;		// To tlm_top of tlm_top.v
reg [7:0]		psf_tlm_ttag;		// To tlm_top of tlm_top.v
reg			psf_tlm_ttc;		// To tlm_top of tlm_top.v
reg [4:0]		psf_tlm_ttype;		// To tlm_top of tlm_top.v
reg [6:0] 		psf_tlm_tsai;		// To tlm_top of tlm_top.v
reg [6:0] 		tlm_psf_msai;           // To tlm_top of tlm_top.v
reg 			tlm_psf_mep;
reg [1:0] 		tlm_psf_mat; 
reg [13:0] 		tlm_psf_msrc_id;
reg [13:0] tlm_psf_mdest_id;
reg [13:0] tlm_psf_req_dest_id;
reg 	   prim_pok ;
  reg 	   sb_pok;  
reg			sb2_tlm_eom;		// To tlm_top of tlm_top.v
reg			sb2_tlm_npcup;		// To tlm_top of tlm_top.v
reg			sb2_tlm_npput;		// To tlm_top of tlm_top.v
reg [7:0]		sb2_tlm_payload;	// To tlm_top of tlm_top.v
reg			sb2_tlm_pccup;		// To tlm_top of tlm_top.v
reg			sb2_tlm_pcput;		// To tlm_top of tlm_top.v
reg			sb2_tlm_side_clkack;	// To tlm_top of tlm_top.v
reg [2:0]		sb2_tlm_side_ism_fabric;// To tlm_top of tlm_top.v
reg			tlm_power_good_reset;	// To tlm_top of tlm_top.v
reg			tlm_primary_clock;	// To tlm_top of tlm_top.v
reg			tlm_primary_reset;	// To tlm_top of tlm_top.v
reg			tlm_secondary_clock;	// To tlm_top of tlm_top.v
reg			tlm_secondary_reset;	// To tlm_top of tlm_top.v
// End of automatics
/*AUTOWIRE*/
// Beginning of automatic wires (for undeclared instantiated-module outputs)
wire			tlm_int;		// From tlm_top of tlm_top.v
wire			tlm_psf_credit_chid;	// From tlm_top of tlm_top.v
wire			tlm_psf_credit_cmd;	// From tlm_top of tlm_top.v
wire			tlm_psf_credit_data;	// From tlm_top of tlm_top.v
wire			tlm_psf_credit_put;	// From tlm_top of tlm_top.v
wire [1:0]		tlm_psf_credit_rtype;	// From tlm_top of tlm_top.v
wire [35:0]		tlm_psf_maddress;	// From tlm_top of tlm_top.v
wire [63:0]		tlm_psf_mdata;		// From tlm_top of tlm_top.v
wire [3:0]		tlm_psf_mfbe;		// From tlm_top of tlm_top.v
wire [1:0]		tlm_psf_mfmt;		// From tlm_top of tlm_top.v
wire [3:0]		tlm_psf_mlbe;		// From tlm_top of tlm_top.v
wire [9:0]		tlm_psf_mlength;	// From tlm_top of tlm_top.v
wire			tlm_psf_mns;		// From tlm_top of tlm_top.v
wire			tlm_psf_mro;		// From tlm_top of tlm_top.v
wire [15:0]		tlm_psf_mrqid;		// From tlm_top of tlm_top.v
wire [7:0]		tlm_psf_mtag;		// From tlm_top of tlm_top.v
wire			tlm_psf_mtc;		// From tlm_top of tlm_top.v
wire [4:0]		tlm_psf_mtype;		// From tlm_top of tlm_top.v
wire			tlm_psf_prim_clkreq;	// From tlm_top of tlm_top.v
wire [2:0]		tlm_psf_prim_ism_agent;	// From tlm_top of tlm_top.v
wire			tlm_psf_req_cdata;	// From tlm_top of tlm_top.v
wire			tlm_psf_req_chid;	// From tlm_top of tlm_top.v
wire [9:0]		tlm_psf_req_dlen;	// From tlm_top of tlm_top.v
wire			tlm_psf_req_locked;	// From tlm_top of tlm_top.v
wire			tlm_psf_req_ns;		// From tlm_top of tlm_top.v
wire			tlm_psf_req_put;	// From tlm_top of tlm_top.v
wire			tlm_psf_req_ro;		// From tlm_top of tlm_top.v
wire [1:0]		tlm_psf_req_rtype;	// From tlm_top of tlm_top.v
wire			tlm_psf_req_tc;		// From tlm_top of tlm_top.v
wire			tlm_sb2_eom;		// From tlm_top of tlm_top.v
wire			tlm_sb2_npcup;		// From tlm_top of tlm_top.v
wire			tlm_sb2_npput;		// From tlm_top of tlm_top.v
wire [7:0]		tlm_sb2_payload;	// From tlm_top of tlm_top.v
wire			tlm_sb2_pccup;		// From tlm_top of tlm_top.v
wire			tlm_sb2_pcput;		// From tlm_top of tlm_top.v
wire			tlm_sb2_side_clkreq;	// From tlm_top of tlm_top.v
wire [2:0]		tlm_sb2_side_ism_agent;	// From tlm_top of tlm_top.v
// End of automatics
tlm_top tlm_top(/*AUTOINST*/
		// Outputs
		.tlm_psf_credit_chid	(tlm_psf_credit_chid),
		.tlm_psf_credit_cmd	(tlm_psf_credit_cmd),
		.tlm_psf_credit_data	(tlm_psf_credit_data),
		.tlm_psf_credit_put	(tlm_psf_credit_put),
		.tlm_psf_credit_rtype	(tlm_psf_credit_rtype[1:0]),
		.tlm_psf_maddress	(tlm_psf_maddress[35:0]),
		.tlm_psf_mdata		(tlm_psf_mdata[63:0]),
		.tlm_psf_mfbe		(tlm_psf_mfbe[3:0]),
		.tlm_psf_mfmt		(tlm_psf_mfmt[1:0]),
		.tlm_psf_mlbe		(tlm_psf_mlbe[3:0]),
		.tlm_psf_mlength	(tlm_psf_mlength[9:0]),
		.tlm_psf_mns		(tlm_psf_mns),
		.tlm_psf_mro		(tlm_psf_mro),
		.tlm_psf_mrqid		(tlm_psf_mrqid[15:0]),
		.tlm_psf_mtag		(tlm_psf_mtag[7:0]),
		.tlm_psf_mtc		(tlm_psf_mtc),
		.tlm_psf_mtype		(tlm_psf_mtype[4:0]),
		.tlm_psf_prim_clkreq	(tlm_psf_prim_clkreq),
		.tlm_psf_prim_ism_agent	(tlm_psf_prim_ism_agent[2:0]),
		.tlm_psf_req_cdata	(tlm_psf_req_cdata),
		.tlm_psf_req_chid	(tlm_psf_req_chid),
		.tlm_psf_req_dlen	(tlm_psf_req_dlen[9:0]),
		.tlm_psf_req_locked	(tlm_psf_req_locked),
		.tlm_psf_req_ns		(tlm_psf_req_ns),
		.tlm_psf_req_put	(tlm_psf_req_put),
		.tlm_psf_req_ro		(tlm_psf_req_ro),
		.tlm_psf_req_rtype	(tlm_psf_req_rtype[1:0]),
		.tlm_psf_req_tc		(tlm_psf_req_tc),
		.tlm_sb2_eom		(tlm_sb2_eom),
		.tlm_sb2_npcup		(tlm_sb2_npcup),
		.tlm_sb2_npput		(tlm_sb2_npput),
		.tlm_sb2_payload	(tlm_sb2_payload[7:0]),
		.tlm_sb2_pccup		(tlm_sb2_pccup),
		.tlm_sb2_pcput		(tlm_sb2_pcput),
		.tlm_sb2_side_clkreq	(tlm_sb2_side_clkreq),
		.tlm_sb2_side_ism_agent	(tlm_sb2_side_ism_agent[2:0]),
		.tlm_int		(tlm_int),
		// Inputs
		.tlm_power_good_reset	(tlm_power_good_reset),
		.tlm_primary_reset	(tlm_primary_reset),
		.tlm_secondary_reset	(tlm_secondary_reset),
		.tlm_primary_clock	(tlm_primary_clock),
		.tlm_secondary_clock	(tlm_secondary_clock),
		.psf_tlm_cmd_chid	(psf_tlm_cmd_chid),
		.psf_tlm_cmd_put	(psf_tlm_cmd_put),
		.psf_tlm_cmd_rtype	(psf_tlm_cmd_rtype[1:0]),
		.psf_tlm_gnt		(psf_tlm_gnt),
		.psf_tlm_gnt_chid	(psf_tlm_gnt_chid),
		.psf_tlm_gnt_rtype	(psf_tlm_gnt_rtype[1:0]),
		.psf_tlm_gnt_type	(psf_tlm_gnt_type[1:0]),
		.psf_tlm_prim_clkack	(psf_tlm_prim_clkack),
		.psf_tlm_prim_ism_fabric(psf_tlm_prim_ism_fabric[2:0]),
		.psf_tlm_taddress	(psf_tlm_taddress[35:0]),
		.psf_tlm_tdata		(psf_tlm_tdata[63:0]),
		.psf_tlm_tdest_id	(psf_tlm_tdest_id[7:0]),
		.psf_tlm_tfbe		(psf_tlm_tfbe[3:0]),
		.psf_tlm_tfmt		(psf_tlm_tfmt[1:0]),
		.psf_tlm_tlbe		(psf_tlm_tlbe[3:0]),
		.psf_tlm_tlength	(psf_tlm_tlength[9:0]),
		.psf_tlm_tns		(psf_tlm_tns),
		.psf_tlm_tro		(psf_tlm_tro),
		.psf_tlm_trqid		(psf_tlm_trqid[15:0]),
		.psf_tlm_ttag		(psf_tlm_ttag[7:0]),
		.psf_tlm_ttype		(psf_tlm_ttype[4:0]),
		.psf_tlm_ttc		(psf_tlm_ttc),
		.sb2_tlm_eom		(sb2_tlm_eom),
		.sb2_tlm_npcup		(sb2_tlm_npcup),
		.sb2_tlm_npput		(sb2_tlm_npput),
		.sb2_tlm_payload	(sb2_tlm_payload[7:0]),
		.sb2_tlm_pccup		(sb2_tlm_pccup),
		.sb2_tlm_pcput		(sb2_tlm_pcput),
		.sb2_tlm_side_clkack	(sb2_tlm_side_clkack),
		.sb2_tlm_side_ism_fabric(sb2_tlm_side_ism_fabric[2:0]));

`ifdef TLM1_DUMMY_DEF
endmodule
`endif
	 
	 
// Local Variables:
// verilog-library-directories:("." "../../source/tlm/rtl/")
// End:
