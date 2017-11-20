//++++++++++++++++++++++++++++++++++++++++++++++++++++++
// HOW to use this file :
// every time the RTL is updated just run auto instance
// Ctrl c + ctrl A in emacs
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++


`ifdef MBY_DUMMY_DEF
module dummy();
`endif

/*AUTOREGINPUT*/
// Beginning of automatic reg inputs (for undeclared instantiated-module inputs)
reg			psf_mby_cmd_chid;	// To mby_top of mby_top.v
reg			psf_mby_cmd_put;	// To mby_top of mby_top.v
reg [1:0]		psf_mby_cmd_rtype;	// To mby_top of mby_top.v
reg			psf_mby_gnt;		// To mby_top of mby_top.v
reg			psf_mby_gnt_chid;	// To mby_top of mby_top.v
reg [1:0]		psf_mby_gnt_rtype;	// To mby_top of mby_top.v
reg [1:0]		psf_mby_gnt_type;	// To mby_top of mby_top.v
reg			psf_mby_prim_clkack;	// To mby_top of mby_top.v
reg [2:0]		psf_mby_prim_ism_fabric;// To mby_top of mby_top.v
reg [35:0]		psf_mby_taddress;	// To mby_top of mby_top.v
reg [63:0]		psf_mby_tdata;		// To mby_top of mby_top.v
reg [7:0]		psf_mby_tdest_id;	// To mby_top of mby_top.v
reg [3:0]		psf_mby_tfbe;		// To mby_top of mby_top.v
reg [1:0]		psf_mby_tfmt;		// To mby_top of mby_top.v
reg [3:0]		psf_mby_tlbe;		// To mby_top of mby_top.v
reg [9:0]		psf_mby_tlength;	// To mby_top of mby_top.v
reg			psf_mby_tns;		// To mby_top of mby_top.v
reg			psf_mby_tro;		// To mby_top of mby_top.v
reg [15:0]		psf_mby_trqid;		// To mby_top of mby_top.v
reg [7:0]		psf_mby_ttag;		// To mby_top of mby_top.v
reg			psf_mby_ttc;		// To mby_top of mby_top.v
reg [4:0]		psf_mby_ttype;		// To mby_top of mby_top.v
reg [6:0] 		psf_mby_tsai;		// To mby_top of mby_top.v
reg [6:0] 		mby_psf_msai;           // To mby_top of mby_top.v
reg 			mby_psf_mep;
reg [1:0] 		mby_psf_mat; 
reg [13:0] 		mby_psf_msrc_id;
reg [13:0] mby_psf_mdest_id;
reg [13:0] mby_psf_req_dest_id;
reg 	   prim_pok ;
  reg 	   sb_pok;  
reg			sb2_mby_eom;		// To mby_top of mby_top.v
reg			sb2_mby_npcup;		// To mby_top of mby_top.v
reg			sb2_mby_npput;		// To mby_top of mby_top.v
reg [7:0]		sb2_mby_payload;	// To mby_top of mby_top.v
reg			sb2_mby_pccup;		// To mby_top of mby_top.v
reg			sb2_mby_pcput;		// To mby_top of mby_top.v
reg			sb2_mby_side_clkack;	// To mby_top of mby_top.v
reg [2:0]		sb2_mby_side_ism_fabric;// To mby_top of mby_top.v
reg			mby_power_good_reset;	// To mby_top of mby_top.v
reg			mby_primary_clock;	// To mby_top of mby_top.v
reg			mby_primary_reset;	// To mby_top of mby_top.v
reg			mby_secondary_clock;	// To mby_top of mby_top.v
reg			mby_secondary_reset;	// To mby_top of mby_top.v
// End of automatics
/*AUTOWIRE*/
// Beginning of automatic wires (for undeclared instantiated-module outputs)
wire			mby_int;		// From mby_top of mby_top.v
wire			mby_psf_credit_chid;	// From mby_top of mby_top.v
wire			mby_psf_credit_cmd;	// From mby_top of mby_top.v
wire			mby_psf_credit_data;	// From mby_top of mby_top.v
wire			mby_psf_credit_put;	// From mby_top of mby_top.v
wire [1:0]		mby_psf_credit_rtype;	// From mby_top of mby_top.v
wire [35:0]		mby_psf_maddress;	// From mby_top of mby_top.v
wire [63:0]		mby_psf_mdata;		// From mby_top of mby_top.v
wire [3:0]		mby_psf_mfbe;		// From mby_top of mby_top.v
wire [1:0]		mby_psf_mfmt;		// From mby_top of mby_top.v
wire [3:0]		mby_psf_mlbe;		// From mby_top of mby_top.v
wire [9:0]		mby_psf_mlength;	// From mby_top of mby_top.v
wire			mby_psf_mns;		// From mby_top of mby_top.v
wire			mby_psf_mro;		// From mby_top of mby_top.v
wire [15:0]		mby_psf_mrqid;		// From mby_top of mby_top.v
wire [7:0]		mby_psf_mtag;		// From mby_top of mby_top.v
wire			mby_psf_mtc;		// From mby_top of mby_top.v
wire [4:0]		mby_psf_mtype;		// From mby_top of mby_top.v
wire			mby_psf_prim_clkreq;	// From mby_top of mby_top.v
wire [2:0]		mby_psf_prim_ism_agent;	// From mby_top of mby_top.v
wire			mby_psf_req_cdata;	// From mby_top of mby_top.v
wire			mby_psf_req_chid;	// From mby_top of mby_top.v
wire [9:0]		mby_psf_req_dlen;	// From mby_top of mby_top.v
wire			mby_psf_req_locked;	// From mby_top of mby_top.v
wire			mby_psf_req_ns;		// From mby_top of mby_top.v
wire			mby_psf_req_put;	// From mby_top of mby_top.v
wire			mby_psf_req_ro;		// From mby_top of mby_top.v
wire [1:0]		mby_psf_req_rtype;	// From mby_top of mby_top.v
wire			mby_psf_req_tc;		// From mby_top of mby_top.v
wire			mby_sb2_eom;		// From mby_top of mby_top.v
wire			mby_sb2_npcup;		// From mby_top of mby_top.v
wire			mby_sb2_npput;		// From mby_top of mby_top.v
wire [7:0]		mby_sb2_payload;	// From mby_top of mby_top.v
wire			mby_sb2_pccup;		// From mby_top of mby_top.v
wire			mby_sb2_pcput;		// From mby_top of mby_top.v
wire			mby_sb2_side_clkreq;	// From mby_top of mby_top.v
wire [2:0]		mby_sb2_side_ism_agent;	// From mby_top of mby_top.v
// End of automatics
mby_top mby_top(/*AUTOINST*/
		// Outputs
		.mby_psf_credit_chid	(mby_psf_credit_chid),
		.mby_psf_credit_cmd	(mby_psf_credit_cmd),
		.mby_psf_credit_data	(mby_psf_credit_data),
		.mby_psf_credit_put	(mby_psf_credit_put),
		.mby_psf_credit_rtype	(mby_psf_credit_rtype[1:0]),
		.mby_psf_maddress	(mby_psf_maddress[35:0]),
		.mby_psf_mdata		(mby_psf_mdata[63:0]),
		.mby_psf_mfbe		(mby_psf_mfbe[3:0]),
		.mby_psf_mfmt		(mby_psf_mfmt[1:0]),
		.mby_psf_mlbe		(mby_psf_mlbe[3:0]),
		.mby_psf_mlength	(mby_psf_mlength[9:0]),
		.mby_psf_mns		(mby_psf_mns),
		.mby_psf_mro		(mby_psf_mro),
		.mby_psf_mrqid		(mby_psf_mrqid[15:0]),
		.mby_psf_mtag		(mby_psf_mtag[7:0]),
		.mby_psf_mtc		(mby_psf_mtc),
		.mby_psf_mtype		(mby_psf_mtype[4:0]),
		.mby_psf_prim_clkreq	(mby_psf_prim_clkreq),
		.mby_psf_prim_ism_agent	(mby_psf_prim_ism_agent[2:0]),
		.mby_psf_req_cdata	(mby_psf_req_cdata),
		.mby_psf_req_chid	(mby_psf_req_chid),
		.mby_psf_req_dlen	(mby_psf_req_dlen[9:0]),
		.mby_psf_req_locked	(mby_psf_req_locked),
		.mby_psf_req_ns		(mby_psf_req_ns),
		.mby_psf_req_put	(mby_psf_req_put),
		.mby_psf_req_ro		(mby_psf_req_ro),
		.mby_psf_req_rtype	(mby_psf_req_rtype[1:0]),
		.mby_psf_req_tc		(mby_psf_req_tc),
		.mby_sb2_eom		(mby_sb2_eom),
		.mby_sb2_npcup		(mby_sb2_npcup),
		.mby_sb2_npput		(mby_sb2_npput),
		.mby_sb2_payload	(mby_sb2_payload[7:0]),
		.mby_sb2_pccup		(mby_sb2_pccup),
		.mby_sb2_pcput		(mby_sb2_pcput),
		.mby_sb2_side_clkreq	(mby_sb2_side_clkreq),
		.mby_sb2_side_ism_agent	(mby_sb2_side_ism_agent[2:0]),
		.mby_int		(mby_int),
		// Inputs
		.mby_power_good_reset	(mby_power_good_reset),
		.mby_primary_reset	(mby_primary_reset),
		.mby_secondary_reset	(mby_secondary_reset),
		.mby_primary_clock	(mby_primary_clock),
		.mby_secondary_clock	(mby_secondary_clock),
		.psf_mby_cmd_chid	(psf_mby_cmd_chid),
		.psf_mby_cmd_put	(psf_mby_cmd_put),
		.psf_mby_cmd_rtype	(psf_mby_cmd_rtype[1:0]),
		.psf_mby_gnt		(psf_mby_gnt),
		.psf_mby_gnt_chid	(psf_mby_gnt_chid),
		.psf_mby_gnt_rtype	(psf_mby_gnt_rtype[1:0]),
		.psf_mby_gnt_type	(psf_mby_gnt_type[1:0]),
		.psf_mby_prim_clkack	(psf_mby_prim_clkack),
		.psf_mby_prim_ism_fabric(psf_mby_prim_ism_fabric[2:0]),
		.psf_mby_taddress	(psf_mby_taddress[35:0]),
		.psf_mby_tdata		(psf_mby_tdata[63:0]),
		.psf_mby_tdest_id	(psf_mby_tdest_id[7:0]),
		.psf_mby_tfbe		(psf_mby_tfbe[3:0]),
		.psf_mby_tfmt		(psf_mby_tfmt[1:0]),
		.psf_mby_tlbe		(psf_mby_tlbe[3:0]),
		.psf_mby_tlength	(psf_mby_tlength[9:0]),
		.psf_mby_tns		(psf_mby_tns),
		.psf_mby_tro		(psf_mby_tro),
		.psf_mby_trqid		(psf_mby_trqid[15:0]),
		.psf_mby_ttag		(psf_mby_ttag[7:0]),
		.psf_mby_ttype		(psf_mby_ttype[4:0]),
		.psf_mby_ttc		(psf_mby_ttc),
		.sb2_mby_eom		(sb2_mby_eom),
		.sb2_mby_npcup		(sb2_mby_npcup),
		.sb2_mby_npput		(sb2_mby_npput),
		.sb2_mby_payload	(sb2_mby_payload[7:0]),
		.sb2_mby_pccup		(sb2_mby_pccup),
		.sb2_mby_pcput		(sb2_mby_pcput),
		.sb2_mby_side_clkack	(sb2_mby_side_clkack),
		.sb2_mby_side_ism_fabric(sb2_mby_side_ism_fabric[2:0]));

`ifdef MBY_DUMMY_DEF
endmodule
`endif
	 
	 
// Local Variables:
// verilog-library-directories:("." "../../source/mby/rtl/")
// End:
