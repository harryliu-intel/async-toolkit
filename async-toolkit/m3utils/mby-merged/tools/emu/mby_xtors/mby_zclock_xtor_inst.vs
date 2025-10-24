`ifndef MBY_ZCLOCK_XTOR_INST_V
`define MBY_ZCLOCK_XTOR_INST_V 1

module mby_zclock_xtor_inst (
	output cclk,
	output mclk
	);

//Clocking
wire ccosim_ctrl;
wire cclk;
wire mclk;
wire dummy02;
wire dummy03;
wire dummy04;
wire dummy05;
wire dummy06;
wire dummy07;

//--------------------------------------------------------------------
// clock driver
//--------------------------------------------------------------------
zceiClockPort ccosim_ctrl_ClockPort0 (
    .cclock( ccosim_ctrl )
);

//--------------------------------------------------------------------
// C-Cosim clock driver
//--------------------------------------------------------------------
C_COSIM mby_msh_top_ccosim (
);
defparam mby_msh_top.mby_zclock_xtor_inst.mby_msh_top_ccosim.cclock  = "mby_msh_top.mby_zclock_xtor_inst.ccosim_ctrl_ClockPort0";

//--------------------------------------------------------------------
// z_clock transactor
//--------------------------------------------------------------------
z_clock mby_z_clock (
    .clock({
	   dummy07,   //0MHz
	   dummy06,   //0MHz
	   dummy05,   //0MHz
	   dummy04,   //831MHz
           dummy03,   //900MHz
           dummy02,   //900MHz
           mclk,      //950MHz
           cclk       //950MHz
           }),
    .next({
           _z_clock_n_0_7,
           _z_clock_n_0_6,
           _z_clock_n_0_5,
           _z_clock_n_0_4,
           _z_clock_n_0_3,
           _z_clock_n_0_2,
           _z_clock_n_0_1,
           _z_clock_n_0_0
          }),
    .last({
           _z_clock_l_0_7,
           _z_clock_l_0_6,
           _z_clock_l_0_5,
           _z_clock_l_0_4,
           _z_clock_l_0_3,
           _z_clock_l_0_2,
           _z_clock_l_0_1,
           _z_clock_l_0_0
          })
);

defparam mby_msh_top.mby_zclock_xtor_inst.mby_z_clock.cclock  = "mby_msh_top.mby_zclock_xtor_inst.ccosim_ctrl_ClockPort0";

endmodule

module z_clock (output [7:0] clock, [7:0] next, [7:0] last, [7:0] negedge_enabled, [7:0] posedge_enabled);

endmodule

bind mby_msh_top mby_zclock_xtor_inst mby_zclock_xtor_inst(.*);

`endif //MBY_ZCLOCK_XTOR_INST_V
