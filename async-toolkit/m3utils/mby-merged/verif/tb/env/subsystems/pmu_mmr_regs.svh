//RAL code compatible with saola versions: 16.2.04 and newer 

`ifndef RAL_PMU_MMR_FILE
`define RAL_PMU_MMR_FILE

class pmu_mmr_INT_STS_0_reg extends sla_ral_reg;

  // --------------------------
  sla_ral_field SIDE_POK_ALL0_STS;
  sla_ral_field SIDE_POK_ALL1_STS;
  sla_ral_field PRIM_POK_ALL0_STS;
  sla_ral_field PRIM_POK_ALL1_STS;
  sla_ral_field IP_PWR_STS_ALL0_STS;
  sla_ral_field IP_PWR_STS_ALL1_STS;
  sla_ral_field IN_SB_MSG_AVAIL_STS;
  sla_ral_field SEND_SB_PC_STS;
  sla_ral_field SB_NP_CPL_AVAIL_STS;
  sla_ral_field IN_SB_FIFO_FULL_STS;
  sla_ral_field SEND_SB_CHMSG_STS;
  sla_ral_field RSVD_13;
  sla_ral_field SSC_PLL_VALID_STS;
  sla_ral_field RSVD_15;
  sla_ral_field CGU_PLL_VALID_STS;
  sla_ral_field IP_SB_CPL_ALL1_STS;
  sla_ral_field SPIRDY_STS;
  sla_ral_field FUSE_LOAD_STS;
  sla_ral_field IP_READY_ALL1_STS;
  sla_ral_field IP_BP_RP_ALL1_STS;
  sla_ral_field RSVD_22;
  sla_ral_field RSVD_23;
  sla_ral_field FPGPOK_STS;
  sla_ral_field SETID_STS;
  sla_ral_field IP_READY_ERR_STS;
  sla_ral_field SB_CPL_ERR_STS;
  sla_ral_field RSVD_31_27;

  // --------------------------
  `ovm_object_utils(pmu_mmr_INT_STS_0_reg)

  // --------------------------
  function new(
                string         name = "",
                `ifdef SLA_RAL_COVERAGE
                sla_ral_coverage_t cov_t = COVERAGE_OFF,
                `endif
                int            bus_num=0,
                int            dev_num=0,
                int            func_num=0,
                int            offset=0,
                int            size=0,
                sla_ral_data_t reset_val=0,
                boolean_t      mon_enabled = SLA_FALSE
             );
      super.new();
      set_space_addr("CFG",undef);
      set_space_addr("MEM",undef);
      set_space_addr("IO",undef);
      set_space_addr("MSG",undef);
      set_space_addr("CREG",undef); 
      set_space_addr("LT_MEM",undef);    
 build();

      `ifdef SLA_RAL_COVERAGE
      `CONFIG_RAL_COVERAGE(regname, COVERAGE_ON, FD_RW_SAMPLE)
      if(_coverage_type != COVERAGE_OFF) begin
        if(_sample_type inside {FD_RO_SAMPLE, RO_SAMPLE})
        `CREATE_OP_SAMPLE_COV(RAL_WRITE)
        else if(_sample_type inside {FD_WO_SAMPLE, WO_SAMPLE})
        `CREATE_OP_SAMPLE_COV(RAL_READ)
        else
        `CREATE_OP_SAMPLE_COV(RAL_NONE)
        `CREATE_DESIRED_COV
        `CREATE_ACTUAL_COV
      end
      `endif

  endfunction

  `ifdef SLA_RAL_COVERAGE
  `CREATE_RAL_OP_COVERGROUP
  covergroup desired_cg @(cov_ev);
     option.per_instance = 1;
     option.name = {sla_ral_file::regfilename,".",regname};
     `RAL_FIELD_CP(SIDE_POK_ALL0_STS, SIDE_POK_ALL0_STS.desired)
     `RAL_FIELD_CP_1(SIDE_POK_ALL0_STS, SIDE_POK_ALL0_STS.desired, 0)
     `RAL_FIELD_CP(SIDE_POK_ALL1_STS, SIDE_POK_ALL1_STS.desired)
     `RAL_FIELD_CP_1(SIDE_POK_ALL1_STS, SIDE_POK_ALL1_STS.desired, 0)
     `RAL_FIELD_CP(PRIM_POK_ALL0_STS, PRIM_POK_ALL0_STS.desired)
     `RAL_FIELD_CP_1(PRIM_POK_ALL0_STS, PRIM_POK_ALL0_STS.desired, 0)
     `RAL_FIELD_CP(PRIM_POK_ALL1_STS, PRIM_POK_ALL1_STS.desired)
     `RAL_FIELD_CP_1(PRIM_POK_ALL1_STS, PRIM_POK_ALL1_STS.desired, 0)
     `RAL_FIELD_CP(IP_PWR_STS_ALL0_STS, IP_PWR_STS_ALL0_STS.desired)
     `RAL_FIELD_CP_1(IP_PWR_STS_ALL0_STS, IP_PWR_STS_ALL0_STS.desired, 0)
     `RAL_FIELD_CP(IP_PWR_STS_ALL1_STS, IP_PWR_STS_ALL1_STS.desired)
     `RAL_FIELD_CP_1(IP_PWR_STS_ALL1_STS, IP_PWR_STS_ALL1_STS.desired, 0)
     `RAL_FIELD_CP(IN_SB_MSG_AVAIL_STS, IN_SB_MSG_AVAIL_STS.desired)
     `RAL_FIELD_CP_1(IN_SB_MSG_AVAIL_STS, IN_SB_MSG_AVAIL_STS.desired, 0)
     `RAL_FIELD_CP(SEND_SB_PC_STS, SEND_SB_PC_STS.desired)
     `RAL_FIELD_CP_1(SEND_SB_PC_STS, SEND_SB_PC_STS.desired, 0)
     `RAL_FIELD_CP(SB_NP_CPL_AVAIL_STS, SB_NP_CPL_AVAIL_STS.desired)
     `RAL_FIELD_CP_1(SB_NP_CPL_AVAIL_STS, SB_NP_CPL_AVAIL_STS.desired, 0)
     `RAL_FIELD_CP(IN_SB_FIFO_FULL_STS, IN_SB_FIFO_FULL_STS.desired)
     `RAL_FIELD_CP_1(IN_SB_FIFO_FULL_STS, IN_SB_FIFO_FULL_STS.desired, 0)
     `RAL_FIELD_CP(SEND_SB_CHMSG_STS, SEND_SB_CHMSG_STS.desired)
     `RAL_FIELD_CP_1(SEND_SB_CHMSG_STS, SEND_SB_CHMSG_STS.desired, 0)
     `RAL_FIELD_CP(RSVD_13, RSVD_13.desired)
     `RAL_FIELD_CP_1(RSVD_13, RSVD_13.desired, 0)
     `RAL_FIELD_CP(SSC_PLL_VALID_STS, SSC_PLL_VALID_STS.desired)
     `RAL_FIELD_CP_1(SSC_PLL_VALID_STS, SSC_PLL_VALID_STS.desired, 0)
     `RAL_FIELD_CP(RSVD_15, RSVD_15.desired)
     `RAL_FIELD_CP_1(RSVD_15, RSVD_15.desired, 0)
     `RAL_FIELD_CP(CGU_PLL_VALID_STS, CGU_PLL_VALID_STS.desired)
     `RAL_FIELD_CP_1(CGU_PLL_VALID_STS, CGU_PLL_VALID_STS.desired, 0)
     `RAL_FIELD_CP(IP_SB_CPL_ALL1_STS, IP_SB_CPL_ALL1_STS.desired)
     `RAL_FIELD_CP_1(IP_SB_CPL_ALL1_STS, IP_SB_CPL_ALL1_STS.desired, 0)
     `RAL_FIELD_CP(SPIRDY_STS, SPIRDY_STS.desired)
     `RAL_FIELD_CP_1(SPIRDY_STS, SPIRDY_STS.desired, 0)
     `RAL_FIELD_CP(FUSE_LOAD_STS, FUSE_LOAD_STS.desired)
     `RAL_FIELD_CP_1(FUSE_LOAD_STS, FUSE_LOAD_STS.desired, 0)
     `RAL_FIELD_CP(IP_READY_ALL1_STS, IP_READY_ALL1_STS.desired)
     `RAL_FIELD_CP_1(IP_READY_ALL1_STS, IP_READY_ALL1_STS.desired, 0)
     `RAL_FIELD_CP(IP_BP_RP_ALL1_STS, IP_BP_RP_ALL1_STS.desired)
     `RAL_FIELD_CP_1(IP_BP_RP_ALL1_STS, IP_BP_RP_ALL1_STS.desired, 0)
     `RAL_FIELD_CP(RSVD_22, RSVD_22.desired)
     `RAL_FIELD_CP_1(RSVD_22, RSVD_22.desired, 0)
     `RAL_FIELD_CP(RSVD_23, RSVD_23.desired)
     `RAL_FIELD_CP_1(RSVD_23, RSVD_23.desired, 0)
     `RAL_FIELD_CP(FPGPOK_STS, FPGPOK_STS.desired)
     `RAL_FIELD_CP_1(FPGPOK_STS, FPGPOK_STS.desired, 0)
     `RAL_FIELD_CP(SETID_STS, SETID_STS.desired)
     `RAL_FIELD_CP_1(SETID_STS, SETID_STS.desired, 0)
     `RAL_FIELD_CP(IP_READY_ERR_STS, IP_READY_ERR_STS.desired)
     `RAL_FIELD_CP_1(IP_READY_ERR_STS, IP_READY_ERR_STS.desired, 0)
     `RAL_FIELD_CP(SB_CPL_ERR_STS, SB_CPL_ERR_STS.desired)
     `RAL_FIELD_CP_1(SB_CPL_ERR_STS, SB_CPL_ERR_STS.desired, 0)
     `RAL_FIELD_CP(RSVD_31_27, RSVD_31_27.desired)
     `RAL_FIELD_CP_4(RSVD_31_27, RSVD_31_27.desired, 0,1,2,3)
  endgroup
  covergroup actual_cg @(cov_ev);
     option.per_instance = 1;
     option.name = {sla_ral_file::regfilename,".",regname};
     `RAL_FIELD_CP(SIDE_POK_ALL0_STS, SIDE_POK_ALL0_STS.actual)
     `RAL_FIELD_CP_1(SIDE_POK_ALL0_STS, SIDE_POK_ALL0_STS.actual, 0)
     `RAL_FIELD_CP(SIDE_POK_ALL1_STS, SIDE_POK_ALL1_STS.actual)
     `RAL_FIELD_CP_1(SIDE_POK_ALL1_STS, SIDE_POK_ALL1_STS.actual, 0)
     `RAL_FIELD_CP(PRIM_POK_ALL0_STS, PRIM_POK_ALL0_STS.actual)
     `RAL_FIELD_CP_1(PRIM_POK_ALL0_STS, PRIM_POK_ALL0_STS.actual, 0)
     `RAL_FIELD_CP(PRIM_POK_ALL1_STS, PRIM_POK_ALL1_STS.actual)
     `RAL_FIELD_CP_1(PRIM_POK_ALL1_STS, PRIM_POK_ALL1_STS.actual, 0)
     `RAL_FIELD_CP(IP_PWR_STS_ALL0_STS, IP_PWR_STS_ALL0_STS.actual)
     `RAL_FIELD_CP_1(IP_PWR_STS_ALL0_STS, IP_PWR_STS_ALL0_STS.actual, 0)
     `RAL_FIELD_CP(IP_PWR_STS_ALL1_STS, IP_PWR_STS_ALL1_STS.actual)
     `RAL_FIELD_CP_1(IP_PWR_STS_ALL1_STS, IP_PWR_STS_ALL1_STS.actual, 0)
     `RAL_FIELD_CP(IN_SB_MSG_AVAIL_STS, IN_SB_MSG_AVAIL_STS.actual)
     `RAL_FIELD_CP_1(IN_SB_MSG_AVAIL_STS, IN_SB_MSG_AVAIL_STS.actual, 0)
     `RAL_FIELD_CP(SEND_SB_PC_STS, SEND_SB_PC_STS.actual)
     `RAL_FIELD_CP_1(SEND_SB_PC_STS, SEND_SB_PC_STS.actual, 0)
     `RAL_FIELD_CP(SB_NP_CPL_AVAIL_STS, SB_NP_CPL_AVAIL_STS.actual)
     `RAL_FIELD_CP_1(SB_NP_CPL_AVAIL_STS, SB_NP_CPL_AVAIL_STS.actual, 0)
     `RAL_FIELD_CP(IN_SB_FIFO_FULL_STS, IN_SB_FIFO_FULL_STS.actual)
     `RAL_FIELD_CP_1(IN_SB_FIFO_FULL_STS, IN_SB_FIFO_FULL_STS.actual, 0)
     `RAL_FIELD_CP(SEND_SB_CHMSG_STS, SEND_SB_CHMSG_STS.actual)
     `RAL_FIELD_CP_1(SEND_SB_CHMSG_STS, SEND_SB_CHMSG_STS.actual, 0)
     `RAL_FIELD_CP(RSVD_13, RSVD_13.actual)
     `RAL_FIELD_CP_1(RSVD_13, RSVD_13.actual, 0)
     `RAL_FIELD_CP(SSC_PLL_VALID_STS, SSC_PLL_VALID_STS.actual)
     `RAL_FIELD_CP_1(SSC_PLL_VALID_STS, SSC_PLL_VALID_STS.actual, 0)
     `RAL_FIELD_CP(RSVD_15, RSVD_15.actual)
     `RAL_FIELD_CP_1(RSVD_15, RSVD_15.actual, 0)
     `RAL_FIELD_CP(CGU_PLL_VALID_STS, CGU_PLL_VALID_STS.actual)
     `RAL_FIELD_CP_1(CGU_PLL_VALID_STS, CGU_PLL_VALID_STS.actual, 0)
     `RAL_FIELD_CP(IP_SB_CPL_ALL1_STS, IP_SB_CPL_ALL1_STS.actual)
     `RAL_FIELD_CP_1(IP_SB_CPL_ALL1_STS, IP_SB_CPL_ALL1_STS.actual, 0)
     `RAL_FIELD_CP(SPIRDY_STS, SPIRDY_STS.actual)
     `RAL_FIELD_CP_1(SPIRDY_STS, SPIRDY_STS.actual, 0)
     `RAL_FIELD_CP(FUSE_LOAD_STS, FUSE_LOAD_STS.actual)
     `RAL_FIELD_CP_1(FUSE_LOAD_STS, FUSE_LOAD_STS.actual, 0)
     `RAL_FIELD_CP(IP_READY_ALL1_STS, IP_READY_ALL1_STS.actual)
     `RAL_FIELD_CP_1(IP_READY_ALL1_STS, IP_READY_ALL1_STS.actual, 0)
     `RAL_FIELD_CP(IP_BP_RP_ALL1_STS, IP_BP_RP_ALL1_STS.actual)
     `RAL_FIELD_CP_1(IP_BP_RP_ALL1_STS, IP_BP_RP_ALL1_STS.actual, 0)
     `RAL_FIELD_CP(RSVD_22, RSVD_22.actual)
     `RAL_FIELD_CP_1(RSVD_22, RSVD_22.actual, 0)
     `RAL_FIELD_CP(RSVD_23, RSVD_23.actual)
     `RAL_FIELD_CP_1(RSVD_23, RSVD_23.actual, 0)
     `RAL_FIELD_CP(FPGPOK_STS, FPGPOK_STS.actual)
     `RAL_FIELD_CP_1(FPGPOK_STS, FPGPOK_STS.actual, 0)
     `RAL_FIELD_CP(SETID_STS, SETID_STS.actual)
     `RAL_FIELD_CP_1(SETID_STS, SETID_STS.actual, 0)
     `RAL_FIELD_CP(IP_READY_ERR_STS, IP_READY_ERR_STS.actual)
     `RAL_FIELD_CP_1(IP_READY_ERR_STS, IP_READY_ERR_STS.actual, 0)
     `RAL_FIELD_CP(SB_CPL_ERR_STS, SB_CPL_ERR_STS.actual)
     `RAL_FIELD_CP_1(SB_CPL_ERR_STS, SB_CPL_ERR_STS.actual, 0)
     `RAL_FIELD_CP(RSVD_31_27, RSVD_31_27.actual)
     `RAL_FIELD_CP_4(RSVD_31_27, RSVD_31_27.actual, 0,1,2,3)
  endgroup
  `endif

  // --------------------------
  // add constraint based on database legal values
  // --------------------------
  function void build();
    SIDE_POK_ALL0_STS = new("SIDE_POK_ALL0_STS", "RW/1C/V", 1, 0, {"INT_STS_0.SIDE_POK_ALL0_STS"});
    SIDE_POK_ALL0_STS.set_powerwell("primary");
    SIDE_POK_ALL0_STS.set_rand_mode(0);
   SIDE_POK_ALL0_STS.set_reset_signame("pmu_rst_b");
    void'(add_field( SIDE_POK_ALL0_STS ));

    SIDE_POK_ALL1_STS = new("SIDE_POK_ALL1_STS", "RW/1C/V", 1, 1, {"INT_STS_0.SIDE_POK_ALL1_STS"});
    SIDE_POK_ALL1_STS.set_powerwell("primary");
    SIDE_POK_ALL1_STS.set_rand_mode(0);
   SIDE_POK_ALL1_STS.set_reset_signame("pmu_rst_b");
    void'(add_field( SIDE_POK_ALL1_STS ));

    PRIM_POK_ALL0_STS = new("PRIM_POK_ALL0_STS", "RW/1C/V", 1, 2, {"INT_STS_0.PRIM_POK_ALL0_STS"});
    PRIM_POK_ALL0_STS.set_powerwell("primary");
    PRIM_POK_ALL0_STS.set_rand_mode(0);
   PRIM_POK_ALL0_STS.set_reset_signame("pmu_rst_b");
    void'(add_field( PRIM_POK_ALL0_STS ));

    PRIM_POK_ALL1_STS = new("PRIM_POK_ALL1_STS", "RW/1C/V", 1, 3, {"INT_STS_0.PRIM_POK_ALL1_STS"});
    PRIM_POK_ALL1_STS.set_powerwell("primary");
    PRIM_POK_ALL1_STS.set_rand_mode(0);
   PRIM_POK_ALL1_STS.set_reset_signame("pmu_rst_b");
    void'(add_field( PRIM_POK_ALL1_STS ));

    IP_PWR_STS_ALL0_STS = new("IP_PWR_STS_ALL0_STS", "RW/1C/V", 1, 6, {"INT_STS_0.IP_PWR_STS_ALL0_STS"});
    IP_PWR_STS_ALL0_STS.set_powerwell("primary");
    IP_PWR_STS_ALL0_STS.set_rand_mode(0);
   IP_PWR_STS_ALL0_STS.set_reset_signame("pmu_rst_b");
    void'(add_field( IP_PWR_STS_ALL0_STS ));

    IP_PWR_STS_ALL1_STS = new("IP_PWR_STS_ALL1_STS", "RW/1C/V", 1, 7, {"INT_STS_0.IP_PWR_STS_ALL1_STS"});
    IP_PWR_STS_ALL1_STS.set_powerwell("primary");
    IP_PWR_STS_ALL1_STS.set_rand_mode(0);
   IP_PWR_STS_ALL1_STS.set_reset_signame("pmu_rst_b");
    void'(add_field( IP_PWR_STS_ALL1_STS ));

    IN_SB_MSG_AVAIL_STS = new("IN_SB_MSG_AVAIL_STS", "RW/1C/V", 1, 8, {"INT_STS_0.IN_SB_MSG_AVAIL_STS"});
    IN_SB_MSG_AVAIL_STS.set_powerwell("primary");
    IN_SB_MSG_AVAIL_STS.set_rand_mode(0);
   IN_SB_MSG_AVAIL_STS.set_reset_signame("pmu_rst_b");
    void'(add_field( IN_SB_MSG_AVAIL_STS ));

    SEND_SB_PC_STS = new("SEND_SB_PC_STS", "RW/1C/V", 1, 9, {"INT_STS_0.SEND_SB_PC_STS"});
    SEND_SB_PC_STS.set_powerwell("primary");
    SEND_SB_PC_STS.set_rand_mode(0);
   SEND_SB_PC_STS.set_reset_signame("pmu_rst_b");
    void'(add_field( SEND_SB_PC_STS ));

    SB_NP_CPL_AVAIL_STS = new("SB_NP_CPL_AVAIL_STS", "RW/1C/V", 1, 10, {"INT_STS_0.SB_NP_CPL_AVAIL_STS"});
    SB_NP_CPL_AVAIL_STS.set_powerwell("primary");
    SB_NP_CPL_AVAIL_STS.set_rand_mode(0);
   SB_NP_CPL_AVAIL_STS.set_reset_signame("pmu_rst_b");
    void'(add_field( SB_NP_CPL_AVAIL_STS ));

    IN_SB_FIFO_FULL_STS = new("IN_SB_FIFO_FULL_STS", "RW/1C/V", 1, 11, {"INT_STS_0.IN_SB_FIFO_FULL_STS"});
    IN_SB_FIFO_FULL_STS.set_powerwell("primary");
    IN_SB_FIFO_FULL_STS.set_rand_mode(0);
   IN_SB_FIFO_FULL_STS.set_reset_signame("pmu_rst_b");
    void'(add_field( IN_SB_FIFO_FULL_STS ));

    SEND_SB_CHMSG_STS = new("SEND_SB_CHMSG_STS", "RW/1C/V", 1, 12, {"INT_STS_0.SEND_SB_CHMSG_STS"});
    SEND_SB_CHMSG_STS.set_powerwell("primary");
    SEND_SB_CHMSG_STS.set_rand_mode(0);
   SEND_SB_CHMSG_STS.set_reset_signame("pmu_rst_b");
    void'(add_field( SEND_SB_CHMSG_STS ));

    RSVD_13 = new("RSVD_13", "RO", 1, 13, {"INT_STS_0.RSVD_13"});
    RSVD_13.set_powerwell("primary");
    RSVD_13.set_rand_mode(0);
    void'(add_field( RSVD_13 ));

    SSC_PLL_VALID_STS = new("SSC_PLL_VALID_STS", "RW/1C/V", 1, 14, {"INT_STS_0.SSC_PLL_VALID_STS"});
    SSC_PLL_VALID_STS.set_powerwell("primary");
    SSC_PLL_VALID_STS.set_rand_mode(0);
   SSC_PLL_VALID_STS.set_reset_signame("pmu_rst_b");
    void'(add_field( SSC_PLL_VALID_STS ));

    RSVD_15 = new("RSVD_15", "RO", 1, 15, {"INT_STS_0.RSVD_15"});
    RSVD_15.set_powerwell("primary");
    RSVD_15.set_rand_mode(0);
    void'(add_field( RSVD_15 ));

    CGU_PLL_VALID_STS = new("CGU_PLL_VALID_STS", "RW/V", 1, 16, {"INT_STS_0.CGU_PLL_VALID_STS"});
    CGU_PLL_VALID_STS.set_powerwell("primary");
    CGU_PLL_VALID_STS.set_rand_mode(0);
   CGU_PLL_VALID_STS.set_reset_signame("pmu_rst_b");
    void'(add_field( CGU_PLL_VALID_STS ));

    IP_SB_CPL_ALL1_STS = new("IP_SB_CPL_ALL1_STS", "RW/1C/V", 1, 17, {"INT_STS_0.IP_SB_CPL_ALL1_STS"});
    IP_SB_CPL_ALL1_STS.set_powerwell("primary");
    IP_SB_CPL_ALL1_STS.set_rand_mode(0);
   IP_SB_CPL_ALL1_STS.set_reset_signame("pmu_rst_b");
    void'(add_field( IP_SB_CPL_ALL1_STS ));

    SPIRDY_STS = new("SPIRDY_STS", "RW/1C/V", 1, 18, {"INT_STS_0.SPIRDY_STS"});
    SPIRDY_STS.set_powerwell("primary");
    SPIRDY_STS.set_rand_mode(0);
   SPIRDY_STS.set_reset_signame("pmu_rst_b");
    void'(add_field( SPIRDY_STS ));

    FUSE_LOAD_STS = new("FUSE_LOAD_STS", "RW/1C/V", 1, 19, {"INT_STS_0.FUSE_LOAD_STS"});
    FUSE_LOAD_STS.set_powerwell("primary");
    FUSE_LOAD_STS.set_rand_mode(0);
   FUSE_LOAD_STS.set_reset_signame("pmu_rst_b");
    void'(add_field( FUSE_LOAD_STS ));

    IP_READY_ALL1_STS = new("IP_READY_ALL1_STS", "RW/1C/V", 1, 20, {"INT_STS_0.IP_READY_ALL1_STS"});
    IP_READY_ALL1_STS.set_powerwell("primary");
    IP_READY_ALL1_STS.set_rand_mode(0);
   IP_READY_ALL1_STS.set_reset_signame("pmu_rst_b");
    void'(add_field( IP_READY_ALL1_STS ));

    IP_BP_RP_ALL1_STS = new("IP_BP_RP_ALL1_STS", "RW/1C/V", 1, 21, {"INT_STS_0.IP_BP_RP_ALL1_STS"});
    IP_BP_RP_ALL1_STS.set_powerwell("primary");
    IP_BP_RP_ALL1_STS.set_rand_mode(0);
   IP_BP_RP_ALL1_STS.set_reset_signame("pmu_rst_b");
    void'(add_field( IP_BP_RP_ALL1_STS ));

    RSVD_22 = new("RSVD_22", "RW/1C/V", 1, 22, {"INT_STS_0.RSVD_22"});
    RSVD_22.set_powerwell("primary");
    RSVD_22.set_rand_mode(0);
   RSVD_22.set_reset_signame("pmu_rst_b");
    void'(add_field( RSVD_22 ));

    RSVD_23 = new("RSVD_23", "RW/1C/V", 1, 23, {"INT_STS_0.RSVD_23"});
    RSVD_23.set_powerwell("primary");
    RSVD_23.set_rand_mode(0);
   RSVD_23.set_reset_signame("pmu_rst_b");
    void'(add_field( RSVD_23 ));

    FPGPOK_STS = new("FPGPOK_STS", "RW/1C/V", 1, 24, {"INT_STS_0.FPGPOK_STS"});
    FPGPOK_STS.set_powerwell("primary");
    FPGPOK_STS.set_rand_mode(0);
   FPGPOK_STS.set_reset_signame("pmu_rst_b");
    void'(add_field( FPGPOK_STS ));

    SETID_STS = new("SETID_STS", "RW/1C/V", 1, 25, {"INT_STS_0.SETID_STS"});
    SETID_STS.set_powerwell("primary");
    SETID_STS.set_rand_mode(0);
   SETID_STS.set_reset_signame("pmu_rst_b");
    void'(add_field( SETID_STS ));

    IP_READY_ERR_STS = new("IP_READY_ERR_STS", "RW/1C/V", 1, 26, {"INT_STS_0.IP_READY_ERR_STS"});
    IP_READY_ERR_STS.set_powerwell("primary");
    IP_READY_ERR_STS.set_rand_mode(0);
   IP_READY_ERR_STS.set_reset_signame("pmu_rst_b");
    void'(add_field( IP_READY_ERR_STS ));

    SB_CPL_ERR_STS = new("SB_CPL_ERR_STS", "RW/1C/V", 1, 27, {"INT_STS_0.SB_CPL_ERR_STS"});
    SB_CPL_ERR_STS.set_powerwell("primary");
    SB_CPL_ERR_STS.set_rand_mode(0);
   SB_CPL_ERR_STS.set_reset_signame("pmu_rst_b");
    void'(add_field( SB_CPL_ERR_STS ));

    RSVD_31_27 = new("RSVD_31_27", "RO", 4, 28, {"INT_STS_0.RSVD_31_27"});
    RSVD_31_27.set_powerwell("primary");
    RSVD_31_27.set_rand_mode(0);
    void'(add_field( RSVD_31_27 ));

  endfunction


`DEFINE_NONE_SCOPE_REG_API(pmu_mmr_INT_STS_0_reg) 
endclass : pmu_mmr_INT_STS_0_reg

// ================================================

class pmu_mmr_INT_STS_1_reg extends sla_ral_reg;

  // --------------------------
  sla_ral_field RSVD_31_0;

  // --------------------------
  `ovm_object_utils(pmu_mmr_INT_STS_1_reg)

  // --------------------------
  function new(
                string         name = "",
                `ifdef SLA_RAL_COVERAGE
                sla_ral_coverage_t cov_t = COVERAGE_OFF,
                `endif
                int            bus_num=0,
                int            dev_num=0,
                int            func_num=0,
                int            offset=0,
                int            size=0,
                sla_ral_data_t reset_val=0,
                boolean_t      mon_enabled = SLA_FALSE
             );
      super.new();
      set_space_addr("CFG",undef);
      set_space_addr("MEM",undef);
      set_space_addr("IO",undef);
      set_space_addr("MSG",undef);
      set_space_addr("CREG",undef); 
      set_space_addr("LT_MEM",undef);    
 build();

      `ifdef SLA_RAL_COVERAGE
      `CONFIG_RAL_COVERAGE(regname, COVERAGE_ON, FD_RW_SAMPLE)
      if(_coverage_type != COVERAGE_OFF) begin
        if(_sample_type inside {FD_RO_SAMPLE, RO_SAMPLE})
        `CREATE_OP_SAMPLE_COV(RAL_WRITE)
        else if(_sample_type inside {FD_WO_SAMPLE, WO_SAMPLE})
        `CREATE_OP_SAMPLE_COV(RAL_READ)
        else
        `CREATE_OP_SAMPLE_COV(RAL_NONE)
        `CREATE_DESIRED_COV
        `CREATE_ACTUAL_COV
      end
      `endif

  endfunction

  `ifdef SLA_RAL_COVERAGE
  `CREATE_RAL_OP_COVERGROUP
  covergroup desired_cg @(cov_ev);
     option.per_instance = 1;
     option.name = {sla_ral_file::regfilename,".",regname};
     `RAL_FIELD_CP(RSVD_31_0, RSVD_31_0.desired)
     `RAL_FIELD_CP_16(RSVD_31_0, RSVD_31_0.desired, 0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)
     `RAL_FIELD_CP_16(RSVD_31_0, RSVD_31_0.desired, 16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31)
  endgroup
  covergroup actual_cg @(cov_ev);
     option.per_instance = 1;
     option.name = {sla_ral_file::regfilename,".",regname};
     `RAL_FIELD_CP(RSVD_31_0, RSVD_31_0.actual)
     `RAL_FIELD_CP_16(RSVD_31_0, RSVD_31_0.actual, 0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)
     `RAL_FIELD_CP_16(RSVD_31_0, RSVD_31_0.actual, 16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31)
  endgroup
  `endif

  // --------------------------
  // add constraint based on database legal values
  // --------------------------
  function void build();
    RSVD_31_0 = new("RSVD_31_0", "RO", 32, 0, {"INT_STS_1.RSVD_31_0"});
    RSVD_31_0.set_powerwell("primary");
    RSVD_31_0.set_rand_mode(0);
    void'(add_field( RSVD_31_0 ));

  endfunction


`DEFINE_NONE_SCOPE_REG_API(pmu_mmr_INT_STS_1_reg) 
endclass : pmu_mmr_INT_STS_1_reg

// ================================================

class pmu_mmr_INT_EN_0_reg extends sla_ral_reg;

  // --------------------------
  sla_ral_field SIDE_POK_ALL0_EN;
  sla_ral_field SIDE_POK_ALL1_EN;
  sla_ral_field PRIM_POK_ALL0_EN;
  sla_ral_field PRIM_POK_ALL1_EN;
  sla_ral_field IP_PWR_STS_ALL0_EN;
  sla_ral_field IP_PWR_STS_ALL1_EN;
  sla_ral_field IN_SB_MSG_AVAIL_EN;
  sla_ral_field SEND_SB_PC_EN;
  sla_ral_field SB_NP_CPL_AVAIL_EN;
  sla_ral_field IN_SB_FIFO_FULL_EN;
  sla_ral_field RSVD_12;
  sla_ral_field RSVD_13;
  sla_ral_field SSC_PLL_VALID_EN;
  sla_ral_field RSVD_15;
  sla_ral_field CGU_PLL_VALID_EN;
  sla_ral_field IP_SB_CPL_ALL1_EN;
  sla_ral_field SPIRDY_EN;
  sla_ral_field FUSE_LOAD_EN;
  sla_ral_field IP_READY_ALL1_EN;
  sla_ral_field IP_BP_RP_ALL1_EN;
  sla_ral_field RSVD_22;
  sla_ral_field RSVD_23;
  sla_ral_field FPGPOK_EN;
  sla_ral_field SETID_EN;
  sla_ral_field IP_READY_ERR_EN;
  sla_ral_field RSVD_31_25;

  // --------------------------
  `ovm_object_utils(pmu_mmr_INT_EN_0_reg)

  // --------------------------
  function new(
                string         name = "",
                `ifdef SLA_RAL_COVERAGE
                sla_ral_coverage_t cov_t = COVERAGE_OFF,
                `endif
                int            bus_num=0,
                int            dev_num=0,
                int            func_num=0,
                int            offset=0,
                int            size=0,
                sla_ral_data_t reset_val=0,
                boolean_t      mon_enabled = SLA_FALSE
             );
      super.new();
      set_space_addr("CFG",undef);
      set_space_addr("MEM",undef);
      set_space_addr("IO",undef);
      set_space_addr("MSG",undef);
      set_space_addr("CREG",undef); 
      set_space_addr("LT_MEM",undef);    
 build();

      `ifdef SLA_RAL_COVERAGE
      `CONFIG_RAL_COVERAGE(regname, COVERAGE_ON, FD_RW_SAMPLE)
      if(_coverage_type != COVERAGE_OFF) begin
        if(_sample_type inside {FD_RO_SAMPLE, RO_SAMPLE})
        `CREATE_OP_SAMPLE_COV(RAL_WRITE)
        else if(_sample_type inside {FD_WO_SAMPLE, WO_SAMPLE})
        `CREATE_OP_SAMPLE_COV(RAL_READ)
        else
        `CREATE_OP_SAMPLE_COV(RAL_NONE)
        `CREATE_DESIRED_COV
        `CREATE_ACTUAL_COV
      end
      `endif

  endfunction

  `ifdef SLA_RAL_COVERAGE
  `CREATE_RAL_OP_COVERGROUP
  covergroup desired_cg @(cov_ev);
     option.per_instance = 1;
     option.name = {sla_ral_file::regfilename,".",regname};
     `RAL_FIELD_CP(SIDE_POK_ALL0_EN, SIDE_POK_ALL0_EN.desired)
     `RAL_FIELD_CP_1(SIDE_POK_ALL0_EN, SIDE_POK_ALL0_EN.desired, 0)
     `RAL_FIELD_CP(SIDE_POK_ALL1_EN, SIDE_POK_ALL1_EN.desired)
     `RAL_FIELD_CP_1(SIDE_POK_ALL1_EN, SIDE_POK_ALL1_EN.desired, 0)
     `RAL_FIELD_CP(PRIM_POK_ALL0_EN, PRIM_POK_ALL0_EN.desired)
     `RAL_FIELD_CP_1(PRIM_POK_ALL0_EN, PRIM_POK_ALL0_EN.desired, 0)
     `RAL_FIELD_CP(PRIM_POK_ALL1_EN, PRIM_POK_ALL1_EN.desired)
     `RAL_FIELD_CP_1(PRIM_POK_ALL1_EN, PRIM_POK_ALL1_EN.desired, 0)
     `RAL_FIELD_CP(IP_PWR_STS_ALL0_EN, IP_PWR_STS_ALL0_EN.desired)
     `RAL_FIELD_CP_1(IP_PWR_STS_ALL0_EN, IP_PWR_STS_ALL0_EN.desired, 0)
     `RAL_FIELD_CP(IP_PWR_STS_ALL1_EN, IP_PWR_STS_ALL1_EN.desired)
     `RAL_FIELD_CP_1(IP_PWR_STS_ALL1_EN, IP_PWR_STS_ALL1_EN.desired, 0)
     `RAL_FIELD_CP(IN_SB_MSG_AVAIL_EN, IN_SB_MSG_AVAIL_EN.desired)
     `RAL_FIELD_CP_1(IN_SB_MSG_AVAIL_EN, IN_SB_MSG_AVAIL_EN.desired, 0)
     `RAL_FIELD_CP(SEND_SB_PC_EN, SEND_SB_PC_EN.desired)
     `RAL_FIELD_CP_1(SEND_SB_PC_EN, SEND_SB_PC_EN.desired, 0)
     `RAL_FIELD_CP(SB_NP_CPL_AVAIL_EN, SB_NP_CPL_AVAIL_EN.desired)
     `RAL_FIELD_CP_1(SB_NP_CPL_AVAIL_EN, SB_NP_CPL_AVAIL_EN.desired, 0)
     `RAL_FIELD_CP(IN_SB_FIFO_FULL_EN, IN_SB_FIFO_FULL_EN.desired)
     `RAL_FIELD_CP_1(IN_SB_FIFO_FULL_EN, IN_SB_FIFO_FULL_EN.desired, 0)
     `RAL_FIELD_CP(RSVD_12, RSVD_12.desired)
     `RAL_FIELD_CP_1(RSVD_12, RSVD_12.desired, 0)
     `RAL_FIELD_CP(RSVD_13, RSVD_13.desired)
     `RAL_FIELD_CP_1(RSVD_13, RSVD_13.desired, 0)
     `RAL_FIELD_CP(SSC_PLL_VALID_EN, SSC_PLL_VALID_EN.desired)
     `RAL_FIELD_CP_1(SSC_PLL_VALID_EN, SSC_PLL_VALID_EN.desired, 0)
     `RAL_FIELD_CP(RSVD_15, RSVD_15.desired)
     `RAL_FIELD_CP_1(RSVD_15, RSVD_15.desired, 0)
     `RAL_FIELD_CP(CGU_PLL_VALID_EN, CGU_PLL_VALID_EN.desired)
     `RAL_FIELD_CP_1(CGU_PLL_VALID_EN, CGU_PLL_VALID_EN.desired, 0)
     `RAL_FIELD_CP(IP_SB_CPL_ALL1_EN, IP_SB_CPL_ALL1_EN.desired)
     `RAL_FIELD_CP_1(IP_SB_CPL_ALL1_EN, IP_SB_CPL_ALL1_EN.desired, 0)
     `RAL_FIELD_CP(SPIRDY_EN, SPIRDY_EN.desired)
     `RAL_FIELD_CP_1(SPIRDY_EN, SPIRDY_EN.desired, 0)
     `RAL_FIELD_CP(FUSE_LOAD_EN, FUSE_LOAD_EN.desired)
     `RAL_FIELD_CP_1(FUSE_LOAD_EN, FUSE_LOAD_EN.desired, 0)
     `RAL_FIELD_CP(IP_READY_ALL1_EN, IP_READY_ALL1_EN.desired)
     `RAL_FIELD_CP_1(IP_READY_ALL1_EN, IP_READY_ALL1_EN.desired, 0)
     `RAL_FIELD_CP(IP_BP_RP_ALL1_EN, IP_BP_RP_ALL1_EN.desired)
     `RAL_FIELD_CP_1(IP_BP_RP_ALL1_EN, IP_BP_RP_ALL1_EN.desired, 0)
     `RAL_FIELD_CP(RSVD_22, RSVD_22.desired)
     `RAL_FIELD_CP_1(RSVD_22, RSVD_22.desired, 0)
     `RAL_FIELD_CP(RSVD_23, RSVD_23.desired)
     `RAL_FIELD_CP_1(RSVD_23, RSVD_23.desired, 0)
     `RAL_FIELD_CP(FPGPOK_EN, FPGPOK_EN.desired)
     `RAL_FIELD_CP_1(FPGPOK_EN, FPGPOK_EN.desired, 0)
     `RAL_FIELD_CP(SETID_EN, SETID_EN.desired)
     `RAL_FIELD_CP_1(SETID_EN, SETID_EN.desired, 0)
     `RAL_FIELD_CP(IP_READY_ERR_EN, IP_READY_ERR_EN.desired)
     `RAL_FIELD_CP_1(IP_READY_ERR_EN, IP_READY_ERR_EN.desired, 0)
     `RAL_FIELD_CP(RSVD_31_25, RSVD_31_25.desired)
     `RAL_FIELD_CP_5(RSVD_31_25, RSVD_31_25.desired, 0,1,2,3,4)
  endgroup
  covergroup actual_cg @(cov_ev);
     option.per_instance = 1;
     option.name = {sla_ral_file::regfilename,".",regname};
     `RAL_FIELD_CP(SIDE_POK_ALL0_EN, SIDE_POK_ALL0_EN.actual)
     `RAL_FIELD_CP_1(SIDE_POK_ALL0_EN, SIDE_POK_ALL0_EN.actual, 0)
     `RAL_FIELD_CP(SIDE_POK_ALL1_EN, SIDE_POK_ALL1_EN.actual)
     `RAL_FIELD_CP_1(SIDE_POK_ALL1_EN, SIDE_POK_ALL1_EN.actual, 0)
     `RAL_FIELD_CP(PRIM_POK_ALL0_EN, PRIM_POK_ALL0_EN.actual)
     `RAL_FIELD_CP_1(PRIM_POK_ALL0_EN, PRIM_POK_ALL0_EN.actual, 0)
     `RAL_FIELD_CP(PRIM_POK_ALL1_EN, PRIM_POK_ALL1_EN.actual)
     `RAL_FIELD_CP_1(PRIM_POK_ALL1_EN, PRIM_POK_ALL1_EN.actual, 0)
     `RAL_FIELD_CP(IP_PWR_STS_ALL0_EN, IP_PWR_STS_ALL0_EN.actual)
     `RAL_FIELD_CP_1(IP_PWR_STS_ALL0_EN, IP_PWR_STS_ALL0_EN.actual, 0)
     `RAL_FIELD_CP(IP_PWR_STS_ALL1_EN, IP_PWR_STS_ALL1_EN.actual)
     `RAL_FIELD_CP_1(IP_PWR_STS_ALL1_EN, IP_PWR_STS_ALL1_EN.actual, 0)
     `RAL_FIELD_CP(IN_SB_MSG_AVAIL_EN, IN_SB_MSG_AVAIL_EN.actual)
     `RAL_FIELD_CP_1(IN_SB_MSG_AVAIL_EN, IN_SB_MSG_AVAIL_EN.actual, 0)
     `RAL_FIELD_CP(SEND_SB_PC_EN, SEND_SB_PC_EN.actual)
     `RAL_FIELD_CP_1(SEND_SB_PC_EN, SEND_SB_PC_EN.actual, 0)
     `RAL_FIELD_CP(SB_NP_CPL_AVAIL_EN, SB_NP_CPL_AVAIL_EN.actual)
     `RAL_FIELD_CP_1(SB_NP_CPL_AVAIL_EN, SB_NP_CPL_AVAIL_EN.actual, 0)
     `RAL_FIELD_CP(IN_SB_FIFO_FULL_EN, IN_SB_FIFO_FULL_EN.actual)
     `RAL_FIELD_CP_1(IN_SB_FIFO_FULL_EN, IN_SB_FIFO_FULL_EN.actual, 0)
     `RAL_FIELD_CP(RSVD_12, RSVD_12.actual)
     `RAL_FIELD_CP_1(RSVD_12, RSVD_12.actual, 0)
     `RAL_FIELD_CP(RSVD_13, RSVD_13.actual)
     `RAL_FIELD_CP_1(RSVD_13, RSVD_13.actual, 0)
     `RAL_FIELD_CP(SSC_PLL_VALID_EN, SSC_PLL_VALID_EN.actual)
     `RAL_FIELD_CP_1(SSC_PLL_VALID_EN, SSC_PLL_VALID_EN.actual, 0)
     `RAL_FIELD_CP(RSVD_15, RSVD_15.actual)
     `RAL_FIELD_CP_1(RSVD_15, RSVD_15.actual, 0)
     `RAL_FIELD_CP(CGU_PLL_VALID_EN, CGU_PLL_VALID_EN.actual)
     `RAL_FIELD_CP_1(CGU_PLL_VALID_EN, CGU_PLL_VALID_EN.actual, 0)
     `RAL_FIELD_CP(IP_SB_CPL_ALL1_EN, IP_SB_CPL_ALL1_EN.actual)
     `RAL_FIELD_CP_1(IP_SB_CPL_ALL1_EN, IP_SB_CPL_ALL1_EN.actual, 0)
     `RAL_FIELD_CP(SPIRDY_EN, SPIRDY_EN.actual)
     `RAL_FIELD_CP_1(SPIRDY_EN, SPIRDY_EN.actual, 0)
     `RAL_FIELD_CP(FUSE_LOAD_EN, FUSE_LOAD_EN.actual)
     `RAL_FIELD_CP_1(FUSE_LOAD_EN, FUSE_LOAD_EN.actual, 0)
     `RAL_FIELD_CP(IP_READY_ALL1_EN, IP_READY_ALL1_EN.actual)
     `RAL_FIELD_CP_1(IP_READY_ALL1_EN, IP_READY_ALL1_EN.actual, 0)
     `RAL_FIELD_CP(IP_BP_RP_ALL1_EN, IP_BP_RP_ALL1_EN.actual)
     `RAL_FIELD_CP_1(IP_BP_RP_ALL1_EN, IP_BP_RP_ALL1_EN.actual, 0)
     `RAL_FIELD_CP(RSVD_22, RSVD_22.actual)
     `RAL_FIELD_CP_1(RSVD_22, RSVD_22.actual, 0)
     `RAL_FIELD_CP(RSVD_23, RSVD_23.actual)
     `RAL_FIELD_CP_1(RSVD_23, RSVD_23.actual, 0)
     `RAL_FIELD_CP(FPGPOK_EN, FPGPOK_EN.actual)
     `RAL_FIELD_CP_1(FPGPOK_EN, FPGPOK_EN.actual, 0)
     `RAL_FIELD_CP(SETID_EN, SETID_EN.actual)
     `RAL_FIELD_CP_1(SETID_EN, SETID_EN.actual, 0)
     `RAL_FIELD_CP(IP_READY_ERR_EN, IP_READY_ERR_EN.actual)
     `RAL_FIELD_CP_1(IP_READY_ERR_EN, IP_READY_ERR_EN.actual, 0)
     `RAL_FIELD_CP(RSVD_31_25, RSVD_31_25.actual)
     `RAL_FIELD_CP_5(RSVD_31_25, RSVD_31_25.actual, 0,1,2,3,4)
  endgroup
  `endif

  // --------------------------
  // add constraint based on database legal values
  // --------------------------
  function void build();
    SIDE_POK_ALL0_EN = new("SIDE_POK_ALL0_EN", "RW", 1, 0, {"INT_EN_0.SIDE_POK_ALL0_EN"});
    SIDE_POK_ALL0_EN.set_powerwell("primary");
    SIDE_POK_ALL0_EN.set_rand_mode(0);
   SIDE_POK_ALL0_EN.set_reset_signame("pmu_rst_b");
    void'(add_field( SIDE_POK_ALL0_EN ));

    SIDE_POK_ALL1_EN = new("SIDE_POK_ALL1_EN", "RW", 1, 1, {"INT_EN_0.SIDE_POK_ALL1_EN"});
    SIDE_POK_ALL1_EN.set_powerwell("primary");
    SIDE_POK_ALL1_EN.set_rand_mode(0);
   SIDE_POK_ALL1_EN.set_reset_signame("pmu_rst_b");
    void'(add_field( SIDE_POK_ALL1_EN ));

    PRIM_POK_ALL0_EN = new("PRIM_POK_ALL0_EN", "RW", 1, 2, {"INT_EN_0.PRIM_POK_ALL0_EN"});
    PRIM_POK_ALL0_EN.set_powerwell("primary");
    PRIM_POK_ALL0_EN.set_rand_mode(0);
   PRIM_POK_ALL0_EN.set_reset_signame("pmu_rst_b");
    void'(add_field( PRIM_POK_ALL0_EN ));

    PRIM_POK_ALL1_EN = new("PRIM_POK_ALL1_EN", "RW", 1, 3, {"INT_EN_0.PRIM_POK_ALL1_EN"});
    PRIM_POK_ALL1_EN.set_powerwell("primary");
    PRIM_POK_ALL1_EN.set_rand_mode(0);
   PRIM_POK_ALL1_EN.set_reset_signame("pmu_rst_b");
    void'(add_field( PRIM_POK_ALL1_EN ));

    IP_PWR_STS_ALL0_EN = new("IP_PWR_STS_ALL0_EN", "RW", 1, 6, {"INT_EN_0.IP_PWR_STS_ALL0_EN"});
    IP_PWR_STS_ALL0_EN.set_powerwell("primary");
    IP_PWR_STS_ALL0_EN.set_rand_mode(0);
   IP_PWR_STS_ALL0_EN.set_reset_signame("pmu_rst_b");
    void'(add_field( IP_PWR_STS_ALL0_EN ));

    IP_PWR_STS_ALL1_EN = new("IP_PWR_STS_ALL1_EN", "RW", 1, 7, {"INT_EN_0.IP_PWR_STS_ALL1_EN"});
    IP_PWR_STS_ALL1_EN.set_powerwell("primary");
    IP_PWR_STS_ALL1_EN.set_rand_mode(0);
   IP_PWR_STS_ALL1_EN.set_reset_signame("pmu_rst_b");
    void'(add_field( IP_PWR_STS_ALL1_EN ));

    IN_SB_MSG_AVAIL_EN = new("IN_SB_MSG_AVAIL_EN", "RW", 1, 8, {"INT_EN_0.IN_SB_MSG_AVAIL_EN"});
    IN_SB_MSG_AVAIL_EN.set_powerwell("primary");
    IN_SB_MSG_AVAIL_EN.set_rand_mode(0);
   IN_SB_MSG_AVAIL_EN.set_reset_signame("pmu_rst_b");
    void'(add_field( IN_SB_MSG_AVAIL_EN ));

    SEND_SB_PC_EN = new("SEND_SB_PC_EN", "RW", 1, 9, {"INT_EN_0.SEND_SB_PC_EN"});
    SEND_SB_PC_EN.set_powerwell("primary");
    SEND_SB_PC_EN.set_rand_mode(0);
   SEND_SB_PC_EN.set_reset_signame("pmu_rst_b");
    void'(add_field( SEND_SB_PC_EN ));

    SB_NP_CPL_AVAIL_EN = new("SB_NP_CPL_AVAIL_EN", "RW", 1, 10, {"INT_EN_0.SB_NP_CPL_AVAIL_EN"});
    SB_NP_CPL_AVAIL_EN.set_powerwell("primary");
    SB_NP_CPL_AVAIL_EN.set_rand_mode(0);
   SB_NP_CPL_AVAIL_EN.set_reset_signame("pmu_rst_b");
    void'(add_field( SB_NP_CPL_AVAIL_EN ));

    IN_SB_FIFO_FULL_EN = new("IN_SB_FIFO_FULL_EN", "RW", 1, 11, {"INT_EN_0.IN_SB_FIFO_FULL_EN"});
    IN_SB_FIFO_FULL_EN.set_powerwell("primary");
    IN_SB_FIFO_FULL_EN.set_rand_mode(0);
   IN_SB_FIFO_FULL_EN.set_reset_signame("pmu_rst_b");
    void'(add_field( IN_SB_FIFO_FULL_EN ));

    RSVD_12 = new("RSVD_12", "RO", 1, 12, {"INT_EN_0.RSVD_12"});
    RSVD_12.set_powerwell("primary");
    RSVD_12.set_rand_mode(0);
    void'(add_field( RSVD_12 ));

    RSVD_13 = new("RSVD_13", "RO", 1, 13, {"INT_EN_0.RSVD_13"});
    RSVD_13.set_powerwell("primary");
    RSVD_13.set_rand_mode(0);
    void'(add_field( RSVD_13 ));

    SSC_PLL_VALID_EN = new("SSC_PLL_VALID_EN", "RW", 1, 14, {"INT_EN_0.SSC_PLL_VALID_EN"});
    SSC_PLL_VALID_EN.set_powerwell("primary");
    SSC_PLL_VALID_EN.set_rand_mode(0);
   SSC_PLL_VALID_EN.set_reset_signame("pmu_rst_b");
    void'(add_field( SSC_PLL_VALID_EN ));

    RSVD_15 = new("RSVD_15", "RO", 1, 15, {"INT_EN_0.RSVD_15"});
    RSVD_15.set_powerwell("primary");
    RSVD_15.set_rand_mode(0);
    void'(add_field( RSVD_15 ));

    CGU_PLL_VALID_EN = new("CGU_PLL_VALID_EN", "RW", 1, 16, {"INT_EN_0.CGU_PLL_VALID_EN"});
    CGU_PLL_VALID_EN.set_powerwell("primary");
    CGU_PLL_VALID_EN.set_rand_mode(0);
   CGU_PLL_VALID_EN.set_reset_signame("pmu_rst_b");
    void'(add_field( CGU_PLL_VALID_EN ));

    IP_SB_CPL_ALL1_EN = new("IP_SB_CPL_ALL1_EN", "RW", 1, 17, {"INT_EN_0.IP_SB_CPL_ALL1_EN"});
    IP_SB_CPL_ALL1_EN.set_powerwell("primary");
    IP_SB_CPL_ALL1_EN.set_rand_mode(0);
   IP_SB_CPL_ALL1_EN.set_reset_signame("pmu_rst_b");
    void'(add_field( IP_SB_CPL_ALL1_EN ));

    SPIRDY_EN = new("SPIRDY_EN", "RW", 1, 18, {"INT_EN_0.SPIRDY_EN"});
    SPIRDY_EN.set_powerwell("primary");
    SPIRDY_EN.set_rand_mode(0);
   SPIRDY_EN.set_reset_signame("pmu_rst_b");
    void'(add_field( SPIRDY_EN ));

    FUSE_LOAD_EN = new("FUSE_LOAD_EN", "RW", 1, 19, {"INT_EN_0.FUSE_LOAD_EN"});
    FUSE_LOAD_EN.set_powerwell("primary");
    FUSE_LOAD_EN.set_rand_mode(0);
   FUSE_LOAD_EN.set_reset_signame("pmu_rst_b");
    void'(add_field( FUSE_LOAD_EN ));

    IP_READY_ALL1_EN = new("IP_READY_ALL1_EN", "RW", 1, 20, {"INT_EN_0.IP_READY_ALL1_EN"});
    IP_READY_ALL1_EN.set_powerwell("primary");
    IP_READY_ALL1_EN.set_rand_mode(0);
   IP_READY_ALL1_EN.set_reset_signame("pmu_rst_b");
    void'(add_field( IP_READY_ALL1_EN ));

    IP_BP_RP_ALL1_EN = new("IP_BP_RP_ALL1_EN", "RW", 1, 21, {"INT_EN_0.IP_BP_RP_ALL1_EN"});
    IP_BP_RP_ALL1_EN.set_powerwell("primary");
    IP_BP_RP_ALL1_EN.set_rand_mode(0);
   IP_BP_RP_ALL1_EN.set_reset_signame("pmu_rst_b");
    void'(add_field( IP_BP_RP_ALL1_EN ));

    RSVD_22 = new("RSVD_22", "RW", 1, 22, {"INT_EN_0.RSVD_22"});
    RSVD_22.set_powerwell("primary");
    RSVD_22.set_rand_mode(0);
   RSVD_22.set_reset_signame("pmu_rst_b");
    void'(add_field( RSVD_22 ));

    RSVD_23 = new("RSVD_23", "RW", 1, 23, {"INT_EN_0.RSVD_23"});
    RSVD_23.set_powerwell("primary");
    RSVD_23.set_rand_mode(0);
   RSVD_23.set_reset_signame("pmu_rst_b");
    void'(add_field( RSVD_23 ));

    FPGPOK_EN = new("FPGPOK_EN", "RW", 1, 24, {"INT_EN_0.FPGPOK_EN"});
    FPGPOK_EN.set_powerwell("primary");
    FPGPOK_EN.set_rand_mode(0);
   FPGPOK_EN.set_reset_signame("pmu_rst_b");
    void'(add_field( FPGPOK_EN ));

    SETID_EN = new("SETID_EN", "RW", 1, 25, {"INT_EN_0.SETID_EN"});
    SETID_EN.set_powerwell("primary");
    SETID_EN.set_rand_mode(0);
   SETID_EN.set_reset_signame("pmu_rst_b");
    void'(add_field( SETID_EN ));

    IP_READY_ERR_EN = new("IP_READY_ERR_EN", "RW", 1, 26, {"INT_EN_0.IP_READY_ERR_EN"});
    IP_READY_ERR_EN.set_powerwell("primary");
    IP_READY_ERR_EN.set_rand_mode(0);
   IP_READY_ERR_EN.set_reset_signame("pmu_rst_b");
    void'(add_field( IP_READY_ERR_EN ));

    RSVD_31_25 = new("RSVD_31_25", "RO", 5, 27, {"INT_EN_0.RSVD_31_25"});
    RSVD_31_25.set_powerwell("primary");
    RSVD_31_25.set_rand_mode(0);
    void'(add_field( RSVD_31_25 ));

  endfunction


`DEFINE_NONE_SCOPE_REG_API(pmu_mmr_INT_EN_0_reg) 
endclass : pmu_mmr_INT_EN_0_reg

// ================================================

class pmu_mmr_INT_EN_1_reg extends sla_ral_reg;

  // --------------------------
  sla_ral_field RSVD_31_0;

  // --------------------------
  `ovm_object_utils(pmu_mmr_INT_EN_1_reg)

  // --------------------------
  function new(
                string         name = "",
                `ifdef SLA_RAL_COVERAGE
                sla_ral_coverage_t cov_t = COVERAGE_OFF,
                `endif
                int            bus_num=0,
                int            dev_num=0,
                int            func_num=0,
                int            offset=0,
                int            size=0,
                sla_ral_data_t reset_val=0,
                boolean_t      mon_enabled = SLA_FALSE
             );
      super.new();
      set_space_addr("CFG",undef);
      set_space_addr("MEM",undef);
      set_space_addr("IO",undef);
      set_space_addr("MSG",undef);
      set_space_addr("CREG",undef); 
      set_space_addr("LT_MEM",undef);    
 build();

      `ifdef SLA_RAL_COVERAGE
      `CONFIG_RAL_COVERAGE(regname, COVERAGE_ON, FD_RW_SAMPLE)
      if(_coverage_type != COVERAGE_OFF) begin
        if(_sample_type inside {FD_RO_SAMPLE, RO_SAMPLE})
        `CREATE_OP_SAMPLE_COV(RAL_WRITE)
        else if(_sample_type inside {FD_WO_SAMPLE, WO_SAMPLE})
        `CREATE_OP_SAMPLE_COV(RAL_READ)
        else
        `CREATE_OP_SAMPLE_COV(RAL_NONE)
        `CREATE_DESIRED_COV
        `CREATE_ACTUAL_COV
      end
      `endif

  endfunction

  `ifdef SLA_RAL_COVERAGE
  `CREATE_RAL_OP_COVERGROUP
  covergroup desired_cg @(cov_ev);
     option.per_instance = 1;
     option.name = {sla_ral_file::regfilename,".",regname};
     `RAL_FIELD_CP(RSVD_31_0, RSVD_31_0.desired)
     `RAL_FIELD_CP_16(RSVD_31_0, RSVD_31_0.desired, 0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)
     `RAL_FIELD_CP_16(RSVD_31_0, RSVD_31_0.desired, 16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31)
  endgroup
  covergroup actual_cg @(cov_ev);
     option.per_instance = 1;
     option.name = {sla_ral_file::regfilename,".",regname};
     `RAL_FIELD_CP(RSVD_31_0, RSVD_31_0.actual)
     `RAL_FIELD_CP_16(RSVD_31_0, RSVD_31_0.actual, 0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)
     `RAL_FIELD_CP_16(RSVD_31_0, RSVD_31_0.actual, 16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31)
  endgroup
  `endif

  // --------------------------
  // add constraint based on database legal values
  // --------------------------
  function void build();
    RSVD_31_0 = new("RSVD_31_0", "RO", 32, 0, {"INT_EN_1.RSVD_31_0"});
    RSVD_31_0.set_powerwell("primary");
    RSVD_31_0.set_rand_mode(0);
   RSVD_31_0.set_reset_signame("pmu_rst_b");
    void'(add_field( RSVD_31_0 ));

  endfunction


`DEFINE_NONE_SCOPE_REG_API(pmu_mmr_INT_EN_1_reg) 
endclass : pmu_mmr_INT_EN_1_reg

// ================================================

class pmu_mmr_INT_CTRL_STS_reg extends sla_ral_reg;

  // --------------------------
  sla_ral_field INT_ADDR;
  sla_ral_field INT_CMPL_UC_F;
  sla_ral_field INT_CMPL_SAI_F;
  sla_ral_field INT_CMPL_UR;

  // --------------------------
  `ovm_object_utils(pmu_mmr_INT_CTRL_STS_reg)

  // --------------------------
  function new(
                string         name = "",
                `ifdef SLA_RAL_COVERAGE
                sla_ral_coverage_t cov_t = COVERAGE_OFF,
                `endif
                int            bus_num=0,
                int            dev_num=0,
                int            func_num=0,
                int            offset=0,
                int            size=0,
                sla_ral_data_t reset_val=0,
                boolean_t      mon_enabled = SLA_FALSE
             );
      super.new();
      set_space_addr("CFG",undef);
      set_space_addr("MEM",undef);
      set_space_addr("IO",undef);
      set_space_addr("MSG",undef);
      set_space_addr("CREG",undef); 
      set_space_addr("LT_MEM",undef);    
 build();

      `ifdef SLA_RAL_COVERAGE
      `CONFIG_RAL_COVERAGE(regname, COVERAGE_ON, FD_RW_SAMPLE)
      if(_coverage_type != COVERAGE_OFF) begin
        if(_sample_type inside {FD_RO_SAMPLE, RO_SAMPLE})
        `CREATE_OP_SAMPLE_COV(RAL_WRITE)
        else if(_sample_type inside {FD_WO_SAMPLE, WO_SAMPLE})
        `CREATE_OP_SAMPLE_COV(RAL_READ)
        else
        `CREATE_OP_SAMPLE_COV(RAL_NONE)
        `CREATE_DESIRED_COV
        `CREATE_ACTUAL_COV
      end
      `endif

  endfunction

  `ifdef SLA_RAL_COVERAGE
  `CREATE_RAL_OP_COVERGROUP
  covergroup desired_cg @(cov_ev);
     option.per_instance = 1;
     option.name = {sla_ral_file::regfilename,".",regname};
     `RAL_FIELD_CP(INT_ADDR, INT_ADDR.desired)
     `RAL_FIELD_CP_16(INT_ADDR, INT_ADDR.desired, 0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)
     `RAL_FIELD_CP(INT_CMPL_UC_F, INT_CMPL_UC_F.desired)
     `RAL_FIELD_CP_1(INT_CMPL_UC_F, INT_CMPL_UC_F.desired, 0)
     `RAL_FIELD_CP(INT_CMPL_SAI_F, INT_CMPL_SAI_F.desired)
     `RAL_FIELD_CP_1(INT_CMPL_SAI_F, INT_CMPL_SAI_F.desired, 0)
     `RAL_FIELD_CP(INT_CMPL_UR, INT_CMPL_UR.desired)
     `RAL_FIELD_CP_1(INT_CMPL_UR, INT_CMPL_UR.desired, 0)
  endgroup
  covergroup actual_cg @(cov_ev);
     option.per_instance = 1;
     option.name = {sla_ral_file::regfilename,".",regname};
     `RAL_FIELD_CP(INT_ADDR, INT_ADDR.actual)
     `RAL_FIELD_CP_16(INT_ADDR, INT_ADDR.actual, 0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)
     `RAL_FIELD_CP(INT_CMPL_UC_F, INT_CMPL_UC_F.actual)
     `RAL_FIELD_CP_1(INT_CMPL_UC_F, INT_CMPL_UC_F.actual, 0)
     `RAL_FIELD_CP(INT_CMPL_SAI_F, INT_CMPL_SAI_F.actual)
     `RAL_FIELD_CP_1(INT_CMPL_SAI_F, INT_CMPL_SAI_F.actual, 0)
     `RAL_FIELD_CP(INT_CMPL_UR, INT_CMPL_UR.actual)
     `RAL_FIELD_CP_1(INT_CMPL_UR, INT_CMPL_UR.actual, 0)
  endgroup
  `endif

  // --------------------------
  // add constraint based on database legal values
  // --------------------------
  function void build();
    INT_ADDR = new("INT_ADDR", "RW", 16, 0, {"INT_CTRL_STS.INT_ADDR"});
    INT_ADDR.set_powerwell("primary");
    INT_ADDR.set_rand_mode(0);
   INT_ADDR.set_reset_signame("pmu_rst_b");
    void'(add_field( INT_ADDR ));

    INT_CMPL_UC_F = new("INT_CMPL_UC_F", "RW/V", 1, 29, {"INT_CTRL_STS.INT_CMPL_UC_F"});
    INT_CMPL_UC_F.set_powerwell("primary");
    INT_CMPL_UC_F.set_rand_mode(0);
   INT_CMPL_UC_F.set_reset_signame("pmu_rst_b");
    void'(add_field( INT_CMPL_UC_F ));

    INT_CMPL_SAI_F = new("INT_CMPL_SAI_F", "RW/V", 1, 30, {"INT_CTRL_STS.INT_CMPL_SAI_F"});
    INT_CMPL_SAI_F.set_powerwell("primary");
    INT_CMPL_SAI_F.set_rand_mode(0);
   INT_CMPL_SAI_F.set_reset_signame("pmu_rst_b");
    void'(add_field( INT_CMPL_SAI_F ));

    INT_CMPL_UR = new("INT_CMPL_UR", "RW/V", 1, 31, {"INT_CTRL_STS.INT_CMPL_UR"});
    INT_CMPL_UR.set_powerwell("primary");
    INT_CMPL_UR.set_rand_mode(0);
   INT_CMPL_UR.set_reset_signame("pmu_rst_b");
    void'(add_field( INT_CMPL_UR ));

  endfunction


`DEFINE_NONE_SCOPE_REG_API(pmu_mmr_INT_CTRL_STS_reg) 
endclass : pmu_mmr_INT_CTRL_STS_reg

// ================================================

class pmu_mmr_IP_ST_DIS_MASK_0_reg extends sla_ral_reg;

  // --------------------------
  sla_ral_field IP_ST_DIS_IND_0_0;
  sla_ral_field IP_ST_DIS_IND_0_1;
  sla_ral_field IP_ST_DIS_IND_0_2;
  sla_ral_field IP_ST_DIS_IND_0_3;
  sla_ral_field IP_ST_DIS_IND_0_4;
  sla_ral_field IP_ST_DIS_IND_0_5;
  sla_ral_field IP_ST_DIS_IND_0_6;
  sla_ral_field IP_ST_DIS_IND_0_7;
  sla_ral_field IP_ST_DIS_IND_0_8;
  sla_ral_field IP_ST_DIS_IND_0_9;
  sla_ral_field IP_ST_DIS_IND_0_10;
  sla_ral_field IP_ST_DIS_IND_0_11;
  sla_ral_field IP_ST_DIS_IND_0_12;
  sla_ral_field IP_ST_DIS_IND_0_13;
  sla_ral_field IP_ST_DIS_IND_0_14;
  sla_ral_field IP_ST_DIS_IND_0_15;
  sla_ral_field IP_ST_DIS_IND_0_16;
  sla_ral_field IP_ST_DIS_IND_0_17;
  sla_ral_field IP_ST_DIS_IND_0_18;
  sla_ral_field IP_ST_DIS_IND_0_19;
  sla_ral_field IP_ST_DIS_IND_0_20;
  sla_ral_field IP_ST_DIS_IND_0_21;
  sla_ral_field IP_ST_DIS_IND_0_22;
  sla_ral_field IP_ST_DIS_IND_0_23;
  sla_ral_field IP_ST_DIS_IND_0_24;
  sla_ral_field IP_ST_DIS_IND_0_25;
  sla_ral_field IP_ST_DIS_IND_0_26;
  sla_ral_field IP_ST_DIS_IND_0_27;
  sla_ral_field IP_ST_DIS_IND_0_28;
  sla_ral_field IP_ST_DIS_IND_0_29;
  sla_ral_field IP_ST_DIS_IND_0_30;
  sla_ral_field IP_ST_DIS_IND_0_31;

  // --------------------------
  `ovm_object_utils(pmu_mmr_IP_ST_DIS_MASK_0_reg)

  // --------------------------
  function new(
                string         name = "",
                `ifdef SLA_RAL_COVERAGE
                sla_ral_coverage_t cov_t = COVERAGE_OFF,
                `endif
                int            bus_num=0,
                int            dev_num=0,
                int            func_num=0,
                int            offset=0,
                int            size=0,
                sla_ral_data_t reset_val=0,
                boolean_t      mon_enabled = SLA_FALSE
             );
      super.new();
      set_space_addr("CFG",undef);
      set_space_addr("MEM",undef);
      set_space_addr("IO",undef);
      set_space_addr("MSG",undef);
      set_space_addr("CREG",undef); 
      set_space_addr("LT_MEM",undef);    
 build();

      `ifdef SLA_RAL_COVERAGE
      `CONFIG_RAL_COVERAGE(regname, COVERAGE_ON, FD_RW_SAMPLE)
      if(_coverage_type != COVERAGE_OFF) begin
        if(_sample_type inside {FD_RO_SAMPLE, RO_SAMPLE})
        `CREATE_OP_SAMPLE_COV(RAL_WRITE)
        else if(_sample_type inside {FD_WO_SAMPLE, WO_SAMPLE})
        `CREATE_OP_SAMPLE_COV(RAL_READ)
        else
        `CREATE_OP_SAMPLE_COV(RAL_NONE)
        `CREATE_DESIRED_COV
        `CREATE_ACTUAL_COV
      end
      `endif

  endfunction

  `ifdef SLA_RAL_COVERAGE
  `CREATE_RAL_OP_COVERGROUP
  covergroup desired_cg @(cov_ev);
     option.per_instance = 1;
     option.name = {sla_ral_file::regfilename,".",regname};
     `RAL_FIELD_CP(IP_ST_DIS_IND_0_0, IP_ST_DIS_IND_0_0.desired)
     `RAL_FIELD_CP_1(IP_ST_DIS_IND_0_0, IP_ST_DIS_IND_0_0.desired, 0)
     `RAL_FIELD_CP(IP_ST_DIS_IND_0_1, IP_ST_DIS_IND_0_1.desired)
     `RAL_FIELD_CP_1(IP_ST_DIS_IND_0_1, IP_ST_DIS_IND_0_1.desired, 0)
     `RAL_FIELD_CP(IP_ST_DIS_IND_0_2, IP_ST_DIS_IND_0_2.desired)
     `RAL_FIELD_CP_1(IP_ST_DIS_IND_0_2, IP_ST_DIS_IND_0_2.desired, 0)
     `RAL_FIELD_CP(IP_ST_DIS_IND_0_3, IP_ST_DIS_IND_0_3.desired)
     `RAL_FIELD_CP_1(IP_ST_DIS_IND_0_3, IP_ST_DIS_IND_0_3.desired, 0)
     `RAL_FIELD_CP(IP_ST_DIS_IND_0_4, IP_ST_DIS_IND_0_4.desired)
     `RAL_FIELD_CP_1(IP_ST_DIS_IND_0_4, IP_ST_DIS_IND_0_4.desired, 0)
     `RAL_FIELD_CP(IP_ST_DIS_IND_0_5, IP_ST_DIS_IND_0_5.desired)
     `RAL_FIELD_CP_1(IP_ST_DIS_IND_0_5, IP_ST_DIS_IND_0_5.desired, 0)
     `RAL_FIELD_CP(IP_ST_DIS_IND_0_6, IP_ST_DIS_IND_0_6.desired)
     `RAL_FIELD_CP_1(IP_ST_DIS_IND_0_6, IP_ST_DIS_IND_0_6.desired, 0)
     `RAL_FIELD_CP(IP_ST_DIS_IND_0_7, IP_ST_DIS_IND_0_7.desired)
     `RAL_FIELD_CP_1(IP_ST_DIS_IND_0_7, IP_ST_DIS_IND_0_7.desired, 0)
     `RAL_FIELD_CP(IP_ST_DIS_IND_0_8, IP_ST_DIS_IND_0_8.desired)
     `RAL_FIELD_CP_1(IP_ST_DIS_IND_0_8, IP_ST_DIS_IND_0_8.desired, 0)
     `RAL_FIELD_CP(IP_ST_DIS_IND_0_9, IP_ST_DIS_IND_0_9.desired)
     `RAL_FIELD_CP_1(IP_ST_DIS_IND_0_9, IP_ST_DIS_IND_0_9.desired, 0)
     `RAL_FIELD_CP(IP_ST_DIS_IND_0_10, IP_ST_DIS_IND_0_10.desired)
     `RAL_FIELD_CP_1(IP_ST_DIS_IND_0_10, IP_ST_DIS_IND_0_10.desired, 0)
     `RAL_FIELD_CP(IP_ST_DIS_IND_0_11, IP_ST_DIS_IND_0_11.desired)
     `RAL_FIELD_CP_1(IP_ST_DIS_IND_0_11, IP_ST_DIS_IND_0_11.desired, 0)
     `RAL_FIELD_CP(IP_ST_DIS_IND_0_12, IP_ST_DIS_IND_0_12.desired)
     `RAL_FIELD_CP_1(IP_ST_DIS_IND_0_12, IP_ST_DIS_IND_0_12.desired, 0)
     `RAL_FIELD_CP(IP_ST_DIS_IND_0_13, IP_ST_DIS_IND_0_13.desired)
     `RAL_FIELD_CP_1(IP_ST_DIS_IND_0_13, IP_ST_DIS_IND_0_13.desired, 0)
     `RAL_FIELD_CP(IP_ST_DIS_IND_0_14, IP_ST_DIS_IND_0_14.desired)
     `RAL_FIELD_CP_1(IP_ST_DIS_IND_0_14, IP_ST_DIS_IND_0_14.desired, 0)
     `RAL_FIELD_CP(IP_ST_DIS_IND_0_15, IP_ST_DIS_IND_0_15.desired)
     `RAL_FIELD_CP_1(IP_ST_DIS_IND_0_15, IP_ST_DIS_IND_0_15.desired, 0)
     `RAL_FIELD_CP(IP_ST_DIS_IND_0_16, IP_ST_DIS_IND_0_16.desired)
     `RAL_FIELD_CP_1(IP_ST_DIS_IND_0_16, IP_ST_DIS_IND_0_16.desired, 0)
     `RAL_FIELD_CP(IP_ST_DIS_IND_0_17, IP_ST_DIS_IND_0_17.desired)
     `RAL_FIELD_CP_1(IP_ST_DIS_IND_0_17, IP_ST_DIS_IND_0_17.desired, 0)
     `RAL_FIELD_CP(IP_ST_DIS_IND_0_18, IP_ST_DIS_IND_0_18.desired)
     `RAL_FIELD_CP_1(IP_ST_DIS_IND_0_18, IP_ST_DIS_IND_0_18.desired, 0)
     `RAL_FIELD_CP(IP_ST_DIS_IND_0_19, IP_ST_DIS_IND_0_19.desired)
     `RAL_FIELD_CP_1(IP_ST_DIS_IND_0_19, IP_ST_DIS_IND_0_19.desired, 0)
     `RAL_FIELD_CP(IP_ST_DIS_IND_0_20, IP_ST_DIS_IND_0_20.desired)
     `RAL_FIELD_CP_1(IP_ST_DIS_IND_0_20, IP_ST_DIS_IND_0_20.desired, 0)
     `RAL_FIELD_CP(IP_ST_DIS_IND_0_21, IP_ST_DIS_IND_0_21.desired)
     `RAL_FIELD_CP_1(IP_ST_DIS_IND_0_21, IP_ST_DIS_IND_0_21.desired, 0)
     `RAL_FIELD_CP(IP_ST_DIS_IND_0_22, IP_ST_DIS_IND_0_22.desired)
     `RAL_FIELD_CP_1(IP_ST_DIS_IND_0_22, IP_ST_DIS_IND_0_22.desired, 0)
     `RAL_FIELD_CP(IP_ST_DIS_IND_0_23, IP_ST_DIS_IND_0_23.desired)
     `RAL_FIELD_CP_1(IP_ST_DIS_IND_0_23, IP_ST_DIS_IND_0_23.desired, 0)
     `RAL_FIELD_CP(IP_ST_DIS_IND_0_24, IP_ST_DIS_IND_0_24.desired)
     `RAL_FIELD_CP_1(IP_ST_DIS_IND_0_24, IP_ST_DIS_IND_0_24.desired, 0)
     `RAL_FIELD_CP(IP_ST_DIS_IND_0_25, IP_ST_DIS_IND_0_25.desired)
     `RAL_FIELD_CP_1(IP_ST_DIS_IND_0_25, IP_ST_DIS_IND_0_25.desired, 0)
     `RAL_FIELD_CP(IP_ST_DIS_IND_0_26, IP_ST_DIS_IND_0_26.desired)
     `RAL_FIELD_CP_1(IP_ST_DIS_IND_0_26, IP_ST_DIS_IND_0_26.desired, 0)
     `RAL_FIELD_CP(IP_ST_DIS_IND_0_27, IP_ST_DIS_IND_0_27.desired)
     `RAL_FIELD_CP_1(IP_ST_DIS_IND_0_27, IP_ST_DIS_IND_0_27.desired, 0)
     `RAL_FIELD_CP(IP_ST_DIS_IND_0_28, IP_ST_DIS_IND_0_28.desired)
     `RAL_FIELD_CP_1(IP_ST_DIS_IND_0_28, IP_ST_DIS_IND_0_28.desired, 0)
     `RAL_FIELD_CP(IP_ST_DIS_IND_0_29, IP_ST_DIS_IND_0_29.desired)
     `RAL_FIELD_CP_1(IP_ST_DIS_IND_0_29, IP_ST_DIS_IND_0_29.desired, 0)
     `RAL_FIELD_CP(IP_ST_DIS_IND_0_30, IP_ST_DIS_IND_0_30.desired)
     `RAL_FIELD_CP_1(IP_ST_DIS_IND_0_30, IP_ST_DIS_IND_0_30.desired, 0)
     `RAL_FIELD_CP(IP_ST_DIS_IND_0_31, IP_ST_DIS_IND_0_31.desired)
     `RAL_FIELD_CP_1(IP_ST_DIS_IND_0_31, IP_ST_DIS_IND_0_31.desired, 0)
  endgroup
  covergroup actual_cg @(cov_ev);
     option.per_instance = 1;
     option.name = {sla_ral_file::regfilename,".",regname};
     `RAL_FIELD_CP(IP_ST_DIS_IND_0_0, IP_ST_DIS_IND_0_0.actual)
     `RAL_FIELD_CP_1(IP_ST_DIS_IND_0_0, IP_ST_DIS_IND_0_0.actual, 0)
     `RAL_FIELD_CP(IP_ST_DIS_IND_0_1, IP_ST_DIS_IND_0_1.actual)
     `RAL_FIELD_CP_1(IP_ST_DIS_IND_0_1, IP_ST_DIS_IND_0_1.actual, 0)
     `RAL_FIELD_CP(IP_ST_DIS_IND_0_2, IP_ST_DIS_IND_0_2.actual)
     `RAL_FIELD_CP_1(IP_ST_DIS_IND_0_2, IP_ST_DIS_IND_0_2.actual, 0)
     `RAL_FIELD_CP(IP_ST_DIS_IND_0_3, IP_ST_DIS_IND_0_3.actual)
     `RAL_FIELD_CP_1(IP_ST_DIS_IND_0_3, IP_ST_DIS_IND_0_3.actual, 0)
     `RAL_FIELD_CP(IP_ST_DIS_IND_0_4, IP_ST_DIS_IND_0_4.actual)
     `RAL_FIELD_CP_1(IP_ST_DIS_IND_0_4, IP_ST_DIS_IND_0_4.actual, 0)
     `RAL_FIELD_CP(IP_ST_DIS_IND_0_5, IP_ST_DIS_IND_0_5.actual)
     `RAL_FIELD_CP_1(IP_ST_DIS_IND_0_5, IP_ST_DIS_IND_0_5.actual, 0)
     `RAL_FIELD_CP(IP_ST_DIS_IND_0_6, IP_ST_DIS_IND_0_6.actual)
     `RAL_FIELD_CP_1(IP_ST_DIS_IND_0_6, IP_ST_DIS_IND_0_6.actual, 0)
     `RAL_FIELD_CP(IP_ST_DIS_IND_0_7, IP_ST_DIS_IND_0_7.actual)
     `RAL_FIELD_CP_1(IP_ST_DIS_IND_0_7, IP_ST_DIS_IND_0_7.actual, 0)
     `RAL_FIELD_CP(IP_ST_DIS_IND_0_8, IP_ST_DIS_IND_0_8.actual)
     `RAL_FIELD_CP_1(IP_ST_DIS_IND_0_8, IP_ST_DIS_IND_0_8.actual, 0)
     `RAL_FIELD_CP(IP_ST_DIS_IND_0_9, IP_ST_DIS_IND_0_9.actual)
     `RAL_FIELD_CP_1(IP_ST_DIS_IND_0_9, IP_ST_DIS_IND_0_9.actual, 0)
     `RAL_FIELD_CP(IP_ST_DIS_IND_0_10, IP_ST_DIS_IND_0_10.actual)
     `RAL_FIELD_CP_1(IP_ST_DIS_IND_0_10, IP_ST_DIS_IND_0_10.actual, 0)
     `RAL_FIELD_CP(IP_ST_DIS_IND_0_11, IP_ST_DIS_IND_0_11.actual)
     `RAL_FIELD_CP_1(IP_ST_DIS_IND_0_11, IP_ST_DIS_IND_0_11.actual, 0)
     `RAL_FIELD_CP(IP_ST_DIS_IND_0_12, IP_ST_DIS_IND_0_12.actual)
     `RAL_FIELD_CP_1(IP_ST_DIS_IND_0_12, IP_ST_DIS_IND_0_12.actual, 0)
     `RAL_FIELD_CP(IP_ST_DIS_IND_0_13, IP_ST_DIS_IND_0_13.actual)
     `RAL_FIELD_CP_1(IP_ST_DIS_IND_0_13, IP_ST_DIS_IND_0_13.actual, 0)
     `RAL_FIELD_CP(IP_ST_DIS_IND_0_14, IP_ST_DIS_IND_0_14.actual)
     `RAL_FIELD_CP_1(IP_ST_DIS_IND_0_14, IP_ST_DIS_IND_0_14.actual, 0)
     `RAL_FIELD_CP(IP_ST_DIS_IND_0_15, IP_ST_DIS_IND_0_15.actual)
     `RAL_FIELD_CP_1(IP_ST_DIS_IND_0_15, IP_ST_DIS_IND_0_15.actual, 0)
     `RAL_FIELD_CP(IP_ST_DIS_IND_0_16, IP_ST_DIS_IND_0_16.actual)
     `RAL_FIELD_CP_1(IP_ST_DIS_IND_0_16, IP_ST_DIS_IND_0_16.actual, 0)
     `RAL_FIELD_CP(IP_ST_DIS_IND_0_17, IP_ST_DIS_IND_0_17.actual)
     `RAL_FIELD_CP_1(IP_ST_DIS_IND_0_17, IP_ST_DIS_IND_0_17.actual, 0)
     `RAL_FIELD_CP(IP_ST_DIS_IND_0_18, IP_ST_DIS_IND_0_18.actual)
     `RAL_FIELD_CP_1(IP_ST_DIS_IND_0_18, IP_ST_DIS_IND_0_18.actual, 0)
     `RAL_FIELD_CP(IP_ST_DIS_IND_0_19, IP_ST_DIS_IND_0_19.actual)
     `RAL_FIELD_CP_1(IP_ST_DIS_IND_0_19, IP_ST_DIS_IND_0_19.actual, 0)
     `RAL_FIELD_CP(IP_ST_DIS_IND_0_20, IP_ST_DIS_IND_0_20.actual)
     `RAL_FIELD_CP_1(IP_ST_DIS_IND_0_20, IP_ST_DIS_IND_0_20.actual, 0)
     `RAL_FIELD_CP(IP_ST_DIS_IND_0_21, IP_ST_DIS_IND_0_21.actual)
     `RAL_FIELD_CP_1(IP_ST_DIS_IND_0_21, IP_ST_DIS_IND_0_21.actual, 0)
     `RAL_FIELD_CP(IP_ST_DIS_IND_0_22, IP_ST_DIS_IND_0_22.actual)
     `RAL_FIELD_CP_1(IP_ST_DIS_IND_0_22, IP_ST_DIS_IND_0_22.actual, 0)
     `RAL_FIELD_CP(IP_ST_DIS_IND_0_23, IP_ST_DIS_IND_0_23.actual)
     `RAL_FIELD_CP_1(IP_ST_DIS_IND_0_23, IP_ST_DIS_IND_0_23.actual, 0)
     `RAL_FIELD_CP(IP_ST_DIS_IND_0_24, IP_ST_DIS_IND_0_24.actual)
     `RAL_FIELD_CP_1(IP_ST_DIS_IND_0_24, IP_ST_DIS_IND_0_24.actual, 0)
     `RAL_FIELD_CP(IP_ST_DIS_IND_0_25, IP_ST_DIS_IND_0_25.actual)
     `RAL_FIELD_CP_1(IP_ST_DIS_IND_0_25, IP_ST_DIS_IND_0_25.actual, 0)
     `RAL_FIELD_CP(IP_ST_DIS_IND_0_26, IP_ST_DIS_IND_0_26.actual)
     `RAL_FIELD_CP_1(IP_ST_DIS_IND_0_26, IP_ST_DIS_IND_0_26.actual, 0)
     `RAL_FIELD_CP(IP_ST_DIS_IND_0_27, IP_ST_DIS_IND_0_27.actual)
     `RAL_FIELD_CP_1(IP_ST_DIS_IND_0_27, IP_ST_DIS_IND_0_27.actual, 0)
     `RAL_FIELD_CP(IP_ST_DIS_IND_0_28, IP_ST_DIS_IND_0_28.actual)
     `RAL_FIELD_CP_1(IP_ST_DIS_IND_0_28, IP_ST_DIS_IND_0_28.actual, 0)
     `RAL_FIELD_CP(IP_ST_DIS_IND_0_29, IP_ST_DIS_IND_0_29.actual)
     `RAL_FIELD_CP_1(IP_ST_DIS_IND_0_29, IP_ST_DIS_IND_0_29.actual, 0)
     `RAL_FIELD_CP(IP_ST_DIS_IND_0_30, IP_ST_DIS_IND_0_30.actual)
     `RAL_FIELD_CP_1(IP_ST_DIS_IND_0_30, IP_ST_DIS_IND_0_30.actual, 0)
     `RAL_FIELD_CP(IP_ST_DIS_IND_0_31, IP_ST_DIS_IND_0_31.actual)
     `RAL_FIELD_CP_1(IP_ST_DIS_IND_0_31, IP_ST_DIS_IND_0_31.actual, 0)
  endgroup
  `endif

  // --------------------------
  // add constraint based on database legal values
  // --------------------------
  function void build();
    IP_ST_DIS_IND_0_0 = new("IP_ST_DIS_IND_0_0", "RO/V", 1, 0, {"IP_ST_DIS_MASK_0.IP_ST_DIS_IND_0_0"});
    IP_ST_DIS_IND_0_0.set_powerwell("primary");
    IP_ST_DIS_IND_0_0.set_rand_mode(0);
   IP_ST_DIS_IND_0_0.set_reset_signame("pmu_rst_b");
    void'(add_field( IP_ST_DIS_IND_0_0 ));

    IP_ST_DIS_IND_0_1 = new("IP_ST_DIS_IND_0_1", "RO/V", 1, 1, {"IP_ST_DIS_MASK_0.IP_ST_DIS_IND_0_1"});
    IP_ST_DIS_IND_0_1.set_powerwell("primary");
    IP_ST_DIS_IND_0_1.set_rand_mode(0);
   IP_ST_DIS_IND_0_1.set_reset_signame("pmu_rst_b");
    void'(add_field( IP_ST_DIS_IND_0_1 ));

    IP_ST_DIS_IND_0_2 = new("IP_ST_DIS_IND_0_2", "RO/V", 1, 2, {"IP_ST_DIS_MASK_0.IP_ST_DIS_IND_0_2"});
    IP_ST_DIS_IND_0_2.set_powerwell("primary");
    IP_ST_DIS_IND_0_2.set_rand_mode(0);
   IP_ST_DIS_IND_0_2.set_reset_signame("pmu_rst_b");
    void'(add_field( IP_ST_DIS_IND_0_2 ));

    IP_ST_DIS_IND_0_3 = new("IP_ST_DIS_IND_0_3", "RO/V", 1, 3, {"IP_ST_DIS_MASK_0.IP_ST_DIS_IND_0_3"});
    IP_ST_DIS_IND_0_3.set_powerwell("primary");
    IP_ST_DIS_IND_0_3.set_rand_mode(0);
   IP_ST_DIS_IND_0_3.set_reset_signame("pmu_rst_b");
    void'(add_field( IP_ST_DIS_IND_0_3 ));

    IP_ST_DIS_IND_0_4 = new("IP_ST_DIS_IND_0_4", "RO/V", 1, 4, {"IP_ST_DIS_MASK_0.IP_ST_DIS_IND_0_4"});
    IP_ST_DIS_IND_0_4.set_powerwell("primary");
    IP_ST_DIS_IND_0_4.set_rand_mode(0);
   IP_ST_DIS_IND_0_4.set_reset_signame("pmu_rst_b");
    void'(add_field( IP_ST_DIS_IND_0_4 ));

    IP_ST_DIS_IND_0_5 = new("IP_ST_DIS_IND_0_5", "RO/V", 1, 5, {"IP_ST_DIS_MASK_0.IP_ST_DIS_IND_0_5"});
    IP_ST_DIS_IND_0_5.set_powerwell("primary");
    IP_ST_DIS_IND_0_5.set_rand_mode(0);
   IP_ST_DIS_IND_0_5.set_reset_signame("pmu_rst_b");
    void'(add_field( IP_ST_DIS_IND_0_5 ));

    IP_ST_DIS_IND_0_6 = new("IP_ST_DIS_IND_0_6", "RO/V", 1, 6, {"IP_ST_DIS_MASK_0.IP_ST_DIS_IND_0_6"});
    IP_ST_DIS_IND_0_6.set_powerwell("primary");
    IP_ST_DIS_IND_0_6.set_rand_mode(0);
   IP_ST_DIS_IND_0_6.set_reset_signame("pmu_rst_b");
    void'(add_field( IP_ST_DIS_IND_0_6 ));

    IP_ST_DIS_IND_0_7 = new("IP_ST_DIS_IND_0_7", "RO/V", 1, 7, {"IP_ST_DIS_MASK_0.IP_ST_DIS_IND_0_7"});
    IP_ST_DIS_IND_0_7.set_powerwell("primary");
    IP_ST_DIS_IND_0_7.set_rand_mode(0);
   IP_ST_DIS_IND_0_7.set_reset_signame("pmu_rst_b");
    void'(add_field( IP_ST_DIS_IND_0_7 ));

    IP_ST_DIS_IND_0_8 = new("IP_ST_DIS_IND_0_8", "RO/V", 1, 8, {"IP_ST_DIS_MASK_0.IP_ST_DIS_IND_0_8"});
    IP_ST_DIS_IND_0_8.set_powerwell("primary");
    IP_ST_DIS_IND_0_8.set_rand_mode(0);
   IP_ST_DIS_IND_0_8.set_reset_signame("pmu_rst_b");
    void'(add_field( IP_ST_DIS_IND_0_8 ));

    IP_ST_DIS_IND_0_9 = new("IP_ST_DIS_IND_0_9", "RO/V", 1, 9, {"IP_ST_DIS_MASK_0.IP_ST_DIS_IND_0_9"});
    IP_ST_DIS_IND_0_9.set_powerwell("primary");
    IP_ST_DIS_IND_0_9.set_rand_mode(0);
   IP_ST_DIS_IND_0_9.set_reset_signame("pmu_rst_b");
    void'(add_field( IP_ST_DIS_IND_0_9 ));

    IP_ST_DIS_IND_0_10 = new("IP_ST_DIS_IND_0_10", "RO/V", 1, 10, {"IP_ST_DIS_MASK_0.IP_ST_DIS_IND_0_10"});
    IP_ST_DIS_IND_0_10.set_powerwell("primary");
    IP_ST_DIS_IND_0_10.set_rand_mode(0);
   IP_ST_DIS_IND_0_10.set_reset_signame("pmu_rst_b");
    void'(add_field( IP_ST_DIS_IND_0_10 ));

    IP_ST_DIS_IND_0_11 = new("IP_ST_DIS_IND_0_11", "RO/V", 1, 11, {"IP_ST_DIS_MASK_0.IP_ST_DIS_IND_0_11"});
    IP_ST_DIS_IND_0_11.set_powerwell("primary");
    IP_ST_DIS_IND_0_11.set_rand_mode(0);
   IP_ST_DIS_IND_0_11.set_reset_signame("pmu_rst_b");
    void'(add_field( IP_ST_DIS_IND_0_11 ));

    IP_ST_DIS_IND_0_12 = new("IP_ST_DIS_IND_0_12", "RO/V", 1, 12, {"IP_ST_DIS_MASK_0.IP_ST_DIS_IND_0_12"});
    IP_ST_DIS_IND_0_12.set_powerwell("primary");
    IP_ST_DIS_IND_0_12.set_rand_mode(0);
   IP_ST_DIS_IND_0_12.set_reset_signame("pmu_rst_b");
    void'(add_field( IP_ST_DIS_IND_0_12 ));

    IP_ST_DIS_IND_0_13 = new("IP_ST_DIS_IND_0_13", "RO/V", 1, 13, {"IP_ST_DIS_MASK_0.IP_ST_DIS_IND_0_13"});
    IP_ST_DIS_IND_0_13.set_powerwell("primary");
    IP_ST_DIS_IND_0_13.set_rand_mode(0);
   IP_ST_DIS_IND_0_13.set_reset_signame("pmu_rst_b");
    void'(add_field( IP_ST_DIS_IND_0_13 ));

    IP_ST_DIS_IND_0_14 = new("IP_ST_DIS_IND_0_14", "RO/V", 1, 14, {"IP_ST_DIS_MASK_0.IP_ST_DIS_IND_0_14"});
    IP_ST_DIS_IND_0_14.set_powerwell("primary");
    IP_ST_DIS_IND_0_14.set_rand_mode(0);
   IP_ST_DIS_IND_0_14.set_reset_signame("pmu_rst_b");
    void'(add_field( IP_ST_DIS_IND_0_14 ));

    IP_ST_DIS_IND_0_15 = new("IP_ST_DIS_IND_0_15", "RO/V", 1, 15, {"IP_ST_DIS_MASK_0.IP_ST_DIS_IND_0_15"});
    IP_ST_DIS_IND_0_15.set_powerwell("primary");
    IP_ST_DIS_IND_0_15.set_rand_mode(0);
   IP_ST_DIS_IND_0_15.set_reset_signame("pmu_rst_b");
    void'(add_field( IP_ST_DIS_IND_0_15 ));

    IP_ST_DIS_IND_0_16 = new("IP_ST_DIS_IND_0_16", "RO/V", 1, 16, {"IP_ST_DIS_MASK_0.IP_ST_DIS_IND_0_16"});
    IP_ST_DIS_IND_0_16.set_powerwell("primary");
    IP_ST_DIS_IND_0_16.set_rand_mode(0);
   IP_ST_DIS_IND_0_16.set_reset_signame("pmu_rst_b");
    void'(add_field( IP_ST_DIS_IND_0_16 ));

    IP_ST_DIS_IND_0_17 = new("IP_ST_DIS_IND_0_17", "RO/V", 1, 17, {"IP_ST_DIS_MASK_0.IP_ST_DIS_IND_0_17"});
    IP_ST_DIS_IND_0_17.set_powerwell("primary");
    IP_ST_DIS_IND_0_17.set_rand_mode(0);
   IP_ST_DIS_IND_0_17.set_reset_signame("pmu_rst_b");
    void'(add_field( IP_ST_DIS_IND_0_17 ));

    IP_ST_DIS_IND_0_18 = new("IP_ST_DIS_IND_0_18", "RO/V", 1, 18, {"IP_ST_DIS_MASK_0.IP_ST_DIS_IND_0_18"});
    IP_ST_DIS_IND_0_18.set_powerwell("primary");
    IP_ST_DIS_IND_0_18.set_rand_mode(0);
   IP_ST_DIS_IND_0_18.set_reset_signame("pmu_rst_b");
    void'(add_field( IP_ST_DIS_IND_0_18 ));

    IP_ST_DIS_IND_0_19 = new("IP_ST_DIS_IND_0_19", "RO/V", 1, 19, {"IP_ST_DIS_MASK_0.IP_ST_DIS_IND_0_19"});
    IP_ST_DIS_IND_0_19.set_powerwell("primary");
    IP_ST_DIS_IND_0_19.set_rand_mode(0);
   IP_ST_DIS_IND_0_19.set_reset_signame("pmu_rst_b");
    void'(add_field( IP_ST_DIS_IND_0_19 ));

    IP_ST_DIS_IND_0_20 = new("IP_ST_DIS_IND_0_20", "RO/V", 1, 20, {"IP_ST_DIS_MASK_0.IP_ST_DIS_IND_0_20"});
    IP_ST_DIS_IND_0_20.set_powerwell("primary");
    IP_ST_DIS_IND_0_20.set_rand_mode(0);
   IP_ST_DIS_IND_0_20.set_reset_signame("pmu_rst_b");
    void'(add_field( IP_ST_DIS_IND_0_20 ));

    IP_ST_DIS_IND_0_21 = new("IP_ST_DIS_IND_0_21", "RO/V", 1, 21, {"IP_ST_DIS_MASK_0.IP_ST_DIS_IND_0_21"});
    IP_ST_DIS_IND_0_21.set_powerwell("primary");
    IP_ST_DIS_IND_0_21.set_rand_mode(0);
   IP_ST_DIS_IND_0_21.set_reset_signame("pmu_rst_b");
    void'(add_field( IP_ST_DIS_IND_0_21 ));

    IP_ST_DIS_IND_0_22 = new("IP_ST_DIS_IND_0_22", "RO/V", 1, 22, {"IP_ST_DIS_MASK_0.IP_ST_DIS_IND_0_22"});
    IP_ST_DIS_IND_0_22.set_powerwell("primary");
    IP_ST_DIS_IND_0_22.set_rand_mode(0);
   IP_ST_DIS_IND_0_22.set_reset_signame("pmu_rst_b");
    void'(add_field( IP_ST_DIS_IND_0_22 ));

    IP_ST_DIS_IND_0_23 = new("IP_ST_DIS_IND_0_23", "RO/V", 1, 23, {"IP_ST_DIS_MASK_0.IP_ST_DIS_IND_0_23"});
    IP_ST_DIS_IND_0_23.set_powerwell("primary");
    IP_ST_DIS_IND_0_23.set_rand_mode(0);
   IP_ST_DIS_IND_0_23.set_reset_signame("pmu_rst_b");
    void'(add_field( IP_ST_DIS_IND_0_23 ));

    IP_ST_DIS_IND_0_24 = new("IP_ST_DIS_IND_0_24", "RO/V", 1, 24, {"IP_ST_DIS_MASK_0.IP_ST_DIS_IND_0_24"});
    IP_ST_DIS_IND_0_24.set_powerwell("primary");
    IP_ST_DIS_IND_0_24.set_rand_mode(0);
   IP_ST_DIS_IND_0_24.set_reset_signame("pmu_rst_b");
    void'(add_field( IP_ST_DIS_IND_0_24 ));

    IP_ST_DIS_IND_0_25 = new("IP_ST_DIS_IND_0_25", "RO/V", 1, 25, {"IP_ST_DIS_MASK_0.IP_ST_DIS_IND_0_25"});
    IP_ST_DIS_IND_0_25.set_powerwell("primary");
    IP_ST_DIS_IND_0_25.set_rand_mode(0);
   IP_ST_DIS_IND_0_25.set_reset_signame("pmu_rst_b");
    void'(add_field( IP_ST_DIS_IND_0_25 ));

    IP_ST_DIS_IND_0_26 = new("IP_ST_DIS_IND_0_26", "RO/V", 1, 26, {"IP_ST_DIS_MASK_0.IP_ST_DIS_IND_0_26"});
    IP_ST_DIS_IND_0_26.set_powerwell("primary");
    IP_ST_DIS_IND_0_26.set_rand_mode(0);
   IP_ST_DIS_IND_0_26.set_reset_signame("pmu_rst_b");
    void'(add_field( IP_ST_DIS_IND_0_26 ));

    IP_ST_DIS_IND_0_27 = new("IP_ST_DIS_IND_0_27", "RO/V", 1, 27, {"IP_ST_DIS_MASK_0.IP_ST_DIS_IND_0_27"});
    IP_ST_DIS_IND_0_27.set_powerwell("primary");
    IP_ST_DIS_IND_0_27.set_rand_mode(0);
   IP_ST_DIS_IND_0_27.set_reset_signame("pmu_rst_b");
    void'(add_field( IP_ST_DIS_IND_0_27 ));

    IP_ST_DIS_IND_0_28 = new("IP_ST_DIS_IND_0_28", "RO/V", 1, 28, {"IP_ST_DIS_MASK_0.IP_ST_DIS_IND_0_28"});
    IP_ST_DIS_IND_0_28.set_powerwell("primary");
    IP_ST_DIS_IND_0_28.set_rand_mode(0);
   IP_ST_DIS_IND_0_28.set_reset_signame("pmu_rst_b");
    void'(add_field( IP_ST_DIS_IND_0_28 ));

    IP_ST_DIS_IND_0_29 = new("IP_ST_DIS_IND_0_29", "RO/V", 1, 29, {"IP_ST_DIS_MASK_0.IP_ST_DIS_IND_0_29"});
    IP_ST_DIS_IND_0_29.set_powerwell("primary");
    IP_ST_DIS_IND_0_29.set_rand_mode(0);
   IP_ST_DIS_IND_0_29.set_reset_signame("pmu_rst_b");
    void'(add_field( IP_ST_DIS_IND_0_29 ));

    IP_ST_DIS_IND_0_30 = new("IP_ST_DIS_IND_0_30", "RO/V", 1, 30, {"IP_ST_DIS_MASK_0.IP_ST_DIS_IND_0_30"});
    IP_ST_DIS_IND_0_30.set_powerwell("primary");
    IP_ST_DIS_IND_0_30.set_rand_mode(0);
   IP_ST_DIS_IND_0_30.set_reset_signame("pmu_rst_b");
    void'(add_field( IP_ST_DIS_IND_0_30 ));

    IP_ST_DIS_IND_0_31 = new("IP_ST_DIS_IND_0_31", "RO/V", 1, 31, {"IP_ST_DIS_MASK_0.IP_ST_DIS_IND_0_31"});
    IP_ST_DIS_IND_0_31.set_powerwell("primary");
    IP_ST_DIS_IND_0_31.set_rand_mode(0);
   IP_ST_DIS_IND_0_31.set_reset_signame("pmu_rst_b");
    void'(add_field( IP_ST_DIS_IND_0_31 ));

  endfunction


`DEFINE_NONE_SCOPE_REG_API(pmu_mmr_IP_ST_DIS_MASK_0_reg) 
endclass : pmu_mmr_IP_ST_DIS_MASK_0_reg

// ================================================

class pmu_mmr_IP_ST_DIS_MASK_1_reg extends sla_ral_reg;

  // --------------------------
  sla_ral_field IP_ST_DIS_IND_1_0;
  sla_ral_field IP_ST_DIS_IND_1_1;
  sla_ral_field IP_ST_DIS_IND_1_2;
  sla_ral_field IP_ST_DIS_IND_1_3;
  sla_ral_field IP_ST_DIS_IND_1_4;
  sla_ral_field IP_ST_DIS_IND_1_5;
  sla_ral_field IP_ST_DIS_IND_1_6;
  sla_ral_field IP_ST_DIS_IND_1_7;
  sla_ral_field IP_ST_DIS_IND_1_8;
  sla_ral_field IP_ST_DIS_IND_1_9;
  sla_ral_field IP_ST_DIS_IND_1_10;
  sla_ral_field IP_ST_DIS_IND_1_11;
  sla_ral_field IP_ST_DIS_IND_1_12;
  sla_ral_field IP_ST_DIS_IND_1_13;
  sla_ral_field IP_ST_DIS_IND_1_14;
  sla_ral_field IP_ST_DIS_IND_1_15;
  sla_ral_field IP_ST_DIS_IND_1_16;
  sla_ral_field IP_ST_DIS_IND_1_17;
  sla_ral_field IP_ST_DIS_IND_1_18;
  sla_ral_field IP_ST_DIS_IND_1_19;
  sla_ral_field IP_ST_DIS_IND_1_20;
  sla_ral_field IP_ST_DIS_IND_1_21;
  sla_ral_field IP_ST_DIS_IND_1_22;
  sla_ral_field IP_ST_DIS_IND_1_23;
  sla_ral_field IP_ST_DIS_IND_1_24;
  sla_ral_field IP_ST_DIS_IND_1_25;
  sla_ral_field IP_ST_DIS_IND_1_26;
  sla_ral_field IP_ST_DIS_IND_1_27;
  sla_ral_field IP_ST_DIS_IND_1_28;
  sla_ral_field IP_ST_DIS_IND_1_29;
  sla_ral_field IP_ST_DIS_IND_1_30;
  sla_ral_field IP_ST_DIS_IND_1_31;

  // --------------------------
  `ovm_object_utils(pmu_mmr_IP_ST_DIS_MASK_1_reg)

  // --------------------------
  function new(
                string         name = "",
                `ifdef SLA_RAL_COVERAGE
                sla_ral_coverage_t cov_t = COVERAGE_OFF,
                `endif
                int            bus_num=0,
                int            dev_num=0,
                int            func_num=0,
                int            offset=0,
                int            size=0,
                sla_ral_data_t reset_val=0,
                boolean_t      mon_enabled = SLA_FALSE
             );
      super.new();
      set_space_addr("CFG",undef);
      set_space_addr("MEM",undef);
      set_space_addr("IO",undef);
      set_space_addr("MSG",undef);
      set_space_addr("CREG",undef); 
      set_space_addr("LT_MEM",undef);    
 build();

      `ifdef SLA_RAL_COVERAGE
      `CONFIG_RAL_COVERAGE(regname, COVERAGE_ON, FD_RW_SAMPLE)
      if(_coverage_type != COVERAGE_OFF) begin
        if(_sample_type inside {FD_RO_SAMPLE, RO_SAMPLE})
        `CREATE_OP_SAMPLE_COV(RAL_WRITE)
        else if(_sample_type inside {FD_WO_SAMPLE, WO_SAMPLE})
        `CREATE_OP_SAMPLE_COV(RAL_READ)
        else
        `CREATE_OP_SAMPLE_COV(RAL_NONE)
        `CREATE_DESIRED_COV
        `CREATE_ACTUAL_COV
      end
      `endif

  endfunction

  `ifdef SLA_RAL_COVERAGE
  `CREATE_RAL_OP_COVERGROUP
  covergroup desired_cg @(cov_ev);
     option.per_instance = 1;
     option.name = {sla_ral_file::regfilename,".",regname};
     `RAL_FIELD_CP(IP_ST_DIS_IND_1_0, IP_ST_DIS_IND_1_0.desired)
     `RAL_FIELD_CP_1(IP_ST_DIS_IND_1_0, IP_ST_DIS_IND_1_0.desired, 0)
     `RAL_FIELD_CP(IP_ST_DIS_IND_1_1, IP_ST_DIS_IND_1_1.desired)
     `RAL_FIELD_CP_1(IP_ST_DIS_IND_1_1, IP_ST_DIS_IND_1_1.desired, 0)
     `RAL_FIELD_CP(IP_ST_DIS_IND_1_2, IP_ST_DIS_IND_1_2.desired)
     `RAL_FIELD_CP_1(IP_ST_DIS_IND_1_2, IP_ST_DIS_IND_1_2.desired, 0)
     `RAL_FIELD_CP(IP_ST_DIS_IND_1_3, IP_ST_DIS_IND_1_3.desired)
     `RAL_FIELD_CP_1(IP_ST_DIS_IND_1_3, IP_ST_DIS_IND_1_3.desired, 0)
     `RAL_FIELD_CP(IP_ST_DIS_IND_1_4, IP_ST_DIS_IND_1_4.desired)
     `RAL_FIELD_CP_1(IP_ST_DIS_IND_1_4, IP_ST_DIS_IND_1_4.desired, 0)
     `RAL_FIELD_CP(IP_ST_DIS_IND_1_5, IP_ST_DIS_IND_1_5.desired)
     `RAL_FIELD_CP_1(IP_ST_DIS_IND_1_5, IP_ST_DIS_IND_1_5.desired, 0)
     `RAL_FIELD_CP(IP_ST_DIS_IND_1_6, IP_ST_DIS_IND_1_6.desired)
     `RAL_FIELD_CP_1(IP_ST_DIS_IND_1_6, IP_ST_DIS_IND_1_6.desired, 0)
     `RAL_FIELD_CP(IP_ST_DIS_IND_1_7, IP_ST_DIS_IND_1_7.desired)
     `RAL_FIELD_CP_1(IP_ST_DIS_IND_1_7, IP_ST_DIS_IND_1_7.desired, 0)
     `RAL_FIELD_CP(IP_ST_DIS_IND_1_8, IP_ST_DIS_IND_1_8.desired)
     `RAL_FIELD_CP_1(IP_ST_DIS_IND_1_8, IP_ST_DIS_IND_1_8.desired, 0)
     `RAL_FIELD_CP(IP_ST_DIS_IND_1_9, IP_ST_DIS_IND_1_9.desired)
     `RAL_FIELD_CP_1(IP_ST_DIS_IND_1_9, IP_ST_DIS_IND_1_9.desired, 0)
     `RAL_FIELD_CP(IP_ST_DIS_IND_1_10, IP_ST_DIS_IND_1_10.desired)
     `RAL_FIELD_CP_1(IP_ST_DIS_IND_1_10, IP_ST_DIS_IND_1_10.desired, 0)
     `RAL_FIELD_CP(IP_ST_DIS_IND_1_11, IP_ST_DIS_IND_1_11.desired)
     `RAL_FIELD_CP_1(IP_ST_DIS_IND_1_11, IP_ST_DIS_IND_1_11.desired, 0)
     `RAL_FIELD_CP(IP_ST_DIS_IND_1_12, IP_ST_DIS_IND_1_12.desired)
     `RAL_FIELD_CP_1(IP_ST_DIS_IND_1_12, IP_ST_DIS_IND_1_12.desired, 0)
     `RAL_FIELD_CP(IP_ST_DIS_IND_1_13, IP_ST_DIS_IND_1_13.desired)
     `RAL_FIELD_CP_1(IP_ST_DIS_IND_1_13, IP_ST_DIS_IND_1_13.desired, 0)
     `RAL_FIELD_CP(IP_ST_DIS_IND_1_14, IP_ST_DIS_IND_1_14.desired)
     `RAL_FIELD_CP_1(IP_ST_DIS_IND_1_14, IP_ST_DIS_IND_1_14.desired, 0)
     `RAL_FIELD_CP(IP_ST_DIS_IND_1_15, IP_ST_DIS_IND_1_15.desired)
     `RAL_FIELD_CP_1(IP_ST_DIS_IND_1_15, IP_ST_DIS_IND_1_15.desired, 0)
     `RAL_FIELD_CP(IP_ST_DIS_IND_1_16, IP_ST_DIS_IND_1_16.desired)
     `RAL_FIELD_CP_1(IP_ST_DIS_IND_1_16, IP_ST_DIS_IND_1_16.desired, 0)
     `RAL_FIELD_CP(IP_ST_DIS_IND_1_17, IP_ST_DIS_IND_1_17.desired)
     `RAL_FIELD_CP_1(IP_ST_DIS_IND_1_17, IP_ST_DIS_IND_1_17.desired, 0)
     `RAL_FIELD_CP(IP_ST_DIS_IND_1_18, IP_ST_DIS_IND_1_18.desired)
     `RAL_FIELD_CP_1(IP_ST_DIS_IND_1_18, IP_ST_DIS_IND_1_18.desired, 0)
     `RAL_FIELD_CP(IP_ST_DIS_IND_1_19, IP_ST_DIS_IND_1_19.desired)
     `RAL_FIELD_CP_1(IP_ST_DIS_IND_1_19, IP_ST_DIS_IND_1_19.desired, 0)
     `RAL_FIELD_CP(IP_ST_DIS_IND_1_20, IP_ST_DIS_IND_1_20.desired)
     `RAL_FIELD_CP_1(IP_ST_DIS_IND_1_20, IP_ST_DIS_IND_1_20.desired, 0)
     `RAL_FIELD_CP(IP_ST_DIS_IND_1_21, IP_ST_DIS_IND_1_21.desired)
     `RAL_FIELD_CP_1(IP_ST_DIS_IND_1_21, IP_ST_DIS_IND_1_21.desired, 0)
     `RAL_FIELD_CP(IP_ST_DIS_IND_1_22, IP_ST_DIS_IND_1_22.desired)
     `RAL_FIELD_CP_1(IP_ST_DIS_IND_1_22, IP_ST_DIS_IND_1_22.desired, 0)
     `RAL_FIELD_CP(IP_ST_DIS_IND_1_23, IP_ST_DIS_IND_1_23.desired)
     `RAL_FIELD_CP_1(IP_ST_DIS_IND_1_23, IP_ST_DIS_IND_1_23.desired, 0)
     `RAL_FIELD_CP(IP_ST_DIS_IND_1_24, IP_ST_DIS_IND_1_24.desired)
     `RAL_FIELD_CP_1(IP_ST_DIS_IND_1_24, IP_ST_DIS_IND_1_24.desired, 0)
     `RAL_FIELD_CP(IP_ST_DIS_IND_1_25, IP_ST_DIS_IND_1_25.desired)
     `RAL_FIELD_CP_1(IP_ST_DIS_IND_1_25, IP_ST_DIS_IND_1_25.desired, 0)
     `RAL_FIELD_CP(IP_ST_DIS_IND_1_26, IP_ST_DIS_IND_1_26.desired)
     `RAL_FIELD_CP_1(IP_ST_DIS_IND_1_26, IP_ST_DIS_IND_1_26.desired, 0)
     `RAL_FIELD_CP(IP_ST_DIS_IND_1_27, IP_ST_DIS_IND_1_27.desired)
     `RAL_FIELD_CP_1(IP_ST_DIS_IND_1_27, IP_ST_DIS_IND_1_27.desired, 0)
     `RAL_FIELD_CP(IP_ST_DIS_IND_1_28, IP_ST_DIS_IND_1_28.desired)
     `RAL_FIELD_CP_1(IP_ST_DIS_IND_1_28, IP_ST_DIS_IND_1_28.desired, 0)
     `RAL_FIELD_CP(IP_ST_DIS_IND_1_29, IP_ST_DIS_IND_1_29.desired)
     `RAL_FIELD_CP_1(IP_ST_DIS_IND_1_29, IP_ST_DIS_IND_1_29.desired, 0)
     `RAL_FIELD_CP(IP_ST_DIS_IND_1_30, IP_ST_DIS_IND_1_30.desired)
     `RAL_FIELD_CP_1(IP_ST_DIS_IND_1_30, IP_ST_DIS_IND_1_30.desired, 0)
     `RAL_FIELD_CP(IP_ST_DIS_IND_1_31, IP_ST_DIS_IND_1_31.desired)
     `RAL_FIELD_CP_1(IP_ST_DIS_IND_1_31, IP_ST_DIS_IND_1_31.desired, 0)
  endgroup
  covergroup actual_cg @(cov_ev);
     option.per_instance = 1;
     option.name = {sla_ral_file::regfilename,".",regname};
     `RAL_FIELD_CP(IP_ST_DIS_IND_1_0, IP_ST_DIS_IND_1_0.actual)
     `RAL_FIELD_CP_1(IP_ST_DIS_IND_1_0, IP_ST_DIS_IND_1_0.actual, 0)
     `RAL_FIELD_CP(IP_ST_DIS_IND_1_1, IP_ST_DIS_IND_1_1.actual)
     `RAL_FIELD_CP_1(IP_ST_DIS_IND_1_1, IP_ST_DIS_IND_1_1.actual, 0)
     `RAL_FIELD_CP(IP_ST_DIS_IND_1_2, IP_ST_DIS_IND_1_2.actual)
     `RAL_FIELD_CP_1(IP_ST_DIS_IND_1_2, IP_ST_DIS_IND_1_2.actual, 0)
     `RAL_FIELD_CP(IP_ST_DIS_IND_1_3, IP_ST_DIS_IND_1_3.actual)
     `RAL_FIELD_CP_1(IP_ST_DIS_IND_1_3, IP_ST_DIS_IND_1_3.actual, 0)
     `RAL_FIELD_CP(IP_ST_DIS_IND_1_4, IP_ST_DIS_IND_1_4.actual)
     `RAL_FIELD_CP_1(IP_ST_DIS_IND_1_4, IP_ST_DIS_IND_1_4.actual, 0)
     `RAL_FIELD_CP(IP_ST_DIS_IND_1_5, IP_ST_DIS_IND_1_5.actual)
     `RAL_FIELD_CP_1(IP_ST_DIS_IND_1_5, IP_ST_DIS_IND_1_5.actual, 0)
     `RAL_FIELD_CP(IP_ST_DIS_IND_1_6, IP_ST_DIS_IND_1_6.actual)
     `RAL_FIELD_CP_1(IP_ST_DIS_IND_1_6, IP_ST_DIS_IND_1_6.actual, 0)
     `RAL_FIELD_CP(IP_ST_DIS_IND_1_7, IP_ST_DIS_IND_1_7.actual)
     `RAL_FIELD_CP_1(IP_ST_DIS_IND_1_7, IP_ST_DIS_IND_1_7.actual, 0)
     `RAL_FIELD_CP(IP_ST_DIS_IND_1_8, IP_ST_DIS_IND_1_8.actual)
     `RAL_FIELD_CP_1(IP_ST_DIS_IND_1_8, IP_ST_DIS_IND_1_8.actual, 0)
     `RAL_FIELD_CP(IP_ST_DIS_IND_1_9, IP_ST_DIS_IND_1_9.actual)
     `RAL_FIELD_CP_1(IP_ST_DIS_IND_1_9, IP_ST_DIS_IND_1_9.actual, 0)
     `RAL_FIELD_CP(IP_ST_DIS_IND_1_10, IP_ST_DIS_IND_1_10.actual)
     `RAL_FIELD_CP_1(IP_ST_DIS_IND_1_10, IP_ST_DIS_IND_1_10.actual, 0)
     `RAL_FIELD_CP(IP_ST_DIS_IND_1_11, IP_ST_DIS_IND_1_11.actual)
     `RAL_FIELD_CP_1(IP_ST_DIS_IND_1_11, IP_ST_DIS_IND_1_11.actual, 0)
     `RAL_FIELD_CP(IP_ST_DIS_IND_1_12, IP_ST_DIS_IND_1_12.actual)
     `RAL_FIELD_CP_1(IP_ST_DIS_IND_1_12, IP_ST_DIS_IND_1_12.actual, 0)
     `RAL_FIELD_CP(IP_ST_DIS_IND_1_13, IP_ST_DIS_IND_1_13.actual)
     `RAL_FIELD_CP_1(IP_ST_DIS_IND_1_13, IP_ST_DIS_IND_1_13.actual, 0)
     `RAL_FIELD_CP(IP_ST_DIS_IND_1_14, IP_ST_DIS_IND_1_14.actual)
     `RAL_FIELD_CP_1(IP_ST_DIS_IND_1_14, IP_ST_DIS_IND_1_14.actual, 0)
     `RAL_FIELD_CP(IP_ST_DIS_IND_1_15, IP_ST_DIS_IND_1_15.actual)
     `RAL_FIELD_CP_1(IP_ST_DIS_IND_1_15, IP_ST_DIS_IND_1_15.actual, 0)
     `RAL_FIELD_CP(IP_ST_DIS_IND_1_16, IP_ST_DIS_IND_1_16.actual)
     `RAL_FIELD_CP_1(IP_ST_DIS_IND_1_16, IP_ST_DIS_IND_1_16.actual, 0)
     `RAL_FIELD_CP(IP_ST_DIS_IND_1_17, IP_ST_DIS_IND_1_17.actual)
     `RAL_FIELD_CP_1(IP_ST_DIS_IND_1_17, IP_ST_DIS_IND_1_17.actual, 0)
     `RAL_FIELD_CP(IP_ST_DIS_IND_1_18, IP_ST_DIS_IND_1_18.actual)
     `RAL_FIELD_CP_1(IP_ST_DIS_IND_1_18, IP_ST_DIS_IND_1_18.actual, 0)
     `RAL_FIELD_CP(IP_ST_DIS_IND_1_19, IP_ST_DIS_IND_1_19.actual)
     `RAL_FIELD_CP_1(IP_ST_DIS_IND_1_19, IP_ST_DIS_IND_1_19.actual, 0)
     `RAL_FIELD_CP(IP_ST_DIS_IND_1_20, IP_ST_DIS_IND_1_20.actual)
     `RAL_FIELD_CP_1(IP_ST_DIS_IND_1_20, IP_ST_DIS_IND_1_20.actual, 0)
     `RAL_FIELD_CP(IP_ST_DIS_IND_1_21, IP_ST_DIS_IND_1_21.actual)
     `RAL_FIELD_CP_1(IP_ST_DIS_IND_1_21, IP_ST_DIS_IND_1_21.actual, 0)
     `RAL_FIELD_CP(IP_ST_DIS_IND_1_22, IP_ST_DIS_IND_1_22.actual)
     `RAL_FIELD_CP_1(IP_ST_DIS_IND_1_22, IP_ST_DIS_IND_1_22.actual, 0)
     `RAL_FIELD_CP(IP_ST_DIS_IND_1_23, IP_ST_DIS_IND_1_23.actual)
     `RAL_FIELD_CP_1(IP_ST_DIS_IND_1_23, IP_ST_DIS_IND_1_23.actual, 0)
     `RAL_FIELD_CP(IP_ST_DIS_IND_1_24, IP_ST_DIS_IND_1_24.actual)
     `RAL_FIELD_CP_1(IP_ST_DIS_IND_1_24, IP_ST_DIS_IND_1_24.actual, 0)
     `RAL_FIELD_CP(IP_ST_DIS_IND_1_25, IP_ST_DIS_IND_1_25.actual)
     `RAL_FIELD_CP_1(IP_ST_DIS_IND_1_25, IP_ST_DIS_IND_1_25.actual, 0)
     `RAL_FIELD_CP(IP_ST_DIS_IND_1_26, IP_ST_DIS_IND_1_26.actual)
     `RAL_FIELD_CP_1(IP_ST_DIS_IND_1_26, IP_ST_DIS_IND_1_26.actual, 0)
     `RAL_FIELD_CP(IP_ST_DIS_IND_1_27, IP_ST_DIS_IND_1_27.actual)
     `RAL_FIELD_CP_1(IP_ST_DIS_IND_1_27, IP_ST_DIS_IND_1_27.actual, 0)
     `RAL_FIELD_CP(IP_ST_DIS_IND_1_28, IP_ST_DIS_IND_1_28.actual)
     `RAL_FIELD_CP_1(IP_ST_DIS_IND_1_28, IP_ST_DIS_IND_1_28.actual, 0)
     `RAL_FIELD_CP(IP_ST_DIS_IND_1_29, IP_ST_DIS_IND_1_29.actual)
     `RAL_FIELD_CP_1(IP_ST_DIS_IND_1_29, IP_ST_DIS_IND_1_29.actual, 0)
     `RAL_FIELD_CP(IP_ST_DIS_IND_1_30, IP_ST_DIS_IND_1_30.actual)
     `RAL_FIELD_CP_1(IP_ST_DIS_IND_1_30, IP_ST_DIS_IND_1_30.actual, 0)
     `RAL_FIELD_CP(IP_ST_DIS_IND_1_31, IP_ST_DIS_IND_1_31.actual)
     `RAL_FIELD_CP_1(IP_ST_DIS_IND_1_31, IP_ST_DIS_IND_1_31.actual, 0)
  endgroup
  `endif

  // --------------------------
  // add constraint based on database legal values
  // --------------------------
  function void build();
    IP_ST_DIS_IND_1_0 = new("IP_ST_DIS_IND_1_0", "RO/V", 1, 0, {"IP_ST_DIS_MASK_1.IP_ST_DIS_IND_1_0"});
    IP_ST_DIS_IND_1_0.set_powerwell("primary");
    IP_ST_DIS_IND_1_0.set_rand_mode(0);
   IP_ST_DIS_IND_1_0.set_reset_signame("pmu_rst_b");
    void'(add_field( IP_ST_DIS_IND_1_0 ));

    IP_ST_DIS_IND_1_1 = new("IP_ST_DIS_IND_1_1", "RO/V", 1, 1, {"IP_ST_DIS_MASK_1.IP_ST_DIS_IND_1_1"});
    IP_ST_DIS_IND_1_1.set_powerwell("primary");
    IP_ST_DIS_IND_1_1.set_rand_mode(0);
   IP_ST_DIS_IND_1_1.set_reset_signame("pmu_rst_b");
    void'(add_field( IP_ST_DIS_IND_1_1 ));

    IP_ST_DIS_IND_1_2 = new("IP_ST_DIS_IND_1_2", "RO/V", 1, 2, {"IP_ST_DIS_MASK_1.IP_ST_DIS_IND_1_2"});
    IP_ST_DIS_IND_1_2.set_powerwell("primary");
    IP_ST_DIS_IND_1_2.set_rand_mode(0);
   IP_ST_DIS_IND_1_2.set_reset_signame("pmu_rst_b");
    void'(add_field( IP_ST_DIS_IND_1_2 ));

    IP_ST_DIS_IND_1_3 = new("IP_ST_DIS_IND_1_3", "RO/V", 1, 3, {"IP_ST_DIS_MASK_1.IP_ST_DIS_IND_1_3"});
    IP_ST_DIS_IND_1_3.set_powerwell("primary");
    IP_ST_DIS_IND_1_3.set_rand_mode(0);
   IP_ST_DIS_IND_1_3.set_reset_signame("pmu_rst_b");
    void'(add_field( IP_ST_DIS_IND_1_3 ));

    IP_ST_DIS_IND_1_4 = new("IP_ST_DIS_IND_1_4", "RO/V", 1, 4, {"IP_ST_DIS_MASK_1.IP_ST_DIS_IND_1_4"});
    IP_ST_DIS_IND_1_4.set_powerwell("primary");
    IP_ST_DIS_IND_1_4.set_rand_mode(0);
   IP_ST_DIS_IND_1_4.set_reset_signame("pmu_rst_b");
    void'(add_field( IP_ST_DIS_IND_1_4 ));

    IP_ST_DIS_IND_1_5 = new("IP_ST_DIS_IND_1_5", "RO/V", 1, 5, {"IP_ST_DIS_MASK_1.IP_ST_DIS_IND_1_5"});
    IP_ST_DIS_IND_1_5.set_powerwell("primary");
    IP_ST_DIS_IND_1_5.set_rand_mode(0);
   IP_ST_DIS_IND_1_5.set_reset_signame("pmu_rst_b");
    void'(add_field( IP_ST_DIS_IND_1_5 ));

    IP_ST_DIS_IND_1_6 = new("IP_ST_DIS_IND_1_6", "RO/V", 1, 6, {"IP_ST_DIS_MASK_1.IP_ST_DIS_IND_1_6"});
    IP_ST_DIS_IND_1_6.set_powerwell("primary");
    IP_ST_DIS_IND_1_6.set_rand_mode(0);
   IP_ST_DIS_IND_1_6.set_reset_signame("pmu_rst_b");
    void'(add_field( IP_ST_DIS_IND_1_6 ));

    IP_ST_DIS_IND_1_7 = new("IP_ST_DIS_IND_1_7", "RO/V", 1, 7, {"IP_ST_DIS_MASK_1.IP_ST_DIS_IND_1_7"});
    IP_ST_DIS_IND_1_7.set_powerwell("primary");
    IP_ST_DIS_IND_1_7.set_rand_mode(0);
   IP_ST_DIS_IND_1_7.set_reset_signame("pmu_rst_b");
    void'(add_field( IP_ST_DIS_IND_1_7 ));

    IP_ST_DIS_IND_1_8 = new("IP_ST_DIS_IND_1_8", "RO/V", 1, 8, {"IP_ST_DIS_MASK_1.IP_ST_DIS_IND_1_8"});
    IP_ST_DIS_IND_1_8.set_powerwell("primary");
    IP_ST_DIS_IND_1_8.set_rand_mode(0);
   IP_ST_DIS_IND_1_8.set_reset_signame("pmu_rst_b");
    void'(add_field( IP_ST_DIS_IND_1_8 ));

    IP_ST_DIS_IND_1_9 = new("IP_ST_DIS_IND_1_9", "RO/V", 1, 9, {"IP_ST_DIS_MASK_1.IP_ST_DIS_IND_1_9"});
    IP_ST_DIS_IND_1_9.set_powerwell("primary");
    IP_ST_DIS_IND_1_9.set_rand_mode(0);
   IP_ST_DIS_IND_1_9.set_reset_signame("pmu_rst_b");
    void'(add_field( IP_ST_DIS_IND_1_9 ));

    IP_ST_DIS_IND_1_10 = new("IP_ST_DIS_IND_1_10", "RO/V", 1, 10, {"IP_ST_DIS_MASK_1.IP_ST_DIS_IND_1_10"});
    IP_ST_DIS_IND_1_10.set_powerwell("primary");
    IP_ST_DIS_IND_1_10.set_rand_mode(0);
   IP_ST_DIS_IND_1_10.set_reset_signame("pmu_rst_b");
    void'(add_field( IP_ST_DIS_IND_1_10 ));

    IP_ST_DIS_IND_1_11 = new("IP_ST_DIS_IND_1_11", "RO/V", 1, 11, {"IP_ST_DIS_MASK_1.IP_ST_DIS_IND_1_11"});
    IP_ST_DIS_IND_1_11.set_powerwell("primary");
    IP_ST_DIS_IND_1_11.set_rand_mode(0);
   IP_ST_DIS_IND_1_11.set_reset_signame("pmu_rst_b");
    void'(add_field( IP_ST_DIS_IND_1_11 ));

    IP_ST_DIS_IND_1_12 = new("IP_ST_DIS_IND_1_12", "RO/V", 1, 12, {"IP_ST_DIS_MASK_1.IP_ST_DIS_IND_1_12"});
    IP_ST_DIS_IND_1_12.set_powerwell("primary");
    IP_ST_DIS_IND_1_12.set_rand_mode(0);
   IP_ST_DIS_IND_1_12.set_reset_signame("pmu_rst_b");
    void'(add_field( IP_ST_DIS_IND_1_12 ));

    IP_ST_DIS_IND_1_13 = new("IP_ST_DIS_IND_1_13", "RO/V", 1, 13, {"IP_ST_DIS_MASK_1.IP_ST_DIS_IND_1_13"});
    IP_ST_DIS_IND_1_13.set_powerwell("primary");
    IP_ST_DIS_IND_1_13.set_rand_mode(0);
   IP_ST_DIS_IND_1_13.set_reset_signame("pmu_rst_b");
    void'(add_field( IP_ST_DIS_IND_1_13 ));

    IP_ST_DIS_IND_1_14 = new("IP_ST_DIS_IND_1_14", "RO/V", 1, 14, {"IP_ST_DIS_MASK_1.IP_ST_DIS_IND_1_14"});
    IP_ST_DIS_IND_1_14.set_powerwell("primary");
    IP_ST_DIS_IND_1_14.set_rand_mode(0);
   IP_ST_DIS_IND_1_14.set_reset_signame("pmu_rst_b");
    void'(add_field( IP_ST_DIS_IND_1_14 ));

    IP_ST_DIS_IND_1_15 = new("IP_ST_DIS_IND_1_15", "RO/V", 1, 15, {"IP_ST_DIS_MASK_1.IP_ST_DIS_IND_1_15"});
    IP_ST_DIS_IND_1_15.set_powerwell("primary");
    IP_ST_DIS_IND_1_15.set_rand_mode(0);
   IP_ST_DIS_IND_1_15.set_reset_signame("pmu_rst_b");
    void'(add_field( IP_ST_DIS_IND_1_15 ));

    IP_ST_DIS_IND_1_16 = new("IP_ST_DIS_IND_1_16", "RO/V", 1, 16, {"IP_ST_DIS_MASK_1.IP_ST_DIS_IND_1_16"});
    IP_ST_DIS_IND_1_16.set_powerwell("primary");
    IP_ST_DIS_IND_1_16.set_rand_mode(0);
   IP_ST_DIS_IND_1_16.set_reset_signame("pmu_rst_b");
    void'(add_field( IP_ST_DIS_IND_1_16 ));

    IP_ST_DIS_IND_1_17 = new("IP_ST_DIS_IND_1_17", "RO/V", 1, 17, {"IP_ST_DIS_MASK_1.IP_ST_DIS_IND_1_17"});
    IP_ST_DIS_IND_1_17.set_powerwell("primary");
    IP_ST_DIS_IND_1_17.set_rand_mode(0);
   IP_ST_DIS_IND_1_17.set_reset_signame("pmu_rst_b");
    void'(add_field( IP_ST_DIS_IND_1_17 ));

    IP_ST_DIS_IND_1_18 = new("IP_ST_DIS_IND_1_18", "RO/V", 1, 18, {"IP_ST_DIS_MASK_1.IP_ST_DIS_IND_1_18"});
    IP_ST_DIS_IND_1_18.set_powerwell("primary");
    IP_ST_DIS_IND_1_18.set_rand_mode(0);
   IP_ST_DIS_IND_1_18.set_reset_signame("pmu_rst_b");
    void'(add_field( IP_ST_DIS_IND_1_18 ));

    IP_ST_DIS_IND_1_19 = new("IP_ST_DIS_IND_1_19", "RO/V", 1, 19, {"IP_ST_DIS_MASK_1.IP_ST_DIS_IND_1_19"});
    IP_ST_DIS_IND_1_19.set_powerwell("primary");
    IP_ST_DIS_IND_1_19.set_rand_mode(0);
   IP_ST_DIS_IND_1_19.set_reset_signame("pmu_rst_b");
    void'(add_field( IP_ST_DIS_IND_1_19 ));

    IP_ST_DIS_IND_1_20 = new("IP_ST_DIS_IND_1_20", "RO/V", 1, 20, {"IP_ST_DIS_MASK_1.IP_ST_DIS_IND_1_20"});
    IP_ST_DIS_IND_1_20.set_powerwell("primary");
    IP_ST_DIS_IND_1_20.set_rand_mode(0);
   IP_ST_DIS_IND_1_20.set_reset_signame("pmu_rst_b");
    void'(add_field( IP_ST_DIS_IND_1_20 ));

    IP_ST_DIS_IND_1_21 = new("IP_ST_DIS_IND_1_21", "RO/V", 1, 21, {"IP_ST_DIS_MASK_1.IP_ST_DIS_IND_1_21"});
    IP_ST_DIS_IND_1_21.set_powerwell("primary");
    IP_ST_DIS_IND_1_21.set_rand_mode(0);
   IP_ST_DIS_IND_1_21.set_reset_signame("pmu_rst_b");
    void'(add_field( IP_ST_DIS_IND_1_21 ));

    IP_ST_DIS_IND_1_22 = new("IP_ST_DIS_IND_1_22", "RO/V", 1, 22, {"IP_ST_DIS_MASK_1.IP_ST_DIS_IND_1_22"});
    IP_ST_DIS_IND_1_22.set_powerwell("primary");
    IP_ST_DIS_IND_1_22.set_rand_mode(0);
   IP_ST_DIS_IND_1_22.set_reset_signame("pmu_rst_b");
    void'(add_field( IP_ST_DIS_IND_1_22 ));

    IP_ST_DIS_IND_1_23 = new("IP_ST_DIS_IND_1_23", "RO/V", 1, 23, {"IP_ST_DIS_MASK_1.IP_ST_DIS_IND_1_23"});
    IP_ST_DIS_IND_1_23.set_powerwell("primary");
    IP_ST_DIS_IND_1_23.set_rand_mode(0);
   IP_ST_DIS_IND_1_23.set_reset_signame("pmu_rst_b");
    void'(add_field( IP_ST_DIS_IND_1_23 ));

    IP_ST_DIS_IND_1_24 = new("IP_ST_DIS_IND_1_24", "RO/V", 1, 24, {"IP_ST_DIS_MASK_1.IP_ST_DIS_IND_1_24"});
    IP_ST_DIS_IND_1_24.set_powerwell("primary");
    IP_ST_DIS_IND_1_24.set_rand_mode(0);
   IP_ST_DIS_IND_1_24.set_reset_signame("pmu_rst_b");
    void'(add_field( IP_ST_DIS_IND_1_24 ));

    IP_ST_DIS_IND_1_25 = new("IP_ST_DIS_IND_1_25", "RO/V", 1, 25, {"IP_ST_DIS_MASK_1.IP_ST_DIS_IND_1_25"});
    IP_ST_DIS_IND_1_25.set_powerwell("primary");
    IP_ST_DIS_IND_1_25.set_rand_mode(0);
   IP_ST_DIS_IND_1_25.set_reset_signame("pmu_rst_b");
    void'(add_field( IP_ST_DIS_IND_1_25 ));

    IP_ST_DIS_IND_1_26 = new("IP_ST_DIS_IND_1_26", "RO/V", 1, 26, {"IP_ST_DIS_MASK_1.IP_ST_DIS_IND_1_26"});
    IP_ST_DIS_IND_1_26.set_powerwell("primary");
    IP_ST_DIS_IND_1_26.set_rand_mode(0);
   IP_ST_DIS_IND_1_26.set_reset_signame("pmu_rst_b");
    void'(add_field( IP_ST_DIS_IND_1_26 ));

    IP_ST_DIS_IND_1_27 = new("IP_ST_DIS_IND_1_27", "RO/V", 1, 27, {"IP_ST_DIS_MASK_1.IP_ST_DIS_IND_1_27"});
    IP_ST_DIS_IND_1_27.set_powerwell("primary");
    IP_ST_DIS_IND_1_27.set_rand_mode(0);
   IP_ST_DIS_IND_1_27.set_reset_signame("pmu_rst_b");
    void'(add_field( IP_ST_DIS_IND_1_27 ));

    IP_ST_DIS_IND_1_28 = new("IP_ST_DIS_IND_1_28", "RO/V", 1, 28, {"IP_ST_DIS_MASK_1.IP_ST_DIS_IND_1_28"});
    IP_ST_DIS_IND_1_28.set_powerwell("primary");
    IP_ST_DIS_IND_1_28.set_rand_mode(0);
   IP_ST_DIS_IND_1_28.set_reset_signame("pmu_rst_b");
    void'(add_field( IP_ST_DIS_IND_1_28 ));

    IP_ST_DIS_IND_1_29 = new("IP_ST_DIS_IND_1_29", "RO/V", 1, 29, {"IP_ST_DIS_MASK_1.IP_ST_DIS_IND_1_29"});
    IP_ST_DIS_IND_1_29.set_powerwell("primary");
    IP_ST_DIS_IND_1_29.set_rand_mode(0);
   IP_ST_DIS_IND_1_29.set_reset_signame("pmu_rst_b");
    void'(add_field( IP_ST_DIS_IND_1_29 ));

    IP_ST_DIS_IND_1_30 = new("IP_ST_DIS_IND_1_30", "RO/V", 1, 30, {"IP_ST_DIS_MASK_1.IP_ST_DIS_IND_1_30"});
    IP_ST_DIS_IND_1_30.set_powerwell("primary");
    IP_ST_DIS_IND_1_30.set_rand_mode(0);
   IP_ST_DIS_IND_1_30.set_reset_signame("pmu_rst_b");
    void'(add_field( IP_ST_DIS_IND_1_30 ));

    IP_ST_DIS_IND_1_31 = new("IP_ST_DIS_IND_1_31", "RO/V", 1, 31, {"IP_ST_DIS_MASK_1.IP_ST_DIS_IND_1_31"});
    IP_ST_DIS_IND_1_31.set_powerwell("primary");
    IP_ST_DIS_IND_1_31.set_rand_mode(0);
   IP_ST_DIS_IND_1_31.set_reset_signame("pmu_rst_b");
    void'(add_field( IP_ST_DIS_IND_1_31 ));

  endfunction


`DEFINE_NONE_SCOPE_REG_API(pmu_mmr_IP_ST_DIS_MASK_1_reg) 
endclass : pmu_mmr_IP_ST_DIS_MASK_1_reg

// ================================================

class pmu_mmr_IP_RST_EN_0_reg extends sla_ral_reg;

  // --------------------------
  sla_ral_field RST_EN_0;

  // --------------------------
  `ovm_object_utils(pmu_mmr_IP_RST_EN_0_reg)

  // --------------------------
  function new(
                string         name = "",
                `ifdef SLA_RAL_COVERAGE
                sla_ral_coverage_t cov_t = COVERAGE_OFF,
                `endif
                int            bus_num=0,
                int            dev_num=0,
                int            func_num=0,
                int            offset=0,
                int            size=0,
                sla_ral_data_t reset_val=0,
                boolean_t      mon_enabled = SLA_FALSE
             );
      super.new();
      set_space_addr("CFG",undef);
      set_space_addr("MEM",undef);
      set_space_addr("IO",undef);
      set_space_addr("MSG",undef);
      set_space_addr("CREG",undef); 
      set_space_addr("LT_MEM",undef);    
 build();

      `ifdef SLA_RAL_COVERAGE
      `CONFIG_RAL_COVERAGE(regname, COVERAGE_ON, FD_RW_SAMPLE)
      if(_coverage_type != COVERAGE_OFF) begin
        if(_sample_type inside {FD_RO_SAMPLE, RO_SAMPLE})
        `CREATE_OP_SAMPLE_COV(RAL_WRITE)
        else if(_sample_type inside {FD_WO_SAMPLE, WO_SAMPLE})
        `CREATE_OP_SAMPLE_COV(RAL_READ)
        else
        `CREATE_OP_SAMPLE_COV(RAL_NONE)
        `CREATE_DESIRED_COV
        `CREATE_ACTUAL_COV
      end
      `endif

  endfunction

  `ifdef SLA_RAL_COVERAGE
  `CREATE_RAL_OP_COVERGROUP
  covergroup desired_cg @(cov_ev);
     option.per_instance = 1;
     option.name = {sla_ral_file::regfilename,".",regname};
     `RAL_FIELD_CP(RST_EN_0, RST_EN_0.desired)
     `RAL_FIELD_CP_16(RST_EN_0, RST_EN_0.desired, 0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)
     `RAL_FIELD_CP_16(RST_EN_0, RST_EN_0.desired, 16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31)
  endgroup
  covergroup actual_cg @(cov_ev);
     option.per_instance = 1;
     option.name = {sla_ral_file::regfilename,".",regname};
     `RAL_FIELD_CP(RST_EN_0, RST_EN_0.actual)
     `RAL_FIELD_CP_16(RST_EN_0, RST_EN_0.actual, 0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)
     `RAL_FIELD_CP_16(RST_EN_0, RST_EN_0.actual, 16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31)
  endgroup
  `endif

  // --------------------------
  // add constraint based on database legal values
  // --------------------------
  function void build();
    RST_EN_0 = new("RST_EN_0", "RW", 32, 0, {"IP_RST_EN_0.RST_EN_0"});
    RST_EN_0.set_powerwell("primary");
    RST_EN_0.set_rand_mode(0);
   RST_EN_0.set_reset_signame("pmu_rst_b");
    void'(add_field( RST_EN_0 ));

  endfunction


`DEFINE_NONE_SCOPE_REG_API(pmu_mmr_IP_RST_EN_0_reg) 
endclass : pmu_mmr_IP_RST_EN_0_reg

// ================================================

class pmu_mmr_IP_RST_EN_1_reg extends sla_ral_reg;

  // --------------------------
  sla_ral_field RST_EN_1;

  // --------------------------
  `ovm_object_utils(pmu_mmr_IP_RST_EN_1_reg)

  // --------------------------
  function new(
                string         name = "",
                `ifdef SLA_RAL_COVERAGE
                sla_ral_coverage_t cov_t = COVERAGE_OFF,
                `endif
                int            bus_num=0,
                int            dev_num=0,
                int            func_num=0,
                int            offset=0,
                int            size=0,
                sla_ral_data_t reset_val=0,
                boolean_t      mon_enabled = SLA_FALSE
             );
      super.new();
      set_space_addr("CFG",undef);
      set_space_addr("MEM",undef);
      set_space_addr("IO",undef);
      set_space_addr("MSG",undef);
      set_space_addr("CREG",undef); 
      set_space_addr("LT_MEM",undef);    
 build();

      `ifdef SLA_RAL_COVERAGE
      `CONFIG_RAL_COVERAGE(regname, COVERAGE_ON, FD_RW_SAMPLE)
      if(_coverage_type != COVERAGE_OFF) begin
        if(_sample_type inside {FD_RO_SAMPLE, RO_SAMPLE})
        `CREATE_OP_SAMPLE_COV(RAL_WRITE)
        else if(_sample_type inside {FD_WO_SAMPLE, WO_SAMPLE})
        `CREATE_OP_SAMPLE_COV(RAL_READ)
        else
        `CREATE_OP_SAMPLE_COV(RAL_NONE)
        `CREATE_DESIRED_COV
        `CREATE_ACTUAL_COV
      end
      `endif

  endfunction

  `ifdef SLA_RAL_COVERAGE
  `CREATE_RAL_OP_COVERGROUP
  covergroup desired_cg @(cov_ev);
     option.per_instance = 1;
     option.name = {sla_ral_file::regfilename,".",regname};
     `RAL_FIELD_CP(RST_EN_1, RST_EN_1.desired)
     `RAL_FIELD_CP_16(RST_EN_1, RST_EN_1.desired, 0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)
     `RAL_FIELD_CP_16(RST_EN_1, RST_EN_1.desired, 16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31)
  endgroup
  covergroup actual_cg @(cov_ev);
     option.per_instance = 1;
     option.name = {sla_ral_file::regfilename,".",regname};
     `RAL_FIELD_CP(RST_EN_1, RST_EN_1.actual)
     `RAL_FIELD_CP_16(RST_EN_1, RST_EN_1.actual, 0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)
     `RAL_FIELD_CP_16(RST_EN_1, RST_EN_1.actual, 16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31)
  endgroup
  `endif

  // --------------------------
  // add constraint based on database legal values
  // --------------------------
  function void build();
    RST_EN_1 = new("RST_EN_1", "RW", 32, 0, {"IP_RST_EN_1.RST_EN_1"});
    RST_EN_1.set_powerwell("primary");
    RST_EN_1.set_rand_mode(0);
   RST_EN_1.set_reset_signame("pmu_rst_b");
    void'(add_field( RST_EN_1 ));

  endfunction


`DEFINE_NONE_SCOPE_REG_API(pmu_mmr_IP_RST_EN_1_reg) 
endclass : pmu_mmr_IP_RST_EN_1_reg

// ================================================

class pmu_mmr_RST_CTRL_reg extends sla_ral_reg;

  // --------------------------
  sla_ral_field UPDATE;
  sla_ral_field RST_VALUE;
  sla_ral_field RSVD_3_2;
  sla_ral_field RST_TYPE;
  sla_ral_field RSVD_31_7;

  // --------------------------
  `ovm_object_utils(pmu_mmr_RST_CTRL_reg)

  // --------------------------
  function new(
                string         name = "",
                `ifdef SLA_RAL_COVERAGE
                sla_ral_coverage_t cov_t = COVERAGE_OFF,
                `endif
                int            bus_num=0,
                int            dev_num=0,
                int            func_num=0,
                int            offset=0,
                int            size=0,
                sla_ral_data_t reset_val=0,
                boolean_t      mon_enabled = SLA_FALSE
             );
      super.new();
      set_space_addr("CFG",undef);
      set_space_addr("MEM",undef);
      set_space_addr("IO",undef);
      set_space_addr("MSG",undef);
      set_space_addr("CREG",undef); 
      set_space_addr("LT_MEM",undef);    
 build();

      `ifdef SLA_RAL_COVERAGE
      `CONFIG_RAL_COVERAGE(regname, COVERAGE_ON, FD_RW_SAMPLE)
      if(_coverage_type != COVERAGE_OFF) begin
        if(_sample_type inside {FD_RO_SAMPLE, RO_SAMPLE})
        `CREATE_OP_SAMPLE_COV(RAL_WRITE)
        else if(_sample_type inside {FD_WO_SAMPLE, WO_SAMPLE})
        `CREATE_OP_SAMPLE_COV(RAL_READ)
        else
        `CREATE_OP_SAMPLE_COV(RAL_NONE)
        `CREATE_DESIRED_COV
        `CREATE_ACTUAL_COV
      end
      `endif

  endfunction

  `ifdef SLA_RAL_COVERAGE
  `CREATE_RAL_OP_COVERGROUP
  covergroup desired_cg @(cov_ev);
     option.per_instance = 1;
     option.name = {sla_ral_file::regfilename,".",regname};
     `RAL_FIELD_CP(UPDATE, UPDATE.desired)
     `RAL_FIELD_CP_1(UPDATE, UPDATE.desired, 0)
     `RAL_FIELD_CP(RST_VALUE, RST_VALUE.desired)
     `RAL_FIELD_CP_1(RST_VALUE, RST_VALUE.desired, 0)
     `RAL_FIELD_CP(RSVD_3_2, RSVD_3_2.desired)
     `RAL_FIELD_CP_2(RSVD_3_2, RSVD_3_2.desired, 0,1)
     `RAL_FIELD_CP(RST_TYPE, RST_TYPE.desired)
     `RAL_FIELD_CP_3(RST_TYPE, RST_TYPE.desired, 0,1,2)
     `RAL_FIELD_CP(RSVD_31_7, RSVD_31_7.desired)
     `RAL_FIELD_CP_16(RSVD_31_7, RSVD_31_7.desired, 0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)
     `RAL_FIELD_CP_9(RSVD_31_7, RSVD_31_7.desired, 16,17,18,19,20,21,22,23,24)
  endgroup
  covergroup actual_cg @(cov_ev);
     option.per_instance = 1;
     option.name = {sla_ral_file::regfilename,".",regname};
     `RAL_FIELD_CP(UPDATE, UPDATE.actual)
     `RAL_FIELD_CP_1(UPDATE, UPDATE.actual, 0)
     `RAL_FIELD_CP(RST_VALUE, RST_VALUE.actual)
     `RAL_FIELD_CP_1(RST_VALUE, RST_VALUE.actual, 0)
     `RAL_FIELD_CP(RSVD_3_2, RSVD_3_2.actual)
     `RAL_FIELD_CP_2(RSVD_3_2, RSVD_3_2.actual, 0,1)
     `RAL_FIELD_CP(RST_TYPE, RST_TYPE.actual)
     `RAL_FIELD_CP_3(RST_TYPE, RST_TYPE.actual, 0,1,2)
     `RAL_FIELD_CP(RSVD_31_7, RSVD_31_7.actual)
     `RAL_FIELD_CP_16(RSVD_31_7, RSVD_31_7.actual, 0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)
     `RAL_FIELD_CP_9(RSVD_31_7, RSVD_31_7.actual, 16,17,18,19,20,21,22,23,24)
  endgroup
  `endif

  // --------------------------
  // add constraint based on database legal values
  // --------------------------
  function void build();
    UPDATE = new("UPDATE", "WO", 1, 0, {"RST_CTRL.UPDATE"});
    UPDATE.set_powerwell("primary");
    UPDATE.set_rand_mode(0);
   UPDATE.set_reset_signame("pmu_rst_b");
    void'(add_field( UPDATE ));

    RST_VALUE = new("RST_VALUE", "RW", 1, 1, {"RST_CTRL.RST_VALUE"});
    RST_VALUE.set_powerwell("primary");
    RST_VALUE.set_rand_mode(0);
   RST_VALUE.set_reset_signame("pmu_rst_b");
    void'(add_field( RST_VALUE ));

    RSVD_3_2 = new("RSVD_3_2", "RO", 2, 2, {"RST_CTRL.RSVD_3_2"});
    RSVD_3_2.set_powerwell("primary");
    RSVD_3_2.set_rand_mode(0);
    void'(add_field( RSVD_3_2 ));

    RST_TYPE = new("RST_TYPE", "RW", 3, 4, {"RST_CTRL.RST_TYPE"});
    RST_TYPE.set_powerwell("primary");
    RST_TYPE.set_rand_mode(0);
   RST_TYPE.set_reset_signame("pmu_rst_b");
    void'(add_field( RST_TYPE ));

    RSVD_31_7 = new("RSVD_31_7", "RO", 25, 7, {"RST_CTRL.RSVD_31_7"});
    RSVD_31_7.set_powerwell("primary");
    RSVD_31_7.set_rand_mode(0);
    void'(add_field( RSVD_31_7 ));

  endfunction


`DEFINE_NONE_SCOPE_REG_API(pmu_mmr_RST_CTRL_reg) 
endclass : pmu_mmr_RST_CTRL_reg

// ================================================

class pmu_mmr_IP_RST_STS_0_reg extends sla_ral_reg;

  // --------------------------
  sla_ral_field RST_STS;

  // --------------------------
  `ovm_object_utils(pmu_mmr_IP_RST_STS_0_reg)

  // --------------------------
  function new(
                string         name = "",
                `ifdef SLA_RAL_COVERAGE
                sla_ral_coverage_t cov_t = COVERAGE_OFF,
                `endif
                int            bus_num=0,
                int            dev_num=0,
                int            func_num=0,
                int            offset=0,
                int            size=0,
                sla_ral_data_t reset_val=0,
                boolean_t      mon_enabled = SLA_FALSE
             );
      super.new();
      set_space_addr("CFG",undef);
      set_space_addr("MEM",undef);
      set_space_addr("IO",undef);
      set_space_addr("MSG",undef);
      set_space_addr("CREG",undef); 
      set_space_addr("LT_MEM",undef);    
 build();

      `ifdef SLA_RAL_COVERAGE
      `CONFIG_RAL_COVERAGE(regname, COVERAGE_ON, FD_RW_SAMPLE)
      if(_coverage_type != COVERAGE_OFF) begin
        if(_sample_type inside {FD_RO_SAMPLE, RO_SAMPLE})
        `CREATE_OP_SAMPLE_COV(RAL_WRITE)
        else if(_sample_type inside {FD_WO_SAMPLE, WO_SAMPLE})
        `CREATE_OP_SAMPLE_COV(RAL_READ)
        else
        `CREATE_OP_SAMPLE_COV(RAL_NONE)
        `CREATE_DESIRED_COV
        `CREATE_ACTUAL_COV
      end
      `endif

  endfunction

  `ifdef SLA_RAL_COVERAGE
  `CREATE_RAL_OP_COVERGROUP
  covergroup desired_cg @(cov_ev);
     option.per_instance = 1;
     option.name = {sla_ral_file::regfilename,".",regname};
     `RAL_FIELD_CP(RST_STS, RST_STS.desired)
     `RAL_FIELD_CP_16(RST_STS, RST_STS.desired, 0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)
     `RAL_FIELD_CP_16(RST_STS, RST_STS.desired, 16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31)
  endgroup
  covergroup actual_cg @(cov_ev);
     option.per_instance = 1;
     option.name = {sla_ral_file::regfilename,".",regname};
     `RAL_FIELD_CP(RST_STS, RST_STS.actual)
     `RAL_FIELD_CP_16(RST_STS, RST_STS.actual, 0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)
     `RAL_FIELD_CP_16(RST_STS, RST_STS.actual, 16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31)
  endgroup
  `endif

  // --------------------------
  // add constraint based on database legal values
  // --------------------------
  function void build();
    RST_STS = new("RST_STS", "RO/V", 32, 0, {"IP_RST_STS_0.RST_STS"});
    RST_STS.set_powerwell("primary");
    RST_STS.set_rand_mode(0);
   RST_STS.set_reset_signame("pmu_rst_b");
    void'(add_field( RST_STS ));

  endfunction


`DEFINE_NONE_SCOPE_REG_API(pmu_mmr_IP_RST_STS_0_reg) 
endclass : pmu_mmr_IP_RST_STS_0_reg

// ================================================

class pmu_mmr_IP_RST_STS_1_reg extends sla_ral_reg;

  // --------------------------
  sla_ral_field RST_STS;

  // --------------------------
  `ovm_object_utils(pmu_mmr_IP_RST_STS_1_reg)

  // --------------------------
  function new(
                string         name = "",
                `ifdef SLA_RAL_COVERAGE
                sla_ral_coverage_t cov_t = COVERAGE_OFF,
                `endif
                int            bus_num=0,
                int            dev_num=0,
                int            func_num=0,
                int            offset=0,
                int            size=0,
                sla_ral_data_t reset_val=0,
                boolean_t      mon_enabled = SLA_FALSE
             );
      super.new();
      set_space_addr("CFG",undef);
      set_space_addr("MEM",undef);
      set_space_addr("IO",undef);
      set_space_addr("MSG",undef);
      set_space_addr("CREG",undef); 
      set_space_addr("LT_MEM",undef);    
 build();

      `ifdef SLA_RAL_COVERAGE
      `CONFIG_RAL_COVERAGE(regname, COVERAGE_ON, FD_RW_SAMPLE)
      if(_coverage_type != COVERAGE_OFF) begin
        if(_sample_type inside {FD_RO_SAMPLE, RO_SAMPLE})
        `CREATE_OP_SAMPLE_COV(RAL_WRITE)
        else if(_sample_type inside {FD_WO_SAMPLE, WO_SAMPLE})
        `CREATE_OP_SAMPLE_COV(RAL_READ)
        else
        `CREATE_OP_SAMPLE_COV(RAL_NONE)
        `CREATE_DESIRED_COV
        `CREATE_ACTUAL_COV
      end
      `endif

  endfunction

  `ifdef SLA_RAL_COVERAGE
  `CREATE_RAL_OP_COVERGROUP
  covergroup desired_cg @(cov_ev);
     option.per_instance = 1;
     option.name = {sla_ral_file::regfilename,".",regname};
     `RAL_FIELD_CP(RST_STS, RST_STS.desired)
     `RAL_FIELD_CP_16(RST_STS, RST_STS.desired, 0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)
     `RAL_FIELD_CP_16(RST_STS, RST_STS.desired, 16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31)
  endgroup
  covergroup actual_cg @(cov_ev);
     option.per_instance = 1;
     option.name = {sla_ral_file::regfilename,".",regname};
     `RAL_FIELD_CP(RST_STS, RST_STS.actual)
     `RAL_FIELD_CP_16(RST_STS, RST_STS.actual, 0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)
     `RAL_FIELD_CP_16(RST_STS, RST_STS.actual, 16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31)
  endgroup
  `endif

  // --------------------------
  // add constraint based on database legal values
  // --------------------------
  function void build();
    RST_STS = new("RST_STS", "RO/V", 32, 0, {"IP_RST_STS_1.RST_STS"});
    RST_STS.set_powerwell("primary");
    RST_STS.set_rand_mode(0);
   RST_STS.set_reset_signame("pmu_rst_b");
    void'(add_field( RST_STS ));

  endfunction


`DEFINE_NONE_SCOPE_REG_API(pmu_mmr_IP_RST_STS_1_reg) 
endclass : pmu_mmr_IP_RST_STS_1_reg

// ================================================

class pmu_mmr_PWRGD_CTRL_reg extends sla_ral_reg;

  // --------------------------
  sla_ral_field PMU_PHY_PWRGD_B_0;
  sla_ral_field PMU_PHY_PWRGD_B_1;
  sla_ral_field PMU_PHY_PWRGD_B_2;
  sla_ral_field PMU_PHY_PWRGD_B_3;
  sla_ral_field PWRGD_4;
  sla_ral_field PWRGD_5;
  sla_ral_field PWRGD_6;
  sla_ral_field PWRGD_7;
  sla_ral_field RSVD_31_8;

  // --------------------------
  `ovm_object_utils(pmu_mmr_PWRGD_CTRL_reg)

  // --------------------------
  function new(
                string         name = "",
                `ifdef SLA_RAL_COVERAGE
                sla_ral_coverage_t cov_t = COVERAGE_OFF,
                `endif
                int            bus_num=0,
                int            dev_num=0,
                int            func_num=0,
                int            offset=0,
                int            size=0,
                sla_ral_data_t reset_val=0,
                boolean_t      mon_enabled = SLA_FALSE
             );
      super.new();
      set_space_addr("CFG",undef);
      set_space_addr("MEM",undef);
      set_space_addr("IO",undef);
      set_space_addr("MSG",undef);
      set_space_addr("CREG",undef); 
      set_space_addr("LT_MEM",undef);    
 build();

      `ifdef SLA_RAL_COVERAGE
      `CONFIG_RAL_COVERAGE(regname, COVERAGE_ON, FD_RW_SAMPLE)
      if(_coverage_type != COVERAGE_OFF) begin
        if(_sample_type inside {FD_RO_SAMPLE, RO_SAMPLE})
        `CREATE_OP_SAMPLE_COV(RAL_WRITE)
        else if(_sample_type inside {FD_WO_SAMPLE, WO_SAMPLE})
        `CREATE_OP_SAMPLE_COV(RAL_READ)
        else
        `CREATE_OP_SAMPLE_COV(RAL_NONE)
        `CREATE_DESIRED_COV
        `CREATE_ACTUAL_COV
      end
      `endif

  endfunction

  `ifdef SLA_RAL_COVERAGE
  `CREATE_RAL_OP_COVERGROUP
  covergroup desired_cg @(cov_ev);
     option.per_instance = 1;
     option.name = {sla_ral_file::regfilename,".",regname};
     `RAL_FIELD_CP(PMU_PHY_PWRGD_B_0, PMU_PHY_PWRGD_B_0.desired)
     `RAL_FIELD_CP_1(PMU_PHY_PWRGD_B_0, PMU_PHY_PWRGD_B_0.desired, 0)
     `RAL_FIELD_CP(PMU_PHY_PWRGD_B_1, PMU_PHY_PWRGD_B_1.desired)
     `RAL_FIELD_CP_1(PMU_PHY_PWRGD_B_1, PMU_PHY_PWRGD_B_1.desired, 0)
     `RAL_FIELD_CP(PMU_PHY_PWRGD_B_2, PMU_PHY_PWRGD_B_2.desired)
     `RAL_FIELD_CP_1(PMU_PHY_PWRGD_B_2, PMU_PHY_PWRGD_B_2.desired, 0)
     `RAL_FIELD_CP(PMU_PHY_PWRGD_B_3, PMU_PHY_PWRGD_B_3.desired)
     `RAL_FIELD_CP_1(PMU_PHY_PWRGD_B_3, PMU_PHY_PWRGD_B_3.desired, 0)
     `RAL_FIELD_CP(PWRGD_4, PWRGD_4.desired)
     `RAL_FIELD_CP_1(PWRGD_4, PWRGD_4.desired, 0)
     `RAL_FIELD_CP(PWRGD_5, PWRGD_5.desired)
     `RAL_FIELD_CP_1(PWRGD_5, PWRGD_5.desired, 0)
     `RAL_FIELD_CP(PWRGD_6, PWRGD_6.desired)
     `RAL_FIELD_CP_1(PWRGD_6, PWRGD_6.desired, 0)
     `RAL_FIELD_CP(PWRGD_7, PWRGD_7.desired)
     `RAL_FIELD_CP_1(PWRGD_7, PWRGD_7.desired, 0)
     `RAL_FIELD_CP(RSVD_31_8, RSVD_31_8.desired)
     `RAL_FIELD_CP_16(RSVD_31_8, RSVD_31_8.desired, 0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)
     `RAL_FIELD_CP_8(RSVD_31_8, RSVD_31_8.desired, 16,17,18,19,20,21,22,23)
  endgroup
  covergroup actual_cg @(cov_ev);
     option.per_instance = 1;
     option.name = {sla_ral_file::regfilename,".",regname};
     `RAL_FIELD_CP(PMU_PHY_PWRGD_B_0, PMU_PHY_PWRGD_B_0.actual)
     `RAL_FIELD_CP_1(PMU_PHY_PWRGD_B_0, PMU_PHY_PWRGD_B_0.actual, 0)
     `RAL_FIELD_CP(PMU_PHY_PWRGD_B_1, PMU_PHY_PWRGD_B_1.actual)
     `RAL_FIELD_CP_1(PMU_PHY_PWRGD_B_1, PMU_PHY_PWRGD_B_1.actual, 0)
     `RAL_FIELD_CP(PMU_PHY_PWRGD_B_2, PMU_PHY_PWRGD_B_2.actual)
     `RAL_FIELD_CP_1(PMU_PHY_PWRGD_B_2, PMU_PHY_PWRGD_B_2.actual, 0)
     `RAL_FIELD_CP(PMU_PHY_PWRGD_B_3, PMU_PHY_PWRGD_B_3.actual)
     `RAL_FIELD_CP_1(PMU_PHY_PWRGD_B_3, PMU_PHY_PWRGD_B_3.actual, 0)
     `RAL_FIELD_CP(PWRGD_4, PWRGD_4.actual)
     `RAL_FIELD_CP_1(PWRGD_4, PWRGD_4.actual, 0)
     `RAL_FIELD_CP(PWRGD_5, PWRGD_5.actual)
     `RAL_FIELD_CP_1(PWRGD_5, PWRGD_5.actual, 0)
     `RAL_FIELD_CP(PWRGD_6, PWRGD_6.actual)
     `RAL_FIELD_CP_1(PWRGD_6, PWRGD_6.actual, 0)
     `RAL_FIELD_CP(PWRGD_7, PWRGD_7.actual)
     `RAL_FIELD_CP_1(PWRGD_7, PWRGD_7.actual, 0)
     `RAL_FIELD_CP(RSVD_31_8, RSVD_31_8.actual)
     `RAL_FIELD_CP_16(RSVD_31_8, RSVD_31_8.actual, 0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)
     `RAL_FIELD_CP_8(RSVD_31_8, RSVD_31_8.actual, 16,17,18,19,20,21,22,23)
  endgroup
  `endif

  // --------------------------
  // add constraint based on database legal values
  // --------------------------
  function void build();
    PMU_PHY_PWRGD_B_0 = new("PMU_PHY_PWRGD_B_0", "RW/V", 1, 0, {"PWRGD_CTRL.PMU_PHY_PWRGD_B_0"});
    PMU_PHY_PWRGD_B_0.set_powerwell("primary");
    PMU_PHY_PWRGD_B_0.set_rand_mode(0);
   PMU_PHY_PWRGD_B_0.set_reset_signame("pmu_rst_b");
    void'(add_field( PMU_PHY_PWRGD_B_0 ));

    PMU_PHY_PWRGD_B_1 = new("PMU_PHY_PWRGD_B_1", "RW/V", 1, 1, {"PWRGD_CTRL.PMU_PHY_PWRGD_B_1"});
    PMU_PHY_PWRGD_B_1.set_powerwell("primary");
    PMU_PHY_PWRGD_B_1.set_rand_mode(0);
   PMU_PHY_PWRGD_B_1.set_reset_signame("pmu_rst_b");
    void'(add_field( PMU_PHY_PWRGD_B_1 ));

    PMU_PHY_PWRGD_B_2 = new("PMU_PHY_PWRGD_B_2", "RW/V", 1, 2, {"PWRGD_CTRL.PMU_PHY_PWRGD_B_2"});
    PMU_PHY_PWRGD_B_2.set_powerwell("primary");
    PMU_PHY_PWRGD_B_2.set_rand_mode(0);
   PMU_PHY_PWRGD_B_2.set_reset_signame("pmu_rst_b");
    void'(add_field( PMU_PHY_PWRGD_B_2 ));

    PMU_PHY_PWRGD_B_3 = new("PMU_PHY_PWRGD_B_3", "RW/V", 1, 3, {"PWRGD_CTRL.PMU_PHY_PWRGD_B_3"});
    PMU_PHY_PWRGD_B_3.set_powerwell("primary");
    PMU_PHY_PWRGD_B_3.set_rand_mode(0);
   PMU_PHY_PWRGD_B_3.set_reset_signame("pmu_rst_b");
    void'(add_field( PMU_PHY_PWRGD_B_3 ));

    PWRGD_4 = new("PWRGD_4", "RW/V", 1, 4, {"PWRGD_CTRL.PWRGD_4"});
    PWRGD_4.set_powerwell("primary");
    PWRGD_4.set_rand_mode(0);
   PWRGD_4.set_reset_signame("pmu_rst_b");
    void'(add_field( PWRGD_4 ));

    PWRGD_5 = new("PWRGD_5", "RW/V", 1, 5, {"PWRGD_CTRL.PWRGD_5"});
    PWRGD_5.set_powerwell("primary");
    PWRGD_5.set_rand_mode(0);
   PWRGD_5.set_reset_signame("pmu_rst_b");
    void'(add_field( PWRGD_5 ));

    PWRGD_6 = new("PWRGD_6", "RW/V", 1, 6, {"PWRGD_CTRL.PWRGD_6"});
    PWRGD_6.set_powerwell("primary");
    PWRGD_6.set_rand_mode(0);
   PWRGD_6.set_reset_signame("pmu_rst_b");
    void'(add_field( PWRGD_6 ));

    PWRGD_7 = new("PWRGD_7", "RW/V", 1, 7, {"PWRGD_CTRL.PWRGD_7"});
    PWRGD_7.set_powerwell("primary");
    PWRGD_7.set_rand_mode(0);
   PWRGD_7.set_reset_signame("pmu_rst_b");
    void'(add_field( PWRGD_7 ));

    RSVD_31_8 = new("RSVD_31_8", "RO", 24, 8, {"PWRGD_CTRL.RSVD_31_8"});
    RSVD_31_8.set_powerwell("primary");
    RSVD_31_8.set_rand_mode(0);
    void'(add_field( RSVD_31_8 ));

  endfunction


`DEFINE_NONE_SCOPE_REG_API(pmu_mmr_PWRGD_CTRL_reg) 
endclass : pmu_mmr_PWRGD_CTRL_reg

// ================================================

class pmu_mmr_PWRGD_STS_reg extends sla_ral_reg;

  // --------------------------
  sla_ral_field PHY_PMU_PWR_STS_0;
  sla_ral_field PHY_PMU_PWR_STS_1;
  sla_ral_field PHY_PMU_PWR_STS_2;
  sla_ral_field PHY_PMU_PWR_STS_3;
  sla_ral_field PWRSTS_4;
  sla_ral_field PWRSTS_5;
  sla_ral_field PWRSTS_6;
  sla_ral_field PWRSTS_7;
  sla_ral_field RSVD_31_8;

  // --------------------------
  `ovm_object_utils(pmu_mmr_PWRGD_STS_reg)

  // --------------------------
  function new(
                string         name = "",
                `ifdef SLA_RAL_COVERAGE
                sla_ral_coverage_t cov_t = COVERAGE_OFF,
                `endif
                int            bus_num=0,
                int            dev_num=0,
                int            func_num=0,
                int            offset=0,
                int            size=0,
                sla_ral_data_t reset_val=0,
                boolean_t      mon_enabled = SLA_FALSE
             );
      super.new();
      set_space_addr("CFG",undef);
      set_space_addr("MEM",undef);
      set_space_addr("IO",undef);
      set_space_addr("MSG",undef);
      set_space_addr("CREG",undef); 
      set_space_addr("LT_MEM",undef);    
 build();

      `ifdef SLA_RAL_COVERAGE
      `CONFIG_RAL_COVERAGE(regname, COVERAGE_ON, FD_RW_SAMPLE)
      if(_coverage_type != COVERAGE_OFF) begin
        if(_sample_type inside {FD_RO_SAMPLE, RO_SAMPLE})
        `CREATE_OP_SAMPLE_COV(RAL_WRITE)
        else if(_sample_type inside {FD_WO_SAMPLE, WO_SAMPLE})
        `CREATE_OP_SAMPLE_COV(RAL_READ)
        else
        `CREATE_OP_SAMPLE_COV(RAL_NONE)
        `CREATE_DESIRED_COV
        `CREATE_ACTUAL_COV
      end
      `endif

  endfunction

  `ifdef SLA_RAL_COVERAGE
  `CREATE_RAL_OP_COVERGROUP
  covergroup desired_cg @(cov_ev);
     option.per_instance = 1;
     option.name = {sla_ral_file::regfilename,".",regname};
     `RAL_FIELD_CP(PHY_PMU_PWR_STS_0, PHY_PMU_PWR_STS_0.desired)
     `RAL_FIELD_CP_1(PHY_PMU_PWR_STS_0, PHY_PMU_PWR_STS_0.desired, 0)
     `RAL_FIELD_CP(PHY_PMU_PWR_STS_1, PHY_PMU_PWR_STS_1.desired)
     `RAL_FIELD_CP_1(PHY_PMU_PWR_STS_1, PHY_PMU_PWR_STS_1.desired, 0)
     `RAL_FIELD_CP(PHY_PMU_PWR_STS_2, PHY_PMU_PWR_STS_2.desired)
     `RAL_FIELD_CP_1(PHY_PMU_PWR_STS_2, PHY_PMU_PWR_STS_2.desired, 0)
     `RAL_FIELD_CP(PHY_PMU_PWR_STS_3, PHY_PMU_PWR_STS_3.desired)
     `RAL_FIELD_CP_1(PHY_PMU_PWR_STS_3, PHY_PMU_PWR_STS_3.desired, 0)
     `RAL_FIELD_CP(PWRSTS_4, PWRSTS_4.desired)
     `RAL_FIELD_CP_1(PWRSTS_4, PWRSTS_4.desired, 0)
     `RAL_FIELD_CP(PWRSTS_5, PWRSTS_5.desired)
     `RAL_FIELD_CP_1(PWRSTS_5, PWRSTS_5.desired, 0)
     `RAL_FIELD_CP(PWRSTS_6, PWRSTS_6.desired)
     `RAL_FIELD_CP_1(PWRSTS_6, PWRSTS_6.desired, 0)
     `RAL_FIELD_CP(PWRSTS_7, PWRSTS_7.desired)
     `RAL_FIELD_CP_1(PWRSTS_7, PWRSTS_7.desired, 0)
     `RAL_FIELD_CP(RSVD_31_8, RSVD_31_8.desired)
     `RAL_FIELD_CP_16(RSVD_31_8, RSVD_31_8.desired, 0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)
     `RAL_FIELD_CP_8(RSVD_31_8, RSVD_31_8.desired, 16,17,18,19,20,21,22,23)
  endgroup
  covergroup actual_cg @(cov_ev);
     option.per_instance = 1;
     option.name = {sla_ral_file::regfilename,".",regname};
     `RAL_FIELD_CP(PHY_PMU_PWR_STS_0, PHY_PMU_PWR_STS_0.actual)
     `RAL_FIELD_CP_1(PHY_PMU_PWR_STS_0, PHY_PMU_PWR_STS_0.actual, 0)
     `RAL_FIELD_CP(PHY_PMU_PWR_STS_1, PHY_PMU_PWR_STS_1.actual)
     `RAL_FIELD_CP_1(PHY_PMU_PWR_STS_1, PHY_PMU_PWR_STS_1.actual, 0)
     `RAL_FIELD_CP(PHY_PMU_PWR_STS_2, PHY_PMU_PWR_STS_2.actual)
     `RAL_FIELD_CP_1(PHY_PMU_PWR_STS_2, PHY_PMU_PWR_STS_2.actual, 0)
     `RAL_FIELD_CP(PHY_PMU_PWR_STS_3, PHY_PMU_PWR_STS_3.actual)
     `RAL_FIELD_CP_1(PHY_PMU_PWR_STS_3, PHY_PMU_PWR_STS_3.actual, 0)
     `RAL_FIELD_CP(PWRSTS_4, PWRSTS_4.actual)
     `RAL_FIELD_CP_1(PWRSTS_4, PWRSTS_4.actual, 0)
     `RAL_FIELD_CP(PWRSTS_5, PWRSTS_5.actual)
     `RAL_FIELD_CP_1(PWRSTS_5, PWRSTS_5.actual, 0)
     `RAL_FIELD_CP(PWRSTS_6, PWRSTS_6.actual)
     `RAL_FIELD_CP_1(PWRSTS_6, PWRSTS_6.actual, 0)
     `RAL_FIELD_CP(PWRSTS_7, PWRSTS_7.actual)
     `RAL_FIELD_CP_1(PWRSTS_7, PWRSTS_7.actual, 0)
     `RAL_FIELD_CP(RSVD_31_8, RSVD_31_8.actual)
     `RAL_FIELD_CP_16(RSVD_31_8, RSVD_31_8.actual, 0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)
     `RAL_FIELD_CP_8(RSVD_31_8, RSVD_31_8.actual, 16,17,18,19,20,21,22,23)
  endgroup
  `endif

  // --------------------------
  // add constraint based on database legal values
  // --------------------------
  function void build();
    PHY_PMU_PWR_STS_0 = new("PHY_PMU_PWR_STS_0", "RO/V", 1, 0, {"PWRGD_STS.PHY_PMU_PWR_STS_0"});
    PHY_PMU_PWR_STS_0.set_powerwell("primary");
    PHY_PMU_PWR_STS_0.set_rand_mode(0);
   PHY_PMU_PWR_STS_0.set_reset_signame("pmu_rst_b");
    void'(add_field( PHY_PMU_PWR_STS_0 ));

    PHY_PMU_PWR_STS_1 = new("PHY_PMU_PWR_STS_1", "RO/V", 1, 1, {"PWRGD_STS.PHY_PMU_PWR_STS_1"});
    PHY_PMU_PWR_STS_1.set_powerwell("primary");
    PHY_PMU_PWR_STS_1.set_rand_mode(0);
   PHY_PMU_PWR_STS_1.set_reset_signame("pmu_rst_b");
    void'(add_field( PHY_PMU_PWR_STS_1 ));

    PHY_PMU_PWR_STS_2 = new("PHY_PMU_PWR_STS_2", "RO/V", 1, 2, {"PWRGD_STS.PHY_PMU_PWR_STS_2"});
    PHY_PMU_PWR_STS_2.set_powerwell("primary");
    PHY_PMU_PWR_STS_2.set_rand_mode(0);
   PHY_PMU_PWR_STS_2.set_reset_signame("pmu_rst_b");
    void'(add_field( PHY_PMU_PWR_STS_2 ));

    PHY_PMU_PWR_STS_3 = new("PHY_PMU_PWR_STS_3", "RO/V", 1, 3, {"PWRGD_STS.PHY_PMU_PWR_STS_3"});
    PHY_PMU_PWR_STS_3.set_powerwell("primary");
    PHY_PMU_PWR_STS_3.set_rand_mode(0);
   PHY_PMU_PWR_STS_3.set_reset_signame("pmu_rst_b");
    void'(add_field( PHY_PMU_PWR_STS_3 ));

    PWRSTS_4 = new("PWRSTS_4", "RO/V", 1, 4, {"PWRGD_STS.PWRSTS_4"});
    PWRSTS_4.set_powerwell("primary");
    PWRSTS_4.set_rand_mode(0);
   PWRSTS_4.set_reset_signame("pmu_rst_b");
    void'(add_field( PWRSTS_4 ));

    PWRSTS_5 = new("PWRSTS_5", "RO/V", 1, 5, {"PWRGD_STS.PWRSTS_5"});
    PWRSTS_5.set_powerwell("primary");
    PWRSTS_5.set_rand_mode(0);
   PWRSTS_5.set_reset_signame("pmu_rst_b");
    void'(add_field( PWRSTS_5 ));

    PWRSTS_6 = new("PWRSTS_6", "RO/V", 1, 6, {"PWRGD_STS.PWRSTS_6"});
    PWRSTS_6.set_powerwell("primary");
    PWRSTS_6.set_rand_mode(0);
   PWRSTS_6.set_reset_signame("pmu_rst_b");
    void'(add_field( PWRSTS_6 ));

    PWRSTS_7 = new("PWRSTS_7", "RO/V", 1, 7, {"PWRGD_STS.PWRSTS_7"});
    PWRSTS_7.set_powerwell("primary");
    PWRSTS_7.set_rand_mode(0);
   PWRSTS_7.set_reset_signame("pmu_rst_b");
    void'(add_field( PWRSTS_7 ));

    RSVD_31_8 = new("RSVD_31_8", "RO/V", 24, 8, {"PWRGD_STS.RSVD_31_8"});
    RSVD_31_8.set_powerwell("primary");
    RSVD_31_8.set_rand_mode(0);
    void'(add_field( RSVD_31_8 ));

  endfunction


`DEFINE_NONE_SCOPE_REG_API(pmu_mmr_PWRGD_STS_reg) 
endclass : pmu_mmr_PWRGD_STS_reg

// ================================================

class pmu_mmr_ISOL_CTRL_reg extends sla_ral_reg;

  // --------------------------
  sla_ral_field ISOL_0;
  sla_ral_field ISOL_1;
  sla_ral_field ISOL_2;
  sla_ral_field ISOL_3;
  sla_ral_field ISOL_4;
  sla_ral_field ISOL_5;
  sla_ral_field ISOL_6;
  sla_ral_field ISOL_7;
  sla_ral_field RSVD_31_8;

  // --------------------------
  `ovm_object_utils(pmu_mmr_ISOL_CTRL_reg)

  // --------------------------
  function new(
                string         name = "",
                `ifdef SLA_RAL_COVERAGE
                sla_ral_coverage_t cov_t = COVERAGE_OFF,
                `endif
                int            bus_num=0,
                int            dev_num=0,
                int            func_num=0,
                int            offset=0,
                int            size=0,
                sla_ral_data_t reset_val=0,
                boolean_t      mon_enabled = SLA_FALSE
             );
      super.new();
      set_space_addr("CFG",undef);
      set_space_addr("MEM",undef);
      set_space_addr("IO",undef);
      set_space_addr("MSG",undef);
      set_space_addr("CREG",undef); 
      set_space_addr("LT_MEM",undef);    
 build();

      `ifdef SLA_RAL_COVERAGE
      `CONFIG_RAL_COVERAGE(regname, COVERAGE_ON, FD_RW_SAMPLE)
      if(_coverage_type != COVERAGE_OFF) begin
        if(_sample_type inside {FD_RO_SAMPLE, RO_SAMPLE})
        `CREATE_OP_SAMPLE_COV(RAL_WRITE)
        else if(_sample_type inside {FD_WO_SAMPLE, WO_SAMPLE})
        `CREATE_OP_SAMPLE_COV(RAL_READ)
        else
        `CREATE_OP_SAMPLE_COV(RAL_NONE)
        `CREATE_DESIRED_COV
        `CREATE_ACTUAL_COV
      end
      `endif

  endfunction

  `ifdef SLA_RAL_COVERAGE
  `CREATE_RAL_OP_COVERGROUP
  covergroup desired_cg @(cov_ev);
     option.per_instance = 1;
     option.name = {sla_ral_file::regfilename,".",regname};
     `RAL_FIELD_CP(ISOL_0, ISOL_0.desired)
     `RAL_FIELD_CP_1(ISOL_0, ISOL_0.desired, 0)
     `RAL_FIELD_CP(ISOL_1, ISOL_1.desired)
     `RAL_FIELD_CP_1(ISOL_1, ISOL_1.desired, 0)
     `RAL_FIELD_CP(ISOL_2, ISOL_2.desired)
     `RAL_FIELD_CP_1(ISOL_2, ISOL_2.desired, 0)
     `RAL_FIELD_CP(ISOL_3, ISOL_3.desired)
     `RAL_FIELD_CP_1(ISOL_3, ISOL_3.desired, 0)
     `RAL_FIELD_CP(ISOL_4, ISOL_4.desired)
     `RAL_FIELD_CP_1(ISOL_4, ISOL_4.desired, 0)
     `RAL_FIELD_CP(ISOL_5, ISOL_5.desired)
     `RAL_FIELD_CP_1(ISOL_5, ISOL_5.desired, 0)
     `RAL_FIELD_CP(ISOL_6, ISOL_6.desired)
     `RAL_FIELD_CP_1(ISOL_6, ISOL_6.desired, 0)
     `RAL_FIELD_CP(ISOL_7, ISOL_7.desired)
     `RAL_FIELD_CP_1(ISOL_7, ISOL_7.desired, 0)
     `RAL_FIELD_CP(RSVD_31_8, RSVD_31_8.desired)
     `RAL_FIELD_CP_16(RSVD_31_8, RSVD_31_8.desired, 0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)
     `RAL_FIELD_CP_8(RSVD_31_8, RSVD_31_8.desired, 16,17,18,19,20,21,22,23)
  endgroup
  covergroup actual_cg @(cov_ev);
     option.per_instance = 1;
     option.name = {sla_ral_file::regfilename,".",regname};
     `RAL_FIELD_CP(ISOL_0, ISOL_0.actual)
     `RAL_FIELD_CP_1(ISOL_0, ISOL_0.actual, 0)
     `RAL_FIELD_CP(ISOL_1, ISOL_1.actual)
     `RAL_FIELD_CP_1(ISOL_1, ISOL_1.actual, 0)
     `RAL_FIELD_CP(ISOL_2, ISOL_2.actual)
     `RAL_FIELD_CP_1(ISOL_2, ISOL_2.actual, 0)
     `RAL_FIELD_CP(ISOL_3, ISOL_3.actual)
     `RAL_FIELD_CP_1(ISOL_3, ISOL_3.actual, 0)
     `RAL_FIELD_CP(ISOL_4, ISOL_4.actual)
     `RAL_FIELD_CP_1(ISOL_4, ISOL_4.actual, 0)
     `RAL_FIELD_CP(ISOL_5, ISOL_5.actual)
     `RAL_FIELD_CP_1(ISOL_5, ISOL_5.actual, 0)
     `RAL_FIELD_CP(ISOL_6, ISOL_6.actual)
     `RAL_FIELD_CP_1(ISOL_6, ISOL_6.actual, 0)
     `RAL_FIELD_CP(ISOL_7, ISOL_7.actual)
     `RAL_FIELD_CP_1(ISOL_7, ISOL_7.actual, 0)
     `RAL_FIELD_CP(RSVD_31_8, RSVD_31_8.actual)
     `RAL_FIELD_CP_16(RSVD_31_8, RSVD_31_8.actual, 0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)
     `RAL_FIELD_CP_8(RSVD_31_8, RSVD_31_8.actual, 16,17,18,19,20,21,22,23)
  endgroup
  `endif

  // --------------------------
  // add constraint based on database legal values
  // --------------------------
  function void build();
    ISOL_0 = new("ISOL_0", "RW/V", 1, 0, {"ISOL_CTRL.ISOL_0"});
    ISOL_0.set_powerwell("primary");
    ISOL_0.set_rand_mode(0);
   ISOL_0.set_reset_signame("pmu_rst_b");
    void'(add_field( ISOL_0 ));

    ISOL_1 = new("ISOL_1", "RW/V", 1, 1, {"ISOL_CTRL.ISOL_1"});
    ISOL_1.set_powerwell("primary");
    ISOL_1.set_rand_mode(0);
   ISOL_1.set_reset_signame("pmu_rst_b");
    void'(add_field( ISOL_1 ));

    ISOL_2 = new("ISOL_2", "RW/V", 1, 2, {"ISOL_CTRL.ISOL_2"});
    ISOL_2.set_powerwell("primary");
    ISOL_2.set_rand_mode(0);
   ISOL_2.set_reset_signame("pmu_rst_b");
    void'(add_field( ISOL_2 ));

    ISOL_3 = new("ISOL_3", "RW/V", 1, 3, {"ISOL_CTRL.ISOL_3"});
    ISOL_3.set_powerwell("primary");
    ISOL_3.set_rand_mode(0);
   ISOL_3.set_reset_signame("pmu_rst_b");
    void'(add_field( ISOL_3 ));

    ISOL_4 = new("ISOL_4", "RW/V", 1, 4, {"ISOL_CTRL.ISOL_4"});
    ISOL_4.set_powerwell("primary");
    ISOL_4.set_rand_mode(0);
   ISOL_4.set_reset_signame("pmu_rst_b");
    void'(add_field( ISOL_4 ));

    ISOL_5 = new("ISOL_5", "RW/V", 1, 5, {"ISOL_CTRL.ISOL_5"});
    ISOL_5.set_powerwell("primary");
    ISOL_5.set_rand_mode(0);
   ISOL_5.set_reset_signame("pmu_rst_b");
    void'(add_field( ISOL_5 ));

    ISOL_6 = new("ISOL_6", "RW/V", 1, 6, {"ISOL_CTRL.ISOL_6"});
    ISOL_6.set_powerwell("primary");
    ISOL_6.set_rand_mode(0);
   ISOL_6.set_reset_signame("pmu_rst_b");
    void'(add_field( ISOL_6 ));

    ISOL_7 = new("ISOL_7", "RW/V", 1, 7, {"ISOL_CTRL.ISOL_7"});
    ISOL_7.set_powerwell("primary");
    ISOL_7.set_rand_mode(0);
   ISOL_7.set_reset_signame("pmu_rst_b");
    void'(add_field( ISOL_7 ));

    RSVD_31_8 = new("RSVD_31_8", "RO", 24, 8, {"ISOL_CTRL.RSVD_31_8"});
    RSVD_31_8.set_powerwell("primary");
    RSVD_31_8.set_rand_mode(0);
    void'(add_field( RSVD_31_8 ));

  endfunction


`DEFINE_NONE_SCOPE_REG_API(pmu_mmr_ISOL_CTRL_reg) 
endclass : pmu_mmr_ISOL_CTRL_reg

// ================================================

class pmu_mmr_PLL_SIG_0_reg extends sla_ral_reg;

  // --------------------------
  sla_ral_field CGU_PLL_VALID;
  sla_ral_field SSC_PLL_VALID;
  sla_ral_field PLL_VALID_2;
  sla_ral_field PLL_VALID_3;
  sla_ral_field PLL_VALID_4;
  sla_ral_field PLL_VALID_5;
  sla_ral_field PLL_VALID_6;
  sla_ral_field PLL_VALID_7;
  sla_ral_field PLL_VALID_8;
  sla_ral_field PLL_VALID_9;
  sla_ral_field PLL_VALID_10;
  sla_ral_field PLL_VALID_11;
  sla_ral_field PLL_VALID_12;
  sla_ral_field PLL_VALID_13;
  sla_ral_field PLL_VALID_14;
  sla_ral_field PLL_VALID_15;
  sla_ral_field CGU_PLL_EN;
  sla_ral_field SSC_PLL_EN;
  sla_ral_field PLL_EN_2;
  sla_ral_field PLL_EN_3;
  sla_ral_field PLL_EN_4;
  sla_ral_field PLL_EN_5;
  sla_ral_field PLL_EN_6;
  sla_ral_field PLL_EN_7;
  sla_ral_field PLL_EN_8;
  sla_ral_field PLL_EN_9;
  sla_ral_field PLL_EN_10;
  sla_ral_field PLL_EN_11;
  sla_ral_field PLL_EN_12;
  sla_ral_field PLL_EN_13;
  sla_ral_field PLL_EN_14;
  sla_ral_field PLL_EN_15;

  // --------------------------
  `ovm_object_utils(pmu_mmr_PLL_SIG_0_reg)

  // --------------------------
  function new(
                string         name = "",
                `ifdef SLA_RAL_COVERAGE
                sla_ral_coverage_t cov_t = COVERAGE_OFF,
                `endif
                int            bus_num=0,
                int            dev_num=0,
                int            func_num=0,
                int            offset=0,
                int            size=0,
                sla_ral_data_t reset_val=0,
                boolean_t      mon_enabled = SLA_FALSE
             );
      super.new();
      set_space_addr("CFG",undef);
      set_space_addr("MEM",undef);
      set_space_addr("IO",undef);
      set_space_addr("MSG",undef);
      set_space_addr("CREG",undef); 
      set_space_addr("LT_MEM",undef);    
 build();

      `ifdef SLA_RAL_COVERAGE
      `CONFIG_RAL_COVERAGE(regname, COVERAGE_ON, FD_RW_SAMPLE)
      if(_coverage_type != COVERAGE_OFF) begin
        if(_sample_type inside {FD_RO_SAMPLE, RO_SAMPLE})
        `CREATE_OP_SAMPLE_COV(RAL_WRITE)
        else if(_sample_type inside {FD_WO_SAMPLE, WO_SAMPLE})
        `CREATE_OP_SAMPLE_COV(RAL_READ)
        else
        `CREATE_OP_SAMPLE_COV(RAL_NONE)
        `CREATE_DESIRED_COV
        `CREATE_ACTUAL_COV
      end
      `endif

  endfunction

  `ifdef SLA_RAL_COVERAGE
  `CREATE_RAL_OP_COVERGROUP
  covergroup desired_cg @(cov_ev);
     option.per_instance = 1;
     option.name = {sla_ral_file::regfilename,".",regname};
     `RAL_FIELD_CP(CGU_PLL_VALID, CGU_PLL_VALID.desired)
     `RAL_FIELD_CP_1(CGU_PLL_VALID, CGU_PLL_VALID.desired, 0)
     `RAL_FIELD_CP(SSC_PLL_VALID, SSC_PLL_VALID.desired)
     `RAL_FIELD_CP_1(SSC_PLL_VALID, SSC_PLL_VALID.desired, 0)
     `RAL_FIELD_CP(PLL_VALID_2, PLL_VALID_2.desired)
     `RAL_FIELD_CP_1(PLL_VALID_2, PLL_VALID_2.desired, 0)
     `RAL_FIELD_CP(PLL_VALID_3, PLL_VALID_3.desired)
     `RAL_FIELD_CP_1(PLL_VALID_3, PLL_VALID_3.desired, 0)
     `RAL_FIELD_CP(PLL_VALID_4, PLL_VALID_4.desired)
     `RAL_FIELD_CP_1(PLL_VALID_4, PLL_VALID_4.desired, 0)
     `RAL_FIELD_CP(PLL_VALID_5, PLL_VALID_5.desired)
     `RAL_FIELD_CP_1(PLL_VALID_5, PLL_VALID_5.desired, 0)
     `RAL_FIELD_CP(PLL_VALID_6, PLL_VALID_6.desired)
     `RAL_FIELD_CP_1(PLL_VALID_6, PLL_VALID_6.desired, 0)
     `RAL_FIELD_CP(PLL_VALID_7, PLL_VALID_7.desired)
     `RAL_FIELD_CP_1(PLL_VALID_7, PLL_VALID_7.desired, 0)
     `RAL_FIELD_CP(PLL_VALID_8, PLL_VALID_8.desired)
     `RAL_FIELD_CP_1(PLL_VALID_8, PLL_VALID_8.desired, 0)
     `RAL_FIELD_CP(PLL_VALID_9, PLL_VALID_9.desired)
     `RAL_FIELD_CP_1(PLL_VALID_9, PLL_VALID_9.desired, 0)
     `RAL_FIELD_CP(PLL_VALID_10, PLL_VALID_10.desired)
     `RAL_FIELD_CP_1(PLL_VALID_10, PLL_VALID_10.desired, 0)
     `RAL_FIELD_CP(PLL_VALID_11, PLL_VALID_11.desired)
     `RAL_FIELD_CP_1(PLL_VALID_11, PLL_VALID_11.desired, 0)
     `RAL_FIELD_CP(PLL_VALID_12, PLL_VALID_12.desired)
     `RAL_FIELD_CP_1(PLL_VALID_12, PLL_VALID_12.desired, 0)
     `RAL_FIELD_CP(PLL_VALID_13, PLL_VALID_13.desired)
     `RAL_FIELD_CP_1(PLL_VALID_13, PLL_VALID_13.desired, 0)
     `RAL_FIELD_CP(PLL_VALID_14, PLL_VALID_14.desired)
     `RAL_FIELD_CP_1(PLL_VALID_14, PLL_VALID_14.desired, 0)
     `RAL_FIELD_CP(PLL_VALID_15, PLL_VALID_15.desired)
     `RAL_FIELD_CP_1(PLL_VALID_15, PLL_VALID_15.desired, 0)
     `RAL_FIELD_CP(CGU_PLL_EN, CGU_PLL_EN.desired)
     `RAL_FIELD_CP_1(CGU_PLL_EN, CGU_PLL_EN.desired, 0)
     `RAL_FIELD_CP(SSC_PLL_EN, SSC_PLL_EN.desired)
     `RAL_FIELD_CP_1(SSC_PLL_EN, SSC_PLL_EN.desired, 0)
     `RAL_FIELD_CP(PLL_EN_2, PLL_EN_2.desired)
     `RAL_FIELD_CP_1(PLL_EN_2, PLL_EN_2.desired, 0)
     `RAL_FIELD_CP(PLL_EN_3, PLL_EN_3.desired)
     `RAL_FIELD_CP_1(PLL_EN_3, PLL_EN_3.desired, 0)
     `RAL_FIELD_CP(PLL_EN_4, PLL_EN_4.desired)
     `RAL_FIELD_CP_1(PLL_EN_4, PLL_EN_4.desired, 0)
     `RAL_FIELD_CP(PLL_EN_5, PLL_EN_5.desired)
     `RAL_FIELD_CP_1(PLL_EN_5, PLL_EN_5.desired, 0)
     `RAL_FIELD_CP(PLL_EN_6, PLL_EN_6.desired)
     `RAL_FIELD_CP_1(PLL_EN_6, PLL_EN_6.desired, 0)
     `RAL_FIELD_CP(PLL_EN_7, PLL_EN_7.desired)
     `RAL_FIELD_CP_1(PLL_EN_7, PLL_EN_7.desired, 0)
     `RAL_FIELD_CP(PLL_EN_8, PLL_EN_8.desired)
     `RAL_FIELD_CP_1(PLL_EN_8, PLL_EN_8.desired, 0)
     `RAL_FIELD_CP(PLL_EN_9, PLL_EN_9.desired)
     `RAL_FIELD_CP_1(PLL_EN_9, PLL_EN_9.desired, 0)
     `RAL_FIELD_CP(PLL_EN_10, PLL_EN_10.desired)
     `RAL_FIELD_CP_1(PLL_EN_10, PLL_EN_10.desired, 0)
     `RAL_FIELD_CP(PLL_EN_11, PLL_EN_11.desired)
     `RAL_FIELD_CP_1(PLL_EN_11, PLL_EN_11.desired, 0)
     `RAL_FIELD_CP(PLL_EN_12, PLL_EN_12.desired)
     `RAL_FIELD_CP_1(PLL_EN_12, PLL_EN_12.desired, 0)
     `RAL_FIELD_CP(PLL_EN_13, PLL_EN_13.desired)
     `RAL_FIELD_CP_1(PLL_EN_13, PLL_EN_13.desired, 0)
     `RAL_FIELD_CP(PLL_EN_14, PLL_EN_14.desired)
     `RAL_FIELD_CP_1(PLL_EN_14, PLL_EN_14.desired, 0)
     `RAL_FIELD_CP(PLL_EN_15, PLL_EN_15.desired)
     `RAL_FIELD_CP_1(PLL_EN_15, PLL_EN_15.desired, 0)
  endgroup
  covergroup actual_cg @(cov_ev);
     option.per_instance = 1;
     option.name = {sla_ral_file::regfilename,".",regname};
     `RAL_FIELD_CP(CGU_PLL_VALID, CGU_PLL_VALID.actual)
     `RAL_FIELD_CP_1(CGU_PLL_VALID, CGU_PLL_VALID.actual, 0)
     `RAL_FIELD_CP(SSC_PLL_VALID, SSC_PLL_VALID.actual)
     `RAL_FIELD_CP_1(SSC_PLL_VALID, SSC_PLL_VALID.actual, 0)
     `RAL_FIELD_CP(PLL_VALID_2, PLL_VALID_2.actual)
     `RAL_FIELD_CP_1(PLL_VALID_2, PLL_VALID_2.actual, 0)
     `RAL_FIELD_CP(PLL_VALID_3, PLL_VALID_3.actual)
     `RAL_FIELD_CP_1(PLL_VALID_3, PLL_VALID_3.actual, 0)
     `RAL_FIELD_CP(PLL_VALID_4, PLL_VALID_4.actual)
     `RAL_FIELD_CP_1(PLL_VALID_4, PLL_VALID_4.actual, 0)
     `RAL_FIELD_CP(PLL_VALID_5, PLL_VALID_5.actual)
     `RAL_FIELD_CP_1(PLL_VALID_5, PLL_VALID_5.actual, 0)
     `RAL_FIELD_CP(PLL_VALID_6, PLL_VALID_6.actual)
     `RAL_FIELD_CP_1(PLL_VALID_6, PLL_VALID_6.actual, 0)
     `RAL_FIELD_CP(PLL_VALID_7, PLL_VALID_7.actual)
     `RAL_FIELD_CP_1(PLL_VALID_7, PLL_VALID_7.actual, 0)
     `RAL_FIELD_CP(PLL_VALID_8, PLL_VALID_8.actual)
     `RAL_FIELD_CP_1(PLL_VALID_8, PLL_VALID_8.actual, 0)
     `RAL_FIELD_CP(PLL_VALID_9, PLL_VALID_9.actual)
     `RAL_FIELD_CP_1(PLL_VALID_9, PLL_VALID_9.actual, 0)
     `RAL_FIELD_CP(PLL_VALID_10, PLL_VALID_10.actual)
     `RAL_FIELD_CP_1(PLL_VALID_10, PLL_VALID_10.actual, 0)
     `RAL_FIELD_CP(PLL_VALID_11, PLL_VALID_11.actual)
     `RAL_FIELD_CP_1(PLL_VALID_11, PLL_VALID_11.actual, 0)
     `RAL_FIELD_CP(PLL_VALID_12, PLL_VALID_12.actual)
     `RAL_FIELD_CP_1(PLL_VALID_12, PLL_VALID_12.actual, 0)
     `RAL_FIELD_CP(PLL_VALID_13, PLL_VALID_13.actual)
     `RAL_FIELD_CP_1(PLL_VALID_13, PLL_VALID_13.actual, 0)
     `RAL_FIELD_CP(PLL_VALID_14, PLL_VALID_14.actual)
     `RAL_FIELD_CP_1(PLL_VALID_14, PLL_VALID_14.actual, 0)
     `RAL_FIELD_CP(PLL_VALID_15, PLL_VALID_15.actual)
     `RAL_FIELD_CP_1(PLL_VALID_15, PLL_VALID_15.actual, 0)
     `RAL_FIELD_CP(CGU_PLL_EN, CGU_PLL_EN.actual)
     `RAL_FIELD_CP_1(CGU_PLL_EN, CGU_PLL_EN.actual, 0)
     `RAL_FIELD_CP(SSC_PLL_EN, SSC_PLL_EN.actual)
     `RAL_FIELD_CP_1(SSC_PLL_EN, SSC_PLL_EN.actual, 0)
     `RAL_FIELD_CP(PLL_EN_2, PLL_EN_2.actual)
     `RAL_FIELD_CP_1(PLL_EN_2, PLL_EN_2.actual, 0)
     `RAL_FIELD_CP(PLL_EN_3, PLL_EN_3.actual)
     `RAL_FIELD_CP_1(PLL_EN_3, PLL_EN_3.actual, 0)
     `RAL_FIELD_CP(PLL_EN_4, PLL_EN_4.actual)
     `RAL_FIELD_CP_1(PLL_EN_4, PLL_EN_4.actual, 0)
     `RAL_FIELD_CP(PLL_EN_5, PLL_EN_5.actual)
     `RAL_FIELD_CP_1(PLL_EN_5, PLL_EN_5.actual, 0)
     `RAL_FIELD_CP(PLL_EN_6, PLL_EN_6.actual)
     `RAL_FIELD_CP_1(PLL_EN_6, PLL_EN_6.actual, 0)
     `RAL_FIELD_CP(PLL_EN_7, PLL_EN_7.actual)
     `RAL_FIELD_CP_1(PLL_EN_7, PLL_EN_7.actual, 0)
     `RAL_FIELD_CP(PLL_EN_8, PLL_EN_8.actual)
     `RAL_FIELD_CP_1(PLL_EN_8, PLL_EN_8.actual, 0)
     `RAL_FIELD_CP(PLL_EN_9, PLL_EN_9.actual)
     `RAL_FIELD_CP_1(PLL_EN_9, PLL_EN_9.actual, 0)
     `RAL_FIELD_CP(PLL_EN_10, PLL_EN_10.actual)
     `RAL_FIELD_CP_1(PLL_EN_10, PLL_EN_10.actual, 0)
     `RAL_FIELD_CP(PLL_EN_11, PLL_EN_11.actual)
     `RAL_FIELD_CP_1(PLL_EN_11, PLL_EN_11.actual, 0)
     `RAL_FIELD_CP(PLL_EN_12, PLL_EN_12.actual)
     `RAL_FIELD_CP_1(PLL_EN_12, PLL_EN_12.actual, 0)
     `RAL_FIELD_CP(PLL_EN_13, PLL_EN_13.actual)
     `RAL_FIELD_CP_1(PLL_EN_13, PLL_EN_13.actual, 0)
     `RAL_FIELD_CP(PLL_EN_14, PLL_EN_14.actual)
     `RAL_FIELD_CP_1(PLL_EN_14, PLL_EN_14.actual, 0)
     `RAL_FIELD_CP(PLL_EN_15, PLL_EN_15.actual)
     `RAL_FIELD_CP_1(PLL_EN_15, PLL_EN_15.actual, 0)
  endgroup
  `endif

  // --------------------------
  // add constraint based on database legal values
  // --------------------------
  function void build();
    CGU_PLL_VALID = new("CGU_PLL_VALID", "RO/V", 1, 0, {"PLL_SIG_0.CGU_PLL_VALID"});
    CGU_PLL_VALID.set_powerwell("primary");
    CGU_PLL_VALID.set_rand_mode(0);
    void'(add_field( CGU_PLL_VALID ));

    SSC_PLL_VALID = new("SSC_PLL_VALID", "RO/V", 1, 1, {"PLL_SIG_0.SSC_PLL_VALID"});
    SSC_PLL_VALID.set_powerwell("primary");
    SSC_PLL_VALID.set_rand_mode(0);
    void'(add_field( SSC_PLL_VALID ));

    PLL_VALID_2 = new("PLL_VALID_2", "RO/V", 1, 2, {"PLL_SIG_0.PLL_VALID_2"});
    PLL_VALID_2.set_powerwell("primary");
    PLL_VALID_2.set_rand_mode(0);
    void'(add_field( PLL_VALID_2 ));

    PLL_VALID_3 = new("PLL_VALID_3", "RO/V", 1, 3, {"PLL_SIG_0.PLL_VALID_3"});
    PLL_VALID_3.set_powerwell("primary");
    PLL_VALID_3.set_rand_mode(0);
    void'(add_field( PLL_VALID_3 ));

    PLL_VALID_4 = new("PLL_VALID_4", "RO/V", 1, 4, {"PLL_SIG_0.PLL_VALID_4"});
    PLL_VALID_4.set_powerwell("primary");
    PLL_VALID_4.set_rand_mode(0);
    void'(add_field( PLL_VALID_4 ));

    PLL_VALID_5 = new("PLL_VALID_5", "RO/V", 1, 5, {"PLL_SIG_0.PLL_VALID_5"});
    PLL_VALID_5.set_powerwell("primary");
    PLL_VALID_5.set_rand_mode(0);
    void'(add_field( PLL_VALID_5 ));

    PLL_VALID_6 = new("PLL_VALID_6", "RO/V", 1, 6, {"PLL_SIG_0.PLL_VALID_6"});
    PLL_VALID_6.set_powerwell("primary");
    PLL_VALID_6.set_rand_mode(0);
    void'(add_field( PLL_VALID_6 ));

    PLL_VALID_7 = new("PLL_VALID_7", "RO/V", 1, 7, {"PLL_SIG_0.PLL_VALID_7"});
    PLL_VALID_7.set_powerwell("primary");
    PLL_VALID_7.set_rand_mode(0);
    void'(add_field( PLL_VALID_7 ));

    PLL_VALID_8 = new("PLL_VALID_8", "RO/V", 1, 8, {"PLL_SIG_0.PLL_VALID_8"});
    PLL_VALID_8.set_powerwell("primary");
    PLL_VALID_8.set_rand_mode(0);
    void'(add_field( PLL_VALID_8 ));

    PLL_VALID_9 = new("PLL_VALID_9", "RO/V", 1, 9, {"PLL_SIG_0.PLL_VALID_9"});
    PLL_VALID_9.set_powerwell("primary");
    PLL_VALID_9.set_rand_mode(0);
    void'(add_field( PLL_VALID_9 ));

    PLL_VALID_10 = new("PLL_VALID_10", "RO/V", 1, 10, {"PLL_SIG_0.PLL_VALID_10"});
    PLL_VALID_10.set_powerwell("primary");
    PLL_VALID_10.set_rand_mode(0);
    void'(add_field( PLL_VALID_10 ));

    PLL_VALID_11 = new("PLL_VALID_11", "RO/V", 1, 11, {"PLL_SIG_0.PLL_VALID_11"});
    PLL_VALID_11.set_powerwell("primary");
    PLL_VALID_11.set_rand_mode(0);
    void'(add_field( PLL_VALID_11 ));

    PLL_VALID_12 = new("PLL_VALID_12", "RO/V", 1, 12, {"PLL_SIG_0.PLL_VALID_12"});
    PLL_VALID_12.set_powerwell("primary");
    PLL_VALID_12.set_rand_mode(0);
    void'(add_field( PLL_VALID_12 ));

    PLL_VALID_13 = new("PLL_VALID_13", "RO/V", 1, 13, {"PLL_SIG_0.PLL_VALID_13"});
    PLL_VALID_13.set_powerwell("primary");
    PLL_VALID_13.set_rand_mode(0);
    void'(add_field( PLL_VALID_13 ));

    PLL_VALID_14 = new("PLL_VALID_14", "RO/V", 1, 14, {"PLL_SIG_0.PLL_VALID_14"});
    PLL_VALID_14.set_powerwell("primary");
    PLL_VALID_14.set_rand_mode(0);
    void'(add_field( PLL_VALID_14 ));

    PLL_VALID_15 = new("PLL_VALID_15", "RO/V", 1, 15, {"PLL_SIG_0.PLL_VALID_15"});
    PLL_VALID_15.set_powerwell("primary");
    PLL_VALID_15.set_rand_mode(0);
    void'(add_field( PLL_VALID_15 ));

    CGU_PLL_EN = new("CGU_PLL_EN", "RW", 1, 16, {"PLL_SIG_0.CGU_PLL_EN"});
    CGU_PLL_EN.set_powerwell("primary");
    CGU_PLL_EN.set_rand_mode(0);
   CGU_PLL_EN.set_reset_signame("pmu_rst_b");
    void'(add_field( CGU_PLL_EN ));

    SSC_PLL_EN = new("SSC_PLL_EN", "RW", 1, 17, {"PLL_SIG_0.SSC_PLL_EN"});
    SSC_PLL_EN.set_powerwell("primary");
    SSC_PLL_EN.set_rand_mode(0);
   SSC_PLL_EN.set_reset_signame("pmu_rst_b");
    void'(add_field( SSC_PLL_EN ));

    PLL_EN_2 = new("PLL_EN_2", "RW", 1, 18, {"PLL_SIG_0.PLL_EN_2"});
    PLL_EN_2.set_powerwell("primary");
    PLL_EN_2.set_rand_mode(0);
   PLL_EN_2.set_reset_signame("pmu_rst_b");
    void'(add_field( PLL_EN_2 ));

    PLL_EN_3 = new("PLL_EN_3", "RW", 1, 19, {"PLL_SIG_0.PLL_EN_3"});
    PLL_EN_3.set_powerwell("primary");
    PLL_EN_3.set_rand_mode(0);
   PLL_EN_3.set_reset_signame("pmu_rst_b");
    void'(add_field( PLL_EN_3 ));

    PLL_EN_4 = new("PLL_EN_4", "RW", 1, 20, {"PLL_SIG_0.PLL_EN_4"});
    PLL_EN_4.set_powerwell("primary");
    PLL_EN_4.set_rand_mode(0);
   PLL_EN_4.set_reset_signame("pmu_rst_b");
    void'(add_field( PLL_EN_4 ));

    PLL_EN_5 = new("PLL_EN_5", "RW", 1, 21, {"PLL_SIG_0.PLL_EN_5"});
    PLL_EN_5.set_powerwell("primary");
    PLL_EN_5.set_rand_mode(0);
   PLL_EN_5.set_reset_signame("pmu_rst_b");
    void'(add_field( PLL_EN_5 ));

    PLL_EN_6 = new("PLL_EN_6", "RW", 1, 22, {"PLL_SIG_0.PLL_EN_6"});
    PLL_EN_6.set_powerwell("primary");
    PLL_EN_6.set_rand_mode(0);
   PLL_EN_6.set_reset_signame("pmu_rst_b");
    void'(add_field( PLL_EN_6 ));

    PLL_EN_7 = new("PLL_EN_7", "RW", 1, 23, {"PLL_SIG_0.PLL_EN_7"});
    PLL_EN_7.set_powerwell("primary");
    PLL_EN_7.set_rand_mode(0);
   PLL_EN_7.set_reset_signame("pmu_rst_b");
    void'(add_field( PLL_EN_7 ));

    PLL_EN_8 = new("PLL_EN_8", "RW", 1, 24, {"PLL_SIG_0.PLL_EN_8"});
    PLL_EN_8.set_powerwell("primary");
    PLL_EN_8.set_rand_mode(0);
   PLL_EN_8.set_reset_signame("pmu_rst_b");
    void'(add_field( PLL_EN_8 ));

    PLL_EN_9 = new("PLL_EN_9", "RW", 1, 25, {"PLL_SIG_0.PLL_EN_9"});
    PLL_EN_9.set_powerwell("primary");
    PLL_EN_9.set_rand_mode(0);
   PLL_EN_9.set_reset_signame("pmu_rst_b");
    void'(add_field( PLL_EN_9 ));

    PLL_EN_10 = new("PLL_EN_10", "RW", 1, 26, {"PLL_SIG_0.PLL_EN_10"});
    PLL_EN_10.set_powerwell("primary");
    PLL_EN_10.set_rand_mode(0);
   PLL_EN_10.set_reset_signame("pmu_rst_b");
    void'(add_field( PLL_EN_10 ));

    PLL_EN_11 = new("PLL_EN_11", "RW", 1, 27, {"PLL_SIG_0.PLL_EN_11"});
    PLL_EN_11.set_powerwell("primary");
    PLL_EN_11.set_rand_mode(0);
   PLL_EN_11.set_reset_signame("pmu_rst_b");
    void'(add_field( PLL_EN_11 ));

    PLL_EN_12 = new("PLL_EN_12", "RW", 1, 28, {"PLL_SIG_0.PLL_EN_12"});
    PLL_EN_12.set_powerwell("primary");
    PLL_EN_12.set_rand_mode(0);
   PLL_EN_12.set_reset_signame("pmu_rst_b");
    void'(add_field( PLL_EN_12 ));

    PLL_EN_13 = new("PLL_EN_13", "RW", 1, 29, {"PLL_SIG_0.PLL_EN_13"});
    PLL_EN_13.set_powerwell("primary");
    PLL_EN_13.set_rand_mode(0);
   PLL_EN_13.set_reset_signame("pmu_rst_b");
    void'(add_field( PLL_EN_13 ));

    PLL_EN_14 = new("PLL_EN_14", "RW", 1, 30, {"PLL_SIG_0.PLL_EN_14"});
    PLL_EN_14.set_powerwell("primary");
    PLL_EN_14.set_rand_mode(0);
   PLL_EN_14.set_reset_signame("pmu_rst_b");
    void'(add_field( PLL_EN_14 ));

    PLL_EN_15 = new("PLL_EN_15", "RW", 1, 31, {"PLL_SIG_0.PLL_EN_15"});
    PLL_EN_15.set_powerwell("primary");
    PLL_EN_15.set_rand_mode(0);
   PLL_EN_15.set_reset_signame("pmu_rst_b");
    void'(add_field( PLL_EN_15 ));

  endfunction


`DEFINE_NONE_SCOPE_REG_API(pmu_mmr_PLL_SIG_0_reg) 
endclass : pmu_mmr_PLL_SIG_0_reg

// ================================================

class pmu_mmr_PG_CTL_reg extends sla_ral_reg;

  // --------------------------
  sla_ral_field PG_PGACK_PFETEN_LTCY;
  sla_ral_field PG_STALL_LTCY;
  sla_ral_field PUG_STALL_LTCY;
  sla_ral_field IP_FAST_PWRUNGATE_LTCY;
  sla_ral_field IP_SLOW_PWRUNGATE_LTCY;

  // --------------------------
  `ovm_object_utils(pmu_mmr_PG_CTL_reg)

  // --------------------------
  function new(
                string         name = "",
                `ifdef SLA_RAL_COVERAGE
                sla_ral_coverage_t cov_t = COVERAGE_OFF,
                `endif
                int            bus_num=0,
                int            dev_num=0,
                int            func_num=0,
                int            offset=0,
                int            size=0,
                sla_ral_data_t reset_val=0,
                boolean_t      mon_enabled = SLA_FALSE
             );
      super.new();
      set_space_addr("CFG",undef);
      set_space_addr("MEM",undef);
      set_space_addr("IO",undef);
      set_space_addr("MSG",undef);
      set_space_addr("CREG",undef); 
      set_space_addr("LT_MEM",undef);    
 build();

      `ifdef SLA_RAL_COVERAGE
      `CONFIG_RAL_COVERAGE(regname, COVERAGE_ON, FD_RW_SAMPLE)
      if(_coverage_type != COVERAGE_OFF) begin
        if(_sample_type inside {FD_RO_SAMPLE, RO_SAMPLE})
        `CREATE_OP_SAMPLE_COV(RAL_WRITE)
        else if(_sample_type inside {FD_WO_SAMPLE, WO_SAMPLE})
        `CREATE_OP_SAMPLE_COV(RAL_READ)
        else
        `CREATE_OP_SAMPLE_COV(RAL_NONE)
        `CREATE_DESIRED_COV
        `CREATE_ACTUAL_COV
      end
      `endif

  endfunction

  `ifdef SLA_RAL_COVERAGE
  `CREATE_RAL_OP_COVERGROUP
  covergroup desired_cg @(cov_ev);
     option.per_instance = 1;
     option.name = {sla_ral_file::regfilename,".",regname};
     `RAL_FIELD_CP(PG_PGACK_PFETEN_LTCY, PG_PGACK_PFETEN_LTCY.desired)
     `RAL_FIELD_CP_4(PG_PGACK_PFETEN_LTCY, PG_PGACK_PFETEN_LTCY.desired, 0,1,2,3)
     `RAL_FIELD_CP(PG_STALL_LTCY, PG_STALL_LTCY.desired)
     `RAL_FIELD_CP_4(PG_STALL_LTCY, PG_STALL_LTCY.desired, 0,1,2,3)
     `RAL_FIELD_CP(PUG_STALL_LTCY, PUG_STALL_LTCY.desired)
     `RAL_FIELD_CP_4(PUG_STALL_LTCY, PUG_STALL_LTCY.desired, 0,1,2,3)
     `RAL_FIELD_CP(IP_FAST_PWRUNGATE_LTCY, IP_FAST_PWRUNGATE_LTCY.desired)
     `RAL_FIELD_CP_2(IP_FAST_PWRUNGATE_LTCY, IP_FAST_PWRUNGATE_LTCY.desired, 0,1)
     `RAL_FIELD_CP(IP_SLOW_PWRUNGATE_LTCY, IP_SLOW_PWRUNGATE_LTCY.desired)
     `RAL_FIELD_CP_2(IP_SLOW_PWRUNGATE_LTCY, IP_SLOW_PWRUNGATE_LTCY.desired, 0,1)
  endgroup
  covergroup actual_cg @(cov_ev);
     option.per_instance = 1;
     option.name = {sla_ral_file::regfilename,".",regname};
     `RAL_FIELD_CP(PG_PGACK_PFETEN_LTCY, PG_PGACK_PFETEN_LTCY.actual)
     `RAL_FIELD_CP_4(PG_PGACK_PFETEN_LTCY, PG_PGACK_PFETEN_LTCY.actual, 0,1,2,3)
     `RAL_FIELD_CP(PG_STALL_LTCY, PG_STALL_LTCY.actual)
     `RAL_FIELD_CP_4(PG_STALL_LTCY, PG_STALL_LTCY.actual, 0,1,2,3)
     `RAL_FIELD_CP(PUG_STALL_LTCY, PUG_STALL_LTCY.actual)
     `RAL_FIELD_CP_4(PUG_STALL_LTCY, PUG_STALL_LTCY.actual, 0,1,2,3)
     `RAL_FIELD_CP(IP_FAST_PWRUNGATE_LTCY, IP_FAST_PWRUNGATE_LTCY.actual)
     `RAL_FIELD_CP_2(IP_FAST_PWRUNGATE_LTCY, IP_FAST_PWRUNGATE_LTCY.actual, 0,1)
     `RAL_FIELD_CP(IP_SLOW_PWRUNGATE_LTCY, IP_SLOW_PWRUNGATE_LTCY.actual)
     `RAL_FIELD_CP_2(IP_SLOW_PWRUNGATE_LTCY, IP_SLOW_PWRUNGATE_LTCY.actual, 0,1)
  endgroup
  `endif

  // --------------------------
  // add constraint based on database legal values
  // --------------------------
  function void build();
    PG_PGACK_PFETEN_LTCY = new("PG_PGACK_PFETEN_LTCY", "RW", 4, 0, {"PG_CTL.PG_PGACK_PFETEN_LTCY"});
    PG_PGACK_PFETEN_LTCY.set_powerwell("primary");
    PG_PGACK_PFETEN_LTCY.set_rand_mode(0);
   PG_PGACK_PFETEN_LTCY.set_reset_signame("pmu_rst_b");
    void'(add_field( PG_PGACK_PFETEN_LTCY ));

    PG_STALL_LTCY = new("PG_STALL_LTCY", "RW", 4, 4, {"PG_CTL.PG_STALL_LTCY"});
    PG_STALL_LTCY.set_powerwell("primary");
    PG_STALL_LTCY.set_rand_mode(0);
   PG_STALL_LTCY.set_reset_signame("pmu_rst_b");
    void'(add_field( PG_STALL_LTCY ));

    PUG_STALL_LTCY = new("PUG_STALL_LTCY", "RW", 4, 8, {"PG_CTL.PUG_STALL_LTCY"});
    PUG_STALL_LTCY.set_powerwell("primary");
    PUG_STALL_LTCY.set_rand_mode(0);
   PUG_STALL_LTCY.set_reset_signame("pmu_rst_b");
    void'(add_field( PUG_STALL_LTCY ));

    IP_FAST_PWRUNGATE_LTCY = new("IP_FAST_PWRUNGATE_LTCY", "RW", 2, 16, {"PG_CTL.IP_FAST_PWRUNGATE_LTCY"});
    IP_FAST_PWRUNGATE_LTCY.set_powerwell("primary");
    IP_FAST_PWRUNGATE_LTCY.set_rand_mode(0);
   IP_FAST_PWRUNGATE_LTCY.set_reset_signame("pmu_rst_b");
    void'(add_field( IP_FAST_PWRUNGATE_LTCY ));

    IP_SLOW_PWRUNGATE_LTCY = new("IP_SLOW_PWRUNGATE_LTCY", "RW", 2, 18, {"PG_CTL.IP_SLOW_PWRUNGATE_LTCY"});
    IP_SLOW_PWRUNGATE_LTCY.set_powerwell("primary");
    IP_SLOW_PWRUNGATE_LTCY.set_rand_mode(0);
   IP_SLOW_PWRUNGATE_LTCY.set_reset_signame("pmu_rst_b");
    void'(add_field( IP_SLOW_PWRUNGATE_LTCY ));

  endfunction


`DEFINE_NONE_SCOPE_REG_API(pmu_mmr_PG_CTL_reg) 
endclass : pmu_mmr_PG_CTL_reg

// ================================================

class pmu_mmr_IP_PUG_TYPE_0_reg extends sla_ral_reg;

  // --------------------------
  sla_ral_field PUG_TYPE;

  // --------------------------
  `ovm_object_utils(pmu_mmr_IP_PUG_TYPE_0_reg)

  // --------------------------
  function new(
                string         name = "",
                `ifdef SLA_RAL_COVERAGE
                sla_ral_coverage_t cov_t = COVERAGE_OFF,
                `endif
                int            bus_num=0,
                int            dev_num=0,
                int            func_num=0,
                int            offset=0,
                int            size=0,
                sla_ral_data_t reset_val=0,
                boolean_t      mon_enabled = SLA_FALSE
             );
      super.new();
      set_space_addr("CFG",undef);
      set_space_addr("MEM",undef);
      set_space_addr("IO",undef);
      set_space_addr("MSG",undef);
      set_space_addr("CREG",undef); 
      set_space_addr("LT_MEM",undef);    
 build();

      `ifdef SLA_RAL_COVERAGE
      `CONFIG_RAL_COVERAGE(regname, COVERAGE_ON, FD_RW_SAMPLE)
      if(_coverage_type != COVERAGE_OFF) begin
        if(_sample_type inside {FD_RO_SAMPLE, RO_SAMPLE})
        `CREATE_OP_SAMPLE_COV(RAL_WRITE)
        else if(_sample_type inside {FD_WO_SAMPLE, WO_SAMPLE})
        `CREATE_OP_SAMPLE_COV(RAL_READ)
        else
        `CREATE_OP_SAMPLE_COV(RAL_NONE)
        `CREATE_DESIRED_COV
        `CREATE_ACTUAL_COV
      end
      `endif

  endfunction

  `ifdef SLA_RAL_COVERAGE
  `CREATE_RAL_OP_COVERGROUP
  covergroup desired_cg @(cov_ev);
     option.per_instance = 1;
     option.name = {sla_ral_file::regfilename,".",regname};
     `RAL_FIELD_CP(PUG_TYPE, PUG_TYPE.desired)
     `RAL_FIELD_CP_16(PUG_TYPE, PUG_TYPE.desired, 0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)
     `RAL_FIELD_CP_16(PUG_TYPE, PUG_TYPE.desired, 16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31)
  endgroup
  covergroup actual_cg @(cov_ev);
     option.per_instance = 1;
     option.name = {sla_ral_file::regfilename,".",regname};
     `RAL_FIELD_CP(PUG_TYPE, PUG_TYPE.actual)
     `RAL_FIELD_CP_16(PUG_TYPE, PUG_TYPE.actual, 0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)
     `RAL_FIELD_CP_16(PUG_TYPE, PUG_TYPE.actual, 16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31)
  endgroup
  `endif

  // --------------------------
  // add constraint based on database legal values
  // --------------------------
  function void build();
    PUG_TYPE = new("PUG_TYPE", "RW", 32, 0, {"IP_PUG_TYPE_0.PUG_TYPE"});
    PUG_TYPE.set_powerwell("primary");
    PUG_TYPE.set_rand_mode(0);
   PUG_TYPE.set_reset_signame("pmu_rst_b");
    void'(add_field( PUG_TYPE ));

  endfunction


`DEFINE_NONE_SCOPE_REG_API(pmu_mmr_IP_PUG_TYPE_0_reg) 
endclass : pmu_mmr_IP_PUG_TYPE_0_reg

// ================================================

class pmu_mmr_IP_PUG_TYPE_1_reg extends sla_ral_reg;

  // --------------------------
  sla_ral_field PUG_TYPE;

  // --------------------------
  `ovm_object_utils(pmu_mmr_IP_PUG_TYPE_1_reg)

  // --------------------------
  function new(
                string         name = "",
                `ifdef SLA_RAL_COVERAGE
                sla_ral_coverage_t cov_t = COVERAGE_OFF,
                `endif
                int            bus_num=0,
                int            dev_num=0,
                int            func_num=0,
                int            offset=0,
                int            size=0,
                sla_ral_data_t reset_val=0,
                boolean_t      mon_enabled = SLA_FALSE
             );
      super.new();
      set_space_addr("CFG",undef);
      set_space_addr("MEM",undef);
      set_space_addr("IO",undef);
      set_space_addr("MSG",undef);
      set_space_addr("CREG",undef); 
      set_space_addr("LT_MEM",undef);    
 build();

      `ifdef SLA_RAL_COVERAGE
      `CONFIG_RAL_COVERAGE(regname, COVERAGE_ON, FD_RW_SAMPLE)
      if(_coverage_type != COVERAGE_OFF) begin
        if(_sample_type inside {FD_RO_SAMPLE, RO_SAMPLE})
        `CREATE_OP_SAMPLE_COV(RAL_WRITE)
        else if(_sample_type inside {FD_WO_SAMPLE, WO_SAMPLE})
        `CREATE_OP_SAMPLE_COV(RAL_READ)
        else
        `CREATE_OP_SAMPLE_COV(RAL_NONE)
        `CREATE_DESIRED_COV
        `CREATE_ACTUAL_COV
      end
      `endif

  endfunction

  `ifdef SLA_RAL_COVERAGE
  `CREATE_RAL_OP_COVERGROUP
  covergroup desired_cg @(cov_ev);
     option.per_instance = 1;
     option.name = {sla_ral_file::regfilename,".",regname};
     `RAL_FIELD_CP(PUG_TYPE, PUG_TYPE.desired)
     `RAL_FIELD_CP_16(PUG_TYPE, PUG_TYPE.desired, 0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)
     `RAL_FIELD_CP_16(PUG_TYPE, PUG_TYPE.desired, 16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31)
  endgroup
  covergroup actual_cg @(cov_ev);
     option.per_instance = 1;
     option.name = {sla_ral_file::regfilename,".",regname};
     `RAL_FIELD_CP(PUG_TYPE, PUG_TYPE.actual)
     `RAL_FIELD_CP_16(PUG_TYPE, PUG_TYPE.actual, 0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)
     `RAL_FIELD_CP_16(PUG_TYPE, PUG_TYPE.actual, 16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31)
  endgroup
  `endif

  // --------------------------
  // add constraint based on database legal values
  // --------------------------
  function void build();
    PUG_TYPE = new("PUG_TYPE", "RW", 32, 0, {"IP_PUG_TYPE_1.PUG_TYPE"});
    PUG_TYPE.set_powerwell("primary");
    PUG_TYPE.set_rand_mode(0);
   PUG_TYPE.set_reset_signame("pmu_rst_b");
    void'(add_field( PUG_TYPE ));

  endfunction


`DEFINE_NONE_SCOPE_REG_API(pmu_mmr_IP_PUG_TYPE_1_reg) 
endclass : pmu_mmr_IP_PUG_TYPE_1_reg

// ================================================

class pmu_mmr_IP_PG_REQ_STS_0_reg extends sla_ral_reg;

  // --------------------------
  sla_ral_field PG_REQ_STS;

  // --------------------------
  `ovm_object_utils(pmu_mmr_IP_PG_REQ_STS_0_reg)

  // --------------------------
  function new(
                string         name = "",
                `ifdef SLA_RAL_COVERAGE
                sla_ral_coverage_t cov_t = COVERAGE_OFF,
                `endif
                int            bus_num=0,
                int            dev_num=0,
                int            func_num=0,
                int            offset=0,
                int            size=0,
                sla_ral_data_t reset_val=0,
                boolean_t      mon_enabled = SLA_FALSE
             );
      super.new();
      set_space_addr("CFG",undef);
      set_space_addr("MEM",undef);
      set_space_addr("IO",undef);
      set_space_addr("MSG",undef);
      set_space_addr("CREG",undef); 
      set_space_addr("LT_MEM",undef);    
 build();

      `ifdef SLA_RAL_COVERAGE
      `CONFIG_RAL_COVERAGE(regname, COVERAGE_ON, FD_RW_SAMPLE)
      if(_coverage_type != COVERAGE_OFF) begin
        if(_sample_type inside {FD_RO_SAMPLE, RO_SAMPLE})
        `CREATE_OP_SAMPLE_COV(RAL_WRITE)
        else if(_sample_type inside {FD_WO_SAMPLE, WO_SAMPLE})
        `CREATE_OP_SAMPLE_COV(RAL_READ)
        else
        `CREATE_OP_SAMPLE_COV(RAL_NONE)
        `CREATE_DESIRED_COV
        `CREATE_ACTUAL_COV
      end
      `endif

  endfunction

  `ifdef SLA_RAL_COVERAGE
  `CREATE_RAL_OP_COVERGROUP
  covergroup desired_cg @(cov_ev);
     option.per_instance = 1;
     option.name = {sla_ral_file::regfilename,".",regname};
     `RAL_FIELD_CP(PG_REQ_STS, PG_REQ_STS.desired)
     `RAL_FIELD_CP_16(PG_REQ_STS, PG_REQ_STS.desired, 0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)
     `RAL_FIELD_CP_16(PG_REQ_STS, PG_REQ_STS.desired, 16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31)
  endgroup
  covergroup actual_cg @(cov_ev);
     option.per_instance = 1;
     option.name = {sla_ral_file::regfilename,".",regname};
     `RAL_FIELD_CP(PG_REQ_STS, PG_REQ_STS.actual)
     `RAL_FIELD_CP_16(PG_REQ_STS, PG_REQ_STS.actual, 0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)
     `RAL_FIELD_CP_16(PG_REQ_STS, PG_REQ_STS.actual, 16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31)
  endgroup
  `endif

  // --------------------------
  // add constraint based on database legal values
  // --------------------------
  function void build();
    PG_REQ_STS = new("PG_REQ_STS", "RO/V", 32, 0, {"IP_PG_REQ_STS_0.PG_REQ_STS"});
    PG_REQ_STS.set_powerwell("primary");
    PG_REQ_STS.set_rand_mode(0);
    void'(add_field( PG_REQ_STS ));

  endfunction


`DEFINE_NONE_SCOPE_REG_API(pmu_mmr_IP_PG_REQ_STS_0_reg) 
endclass : pmu_mmr_IP_PG_REQ_STS_0_reg

// ================================================

class pmu_mmr_IP_PG_REQ_STS_1_reg extends sla_ral_reg;

  // --------------------------
  sla_ral_field PG_REQ_STS;

  // --------------------------
  `ovm_object_utils(pmu_mmr_IP_PG_REQ_STS_1_reg)

  // --------------------------
  function new(
                string         name = "",
                `ifdef SLA_RAL_COVERAGE
                sla_ral_coverage_t cov_t = COVERAGE_OFF,
                `endif
                int            bus_num=0,
                int            dev_num=0,
                int            func_num=0,
                int            offset=0,
                int            size=0,
                sla_ral_data_t reset_val=0,
                boolean_t      mon_enabled = SLA_FALSE
             );
      super.new();
      set_space_addr("CFG",undef);
      set_space_addr("MEM",undef);
      set_space_addr("IO",undef);
      set_space_addr("MSG",undef);
      set_space_addr("CREG",undef); 
      set_space_addr("LT_MEM",undef);    
 build();

      `ifdef SLA_RAL_COVERAGE
      `CONFIG_RAL_COVERAGE(regname, COVERAGE_ON, FD_RW_SAMPLE)
      if(_coverage_type != COVERAGE_OFF) begin
        if(_sample_type inside {FD_RO_SAMPLE, RO_SAMPLE})
        `CREATE_OP_SAMPLE_COV(RAL_WRITE)
        else if(_sample_type inside {FD_WO_SAMPLE, WO_SAMPLE})
        `CREATE_OP_SAMPLE_COV(RAL_READ)
        else
        `CREATE_OP_SAMPLE_COV(RAL_NONE)
        `CREATE_DESIRED_COV
        `CREATE_ACTUAL_COV
      end
      `endif

  endfunction

  `ifdef SLA_RAL_COVERAGE
  `CREATE_RAL_OP_COVERGROUP
  covergroup desired_cg @(cov_ev);
     option.per_instance = 1;
     option.name = {sla_ral_file::regfilename,".",regname};
     `RAL_FIELD_CP(PG_REQ_STS, PG_REQ_STS.desired)
     `RAL_FIELD_CP_16(PG_REQ_STS, PG_REQ_STS.desired, 0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)
     `RAL_FIELD_CP_16(PG_REQ_STS, PG_REQ_STS.desired, 16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31)
  endgroup
  covergroup actual_cg @(cov_ev);
     option.per_instance = 1;
     option.name = {sla_ral_file::regfilename,".",regname};
     `RAL_FIELD_CP(PG_REQ_STS, PG_REQ_STS.actual)
     `RAL_FIELD_CP_16(PG_REQ_STS, PG_REQ_STS.actual, 0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)
     `RAL_FIELD_CP_16(PG_REQ_STS, PG_REQ_STS.actual, 16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31)
  endgroup
  `endif

  // --------------------------
  // add constraint based on database legal values
  // --------------------------
  function void build();
    PG_REQ_STS = new("PG_REQ_STS", "RO/V", 32, 0, {"IP_PG_REQ_STS_1.PG_REQ_STS"});
    PG_REQ_STS.set_powerwell("primary");
    PG_REQ_STS.set_rand_mode(0);
    void'(add_field( PG_REQ_STS ));

  endfunction


`DEFINE_NONE_SCOPE_REG_API(pmu_mmr_IP_PG_REQ_STS_1_reg) 
endclass : pmu_mmr_IP_PG_REQ_STS_1_reg

// ================================================

class pmu_mmr_IP_PWR_STS_0_reg extends sla_ral_reg;

  // --------------------------
  sla_ral_field IP_PWR_STS;

  // --------------------------
  `ovm_object_utils(pmu_mmr_IP_PWR_STS_0_reg)

  // --------------------------
  function new(
                string         name = "",
                `ifdef SLA_RAL_COVERAGE
                sla_ral_coverage_t cov_t = COVERAGE_OFF,
                `endif
                int            bus_num=0,
                int            dev_num=0,
                int            func_num=0,
                int            offset=0,
                int            size=0,
                sla_ral_data_t reset_val=0,
                boolean_t      mon_enabled = SLA_FALSE
             );
      super.new();
      set_space_addr("CFG",undef);
      set_space_addr("MEM",undef);
      set_space_addr("IO",undef);
      set_space_addr("MSG",undef);
      set_space_addr("CREG",undef); 
      set_space_addr("LT_MEM",undef);    
 build();

      `ifdef SLA_RAL_COVERAGE
      `CONFIG_RAL_COVERAGE(regname, COVERAGE_ON, FD_RW_SAMPLE)
      if(_coverage_type != COVERAGE_OFF) begin
        if(_sample_type inside {FD_RO_SAMPLE, RO_SAMPLE})
        `CREATE_OP_SAMPLE_COV(RAL_WRITE)
        else if(_sample_type inside {FD_WO_SAMPLE, WO_SAMPLE})
        `CREATE_OP_SAMPLE_COV(RAL_READ)
        else
        `CREATE_OP_SAMPLE_COV(RAL_NONE)
        `CREATE_DESIRED_COV
        `CREATE_ACTUAL_COV
      end
      `endif

  endfunction

  `ifdef SLA_RAL_COVERAGE
  `CREATE_RAL_OP_COVERGROUP
  covergroup desired_cg @(cov_ev);
     option.per_instance = 1;
     option.name = {sla_ral_file::regfilename,".",regname};
     `RAL_FIELD_CP(IP_PWR_STS, IP_PWR_STS.desired)
     `RAL_FIELD_CP_16(IP_PWR_STS, IP_PWR_STS.desired, 0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)
     `RAL_FIELD_CP_16(IP_PWR_STS, IP_PWR_STS.desired, 16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31)
  endgroup
  covergroup actual_cg @(cov_ev);
     option.per_instance = 1;
     option.name = {sla_ral_file::regfilename,".",regname};
     `RAL_FIELD_CP(IP_PWR_STS, IP_PWR_STS.actual)
     `RAL_FIELD_CP_16(IP_PWR_STS, IP_PWR_STS.actual, 0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)
     `RAL_FIELD_CP_16(IP_PWR_STS, IP_PWR_STS.actual, 16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31)
  endgroup
  `endif

  // --------------------------
  // add constraint based on database legal values
  // --------------------------
  function void build();
    IP_PWR_STS = new("IP_PWR_STS", "RO/V", 32, 0, {"IP_PWR_STS_0.IP_PWR_STS"});
    IP_PWR_STS.set_powerwell("primary");
    IP_PWR_STS.set_rand_mode(0);
    void'(add_field( IP_PWR_STS ));

  endfunction


`DEFINE_NONE_SCOPE_REG_API(pmu_mmr_IP_PWR_STS_0_reg) 
endclass : pmu_mmr_IP_PWR_STS_0_reg

// ================================================

class pmu_mmr_IP_PWR_STS_1_reg extends sla_ral_reg;

  // --------------------------
  sla_ral_field IP_PWR_STS;

  // --------------------------
  `ovm_object_utils(pmu_mmr_IP_PWR_STS_1_reg)

  // --------------------------
  function new(
                string         name = "",
                `ifdef SLA_RAL_COVERAGE
                sla_ral_coverage_t cov_t = COVERAGE_OFF,
                `endif
                int            bus_num=0,
                int            dev_num=0,
                int            func_num=0,
                int            offset=0,
                int            size=0,
                sla_ral_data_t reset_val=0,
                boolean_t      mon_enabled = SLA_FALSE
             );
      super.new();
      set_space_addr("CFG",undef);
      set_space_addr("MEM",undef);
      set_space_addr("IO",undef);
      set_space_addr("MSG",undef);
      set_space_addr("CREG",undef); 
      set_space_addr("LT_MEM",undef);    
 build();

      `ifdef SLA_RAL_COVERAGE
      `CONFIG_RAL_COVERAGE(regname, COVERAGE_ON, FD_RW_SAMPLE)
      if(_coverage_type != COVERAGE_OFF) begin
        if(_sample_type inside {FD_RO_SAMPLE, RO_SAMPLE})
        `CREATE_OP_SAMPLE_COV(RAL_WRITE)
        else if(_sample_type inside {FD_WO_SAMPLE, WO_SAMPLE})
        `CREATE_OP_SAMPLE_COV(RAL_READ)
        else
        `CREATE_OP_SAMPLE_COV(RAL_NONE)
        `CREATE_DESIRED_COV
        `CREATE_ACTUAL_COV
      end
      `endif

  endfunction

  `ifdef SLA_RAL_COVERAGE
  `CREATE_RAL_OP_COVERGROUP
  covergroup desired_cg @(cov_ev);
     option.per_instance = 1;
     option.name = {sla_ral_file::regfilename,".",regname};
     `RAL_FIELD_CP(IP_PWR_STS, IP_PWR_STS.desired)
     `RAL_FIELD_CP_16(IP_PWR_STS, IP_PWR_STS.desired, 0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)
     `RAL_FIELD_CP_16(IP_PWR_STS, IP_PWR_STS.desired, 16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31)
  endgroup
  covergroup actual_cg @(cov_ev);
     option.per_instance = 1;
     option.name = {sla_ral_file::regfilename,".",regname};
     `RAL_FIELD_CP(IP_PWR_STS, IP_PWR_STS.actual)
     `RAL_FIELD_CP_16(IP_PWR_STS, IP_PWR_STS.actual, 0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)
     `RAL_FIELD_CP_16(IP_PWR_STS, IP_PWR_STS.actual, 16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31)
  endgroup
  `endif

  // --------------------------
  // add constraint based on database legal values
  // --------------------------
  function void build();
    IP_PWR_STS = new("IP_PWR_STS", "RO/V", 32, 0, {"IP_PWR_STS_1.IP_PWR_STS"});
    IP_PWR_STS.set_powerwell("primary");
    IP_PWR_STS.set_rand_mode(0);
    void'(add_field( IP_PWR_STS ));

  endfunction


`DEFINE_NONE_SCOPE_REG_API(pmu_mmr_IP_PWR_STS_1_reg) 
endclass : pmu_mmr_IP_PWR_STS_1_reg

// ================================================

class pmu_mmr_IP_FET_EN_ACK_STS_0_reg extends sla_ral_reg;

  // --------------------------
  sla_ral_field FET_EN_ACK_STS;

  // --------------------------
  `ovm_object_utils(pmu_mmr_IP_FET_EN_ACK_STS_0_reg)

  // --------------------------
  function new(
                string         name = "",
                `ifdef SLA_RAL_COVERAGE
                sla_ral_coverage_t cov_t = COVERAGE_OFF,
                `endif
                int            bus_num=0,
                int            dev_num=0,
                int            func_num=0,
                int            offset=0,
                int            size=0,
                sla_ral_data_t reset_val=0,
                boolean_t      mon_enabled = SLA_FALSE
             );
      super.new();
      set_space_addr("CFG",undef);
      set_space_addr("MEM",undef);
      set_space_addr("IO",undef);
      set_space_addr("MSG",undef);
      set_space_addr("CREG",undef); 
      set_space_addr("LT_MEM",undef);    
 build();

      `ifdef SLA_RAL_COVERAGE
      `CONFIG_RAL_COVERAGE(regname, COVERAGE_ON, FD_RW_SAMPLE)
      if(_coverage_type != COVERAGE_OFF) begin
        if(_sample_type inside {FD_RO_SAMPLE, RO_SAMPLE})
        `CREATE_OP_SAMPLE_COV(RAL_WRITE)
        else if(_sample_type inside {FD_WO_SAMPLE, WO_SAMPLE})
        `CREATE_OP_SAMPLE_COV(RAL_READ)
        else
        `CREATE_OP_SAMPLE_COV(RAL_NONE)
        `CREATE_DESIRED_COV
        `CREATE_ACTUAL_COV
      end
      `endif

  endfunction

  `ifdef SLA_RAL_COVERAGE
  `CREATE_RAL_OP_COVERGROUP
  covergroup desired_cg @(cov_ev);
     option.per_instance = 1;
     option.name = {sla_ral_file::regfilename,".",regname};
     `RAL_FIELD_CP(FET_EN_ACK_STS, FET_EN_ACK_STS.desired)
     `RAL_FIELD_CP_16(FET_EN_ACK_STS, FET_EN_ACK_STS.desired, 0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)
     `RAL_FIELD_CP_16(FET_EN_ACK_STS, FET_EN_ACK_STS.desired, 16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31)
  endgroup
  covergroup actual_cg @(cov_ev);
     option.per_instance = 1;
     option.name = {sla_ral_file::regfilename,".",regname};
     `RAL_FIELD_CP(FET_EN_ACK_STS, FET_EN_ACK_STS.actual)
     `RAL_FIELD_CP_16(FET_EN_ACK_STS, FET_EN_ACK_STS.actual, 0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)
     `RAL_FIELD_CP_16(FET_EN_ACK_STS, FET_EN_ACK_STS.actual, 16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31)
  endgroup
  `endif

  // --------------------------
  // add constraint based on database legal values
  // --------------------------
  function void build();
    FET_EN_ACK_STS = new("FET_EN_ACK_STS", "RO/V", 32, 0, {"IP_FET_EN_ACK_STS_0.FET_EN_ACK_STS"});
    FET_EN_ACK_STS.set_powerwell("primary");
    FET_EN_ACK_STS.set_rand_mode(0);
    void'(add_field( FET_EN_ACK_STS ));

  endfunction


`DEFINE_NONE_SCOPE_REG_API(pmu_mmr_IP_FET_EN_ACK_STS_0_reg) 
endclass : pmu_mmr_IP_FET_EN_ACK_STS_0_reg

// ================================================

class pmu_mmr_IP_FET_EN_ACK_STS_1_reg extends sla_ral_reg;

  // --------------------------
  sla_ral_field FET_EN_ACK_STS;

  // --------------------------
  `ovm_object_utils(pmu_mmr_IP_FET_EN_ACK_STS_1_reg)

  // --------------------------
  function new(
                string         name = "",
                `ifdef SLA_RAL_COVERAGE
                sla_ral_coverage_t cov_t = COVERAGE_OFF,
                `endif
                int            bus_num=0,
                int            dev_num=0,
                int            func_num=0,
                int            offset=0,
                int            size=0,
                sla_ral_data_t reset_val=0,
                boolean_t      mon_enabled = SLA_FALSE
             );
      super.new();
      set_space_addr("CFG",undef);
      set_space_addr("MEM",undef);
      set_space_addr("IO",undef);
      set_space_addr("MSG",undef);
      set_space_addr("CREG",undef); 
      set_space_addr("LT_MEM",undef);    
 build();

      `ifdef SLA_RAL_COVERAGE
      `CONFIG_RAL_COVERAGE(regname, COVERAGE_ON, FD_RW_SAMPLE)
      if(_coverage_type != COVERAGE_OFF) begin
        if(_sample_type inside {FD_RO_SAMPLE, RO_SAMPLE})
        `CREATE_OP_SAMPLE_COV(RAL_WRITE)
        else if(_sample_type inside {FD_WO_SAMPLE, WO_SAMPLE})
        `CREATE_OP_SAMPLE_COV(RAL_READ)
        else
        `CREATE_OP_SAMPLE_COV(RAL_NONE)
        `CREATE_DESIRED_COV
        `CREATE_ACTUAL_COV
      end
      `endif

  endfunction

  `ifdef SLA_RAL_COVERAGE
  `CREATE_RAL_OP_COVERGROUP
  covergroup desired_cg @(cov_ev);
     option.per_instance = 1;
     option.name = {sla_ral_file::regfilename,".",regname};
     `RAL_FIELD_CP(FET_EN_ACK_STS, FET_EN_ACK_STS.desired)
     `RAL_FIELD_CP_16(FET_EN_ACK_STS, FET_EN_ACK_STS.desired, 0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)
     `RAL_FIELD_CP_16(FET_EN_ACK_STS, FET_EN_ACK_STS.desired, 16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31)
  endgroup
  covergroup actual_cg @(cov_ev);
     option.per_instance = 1;
     option.name = {sla_ral_file::regfilename,".",regname};
     `RAL_FIELD_CP(FET_EN_ACK_STS, FET_EN_ACK_STS.actual)
     `RAL_FIELD_CP_16(FET_EN_ACK_STS, FET_EN_ACK_STS.actual, 0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)
     `RAL_FIELD_CP_16(FET_EN_ACK_STS, FET_EN_ACK_STS.actual, 16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31)
  endgroup
  `endif

  // --------------------------
  // add constraint based on database legal values
  // --------------------------
  function void build();
    FET_EN_ACK_STS = new("FET_EN_ACK_STS", "RO/V", 32, 0, {"IP_FET_EN_ACK_STS_1.FET_EN_ACK_STS"});
    FET_EN_ACK_STS.set_powerwell("primary");
    FET_EN_ACK_STS.set_rand_mode(0);
    void'(add_field( FET_EN_ACK_STS ));

  endfunction


`DEFINE_NONE_SCOPE_REG_API(pmu_mmr_IP_FET_EN_ACK_STS_1_reg) 
endclass : pmu_mmr_IP_FET_EN_ACK_STS_1_reg

// ================================================

class pmu_mmr_IP_STS_MASK_0_reg extends sla_ral_reg;

  // --------------------------
  sla_ral_field IP_STS_MASK;

  // --------------------------
  `ovm_object_utils(pmu_mmr_IP_STS_MASK_0_reg)

  // --------------------------
  function new(
                string         name = "",
                `ifdef SLA_RAL_COVERAGE
                sla_ral_coverage_t cov_t = COVERAGE_OFF,
                `endif
                int            bus_num=0,
                int            dev_num=0,
                int            func_num=0,
                int            offset=0,
                int            size=0,
                sla_ral_data_t reset_val=0,
                boolean_t      mon_enabled = SLA_FALSE
             );
      super.new();
      set_space_addr("CFG",undef);
      set_space_addr("MEM",undef);
      set_space_addr("IO",undef);
      set_space_addr("MSG",undef);
      set_space_addr("CREG",undef); 
      set_space_addr("LT_MEM",undef);    
 build();

      `ifdef SLA_RAL_COVERAGE
      `CONFIG_RAL_COVERAGE(regname, COVERAGE_ON, FD_RW_SAMPLE)
      if(_coverage_type != COVERAGE_OFF) begin
        if(_sample_type inside {FD_RO_SAMPLE, RO_SAMPLE})
        `CREATE_OP_SAMPLE_COV(RAL_WRITE)
        else if(_sample_type inside {FD_WO_SAMPLE, WO_SAMPLE})
        `CREATE_OP_SAMPLE_COV(RAL_READ)
        else
        `CREATE_OP_SAMPLE_COV(RAL_NONE)
        `CREATE_DESIRED_COV
        `CREATE_ACTUAL_COV
      end
      `endif

  endfunction

  `ifdef SLA_RAL_COVERAGE
  `CREATE_RAL_OP_COVERGROUP
  covergroup desired_cg @(cov_ev);
     option.per_instance = 1;
     option.name = {sla_ral_file::regfilename,".",regname};
     `RAL_FIELD_CP(IP_STS_MASK, IP_STS_MASK.desired)
     `RAL_FIELD_CP_16(IP_STS_MASK, IP_STS_MASK.desired, 0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)
     `RAL_FIELD_CP_16(IP_STS_MASK, IP_STS_MASK.desired, 16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31)
  endgroup
  covergroup actual_cg @(cov_ev);
     option.per_instance = 1;
     option.name = {sla_ral_file::regfilename,".",regname};
     `RAL_FIELD_CP(IP_STS_MASK, IP_STS_MASK.actual)
     `RAL_FIELD_CP_16(IP_STS_MASK, IP_STS_MASK.actual, 0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)
     `RAL_FIELD_CP_16(IP_STS_MASK, IP_STS_MASK.actual, 16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31)
  endgroup
  `endif

  // --------------------------
  // add constraint based on database legal values
  // --------------------------
  function void build();
    IP_STS_MASK = new("IP_STS_MASK", "RW", 32, 0, {"IP_STS_MASK_0.IP_STS_MASK"});
    IP_STS_MASK.set_powerwell("primary");
    IP_STS_MASK.set_rand_mode(0);
   IP_STS_MASK.set_reset_signame("pmu_rst_b");
    void'(add_field( IP_STS_MASK ));

  endfunction


`DEFINE_NONE_SCOPE_REG_API(pmu_mmr_IP_STS_MASK_0_reg) 
endclass : pmu_mmr_IP_STS_MASK_0_reg

// ================================================

class pmu_mmr_IP_STS_MASK_1_reg extends sla_ral_reg;

  // --------------------------
  sla_ral_field IP_STS_MASK;

  // --------------------------
  `ovm_object_utils(pmu_mmr_IP_STS_MASK_1_reg)

  // --------------------------
  function new(
                string         name = "",
                `ifdef SLA_RAL_COVERAGE
                sla_ral_coverage_t cov_t = COVERAGE_OFF,
                `endif
                int            bus_num=0,
                int            dev_num=0,
                int            func_num=0,
                int            offset=0,
                int            size=0,
                sla_ral_data_t reset_val=0,
                boolean_t      mon_enabled = SLA_FALSE
             );
      super.new();
      set_space_addr("CFG",undef);
      set_space_addr("MEM",undef);
      set_space_addr("IO",undef);
      set_space_addr("MSG",undef);
      set_space_addr("CREG",undef); 
      set_space_addr("LT_MEM",undef);    
 build();

      `ifdef SLA_RAL_COVERAGE
      `CONFIG_RAL_COVERAGE(regname, COVERAGE_ON, FD_RW_SAMPLE)
      if(_coverage_type != COVERAGE_OFF) begin
        if(_sample_type inside {FD_RO_SAMPLE, RO_SAMPLE})
        `CREATE_OP_SAMPLE_COV(RAL_WRITE)
        else if(_sample_type inside {FD_WO_SAMPLE, WO_SAMPLE})
        `CREATE_OP_SAMPLE_COV(RAL_READ)
        else
        `CREATE_OP_SAMPLE_COV(RAL_NONE)
        `CREATE_DESIRED_COV
        `CREATE_ACTUAL_COV
      end
      `endif

  endfunction

  `ifdef SLA_RAL_COVERAGE
  `CREATE_RAL_OP_COVERGROUP
  covergroup desired_cg @(cov_ev);
     option.per_instance = 1;
     option.name = {sla_ral_file::regfilename,".",regname};
     `RAL_FIELD_CP(IP_STS_MASK, IP_STS_MASK.desired)
     `RAL_FIELD_CP_16(IP_STS_MASK, IP_STS_MASK.desired, 0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)
     `RAL_FIELD_CP_16(IP_STS_MASK, IP_STS_MASK.desired, 16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31)
  endgroup
  covergroup actual_cg @(cov_ev);
     option.per_instance = 1;
     option.name = {sla_ral_file::regfilename,".",regname};
     `RAL_FIELD_CP(IP_STS_MASK, IP_STS_MASK.actual)
     `RAL_FIELD_CP_16(IP_STS_MASK, IP_STS_MASK.actual, 0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)
     `RAL_FIELD_CP_16(IP_STS_MASK, IP_STS_MASK.actual, 16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31)
  endgroup
  `endif

  // --------------------------
  // add constraint based on database legal values
  // --------------------------
  function void build();
    IP_STS_MASK = new("IP_STS_MASK", "RW", 32, 0, {"IP_STS_MASK_1.IP_STS_MASK"});
    IP_STS_MASK.set_powerwell("primary");
    IP_STS_MASK.set_rand_mode(0);
   IP_STS_MASK.set_reset_signame("pmu_rst_b");
    void'(add_field( IP_STS_MASK ));

  endfunction


`DEFINE_NONE_SCOPE_REG_API(pmu_mmr_IP_STS_MASK_1_reg) 
endclass : pmu_mmr_IP_STS_MASK_1_reg

// ================================================

class pmu_mmr_IP_WAKE_CTL_0_reg extends sla_ral_reg;

  // --------------------------
  sla_ral_field DRV_PG_WAKE;

  // --------------------------
  `ovm_object_utils(pmu_mmr_IP_WAKE_CTL_0_reg)

  // --------------------------
  function new(
                string         name = "",
                `ifdef SLA_RAL_COVERAGE
                sla_ral_coverage_t cov_t = COVERAGE_OFF,
                `endif
                int            bus_num=0,
                int            dev_num=0,
                int            func_num=0,
                int            offset=0,
                int            size=0,
                sla_ral_data_t reset_val=0,
                boolean_t      mon_enabled = SLA_FALSE
             );
      super.new();
      set_space_addr("CFG",undef);
      set_space_addr("MEM",undef);
      set_space_addr("IO",undef);
      set_space_addr("MSG",undef);
      set_space_addr("CREG",undef); 
      set_space_addr("LT_MEM",undef);    
 build();

      `ifdef SLA_RAL_COVERAGE
      `CONFIG_RAL_COVERAGE(regname, COVERAGE_ON, FD_RW_SAMPLE)
      if(_coverage_type != COVERAGE_OFF) begin
        if(_sample_type inside {FD_RO_SAMPLE, RO_SAMPLE})
        `CREATE_OP_SAMPLE_COV(RAL_WRITE)
        else if(_sample_type inside {FD_WO_SAMPLE, WO_SAMPLE})
        `CREATE_OP_SAMPLE_COV(RAL_READ)
        else
        `CREATE_OP_SAMPLE_COV(RAL_NONE)
        `CREATE_DESIRED_COV
        `CREATE_ACTUAL_COV
      end
      `endif

  endfunction

  `ifdef SLA_RAL_COVERAGE
  `CREATE_RAL_OP_COVERGROUP
  covergroup desired_cg @(cov_ev);
     option.per_instance = 1;
     option.name = {sla_ral_file::regfilename,".",regname};
     `RAL_FIELD_CP(DRV_PG_WAKE, DRV_PG_WAKE.desired)
     `RAL_FIELD_CP_16(DRV_PG_WAKE, DRV_PG_WAKE.desired, 0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)
     `RAL_FIELD_CP_16(DRV_PG_WAKE, DRV_PG_WAKE.desired, 16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31)
  endgroup
  covergroup actual_cg @(cov_ev);
     option.per_instance = 1;
     option.name = {sla_ral_file::regfilename,".",regname};
     `RAL_FIELD_CP(DRV_PG_WAKE, DRV_PG_WAKE.actual)
     `RAL_FIELD_CP_16(DRV_PG_WAKE, DRV_PG_WAKE.actual, 0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)
     `RAL_FIELD_CP_16(DRV_PG_WAKE, DRV_PG_WAKE.actual, 16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31)
  endgroup
  `endif

  // --------------------------
  // add constraint based on database legal values
  // --------------------------
  function void build();
    DRV_PG_WAKE = new("DRV_PG_WAKE", "RW", 32, 0, {"IP_WAKE_CTL_0.DRV_PG_WAKE"});
    DRV_PG_WAKE.set_powerwell("primary");
    DRV_PG_WAKE.set_rand_mode(0);
   DRV_PG_WAKE.set_reset_signame("pmu_rst_b");
    void'(add_field( DRV_PG_WAKE ));

  endfunction


`DEFINE_NONE_SCOPE_REG_API(pmu_mmr_IP_WAKE_CTL_0_reg) 
endclass : pmu_mmr_IP_WAKE_CTL_0_reg

// ================================================

class pmu_mmr_IP_WAKE_CTL_1_reg extends sla_ral_reg;

  // --------------------------
  sla_ral_field DRV_PG_WAKE;

  // --------------------------
  `ovm_object_utils(pmu_mmr_IP_WAKE_CTL_1_reg)

  // --------------------------
  function new(
                string         name = "",
                `ifdef SLA_RAL_COVERAGE
                sla_ral_coverage_t cov_t = COVERAGE_OFF,
                `endif
                int            bus_num=0,
                int            dev_num=0,
                int            func_num=0,
                int            offset=0,
                int            size=0,
                sla_ral_data_t reset_val=0,
                boolean_t      mon_enabled = SLA_FALSE
             );
      super.new();
      set_space_addr("CFG",undef);
      set_space_addr("MEM",undef);
      set_space_addr("IO",undef);
      set_space_addr("MSG",undef);
      set_space_addr("CREG",undef); 
      set_space_addr("LT_MEM",undef);    
 build();

      `ifdef SLA_RAL_COVERAGE
      `CONFIG_RAL_COVERAGE(regname, COVERAGE_ON, FD_RW_SAMPLE)
      if(_coverage_type != COVERAGE_OFF) begin
        if(_sample_type inside {FD_RO_SAMPLE, RO_SAMPLE})
        `CREATE_OP_SAMPLE_COV(RAL_WRITE)
        else if(_sample_type inside {FD_WO_SAMPLE, WO_SAMPLE})
        `CREATE_OP_SAMPLE_COV(RAL_READ)
        else
        `CREATE_OP_SAMPLE_COV(RAL_NONE)
        `CREATE_DESIRED_COV
        `CREATE_ACTUAL_COV
      end
      `endif

  endfunction

  `ifdef SLA_RAL_COVERAGE
  `CREATE_RAL_OP_COVERGROUP
  covergroup desired_cg @(cov_ev);
     option.per_instance = 1;
     option.name = {sla_ral_file::regfilename,".",regname};
     `RAL_FIELD_CP(DRV_PG_WAKE, DRV_PG_WAKE.desired)
     `RAL_FIELD_CP_16(DRV_PG_WAKE, DRV_PG_WAKE.desired, 0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)
     `RAL_FIELD_CP_16(DRV_PG_WAKE, DRV_PG_WAKE.desired, 16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31)
  endgroup
  covergroup actual_cg @(cov_ev);
     option.per_instance = 1;
     option.name = {sla_ral_file::regfilename,".",regname};
     `RAL_FIELD_CP(DRV_PG_WAKE, DRV_PG_WAKE.actual)
     `RAL_FIELD_CP_16(DRV_PG_WAKE, DRV_PG_WAKE.actual, 0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)
     `RAL_FIELD_CP_16(DRV_PG_WAKE, DRV_PG_WAKE.actual, 16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31)
  endgroup
  `endif

  // --------------------------
  // add constraint based on database legal values
  // --------------------------
  function void build();
    DRV_PG_WAKE = new("DRV_PG_WAKE", "RW", 32, 0, {"IP_WAKE_CTL_1.DRV_PG_WAKE"});
    DRV_PG_WAKE.set_powerwell("primary");
    DRV_PG_WAKE.set_rand_mode(0);
   DRV_PG_WAKE.set_reset_signame("pmu_rst_b");
    void'(add_field( DRV_PG_WAKE ));

  endfunction


`DEFINE_NONE_SCOPE_REG_API(pmu_mmr_IP_WAKE_CTL_1_reg) 
endclass : pmu_mmr_IP_WAKE_CTL_1_reg

// ================================================

class pmu_mmr_IP_SIDE_POK_STS_0_reg extends sla_ral_reg;

  // --------------------------
  sla_ral_field IP_SIDE_POK;

  // --------------------------
  `ovm_object_utils(pmu_mmr_IP_SIDE_POK_STS_0_reg)

  // --------------------------
  function new(
                string         name = "",
                `ifdef SLA_RAL_COVERAGE
                sla_ral_coverage_t cov_t = COVERAGE_OFF,
                `endif
                int            bus_num=0,
                int            dev_num=0,
                int            func_num=0,
                int            offset=0,
                int            size=0,
                sla_ral_data_t reset_val=0,
                boolean_t      mon_enabled = SLA_FALSE
             );
      super.new();
      set_space_addr("CFG",undef);
      set_space_addr("MEM",undef);
      set_space_addr("IO",undef);
      set_space_addr("MSG",undef);
      set_space_addr("CREG",undef); 
      set_space_addr("LT_MEM",undef);    
 build();

      `ifdef SLA_RAL_COVERAGE
      `CONFIG_RAL_COVERAGE(regname, COVERAGE_ON, FD_RW_SAMPLE)
      if(_coverage_type != COVERAGE_OFF) begin
        if(_sample_type inside {FD_RO_SAMPLE, RO_SAMPLE})
        `CREATE_OP_SAMPLE_COV(RAL_WRITE)
        else if(_sample_type inside {FD_WO_SAMPLE, WO_SAMPLE})
        `CREATE_OP_SAMPLE_COV(RAL_READ)
        else
        `CREATE_OP_SAMPLE_COV(RAL_NONE)
        `CREATE_DESIRED_COV
        `CREATE_ACTUAL_COV
      end
      `endif

  endfunction

  `ifdef SLA_RAL_COVERAGE
  `CREATE_RAL_OP_COVERGROUP
  covergroup desired_cg @(cov_ev);
     option.per_instance = 1;
     option.name = {sla_ral_file::regfilename,".",regname};
     `RAL_FIELD_CP(IP_SIDE_POK, IP_SIDE_POK.desired)
     `RAL_FIELD_CP_16(IP_SIDE_POK, IP_SIDE_POK.desired, 0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)
     `RAL_FIELD_CP_16(IP_SIDE_POK, IP_SIDE_POK.desired, 16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31)
  endgroup
  covergroup actual_cg @(cov_ev);
     option.per_instance = 1;
     option.name = {sla_ral_file::regfilename,".",regname};
     `RAL_FIELD_CP(IP_SIDE_POK, IP_SIDE_POK.actual)
     `RAL_FIELD_CP_16(IP_SIDE_POK, IP_SIDE_POK.actual, 0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)
     `RAL_FIELD_CP_16(IP_SIDE_POK, IP_SIDE_POK.actual, 16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31)
  endgroup
  `endif

  // --------------------------
  // add constraint based on database legal values
  // --------------------------
  function void build();
    IP_SIDE_POK = new("IP_SIDE_POK", "RO/V", 32, 0, {"IP_SIDE_POK_STS_0.IP_SIDE_POK"});
    IP_SIDE_POK.set_powerwell("primary");
    IP_SIDE_POK.set_rand_mode(0);
    void'(add_field( IP_SIDE_POK ));

  endfunction


`DEFINE_NONE_SCOPE_REG_API(pmu_mmr_IP_SIDE_POK_STS_0_reg) 
endclass : pmu_mmr_IP_SIDE_POK_STS_0_reg

// ================================================

class pmu_mmr_IP_SIDE_POK_STS_1_reg extends sla_ral_reg;

  // --------------------------
  sla_ral_field IP_SIDE_POK;

  // --------------------------
  `ovm_object_utils(pmu_mmr_IP_SIDE_POK_STS_1_reg)

  // --------------------------
  function new(
                string         name = "",
                `ifdef SLA_RAL_COVERAGE
                sla_ral_coverage_t cov_t = COVERAGE_OFF,
                `endif
                int            bus_num=0,
                int            dev_num=0,
                int            func_num=0,
                int            offset=0,
                int            size=0,
                sla_ral_data_t reset_val=0,
                boolean_t      mon_enabled = SLA_FALSE
             );
      super.new();
      set_space_addr("CFG",undef);
      set_space_addr("MEM",undef);
      set_space_addr("IO",undef);
      set_space_addr("MSG",undef);
      set_space_addr("CREG",undef); 
      set_space_addr("LT_MEM",undef);    
 build();

      `ifdef SLA_RAL_COVERAGE
      `CONFIG_RAL_COVERAGE(regname, COVERAGE_ON, FD_RW_SAMPLE)
      if(_coverage_type != COVERAGE_OFF) begin
        if(_sample_type inside {FD_RO_SAMPLE, RO_SAMPLE})
        `CREATE_OP_SAMPLE_COV(RAL_WRITE)
        else if(_sample_type inside {FD_WO_SAMPLE, WO_SAMPLE})
        `CREATE_OP_SAMPLE_COV(RAL_READ)
        else
        `CREATE_OP_SAMPLE_COV(RAL_NONE)
        `CREATE_DESIRED_COV
        `CREATE_ACTUAL_COV
      end
      `endif

  endfunction

  `ifdef SLA_RAL_COVERAGE
  `CREATE_RAL_OP_COVERGROUP
  covergroup desired_cg @(cov_ev);
     option.per_instance = 1;
     option.name = {sla_ral_file::regfilename,".",regname};
     `RAL_FIELD_CP(IP_SIDE_POK, IP_SIDE_POK.desired)
     `RAL_FIELD_CP_16(IP_SIDE_POK, IP_SIDE_POK.desired, 0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)
     `RAL_FIELD_CP_16(IP_SIDE_POK, IP_SIDE_POK.desired, 16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31)
  endgroup
  covergroup actual_cg @(cov_ev);
     option.per_instance = 1;
     option.name = {sla_ral_file::regfilename,".",regname};
     `RAL_FIELD_CP(IP_SIDE_POK, IP_SIDE_POK.actual)
     `RAL_FIELD_CP_16(IP_SIDE_POK, IP_SIDE_POK.actual, 0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)
     `RAL_FIELD_CP_16(IP_SIDE_POK, IP_SIDE_POK.actual, 16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31)
  endgroup
  `endif

  // --------------------------
  // add constraint based on database legal values
  // --------------------------
  function void build();
    IP_SIDE_POK = new("IP_SIDE_POK", "RO/V", 32, 0, {"IP_SIDE_POK_STS_1.IP_SIDE_POK"});
    IP_SIDE_POK.set_powerwell("primary");
    IP_SIDE_POK.set_rand_mode(0);
    void'(add_field( IP_SIDE_POK ));

  endfunction


`DEFINE_NONE_SCOPE_REG_API(pmu_mmr_IP_SIDE_POK_STS_1_reg) 
endclass : pmu_mmr_IP_SIDE_POK_STS_1_reg

// ================================================

class pmu_mmr_IP_PRIM_POK_STS_0_reg extends sla_ral_reg;

  // --------------------------
  sla_ral_field IP_PRIM_POK;

  // --------------------------
  `ovm_object_utils(pmu_mmr_IP_PRIM_POK_STS_0_reg)

  // --------------------------
  function new(
                string         name = "",
                `ifdef SLA_RAL_COVERAGE
                sla_ral_coverage_t cov_t = COVERAGE_OFF,
                `endif
                int            bus_num=0,
                int            dev_num=0,
                int            func_num=0,
                int            offset=0,
                int            size=0,
                sla_ral_data_t reset_val=0,
                boolean_t      mon_enabled = SLA_FALSE
             );
      super.new();
      set_space_addr("CFG",undef);
      set_space_addr("MEM",undef);
      set_space_addr("IO",undef);
      set_space_addr("MSG",undef);
      set_space_addr("CREG",undef); 
      set_space_addr("LT_MEM",undef);    
 build();

      `ifdef SLA_RAL_COVERAGE
      `CONFIG_RAL_COVERAGE(regname, COVERAGE_ON, FD_RW_SAMPLE)
      if(_coverage_type != COVERAGE_OFF) begin
        if(_sample_type inside {FD_RO_SAMPLE, RO_SAMPLE})
        `CREATE_OP_SAMPLE_COV(RAL_WRITE)
        else if(_sample_type inside {FD_WO_SAMPLE, WO_SAMPLE})
        `CREATE_OP_SAMPLE_COV(RAL_READ)
        else
        `CREATE_OP_SAMPLE_COV(RAL_NONE)
        `CREATE_DESIRED_COV
        `CREATE_ACTUAL_COV
      end
      `endif

  endfunction

  `ifdef SLA_RAL_COVERAGE
  `CREATE_RAL_OP_COVERGROUP
  covergroup desired_cg @(cov_ev);
     option.per_instance = 1;
     option.name = {sla_ral_file::regfilename,".",regname};
     `RAL_FIELD_CP(IP_PRIM_POK, IP_PRIM_POK.desired)
     `RAL_FIELD_CP_16(IP_PRIM_POK, IP_PRIM_POK.desired, 0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)
     `RAL_FIELD_CP_16(IP_PRIM_POK, IP_PRIM_POK.desired, 16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31)
  endgroup
  covergroup actual_cg @(cov_ev);
     option.per_instance = 1;
     option.name = {sla_ral_file::regfilename,".",regname};
     `RAL_FIELD_CP(IP_PRIM_POK, IP_PRIM_POK.actual)
     `RAL_FIELD_CP_16(IP_PRIM_POK, IP_PRIM_POK.actual, 0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)
     `RAL_FIELD_CP_16(IP_PRIM_POK, IP_PRIM_POK.actual, 16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31)
  endgroup
  `endif

  // --------------------------
  // add constraint based on database legal values
  // --------------------------
  function void build();
    IP_PRIM_POK = new("IP_PRIM_POK", "RO/V", 32, 0, {"IP_PRIM_POK_STS_0.IP_PRIM_POK"});
    IP_PRIM_POK.set_powerwell("primary");
    IP_PRIM_POK.set_rand_mode(0);
    void'(add_field( IP_PRIM_POK ));

  endfunction


`DEFINE_NONE_SCOPE_REG_API(pmu_mmr_IP_PRIM_POK_STS_0_reg) 
endclass : pmu_mmr_IP_PRIM_POK_STS_0_reg

// ================================================

class pmu_mmr_IP_PRIM_POK_STS_1_reg extends sla_ral_reg;

  // --------------------------
  sla_ral_field IP_PRIM_POK;

  // --------------------------
  `ovm_object_utils(pmu_mmr_IP_PRIM_POK_STS_1_reg)

  // --------------------------
  function new(
                string         name = "",
                `ifdef SLA_RAL_COVERAGE
                sla_ral_coverage_t cov_t = COVERAGE_OFF,
                `endif
                int            bus_num=0,
                int            dev_num=0,
                int            func_num=0,
                int            offset=0,
                int            size=0,
                sla_ral_data_t reset_val=0,
                boolean_t      mon_enabled = SLA_FALSE
             );
      super.new();
      set_space_addr("CFG",undef);
      set_space_addr("MEM",undef);
      set_space_addr("IO",undef);
      set_space_addr("MSG",undef);
      set_space_addr("CREG",undef); 
      set_space_addr("LT_MEM",undef);    
 build();

      `ifdef SLA_RAL_COVERAGE
      `CONFIG_RAL_COVERAGE(regname, COVERAGE_ON, FD_RW_SAMPLE)
      if(_coverage_type != COVERAGE_OFF) begin
        if(_sample_type inside {FD_RO_SAMPLE, RO_SAMPLE})
        `CREATE_OP_SAMPLE_COV(RAL_WRITE)
        else if(_sample_type inside {FD_WO_SAMPLE, WO_SAMPLE})
        `CREATE_OP_SAMPLE_COV(RAL_READ)
        else
        `CREATE_OP_SAMPLE_COV(RAL_NONE)
        `CREATE_DESIRED_COV
        `CREATE_ACTUAL_COV
      end
      `endif

  endfunction

  `ifdef SLA_RAL_COVERAGE
  `CREATE_RAL_OP_COVERGROUP
  covergroup desired_cg @(cov_ev);
     option.per_instance = 1;
     option.name = {sla_ral_file::regfilename,".",regname};
     `RAL_FIELD_CP(IP_PRIM_POK, IP_PRIM_POK.desired)
     `RAL_FIELD_CP_16(IP_PRIM_POK, IP_PRIM_POK.desired, 0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)
     `RAL_FIELD_CP_16(IP_PRIM_POK, IP_PRIM_POK.desired, 16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31)
  endgroup
  covergroup actual_cg @(cov_ev);
     option.per_instance = 1;
     option.name = {sla_ral_file::regfilename,".",regname};
     `RAL_FIELD_CP(IP_PRIM_POK, IP_PRIM_POK.actual)
     `RAL_FIELD_CP_16(IP_PRIM_POK, IP_PRIM_POK.actual, 0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)
     `RAL_FIELD_CP_16(IP_PRIM_POK, IP_PRIM_POK.actual, 16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31)
  endgroup
  `endif

  // --------------------------
  // add constraint based on database legal values
  // --------------------------
  function void build();
    IP_PRIM_POK = new("IP_PRIM_POK", "RO/V", 32, 0, {"IP_PRIM_POK_STS_1.IP_PRIM_POK"});
    IP_PRIM_POK.set_powerwell("primary");
    IP_PRIM_POK.set_rand_mode(0);
    void'(add_field( IP_PRIM_POK ));

  endfunction


`DEFINE_NONE_SCOPE_REG_API(pmu_mmr_IP_PRIM_POK_STS_1_reg) 
endclass : pmu_mmr_IP_PRIM_POK_STS_1_reg

// ================================================

class pmu_mmr_IP_READY_STS_0_reg extends sla_ral_reg;

  // --------------------------
  sla_ral_field IP_READY_STS_0_0;
  sla_ral_field IP_READY_STS_0_1;
  sla_ral_field IP_READY_STS_0_2;
  sla_ral_field IP_READY_STS_0_3;
  sla_ral_field IP_READY_STS_0_4;
  sla_ral_field IP_READY_STS_0_5;
  sla_ral_field IP_READY_STS_0_6;
  sla_ral_field IP_READY_STS_0_7;
  sla_ral_field IP_READY_STS_0_8;
  sla_ral_field IP_READY_STS_0_9;
  sla_ral_field IP_READY_STS_0_10;
  sla_ral_field IP_READY_STS_0_11;
  sla_ral_field IP_READY_STS_0_12;
  sla_ral_field IP_READY_STS_0_13;
  sla_ral_field IP_READY_STS_0_14;
  sla_ral_field IP_READY_STS_0_15;
  sla_ral_field IP_READY_STS_0_16;
  sla_ral_field IP_READY_STS_0_17;
  sla_ral_field IP_READY_STS_0_18;
  sla_ral_field IP_READY_STS_0_19;
  sla_ral_field IP_READY_STS_0_20;
  sla_ral_field IP_READY_STS_0_21;
  sla_ral_field IP_READY_STS_0_22;
  sla_ral_field IP_READY_STS_0_23;
  sla_ral_field IP_READY_STS_0_24;
  sla_ral_field IP_READY_STS_0_25;
  sla_ral_field IP_READY_STS_0_26;
  sla_ral_field IP_READY_STS_0_27;
  sla_ral_field IP_READY_STS_0_28;
  sla_ral_field IP_READY_STS_0_29;
  sla_ral_field IP_READY_STS_0_30;
  sla_ral_field IP_READY_STS_0_31;

  // --------------------------
  `ovm_object_utils(pmu_mmr_IP_READY_STS_0_reg)

  // --------------------------
  function new(
                string         name = "",
                `ifdef SLA_RAL_COVERAGE
                sla_ral_coverage_t cov_t = COVERAGE_OFF,
                `endif
                int            bus_num=0,
                int            dev_num=0,
                int            func_num=0,
                int            offset=0,
                int            size=0,
                sla_ral_data_t reset_val=0,
                boolean_t      mon_enabled = SLA_FALSE
             );
      super.new();
      set_space_addr("CFG",undef);
      set_space_addr("MEM",undef);
      set_space_addr("IO",undef);
      set_space_addr("MSG",undef);
      set_space_addr("CREG",undef); 
      set_space_addr("LT_MEM",undef);    
 build();

      `ifdef SLA_RAL_COVERAGE
      `CONFIG_RAL_COVERAGE(regname, COVERAGE_ON, FD_RW_SAMPLE)
      if(_coverage_type != COVERAGE_OFF) begin
        if(_sample_type inside {FD_RO_SAMPLE, RO_SAMPLE})
        `CREATE_OP_SAMPLE_COV(RAL_WRITE)
        else if(_sample_type inside {FD_WO_SAMPLE, WO_SAMPLE})
        `CREATE_OP_SAMPLE_COV(RAL_READ)
        else
        `CREATE_OP_SAMPLE_COV(RAL_NONE)
        `CREATE_DESIRED_COV
        `CREATE_ACTUAL_COV
      end
      `endif

  endfunction

  `ifdef SLA_RAL_COVERAGE
  `CREATE_RAL_OP_COVERGROUP
  covergroup desired_cg @(cov_ev);
     option.per_instance = 1;
     option.name = {sla_ral_file::regfilename,".",regname};
     `RAL_FIELD_CP(IP_READY_STS_0_0, IP_READY_STS_0_0.desired)
     `RAL_FIELD_CP_1(IP_READY_STS_0_0, IP_READY_STS_0_0.desired, 0)
     `RAL_FIELD_CP(IP_READY_STS_0_1, IP_READY_STS_0_1.desired)
     `RAL_FIELD_CP_1(IP_READY_STS_0_1, IP_READY_STS_0_1.desired, 0)
     `RAL_FIELD_CP(IP_READY_STS_0_2, IP_READY_STS_0_2.desired)
     `RAL_FIELD_CP_1(IP_READY_STS_0_2, IP_READY_STS_0_2.desired, 0)
     `RAL_FIELD_CP(IP_READY_STS_0_3, IP_READY_STS_0_3.desired)
     `RAL_FIELD_CP_1(IP_READY_STS_0_3, IP_READY_STS_0_3.desired, 0)
     `RAL_FIELD_CP(IP_READY_STS_0_4, IP_READY_STS_0_4.desired)
     `RAL_FIELD_CP_1(IP_READY_STS_0_4, IP_READY_STS_0_4.desired, 0)
     `RAL_FIELD_CP(IP_READY_STS_0_5, IP_READY_STS_0_5.desired)
     `RAL_FIELD_CP_1(IP_READY_STS_0_5, IP_READY_STS_0_5.desired, 0)
     `RAL_FIELD_CP(IP_READY_STS_0_6, IP_READY_STS_0_6.desired)
     `RAL_FIELD_CP_1(IP_READY_STS_0_6, IP_READY_STS_0_6.desired, 0)
     `RAL_FIELD_CP(IP_READY_STS_0_7, IP_READY_STS_0_7.desired)
     `RAL_FIELD_CP_1(IP_READY_STS_0_7, IP_READY_STS_0_7.desired, 0)
     `RAL_FIELD_CP(IP_READY_STS_0_8, IP_READY_STS_0_8.desired)
     `RAL_FIELD_CP_1(IP_READY_STS_0_8, IP_READY_STS_0_8.desired, 0)
     `RAL_FIELD_CP(IP_READY_STS_0_9, IP_READY_STS_0_9.desired)
     `RAL_FIELD_CP_1(IP_READY_STS_0_9, IP_READY_STS_0_9.desired, 0)
     `RAL_FIELD_CP(IP_READY_STS_0_10, IP_READY_STS_0_10.desired)
     `RAL_FIELD_CP_1(IP_READY_STS_0_10, IP_READY_STS_0_10.desired, 0)
     `RAL_FIELD_CP(IP_READY_STS_0_11, IP_READY_STS_0_11.desired)
     `RAL_FIELD_CP_1(IP_READY_STS_0_11, IP_READY_STS_0_11.desired, 0)
     `RAL_FIELD_CP(IP_READY_STS_0_12, IP_READY_STS_0_12.desired)
     `RAL_FIELD_CP_1(IP_READY_STS_0_12, IP_READY_STS_0_12.desired, 0)
     `RAL_FIELD_CP(IP_READY_STS_0_13, IP_READY_STS_0_13.desired)
     `RAL_FIELD_CP_1(IP_READY_STS_0_13, IP_READY_STS_0_13.desired, 0)
     `RAL_FIELD_CP(IP_READY_STS_0_14, IP_READY_STS_0_14.desired)
     `RAL_FIELD_CP_1(IP_READY_STS_0_14, IP_READY_STS_0_14.desired, 0)
     `RAL_FIELD_CP(IP_READY_STS_0_15, IP_READY_STS_0_15.desired)
     `RAL_FIELD_CP_1(IP_READY_STS_0_15, IP_READY_STS_0_15.desired, 0)
     `RAL_FIELD_CP(IP_READY_STS_0_16, IP_READY_STS_0_16.desired)
     `RAL_FIELD_CP_1(IP_READY_STS_0_16, IP_READY_STS_0_16.desired, 0)
     `RAL_FIELD_CP(IP_READY_STS_0_17, IP_READY_STS_0_17.desired)
     `RAL_FIELD_CP_1(IP_READY_STS_0_17, IP_READY_STS_0_17.desired, 0)
     `RAL_FIELD_CP(IP_READY_STS_0_18, IP_READY_STS_0_18.desired)
     `RAL_FIELD_CP_1(IP_READY_STS_0_18, IP_READY_STS_0_18.desired, 0)
     `RAL_FIELD_CP(IP_READY_STS_0_19, IP_READY_STS_0_19.desired)
     `RAL_FIELD_CP_1(IP_READY_STS_0_19, IP_READY_STS_0_19.desired, 0)
     `RAL_FIELD_CP(IP_READY_STS_0_20, IP_READY_STS_0_20.desired)
     `RAL_FIELD_CP_1(IP_READY_STS_0_20, IP_READY_STS_0_20.desired, 0)
     `RAL_FIELD_CP(IP_READY_STS_0_21, IP_READY_STS_0_21.desired)
     `RAL_FIELD_CP_1(IP_READY_STS_0_21, IP_READY_STS_0_21.desired, 0)
     `RAL_FIELD_CP(IP_READY_STS_0_22, IP_READY_STS_0_22.desired)
     `RAL_FIELD_CP_1(IP_READY_STS_0_22, IP_READY_STS_0_22.desired, 0)
     `RAL_FIELD_CP(IP_READY_STS_0_23, IP_READY_STS_0_23.desired)
     `RAL_FIELD_CP_1(IP_READY_STS_0_23, IP_READY_STS_0_23.desired, 0)
     `RAL_FIELD_CP(IP_READY_STS_0_24, IP_READY_STS_0_24.desired)
     `RAL_FIELD_CP_1(IP_READY_STS_0_24, IP_READY_STS_0_24.desired, 0)
     `RAL_FIELD_CP(IP_READY_STS_0_25, IP_READY_STS_0_25.desired)
     `RAL_FIELD_CP_1(IP_READY_STS_0_25, IP_READY_STS_0_25.desired, 0)
     `RAL_FIELD_CP(IP_READY_STS_0_26, IP_READY_STS_0_26.desired)
     `RAL_FIELD_CP_1(IP_READY_STS_0_26, IP_READY_STS_0_26.desired, 0)
     `RAL_FIELD_CP(IP_READY_STS_0_27, IP_READY_STS_0_27.desired)
     `RAL_FIELD_CP_1(IP_READY_STS_0_27, IP_READY_STS_0_27.desired, 0)
     `RAL_FIELD_CP(IP_READY_STS_0_28, IP_READY_STS_0_28.desired)
     `RAL_FIELD_CP_1(IP_READY_STS_0_28, IP_READY_STS_0_28.desired, 0)
     `RAL_FIELD_CP(IP_READY_STS_0_29, IP_READY_STS_0_29.desired)
     `RAL_FIELD_CP_1(IP_READY_STS_0_29, IP_READY_STS_0_29.desired, 0)
     `RAL_FIELD_CP(IP_READY_STS_0_30, IP_READY_STS_0_30.desired)
     `RAL_FIELD_CP_1(IP_READY_STS_0_30, IP_READY_STS_0_30.desired, 0)
     `RAL_FIELD_CP(IP_READY_STS_0_31, IP_READY_STS_0_31.desired)
     `RAL_FIELD_CP_1(IP_READY_STS_0_31, IP_READY_STS_0_31.desired, 0)
  endgroup
  covergroup actual_cg @(cov_ev);
     option.per_instance = 1;
     option.name = {sla_ral_file::regfilename,".",regname};
     `RAL_FIELD_CP(IP_READY_STS_0_0, IP_READY_STS_0_0.actual)
     `RAL_FIELD_CP_1(IP_READY_STS_0_0, IP_READY_STS_0_0.actual, 0)
     `RAL_FIELD_CP(IP_READY_STS_0_1, IP_READY_STS_0_1.actual)
     `RAL_FIELD_CP_1(IP_READY_STS_0_1, IP_READY_STS_0_1.actual, 0)
     `RAL_FIELD_CP(IP_READY_STS_0_2, IP_READY_STS_0_2.actual)
     `RAL_FIELD_CP_1(IP_READY_STS_0_2, IP_READY_STS_0_2.actual, 0)
     `RAL_FIELD_CP(IP_READY_STS_0_3, IP_READY_STS_0_3.actual)
     `RAL_FIELD_CP_1(IP_READY_STS_0_3, IP_READY_STS_0_3.actual, 0)
     `RAL_FIELD_CP(IP_READY_STS_0_4, IP_READY_STS_0_4.actual)
     `RAL_FIELD_CP_1(IP_READY_STS_0_4, IP_READY_STS_0_4.actual, 0)
     `RAL_FIELD_CP(IP_READY_STS_0_5, IP_READY_STS_0_5.actual)
     `RAL_FIELD_CP_1(IP_READY_STS_0_5, IP_READY_STS_0_5.actual, 0)
     `RAL_FIELD_CP(IP_READY_STS_0_6, IP_READY_STS_0_6.actual)
     `RAL_FIELD_CP_1(IP_READY_STS_0_6, IP_READY_STS_0_6.actual, 0)
     `RAL_FIELD_CP(IP_READY_STS_0_7, IP_READY_STS_0_7.actual)
     `RAL_FIELD_CP_1(IP_READY_STS_0_7, IP_READY_STS_0_7.actual, 0)
     `RAL_FIELD_CP(IP_READY_STS_0_8, IP_READY_STS_0_8.actual)
     `RAL_FIELD_CP_1(IP_READY_STS_0_8, IP_READY_STS_0_8.actual, 0)
     `RAL_FIELD_CP(IP_READY_STS_0_9, IP_READY_STS_0_9.actual)
     `RAL_FIELD_CP_1(IP_READY_STS_0_9, IP_READY_STS_0_9.actual, 0)
     `RAL_FIELD_CP(IP_READY_STS_0_10, IP_READY_STS_0_10.actual)
     `RAL_FIELD_CP_1(IP_READY_STS_0_10, IP_READY_STS_0_10.actual, 0)
     `RAL_FIELD_CP(IP_READY_STS_0_11, IP_READY_STS_0_11.actual)
     `RAL_FIELD_CP_1(IP_READY_STS_0_11, IP_READY_STS_0_11.actual, 0)
     `RAL_FIELD_CP(IP_READY_STS_0_12, IP_READY_STS_0_12.actual)
     `RAL_FIELD_CP_1(IP_READY_STS_0_12, IP_READY_STS_0_12.actual, 0)
     `RAL_FIELD_CP(IP_READY_STS_0_13, IP_READY_STS_0_13.actual)
     `RAL_FIELD_CP_1(IP_READY_STS_0_13, IP_READY_STS_0_13.actual, 0)
     `RAL_FIELD_CP(IP_READY_STS_0_14, IP_READY_STS_0_14.actual)
     `RAL_FIELD_CP_1(IP_READY_STS_0_14, IP_READY_STS_0_14.actual, 0)
     `RAL_FIELD_CP(IP_READY_STS_0_15, IP_READY_STS_0_15.actual)
     `RAL_FIELD_CP_1(IP_READY_STS_0_15, IP_READY_STS_0_15.actual, 0)
     `RAL_FIELD_CP(IP_READY_STS_0_16, IP_READY_STS_0_16.actual)
     `RAL_FIELD_CP_1(IP_READY_STS_0_16, IP_READY_STS_0_16.actual, 0)
     `RAL_FIELD_CP(IP_READY_STS_0_17, IP_READY_STS_0_17.actual)
     `RAL_FIELD_CP_1(IP_READY_STS_0_17, IP_READY_STS_0_17.actual, 0)
     `RAL_FIELD_CP(IP_READY_STS_0_18, IP_READY_STS_0_18.actual)
     `RAL_FIELD_CP_1(IP_READY_STS_0_18, IP_READY_STS_0_18.actual, 0)
     `RAL_FIELD_CP(IP_READY_STS_0_19, IP_READY_STS_0_19.actual)
     `RAL_FIELD_CP_1(IP_READY_STS_0_19, IP_READY_STS_0_19.actual, 0)
     `RAL_FIELD_CP(IP_READY_STS_0_20, IP_READY_STS_0_20.actual)
     `RAL_FIELD_CP_1(IP_READY_STS_0_20, IP_READY_STS_0_20.actual, 0)
     `RAL_FIELD_CP(IP_READY_STS_0_21, IP_READY_STS_0_21.actual)
     `RAL_FIELD_CP_1(IP_READY_STS_0_21, IP_READY_STS_0_21.actual, 0)
     `RAL_FIELD_CP(IP_READY_STS_0_22, IP_READY_STS_0_22.actual)
     `RAL_FIELD_CP_1(IP_READY_STS_0_22, IP_READY_STS_0_22.actual, 0)
     `RAL_FIELD_CP(IP_READY_STS_0_23, IP_READY_STS_0_23.actual)
     `RAL_FIELD_CP_1(IP_READY_STS_0_23, IP_READY_STS_0_23.actual, 0)
     `RAL_FIELD_CP(IP_READY_STS_0_24, IP_READY_STS_0_24.actual)
     `RAL_FIELD_CP_1(IP_READY_STS_0_24, IP_READY_STS_0_24.actual, 0)
     `RAL_FIELD_CP(IP_READY_STS_0_25, IP_READY_STS_0_25.actual)
     `RAL_FIELD_CP_1(IP_READY_STS_0_25, IP_READY_STS_0_25.actual, 0)
     `RAL_FIELD_CP(IP_READY_STS_0_26, IP_READY_STS_0_26.actual)
     `RAL_FIELD_CP_1(IP_READY_STS_0_26, IP_READY_STS_0_26.actual, 0)
     `RAL_FIELD_CP(IP_READY_STS_0_27, IP_READY_STS_0_27.actual)
     `RAL_FIELD_CP_1(IP_READY_STS_0_27, IP_READY_STS_0_27.actual, 0)
     `RAL_FIELD_CP(IP_READY_STS_0_28, IP_READY_STS_0_28.actual)
     `RAL_FIELD_CP_1(IP_READY_STS_0_28, IP_READY_STS_0_28.actual, 0)
     `RAL_FIELD_CP(IP_READY_STS_0_29, IP_READY_STS_0_29.actual)
     `RAL_FIELD_CP_1(IP_READY_STS_0_29, IP_READY_STS_0_29.actual, 0)
     `RAL_FIELD_CP(IP_READY_STS_0_30, IP_READY_STS_0_30.actual)
     `RAL_FIELD_CP_1(IP_READY_STS_0_30, IP_READY_STS_0_30.actual, 0)
     `RAL_FIELD_CP(IP_READY_STS_0_31, IP_READY_STS_0_31.actual)
     `RAL_FIELD_CP_1(IP_READY_STS_0_31, IP_READY_STS_0_31.actual, 0)
  endgroup
  `endif

  // --------------------------
  // add constraint based on database legal values
  // --------------------------
  function void build();
    IP_READY_STS_0_0 = new("IP_READY_STS_0_0", "RW/1C/V", 1, 0, {"IP_READY_STS_0.IP_READY_STS_0_0"});
    IP_READY_STS_0_0.set_powerwell("primary");
    IP_READY_STS_0_0.set_rand_mode(0);
   IP_READY_STS_0_0.set_reset_signame("pmu_rst_b");
    void'(add_field( IP_READY_STS_0_0 ));

    IP_READY_STS_0_1 = new("IP_READY_STS_0_1", "RW/1C/V", 1, 1, {"IP_READY_STS_0.IP_READY_STS_0_1"});
    IP_READY_STS_0_1.set_powerwell("primary");
    IP_READY_STS_0_1.set_rand_mode(0);
   IP_READY_STS_0_1.set_reset_signame("pmu_rst_b");
    void'(add_field( IP_READY_STS_0_1 ));

    IP_READY_STS_0_2 = new("IP_READY_STS_0_2", "RW/1C/V", 1, 2, {"IP_READY_STS_0.IP_READY_STS_0_2"});
    IP_READY_STS_0_2.set_powerwell("primary");
    IP_READY_STS_0_2.set_rand_mode(0);
   IP_READY_STS_0_2.set_reset_signame("pmu_rst_b");
    void'(add_field( IP_READY_STS_0_2 ));

    IP_READY_STS_0_3 = new("IP_READY_STS_0_3", "RW/1C/V", 1, 3, {"IP_READY_STS_0.IP_READY_STS_0_3"});
    IP_READY_STS_0_3.set_powerwell("primary");
    IP_READY_STS_0_3.set_rand_mode(0);
   IP_READY_STS_0_3.set_reset_signame("pmu_rst_b");
    void'(add_field( IP_READY_STS_0_3 ));

    IP_READY_STS_0_4 = new("IP_READY_STS_0_4", "RW/1C/V", 1, 4, {"IP_READY_STS_0.IP_READY_STS_0_4"});
    IP_READY_STS_0_4.set_powerwell("primary");
    IP_READY_STS_0_4.set_rand_mode(0);
   IP_READY_STS_0_4.set_reset_signame("pmu_rst_b");
    void'(add_field( IP_READY_STS_0_4 ));

    IP_READY_STS_0_5 = new("IP_READY_STS_0_5", "RW/1C/V", 1, 5, {"IP_READY_STS_0.IP_READY_STS_0_5"});
    IP_READY_STS_0_5.set_powerwell("primary");
    IP_READY_STS_0_5.set_rand_mode(0);
   IP_READY_STS_0_5.set_reset_signame("pmu_rst_b");
    void'(add_field( IP_READY_STS_0_5 ));

    IP_READY_STS_0_6 = new("IP_READY_STS_0_6", "RW/1C/V", 1, 6, {"IP_READY_STS_0.IP_READY_STS_0_6"});
    IP_READY_STS_0_6.set_powerwell("primary");
    IP_READY_STS_0_6.set_rand_mode(0);
   IP_READY_STS_0_6.set_reset_signame("pmu_rst_b");
    void'(add_field( IP_READY_STS_0_6 ));

    IP_READY_STS_0_7 = new("IP_READY_STS_0_7", "RW/1C/V", 1, 7, {"IP_READY_STS_0.IP_READY_STS_0_7"});
    IP_READY_STS_0_7.set_powerwell("primary");
    IP_READY_STS_0_7.set_rand_mode(0);
   IP_READY_STS_0_7.set_reset_signame("pmu_rst_b");
    void'(add_field( IP_READY_STS_0_7 ));

    IP_READY_STS_0_8 = new("IP_READY_STS_0_8", "RW/1C/V", 1, 8, {"IP_READY_STS_0.IP_READY_STS_0_8"});
    IP_READY_STS_0_8.set_powerwell("primary");
    IP_READY_STS_0_8.set_rand_mode(0);
   IP_READY_STS_0_8.set_reset_signame("pmu_rst_b");
    void'(add_field( IP_READY_STS_0_8 ));

    IP_READY_STS_0_9 = new("IP_READY_STS_0_9", "RW/1C/V", 1, 9, {"IP_READY_STS_0.IP_READY_STS_0_9"});
    IP_READY_STS_0_9.set_powerwell("primary");
    IP_READY_STS_0_9.set_rand_mode(0);
   IP_READY_STS_0_9.set_reset_signame("pmu_rst_b");
    void'(add_field( IP_READY_STS_0_9 ));

    IP_READY_STS_0_10 = new("IP_READY_STS_0_10", "RW/1C/V", 1, 10, {"IP_READY_STS_0.IP_READY_STS_0_10"});
    IP_READY_STS_0_10.set_powerwell("primary");
    IP_READY_STS_0_10.set_rand_mode(0);
   IP_READY_STS_0_10.set_reset_signame("pmu_rst_b");
    void'(add_field( IP_READY_STS_0_10 ));

    IP_READY_STS_0_11 = new("IP_READY_STS_0_11", "RW/1C/V", 1, 11, {"IP_READY_STS_0.IP_READY_STS_0_11"});
    IP_READY_STS_0_11.set_powerwell("primary");
    IP_READY_STS_0_11.set_rand_mode(0);
   IP_READY_STS_0_11.set_reset_signame("pmu_rst_b");
    void'(add_field( IP_READY_STS_0_11 ));

    IP_READY_STS_0_12 = new("IP_READY_STS_0_12", "RW/1C/V", 1, 12, {"IP_READY_STS_0.IP_READY_STS_0_12"});
    IP_READY_STS_0_12.set_powerwell("primary");
    IP_READY_STS_0_12.set_rand_mode(0);
   IP_READY_STS_0_12.set_reset_signame("pmu_rst_b");
    void'(add_field( IP_READY_STS_0_12 ));

    IP_READY_STS_0_13 = new("IP_READY_STS_0_13", "RW/1C/V", 1, 13, {"IP_READY_STS_0.IP_READY_STS_0_13"});
    IP_READY_STS_0_13.set_powerwell("primary");
    IP_READY_STS_0_13.set_rand_mode(0);
   IP_READY_STS_0_13.set_reset_signame("pmu_rst_b");
    void'(add_field( IP_READY_STS_0_13 ));

    IP_READY_STS_0_14 = new("IP_READY_STS_0_14", "RW/1C/V", 1, 14, {"IP_READY_STS_0.IP_READY_STS_0_14"});
    IP_READY_STS_0_14.set_powerwell("primary");
    IP_READY_STS_0_14.set_rand_mode(0);
   IP_READY_STS_0_14.set_reset_signame("pmu_rst_b");
    void'(add_field( IP_READY_STS_0_14 ));

    IP_READY_STS_0_15 = new("IP_READY_STS_0_15", "RW/1C/V", 1, 15, {"IP_READY_STS_0.IP_READY_STS_0_15"});
    IP_READY_STS_0_15.set_powerwell("primary");
    IP_READY_STS_0_15.set_rand_mode(0);
   IP_READY_STS_0_15.set_reset_signame("pmu_rst_b");
    void'(add_field( IP_READY_STS_0_15 ));

    IP_READY_STS_0_16 = new("IP_READY_STS_0_16", "RW/1C/V", 1, 16, {"IP_READY_STS_0.IP_READY_STS_0_16"});
    IP_READY_STS_0_16.set_powerwell("primary");
    IP_READY_STS_0_16.set_rand_mode(0);
   IP_READY_STS_0_16.set_reset_signame("pmu_rst_b");
    void'(add_field( IP_READY_STS_0_16 ));

    IP_READY_STS_0_17 = new("IP_READY_STS_0_17", "RW/1C/V", 1, 17, {"IP_READY_STS_0.IP_READY_STS_0_17"});
    IP_READY_STS_0_17.set_powerwell("primary");
    IP_READY_STS_0_17.set_rand_mode(0);
   IP_READY_STS_0_17.set_reset_signame("pmu_rst_b");
    void'(add_field( IP_READY_STS_0_17 ));

    IP_READY_STS_0_18 = new("IP_READY_STS_0_18", "RW/1C/V", 1, 18, {"IP_READY_STS_0.IP_READY_STS_0_18"});
    IP_READY_STS_0_18.set_powerwell("primary");
    IP_READY_STS_0_18.set_rand_mode(0);
   IP_READY_STS_0_18.set_reset_signame("pmu_rst_b");
    void'(add_field( IP_READY_STS_0_18 ));

    IP_READY_STS_0_19 = new("IP_READY_STS_0_19", "RW/1C/V", 1, 19, {"IP_READY_STS_0.IP_READY_STS_0_19"});
    IP_READY_STS_0_19.set_powerwell("primary");
    IP_READY_STS_0_19.set_rand_mode(0);
   IP_READY_STS_0_19.set_reset_signame("pmu_rst_b");
    void'(add_field( IP_READY_STS_0_19 ));

    IP_READY_STS_0_20 = new("IP_READY_STS_0_20", "RW/1C/V", 1, 20, {"IP_READY_STS_0.IP_READY_STS_0_20"});
    IP_READY_STS_0_20.set_powerwell("primary");
    IP_READY_STS_0_20.set_rand_mode(0);
   IP_READY_STS_0_20.set_reset_signame("pmu_rst_b");
    void'(add_field( IP_READY_STS_0_20 ));

    IP_READY_STS_0_21 = new("IP_READY_STS_0_21", "RW/1C/V", 1, 21, {"IP_READY_STS_0.IP_READY_STS_0_21"});
    IP_READY_STS_0_21.set_powerwell("primary");
    IP_READY_STS_0_21.set_rand_mode(0);
   IP_READY_STS_0_21.set_reset_signame("pmu_rst_b");
    void'(add_field( IP_READY_STS_0_21 ));

    IP_READY_STS_0_22 = new("IP_READY_STS_0_22", "RW/1C/V", 1, 22, {"IP_READY_STS_0.IP_READY_STS_0_22"});
    IP_READY_STS_0_22.set_powerwell("primary");
    IP_READY_STS_0_22.set_rand_mode(0);
   IP_READY_STS_0_22.set_reset_signame("pmu_rst_b");
    void'(add_field( IP_READY_STS_0_22 ));

    IP_READY_STS_0_23 = new("IP_READY_STS_0_23", "RW/1C/V", 1, 23, {"IP_READY_STS_0.IP_READY_STS_0_23"});
    IP_READY_STS_0_23.set_powerwell("primary");
    IP_READY_STS_0_23.set_rand_mode(0);
   IP_READY_STS_0_23.set_reset_signame("pmu_rst_b");
    void'(add_field( IP_READY_STS_0_23 ));

    IP_READY_STS_0_24 = new("IP_READY_STS_0_24", "RW/1C/V", 1, 24, {"IP_READY_STS_0.IP_READY_STS_0_24"});
    IP_READY_STS_0_24.set_powerwell("primary");
    IP_READY_STS_0_24.set_rand_mode(0);
   IP_READY_STS_0_24.set_reset_signame("pmu_rst_b");
    void'(add_field( IP_READY_STS_0_24 ));

    IP_READY_STS_0_25 = new("IP_READY_STS_0_25", "RW/1C/V", 1, 25, {"IP_READY_STS_0.IP_READY_STS_0_25"});
    IP_READY_STS_0_25.set_powerwell("primary");
    IP_READY_STS_0_25.set_rand_mode(0);
   IP_READY_STS_0_25.set_reset_signame("pmu_rst_b");
    void'(add_field( IP_READY_STS_0_25 ));

    IP_READY_STS_0_26 = new("IP_READY_STS_0_26", "RW/1C/V", 1, 26, {"IP_READY_STS_0.IP_READY_STS_0_26"});
    IP_READY_STS_0_26.set_powerwell("primary");
    IP_READY_STS_0_26.set_rand_mode(0);
   IP_READY_STS_0_26.set_reset_signame("pmu_rst_b");
    void'(add_field( IP_READY_STS_0_26 ));

    IP_READY_STS_0_27 = new("IP_READY_STS_0_27", "RW/1C/V", 1, 27, {"IP_READY_STS_0.IP_READY_STS_0_27"});
    IP_READY_STS_0_27.set_powerwell("primary");
    IP_READY_STS_0_27.set_rand_mode(0);
   IP_READY_STS_0_27.set_reset_signame("pmu_rst_b");
    void'(add_field( IP_READY_STS_0_27 ));

    IP_READY_STS_0_28 = new("IP_READY_STS_0_28", "RW/1C/V", 1, 28, {"IP_READY_STS_0.IP_READY_STS_0_28"});
    IP_READY_STS_0_28.set_powerwell("primary");
    IP_READY_STS_0_28.set_rand_mode(0);
   IP_READY_STS_0_28.set_reset_signame("pmu_rst_b");
    void'(add_field( IP_READY_STS_0_28 ));

    IP_READY_STS_0_29 = new("IP_READY_STS_0_29", "RW/1C/V", 1, 29, {"IP_READY_STS_0.IP_READY_STS_0_29"});
    IP_READY_STS_0_29.set_powerwell("primary");
    IP_READY_STS_0_29.set_rand_mode(0);
   IP_READY_STS_0_29.set_reset_signame("pmu_rst_b");
    void'(add_field( IP_READY_STS_0_29 ));

    IP_READY_STS_0_30 = new("IP_READY_STS_0_30", "RW/1C/V", 1, 30, {"IP_READY_STS_0.IP_READY_STS_0_30"});
    IP_READY_STS_0_30.set_powerwell("primary");
    IP_READY_STS_0_30.set_rand_mode(0);
   IP_READY_STS_0_30.set_reset_signame("pmu_rst_b");
    void'(add_field( IP_READY_STS_0_30 ));

    IP_READY_STS_0_31 = new("IP_READY_STS_0_31", "RW/1C/V", 1, 31, {"IP_READY_STS_0.IP_READY_STS_0_31"});
    IP_READY_STS_0_31.set_powerwell("primary");
    IP_READY_STS_0_31.set_rand_mode(0);
   IP_READY_STS_0_31.set_reset_signame("pmu_rst_b");
    void'(add_field( IP_READY_STS_0_31 ));

  endfunction


`DEFINE_NONE_SCOPE_REG_API(pmu_mmr_IP_READY_STS_0_reg) 
endclass : pmu_mmr_IP_READY_STS_0_reg

// ================================================

class pmu_mmr_IP_READY_STS_1_reg extends sla_ral_reg;

  // --------------------------
  sla_ral_field IP_READY_STS_1_0;
  sla_ral_field IP_READY_STS_1_1;
  sla_ral_field IP_READY_STS_1_2;
  sla_ral_field IP_READY_STS_1_3;
  sla_ral_field IP_READY_STS_1_4;
  sla_ral_field IP_READY_STS_1_5;
  sla_ral_field IP_READY_STS_1_6;
  sla_ral_field IP_READY_STS_1_7;
  sla_ral_field IP_READY_STS_1_8;
  sla_ral_field IP_READY_STS_1_9;
  sla_ral_field IP_READY_STS_1_10;
  sla_ral_field IP_READY_STS_1_11;
  sla_ral_field IP_READY_STS_1_12;
  sla_ral_field IP_READY_STS_1_13;
  sla_ral_field IP_READY_STS_1_14;
  sla_ral_field IP_READY_STS_1_15;
  sla_ral_field IP_READY_STS_1_16;
  sla_ral_field IP_READY_STS_1_17;
  sla_ral_field IP_READY_STS_1_18;
  sla_ral_field IP_READY_STS_1_19;
  sla_ral_field IP_READY_STS_1_20;
  sla_ral_field IP_READY_STS_1_21;
  sla_ral_field IP_READY_STS_1_22;
  sla_ral_field IP_READY_STS_1_23;
  sla_ral_field IP_READY_STS_1_24;
  sla_ral_field IP_READY_STS_1_25;
  sla_ral_field IP_READY_STS_1_26;
  sla_ral_field IP_READY_STS_1_27;
  sla_ral_field IP_READY_STS_1_28;
  sla_ral_field IP_READY_STS_1_29;
  sla_ral_field IP_READY_STS_1_30;
  sla_ral_field IP_READY_STS_1_31;

  // --------------------------
  `ovm_object_utils(pmu_mmr_IP_READY_STS_1_reg)

  // --------------------------
  function new(
                string         name = "",
                `ifdef SLA_RAL_COVERAGE
                sla_ral_coverage_t cov_t = COVERAGE_OFF,
                `endif
                int            bus_num=0,
                int            dev_num=0,
                int            func_num=0,
                int            offset=0,
                int            size=0,
                sla_ral_data_t reset_val=0,
                boolean_t      mon_enabled = SLA_FALSE
             );
      super.new();
      set_space_addr("CFG",undef);
      set_space_addr("MEM",undef);
      set_space_addr("IO",undef);
      set_space_addr("MSG",undef);
      set_space_addr("CREG",undef); 
      set_space_addr("LT_MEM",undef);    
 build();

      `ifdef SLA_RAL_COVERAGE
      `CONFIG_RAL_COVERAGE(regname, COVERAGE_ON, FD_RW_SAMPLE)
      if(_coverage_type != COVERAGE_OFF) begin
        if(_sample_type inside {FD_RO_SAMPLE, RO_SAMPLE})
        `CREATE_OP_SAMPLE_COV(RAL_WRITE)
        else if(_sample_type inside {FD_WO_SAMPLE, WO_SAMPLE})
        `CREATE_OP_SAMPLE_COV(RAL_READ)
        else
        `CREATE_OP_SAMPLE_COV(RAL_NONE)
        `CREATE_DESIRED_COV
        `CREATE_ACTUAL_COV
      end
      `endif

  endfunction

  `ifdef SLA_RAL_COVERAGE
  `CREATE_RAL_OP_COVERGROUP
  covergroup desired_cg @(cov_ev);
     option.per_instance = 1;
     option.name = {sla_ral_file::regfilename,".",regname};
     `RAL_FIELD_CP(IP_READY_STS_1_0, IP_READY_STS_1_0.desired)
     `RAL_FIELD_CP_1(IP_READY_STS_1_0, IP_READY_STS_1_0.desired, 0)
     `RAL_FIELD_CP(IP_READY_STS_1_1, IP_READY_STS_1_1.desired)
     `RAL_FIELD_CP_1(IP_READY_STS_1_1, IP_READY_STS_1_1.desired, 0)
     `RAL_FIELD_CP(IP_READY_STS_1_2, IP_READY_STS_1_2.desired)
     `RAL_FIELD_CP_1(IP_READY_STS_1_2, IP_READY_STS_1_2.desired, 0)
     `RAL_FIELD_CP(IP_READY_STS_1_3, IP_READY_STS_1_3.desired)
     `RAL_FIELD_CP_1(IP_READY_STS_1_3, IP_READY_STS_1_3.desired, 0)
     `RAL_FIELD_CP(IP_READY_STS_1_4, IP_READY_STS_1_4.desired)
     `RAL_FIELD_CP_1(IP_READY_STS_1_4, IP_READY_STS_1_4.desired, 0)
     `RAL_FIELD_CP(IP_READY_STS_1_5, IP_READY_STS_1_5.desired)
     `RAL_FIELD_CP_1(IP_READY_STS_1_5, IP_READY_STS_1_5.desired, 0)
     `RAL_FIELD_CP(IP_READY_STS_1_6, IP_READY_STS_1_6.desired)
     `RAL_FIELD_CP_1(IP_READY_STS_1_6, IP_READY_STS_1_6.desired, 0)
     `RAL_FIELD_CP(IP_READY_STS_1_7, IP_READY_STS_1_7.desired)
     `RAL_FIELD_CP_1(IP_READY_STS_1_7, IP_READY_STS_1_7.desired, 0)
     `RAL_FIELD_CP(IP_READY_STS_1_8, IP_READY_STS_1_8.desired)
     `RAL_FIELD_CP_1(IP_READY_STS_1_8, IP_READY_STS_1_8.desired, 0)
     `RAL_FIELD_CP(IP_READY_STS_1_9, IP_READY_STS_1_9.desired)
     `RAL_FIELD_CP_1(IP_READY_STS_1_9, IP_READY_STS_1_9.desired, 0)
     `RAL_FIELD_CP(IP_READY_STS_1_10, IP_READY_STS_1_10.desired)
     `RAL_FIELD_CP_1(IP_READY_STS_1_10, IP_READY_STS_1_10.desired, 0)
     `RAL_FIELD_CP(IP_READY_STS_1_11, IP_READY_STS_1_11.desired)
     `RAL_FIELD_CP_1(IP_READY_STS_1_11, IP_READY_STS_1_11.desired, 0)
     `RAL_FIELD_CP(IP_READY_STS_1_12, IP_READY_STS_1_12.desired)
     `RAL_FIELD_CP_1(IP_READY_STS_1_12, IP_READY_STS_1_12.desired, 0)
     `RAL_FIELD_CP(IP_READY_STS_1_13, IP_READY_STS_1_13.desired)
     `RAL_FIELD_CP_1(IP_READY_STS_1_13, IP_READY_STS_1_13.desired, 0)
     `RAL_FIELD_CP(IP_READY_STS_1_14, IP_READY_STS_1_14.desired)
     `RAL_FIELD_CP_1(IP_READY_STS_1_14, IP_READY_STS_1_14.desired, 0)
     `RAL_FIELD_CP(IP_READY_STS_1_15, IP_READY_STS_1_15.desired)
     `RAL_FIELD_CP_1(IP_READY_STS_1_15, IP_READY_STS_1_15.desired, 0)
     `RAL_FIELD_CP(IP_READY_STS_1_16, IP_READY_STS_1_16.desired)
     `RAL_FIELD_CP_1(IP_READY_STS_1_16, IP_READY_STS_1_16.desired, 0)
     `RAL_FIELD_CP(IP_READY_STS_1_17, IP_READY_STS_1_17.desired)
     `RAL_FIELD_CP_1(IP_READY_STS_1_17, IP_READY_STS_1_17.desired, 0)
     `RAL_FIELD_CP(IP_READY_STS_1_18, IP_READY_STS_1_18.desired)
     `RAL_FIELD_CP_1(IP_READY_STS_1_18, IP_READY_STS_1_18.desired, 0)
     `RAL_FIELD_CP(IP_READY_STS_1_19, IP_READY_STS_1_19.desired)
     `RAL_FIELD_CP_1(IP_READY_STS_1_19, IP_READY_STS_1_19.desired, 0)
     `RAL_FIELD_CP(IP_READY_STS_1_20, IP_READY_STS_1_20.desired)
     `RAL_FIELD_CP_1(IP_READY_STS_1_20, IP_READY_STS_1_20.desired, 0)
     `RAL_FIELD_CP(IP_READY_STS_1_21, IP_READY_STS_1_21.desired)
     `RAL_FIELD_CP_1(IP_READY_STS_1_21, IP_READY_STS_1_21.desired, 0)
     `RAL_FIELD_CP(IP_READY_STS_1_22, IP_READY_STS_1_22.desired)
     `RAL_FIELD_CP_1(IP_READY_STS_1_22, IP_READY_STS_1_22.desired, 0)
     `RAL_FIELD_CP(IP_READY_STS_1_23, IP_READY_STS_1_23.desired)
     `RAL_FIELD_CP_1(IP_READY_STS_1_23, IP_READY_STS_1_23.desired, 0)
     `RAL_FIELD_CP(IP_READY_STS_1_24, IP_READY_STS_1_24.desired)
     `RAL_FIELD_CP_1(IP_READY_STS_1_24, IP_READY_STS_1_24.desired, 0)
     `RAL_FIELD_CP(IP_READY_STS_1_25, IP_READY_STS_1_25.desired)
     `RAL_FIELD_CP_1(IP_READY_STS_1_25, IP_READY_STS_1_25.desired, 0)
     `RAL_FIELD_CP(IP_READY_STS_1_26, IP_READY_STS_1_26.desired)
     `RAL_FIELD_CP_1(IP_READY_STS_1_26, IP_READY_STS_1_26.desired, 0)
     `RAL_FIELD_CP(IP_READY_STS_1_27, IP_READY_STS_1_27.desired)
     `RAL_FIELD_CP_1(IP_READY_STS_1_27, IP_READY_STS_1_27.desired, 0)
     `RAL_FIELD_CP(IP_READY_STS_1_28, IP_READY_STS_1_28.desired)
     `RAL_FIELD_CP_1(IP_READY_STS_1_28, IP_READY_STS_1_28.desired, 0)
     `RAL_FIELD_CP(IP_READY_STS_1_29, IP_READY_STS_1_29.desired)
     `RAL_FIELD_CP_1(IP_READY_STS_1_29, IP_READY_STS_1_29.desired, 0)
     `RAL_FIELD_CP(IP_READY_STS_1_30, IP_READY_STS_1_30.desired)
     `RAL_FIELD_CP_1(IP_READY_STS_1_30, IP_READY_STS_1_30.desired, 0)
     `RAL_FIELD_CP(IP_READY_STS_1_31, IP_READY_STS_1_31.desired)
     `RAL_FIELD_CP_1(IP_READY_STS_1_31, IP_READY_STS_1_31.desired, 0)
  endgroup
  covergroup actual_cg @(cov_ev);
     option.per_instance = 1;
     option.name = {sla_ral_file::regfilename,".",regname};
     `RAL_FIELD_CP(IP_READY_STS_1_0, IP_READY_STS_1_0.actual)
     `RAL_FIELD_CP_1(IP_READY_STS_1_0, IP_READY_STS_1_0.actual, 0)
     `RAL_FIELD_CP(IP_READY_STS_1_1, IP_READY_STS_1_1.actual)
     `RAL_FIELD_CP_1(IP_READY_STS_1_1, IP_READY_STS_1_1.actual, 0)
     `RAL_FIELD_CP(IP_READY_STS_1_2, IP_READY_STS_1_2.actual)
     `RAL_FIELD_CP_1(IP_READY_STS_1_2, IP_READY_STS_1_2.actual, 0)
     `RAL_FIELD_CP(IP_READY_STS_1_3, IP_READY_STS_1_3.actual)
     `RAL_FIELD_CP_1(IP_READY_STS_1_3, IP_READY_STS_1_3.actual, 0)
     `RAL_FIELD_CP(IP_READY_STS_1_4, IP_READY_STS_1_4.actual)
     `RAL_FIELD_CP_1(IP_READY_STS_1_4, IP_READY_STS_1_4.actual, 0)
     `RAL_FIELD_CP(IP_READY_STS_1_5, IP_READY_STS_1_5.actual)
     `RAL_FIELD_CP_1(IP_READY_STS_1_5, IP_READY_STS_1_5.actual, 0)
     `RAL_FIELD_CP(IP_READY_STS_1_6, IP_READY_STS_1_6.actual)
     `RAL_FIELD_CP_1(IP_READY_STS_1_6, IP_READY_STS_1_6.actual, 0)
     `RAL_FIELD_CP(IP_READY_STS_1_7, IP_READY_STS_1_7.actual)
     `RAL_FIELD_CP_1(IP_READY_STS_1_7, IP_READY_STS_1_7.actual, 0)
     `RAL_FIELD_CP(IP_READY_STS_1_8, IP_READY_STS_1_8.actual)
     `RAL_FIELD_CP_1(IP_READY_STS_1_8, IP_READY_STS_1_8.actual, 0)
     `RAL_FIELD_CP(IP_READY_STS_1_9, IP_READY_STS_1_9.actual)
     `RAL_FIELD_CP_1(IP_READY_STS_1_9, IP_READY_STS_1_9.actual, 0)
     `RAL_FIELD_CP(IP_READY_STS_1_10, IP_READY_STS_1_10.actual)
     `RAL_FIELD_CP_1(IP_READY_STS_1_10, IP_READY_STS_1_10.actual, 0)
     `RAL_FIELD_CP(IP_READY_STS_1_11, IP_READY_STS_1_11.actual)
     `RAL_FIELD_CP_1(IP_READY_STS_1_11, IP_READY_STS_1_11.actual, 0)
     `RAL_FIELD_CP(IP_READY_STS_1_12, IP_READY_STS_1_12.actual)
     `RAL_FIELD_CP_1(IP_READY_STS_1_12, IP_READY_STS_1_12.actual, 0)
     `RAL_FIELD_CP(IP_READY_STS_1_13, IP_READY_STS_1_13.actual)
     `RAL_FIELD_CP_1(IP_READY_STS_1_13, IP_READY_STS_1_13.actual, 0)
     `RAL_FIELD_CP(IP_READY_STS_1_14, IP_READY_STS_1_14.actual)
     `RAL_FIELD_CP_1(IP_READY_STS_1_14, IP_READY_STS_1_14.actual, 0)
     `RAL_FIELD_CP(IP_READY_STS_1_15, IP_READY_STS_1_15.actual)
     `RAL_FIELD_CP_1(IP_READY_STS_1_15, IP_READY_STS_1_15.actual, 0)
     `RAL_FIELD_CP(IP_READY_STS_1_16, IP_READY_STS_1_16.actual)
     `RAL_FIELD_CP_1(IP_READY_STS_1_16, IP_READY_STS_1_16.actual, 0)
     `RAL_FIELD_CP(IP_READY_STS_1_17, IP_READY_STS_1_17.actual)
     `RAL_FIELD_CP_1(IP_READY_STS_1_17, IP_READY_STS_1_17.actual, 0)
     `RAL_FIELD_CP(IP_READY_STS_1_18, IP_READY_STS_1_18.actual)
     `RAL_FIELD_CP_1(IP_READY_STS_1_18, IP_READY_STS_1_18.actual, 0)
     `RAL_FIELD_CP(IP_READY_STS_1_19, IP_READY_STS_1_19.actual)
     `RAL_FIELD_CP_1(IP_READY_STS_1_19, IP_READY_STS_1_19.actual, 0)
     `RAL_FIELD_CP(IP_READY_STS_1_20, IP_READY_STS_1_20.actual)
     `RAL_FIELD_CP_1(IP_READY_STS_1_20, IP_READY_STS_1_20.actual, 0)
     `RAL_FIELD_CP(IP_READY_STS_1_21, IP_READY_STS_1_21.actual)
     `RAL_FIELD_CP_1(IP_READY_STS_1_21, IP_READY_STS_1_21.actual, 0)
     `RAL_FIELD_CP(IP_READY_STS_1_22, IP_READY_STS_1_22.actual)
     `RAL_FIELD_CP_1(IP_READY_STS_1_22, IP_READY_STS_1_22.actual, 0)
     `RAL_FIELD_CP(IP_READY_STS_1_23, IP_READY_STS_1_23.actual)
     `RAL_FIELD_CP_1(IP_READY_STS_1_23, IP_READY_STS_1_23.actual, 0)
     `RAL_FIELD_CP(IP_READY_STS_1_24, IP_READY_STS_1_24.actual)
     `RAL_FIELD_CP_1(IP_READY_STS_1_24, IP_READY_STS_1_24.actual, 0)
     `RAL_FIELD_CP(IP_READY_STS_1_25, IP_READY_STS_1_25.actual)
     `RAL_FIELD_CP_1(IP_READY_STS_1_25, IP_READY_STS_1_25.actual, 0)
     `RAL_FIELD_CP(IP_READY_STS_1_26, IP_READY_STS_1_26.actual)
     `RAL_FIELD_CP_1(IP_READY_STS_1_26, IP_READY_STS_1_26.actual, 0)
     `RAL_FIELD_CP(IP_READY_STS_1_27, IP_READY_STS_1_27.actual)
     `RAL_FIELD_CP_1(IP_READY_STS_1_27, IP_READY_STS_1_27.actual, 0)
     `RAL_FIELD_CP(IP_READY_STS_1_28, IP_READY_STS_1_28.actual)
     `RAL_FIELD_CP_1(IP_READY_STS_1_28, IP_READY_STS_1_28.actual, 0)
     `RAL_FIELD_CP(IP_READY_STS_1_29, IP_READY_STS_1_29.actual)
     `RAL_FIELD_CP_1(IP_READY_STS_1_29, IP_READY_STS_1_29.actual, 0)
     `RAL_FIELD_CP(IP_READY_STS_1_30, IP_READY_STS_1_30.actual)
     `RAL_FIELD_CP_1(IP_READY_STS_1_30, IP_READY_STS_1_30.actual, 0)
     `RAL_FIELD_CP(IP_READY_STS_1_31, IP_READY_STS_1_31.actual)
     `RAL_FIELD_CP_1(IP_READY_STS_1_31, IP_READY_STS_1_31.actual, 0)
  endgroup
  `endif

  // --------------------------
  // add constraint based on database legal values
  // --------------------------
  function void build();
    IP_READY_STS_1_0 = new("IP_READY_STS_1_0", "RW/1C/V", 1, 0, {"IP_READY_STS_1.IP_READY_STS_1_0"});
    IP_READY_STS_1_0.set_powerwell("primary");
    IP_READY_STS_1_0.set_rand_mode(0);
   IP_READY_STS_1_0.set_reset_signame("pmu_rst_b");
    void'(add_field( IP_READY_STS_1_0 ));

    IP_READY_STS_1_1 = new("IP_READY_STS_1_1", "RW/1C/V", 1, 1, {"IP_READY_STS_1.IP_READY_STS_1_1"});
    IP_READY_STS_1_1.set_powerwell("primary");
    IP_READY_STS_1_1.set_rand_mode(0);
   IP_READY_STS_1_1.set_reset_signame("pmu_rst_b");
    void'(add_field( IP_READY_STS_1_1 ));

    IP_READY_STS_1_2 = new("IP_READY_STS_1_2", "RW/1C/V", 1, 2, {"IP_READY_STS_1.IP_READY_STS_1_2"});
    IP_READY_STS_1_2.set_powerwell("primary");
    IP_READY_STS_1_2.set_rand_mode(0);
   IP_READY_STS_1_2.set_reset_signame("pmu_rst_b");
    void'(add_field( IP_READY_STS_1_2 ));

    IP_READY_STS_1_3 = new("IP_READY_STS_1_3", "RW/1C/V", 1, 3, {"IP_READY_STS_1.IP_READY_STS_1_3"});
    IP_READY_STS_1_3.set_powerwell("primary");
    IP_READY_STS_1_3.set_rand_mode(0);
   IP_READY_STS_1_3.set_reset_signame("pmu_rst_b");
    void'(add_field( IP_READY_STS_1_3 ));

    IP_READY_STS_1_4 = new("IP_READY_STS_1_4", "RW/1C/V", 1, 4, {"IP_READY_STS_1.IP_READY_STS_1_4"});
    IP_READY_STS_1_4.set_powerwell("primary");
    IP_READY_STS_1_4.set_rand_mode(0);
   IP_READY_STS_1_4.set_reset_signame("pmu_rst_b");
    void'(add_field( IP_READY_STS_1_4 ));

    IP_READY_STS_1_5 = new("IP_READY_STS_1_5", "RW/1C/V", 1, 5, {"IP_READY_STS_1.IP_READY_STS_1_5"});
    IP_READY_STS_1_5.set_powerwell("primary");
    IP_READY_STS_1_5.set_rand_mode(0);
   IP_READY_STS_1_5.set_reset_signame("pmu_rst_b");
    void'(add_field( IP_READY_STS_1_5 ));

    IP_READY_STS_1_6 = new("IP_READY_STS_1_6", "RW/1C/V", 1, 6, {"IP_READY_STS_1.IP_READY_STS_1_6"});
    IP_READY_STS_1_6.set_powerwell("primary");
    IP_READY_STS_1_6.set_rand_mode(0);
   IP_READY_STS_1_6.set_reset_signame("pmu_rst_b");
    void'(add_field( IP_READY_STS_1_6 ));

    IP_READY_STS_1_7 = new("IP_READY_STS_1_7", "RW/1C/V", 1, 7, {"IP_READY_STS_1.IP_READY_STS_1_7"});
    IP_READY_STS_1_7.set_powerwell("primary");
    IP_READY_STS_1_7.set_rand_mode(0);
   IP_READY_STS_1_7.set_reset_signame("pmu_rst_b");
    void'(add_field( IP_READY_STS_1_7 ));

    IP_READY_STS_1_8 = new("IP_READY_STS_1_8", "RW/1C/V", 1, 8, {"IP_READY_STS_1.IP_READY_STS_1_8"});
    IP_READY_STS_1_8.set_powerwell("primary");
    IP_READY_STS_1_8.set_rand_mode(0);
   IP_READY_STS_1_8.set_reset_signame("pmu_rst_b");
    void'(add_field( IP_READY_STS_1_8 ));

    IP_READY_STS_1_9 = new("IP_READY_STS_1_9", "RW/1C/V", 1, 9, {"IP_READY_STS_1.IP_READY_STS_1_9"});
    IP_READY_STS_1_9.set_powerwell("primary");
    IP_READY_STS_1_9.set_rand_mode(0);
   IP_READY_STS_1_9.set_reset_signame("pmu_rst_b");
    void'(add_field( IP_READY_STS_1_9 ));

    IP_READY_STS_1_10 = new("IP_READY_STS_1_10", "RW/1C/V", 1, 10, {"IP_READY_STS_1.IP_READY_STS_1_10"});
    IP_READY_STS_1_10.set_powerwell("primary");
    IP_READY_STS_1_10.set_rand_mode(0);
   IP_READY_STS_1_10.set_reset_signame("pmu_rst_b");
    void'(add_field( IP_READY_STS_1_10 ));

    IP_READY_STS_1_11 = new("IP_READY_STS_1_11", "RW/1C/V", 1, 11, {"IP_READY_STS_1.IP_READY_STS_1_11"});
    IP_READY_STS_1_11.set_powerwell("primary");
    IP_READY_STS_1_11.set_rand_mode(0);
   IP_READY_STS_1_11.set_reset_signame("pmu_rst_b");
    void'(add_field( IP_READY_STS_1_11 ));

    IP_READY_STS_1_12 = new("IP_READY_STS_1_12", "RW/1C/V", 1, 12, {"IP_READY_STS_1.IP_READY_STS_1_12"});
    IP_READY_STS_1_12.set_powerwell("primary");
    IP_READY_STS_1_12.set_rand_mode(0);
   IP_READY_STS_1_12.set_reset_signame("pmu_rst_b");
    void'(add_field( IP_READY_STS_1_12 ));

    IP_READY_STS_1_13 = new("IP_READY_STS_1_13", "RW/1C/V", 1, 13, {"IP_READY_STS_1.IP_READY_STS_1_13"});
    IP_READY_STS_1_13.set_powerwell("primary");
    IP_READY_STS_1_13.set_rand_mode(0);
   IP_READY_STS_1_13.set_reset_signame("pmu_rst_b");
    void'(add_field( IP_READY_STS_1_13 ));

    IP_READY_STS_1_14 = new("IP_READY_STS_1_14", "RW/1C/V", 1, 14, {"IP_READY_STS_1.IP_READY_STS_1_14"});
    IP_READY_STS_1_14.set_powerwell("primary");
    IP_READY_STS_1_14.set_rand_mode(0);
   IP_READY_STS_1_14.set_reset_signame("pmu_rst_b");
    void'(add_field( IP_READY_STS_1_14 ));

    IP_READY_STS_1_15 = new("IP_READY_STS_1_15", "RW/1C/V", 1, 15, {"IP_READY_STS_1.IP_READY_STS_1_15"});
    IP_READY_STS_1_15.set_powerwell("primary");
    IP_READY_STS_1_15.set_rand_mode(0);
   IP_READY_STS_1_15.set_reset_signame("pmu_rst_b");
    void'(add_field( IP_READY_STS_1_15 ));

    IP_READY_STS_1_16 = new("IP_READY_STS_1_16", "RW/1C/V", 1, 16, {"IP_READY_STS_1.IP_READY_STS_1_16"});
    IP_READY_STS_1_16.set_powerwell("primary");
    IP_READY_STS_1_16.set_rand_mode(0);
   IP_READY_STS_1_16.set_reset_signame("pmu_rst_b");
    void'(add_field( IP_READY_STS_1_16 ));

    IP_READY_STS_1_17 = new("IP_READY_STS_1_17", "RW/1C/V", 1, 17, {"IP_READY_STS_1.IP_READY_STS_1_17"});
    IP_READY_STS_1_17.set_powerwell("primary");
    IP_READY_STS_1_17.set_rand_mode(0);
   IP_READY_STS_1_17.set_reset_signame("pmu_rst_b");
    void'(add_field( IP_READY_STS_1_17 ));

    IP_READY_STS_1_18 = new("IP_READY_STS_1_18", "RW/1C/V", 1, 18, {"IP_READY_STS_1.IP_READY_STS_1_18"});
    IP_READY_STS_1_18.set_powerwell("primary");
    IP_READY_STS_1_18.set_rand_mode(0);
   IP_READY_STS_1_18.set_reset_signame("pmu_rst_b");
    void'(add_field( IP_READY_STS_1_18 ));

    IP_READY_STS_1_19 = new("IP_READY_STS_1_19", "RW/1C/V", 1, 19, {"IP_READY_STS_1.IP_READY_STS_1_19"});
    IP_READY_STS_1_19.set_powerwell("primary");
    IP_READY_STS_1_19.set_rand_mode(0);
   IP_READY_STS_1_19.set_reset_signame("pmu_rst_b");
    void'(add_field( IP_READY_STS_1_19 ));

    IP_READY_STS_1_20 = new("IP_READY_STS_1_20", "RW/1C/V", 1, 20, {"IP_READY_STS_1.IP_READY_STS_1_20"});
    IP_READY_STS_1_20.set_powerwell("primary");
    IP_READY_STS_1_20.set_rand_mode(0);
   IP_READY_STS_1_20.set_reset_signame("pmu_rst_b");
    void'(add_field( IP_READY_STS_1_20 ));

    IP_READY_STS_1_21 = new("IP_READY_STS_1_21", "RW/1C/V", 1, 21, {"IP_READY_STS_1.IP_READY_STS_1_21"});
    IP_READY_STS_1_21.set_powerwell("primary");
    IP_READY_STS_1_21.set_rand_mode(0);
   IP_READY_STS_1_21.set_reset_signame("pmu_rst_b");
    void'(add_field( IP_READY_STS_1_21 ));

    IP_READY_STS_1_22 = new("IP_READY_STS_1_22", "RW/1C/V", 1, 22, {"IP_READY_STS_1.IP_READY_STS_1_22"});
    IP_READY_STS_1_22.set_powerwell("primary");
    IP_READY_STS_1_22.set_rand_mode(0);
   IP_READY_STS_1_22.set_reset_signame("pmu_rst_b");
    void'(add_field( IP_READY_STS_1_22 ));

    IP_READY_STS_1_23 = new("IP_READY_STS_1_23", "RW/1C/V", 1, 23, {"IP_READY_STS_1.IP_READY_STS_1_23"});
    IP_READY_STS_1_23.set_powerwell("primary");
    IP_READY_STS_1_23.set_rand_mode(0);
   IP_READY_STS_1_23.set_reset_signame("pmu_rst_b");
    void'(add_field( IP_READY_STS_1_23 ));

    IP_READY_STS_1_24 = new("IP_READY_STS_1_24", "RW/1C/V", 1, 24, {"IP_READY_STS_1.IP_READY_STS_1_24"});
    IP_READY_STS_1_24.set_powerwell("primary");
    IP_READY_STS_1_24.set_rand_mode(0);
   IP_READY_STS_1_24.set_reset_signame("pmu_rst_b");
    void'(add_field( IP_READY_STS_1_24 ));

    IP_READY_STS_1_25 = new("IP_READY_STS_1_25", "RW/1C/V", 1, 25, {"IP_READY_STS_1.IP_READY_STS_1_25"});
    IP_READY_STS_1_25.set_powerwell("primary");
    IP_READY_STS_1_25.set_rand_mode(0);
   IP_READY_STS_1_25.set_reset_signame("pmu_rst_b");
    void'(add_field( IP_READY_STS_1_25 ));

    IP_READY_STS_1_26 = new("IP_READY_STS_1_26", "RW/1C/V", 1, 26, {"IP_READY_STS_1.IP_READY_STS_1_26"});
    IP_READY_STS_1_26.set_powerwell("primary");
    IP_READY_STS_1_26.set_rand_mode(0);
   IP_READY_STS_1_26.set_reset_signame("pmu_rst_b");
    void'(add_field( IP_READY_STS_1_26 ));

    IP_READY_STS_1_27 = new("IP_READY_STS_1_27", "RW/1C/V", 1, 27, {"IP_READY_STS_1.IP_READY_STS_1_27"});
    IP_READY_STS_1_27.set_powerwell("primary");
    IP_READY_STS_1_27.set_rand_mode(0);
   IP_READY_STS_1_27.set_reset_signame("pmu_rst_b");
    void'(add_field( IP_READY_STS_1_27 ));

    IP_READY_STS_1_28 = new("IP_READY_STS_1_28", "RW/1C/V", 1, 28, {"IP_READY_STS_1.IP_READY_STS_1_28"});
    IP_READY_STS_1_28.set_powerwell("primary");
    IP_READY_STS_1_28.set_rand_mode(0);
   IP_READY_STS_1_28.set_reset_signame("pmu_rst_b");
    void'(add_field( IP_READY_STS_1_28 ));

    IP_READY_STS_1_29 = new("IP_READY_STS_1_29", "RW/1C/V", 1, 29, {"IP_READY_STS_1.IP_READY_STS_1_29"});
    IP_READY_STS_1_29.set_powerwell("primary");
    IP_READY_STS_1_29.set_rand_mode(0);
   IP_READY_STS_1_29.set_reset_signame("pmu_rst_b");
    void'(add_field( IP_READY_STS_1_29 ));

    IP_READY_STS_1_30 = new("IP_READY_STS_1_30", "RW/1C/V", 1, 30, {"IP_READY_STS_1.IP_READY_STS_1_30"});
    IP_READY_STS_1_30.set_powerwell("primary");
    IP_READY_STS_1_30.set_rand_mode(0);
   IP_READY_STS_1_30.set_reset_signame("pmu_rst_b");
    void'(add_field( IP_READY_STS_1_30 ));

    IP_READY_STS_1_31 = new("IP_READY_STS_1_31", "RW/1C/V", 1, 31, {"IP_READY_STS_1.IP_READY_STS_1_31"});
    IP_READY_STS_1_31.set_powerwell("primary");
    IP_READY_STS_1_31.set_rand_mode(0);
   IP_READY_STS_1_31.set_reset_signame("pmu_rst_b");
    void'(add_field( IP_READY_STS_1_31 ));

  endfunction


`DEFINE_NONE_SCOPE_REG_API(pmu_mmr_IP_READY_STS_1_reg) 
endclass : pmu_mmr_IP_READY_STS_1_reg

// ================================================

class pmu_mmr_IP_MSG_EN_0_reg extends sla_ral_reg;

  // --------------------------
  sla_ral_field SB_MSG_EN;

  // --------------------------
  `ovm_object_utils(pmu_mmr_IP_MSG_EN_0_reg)

  // --------------------------
  function new(
                string         name = "",
                `ifdef SLA_RAL_COVERAGE
                sla_ral_coverage_t cov_t = COVERAGE_OFF,
                `endif
                int            bus_num=0,
                int            dev_num=0,
                int            func_num=0,
                int            offset=0,
                int            size=0,
                sla_ral_data_t reset_val=0,
                boolean_t      mon_enabled = SLA_FALSE
             );
      super.new();
      set_space_addr("CFG",undef);
      set_space_addr("MEM",undef);
      set_space_addr("IO",undef);
      set_space_addr("MSG",undef);
      set_space_addr("CREG",undef); 
      set_space_addr("LT_MEM",undef);    
 build();

      `ifdef SLA_RAL_COVERAGE
      `CONFIG_RAL_COVERAGE(regname, COVERAGE_ON, FD_RW_SAMPLE)
      if(_coverage_type != COVERAGE_OFF) begin
        if(_sample_type inside {FD_RO_SAMPLE, RO_SAMPLE})
        `CREATE_OP_SAMPLE_COV(RAL_WRITE)
        else if(_sample_type inside {FD_WO_SAMPLE, WO_SAMPLE})
        `CREATE_OP_SAMPLE_COV(RAL_READ)
        else
        `CREATE_OP_SAMPLE_COV(RAL_NONE)
        `CREATE_DESIRED_COV
        `CREATE_ACTUAL_COV
      end
      `endif

  endfunction

  `ifdef SLA_RAL_COVERAGE
  `CREATE_RAL_OP_COVERGROUP
  covergroup desired_cg @(cov_ev);
     option.per_instance = 1;
     option.name = {sla_ral_file::regfilename,".",regname};
     `RAL_FIELD_CP(SB_MSG_EN, SB_MSG_EN.desired)
     `RAL_FIELD_CP_16(SB_MSG_EN, SB_MSG_EN.desired, 0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)
     `RAL_FIELD_CP_16(SB_MSG_EN, SB_MSG_EN.desired, 16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31)
  endgroup
  covergroup actual_cg @(cov_ev);
     option.per_instance = 1;
     option.name = {sla_ral_file::regfilename,".",regname};
     `RAL_FIELD_CP(SB_MSG_EN, SB_MSG_EN.actual)
     `RAL_FIELD_CP_16(SB_MSG_EN, SB_MSG_EN.actual, 0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)
     `RAL_FIELD_CP_16(SB_MSG_EN, SB_MSG_EN.actual, 16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31)
  endgroup
  `endif

  // --------------------------
  // add constraint based on database legal values
  // --------------------------
  function void build();
    SB_MSG_EN = new("SB_MSG_EN", "RW", 32, 0, {"IP_MSG_EN_0.SB_MSG_EN"});
    SB_MSG_EN.set_powerwell("primary");
    SB_MSG_EN.set_rand_mode(0);
   SB_MSG_EN.set_reset_signame("pmu_rst_b");
    void'(add_field( SB_MSG_EN ));

  endfunction


`DEFINE_NONE_SCOPE_REG_API(pmu_mmr_IP_MSG_EN_0_reg) 
endclass : pmu_mmr_IP_MSG_EN_0_reg

// ================================================

class pmu_mmr_IP_MSG_EN_1_reg extends sla_ral_reg;

  // --------------------------
  sla_ral_field SB_MSG_EN;

  // --------------------------
  `ovm_object_utils(pmu_mmr_IP_MSG_EN_1_reg)

  // --------------------------
  function new(
                string         name = "",
                `ifdef SLA_RAL_COVERAGE
                sla_ral_coverage_t cov_t = COVERAGE_OFF,
                `endif
                int            bus_num=0,
                int            dev_num=0,
                int            func_num=0,
                int            offset=0,
                int            size=0,
                sla_ral_data_t reset_val=0,
                boolean_t      mon_enabled = SLA_FALSE
             );
      super.new();
      set_space_addr("CFG",undef);
      set_space_addr("MEM",undef);
      set_space_addr("IO",undef);
      set_space_addr("MSG",undef);
      set_space_addr("CREG",undef); 
      set_space_addr("LT_MEM",undef);    
 build();

      `ifdef SLA_RAL_COVERAGE
      `CONFIG_RAL_COVERAGE(regname, COVERAGE_ON, FD_RW_SAMPLE)
      if(_coverage_type != COVERAGE_OFF) begin
        if(_sample_type inside {FD_RO_SAMPLE, RO_SAMPLE})
        `CREATE_OP_SAMPLE_COV(RAL_WRITE)
        else if(_sample_type inside {FD_WO_SAMPLE, WO_SAMPLE})
        `CREATE_OP_SAMPLE_COV(RAL_READ)
        else
        `CREATE_OP_SAMPLE_COV(RAL_NONE)
        `CREATE_DESIRED_COV
        `CREATE_ACTUAL_COV
      end
      `endif

  endfunction

  `ifdef SLA_RAL_COVERAGE
  `CREATE_RAL_OP_COVERGROUP
  covergroup desired_cg @(cov_ev);
     option.per_instance = 1;
     option.name = {sla_ral_file::regfilename,".",regname};
     `RAL_FIELD_CP(SB_MSG_EN, SB_MSG_EN.desired)
     `RAL_FIELD_CP_16(SB_MSG_EN, SB_MSG_EN.desired, 0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)
     `RAL_FIELD_CP_16(SB_MSG_EN, SB_MSG_EN.desired, 16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31)
  endgroup
  covergroup actual_cg @(cov_ev);
     option.per_instance = 1;
     option.name = {sla_ral_file::regfilename,".",regname};
     `RAL_FIELD_CP(SB_MSG_EN, SB_MSG_EN.actual)
     `RAL_FIELD_CP_16(SB_MSG_EN, SB_MSG_EN.actual, 0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)
     `RAL_FIELD_CP_16(SB_MSG_EN, SB_MSG_EN.actual, 16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31)
  endgroup
  `endif

  // --------------------------
  // add constraint based on database legal values
  // --------------------------
  function void build();
    SB_MSG_EN = new("SB_MSG_EN", "RW", 32, 0, {"IP_MSG_EN_1.SB_MSG_EN"});
    SB_MSG_EN.set_powerwell("primary");
    SB_MSG_EN.set_rand_mode(0);
   SB_MSG_EN.set_reset_signame("pmu_rst_b");
    void'(add_field( SB_MSG_EN ));

  endfunction


`DEFINE_NONE_SCOPE_REG_API(pmu_mmr_IP_MSG_EN_1_reg) 
endclass : pmu_mmr_IP_MSG_EN_1_reg

// ================================================

class pmu_mmr_SBI_CHMSG_CTRL_reg extends sla_ral_reg;

  // --------------------------
  sla_ral_field SEND_CHMSG_SB;
  sla_ral_field FPGPOK_TYPE;
  sla_ral_field NO_EXH;
  sla_ral_field BP_RP_TYPE;
  sla_ral_field BP_RP_PREP_TYPE;
  sla_ral_field BP_RP_PREP_ACK;
  sla_ral_field CHMSG_RS;
  sla_ral_field CHMSG_MT;

  // --------------------------
  `ovm_object_utils(pmu_mmr_SBI_CHMSG_CTRL_reg)

  // --------------------------
  function new(
                string         name = "",
                `ifdef SLA_RAL_COVERAGE
                sla_ral_coverage_t cov_t = COVERAGE_OFF,
                `endif
                int            bus_num=0,
                int            dev_num=0,
                int            func_num=0,
                int            offset=0,
                int            size=0,
                sla_ral_data_t reset_val=0,
                boolean_t      mon_enabled = SLA_FALSE
             );
      super.new();
      set_space_addr("CFG",undef);
      set_space_addr("MEM",undef);
      set_space_addr("IO",undef);
      set_space_addr("MSG",undef);
      set_space_addr("CREG",undef); 
      set_space_addr("LT_MEM",undef);    
 build();

      `ifdef SLA_RAL_COVERAGE
      `CONFIG_RAL_COVERAGE(regname, COVERAGE_ON, FD_RW_SAMPLE)
      if(_coverage_type != COVERAGE_OFF) begin
        if(_sample_type inside {FD_RO_SAMPLE, RO_SAMPLE})
        `CREATE_OP_SAMPLE_COV(RAL_WRITE)
        else if(_sample_type inside {FD_WO_SAMPLE, WO_SAMPLE})
        `CREATE_OP_SAMPLE_COV(RAL_READ)
        else
        `CREATE_OP_SAMPLE_COV(RAL_NONE)
        `CREATE_DESIRED_COV
        `CREATE_ACTUAL_COV
      end
      `endif

  endfunction

  `ifdef SLA_RAL_COVERAGE
  `CREATE_RAL_OP_COVERGROUP
  covergroup desired_cg @(cov_ev);
     option.per_instance = 1;
     option.name = {sla_ral_file::regfilename,".",regname};
     `RAL_FIELD_CP(SEND_CHMSG_SB, SEND_CHMSG_SB.desired)
     `RAL_FIELD_CP_1(SEND_CHMSG_SB, SEND_CHMSG_SB.desired, 0)
     `RAL_FIELD_CP(FPGPOK_TYPE, FPGPOK_TYPE.desired)
     `RAL_FIELD_CP_2(FPGPOK_TYPE, FPGPOK_TYPE.desired, 0,1)
     `RAL_FIELD_CP(NO_EXH, NO_EXH.desired)
     `RAL_FIELD_CP_1(NO_EXH, NO_EXH.desired, 0)
     `RAL_FIELD_CP(BP_RP_TYPE, BP_RP_TYPE.desired)
     `RAL_FIELD_CP_8(BP_RP_TYPE, BP_RP_TYPE.desired, 0,1,2,3,4,5,6,7)
     `RAL_FIELD_CP(BP_RP_PREP_TYPE, BP_RP_PREP_TYPE.desired)
     `RAL_FIELD_CP_8(BP_RP_PREP_TYPE, BP_RP_PREP_TYPE.desired, 0,1,2,3,4,5,6,7)
     `RAL_FIELD_CP(BP_RP_PREP_ACK, BP_RP_PREP_ACK.desired)
     `RAL_FIELD_CP_1(BP_RP_PREP_ACK, BP_RP_PREP_ACK.desired, 0)
     `RAL_FIELD_CP(CHMSG_RS, CHMSG_RS.desired)
     `RAL_FIELD_CP_2(CHMSG_RS, CHMSG_RS.desired, 0,1)
     `RAL_FIELD_CP(CHMSG_MT, CHMSG_MT.desired)
     `RAL_FIELD_CP_2(CHMSG_MT, CHMSG_MT.desired, 0,1)
  endgroup
  covergroup actual_cg @(cov_ev);
     option.per_instance = 1;
     option.name = {sla_ral_file::regfilename,".",regname};
     `RAL_FIELD_CP(SEND_CHMSG_SB, SEND_CHMSG_SB.actual)
     `RAL_FIELD_CP_1(SEND_CHMSG_SB, SEND_CHMSG_SB.actual, 0)
     `RAL_FIELD_CP(FPGPOK_TYPE, FPGPOK_TYPE.actual)
     `RAL_FIELD_CP_2(FPGPOK_TYPE, FPGPOK_TYPE.actual, 0,1)
     `RAL_FIELD_CP(NO_EXH, NO_EXH.actual)
     `RAL_FIELD_CP_1(NO_EXH, NO_EXH.actual, 0)
     `RAL_FIELD_CP(BP_RP_TYPE, BP_RP_TYPE.actual)
     `RAL_FIELD_CP_8(BP_RP_TYPE, BP_RP_TYPE.actual, 0,1,2,3,4,5,6,7)
     `RAL_FIELD_CP(BP_RP_PREP_TYPE, BP_RP_PREP_TYPE.actual)
     `RAL_FIELD_CP_8(BP_RP_PREP_TYPE, BP_RP_PREP_TYPE.actual, 0,1,2,3,4,5,6,7)
     `RAL_FIELD_CP(BP_RP_PREP_ACK, BP_RP_PREP_ACK.actual)
     `RAL_FIELD_CP_1(BP_RP_PREP_ACK, BP_RP_PREP_ACK.actual, 0)
     `RAL_FIELD_CP(CHMSG_RS, CHMSG_RS.actual)
     `RAL_FIELD_CP_2(CHMSG_RS, CHMSG_RS.actual, 0,1)
     `RAL_FIELD_CP(CHMSG_MT, CHMSG_MT.actual)
     `RAL_FIELD_CP_2(CHMSG_MT, CHMSG_MT.actual, 0,1)
  endgroup
  `endif

  // --------------------------
  // add constraint based on database legal values
  // --------------------------
  function void build();
    SEND_CHMSG_SB = new("SEND_CHMSG_SB", "RW/1S/V", 1, 0, {"SBI_CHMSG_CTRL.SEND_CHMSG_SB"});
    SEND_CHMSG_SB.set_powerwell("primary");
    SEND_CHMSG_SB.set_rand_mode(0);
   SEND_CHMSG_SB.set_reset_signame("pmu_rst_b");
    void'(add_field( SEND_CHMSG_SB ));

    FPGPOK_TYPE = new("FPGPOK_TYPE", "RW", 2, 1, {"SBI_CHMSG_CTRL.FPGPOK_TYPE"});
    FPGPOK_TYPE.set_powerwell("primary");
    FPGPOK_TYPE.set_rand_mode(0);
   FPGPOK_TYPE.set_reset_signame("pmu_rst_b");
    void'(add_field( FPGPOK_TYPE ));

    NO_EXH = new("NO_EXH", "RW", 1, 3, {"SBI_CHMSG_CTRL.NO_EXH"});
    NO_EXH.set_powerwell("primary");
    NO_EXH.set_rand_mode(0);
   NO_EXH.set_reset_signame("pmu_rst_b");
    void'(add_field( NO_EXH ));

    BP_RP_TYPE = new("BP_RP_TYPE", "RW", 8, 8, {"SBI_CHMSG_CTRL.BP_RP_TYPE"});
    BP_RP_TYPE.set_powerwell("primary");
    BP_RP_TYPE.set_rand_mode(0);
   BP_RP_TYPE.set_reset_signame("pmu_rst_b");
    void'(add_field( BP_RP_TYPE ));

    BP_RP_PREP_TYPE = new("BP_RP_PREP_TYPE", "RW", 8, 16, {"SBI_CHMSG_CTRL.BP_RP_PREP_TYPE"});
    BP_RP_PREP_TYPE.set_powerwell("primary");
    BP_RP_PREP_TYPE.set_rand_mode(0);
   BP_RP_PREP_TYPE.set_reset_signame("pmu_rst_b");
    void'(add_field( BP_RP_PREP_TYPE ));

    BP_RP_PREP_ACK = new("BP_RP_PREP_ACK", "RW", 1, 24, {"SBI_CHMSG_CTRL.BP_RP_PREP_ACK"});
    BP_RP_PREP_ACK.set_powerwell("primary");
    BP_RP_PREP_ACK.set_rand_mode(0);
   BP_RP_PREP_ACK.set_reset_signame("pmu_rst_b");
    void'(add_field( BP_RP_PREP_ACK ));

    CHMSG_RS = new("CHMSG_RS", "RW", 2, 25, {"SBI_CHMSG_CTRL.CHMSG_RS"});
    CHMSG_RS.set_powerwell("primary");
    CHMSG_RS.set_rand_mode(0);
   CHMSG_RS.set_reset_signame("pmu_rst_b");
    void'(add_field( CHMSG_RS ));

    CHMSG_MT = new("CHMSG_MT", "RW", 2, 27, {"SBI_CHMSG_CTRL.CHMSG_MT"});
    CHMSG_MT.set_powerwell("primary");
    CHMSG_MT.set_rand_mode(0);
   CHMSG_MT.set_reset_signame("pmu_rst_b");
    void'(add_field( CHMSG_MT ));

  endfunction


`DEFINE_NONE_SCOPE_REG_API(pmu_mmr_SBI_CHMSG_CTRL_reg) 
endclass : pmu_mmr_SBI_CHMSG_CTRL_reg

// ================================================

class pmu_mmr_SBI_SETID_DW1_reg extends sla_ral_reg;

  // --------------------------
  sla_ral_field REVID;
  sla_ral_field RSVD_15_8;
  sla_ral_field DEVID;

  // --------------------------
  `ovm_object_utils(pmu_mmr_SBI_SETID_DW1_reg)

  // --------------------------
  function new(
                string         name = "",
                `ifdef SLA_RAL_COVERAGE
                sla_ral_coverage_t cov_t = COVERAGE_OFF,
                `endif
                int            bus_num=0,
                int            dev_num=0,
                int            func_num=0,
                int            offset=0,
                int            size=0,
                sla_ral_data_t reset_val=0,
                boolean_t      mon_enabled = SLA_FALSE
             );
      super.new();
      set_space_addr("CFG",undef);
      set_space_addr("MEM",undef);
      set_space_addr("IO",undef);
      set_space_addr("MSG",undef);
      set_space_addr("CREG",undef); 
      set_space_addr("LT_MEM",undef);    
 build();

      `ifdef SLA_RAL_COVERAGE
      `CONFIG_RAL_COVERAGE(regname, COVERAGE_ON, FD_RW_SAMPLE)
      if(_coverage_type != COVERAGE_OFF) begin
        if(_sample_type inside {FD_RO_SAMPLE, RO_SAMPLE})
        `CREATE_OP_SAMPLE_COV(RAL_WRITE)
        else if(_sample_type inside {FD_WO_SAMPLE, WO_SAMPLE})
        `CREATE_OP_SAMPLE_COV(RAL_READ)
        else
        `CREATE_OP_SAMPLE_COV(RAL_NONE)
        `CREATE_DESIRED_COV
        `CREATE_ACTUAL_COV
      end
      `endif

  endfunction

  `ifdef SLA_RAL_COVERAGE
  `CREATE_RAL_OP_COVERGROUP
  covergroup desired_cg @(cov_ev);
     option.per_instance = 1;
     option.name = {sla_ral_file::regfilename,".",regname};
     `RAL_FIELD_CP(REVID, REVID.desired)
     `RAL_FIELD_CP_8(REVID, REVID.desired, 0,1,2,3,4,5,6,7)
     `RAL_FIELD_CP(RSVD_15_8, RSVD_15_8.desired)
     `RAL_FIELD_CP_8(RSVD_15_8, RSVD_15_8.desired, 0,1,2,3,4,5,6,7)
     `RAL_FIELD_CP(DEVID, DEVID.desired)
     `RAL_FIELD_CP_16(DEVID, DEVID.desired, 0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)
  endgroup
  covergroup actual_cg @(cov_ev);
     option.per_instance = 1;
     option.name = {sla_ral_file::regfilename,".",regname};
     `RAL_FIELD_CP(REVID, REVID.actual)
     `RAL_FIELD_CP_8(REVID, REVID.actual, 0,1,2,3,4,5,6,7)
     `RAL_FIELD_CP(RSVD_15_8, RSVD_15_8.actual)
     `RAL_FIELD_CP_8(RSVD_15_8, RSVD_15_8.actual, 0,1,2,3,4,5,6,7)
     `RAL_FIELD_CP(DEVID, DEVID.actual)
     `RAL_FIELD_CP_16(DEVID, DEVID.actual, 0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)
  endgroup
  `endif

  // --------------------------
  // add constraint based on database legal values
  // --------------------------
  function void build();
    REVID = new("REVID", "RW", 8, 0, {"SBI_SETID_DW1.REVID"});
    REVID.set_powerwell("primary");
    REVID.set_rand_mode(0);
   REVID.set_reset_signame("pmu_rst_b");
    void'(add_field( REVID ));

    RSVD_15_8 = new("RSVD_15_8", "RO", 8, 8, {"SBI_SETID_DW1.RSVD_15_8"});
    RSVD_15_8.set_powerwell("primary");
    RSVD_15_8.set_rand_mode(0);
    void'(add_field( RSVD_15_8 ));

    DEVID = new("DEVID", "RW", 16, 16, {"SBI_SETID_DW1.DEVID"});
    DEVID.set_powerwell("primary");
    DEVID.set_rand_mode(0);
   DEVID.set_reset_signame("pmu_rst_b");
    void'(add_field( DEVID ));

  endfunction


`DEFINE_NONE_SCOPE_REG_API(pmu_mmr_SBI_SETID_DW1_reg) 
endclass : pmu_mmr_SBI_SETID_DW1_reg

// ================================================

class pmu_mmr_SBI_SETID_DW2_reg extends sla_ral_reg;

  // --------------------------
  sla_ral_field PPOP;
  sla_ral_field MID;
  sla_ral_field MSID;
  sla_ral_field DPOP;
  sla_ral_field RSVD_31_28;

  // --------------------------
  `ovm_object_utils(pmu_mmr_SBI_SETID_DW2_reg)

  // --------------------------
  function new(
                string         name = "",
                `ifdef SLA_RAL_COVERAGE
                sla_ral_coverage_t cov_t = COVERAGE_OFF,
                `endif
                int            bus_num=0,
                int            dev_num=0,
                int            func_num=0,
                int            offset=0,
                int            size=0,
                sla_ral_data_t reset_val=0,
                boolean_t      mon_enabled = SLA_FALSE
             );
      super.new();
      set_space_addr("CFG",undef);
      set_space_addr("MEM",undef);
      set_space_addr("IO",undef);
      set_space_addr("MSG",undef);
      set_space_addr("CREG",undef); 
      set_space_addr("LT_MEM",undef);    
 build();

      `ifdef SLA_RAL_COVERAGE
      `CONFIG_RAL_COVERAGE(regname, COVERAGE_ON, FD_RW_SAMPLE)
      if(_coverage_type != COVERAGE_OFF) begin
        if(_sample_type inside {FD_RO_SAMPLE, RO_SAMPLE})
        `CREATE_OP_SAMPLE_COV(RAL_WRITE)
        else if(_sample_type inside {FD_WO_SAMPLE, WO_SAMPLE})
        `CREATE_OP_SAMPLE_COV(RAL_READ)
        else
        `CREATE_OP_SAMPLE_COV(RAL_NONE)
        `CREATE_DESIRED_COV
        `CREATE_ACTUAL_COV
      end
      `endif

  endfunction

  `ifdef SLA_RAL_COVERAGE
  `CREATE_RAL_OP_COVERGROUP
  covergroup desired_cg @(cov_ev);
     option.per_instance = 1;
     option.name = {sla_ral_file::regfilename,".",regname};
     `RAL_FIELD_CP(PPOP, PPOP.desired)
     `RAL_FIELD_CP_8(PPOP, PPOP.desired, 0,1,2,3,4,5,6,7)
     `RAL_FIELD_CP(MID, MID.desired)
     `RAL_FIELD_CP_8(MID, MID.desired, 0,1,2,3,4,5,6,7)
     `RAL_FIELD_CP(MSID, MSID.desired)
     `RAL_FIELD_CP_8(MSID, MSID.desired, 0,1,2,3,4,5,6,7)
     `RAL_FIELD_CP(DPOP, DPOP.desired)
     `RAL_FIELD_CP_4(DPOP, DPOP.desired, 0,1,2,3)
     `RAL_FIELD_CP(RSVD_31_28, RSVD_31_28.desired)
     `RAL_FIELD_CP_4(RSVD_31_28, RSVD_31_28.desired, 0,1,2,3)
  endgroup
  covergroup actual_cg @(cov_ev);
     option.per_instance = 1;
     option.name = {sla_ral_file::regfilename,".",regname};
     `RAL_FIELD_CP(PPOP, PPOP.actual)
     `RAL_FIELD_CP_8(PPOP, PPOP.actual, 0,1,2,3,4,5,6,7)
     `RAL_FIELD_CP(MID, MID.actual)
     `RAL_FIELD_CP_8(MID, MID.actual, 0,1,2,3,4,5,6,7)
     `RAL_FIELD_CP(MSID, MSID.actual)
     `RAL_FIELD_CP_8(MSID, MSID.actual, 0,1,2,3,4,5,6,7)
     `RAL_FIELD_CP(DPOP, DPOP.actual)
     `RAL_FIELD_CP_4(DPOP, DPOP.actual, 0,1,2,3)
     `RAL_FIELD_CP(RSVD_31_28, RSVD_31_28.actual)
     `RAL_FIELD_CP_4(RSVD_31_28, RSVD_31_28.actual, 0,1,2,3)
  endgroup
  `endif

  // --------------------------
  // add constraint based on database legal values
  // --------------------------
  function void build();
    PPOP = new("PPOP", "RW", 8, 0, {"SBI_SETID_DW2.PPOP"});
    PPOP.set_powerwell("primary");
    PPOP.set_rand_mode(0);
   PPOP.set_reset_signame("pmu_rst_b");
    void'(add_field( PPOP ));

    MID = new("MID", "RW", 8, 8, {"SBI_SETID_DW2.MID"});
    MID.set_powerwell("primary");
    MID.set_rand_mode(0);
   MID.set_reset_signame("pmu_rst_b");
    void'(add_field( MID ));

    MSID = new("MSID", "RW", 8, 16, {"SBI_SETID_DW2.MSID"});
    MSID.set_powerwell("primary");
    MSID.set_rand_mode(0);
   MSID.set_reset_signame("pmu_rst_b");
    void'(add_field( MSID ));

    DPOP = new("DPOP", "RW", 4, 24, {"SBI_SETID_DW2.DPOP"});
    DPOP.set_powerwell("primary");
    DPOP.set_rand_mode(0);
   DPOP.set_reset_signame("pmu_rst_b");
    void'(add_field( DPOP ));

    RSVD_31_28 = new("RSVD_31_28", "RO", 4, 28, {"SBI_SETID_DW2.RSVD_31_28"});
    RSVD_31_28.set_powerwell("primary");
    RSVD_31_28.set_rand_mode(0);
    void'(add_field( RSVD_31_28 ));

  endfunction


`DEFINE_NONE_SCOPE_REG_API(pmu_mmr_SBI_SETID_DW2_reg) 
endclass : pmu_mmr_SBI_SETID_DW2_reg

// ================================================

class pmu_mmr_IP_BP_RP_ACK_STS_0_reg extends sla_ral_reg;

  // --------------------------
  sla_ral_field IP_BP_RP_ACK_STS_0_0;
  sla_ral_field IP_BP_RP_ACK_STS_0_1;
  sla_ral_field IP_BP_RP_ACK_STS_0_2;
  sla_ral_field IP_BP_RP_ACK_STS_0_3;
  sla_ral_field IP_BP_RP_ACK_STS_0_4;
  sla_ral_field IP_BP_RP_ACK_STS_0_5;
  sla_ral_field IP_BP_RP_ACK_STS_0_6;
  sla_ral_field IP_BP_RP_ACK_STS_0_7;
  sla_ral_field IP_BP_RP_ACK_STS_0_8;
  sla_ral_field IP_BP_RP_ACK_STS_0_9;
  sla_ral_field IP_BP_RP_ACK_STS_0_10;
  sla_ral_field IP_BP_RP_ACK_STS_0_11;
  sla_ral_field IP_BP_RP_ACK_STS_0_12;
  sla_ral_field IP_BP_RP_ACK_STS_0_13;
  sla_ral_field IP_BP_RP_ACK_STS_0_14;
  sla_ral_field IP_BP_RP_ACK_STS_0_15;
  sla_ral_field IP_BP_RP_ACK_STS_0_16;
  sla_ral_field IP_BP_RP_ACK_STS_0_17;
  sla_ral_field IP_BP_RP_ACK_STS_0_18;
  sla_ral_field IP_BP_RP_ACK_STS_0_19;
  sla_ral_field IP_BP_RP_ACK_STS_0_20;
  sla_ral_field IP_BP_RP_ACK_STS_0_21;
  sla_ral_field IP_BP_RP_ACK_STS_0_22;
  sla_ral_field IP_BP_RP_ACK_STS_0_23;
  sla_ral_field IP_BP_RP_ACK_STS_0_24;
  sla_ral_field IP_BP_RP_ACK_STS_0_25;
  sla_ral_field IP_BP_RP_ACK_STS_0_26;
  sla_ral_field IP_BP_RP_ACK_STS_0_27;
  sla_ral_field IP_BP_RP_ACK_STS_0_28;
  sla_ral_field IP_BP_RP_ACK_STS_0_29;
  sla_ral_field IP_BP_RP_ACK_STS_0_30;
  sla_ral_field IP_BP_RP_ACK_STS_0_31;

  // --------------------------
  `ovm_object_utils(pmu_mmr_IP_BP_RP_ACK_STS_0_reg)

  // --------------------------
  function new(
                string         name = "",
                `ifdef SLA_RAL_COVERAGE
                sla_ral_coverage_t cov_t = COVERAGE_OFF,
                `endif
                int            bus_num=0,
                int            dev_num=0,
                int            func_num=0,
                int            offset=0,
                int            size=0,
                sla_ral_data_t reset_val=0,
                boolean_t      mon_enabled = SLA_FALSE
             );
      super.new();
      set_space_addr("CFG",undef);
      set_space_addr("MEM",undef);
      set_space_addr("IO",undef);
      set_space_addr("MSG",undef);
      set_space_addr("CREG",undef); 
      set_space_addr("LT_MEM",undef);    
 build();

      `ifdef SLA_RAL_COVERAGE
      `CONFIG_RAL_COVERAGE(regname, COVERAGE_ON, FD_RW_SAMPLE)
      if(_coverage_type != COVERAGE_OFF) begin
        if(_sample_type inside {FD_RO_SAMPLE, RO_SAMPLE})
        `CREATE_OP_SAMPLE_COV(RAL_WRITE)
        else if(_sample_type inside {FD_WO_SAMPLE, WO_SAMPLE})
        `CREATE_OP_SAMPLE_COV(RAL_READ)
        else
        `CREATE_OP_SAMPLE_COV(RAL_NONE)
        `CREATE_DESIRED_COV
        `CREATE_ACTUAL_COV
      end
      `endif

  endfunction

  `ifdef SLA_RAL_COVERAGE
  `CREATE_RAL_OP_COVERGROUP
  covergroup desired_cg @(cov_ev);
     option.per_instance = 1;
     option.name = {sla_ral_file::regfilename,".",regname};
     `RAL_FIELD_CP(IP_BP_RP_ACK_STS_0_0, IP_BP_RP_ACK_STS_0_0.desired)
     `RAL_FIELD_CP_1(IP_BP_RP_ACK_STS_0_0, IP_BP_RP_ACK_STS_0_0.desired, 0)
     `RAL_FIELD_CP(IP_BP_RP_ACK_STS_0_1, IP_BP_RP_ACK_STS_0_1.desired)
     `RAL_FIELD_CP_1(IP_BP_RP_ACK_STS_0_1, IP_BP_RP_ACK_STS_0_1.desired, 0)
     `RAL_FIELD_CP(IP_BP_RP_ACK_STS_0_2, IP_BP_RP_ACK_STS_0_2.desired)
     `RAL_FIELD_CP_1(IP_BP_RP_ACK_STS_0_2, IP_BP_RP_ACK_STS_0_2.desired, 0)
     `RAL_FIELD_CP(IP_BP_RP_ACK_STS_0_3, IP_BP_RP_ACK_STS_0_3.desired)
     `RAL_FIELD_CP_1(IP_BP_RP_ACK_STS_0_3, IP_BP_RP_ACK_STS_0_3.desired, 0)
     `RAL_FIELD_CP(IP_BP_RP_ACK_STS_0_4, IP_BP_RP_ACK_STS_0_4.desired)
     `RAL_FIELD_CP_1(IP_BP_RP_ACK_STS_0_4, IP_BP_RP_ACK_STS_0_4.desired, 0)
     `RAL_FIELD_CP(IP_BP_RP_ACK_STS_0_5, IP_BP_RP_ACK_STS_0_5.desired)
     `RAL_FIELD_CP_1(IP_BP_RP_ACK_STS_0_5, IP_BP_RP_ACK_STS_0_5.desired, 0)
     `RAL_FIELD_CP(IP_BP_RP_ACK_STS_0_6, IP_BP_RP_ACK_STS_0_6.desired)
     `RAL_FIELD_CP_1(IP_BP_RP_ACK_STS_0_6, IP_BP_RP_ACK_STS_0_6.desired, 0)
     `RAL_FIELD_CP(IP_BP_RP_ACK_STS_0_7, IP_BP_RP_ACK_STS_0_7.desired)
     `RAL_FIELD_CP_1(IP_BP_RP_ACK_STS_0_7, IP_BP_RP_ACK_STS_0_7.desired, 0)
     `RAL_FIELD_CP(IP_BP_RP_ACK_STS_0_8, IP_BP_RP_ACK_STS_0_8.desired)
     `RAL_FIELD_CP_1(IP_BP_RP_ACK_STS_0_8, IP_BP_RP_ACK_STS_0_8.desired, 0)
     `RAL_FIELD_CP(IP_BP_RP_ACK_STS_0_9, IP_BP_RP_ACK_STS_0_9.desired)
     `RAL_FIELD_CP_1(IP_BP_RP_ACK_STS_0_9, IP_BP_RP_ACK_STS_0_9.desired, 0)
     `RAL_FIELD_CP(IP_BP_RP_ACK_STS_0_10, IP_BP_RP_ACK_STS_0_10.desired)
     `RAL_FIELD_CP_1(IP_BP_RP_ACK_STS_0_10, IP_BP_RP_ACK_STS_0_10.desired, 0)
     `RAL_FIELD_CP(IP_BP_RP_ACK_STS_0_11, IP_BP_RP_ACK_STS_0_11.desired)
     `RAL_FIELD_CP_1(IP_BP_RP_ACK_STS_0_11, IP_BP_RP_ACK_STS_0_11.desired, 0)
     `RAL_FIELD_CP(IP_BP_RP_ACK_STS_0_12, IP_BP_RP_ACK_STS_0_12.desired)
     `RAL_FIELD_CP_1(IP_BP_RP_ACK_STS_0_12, IP_BP_RP_ACK_STS_0_12.desired, 0)
     `RAL_FIELD_CP(IP_BP_RP_ACK_STS_0_13, IP_BP_RP_ACK_STS_0_13.desired)
     `RAL_FIELD_CP_1(IP_BP_RP_ACK_STS_0_13, IP_BP_RP_ACK_STS_0_13.desired, 0)
     `RAL_FIELD_CP(IP_BP_RP_ACK_STS_0_14, IP_BP_RP_ACK_STS_0_14.desired)
     `RAL_FIELD_CP_1(IP_BP_RP_ACK_STS_0_14, IP_BP_RP_ACK_STS_0_14.desired, 0)
     `RAL_FIELD_CP(IP_BP_RP_ACK_STS_0_15, IP_BP_RP_ACK_STS_0_15.desired)
     `RAL_FIELD_CP_1(IP_BP_RP_ACK_STS_0_15, IP_BP_RP_ACK_STS_0_15.desired, 0)
     `RAL_FIELD_CP(IP_BP_RP_ACK_STS_0_16, IP_BP_RP_ACK_STS_0_16.desired)
     `RAL_FIELD_CP_1(IP_BP_RP_ACK_STS_0_16, IP_BP_RP_ACK_STS_0_16.desired, 0)
     `RAL_FIELD_CP(IP_BP_RP_ACK_STS_0_17, IP_BP_RP_ACK_STS_0_17.desired)
     `RAL_FIELD_CP_1(IP_BP_RP_ACK_STS_0_17, IP_BP_RP_ACK_STS_0_17.desired, 0)
     `RAL_FIELD_CP(IP_BP_RP_ACK_STS_0_18, IP_BP_RP_ACK_STS_0_18.desired)
     `RAL_FIELD_CP_1(IP_BP_RP_ACK_STS_0_18, IP_BP_RP_ACK_STS_0_18.desired, 0)
     `RAL_FIELD_CP(IP_BP_RP_ACK_STS_0_19, IP_BP_RP_ACK_STS_0_19.desired)
     `RAL_FIELD_CP_1(IP_BP_RP_ACK_STS_0_19, IP_BP_RP_ACK_STS_0_19.desired, 0)
     `RAL_FIELD_CP(IP_BP_RP_ACK_STS_0_20, IP_BP_RP_ACK_STS_0_20.desired)
     `RAL_FIELD_CP_1(IP_BP_RP_ACK_STS_0_20, IP_BP_RP_ACK_STS_0_20.desired, 0)
     `RAL_FIELD_CP(IP_BP_RP_ACK_STS_0_21, IP_BP_RP_ACK_STS_0_21.desired)
     `RAL_FIELD_CP_1(IP_BP_RP_ACK_STS_0_21, IP_BP_RP_ACK_STS_0_21.desired, 0)
     `RAL_FIELD_CP(IP_BP_RP_ACK_STS_0_22, IP_BP_RP_ACK_STS_0_22.desired)
     `RAL_FIELD_CP_1(IP_BP_RP_ACK_STS_0_22, IP_BP_RP_ACK_STS_0_22.desired, 0)
     `RAL_FIELD_CP(IP_BP_RP_ACK_STS_0_23, IP_BP_RP_ACK_STS_0_23.desired)
     `RAL_FIELD_CP_1(IP_BP_RP_ACK_STS_0_23, IP_BP_RP_ACK_STS_0_23.desired, 0)
     `RAL_FIELD_CP(IP_BP_RP_ACK_STS_0_24, IP_BP_RP_ACK_STS_0_24.desired)
     `RAL_FIELD_CP_1(IP_BP_RP_ACK_STS_0_24, IP_BP_RP_ACK_STS_0_24.desired, 0)
     `RAL_FIELD_CP(IP_BP_RP_ACK_STS_0_25, IP_BP_RP_ACK_STS_0_25.desired)
     `RAL_FIELD_CP_1(IP_BP_RP_ACK_STS_0_25, IP_BP_RP_ACK_STS_0_25.desired, 0)
     `RAL_FIELD_CP(IP_BP_RP_ACK_STS_0_26, IP_BP_RP_ACK_STS_0_26.desired)
     `RAL_FIELD_CP_1(IP_BP_RP_ACK_STS_0_26, IP_BP_RP_ACK_STS_0_26.desired, 0)
     `RAL_FIELD_CP(IP_BP_RP_ACK_STS_0_27, IP_BP_RP_ACK_STS_0_27.desired)
     `RAL_FIELD_CP_1(IP_BP_RP_ACK_STS_0_27, IP_BP_RP_ACK_STS_0_27.desired, 0)
     `RAL_FIELD_CP(IP_BP_RP_ACK_STS_0_28, IP_BP_RP_ACK_STS_0_28.desired)
     `RAL_FIELD_CP_1(IP_BP_RP_ACK_STS_0_28, IP_BP_RP_ACK_STS_0_28.desired, 0)
     `RAL_FIELD_CP(IP_BP_RP_ACK_STS_0_29, IP_BP_RP_ACK_STS_0_29.desired)
     `RAL_FIELD_CP_1(IP_BP_RP_ACK_STS_0_29, IP_BP_RP_ACK_STS_0_29.desired, 0)
     `RAL_FIELD_CP(IP_BP_RP_ACK_STS_0_30, IP_BP_RP_ACK_STS_0_30.desired)
     `RAL_FIELD_CP_1(IP_BP_RP_ACK_STS_0_30, IP_BP_RP_ACK_STS_0_30.desired, 0)
     `RAL_FIELD_CP(IP_BP_RP_ACK_STS_0_31, IP_BP_RP_ACK_STS_0_31.desired)
     `RAL_FIELD_CP_1(IP_BP_RP_ACK_STS_0_31, IP_BP_RP_ACK_STS_0_31.desired, 0)
  endgroup
  covergroup actual_cg @(cov_ev);
     option.per_instance = 1;
     option.name = {sla_ral_file::regfilename,".",regname};
     `RAL_FIELD_CP(IP_BP_RP_ACK_STS_0_0, IP_BP_RP_ACK_STS_0_0.actual)
     `RAL_FIELD_CP_1(IP_BP_RP_ACK_STS_0_0, IP_BP_RP_ACK_STS_0_0.actual, 0)
     `RAL_FIELD_CP(IP_BP_RP_ACK_STS_0_1, IP_BP_RP_ACK_STS_0_1.actual)
     `RAL_FIELD_CP_1(IP_BP_RP_ACK_STS_0_1, IP_BP_RP_ACK_STS_0_1.actual, 0)
     `RAL_FIELD_CP(IP_BP_RP_ACK_STS_0_2, IP_BP_RP_ACK_STS_0_2.actual)
     `RAL_FIELD_CP_1(IP_BP_RP_ACK_STS_0_2, IP_BP_RP_ACK_STS_0_2.actual, 0)
     `RAL_FIELD_CP(IP_BP_RP_ACK_STS_0_3, IP_BP_RP_ACK_STS_0_3.actual)
     `RAL_FIELD_CP_1(IP_BP_RP_ACK_STS_0_3, IP_BP_RP_ACK_STS_0_3.actual, 0)
     `RAL_FIELD_CP(IP_BP_RP_ACK_STS_0_4, IP_BP_RP_ACK_STS_0_4.actual)
     `RAL_FIELD_CP_1(IP_BP_RP_ACK_STS_0_4, IP_BP_RP_ACK_STS_0_4.actual, 0)
     `RAL_FIELD_CP(IP_BP_RP_ACK_STS_0_5, IP_BP_RP_ACK_STS_0_5.actual)
     `RAL_FIELD_CP_1(IP_BP_RP_ACK_STS_0_5, IP_BP_RP_ACK_STS_0_5.actual, 0)
     `RAL_FIELD_CP(IP_BP_RP_ACK_STS_0_6, IP_BP_RP_ACK_STS_0_6.actual)
     `RAL_FIELD_CP_1(IP_BP_RP_ACK_STS_0_6, IP_BP_RP_ACK_STS_0_6.actual, 0)
     `RAL_FIELD_CP(IP_BP_RP_ACK_STS_0_7, IP_BP_RP_ACK_STS_0_7.actual)
     `RAL_FIELD_CP_1(IP_BP_RP_ACK_STS_0_7, IP_BP_RP_ACK_STS_0_7.actual, 0)
     `RAL_FIELD_CP(IP_BP_RP_ACK_STS_0_8, IP_BP_RP_ACK_STS_0_8.actual)
     `RAL_FIELD_CP_1(IP_BP_RP_ACK_STS_0_8, IP_BP_RP_ACK_STS_0_8.actual, 0)
     `RAL_FIELD_CP(IP_BP_RP_ACK_STS_0_9, IP_BP_RP_ACK_STS_0_9.actual)
     `RAL_FIELD_CP_1(IP_BP_RP_ACK_STS_0_9, IP_BP_RP_ACK_STS_0_9.actual, 0)
     `RAL_FIELD_CP(IP_BP_RP_ACK_STS_0_10, IP_BP_RP_ACK_STS_0_10.actual)
     `RAL_FIELD_CP_1(IP_BP_RP_ACK_STS_0_10, IP_BP_RP_ACK_STS_0_10.actual, 0)
     `RAL_FIELD_CP(IP_BP_RP_ACK_STS_0_11, IP_BP_RP_ACK_STS_0_11.actual)
     `RAL_FIELD_CP_1(IP_BP_RP_ACK_STS_0_11, IP_BP_RP_ACK_STS_0_11.actual, 0)
     `RAL_FIELD_CP(IP_BP_RP_ACK_STS_0_12, IP_BP_RP_ACK_STS_0_12.actual)
     `RAL_FIELD_CP_1(IP_BP_RP_ACK_STS_0_12, IP_BP_RP_ACK_STS_0_12.actual, 0)
     `RAL_FIELD_CP(IP_BP_RP_ACK_STS_0_13, IP_BP_RP_ACK_STS_0_13.actual)
     `RAL_FIELD_CP_1(IP_BP_RP_ACK_STS_0_13, IP_BP_RP_ACK_STS_0_13.actual, 0)
     `RAL_FIELD_CP(IP_BP_RP_ACK_STS_0_14, IP_BP_RP_ACK_STS_0_14.actual)
     `RAL_FIELD_CP_1(IP_BP_RP_ACK_STS_0_14, IP_BP_RP_ACK_STS_0_14.actual, 0)
     `RAL_FIELD_CP(IP_BP_RP_ACK_STS_0_15, IP_BP_RP_ACK_STS_0_15.actual)
     `RAL_FIELD_CP_1(IP_BP_RP_ACK_STS_0_15, IP_BP_RP_ACK_STS_0_15.actual, 0)
     `RAL_FIELD_CP(IP_BP_RP_ACK_STS_0_16, IP_BP_RP_ACK_STS_0_16.actual)
     `RAL_FIELD_CP_1(IP_BP_RP_ACK_STS_0_16, IP_BP_RP_ACK_STS_0_16.actual, 0)
     `RAL_FIELD_CP(IP_BP_RP_ACK_STS_0_17, IP_BP_RP_ACK_STS_0_17.actual)
     `RAL_FIELD_CP_1(IP_BP_RP_ACK_STS_0_17, IP_BP_RP_ACK_STS_0_17.actual, 0)
     `RAL_FIELD_CP(IP_BP_RP_ACK_STS_0_18, IP_BP_RP_ACK_STS_0_18.actual)
     `RAL_FIELD_CP_1(IP_BP_RP_ACK_STS_0_18, IP_BP_RP_ACK_STS_0_18.actual, 0)
     `RAL_FIELD_CP(IP_BP_RP_ACK_STS_0_19, IP_BP_RP_ACK_STS_0_19.actual)
     `RAL_FIELD_CP_1(IP_BP_RP_ACK_STS_0_19, IP_BP_RP_ACK_STS_0_19.actual, 0)
     `RAL_FIELD_CP(IP_BP_RP_ACK_STS_0_20, IP_BP_RP_ACK_STS_0_20.actual)
     `RAL_FIELD_CP_1(IP_BP_RP_ACK_STS_0_20, IP_BP_RP_ACK_STS_0_20.actual, 0)
     `RAL_FIELD_CP(IP_BP_RP_ACK_STS_0_21, IP_BP_RP_ACK_STS_0_21.actual)
     `RAL_FIELD_CP_1(IP_BP_RP_ACK_STS_0_21, IP_BP_RP_ACK_STS_0_21.actual, 0)
     `RAL_FIELD_CP(IP_BP_RP_ACK_STS_0_22, IP_BP_RP_ACK_STS_0_22.actual)
     `RAL_FIELD_CP_1(IP_BP_RP_ACK_STS_0_22, IP_BP_RP_ACK_STS_0_22.actual, 0)
     `RAL_FIELD_CP(IP_BP_RP_ACK_STS_0_23, IP_BP_RP_ACK_STS_0_23.actual)
     `RAL_FIELD_CP_1(IP_BP_RP_ACK_STS_0_23, IP_BP_RP_ACK_STS_0_23.actual, 0)
     `RAL_FIELD_CP(IP_BP_RP_ACK_STS_0_24, IP_BP_RP_ACK_STS_0_24.actual)
     `RAL_FIELD_CP_1(IP_BP_RP_ACK_STS_0_24, IP_BP_RP_ACK_STS_0_24.actual, 0)
     `RAL_FIELD_CP(IP_BP_RP_ACK_STS_0_25, IP_BP_RP_ACK_STS_0_25.actual)
     `RAL_FIELD_CP_1(IP_BP_RP_ACK_STS_0_25, IP_BP_RP_ACK_STS_0_25.actual, 0)
     `RAL_FIELD_CP(IP_BP_RP_ACK_STS_0_26, IP_BP_RP_ACK_STS_0_26.actual)
     `RAL_FIELD_CP_1(IP_BP_RP_ACK_STS_0_26, IP_BP_RP_ACK_STS_0_26.actual, 0)
     `RAL_FIELD_CP(IP_BP_RP_ACK_STS_0_27, IP_BP_RP_ACK_STS_0_27.actual)
     `RAL_FIELD_CP_1(IP_BP_RP_ACK_STS_0_27, IP_BP_RP_ACK_STS_0_27.actual, 0)
     `RAL_FIELD_CP(IP_BP_RP_ACK_STS_0_28, IP_BP_RP_ACK_STS_0_28.actual)
     `RAL_FIELD_CP_1(IP_BP_RP_ACK_STS_0_28, IP_BP_RP_ACK_STS_0_28.actual, 0)
     `RAL_FIELD_CP(IP_BP_RP_ACK_STS_0_29, IP_BP_RP_ACK_STS_0_29.actual)
     `RAL_FIELD_CP_1(IP_BP_RP_ACK_STS_0_29, IP_BP_RP_ACK_STS_0_29.actual, 0)
     `RAL_FIELD_CP(IP_BP_RP_ACK_STS_0_30, IP_BP_RP_ACK_STS_0_30.actual)
     `RAL_FIELD_CP_1(IP_BP_RP_ACK_STS_0_30, IP_BP_RP_ACK_STS_0_30.actual, 0)
     `RAL_FIELD_CP(IP_BP_RP_ACK_STS_0_31, IP_BP_RP_ACK_STS_0_31.actual)
     `RAL_FIELD_CP_1(IP_BP_RP_ACK_STS_0_31, IP_BP_RP_ACK_STS_0_31.actual, 0)
  endgroup
  `endif

  // --------------------------
  // add constraint based on database legal values
  // --------------------------
  function void build();
    IP_BP_RP_ACK_STS_0_0 = new("IP_BP_RP_ACK_STS_0_0", "RW/1C/V", 1, 0, {"IP_BP_RP_ACK_STS_0.IP_BP_RP_ACK_STS_0_0"});
    IP_BP_RP_ACK_STS_0_0.set_powerwell("primary");
    IP_BP_RP_ACK_STS_0_0.set_rand_mode(0);
   IP_BP_RP_ACK_STS_0_0.set_reset_signame("pmu_rst_b");
    void'(add_field( IP_BP_RP_ACK_STS_0_0 ));

    IP_BP_RP_ACK_STS_0_1 = new("IP_BP_RP_ACK_STS_0_1", "RW/1C/V", 1, 1, {"IP_BP_RP_ACK_STS_0.IP_BP_RP_ACK_STS_0_1"});
    IP_BP_RP_ACK_STS_0_1.set_powerwell("primary");
    IP_BP_RP_ACK_STS_0_1.set_rand_mode(0);
   IP_BP_RP_ACK_STS_0_1.set_reset_signame("pmu_rst_b");
    void'(add_field( IP_BP_RP_ACK_STS_0_1 ));

    IP_BP_RP_ACK_STS_0_2 = new("IP_BP_RP_ACK_STS_0_2", "RW/1C/V", 1, 2, {"IP_BP_RP_ACK_STS_0.IP_BP_RP_ACK_STS_0_2"});
    IP_BP_RP_ACK_STS_0_2.set_powerwell("primary");
    IP_BP_RP_ACK_STS_0_2.set_rand_mode(0);
   IP_BP_RP_ACK_STS_0_2.set_reset_signame("pmu_rst_b");
    void'(add_field( IP_BP_RP_ACK_STS_0_2 ));

    IP_BP_RP_ACK_STS_0_3 = new("IP_BP_RP_ACK_STS_0_3", "RW/1C/V", 1, 3, {"IP_BP_RP_ACK_STS_0.IP_BP_RP_ACK_STS_0_3"});
    IP_BP_RP_ACK_STS_0_3.set_powerwell("primary");
    IP_BP_RP_ACK_STS_0_3.set_rand_mode(0);
   IP_BP_RP_ACK_STS_0_3.set_reset_signame("pmu_rst_b");
    void'(add_field( IP_BP_RP_ACK_STS_0_3 ));

    IP_BP_RP_ACK_STS_0_4 = new("IP_BP_RP_ACK_STS_0_4", "RW/1C/V", 1, 4, {"IP_BP_RP_ACK_STS_0.IP_BP_RP_ACK_STS_0_4"});
    IP_BP_RP_ACK_STS_0_4.set_powerwell("primary");
    IP_BP_RP_ACK_STS_0_4.set_rand_mode(0);
   IP_BP_RP_ACK_STS_0_4.set_reset_signame("pmu_rst_b");
    void'(add_field( IP_BP_RP_ACK_STS_0_4 ));

    IP_BP_RP_ACK_STS_0_5 = new("IP_BP_RP_ACK_STS_0_5", "RW/1C/V", 1, 5, {"IP_BP_RP_ACK_STS_0.IP_BP_RP_ACK_STS_0_5"});
    IP_BP_RP_ACK_STS_0_5.set_powerwell("primary");
    IP_BP_RP_ACK_STS_0_5.set_rand_mode(0);
   IP_BP_RP_ACK_STS_0_5.set_reset_signame("pmu_rst_b");
    void'(add_field( IP_BP_RP_ACK_STS_0_5 ));

    IP_BP_RP_ACK_STS_0_6 = new("IP_BP_RP_ACK_STS_0_6", "RW/1C/V", 1, 6, {"IP_BP_RP_ACK_STS_0.IP_BP_RP_ACK_STS_0_6"});
    IP_BP_RP_ACK_STS_0_6.set_powerwell("primary");
    IP_BP_RP_ACK_STS_0_6.set_rand_mode(0);
   IP_BP_RP_ACK_STS_0_6.set_reset_signame("pmu_rst_b");
    void'(add_field( IP_BP_RP_ACK_STS_0_6 ));

    IP_BP_RP_ACK_STS_0_7 = new("IP_BP_RP_ACK_STS_0_7", "RW/1C/V", 1, 7, {"IP_BP_RP_ACK_STS_0.IP_BP_RP_ACK_STS_0_7"});
    IP_BP_RP_ACK_STS_0_7.set_powerwell("primary");
    IP_BP_RP_ACK_STS_0_7.set_rand_mode(0);
   IP_BP_RP_ACK_STS_0_7.set_reset_signame("pmu_rst_b");
    void'(add_field( IP_BP_RP_ACK_STS_0_7 ));

    IP_BP_RP_ACK_STS_0_8 = new("IP_BP_RP_ACK_STS_0_8", "RW/1C/V", 1, 8, {"IP_BP_RP_ACK_STS_0.IP_BP_RP_ACK_STS_0_8"});
    IP_BP_RP_ACK_STS_0_8.set_powerwell("primary");
    IP_BP_RP_ACK_STS_0_8.set_rand_mode(0);
   IP_BP_RP_ACK_STS_0_8.set_reset_signame("pmu_rst_b");
    void'(add_field( IP_BP_RP_ACK_STS_0_8 ));

    IP_BP_RP_ACK_STS_0_9 = new("IP_BP_RP_ACK_STS_0_9", "RW/1C/V", 1, 9, {"IP_BP_RP_ACK_STS_0.IP_BP_RP_ACK_STS_0_9"});
    IP_BP_RP_ACK_STS_0_9.set_powerwell("primary");
    IP_BP_RP_ACK_STS_0_9.set_rand_mode(0);
   IP_BP_RP_ACK_STS_0_9.set_reset_signame("pmu_rst_b");
    void'(add_field( IP_BP_RP_ACK_STS_0_9 ));

    IP_BP_RP_ACK_STS_0_10 = new("IP_BP_RP_ACK_STS_0_10", "RW/1C/V", 1, 10, {"IP_BP_RP_ACK_STS_0.IP_BP_RP_ACK_STS_0_10"});
    IP_BP_RP_ACK_STS_0_10.set_powerwell("primary");
    IP_BP_RP_ACK_STS_0_10.set_rand_mode(0);
   IP_BP_RP_ACK_STS_0_10.set_reset_signame("pmu_rst_b");
    void'(add_field( IP_BP_RP_ACK_STS_0_10 ));

    IP_BP_RP_ACK_STS_0_11 = new("IP_BP_RP_ACK_STS_0_11", "RW/1C/V", 1, 11, {"IP_BP_RP_ACK_STS_0.IP_BP_RP_ACK_STS_0_11"});
    IP_BP_RP_ACK_STS_0_11.set_powerwell("primary");
    IP_BP_RP_ACK_STS_0_11.set_rand_mode(0);
   IP_BP_RP_ACK_STS_0_11.set_reset_signame("pmu_rst_b");
    void'(add_field( IP_BP_RP_ACK_STS_0_11 ));

    IP_BP_RP_ACK_STS_0_12 = new("IP_BP_RP_ACK_STS_0_12", "RW/1C/V", 1, 12, {"IP_BP_RP_ACK_STS_0.IP_BP_RP_ACK_STS_0_12"});
    IP_BP_RP_ACK_STS_0_12.set_powerwell("primary");
    IP_BP_RP_ACK_STS_0_12.set_rand_mode(0);
   IP_BP_RP_ACK_STS_0_12.set_reset_signame("pmu_rst_b");
    void'(add_field( IP_BP_RP_ACK_STS_0_12 ));

    IP_BP_RP_ACK_STS_0_13 = new("IP_BP_RP_ACK_STS_0_13", "RW/1C/V", 1, 13, {"IP_BP_RP_ACK_STS_0.IP_BP_RP_ACK_STS_0_13"});
    IP_BP_RP_ACK_STS_0_13.set_powerwell("primary");
    IP_BP_RP_ACK_STS_0_13.set_rand_mode(0);
   IP_BP_RP_ACK_STS_0_13.set_reset_signame("pmu_rst_b");
    void'(add_field( IP_BP_RP_ACK_STS_0_13 ));

    IP_BP_RP_ACK_STS_0_14 = new("IP_BP_RP_ACK_STS_0_14", "RW/1C/V", 1, 14, {"IP_BP_RP_ACK_STS_0.IP_BP_RP_ACK_STS_0_14"});
    IP_BP_RP_ACK_STS_0_14.set_powerwell("primary");
    IP_BP_RP_ACK_STS_0_14.set_rand_mode(0);
   IP_BP_RP_ACK_STS_0_14.set_reset_signame("pmu_rst_b");
    void'(add_field( IP_BP_RP_ACK_STS_0_14 ));

    IP_BP_RP_ACK_STS_0_15 = new("IP_BP_RP_ACK_STS_0_15", "RW/1C/V", 1, 15, {"IP_BP_RP_ACK_STS_0.IP_BP_RP_ACK_STS_0_15"});
    IP_BP_RP_ACK_STS_0_15.set_powerwell("primary");
    IP_BP_RP_ACK_STS_0_15.set_rand_mode(0);
   IP_BP_RP_ACK_STS_0_15.set_reset_signame("pmu_rst_b");
    void'(add_field( IP_BP_RP_ACK_STS_0_15 ));

    IP_BP_RP_ACK_STS_0_16 = new("IP_BP_RP_ACK_STS_0_16", "RW/1C/V", 1, 16, {"IP_BP_RP_ACK_STS_0.IP_BP_RP_ACK_STS_0_16"});
    IP_BP_RP_ACK_STS_0_16.set_powerwell("primary");
    IP_BP_RP_ACK_STS_0_16.set_rand_mode(0);
   IP_BP_RP_ACK_STS_0_16.set_reset_signame("pmu_rst_b");
    void'(add_field( IP_BP_RP_ACK_STS_0_16 ));

    IP_BP_RP_ACK_STS_0_17 = new("IP_BP_RP_ACK_STS_0_17", "RW/1C/V", 1, 17, {"IP_BP_RP_ACK_STS_0.IP_BP_RP_ACK_STS_0_17"});
    IP_BP_RP_ACK_STS_0_17.set_powerwell("primary");
    IP_BP_RP_ACK_STS_0_17.set_rand_mode(0);
   IP_BP_RP_ACK_STS_0_17.set_reset_signame("pmu_rst_b");
    void'(add_field( IP_BP_RP_ACK_STS_0_17 ));

    IP_BP_RP_ACK_STS_0_18 = new("IP_BP_RP_ACK_STS_0_18", "RW/1C/V", 1, 18, {"IP_BP_RP_ACK_STS_0.IP_BP_RP_ACK_STS_0_18"});
    IP_BP_RP_ACK_STS_0_18.set_powerwell("primary");
    IP_BP_RP_ACK_STS_0_18.set_rand_mode(0);
   IP_BP_RP_ACK_STS_0_18.set_reset_signame("pmu_rst_b");
    void'(add_field( IP_BP_RP_ACK_STS_0_18 ));

    IP_BP_RP_ACK_STS_0_19 = new("IP_BP_RP_ACK_STS_0_19", "RW/1C/V", 1, 19, {"IP_BP_RP_ACK_STS_0.IP_BP_RP_ACK_STS_0_19"});
    IP_BP_RP_ACK_STS_0_19.set_powerwell("primary");
    IP_BP_RP_ACK_STS_0_19.set_rand_mode(0);
   IP_BP_RP_ACK_STS_0_19.set_reset_signame("pmu_rst_b");
    void'(add_field( IP_BP_RP_ACK_STS_0_19 ));

    IP_BP_RP_ACK_STS_0_20 = new("IP_BP_RP_ACK_STS_0_20", "RW/1C/V", 1, 20, {"IP_BP_RP_ACK_STS_0.IP_BP_RP_ACK_STS_0_20"});
    IP_BP_RP_ACK_STS_0_20.set_powerwell("primary");
    IP_BP_RP_ACK_STS_0_20.set_rand_mode(0);
   IP_BP_RP_ACK_STS_0_20.set_reset_signame("pmu_rst_b");
    void'(add_field( IP_BP_RP_ACK_STS_0_20 ));

    IP_BP_RP_ACK_STS_0_21 = new("IP_BP_RP_ACK_STS_0_21", "RW/1C/V", 1, 21, {"IP_BP_RP_ACK_STS_0.IP_BP_RP_ACK_STS_0_21"});
    IP_BP_RP_ACK_STS_0_21.set_powerwell("primary");
    IP_BP_RP_ACK_STS_0_21.set_rand_mode(0);
   IP_BP_RP_ACK_STS_0_21.set_reset_signame("pmu_rst_b");
    void'(add_field( IP_BP_RP_ACK_STS_0_21 ));

    IP_BP_RP_ACK_STS_0_22 = new("IP_BP_RP_ACK_STS_0_22", "RW/1C/V", 1, 22, {"IP_BP_RP_ACK_STS_0.IP_BP_RP_ACK_STS_0_22"});
    IP_BP_RP_ACK_STS_0_22.set_powerwell("primary");
    IP_BP_RP_ACK_STS_0_22.set_rand_mode(0);
   IP_BP_RP_ACK_STS_0_22.set_reset_signame("pmu_rst_b");
    void'(add_field( IP_BP_RP_ACK_STS_0_22 ));

    IP_BP_RP_ACK_STS_0_23 = new("IP_BP_RP_ACK_STS_0_23", "RW/1C/V", 1, 23, {"IP_BP_RP_ACK_STS_0.IP_BP_RP_ACK_STS_0_23"});
    IP_BP_RP_ACK_STS_0_23.set_powerwell("primary");
    IP_BP_RP_ACK_STS_0_23.set_rand_mode(0);
   IP_BP_RP_ACK_STS_0_23.set_reset_signame("pmu_rst_b");
    void'(add_field( IP_BP_RP_ACK_STS_0_23 ));

    IP_BP_RP_ACK_STS_0_24 = new("IP_BP_RP_ACK_STS_0_24", "RW/1C/V", 1, 24, {"IP_BP_RP_ACK_STS_0.IP_BP_RP_ACK_STS_0_24"});
    IP_BP_RP_ACK_STS_0_24.set_powerwell("primary");
    IP_BP_RP_ACK_STS_0_24.set_rand_mode(0);
   IP_BP_RP_ACK_STS_0_24.set_reset_signame("pmu_rst_b");
    void'(add_field( IP_BP_RP_ACK_STS_0_24 ));

    IP_BP_RP_ACK_STS_0_25 = new("IP_BP_RP_ACK_STS_0_25", "RW/1C/V", 1, 25, {"IP_BP_RP_ACK_STS_0.IP_BP_RP_ACK_STS_0_25"});
    IP_BP_RP_ACK_STS_0_25.set_powerwell("primary");
    IP_BP_RP_ACK_STS_0_25.set_rand_mode(0);
   IP_BP_RP_ACK_STS_0_25.set_reset_signame("pmu_rst_b");
    void'(add_field( IP_BP_RP_ACK_STS_0_25 ));

    IP_BP_RP_ACK_STS_0_26 = new("IP_BP_RP_ACK_STS_0_26", "RW/1C/V", 1, 26, {"IP_BP_RP_ACK_STS_0.IP_BP_RP_ACK_STS_0_26"});
    IP_BP_RP_ACK_STS_0_26.set_powerwell("primary");
    IP_BP_RP_ACK_STS_0_26.set_rand_mode(0);
   IP_BP_RP_ACK_STS_0_26.set_reset_signame("pmu_rst_b");
    void'(add_field( IP_BP_RP_ACK_STS_0_26 ));

    IP_BP_RP_ACK_STS_0_27 = new("IP_BP_RP_ACK_STS_0_27", "RW/1C/V", 1, 27, {"IP_BP_RP_ACK_STS_0.IP_BP_RP_ACK_STS_0_27"});
    IP_BP_RP_ACK_STS_0_27.set_powerwell("primary");
    IP_BP_RP_ACK_STS_0_27.set_rand_mode(0);
   IP_BP_RP_ACK_STS_0_27.set_reset_signame("pmu_rst_b");
    void'(add_field( IP_BP_RP_ACK_STS_0_27 ));

    IP_BP_RP_ACK_STS_0_28 = new("IP_BP_RP_ACK_STS_0_28", "RW/1C/V", 1, 28, {"IP_BP_RP_ACK_STS_0.IP_BP_RP_ACK_STS_0_28"});
    IP_BP_RP_ACK_STS_0_28.set_powerwell("primary");
    IP_BP_RP_ACK_STS_0_28.set_rand_mode(0);
   IP_BP_RP_ACK_STS_0_28.set_reset_signame("pmu_rst_b");
    void'(add_field( IP_BP_RP_ACK_STS_0_28 ));

    IP_BP_RP_ACK_STS_0_29 = new("IP_BP_RP_ACK_STS_0_29", "RW/1C/V", 1, 29, {"IP_BP_RP_ACK_STS_0.IP_BP_RP_ACK_STS_0_29"});
    IP_BP_RP_ACK_STS_0_29.set_powerwell("primary");
    IP_BP_RP_ACK_STS_0_29.set_rand_mode(0);
   IP_BP_RP_ACK_STS_0_29.set_reset_signame("pmu_rst_b");
    void'(add_field( IP_BP_RP_ACK_STS_0_29 ));

    IP_BP_RP_ACK_STS_0_30 = new("IP_BP_RP_ACK_STS_0_30", "RW/1C/V", 1, 30, {"IP_BP_RP_ACK_STS_0.IP_BP_RP_ACK_STS_0_30"});
    IP_BP_RP_ACK_STS_0_30.set_powerwell("primary");
    IP_BP_RP_ACK_STS_0_30.set_rand_mode(0);
   IP_BP_RP_ACK_STS_0_30.set_reset_signame("pmu_rst_b");
    void'(add_field( IP_BP_RP_ACK_STS_0_30 ));

    IP_BP_RP_ACK_STS_0_31 = new("IP_BP_RP_ACK_STS_0_31", "RW/1C/V", 1, 31, {"IP_BP_RP_ACK_STS_0.IP_BP_RP_ACK_STS_0_31"});
    IP_BP_RP_ACK_STS_0_31.set_powerwell("primary");
    IP_BP_RP_ACK_STS_0_31.set_rand_mode(0);
   IP_BP_RP_ACK_STS_0_31.set_reset_signame("pmu_rst_b");
    void'(add_field( IP_BP_RP_ACK_STS_0_31 ));

  endfunction


`DEFINE_NONE_SCOPE_REG_API(pmu_mmr_IP_BP_RP_ACK_STS_0_reg) 
endclass : pmu_mmr_IP_BP_RP_ACK_STS_0_reg

// ================================================

class pmu_mmr_IP_BP_RP_ACK_STS_1_reg extends sla_ral_reg;

  // --------------------------
  sla_ral_field IP_BP_RP_ACK_STS_1_0;
  sla_ral_field IP_BP_RP_ACK_STS_1_1;
  sla_ral_field IP_BP_RP_ACK_STS_1_2;
  sla_ral_field IP_BP_RP_ACK_STS_1_3;
  sla_ral_field IP_BP_RP_ACK_STS_1_4;
  sla_ral_field IP_BP_RP_ACK_STS_1_5;
  sla_ral_field IP_BP_RP_ACK_STS_1_6;
  sla_ral_field IP_BP_RP_ACK_STS_1_7;
  sla_ral_field IP_BP_RP_ACK_STS_1_8;
  sla_ral_field IP_BP_RP_ACK_STS_1_9;
  sla_ral_field IP_BP_RP_ACK_STS_1_10;
  sla_ral_field IP_BP_RP_ACK_STS_1_11;
  sla_ral_field IP_BP_RP_ACK_STS_1_12;
  sla_ral_field IP_BP_RP_ACK_STS_1_13;
  sla_ral_field IP_BP_RP_ACK_STS_1_14;
  sla_ral_field IP_BP_RP_ACK_STS_1_15;
  sla_ral_field IP_BP_RP_ACK_STS_1_16;
  sla_ral_field IP_BP_RP_ACK_STS_1_17;
  sla_ral_field IP_BP_RP_ACK_STS_1_18;
  sla_ral_field IP_BP_RP_ACK_STS_1_19;
  sla_ral_field IP_BP_RP_ACK_STS_1_20;
  sla_ral_field IP_BP_RP_ACK_STS_1_21;
  sla_ral_field IP_BP_RP_ACK_STS_1_22;
  sla_ral_field IP_BP_RP_ACK_STS_1_23;
  sla_ral_field IP_BP_RP_ACK_STS_1_24;
  sla_ral_field IP_BP_RP_ACK_STS_1_25;
  sla_ral_field IP_BP_RP_ACK_STS_1_26;
  sla_ral_field IP_BP_RP_ACK_STS_1_27;
  sla_ral_field IP_BP_RP_ACK_STS_1_28;
  sla_ral_field IP_BP_RP_ACK_STS_1_29;
  sla_ral_field IP_BP_RP_ACK_STS_1_30;
  sla_ral_field IP_BP_RP_ACK_STS_1_31;

  // --------------------------
  `ovm_object_utils(pmu_mmr_IP_BP_RP_ACK_STS_1_reg)

  // --------------------------
  function new(
                string         name = "",
                `ifdef SLA_RAL_COVERAGE
                sla_ral_coverage_t cov_t = COVERAGE_OFF,
                `endif
                int            bus_num=0,
                int            dev_num=0,
                int            func_num=0,
                int            offset=0,
                int            size=0,
                sla_ral_data_t reset_val=0,
                boolean_t      mon_enabled = SLA_FALSE
             );
      super.new();
      set_space_addr("CFG",undef);
      set_space_addr("MEM",undef);
      set_space_addr("IO",undef);
      set_space_addr("MSG",undef);
      set_space_addr("CREG",undef); 
      set_space_addr("LT_MEM",undef);    
 build();

      `ifdef SLA_RAL_COVERAGE
      `CONFIG_RAL_COVERAGE(regname, COVERAGE_ON, FD_RW_SAMPLE)
      if(_coverage_type != COVERAGE_OFF) begin
        if(_sample_type inside {FD_RO_SAMPLE, RO_SAMPLE})
        `CREATE_OP_SAMPLE_COV(RAL_WRITE)
        else if(_sample_type inside {FD_WO_SAMPLE, WO_SAMPLE})
        `CREATE_OP_SAMPLE_COV(RAL_READ)
        else
        `CREATE_OP_SAMPLE_COV(RAL_NONE)
        `CREATE_DESIRED_COV
        `CREATE_ACTUAL_COV
      end
      `endif

  endfunction

  `ifdef SLA_RAL_COVERAGE
  `CREATE_RAL_OP_COVERGROUP
  covergroup desired_cg @(cov_ev);
     option.per_instance = 1;
     option.name = {sla_ral_file::regfilename,".",regname};
     `RAL_FIELD_CP(IP_BP_RP_ACK_STS_1_0, IP_BP_RP_ACK_STS_1_0.desired)
     `RAL_FIELD_CP_1(IP_BP_RP_ACK_STS_1_0, IP_BP_RP_ACK_STS_1_0.desired, 0)
     `RAL_FIELD_CP(IP_BP_RP_ACK_STS_1_1, IP_BP_RP_ACK_STS_1_1.desired)
     `RAL_FIELD_CP_1(IP_BP_RP_ACK_STS_1_1, IP_BP_RP_ACK_STS_1_1.desired, 0)
     `RAL_FIELD_CP(IP_BP_RP_ACK_STS_1_2, IP_BP_RP_ACK_STS_1_2.desired)
     `RAL_FIELD_CP_1(IP_BP_RP_ACK_STS_1_2, IP_BP_RP_ACK_STS_1_2.desired, 0)
     `RAL_FIELD_CP(IP_BP_RP_ACK_STS_1_3, IP_BP_RP_ACK_STS_1_3.desired)
     `RAL_FIELD_CP_1(IP_BP_RP_ACK_STS_1_3, IP_BP_RP_ACK_STS_1_3.desired, 0)
     `RAL_FIELD_CP(IP_BP_RP_ACK_STS_1_4, IP_BP_RP_ACK_STS_1_4.desired)
     `RAL_FIELD_CP_1(IP_BP_RP_ACK_STS_1_4, IP_BP_RP_ACK_STS_1_4.desired, 0)
     `RAL_FIELD_CP(IP_BP_RP_ACK_STS_1_5, IP_BP_RP_ACK_STS_1_5.desired)
     `RAL_FIELD_CP_1(IP_BP_RP_ACK_STS_1_5, IP_BP_RP_ACK_STS_1_5.desired, 0)
     `RAL_FIELD_CP(IP_BP_RP_ACK_STS_1_6, IP_BP_RP_ACK_STS_1_6.desired)
     `RAL_FIELD_CP_1(IP_BP_RP_ACK_STS_1_6, IP_BP_RP_ACK_STS_1_6.desired, 0)
     `RAL_FIELD_CP(IP_BP_RP_ACK_STS_1_7, IP_BP_RP_ACK_STS_1_7.desired)
     `RAL_FIELD_CP_1(IP_BP_RP_ACK_STS_1_7, IP_BP_RP_ACK_STS_1_7.desired, 0)
     `RAL_FIELD_CP(IP_BP_RP_ACK_STS_1_8, IP_BP_RP_ACK_STS_1_8.desired)
     `RAL_FIELD_CP_1(IP_BP_RP_ACK_STS_1_8, IP_BP_RP_ACK_STS_1_8.desired, 0)
     `RAL_FIELD_CP(IP_BP_RP_ACK_STS_1_9, IP_BP_RP_ACK_STS_1_9.desired)
     `RAL_FIELD_CP_1(IP_BP_RP_ACK_STS_1_9, IP_BP_RP_ACK_STS_1_9.desired, 0)
     `RAL_FIELD_CP(IP_BP_RP_ACK_STS_1_10, IP_BP_RP_ACK_STS_1_10.desired)
     `RAL_FIELD_CP_1(IP_BP_RP_ACK_STS_1_10, IP_BP_RP_ACK_STS_1_10.desired, 0)
     `RAL_FIELD_CP(IP_BP_RP_ACK_STS_1_11, IP_BP_RP_ACK_STS_1_11.desired)
     `RAL_FIELD_CP_1(IP_BP_RP_ACK_STS_1_11, IP_BP_RP_ACK_STS_1_11.desired, 0)
     `RAL_FIELD_CP(IP_BP_RP_ACK_STS_1_12, IP_BP_RP_ACK_STS_1_12.desired)
     `RAL_FIELD_CP_1(IP_BP_RP_ACK_STS_1_12, IP_BP_RP_ACK_STS_1_12.desired, 0)
     `RAL_FIELD_CP(IP_BP_RP_ACK_STS_1_13, IP_BP_RP_ACK_STS_1_13.desired)
     `RAL_FIELD_CP_1(IP_BP_RP_ACK_STS_1_13, IP_BP_RP_ACK_STS_1_13.desired, 0)
     `RAL_FIELD_CP(IP_BP_RP_ACK_STS_1_14, IP_BP_RP_ACK_STS_1_14.desired)
     `RAL_FIELD_CP_1(IP_BP_RP_ACK_STS_1_14, IP_BP_RP_ACK_STS_1_14.desired, 0)
     `RAL_FIELD_CP(IP_BP_RP_ACK_STS_1_15, IP_BP_RP_ACK_STS_1_15.desired)
     `RAL_FIELD_CP_1(IP_BP_RP_ACK_STS_1_15, IP_BP_RP_ACK_STS_1_15.desired, 0)
     `RAL_FIELD_CP(IP_BP_RP_ACK_STS_1_16, IP_BP_RP_ACK_STS_1_16.desired)
     `RAL_FIELD_CP_1(IP_BP_RP_ACK_STS_1_16, IP_BP_RP_ACK_STS_1_16.desired, 0)
     `RAL_FIELD_CP(IP_BP_RP_ACK_STS_1_17, IP_BP_RP_ACK_STS_1_17.desired)
     `RAL_FIELD_CP_1(IP_BP_RP_ACK_STS_1_17, IP_BP_RP_ACK_STS_1_17.desired, 0)
     `RAL_FIELD_CP(IP_BP_RP_ACK_STS_1_18, IP_BP_RP_ACK_STS_1_18.desired)
     `RAL_FIELD_CP_1(IP_BP_RP_ACK_STS_1_18, IP_BP_RP_ACK_STS_1_18.desired, 0)
     `RAL_FIELD_CP(IP_BP_RP_ACK_STS_1_19, IP_BP_RP_ACK_STS_1_19.desired)
     `RAL_FIELD_CP_1(IP_BP_RP_ACK_STS_1_19, IP_BP_RP_ACK_STS_1_19.desired, 0)
     `RAL_FIELD_CP(IP_BP_RP_ACK_STS_1_20, IP_BP_RP_ACK_STS_1_20.desired)
     `RAL_FIELD_CP_1(IP_BP_RP_ACK_STS_1_20, IP_BP_RP_ACK_STS_1_20.desired, 0)
     `RAL_FIELD_CP(IP_BP_RP_ACK_STS_1_21, IP_BP_RP_ACK_STS_1_21.desired)
     `RAL_FIELD_CP_1(IP_BP_RP_ACK_STS_1_21, IP_BP_RP_ACK_STS_1_21.desired, 0)
     `RAL_FIELD_CP(IP_BP_RP_ACK_STS_1_22, IP_BP_RP_ACK_STS_1_22.desired)
     `RAL_FIELD_CP_1(IP_BP_RP_ACK_STS_1_22, IP_BP_RP_ACK_STS_1_22.desired, 0)
     `RAL_FIELD_CP(IP_BP_RP_ACK_STS_1_23, IP_BP_RP_ACK_STS_1_23.desired)
     `RAL_FIELD_CP_1(IP_BP_RP_ACK_STS_1_23, IP_BP_RP_ACK_STS_1_23.desired, 0)
     `RAL_FIELD_CP(IP_BP_RP_ACK_STS_1_24, IP_BP_RP_ACK_STS_1_24.desired)
     `RAL_FIELD_CP_1(IP_BP_RP_ACK_STS_1_24, IP_BP_RP_ACK_STS_1_24.desired, 0)
     `RAL_FIELD_CP(IP_BP_RP_ACK_STS_1_25, IP_BP_RP_ACK_STS_1_25.desired)
     `RAL_FIELD_CP_1(IP_BP_RP_ACK_STS_1_25, IP_BP_RP_ACK_STS_1_25.desired, 0)
     `RAL_FIELD_CP(IP_BP_RP_ACK_STS_1_26, IP_BP_RP_ACK_STS_1_26.desired)
     `RAL_FIELD_CP_1(IP_BP_RP_ACK_STS_1_26, IP_BP_RP_ACK_STS_1_26.desired, 0)
     `RAL_FIELD_CP(IP_BP_RP_ACK_STS_1_27, IP_BP_RP_ACK_STS_1_27.desired)
     `RAL_FIELD_CP_1(IP_BP_RP_ACK_STS_1_27, IP_BP_RP_ACK_STS_1_27.desired, 0)
     `RAL_FIELD_CP(IP_BP_RP_ACK_STS_1_28, IP_BP_RP_ACK_STS_1_28.desired)
     `RAL_FIELD_CP_1(IP_BP_RP_ACK_STS_1_28, IP_BP_RP_ACK_STS_1_28.desired, 0)
     `RAL_FIELD_CP(IP_BP_RP_ACK_STS_1_29, IP_BP_RP_ACK_STS_1_29.desired)
     `RAL_FIELD_CP_1(IP_BP_RP_ACK_STS_1_29, IP_BP_RP_ACK_STS_1_29.desired, 0)
     `RAL_FIELD_CP(IP_BP_RP_ACK_STS_1_30, IP_BP_RP_ACK_STS_1_30.desired)
     `RAL_FIELD_CP_1(IP_BP_RP_ACK_STS_1_30, IP_BP_RP_ACK_STS_1_30.desired, 0)
     `RAL_FIELD_CP(IP_BP_RP_ACK_STS_1_31, IP_BP_RP_ACK_STS_1_31.desired)
     `RAL_FIELD_CP_1(IP_BP_RP_ACK_STS_1_31, IP_BP_RP_ACK_STS_1_31.desired, 0)
  endgroup
  covergroup actual_cg @(cov_ev);
     option.per_instance = 1;
     option.name = {sla_ral_file::regfilename,".",regname};
     `RAL_FIELD_CP(IP_BP_RP_ACK_STS_1_0, IP_BP_RP_ACK_STS_1_0.actual)
     `RAL_FIELD_CP_1(IP_BP_RP_ACK_STS_1_0, IP_BP_RP_ACK_STS_1_0.actual, 0)
     `RAL_FIELD_CP(IP_BP_RP_ACK_STS_1_1, IP_BP_RP_ACK_STS_1_1.actual)
     `RAL_FIELD_CP_1(IP_BP_RP_ACK_STS_1_1, IP_BP_RP_ACK_STS_1_1.actual, 0)
     `RAL_FIELD_CP(IP_BP_RP_ACK_STS_1_2, IP_BP_RP_ACK_STS_1_2.actual)
     `RAL_FIELD_CP_1(IP_BP_RP_ACK_STS_1_2, IP_BP_RP_ACK_STS_1_2.actual, 0)
     `RAL_FIELD_CP(IP_BP_RP_ACK_STS_1_3, IP_BP_RP_ACK_STS_1_3.actual)
     `RAL_FIELD_CP_1(IP_BP_RP_ACK_STS_1_3, IP_BP_RP_ACK_STS_1_3.actual, 0)
     `RAL_FIELD_CP(IP_BP_RP_ACK_STS_1_4, IP_BP_RP_ACK_STS_1_4.actual)
     `RAL_FIELD_CP_1(IP_BP_RP_ACK_STS_1_4, IP_BP_RP_ACK_STS_1_4.actual, 0)
     `RAL_FIELD_CP(IP_BP_RP_ACK_STS_1_5, IP_BP_RP_ACK_STS_1_5.actual)
     `RAL_FIELD_CP_1(IP_BP_RP_ACK_STS_1_5, IP_BP_RP_ACK_STS_1_5.actual, 0)
     `RAL_FIELD_CP(IP_BP_RP_ACK_STS_1_6, IP_BP_RP_ACK_STS_1_6.actual)
     `RAL_FIELD_CP_1(IP_BP_RP_ACK_STS_1_6, IP_BP_RP_ACK_STS_1_6.actual, 0)
     `RAL_FIELD_CP(IP_BP_RP_ACK_STS_1_7, IP_BP_RP_ACK_STS_1_7.actual)
     `RAL_FIELD_CP_1(IP_BP_RP_ACK_STS_1_7, IP_BP_RP_ACK_STS_1_7.actual, 0)
     `RAL_FIELD_CP(IP_BP_RP_ACK_STS_1_8, IP_BP_RP_ACK_STS_1_8.actual)
     `RAL_FIELD_CP_1(IP_BP_RP_ACK_STS_1_8, IP_BP_RP_ACK_STS_1_8.actual, 0)
     `RAL_FIELD_CP(IP_BP_RP_ACK_STS_1_9, IP_BP_RP_ACK_STS_1_9.actual)
     `RAL_FIELD_CP_1(IP_BP_RP_ACK_STS_1_9, IP_BP_RP_ACK_STS_1_9.actual, 0)
     `RAL_FIELD_CP(IP_BP_RP_ACK_STS_1_10, IP_BP_RP_ACK_STS_1_10.actual)
     `RAL_FIELD_CP_1(IP_BP_RP_ACK_STS_1_10, IP_BP_RP_ACK_STS_1_10.actual, 0)
     `RAL_FIELD_CP(IP_BP_RP_ACK_STS_1_11, IP_BP_RP_ACK_STS_1_11.actual)
     `RAL_FIELD_CP_1(IP_BP_RP_ACK_STS_1_11, IP_BP_RP_ACK_STS_1_11.actual, 0)
     `RAL_FIELD_CP(IP_BP_RP_ACK_STS_1_12, IP_BP_RP_ACK_STS_1_12.actual)
     `RAL_FIELD_CP_1(IP_BP_RP_ACK_STS_1_12, IP_BP_RP_ACK_STS_1_12.actual, 0)
     `RAL_FIELD_CP(IP_BP_RP_ACK_STS_1_13, IP_BP_RP_ACK_STS_1_13.actual)
     `RAL_FIELD_CP_1(IP_BP_RP_ACK_STS_1_13, IP_BP_RP_ACK_STS_1_13.actual, 0)
     `RAL_FIELD_CP(IP_BP_RP_ACK_STS_1_14, IP_BP_RP_ACK_STS_1_14.actual)
     `RAL_FIELD_CP_1(IP_BP_RP_ACK_STS_1_14, IP_BP_RP_ACK_STS_1_14.actual, 0)
     `RAL_FIELD_CP(IP_BP_RP_ACK_STS_1_15, IP_BP_RP_ACK_STS_1_15.actual)
     `RAL_FIELD_CP_1(IP_BP_RP_ACK_STS_1_15, IP_BP_RP_ACK_STS_1_15.actual, 0)
     `RAL_FIELD_CP(IP_BP_RP_ACK_STS_1_16, IP_BP_RP_ACK_STS_1_16.actual)
     `RAL_FIELD_CP_1(IP_BP_RP_ACK_STS_1_16, IP_BP_RP_ACK_STS_1_16.actual, 0)
     `RAL_FIELD_CP(IP_BP_RP_ACK_STS_1_17, IP_BP_RP_ACK_STS_1_17.actual)
     `RAL_FIELD_CP_1(IP_BP_RP_ACK_STS_1_17, IP_BP_RP_ACK_STS_1_17.actual, 0)
     `RAL_FIELD_CP(IP_BP_RP_ACK_STS_1_18, IP_BP_RP_ACK_STS_1_18.actual)
     `RAL_FIELD_CP_1(IP_BP_RP_ACK_STS_1_18, IP_BP_RP_ACK_STS_1_18.actual, 0)
     `RAL_FIELD_CP(IP_BP_RP_ACK_STS_1_19, IP_BP_RP_ACK_STS_1_19.actual)
     `RAL_FIELD_CP_1(IP_BP_RP_ACK_STS_1_19, IP_BP_RP_ACK_STS_1_19.actual, 0)
     `RAL_FIELD_CP(IP_BP_RP_ACK_STS_1_20, IP_BP_RP_ACK_STS_1_20.actual)
     `RAL_FIELD_CP_1(IP_BP_RP_ACK_STS_1_20, IP_BP_RP_ACK_STS_1_20.actual, 0)
     `RAL_FIELD_CP(IP_BP_RP_ACK_STS_1_21, IP_BP_RP_ACK_STS_1_21.actual)
     `RAL_FIELD_CP_1(IP_BP_RP_ACK_STS_1_21, IP_BP_RP_ACK_STS_1_21.actual, 0)
     `RAL_FIELD_CP(IP_BP_RP_ACK_STS_1_22, IP_BP_RP_ACK_STS_1_22.actual)
     `RAL_FIELD_CP_1(IP_BP_RP_ACK_STS_1_22, IP_BP_RP_ACK_STS_1_22.actual, 0)
     `RAL_FIELD_CP(IP_BP_RP_ACK_STS_1_23, IP_BP_RP_ACK_STS_1_23.actual)
     `RAL_FIELD_CP_1(IP_BP_RP_ACK_STS_1_23, IP_BP_RP_ACK_STS_1_23.actual, 0)
     `RAL_FIELD_CP(IP_BP_RP_ACK_STS_1_24, IP_BP_RP_ACK_STS_1_24.actual)
     `RAL_FIELD_CP_1(IP_BP_RP_ACK_STS_1_24, IP_BP_RP_ACK_STS_1_24.actual, 0)
     `RAL_FIELD_CP(IP_BP_RP_ACK_STS_1_25, IP_BP_RP_ACK_STS_1_25.actual)
     `RAL_FIELD_CP_1(IP_BP_RP_ACK_STS_1_25, IP_BP_RP_ACK_STS_1_25.actual, 0)
     `RAL_FIELD_CP(IP_BP_RP_ACK_STS_1_26, IP_BP_RP_ACK_STS_1_26.actual)
     `RAL_FIELD_CP_1(IP_BP_RP_ACK_STS_1_26, IP_BP_RP_ACK_STS_1_26.actual, 0)
     `RAL_FIELD_CP(IP_BP_RP_ACK_STS_1_27, IP_BP_RP_ACK_STS_1_27.actual)
     `RAL_FIELD_CP_1(IP_BP_RP_ACK_STS_1_27, IP_BP_RP_ACK_STS_1_27.actual, 0)
     `RAL_FIELD_CP(IP_BP_RP_ACK_STS_1_28, IP_BP_RP_ACK_STS_1_28.actual)
     `RAL_FIELD_CP_1(IP_BP_RP_ACK_STS_1_28, IP_BP_RP_ACK_STS_1_28.actual, 0)
     `RAL_FIELD_CP(IP_BP_RP_ACK_STS_1_29, IP_BP_RP_ACK_STS_1_29.actual)
     `RAL_FIELD_CP_1(IP_BP_RP_ACK_STS_1_29, IP_BP_RP_ACK_STS_1_29.actual, 0)
     `RAL_FIELD_CP(IP_BP_RP_ACK_STS_1_30, IP_BP_RP_ACK_STS_1_30.actual)
     `RAL_FIELD_CP_1(IP_BP_RP_ACK_STS_1_30, IP_BP_RP_ACK_STS_1_30.actual, 0)
     `RAL_FIELD_CP(IP_BP_RP_ACK_STS_1_31, IP_BP_RP_ACK_STS_1_31.actual)
     `RAL_FIELD_CP_1(IP_BP_RP_ACK_STS_1_31, IP_BP_RP_ACK_STS_1_31.actual, 0)
  endgroup
  `endif

  // --------------------------
  // add constraint based on database legal values
  // --------------------------
  function void build();
    IP_BP_RP_ACK_STS_1_0 = new("IP_BP_RP_ACK_STS_1_0", "RW/1C/V", 1, 0, {"IP_BP_RP_ACK_STS_1.IP_BP_RP_ACK_STS_1_0"});
    IP_BP_RP_ACK_STS_1_0.set_powerwell("primary");
    IP_BP_RP_ACK_STS_1_0.set_rand_mode(0);
   IP_BP_RP_ACK_STS_1_0.set_reset_signame("pmu_rst_b");
    void'(add_field( IP_BP_RP_ACK_STS_1_0 ));

    IP_BP_RP_ACK_STS_1_1 = new("IP_BP_RP_ACK_STS_1_1", "RW/1C/V", 1, 1, {"IP_BP_RP_ACK_STS_1.IP_BP_RP_ACK_STS_1_1"});
    IP_BP_RP_ACK_STS_1_1.set_powerwell("primary");
    IP_BP_RP_ACK_STS_1_1.set_rand_mode(0);
   IP_BP_RP_ACK_STS_1_1.set_reset_signame("pmu_rst_b");
    void'(add_field( IP_BP_RP_ACK_STS_1_1 ));

    IP_BP_RP_ACK_STS_1_2 = new("IP_BP_RP_ACK_STS_1_2", "RW/1C/V", 1, 2, {"IP_BP_RP_ACK_STS_1.IP_BP_RP_ACK_STS_1_2"});
    IP_BP_RP_ACK_STS_1_2.set_powerwell("primary");
    IP_BP_RP_ACK_STS_1_2.set_rand_mode(0);
   IP_BP_RP_ACK_STS_1_2.set_reset_signame("pmu_rst_b");
    void'(add_field( IP_BP_RP_ACK_STS_1_2 ));

    IP_BP_RP_ACK_STS_1_3 = new("IP_BP_RP_ACK_STS_1_3", "RW/1C/V", 1, 3, {"IP_BP_RP_ACK_STS_1.IP_BP_RP_ACK_STS_1_3"});
    IP_BP_RP_ACK_STS_1_3.set_powerwell("primary");
    IP_BP_RP_ACK_STS_1_3.set_rand_mode(0);
   IP_BP_RP_ACK_STS_1_3.set_reset_signame("pmu_rst_b");
    void'(add_field( IP_BP_RP_ACK_STS_1_3 ));

    IP_BP_RP_ACK_STS_1_4 = new("IP_BP_RP_ACK_STS_1_4", "RW/1C/V", 1, 4, {"IP_BP_RP_ACK_STS_1.IP_BP_RP_ACK_STS_1_4"});
    IP_BP_RP_ACK_STS_1_4.set_powerwell("primary");
    IP_BP_RP_ACK_STS_1_4.set_rand_mode(0);
   IP_BP_RP_ACK_STS_1_4.set_reset_signame("pmu_rst_b");
    void'(add_field( IP_BP_RP_ACK_STS_1_4 ));

    IP_BP_RP_ACK_STS_1_5 = new("IP_BP_RP_ACK_STS_1_5", "RW/1C/V", 1, 5, {"IP_BP_RP_ACK_STS_1.IP_BP_RP_ACK_STS_1_5"});
    IP_BP_RP_ACK_STS_1_5.set_powerwell("primary");
    IP_BP_RP_ACK_STS_1_5.set_rand_mode(0);
   IP_BP_RP_ACK_STS_1_5.set_reset_signame("pmu_rst_b");
    void'(add_field( IP_BP_RP_ACK_STS_1_5 ));

    IP_BP_RP_ACK_STS_1_6 = new("IP_BP_RP_ACK_STS_1_6", "RW/1C/V", 1, 6, {"IP_BP_RP_ACK_STS_1.IP_BP_RP_ACK_STS_1_6"});
    IP_BP_RP_ACK_STS_1_6.set_powerwell("primary");
    IP_BP_RP_ACK_STS_1_6.set_rand_mode(0);
   IP_BP_RP_ACK_STS_1_6.set_reset_signame("pmu_rst_b");
    void'(add_field( IP_BP_RP_ACK_STS_1_6 ));

    IP_BP_RP_ACK_STS_1_7 = new("IP_BP_RP_ACK_STS_1_7", "RW/1C/V", 1, 7, {"IP_BP_RP_ACK_STS_1.IP_BP_RP_ACK_STS_1_7"});
    IP_BP_RP_ACK_STS_1_7.set_powerwell("primary");
    IP_BP_RP_ACK_STS_1_7.set_rand_mode(0);
   IP_BP_RP_ACK_STS_1_7.set_reset_signame("pmu_rst_b");
    void'(add_field( IP_BP_RP_ACK_STS_1_7 ));

    IP_BP_RP_ACK_STS_1_8 = new("IP_BP_RP_ACK_STS_1_8", "RW/1C/V", 1, 8, {"IP_BP_RP_ACK_STS_1.IP_BP_RP_ACK_STS_1_8"});
    IP_BP_RP_ACK_STS_1_8.set_powerwell("primary");
    IP_BP_RP_ACK_STS_1_8.set_rand_mode(0);
   IP_BP_RP_ACK_STS_1_8.set_reset_signame("pmu_rst_b");
    void'(add_field( IP_BP_RP_ACK_STS_1_8 ));

    IP_BP_RP_ACK_STS_1_9 = new("IP_BP_RP_ACK_STS_1_9", "RW/1C/V", 1, 9, {"IP_BP_RP_ACK_STS_1.IP_BP_RP_ACK_STS_1_9"});
    IP_BP_RP_ACK_STS_1_9.set_powerwell("primary");
    IP_BP_RP_ACK_STS_1_9.set_rand_mode(0);
   IP_BP_RP_ACK_STS_1_9.set_reset_signame("pmu_rst_b");
    void'(add_field( IP_BP_RP_ACK_STS_1_9 ));

    IP_BP_RP_ACK_STS_1_10 = new("IP_BP_RP_ACK_STS_1_10", "RW/1C/V", 1, 10, {"IP_BP_RP_ACK_STS_1.IP_BP_RP_ACK_STS_1_10"});
    IP_BP_RP_ACK_STS_1_10.set_powerwell("primary");
    IP_BP_RP_ACK_STS_1_10.set_rand_mode(0);
   IP_BP_RP_ACK_STS_1_10.set_reset_signame("pmu_rst_b");
    void'(add_field( IP_BP_RP_ACK_STS_1_10 ));

    IP_BP_RP_ACK_STS_1_11 = new("IP_BP_RP_ACK_STS_1_11", "RW/1C/V", 1, 11, {"IP_BP_RP_ACK_STS_1.IP_BP_RP_ACK_STS_1_11"});
    IP_BP_RP_ACK_STS_1_11.set_powerwell("primary");
    IP_BP_RP_ACK_STS_1_11.set_rand_mode(0);
   IP_BP_RP_ACK_STS_1_11.set_reset_signame("pmu_rst_b");
    void'(add_field( IP_BP_RP_ACK_STS_1_11 ));

    IP_BP_RP_ACK_STS_1_12 = new("IP_BP_RP_ACK_STS_1_12", "RW/1C/V", 1, 12, {"IP_BP_RP_ACK_STS_1.IP_BP_RP_ACK_STS_1_12"});
    IP_BP_RP_ACK_STS_1_12.set_powerwell("primary");
    IP_BP_RP_ACK_STS_1_12.set_rand_mode(0);
   IP_BP_RP_ACK_STS_1_12.set_reset_signame("pmu_rst_b");
    void'(add_field( IP_BP_RP_ACK_STS_1_12 ));

    IP_BP_RP_ACK_STS_1_13 = new("IP_BP_RP_ACK_STS_1_13", "RW/1C/V", 1, 13, {"IP_BP_RP_ACK_STS_1.IP_BP_RP_ACK_STS_1_13"});
    IP_BP_RP_ACK_STS_1_13.set_powerwell("primary");
    IP_BP_RP_ACK_STS_1_13.set_rand_mode(0);
   IP_BP_RP_ACK_STS_1_13.set_reset_signame("pmu_rst_b");
    void'(add_field( IP_BP_RP_ACK_STS_1_13 ));

    IP_BP_RP_ACK_STS_1_14 = new("IP_BP_RP_ACK_STS_1_14", "RW/1C/V", 1, 14, {"IP_BP_RP_ACK_STS_1.IP_BP_RP_ACK_STS_1_14"});
    IP_BP_RP_ACK_STS_1_14.set_powerwell("primary");
    IP_BP_RP_ACK_STS_1_14.set_rand_mode(0);
   IP_BP_RP_ACK_STS_1_14.set_reset_signame("pmu_rst_b");
    void'(add_field( IP_BP_RP_ACK_STS_1_14 ));

    IP_BP_RP_ACK_STS_1_15 = new("IP_BP_RP_ACK_STS_1_15", "RW/1C/V", 1, 15, {"IP_BP_RP_ACK_STS_1.IP_BP_RP_ACK_STS_1_15"});
    IP_BP_RP_ACK_STS_1_15.set_powerwell("primary");
    IP_BP_RP_ACK_STS_1_15.set_rand_mode(0);
   IP_BP_RP_ACK_STS_1_15.set_reset_signame("pmu_rst_b");
    void'(add_field( IP_BP_RP_ACK_STS_1_15 ));

    IP_BP_RP_ACK_STS_1_16 = new("IP_BP_RP_ACK_STS_1_16", "RW/1C/V", 1, 16, {"IP_BP_RP_ACK_STS_1.IP_BP_RP_ACK_STS_1_16"});
    IP_BP_RP_ACK_STS_1_16.set_powerwell("primary");
    IP_BP_RP_ACK_STS_1_16.set_rand_mode(0);
   IP_BP_RP_ACK_STS_1_16.set_reset_signame("pmu_rst_b");
    void'(add_field( IP_BP_RP_ACK_STS_1_16 ));

    IP_BP_RP_ACK_STS_1_17 = new("IP_BP_RP_ACK_STS_1_17", "RW/1C/V", 1, 17, {"IP_BP_RP_ACK_STS_1.IP_BP_RP_ACK_STS_1_17"});
    IP_BP_RP_ACK_STS_1_17.set_powerwell("primary");
    IP_BP_RP_ACK_STS_1_17.set_rand_mode(0);
   IP_BP_RP_ACK_STS_1_17.set_reset_signame("pmu_rst_b");
    void'(add_field( IP_BP_RP_ACK_STS_1_17 ));

    IP_BP_RP_ACK_STS_1_18 = new("IP_BP_RP_ACK_STS_1_18", "RW/1C/V", 1, 18, {"IP_BP_RP_ACK_STS_1.IP_BP_RP_ACK_STS_1_18"});
    IP_BP_RP_ACK_STS_1_18.set_powerwell("primary");
    IP_BP_RP_ACK_STS_1_18.set_rand_mode(0);
   IP_BP_RP_ACK_STS_1_18.set_reset_signame("pmu_rst_b");
    void'(add_field( IP_BP_RP_ACK_STS_1_18 ));

    IP_BP_RP_ACK_STS_1_19 = new("IP_BP_RP_ACK_STS_1_19", "RW/1C/V", 1, 19, {"IP_BP_RP_ACK_STS_1.IP_BP_RP_ACK_STS_1_19"});
    IP_BP_RP_ACK_STS_1_19.set_powerwell("primary");
    IP_BP_RP_ACK_STS_1_19.set_rand_mode(0);
   IP_BP_RP_ACK_STS_1_19.set_reset_signame("pmu_rst_b");
    void'(add_field( IP_BP_RP_ACK_STS_1_19 ));

    IP_BP_RP_ACK_STS_1_20 = new("IP_BP_RP_ACK_STS_1_20", "RW/1C/V", 1, 20, {"IP_BP_RP_ACK_STS_1.IP_BP_RP_ACK_STS_1_20"});
    IP_BP_RP_ACK_STS_1_20.set_powerwell("primary");
    IP_BP_RP_ACK_STS_1_20.set_rand_mode(0);
   IP_BP_RP_ACK_STS_1_20.set_reset_signame("pmu_rst_b");
    void'(add_field( IP_BP_RP_ACK_STS_1_20 ));

    IP_BP_RP_ACK_STS_1_21 = new("IP_BP_RP_ACK_STS_1_21", "RW/1C/V", 1, 21, {"IP_BP_RP_ACK_STS_1.IP_BP_RP_ACK_STS_1_21"});
    IP_BP_RP_ACK_STS_1_21.set_powerwell("primary");
    IP_BP_RP_ACK_STS_1_21.set_rand_mode(0);
   IP_BP_RP_ACK_STS_1_21.set_reset_signame("pmu_rst_b");
    void'(add_field( IP_BP_RP_ACK_STS_1_21 ));

    IP_BP_RP_ACK_STS_1_22 = new("IP_BP_RP_ACK_STS_1_22", "RW/1C/V", 1, 22, {"IP_BP_RP_ACK_STS_1.IP_BP_RP_ACK_STS_1_22"});
    IP_BP_RP_ACK_STS_1_22.set_powerwell("primary");
    IP_BP_RP_ACK_STS_1_22.set_rand_mode(0);
   IP_BP_RP_ACK_STS_1_22.set_reset_signame("pmu_rst_b");
    void'(add_field( IP_BP_RP_ACK_STS_1_22 ));

    IP_BP_RP_ACK_STS_1_23 = new("IP_BP_RP_ACK_STS_1_23", "RW/1C/V", 1, 23, {"IP_BP_RP_ACK_STS_1.IP_BP_RP_ACK_STS_1_23"});
    IP_BP_RP_ACK_STS_1_23.set_powerwell("primary");
    IP_BP_RP_ACK_STS_1_23.set_rand_mode(0);
   IP_BP_RP_ACK_STS_1_23.set_reset_signame("pmu_rst_b");
    void'(add_field( IP_BP_RP_ACK_STS_1_23 ));

    IP_BP_RP_ACK_STS_1_24 = new("IP_BP_RP_ACK_STS_1_24", "RW/1C/V", 1, 24, {"IP_BP_RP_ACK_STS_1.IP_BP_RP_ACK_STS_1_24"});
    IP_BP_RP_ACK_STS_1_24.set_powerwell("primary");
    IP_BP_RP_ACK_STS_1_24.set_rand_mode(0);
   IP_BP_RP_ACK_STS_1_24.set_reset_signame("pmu_rst_b");
    void'(add_field( IP_BP_RP_ACK_STS_1_24 ));

    IP_BP_RP_ACK_STS_1_25 = new("IP_BP_RP_ACK_STS_1_25", "RW/1C/V", 1, 25, {"IP_BP_RP_ACK_STS_1.IP_BP_RP_ACK_STS_1_25"});
    IP_BP_RP_ACK_STS_1_25.set_powerwell("primary");
    IP_BP_RP_ACK_STS_1_25.set_rand_mode(0);
   IP_BP_RP_ACK_STS_1_25.set_reset_signame("pmu_rst_b");
    void'(add_field( IP_BP_RP_ACK_STS_1_25 ));

    IP_BP_RP_ACK_STS_1_26 = new("IP_BP_RP_ACK_STS_1_26", "RW/1C/V", 1, 26, {"IP_BP_RP_ACK_STS_1.IP_BP_RP_ACK_STS_1_26"});
    IP_BP_RP_ACK_STS_1_26.set_powerwell("primary");
    IP_BP_RP_ACK_STS_1_26.set_rand_mode(0);
   IP_BP_RP_ACK_STS_1_26.set_reset_signame("pmu_rst_b");
    void'(add_field( IP_BP_RP_ACK_STS_1_26 ));

    IP_BP_RP_ACK_STS_1_27 = new("IP_BP_RP_ACK_STS_1_27", "RW/1C/V", 1, 27, {"IP_BP_RP_ACK_STS_1.IP_BP_RP_ACK_STS_1_27"});
    IP_BP_RP_ACK_STS_1_27.set_powerwell("primary");
    IP_BP_RP_ACK_STS_1_27.set_rand_mode(0);
   IP_BP_RP_ACK_STS_1_27.set_reset_signame("pmu_rst_b");
    void'(add_field( IP_BP_RP_ACK_STS_1_27 ));

    IP_BP_RP_ACK_STS_1_28 = new("IP_BP_RP_ACK_STS_1_28", "RW/1C/V", 1, 28, {"IP_BP_RP_ACK_STS_1.IP_BP_RP_ACK_STS_1_28"});
    IP_BP_RP_ACK_STS_1_28.set_powerwell("primary");
    IP_BP_RP_ACK_STS_1_28.set_rand_mode(0);
   IP_BP_RP_ACK_STS_1_28.set_reset_signame("pmu_rst_b");
    void'(add_field( IP_BP_RP_ACK_STS_1_28 ));

    IP_BP_RP_ACK_STS_1_29 = new("IP_BP_RP_ACK_STS_1_29", "RW/1C/V", 1, 29, {"IP_BP_RP_ACK_STS_1.IP_BP_RP_ACK_STS_1_29"});
    IP_BP_RP_ACK_STS_1_29.set_powerwell("primary");
    IP_BP_RP_ACK_STS_1_29.set_rand_mode(0);
   IP_BP_RP_ACK_STS_1_29.set_reset_signame("pmu_rst_b");
    void'(add_field( IP_BP_RP_ACK_STS_1_29 ));

    IP_BP_RP_ACK_STS_1_30 = new("IP_BP_RP_ACK_STS_1_30", "RW/1C/V", 1, 30, {"IP_BP_RP_ACK_STS_1.IP_BP_RP_ACK_STS_1_30"});
    IP_BP_RP_ACK_STS_1_30.set_powerwell("primary");
    IP_BP_RP_ACK_STS_1_30.set_rand_mode(0);
   IP_BP_RP_ACK_STS_1_30.set_reset_signame("pmu_rst_b");
    void'(add_field( IP_BP_RP_ACK_STS_1_30 ));

    IP_BP_RP_ACK_STS_1_31 = new("IP_BP_RP_ACK_STS_1_31", "RW/1C/V", 1, 31, {"IP_BP_RP_ACK_STS_1.IP_BP_RP_ACK_STS_1_31"});
    IP_BP_RP_ACK_STS_1_31.set_powerwell("primary");
    IP_BP_RP_ACK_STS_1_31.set_rand_mode(0);
   IP_BP_RP_ACK_STS_1_31.set_reset_signame("pmu_rst_b");
    void'(add_field( IP_BP_RP_ACK_STS_1_31 ));

  endfunction


`DEFINE_NONE_SCOPE_REG_API(pmu_mmr_IP_BP_RP_ACK_STS_1_reg) 
endclass : pmu_mmr_IP_BP_RP_ACK_STS_1_reg

// ================================================

class pmu_mmr_IP_SB_CPL_STS_MASK_0_reg extends sla_ral_reg;

  // --------------------------
  sla_ral_field IP_SB_CPL_STS_MASK;

  // --------------------------
  `ovm_object_utils(pmu_mmr_IP_SB_CPL_STS_MASK_0_reg)

  // --------------------------
  function new(
                string         name = "",
                `ifdef SLA_RAL_COVERAGE
                sla_ral_coverage_t cov_t = COVERAGE_OFF,
                `endif
                int            bus_num=0,
                int            dev_num=0,
                int            func_num=0,
                int            offset=0,
                int            size=0,
                sla_ral_data_t reset_val=0,
                boolean_t      mon_enabled = SLA_FALSE
             );
      super.new();
      set_space_addr("CFG",undef);
      set_space_addr("MEM",undef);
      set_space_addr("IO",undef);
      set_space_addr("MSG",undef);
      set_space_addr("CREG",undef); 
      set_space_addr("LT_MEM",undef);    
 build();

      `ifdef SLA_RAL_COVERAGE
      `CONFIG_RAL_COVERAGE(regname, COVERAGE_ON, FD_RW_SAMPLE)
      if(_coverage_type != COVERAGE_OFF) begin
        if(_sample_type inside {FD_RO_SAMPLE, RO_SAMPLE})
        `CREATE_OP_SAMPLE_COV(RAL_WRITE)
        else if(_sample_type inside {FD_WO_SAMPLE, WO_SAMPLE})
        `CREATE_OP_SAMPLE_COV(RAL_READ)
        else
        `CREATE_OP_SAMPLE_COV(RAL_NONE)
        `CREATE_DESIRED_COV
        `CREATE_ACTUAL_COV
      end
      `endif

  endfunction

  `ifdef SLA_RAL_COVERAGE
  `CREATE_RAL_OP_COVERGROUP
  covergroup desired_cg @(cov_ev);
     option.per_instance = 1;
     option.name = {sla_ral_file::regfilename,".",regname};
     `RAL_FIELD_CP(IP_SB_CPL_STS_MASK, IP_SB_CPL_STS_MASK.desired)
     `RAL_FIELD_CP_16(IP_SB_CPL_STS_MASK, IP_SB_CPL_STS_MASK.desired, 0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)
     `RAL_FIELD_CP_16(IP_SB_CPL_STS_MASK, IP_SB_CPL_STS_MASK.desired, 16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31)
  endgroup
  covergroup actual_cg @(cov_ev);
     option.per_instance = 1;
     option.name = {sla_ral_file::regfilename,".",regname};
     `RAL_FIELD_CP(IP_SB_CPL_STS_MASK, IP_SB_CPL_STS_MASK.actual)
     `RAL_FIELD_CP_16(IP_SB_CPL_STS_MASK, IP_SB_CPL_STS_MASK.actual, 0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)
     `RAL_FIELD_CP_16(IP_SB_CPL_STS_MASK, IP_SB_CPL_STS_MASK.actual, 16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31)
  endgroup
  `endif

  // --------------------------
  // add constraint based on database legal values
  // --------------------------
  function void build();
    IP_SB_CPL_STS_MASK = new("IP_SB_CPL_STS_MASK", "RW", 32, 0, {"IP_SB_CPL_STS_MASK_0.IP_SB_CPL_STS_MASK"});
    IP_SB_CPL_STS_MASK.set_powerwell("primary");
    IP_SB_CPL_STS_MASK.set_rand_mode(0);
   IP_SB_CPL_STS_MASK.set_reset_signame("pmu_rst_b");
    void'(add_field( IP_SB_CPL_STS_MASK ));

  endfunction


`DEFINE_NONE_SCOPE_REG_API(pmu_mmr_IP_SB_CPL_STS_MASK_0_reg) 
endclass : pmu_mmr_IP_SB_CPL_STS_MASK_0_reg

// ================================================

class pmu_mmr_IP_SB_CPL_STS_MASK_1_reg extends sla_ral_reg;

  // --------------------------
  sla_ral_field IP_SB_CPL_STS_MASK;

  // --------------------------
  `ovm_object_utils(pmu_mmr_IP_SB_CPL_STS_MASK_1_reg)

  // --------------------------
  function new(
                string         name = "",
                `ifdef SLA_RAL_COVERAGE
                sla_ral_coverage_t cov_t = COVERAGE_OFF,
                `endif
                int            bus_num=0,
                int            dev_num=0,
                int            func_num=0,
                int            offset=0,
                int            size=0,
                sla_ral_data_t reset_val=0,
                boolean_t      mon_enabled = SLA_FALSE
             );
      super.new();
      set_space_addr("CFG",undef);
      set_space_addr("MEM",undef);
      set_space_addr("IO",undef);
      set_space_addr("MSG",undef);
      set_space_addr("CREG",undef); 
      set_space_addr("LT_MEM",undef);    
 build();

      `ifdef SLA_RAL_COVERAGE
      `CONFIG_RAL_COVERAGE(regname, COVERAGE_ON, FD_RW_SAMPLE)
      if(_coverage_type != COVERAGE_OFF) begin
        if(_sample_type inside {FD_RO_SAMPLE, RO_SAMPLE})
        `CREATE_OP_SAMPLE_COV(RAL_WRITE)
        else if(_sample_type inside {FD_WO_SAMPLE, WO_SAMPLE})
        `CREATE_OP_SAMPLE_COV(RAL_READ)
        else
        `CREATE_OP_SAMPLE_COV(RAL_NONE)
        `CREATE_DESIRED_COV
        `CREATE_ACTUAL_COV
      end
      `endif

  endfunction

  `ifdef SLA_RAL_COVERAGE
  `CREATE_RAL_OP_COVERGROUP
  covergroup desired_cg @(cov_ev);
     option.per_instance = 1;
     option.name = {sla_ral_file::regfilename,".",regname};
     `RAL_FIELD_CP(IP_SB_CPL_STS_MASK, IP_SB_CPL_STS_MASK.desired)
     `RAL_FIELD_CP_16(IP_SB_CPL_STS_MASK, IP_SB_CPL_STS_MASK.desired, 0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)
     `RAL_FIELD_CP_16(IP_SB_CPL_STS_MASK, IP_SB_CPL_STS_MASK.desired, 16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31)
  endgroup
  covergroup actual_cg @(cov_ev);
     option.per_instance = 1;
     option.name = {sla_ral_file::regfilename,".",regname};
     `RAL_FIELD_CP(IP_SB_CPL_STS_MASK, IP_SB_CPL_STS_MASK.actual)
     `RAL_FIELD_CP_16(IP_SB_CPL_STS_MASK, IP_SB_CPL_STS_MASK.actual, 0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)
     `RAL_FIELD_CP_16(IP_SB_CPL_STS_MASK, IP_SB_CPL_STS_MASK.actual, 16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31)
  endgroup
  `endif

  // --------------------------
  // add constraint based on database legal values
  // --------------------------
  function void build();
    IP_SB_CPL_STS_MASK = new("IP_SB_CPL_STS_MASK", "RW", 32, 0, {"IP_SB_CPL_STS_MASK_1.IP_SB_CPL_STS_MASK"});
    IP_SB_CPL_STS_MASK.set_powerwell("primary");
    IP_SB_CPL_STS_MASK.set_rand_mode(0);
   IP_SB_CPL_STS_MASK.set_reset_signame("pmu_rst_b");
    void'(add_field( IP_SB_CPL_STS_MASK ));

  endfunction


`DEFINE_NONE_SCOPE_REG_API(pmu_mmr_IP_SB_CPL_STS_MASK_1_reg) 
endclass : pmu_mmr_IP_SB_CPL_STS_MASK_1_reg

// ================================================

class pmu_mmr_IP_SB_CPL_STS_0_reg extends sla_ral_reg;

  // --------------------------
  sla_ral_field IP_SB_CPL_STS_0_0;
  sla_ral_field IP_SB_CPL_STS_0_1;
  sla_ral_field IP_SB_CPL_STS_0_2;
  sla_ral_field IP_SB_CPL_STS_0_3;
  sla_ral_field IP_SB_CPL_STS_0_4;
  sla_ral_field IP_SB_CPL_STS_0_5;
  sla_ral_field IP_SB_CPL_STS_0_6;
  sla_ral_field IP_SB_CPL_STS_0_7;
  sla_ral_field IP_SB_CPL_STS_0_8;
  sla_ral_field IP_SB_CPL_STS_0_9;
  sla_ral_field IP_SB_CPL_STS_0_10;
  sla_ral_field IP_SB_CPL_STS_0_11;
  sla_ral_field IP_SB_CPL_STS_0_12;
  sla_ral_field IP_SB_CPL_STS_0_13;
  sla_ral_field IP_SB_CPL_STS_0_14;
  sla_ral_field IP_SB_CPL_STS_0_15;
  sla_ral_field IP_SB_CPL_STS_0_16;
  sla_ral_field IP_SB_CPL_STS_0_17;
  sla_ral_field IP_SB_CPL_STS_0_18;
  sla_ral_field IP_SB_CPL_STS_0_19;
  sla_ral_field IP_SB_CPL_STS_0_20;
  sla_ral_field IP_SB_CPL_STS_0_21;
  sla_ral_field IP_SB_CPL_STS_0_22;
  sla_ral_field IP_SB_CPL_STS_0_23;
  sla_ral_field IP_SB_CPL_STS_0_24;
  sla_ral_field IP_SB_CPL_STS_0_25;
  sla_ral_field IP_SB_CPL_STS_0_26;
  sla_ral_field IP_SB_CPL_STS_0_27;
  sla_ral_field IP_SB_CPL_STS_0_28;
  sla_ral_field IP_SB_CPL_STS_0_29;
  sla_ral_field IP_SB_CPL_STS_0_30;
  sla_ral_field IP_SB_CPL_STS_0_31;

  // --------------------------
  `ovm_object_utils(pmu_mmr_IP_SB_CPL_STS_0_reg)

  // --------------------------
  function new(
                string         name = "",
                `ifdef SLA_RAL_COVERAGE
                sla_ral_coverage_t cov_t = COVERAGE_OFF,
                `endif
                int            bus_num=0,
                int            dev_num=0,
                int            func_num=0,
                int            offset=0,
                int            size=0,
                sla_ral_data_t reset_val=0,
                boolean_t      mon_enabled = SLA_FALSE
             );
      super.new();
      set_space_addr("CFG",undef);
      set_space_addr("MEM",undef);
      set_space_addr("IO",undef);
      set_space_addr("MSG",undef);
      set_space_addr("CREG",undef); 
      set_space_addr("LT_MEM",undef);    
 build();

      `ifdef SLA_RAL_COVERAGE
      `CONFIG_RAL_COVERAGE(regname, COVERAGE_ON, FD_RW_SAMPLE)
      if(_coverage_type != COVERAGE_OFF) begin
        if(_sample_type inside {FD_RO_SAMPLE, RO_SAMPLE})
        `CREATE_OP_SAMPLE_COV(RAL_WRITE)
        else if(_sample_type inside {FD_WO_SAMPLE, WO_SAMPLE})
        `CREATE_OP_SAMPLE_COV(RAL_READ)
        else
        `CREATE_OP_SAMPLE_COV(RAL_NONE)
        `CREATE_DESIRED_COV
        `CREATE_ACTUAL_COV
      end
      `endif

  endfunction

  `ifdef SLA_RAL_COVERAGE
  `CREATE_RAL_OP_COVERGROUP
  covergroup desired_cg @(cov_ev);
     option.per_instance = 1;
     option.name = {sla_ral_file::regfilename,".",regname};
     `RAL_FIELD_CP(IP_SB_CPL_STS_0_0, IP_SB_CPL_STS_0_0.desired)
     `RAL_FIELD_CP_1(IP_SB_CPL_STS_0_0, IP_SB_CPL_STS_0_0.desired, 0)
     `RAL_FIELD_CP(IP_SB_CPL_STS_0_1, IP_SB_CPL_STS_0_1.desired)
     `RAL_FIELD_CP_1(IP_SB_CPL_STS_0_1, IP_SB_CPL_STS_0_1.desired, 0)
     `RAL_FIELD_CP(IP_SB_CPL_STS_0_2, IP_SB_CPL_STS_0_2.desired)
     `RAL_FIELD_CP_1(IP_SB_CPL_STS_0_2, IP_SB_CPL_STS_0_2.desired, 0)
     `RAL_FIELD_CP(IP_SB_CPL_STS_0_3, IP_SB_CPL_STS_0_3.desired)
     `RAL_FIELD_CP_1(IP_SB_CPL_STS_0_3, IP_SB_CPL_STS_0_3.desired, 0)
     `RAL_FIELD_CP(IP_SB_CPL_STS_0_4, IP_SB_CPL_STS_0_4.desired)
     `RAL_FIELD_CP_1(IP_SB_CPL_STS_0_4, IP_SB_CPL_STS_0_4.desired, 0)
     `RAL_FIELD_CP(IP_SB_CPL_STS_0_5, IP_SB_CPL_STS_0_5.desired)
     `RAL_FIELD_CP_1(IP_SB_CPL_STS_0_5, IP_SB_CPL_STS_0_5.desired, 0)
     `RAL_FIELD_CP(IP_SB_CPL_STS_0_6, IP_SB_CPL_STS_0_6.desired)
     `RAL_FIELD_CP_1(IP_SB_CPL_STS_0_6, IP_SB_CPL_STS_0_6.desired, 0)
     `RAL_FIELD_CP(IP_SB_CPL_STS_0_7, IP_SB_CPL_STS_0_7.desired)
     `RAL_FIELD_CP_1(IP_SB_CPL_STS_0_7, IP_SB_CPL_STS_0_7.desired, 0)
     `RAL_FIELD_CP(IP_SB_CPL_STS_0_8, IP_SB_CPL_STS_0_8.desired)
     `RAL_FIELD_CP_1(IP_SB_CPL_STS_0_8, IP_SB_CPL_STS_0_8.desired, 0)
     `RAL_FIELD_CP(IP_SB_CPL_STS_0_9, IP_SB_CPL_STS_0_9.desired)
     `RAL_FIELD_CP_1(IP_SB_CPL_STS_0_9, IP_SB_CPL_STS_0_9.desired, 0)
     `RAL_FIELD_CP(IP_SB_CPL_STS_0_10, IP_SB_CPL_STS_0_10.desired)
     `RAL_FIELD_CP_1(IP_SB_CPL_STS_0_10, IP_SB_CPL_STS_0_10.desired, 0)
     `RAL_FIELD_CP(IP_SB_CPL_STS_0_11, IP_SB_CPL_STS_0_11.desired)
     `RAL_FIELD_CP_1(IP_SB_CPL_STS_0_11, IP_SB_CPL_STS_0_11.desired, 0)
     `RAL_FIELD_CP(IP_SB_CPL_STS_0_12, IP_SB_CPL_STS_0_12.desired)
     `RAL_FIELD_CP_1(IP_SB_CPL_STS_0_12, IP_SB_CPL_STS_0_12.desired, 0)
     `RAL_FIELD_CP(IP_SB_CPL_STS_0_13, IP_SB_CPL_STS_0_13.desired)
     `RAL_FIELD_CP_1(IP_SB_CPL_STS_0_13, IP_SB_CPL_STS_0_13.desired, 0)
     `RAL_FIELD_CP(IP_SB_CPL_STS_0_14, IP_SB_CPL_STS_0_14.desired)
     `RAL_FIELD_CP_1(IP_SB_CPL_STS_0_14, IP_SB_CPL_STS_0_14.desired, 0)
     `RAL_FIELD_CP(IP_SB_CPL_STS_0_15, IP_SB_CPL_STS_0_15.desired)
     `RAL_FIELD_CP_1(IP_SB_CPL_STS_0_15, IP_SB_CPL_STS_0_15.desired, 0)
     `RAL_FIELD_CP(IP_SB_CPL_STS_0_16, IP_SB_CPL_STS_0_16.desired)
     `RAL_FIELD_CP_1(IP_SB_CPL_STS_0_16, IP_SB_CPL_STS_0_16.desired, 0)
     `RAL_FIELD_CP(IP_SB_CPL_STS_0_17, IP_SB_CPL_STS_0_17.desired)
     `RAL_FIELD_CP_1(IP_SB_CPL_STS_0_17, IP_SB_CPL_STS_0_17.desired, 0)
     `RAL_FIELD_CP(IP_SB_CPL_STS_0_18, IP_SB_CPL_STS_0_18.desired)
     `RAL_FIELD_CP_1(IP_SB_CPL_STS_0_18, IP_SB_CPL_STS_0_18.desired, 0)
     `RAL_FIELD_CP(IP_SB_CPL_STS_0_19, IP_SB_CPL_STS_0_19.desired)
     `RAL_FIELD_CP_1(IP_SB_CPL_STS_0_19, IP_SB_CPL_STS_0_19.desired, 0)
     `RAL_FIELD_CP(IP_SB_CPL_STS_0_20, IP_SB_CPL_STS_0_20.desired)
     `RAL_FIELD_CP_1(IP_SB_CPL_STS_0_20, IP_SB_CPL_STS_0_20.desired, 0)
     `RAL_FIELD_CP(IP_SB_CPL_STS_0_21, IP_SB_CPL_STS_0_21.desired)
     `RAL_FIELD_CP_1(IP_SB_CPL_STS_0_21, IP_SB_CPL_STS_0_21.desired, 0)
     `RAL_FIELD_CP(IP_SB_CPL_STS_0_22, IP_SB_CPL_STS_0_22.desired)
     `RAL_FIELD_CP_1(IP_SB_CPL_STS_0_22, IP_SB_CPL_STS_0_22.desired, 0)
     `RAL_FIELD_CP(IP_SB_CPL_STS_0_23, IP_SB_CPL_STS_0_23.desired)
     `RAL_FIELD_CP_1(IP_SB_CPL_STS_0_23, IP_SB_CPL_STS_0_23.desired, 0)
     `RAL_FIELD_CP(IP_SB_CPL_STS_0_24, IP_SB_CPL_STS_0_24.desired)
     `RAL_FIELD_CP_1(IP_SB_CPL_STS_0_24, IP_SB_CPL_STS_0_24.desired, 0)
     `RAL_FIELD_CP(IP_SB_CPL_STS_0_25, IP_SB_CPL_STS_0_25.desired)
     `RAL_FIELD_CP_1(IP_SB_CPL_STS_0_25, IP_SB_CPL_STS_0_25.desired, 0)
     `RAL_FIELD_CP(IP_SB_CPL_STS_0_26, IP_SB_CPL_STS_0_26.desired)
     `RAL_FIELD_CP_1(IP_SB_CPL_STS_0_26, IP_SB_CPL_STS_0_26.desired, 0)
     `RAL_FIELD_CP(IP_SB_CPL_STS_0_27, IP_SB_CPL_STS_0_27.desired)
     `RAL_FIELD_CP_1(IP_SB_CPL_STS_0_27, IP_SB_CPL_STS_0_27.desired, 0)
     `RAL_FIELD_CP(IP_SB_CPL_STS_0_28, IP_SB_CPL_STS_0_28.desired)
     `RAL_FIELD_CP_1(IP_SB_CPL_STS_0_28, IP_SB_CPL_STS_0_28.desired, 0)
     `RAL_FIELD_CP(IP_SB_CPL_STS_0_29, IP_SB_CPL_STS_0_29.desired)
     `RAL_FIELD_CP_1(IP_SB_CPL_STS_0_29, IP_SB_CPL_STS_0_29.desired, 0)
     `RAL_FIELD_CP(IP_SB_CPL_STS_0_30, IP_SB_CPL_STS_0_30.desired)
     `RAL_FIELD_CP_1(IP_SB_CPL_STS_0_30, IP_SB_CPL_STS_0_30.desired, 0)
     `RAL_FIELD_CP(IP_SB_CPL_STS_0_31, IP_SB_CPL_STS_0_31.desired)
     `RAL_FIELD_CP_1(IP_SB_CPL_STS_0_31, IP_SB_CPL_STS_0_31.desired, 0)
  endgroup
  covergroup actual_cg @(cov_ev);
     option.per_instance = 1;
     option.name = {sla_ral_file::regfilename,".",regname};
     `RAL_FIELD_CP(IP_SB_CPL_STS_0_0, IP_SB_CPL_STS_0_0.actual)
     `RAL_FIELD_CP_1(IP_SB_CPL_STS_0_0, IP_SB_CPL_STS_0_0.actual, 0)
     `RAL_FIELD_CP(IP_SB_CPL_STS_0_1, IP_SB_CPL_STS_0_1.actual)
     `RAL_FIELD_CP_1(IP_SB_CPL_STS_0_1, IP_SB_CPL_STS_0_1.actual, 0)
     `RAL_FIELD_CP(IP_SB_CPL_STS_0_2, IP_SB_CPL_STS_0_2.actual)
     `RAL_FIELD_CP_1(IP_SB_CPL_STS_0_2, IP_SB_CPL_STS_0_2.actual, 0)
     `RAL_FIELD_CP(IP_SB_CPL_STS_0_3, IP_SB_CPL_STS_0_3.actual)
     `RAL_FIELD_CP_1(IP_SB_CPL_STS_0_3, IP_SB_CPL_STS_0_3.actual, 0)
     `RAL_FIELD_CP(IP_SB_CPL_STS_0_4, IP_SB_CPL_STS_0_4.actual)
     `RAL_FIELD_CP_1(IP_SB_CPL_STS_0_4, IP_SB_CPL_STS_0_4.actual, 0)
     `RAL_FIELD_CP(IP_SB_CPL_STS_0_5, IP_SB_CPL_STS_0_5.actual)
     `RAL_FIELD_CP_1(IP_SB_CPL_STS_0_5, IP_SB_CPL_STS_0_5.actual, 0)
     `RAL_FIELD_CP(IP_SB_CPL_STS_0_6, IP_SB_CPL_STS_0_6.actual)
     `RAL_FIELD_CP_1(IP_SB_CPL_STS_0_6, IP_SB_CPL_STS_0_6.actual, 0)
     `RAL_FIELD_CP(IP_SB_CPL_STS_0_7, IP_SB_CPL_STS_0_7.actual)
     `RAL_FIELD_CP_1(IP_SB_CPL_STS_0_7, IP_SB_CPL_STS_0_7.actual, 0)
     `RAL_FIELD_CP(IP_SB_CPL_STS_0_8, IP_SB_CPL_STS_0_8.actual)
     `RAL_FIELD_CP_1(IP_SB_CPL_STS_0_8, IP_SB_CPL_STS_0_8.actual, 0)
     `RAL_FIELD_CP(IP_SB_CPL_STS_0_9, IP_SB_CPL_STS_0_9.actual)
     `RAL_FIELD_CP_1(IP_SB_CPL_STS_0_9, IP_SB_CPL_STS_0_9.actual, 0)
     `RAL_FIELD_CP(IP_SB_CPL_STS_0_10, IP_SB_CPL_STS_0_10.actual)
     `RAL_FIELD_CP_1(IP_SB_CPL_STS_0_10, IP_SB_CPL_STS_0_10.actual, 0)
     `RAL_FIELD_CP(IP_SB_CPL_STS_0_11, IP_SB_CPL_STS_0_11.actual)
     `RAL_FIELD_CP_1(IP_SB_CPL_STS_0_11, IP_SB_CPL_STS_0_11.actual, 0)
     `RAL_FIELD_CP(IP_SB_CPL_STS_0_12, IP_SB_CPL_STS_0_12.actual)
     `RAL_FIELD_CP_1(IP_SB_CPL_STS_0_12, IP_SB_CPL_STS_0_12.actual, 0)
     `RAL_FIELD_CP(IP_SB_CPL_STS_0_13, IP_SB_CPL_STS_0_13.actual)
     `RAL_FIELD_CP_1(IP_SB_CPL_STS_0_13, IP_SB_CPL_STS_0_13.actual, 0)
     `RAL_FIELD_CP(IP_SB_CPL_STS_0_14, IP_SB_CPL_STS_0_14.actual)
     `RAL_FIELD_CP_1(IP_SB_CPL_STS_0_14, IP_SB_CPL_STS_0_14.actual, 0)
     `RAL_FIELD_CP(IP_SB_CPL_STS_0_15, IP_SB_CPL_STS_0_15.actual)
     `RAL_FIELD_CP_1(IP_SB_CPL_STS_0_15, IP_SB_CPL_STS_0_15.actual, 0)
     `RAL_FIELD_CP(IP_SB_CPL_STS_0_16, IP_SB_CPL_STS_0_16.actual)
     `RAL_FIELD_CP_1(IP_SB_CPL_STS_0_16, IP_SB_CPL_STS_0_16.actual, 0)
     `RAL_FIELD_CP(IP_SB_CPL_STS_0_17, IP_SB_CPL_STS_0_17.actual)
     `RAL_FIELD_CP_1(IP_SB_CPL_STS_0_17, IP_SB_CPL_STS_0_17.actual, 0)
     `RAL_FIELD_CP(IP_SB_CPL_STS_0_18, IP_SB_CPL_STS_0_18.actual)
     `RAL_FIELD_CP_1(IP_SB_CPL_STS_0_18, IP_SB_CPL_STS_0_18.actual, 0)
     `RAL_FIELD_CP(IP_SB_CPL_STS_0_19, IP_SB_CPL_STS_0_19.actual)
     `RAL_FIELD_CP_1(IP_SB_CPL_STS_0_19, IP_SB_CPL_STS_0_19.actual, 0)
     `RAL_FIELD_CP(IP_SB_CPL_STS_0_20, IP_SB_CPL_STS_0_20.actual)
     `RAL_FIELD_CP_1(IP_SB_CPL_STS_0_20, IP_SB_CPL_STS_0_20.actual, 0)
     `RAL_FIELD_CP(IP_SB_CPL_STS_0_21, IP_SB_CPL_STS_0_21.actual)
     `RAL_FIELD_CP_1(IP_SB_CPL_STS_0_21, IP_SB_CPL_STS_0_21.actual, 0)
     `RAL_FIELD_CP(IP_SB_CPL_STS_0_22, IP_SB_CPL_STS_0_22.actual)
     `RAL_FIELD_CP_1(IP_SB_CPL_STS_0_22, IP_SB_CPL_STS_0_22.actual, 0)
     `RAL_FIELD_CP(IP_SB_CPL_STS_0_23, IP_SB_CPL_STS_0_23.actual)
     `RAL_FIELD_CP_1(IP_SB_CPL_STS_0_23, IP_SB_CPL_STS_0_23.actual, 0)
     `RAL_FIELD_CP(IP_SB_CPL_STS_0_24, IP_SB_CPL_STS_0_24.actual)
     `RAL_FIELD_CP_1(IP_SB_CPL_STS_0_24, IP_SB_CPL_STS_0_24.actual, 0)
     `RAL_FIELD_CP(IP_SB_CPL_STS_0_25, IP_SB_CPL_STS_0_25.actual)
     `RAL_FIELD_CP_1(IP_SB_CPL_STS_0_25, IP_SB_CPL_STS_0_25.actual, 0)
     `RAL_FIELD_CP(IP_SB_CPL_STS_0_26, IP_SB_CPL_STS_0_26.actual)
     `RAL_FIELD_CP_1(IP_SB_CPL_STS_0_26, IP_SB_CPL_STS_0_26.actual, 0)
     `RAL_FIELD_CP(IP_SB_CPL_STS_0_27, IP_SB_CPL_STS_0_27.actual)
     `RAL_FIELD_CP_1(IP_SB_CPL_STS_0_27, IP_SB_CPL_STS_0_27.actual, 0)
     `RAL_FIELD_CP(IP_SB_CPL_STS_0_28, IP_SB_CPL_STS_0_28.actual)
     `RAL_FIELD_CP_1(IP_SB_CPL_STS_0_28, IP_SB_CPL_STS_0_28.actual, 0)
     `RAL_FIELD_CP(IP_SB_CPL_STS_0_29, IP_SB_CPL_STS_0_29.actual)
     `RAL_FIELD_CP_1(IP_SB_CPL_STS_0_29, IP_SB_CPL_STS_0_29.actual, 0)
     `RAL_FIELD_CP(IP_SB_CPL_STS_0_30, IP_SB_CPL_STS_0_30.actual)
     `RAL_FIELD_CP_1(IP_SB_CPL_STS_0_30, IP_SB_CPL_STS_0_30.actual, 0)
     `RAL_FIELD_CP(IP_SB_CPL_STS_0_31, IP_SB_CPL_STS_0_31.actual)
     `RAL_FIELD_CP_1(IP_SB_CPL_STS_0_31, IP_SB_CPL_STS_0_31.actual, 0)
  endgroup
  `endif

  // --------------------------
  // add constraint based on database legal values
  // --------------------------
  function void build();
    IP_SB_CPL_STS_0_0 = new("IP_SB_CPL_STS_0_0", "RW/1C/V", 1, 0, {"IP_SB_CPL_STS_0.IP_SB_CPL_STS_0_0"});
    IP_SB_CPL_STS_0_0.set_powerwell("primary");
    IP_SB_CPL_STS_0_0.set_rand_mode(0);
   IP_SB_CPL_STS_0_0.set_reset_signame("pmu_rst_b");
    void'(add_field( IP_SB_CPL_STS_0_0 ));

    IP_SB_CPL_STS_0_1 = new("IP_SB_CPL_STS_0_1", "RW/1C/V", 1, 1, {"IP_SB_CPL_STS_0.IP_SB_CPL_STS_0_1"});
    IP_SB_CPL_STS_0_1.set_powerwell("primary");
    IP_SB_CPL_STS_0_1.set_rand_mode(0);
   IP_SB_CPL_STS_0_1.set_reset_signame("pmu_rst_b");
    void'(add_field( IP_SB_CPL_STS_0_1 ));

    IP_SB_CPL_STS_0_2 = new("IP_SB_CPL_STS_0_2", "RW/1C/V", 1, 2, {"IP_SB_CPL_STS_0.IP_SB_CPL_STS_0_2"});
    IP_SB_CPL_STS_0_2.set_powerwell("primary");
    IP_SB_CPL_STS_0_2.set_rand_mode(0);
   IP_SB_CPL_STS_0_2.set_reset_signame("pmu_rst_b");
    void'(add_field( IP_SB_CPL_STS_0_2 ));

    IP_SB_CPL_STS_0_3 = new("IP_SB_CPL_STS_0_3", "RW/1C/V", 1, 3, {"IP_SB_CPL_STS_0.IP_SB_CPL_STS_0_3"});
    IP_SB_CPL_STS_0_3.set_powerwell("primary");
    IP_SB_CPL_STS_0_3.set_rand_mode(0);
   IP_SB_CPL_STS_0_3.set_reset_signame("pmu_rst_b");
    void'(add_field( IP_SB_CPL_STS_0_3 ));

    IP_SB_CPL_STS_0_4 = new("IP_SB_CPL_STS_0_4", "RW/1C/V", 1, 4, {"IP_SB_CPL_STS_0.IP_SB_CPL_STS_0_4"});
    IP_SB_CPL_STS_0_4.set_powerwell("primary");
    IP_SB_CPL_STS_0_4.set_rand_mode(0);
   IP_SB_CPL_STS_0_4.set_reset_signame("pmu_rst_b");
    void'(add_field( IP_SB_CPL_STS_0_4 ));

    IP_SB_CPL_STS_0_5 = new("IP_SB_CPL_STS_0_5", "RW/1C/V", 1, 5, {"IP_SB_CPL_STS_0.IP_SB_CPL_STS_0_5"});
    IP_SB_CPL_STS_0_5.set_powerwell("primary");
    IP_SB_CPL_STS_0_5.set_rand_mode(0);
   IP_SB_CPL_STS_0_5.set_reset_signame("pmu_rst_b");
    void'(add_field( IP_SB_CPL_STS_0_5 ));

    IP_SB_CPL_STS_0_6 = new("IP_SB_CPL_STS_0_6", "RW/1C/V", 1, 6, {"IP_SB_CPL_STS_0.IP_SB_CPL_STS_0_6"});
    IP_SB_CPL_STS_0_6.set_powerwell("primary");
    IP_SB_CPL_STS_0_6.set_rand_mode(0);
   IP_SB_CPL_STS_0_6.set_reset_signame("pmu_rst_b");
    void'(add_field( IP_SB_CPL_STS_0_6 ));

    IP_SB_CPL_STS_0_7 = new("IP_SB_CPL_STS_0_7", "RW/1C/V", 1, 7, {"IP_SB_CPL_STS_0.IP_SB_CPL_STS_0_7"});
    IP_SB_CPL_STS_0_7.set_powerwell("primary");
    IP_SB_CPL_STS_0_7.set_rand_mode(0);
   IP_SB_CPL_STS_0_7.set_reset_signame("pmu_rst_b");
    void'(add_field( IP_SB_CPL_STS_0_7 ));

    IP_SB_CPL_STS_0_8 = new("IP_SB_CPL_STS_0_8", "RW/1C/V", 1, 8, {"IP_SB_CPL_STS_0.IP_SB_CPL_STS_0_8"});
    IP_SB_CPL_STS_0_8.set_powerwell("primary");
    IP_SB_CPL_STS_0_8.set_rand_mode(0);
   IP_SB_CPL_STS_0_8.set_reset_signame("pmu_rst_b");
    void'(add_field( IP_SB_CPL_STS_0_8 ));

    IP_SB_CPL_STS_0_9 = new("IP_SB_CPL_STS_0_9", "RW/1C/V", 1, 9, {"IP_SB_CPL_STS_0.IP_SB_CPL_STS_0_9"});
    IP_SB_CPL_STS_0_9.set_powerwell("primary");
    IP_SB_CPL_STS_0_9.set_rand_mode(0);
   IP_SB_CPL_STS_0_9.set_reset_signame("pmu_rst_b");
    void'(add_field( IP_SB_CPL_STS_0_9 ));

    IP_SB_CPL_STS_0_10 = new("IP_SB_CPL_STS_0_10", "RW/1C/V", 1, 10, {"IP_SB_CPL_STS_0.IP_SB_CPL_STS_0_10"});
    IP_SB_CPL_STS_0_10.set_powerwell("primary");
    IP_SB_CPL_STS_0_10.set_rand_mode(0);
   IP_SB_CPL_STS_0_10.set_reset_signame("pmu_rst_b");
    void'(add_field( IP_SB_CPL_STS_0_10 ));

    IP_SB_CPL_STS_0_11 = new("IP_SB_CPL_STS_0_11", "RW/1C/V", 1, 11, {"IP_SB_CPL_STS_0.IP_SB_CPL_STS_0_11"});
    IP_SB_CPL_STS_0_11.set_powerwell("primary");
    IP_SB_CPL_STS_0_11.set_rand_mode(0);
   IP_SB_CPL_STS_0_11.set_reset_signame("pmu_rst_b");
    void'(add_field( IP_SB_CPL_STS_0_11 ));

    IP_SB_CPL_STS_0_12 = new("IP_SB_CPL_STS_0_12", "RW/1C/V", 1, 12, {"IP_SB_CPL_STS_0.IP_SB_CPL_STS_0_12"});
    IP_SB_CPL_STS_0_12.set_powerwell("primary");
    IP_SB_CPL_STS_0_12.set_rand_mode(0);
   IP_SB_CPL_STS_0_12.set_reset_signame("pmu_rst_b");
    void'(add_field( IP_SB_CPL_STS_0_12 ));

    IP_SB_CPL_STS_0_13 = new("IP_SB_CPL_STS_0_13", "RW/1C/V", 1, 13, {"IP_SB_CPL_STS_0.IP_SB_CPL_STS_0_13"});
    IP_SB_CPL_STS_0_13.set_powerwell("primary");
    IP_SB_CPL_STS_0_13.set_rand_mode(0);
   IP_SB_CPL_STS_0_13.set_reset_signame("pmu_rst_b");
    void'(add_field( IP_SB_CPL_STS_0_13 ));

    IP_SB_CPL_STS_0_14 = new("IP_SB_CPL_STS_0_14", "RW/1C/V", 1, 14, {"IP_SB_CPL_STS_0.IP_SB_CPL_STS_0_14"});
    IP_SB_CPL_STS_0_14.set_powerwell("primary");
    IP_SB_CPL_STS_0_14.set_rand_mode(0);
   IP_SB_CPL_STS_0_14.set_reset_signame("pmu_rst_b");
    void'(add_field( IP_SB_CPL_STS_0_14 ));

    IP_SB_CPL_STS_0_15 = new("IP_SB_CPL_STS_0_15", "RW/1C/V", 1, 15, {"IP_SB_CPL_STS_0.IP_SB_CPL_STS_0_15"});
    IP_SB_CPL_STS_0_15.set_powerwell("primary");
    IP_SB_CPL_STS_0_15.set_rand_mode(0);
   IP_SB_CPL_STS_0_15.set_reset_signame("pmu_rst_b");
    void'(add_field( IP_SB_CPL_STS_0_15 ));

    IP_SB_CPL_STS_0_16 = new("IP_SB_CPL_STS_0_16", "RW/1C/V", 1, 16, {"IP_SB_CPL_STS_0.IP_SB_CPL_STS_0_16"});
    IP_SB_CPL_STS_0_16.set_powerwell("primary");
    IP_SB_CPL_STS_0_16.set_rand_mode(0);
   IP_SB_CPL_STS_0_16.set_reset_signame("pmu_rst_b");
    void'(add_field( IP_SB_CPL_STS_0_16 ));

    IP_SB_CPL_STS_0_17 = new("IP_SB_CPL_STS_0_17", "RW/1C/V", 1, 17, {"IP_SB_CPL_STS_0.IP_SB_CPL_STS_0_17"});
    IP_SB_CPL_STS_0_17.set_powerwell("primary");
    IP_SB_CPL_STS_0_17.set_rand_mode(0);
   IP_SB_CPL_STS_0_17.set_reset_signame("pmu_rst_b");
    void'(add_field( IP_SB_CPL_STS_0_17 ));

    IP_SB_CPL_STS_0_18 = new("IP_SB_CPL_STS_0_18", "RW/1C/V", 1, 18, {"IP_SB_CPL_STS_0.IP_SB_CPL_STS_0_18"});
    IP_SB_CPL_STS_0_18.set_powerwell("primary");
    IP_SB_CPL_STS_0_18.set_rand_mode(0);
   IP_SB_CPL_STS_0_18.set_reset_signame("pmu_rst_b");
    void'(add_field( IP_SB_CPL_STS_0_18 ));

    IP_SB_CPL_STS_0_19 = new("IP_SB_CPL_STS_0_19", "RW/1C/V", 1, 19, {"IP_SB_CPL_STS_0.IP_SB_CPL_STS_0_19"});
    IP_SB_CPL_STS_0_19.set_powerwell("primary");
    IP_SB_CPL_STS_0_19.set_rand_mode(0);
   IP_SB_CPL_STS_0_19.set_reset_signame("pmu_rst_b");
    void'(add_field( IP_SB_CPL_STS_0_19 ));

    IP_SB_CPL_STS_0_20 = new("IP_SB_CPL_STS_0_20", "RW/1C/V", 1, 20, {"IP_SB_CPL_STS_0.IP_SB_CPL_STS_0_20"});
    IP_SB_CPL_STS_0_20.set_powerwell("primary");
    IP_SB_CPL_STS_0_20.set_rand_mode(0);
   IP_SB_CPL_STS_0_20.set_reset_signame("pmu_rst_b");
    void'(add_field( IP_SB_CPL_STS_0_20 ));

    IP_SB_CPL_STS_0_21 = new("IP_SB_CPL_STS_0_21", "RW/1C/V", 1, 21, {"IP_SB_CPL_STS_0.IP_SB_CPL_STS_0_21"});
    IP_SB_CPL_STS_0_21.set_powerwell("primary");
    IP_SB_CPL_STS_0_21.set_rand_mode(0);
   IP_SB_CPL_STS_0_21.set_reset_signame("pmu_rst_b");
    void'(add_field( IP_SB_CPL_STS_0_21 ));

    IP_SB_CPL_STS_0_22 = new("IP_SB_CPL_STS_0_22", "RW/1C/V", 1, 22, {"IP_SB_CPL_STS_0.IP_SB_CPL_STS_0_22"});
    IP_SB_CPL_STS_0_22.set_powerwell("primary");
    IP_SB_CPL_STS_0_22.set_rand_mode(0);
   IP_SB_CPL_STS_0_22.set_reset_signame("pmu_rst_b");
    void'(add_field( IP_SB_CPL_STS_0_22 ));

    IP_SB_CPL_STS_0_23 = new("IP_SB_CPL_STS_0_23", "RW/1C/V", 1, 23, {"IP_SB_CPL_STS_0.IP_SB_CPL_STS_0_23"});
    IP_SB_CPL_STS_0_23.set_powerwell("primary");
    IP_SB_CPL_STS_0_23.set_rand_mode(0);
   IP_SB_CPL_STS_0_23.set_reset_signame("pmu_rst_b");
    void'(add_field( IP_SB_CPL_STS_0_23 ));

    IP_SB_CPL_STS_0_24 = new("IP_SB_CPL_STS_0_24", "RW/1C/V", 1, 24, {"IP_SB_CPL_STS_0.IP_SB_CPL_STS_0_24"});
    IP_SB_CPL_STS_0_24.set_powerwell("primary");
    IP_SB_CPL_STS_0_24.set_rand_mode(0);
   IP_SB_CPL_STS_0_24.set_reset_signame("pmu_rst_b");
    void'(add_field( IP_SB_CPL_STS_0_24 ));

    IP_SB_CPL_STS_0_25 = new("IP_SB_CPL_STS_0_25", "RW/1C/V", 1, 25, {"IP_SB_CPL_STS_0.IP_SB_CPL_STS_0_25"});
    IP_SB_CPL_STS_0_25.set_powerwell("primary");
    IP_SB_CPL_STS_0_25.set_rand_mode(0);
   IP_SB_CPL_STS_0_25.set_reset_signame("pmu_rst_b");
    void'(add_field( IP_SB_CPL_STS_0_25 ));

    IP_SB_CPL_STS_0_26 = new("IP_SB_CPL_STS_0_26", "RW/1C/V", 1, 26, {"IP_SB_CPL_STS_0.IP_SB_CPL_STS_0_26"});
    IP_SB_CPL_STS_0_26.set_powerwell("primary");
    IP_SB_CPL_STS_0_26.set_rand_mode(0);
   IP_SB_CPL_STS_0_26.set_reset_signame("pmu_rst_b");
    void'(add_field( IP_SB_CPL_STS_0_26 ));

    IP_SB_CPL_STS_0_27 = new("IP_SB_CPL_STS_0_27", "RW/1C/V", 1, 27, {"IP_SB_CPL_STS_0.IP_SB_CPL_STS_0_27"});
    IP_SB_CPL_STS_0_27.set_powerwell("primary");
    IP_SB_CPL_STS_0_27.set_rand_mode(0);
   IP_SB_CPL_STS_0_27.set_reset_signame("pmu_rst_b");
    void'(add_field( IP_SB_CPL_STS_0_27 ));

    IP_SB_CPL_STS_0_28 = new("IP_SB_CPL_STS_0_28", "RW/1C/V", 1, 28, {"IP_SB_CPL_STS_0.IP_SB_CPL_STS_0_28"});
    IP_SB_CPL_STS_0_28.set_powerwell("primary");
    IP_SB_CPL_STS_0_28.set_rand_mode(0);
   IP_SB_CPL_STS_0_28.set_reset_signame("pmu_rst_b");
    void'(add_field( IP_SB_CPL_STS_0_28 ));

    IP_SB_CPL_STS_0_29 = new("IP_SB_CPL_STS_0_29", "RW/1C/V", 1, 29, {"IP_SB_CPL_STS_0.IP_SB_CPL_STS_0_29"});
    IP_SB_CPL_STS_0_29.set_powerwell("primary");
    IP_SB_CPL_STS_0_29.set_rand_mode(0);
   IP_SB_CPL_STS_0_29.set_reset_signame("pmu_rst_b");
    void'(add_field( IP_SB_CPL_STS_0_29 ));

    IP_SB_CPL_STS_0_30 = new("IP_SB_CPL_STS_0_30", "RW/1C/V", 1, 30, {"IP_SB_CPL_STS_0.IP_SB_CPL_STS_0_30"});
    IP_SB_CPL_STS_0_30.set_powerwell("primary");
    IP_SB_CPL_STS_0_30.set_rand_mode(0);
   IP_SB_CPL_STS_0_30.set_reset_signame("pmu_rst_b");
    void'(add_field( IP_SB_CPL_STS_0_30 ));

    IP_SB_CPL_STS_0_31 = new("IP_SB_CPL_STS_0_31", "RW/1C/V", 1, 31, {"IP_SB_CPL_STS_0.IP_SB_CPL_STS_0_31"});
    IP_SB_CPL_STS_0_31.set_powerwell("primary");
    IP_SB_CPL_STS_0_31.set_rand_mode(0);
   IP_SB_CPL_STS_0_31.set_reset_signame("pmu_rst_b");
    void'(add_field( IP_SB_CPL_STS_0_31 ));

  endfunction


`DEFINE_NONE_SCOPE_REG_API(pmu_mmr_IP_SB_CPL_STS_0_reg) 
endclass : pmu_mmr_IP_SB_CPL_STS_0_reg

// ================================================

class pmu_mmr_IP_SB_CPL_STS_1_reg extends sla_ral_reg;

  // --------------------------
  sla_ral_field IP_SB_CPL_STS_1_0;
  sla_ral_field IP_SB_CPL_STS_1_1;
  sla_ral_field IP_SB_CPL_STS_1_2;
  sla_ral_field IP_SB_CPL_STS_1_3;
  sla_ral_field IP_SB_CPL_STS_1_4;
  sla_ral_field IP_SB_CPL_STS_1_5;
  sla_ral_field IP_SB_CPL_STS_1_6;
  sla_ral_field IP_SB_CPL_STS_1_7;
  sla_ral_field IP_SB_CPL_STS_1_8;
  sla_ral_field IP_SB_CPL_STS_1_9;
  sla_ral_field IP_SB_CPL_STS_1_10;
  sla_ral_field IP_SB_CPL_STS_1_11;
  sla_ral_field IP_SB_CPL_STS_1_12;
  sla_ral_field IP_SB_CPL_STS_1_13;
  sla_ral_field IP_SB_CPL_STS_1_14;
  sla_ral_field IP_SB_CPL_STS_1_15;
  sla_ral_field IP_SB_CPL_STS_1_16;
  sla_ral_field IP_SB_CPL_STS_1_17;
  sla_ral_field IP_SB_CPL_STS_1_18;
  sla_ral_field IP_SB_CPL_STS_1_19;
  sla_ral_field IP_SB_CPL_STS_1_20;
  sla_ral_field IP_SB_CPL_STS_1_21;
  sla_ral_field IP_SB_CPL_STS_1_22;
  sla_ral_field IP_SB_CPL_STS_1_23;
  sla_ral_field IP_SB_CPL_STS_1_24;
  sla_ral_field IP_SB_CPL_STS_1_25;
  sla_ral_field IP_SB_CPL_STS_1_26;
  sla_ral_field IP_SB_CPL_STS_1_27;
  sla_ral_field IP_SB_CPL_STS_1_28;
  sla_ral_field IP_SB_CPL_STS_1_29;
  sla_ral_field IP_SB_CPL_STS_1_30;
  sla_ral_field IP_SB_CPL_STS_1_31;

  // --------------------------
  `ovm_object_utils(pmu_mmr_IP_SB_CPL_STS_1_reg)

  // --------------------------
  function new(
                string         name = "",
                `ifdef SLA_RAL_COVERAGE
                sla_ral_coverage_t cov_t = COVERAGE_OFF,
                `endif
                int            bus_num=0,
                int            dev_num=0,
                int            func_num=0,
                int            offset=0,
                int            size=0,
                sla_ral_data_t reset_val=0,
                boolean_t      mon_enabled = SLA_FALSE
             );
      super.new();
      set_space_addr("CFG",undef);
      set_space_addr("MEM",undef);
      set_space_addr("IO",undef);
      set_space_addr("MSG",undef);
      set_space_addr("CREG",undef); 
      set_space_addr("LT_MEM",undef);    
 build();

      `ifdef SLA_RAL_COVERAGE
      `CONFIG_RAL_COVERAGE(regname, COVERAGE_ON, FD_RW_SAMPLE)
      if(_coverage_type != COVERAGE_OFF) begin
        if(_sample_type inside {FD_RO_SAMPLE, RO_SAMPLE})
        `CREATE_OP_SAMPLE_COV(RAL_WRITE)
        else if(_sample_type inside {FD_WO_SAMPLE, WO_SAMPLE})
        `CREATE_OP_SAMPLE_COV(RAL_READ)
        else
        `CREATE_OP_SAMPLE_COV(RAL_NONE)
        `CREATE_DESIRED_COV
        `CREATE_ACTUAL_COV
      end
      `endif

  endfunction

  `ifdef SLA_RAL_COVERAGE
  `CREATE_RAL_OP_COVERGROUP
  covergroup desired_cg @(cov_ev);
     option.per_instance = 1;
     option.name = {sla_ral_file::regfilename,".",regname};
     `RAL_FIELD_CP(IP_SB_CPL_STS_1_0, IP_SB_CPL_STS_1_0.desired)
     `RAL_FIELD_CP_1(IP_SB_CPL_STS_1_0, IP_SB_CPL_STS_1_0.desired, 0)
     `RAL_FIELD_CP(IP_SB_CPL_STS_1_1, IP_SB_CPL_STS_1_1.desired)
     `RAL_FIELD_CP_1(IP_SB_CPL_STS_1_1, IP_SB_CPL_STS_1_1.desired, 0)
     `RAL_FIELD_CP(IP_SB_CPL_STS_1_2, IP_SB_CPL_STS_1_2.desired)
     `RAL_FIELD_CP_1(IP_SB_CPL_STS_1_2, IP_SB_CPL_STS_1_2.desired, 0)
     `RAL_FIELD_CP(IP_SB_CPL_STS_1_3, IP_SB_CPL_STS_1_3.desired)
     `RAL_FIELD_CP_1(IP_SB_CPL_STS_1_3, IP_SB_CPL_STS_1_3.desired, 0)
     `RAL_FIELD_CP(IP_SB_CPL_STS_1_4, IP_SB_CPL_STS_1_4.desired)
     `RAL_FIELD_CP_1(IP_SB_CPL_STS_1_4, IP_SB_CPL_STS_1_4.desired, 0)
     `RAL_FIELD_CP(IP_SB_CPL_STS_1_5, IP_SB_CPL_STS_1_5.desired)
     `RAL_FIELD_CP_1(IP_SB_CPL_STS_1_5, IP_SB_CPL_STS_1_5.desired, 0)
     `RAL_FIELD_CP(IP_SB_CPL_STS_1_6, IP_SB_CPL_STS_1_6.desired)
     `RAL_FIELD_CP_1(IP_SB_CPL_STS_1_6, IP_SB_CPL_STS_1_6.desired, 0)
     `RAL_FIELD_CP(IP_SB_CPL_STS_1_7, IP_SB_CPL_STS_1_7.desired)
     `RAL_FIELD_CP_1(IP_SB_CPL_STS_1_7, IP_SB_CPL_STS_1_7.desired, 0)
     `RAL_FIELD_CP(IP_SB_CPL_STS_1_8, IP_SB_CPL_STS_1_8.desired)
     `RAL_FIELD_CP_1(IP_SB_CPL_STS_1_8, IP_SB_CPL_STS_1_8.desired, 0)
     `RAL_FIELD_CP(IP_SB_CPL_STS_1_9, IP_SB_CPL_STS_1_9.desired)
     `RAL_FIELD_CP_1(IP_SB_CPL_STS_1_9, IP_SB_CPL_STS_1_9.desired, 0)
     `RAL_FIELD_CP(IP_SB_CPL_STS_1_10, IP_SB_CPL_STS_1_10.desired)
     `RAL_FIELD_CP_1(IP_SB_CPL_STS_1_10, IP_SB_CPL_STS_1_10.desired, 0)
     `RAL_FIELD_CP(IP_SB_CPL_STS_1_11, IP_SB_CPL_STS_1_11.desired)
     `RAL_FIELD_CP_1(IP_SB_CPL_STS_1_11, IP_SB_CPL_STS_1_11.desired, 0)
     `RAL_FIELD_CP(IP_SB_CPL_STS_1_12, IP_SB_CPL_STS_1_12.desired)
     `RAL_FIELD_CP_1(IP_SB_CPL_STS_1_12, IP_SB_CPL_STS_1_12.desired, 0)
     `RAL_FIELD_CP(IP_SB_CPL_STS_1_13, IP_SB_CPL_STS_1_13.desired)
     `RAL_FIELD_CP_1(IP_SB_CPL_STS_1_13, IP_SB_CPL_STS_1_13.desired, 0)
     `RAL_FIELD_CP(IP_SB_CPL_STS_1_14, IP_SB_CPL_STS_1_14.desired)
     `RAL_FIELD_CP_1(IP_SB_CPL_STS_1_14, IP_SB_CPL_STS_1_14.desired, 0)
     `RAL_FIELD_CP(IP_SB_CPL_STS_1_15, IP_SB_CPL_STS_1_15.desired)
     `RAL_FIELD_CP_1(IP_SB_CPL_STS_1_15, IP_SB_CPL_STS_1_15.desired, 0)
     `RAL_FIELD_CP(IP_SB_CPL_STS_1_16, IP_SB_CPL_STS_1_16.desired)
     `RAL_FIELD_CP_1(IP_SB_CPL_STS_1_16, IP_SB_CPL_STS_1_16.desired, 0)
     `RAL_FIELD_CP(IP_SB_CPL_STS_1_17, IP_SB_CPL_STS_1_17.desired)
     `RAL_FIELD_CP_1(IP_SB_CPL_STS_1_17, IP_SB_CPL_STS_1_17.desired, 0)
     `RAL_FIELD_CP(IP_SB_CPL_STS_1_18, IP_SB_CPL_STS_1_18.desired)
     `RAL_FIELD_CP_1(IP_SB_CPL_STS_1_18, IP_SB_CPL_STS_1_18.desired, 0)
     `RAL_FIELD_CP(IP_SB_CPL_STS_1_19, IP_SB_CPL_STS_1_19.desired)
     `RAL_FIELD_CP_1(IP_SB_CPL_STS_1_19, IP_SB_CPL_STS_1_19.desired, 0)
     `RAL_FIELD_CP(IP_SB_CPL_STS_1_20, IP_SB_CPL_STS_1_20.desired)
     `RAL_FIELD_CP_1(IP_SB_CPL_STS_1_20, IP_SB_CPL_STS_1_20.desired, 0)
     `RAL_FIELD_CP(IP_SB_CPL_STS_1_21, IP_SB_CPL_STS_1_21.desired)
     `RAL_FIELD_CP_1(IP_SB_CPL_STS_1_21, IP_SB_CPL_STS_1_21.desired, 0)
     `RAL_FIELD_CP(IP_SB_CPL_STS_1_22, IP_SB_CPL_STS_1_22.desired)
     `RAL_FIELD_CP_1(IP_SB_CPL_STS_1_22, IP_SB_CPL_STS_1_22.desired, 0)
     `RAL_FIELD_CP(IP_SB_CPL_STS_1_23, IP_SB_CPL_STS_1_23.desired)
     `RAL_FIELD_CP_1(IP_SB_CPL_STS_1_23, IP_SB_CPL_STS_1_23.desired, 0)
     `RAL_FIELD_CP(IP_SB_CPL_STS_1_24, IP_SB_CPL_STS_1_24.desired)
     `RAL_FIELD_CP_1(IP_SB_CPL_STS_1_24, IP_SB_CPL_STS_1_24.desired, 0)
     `RAL_FIELD_CP(IP_SB_CPL_STS_1_25, IP_SB_CPL_STS_1_25.desired)
     `RAL_FIELD_CP_1(IP_SB_CPL_STS_1_25, IP_SB_CPL_STS_1_25.desired, 0)
     `RAL_FIELD_CP(IP_SB_CPL_STS_1_26, IP_SB_CPL_STS_1_26.desired)
     `RAL_FIELD_CP_1(IP_SB_CPL_STS_1_26, IP_SB_CPL_STS_1_26.desired, 0)
     `RAL_FIELD_CP(IP_SB_CPL_STS_1_27, IP_SB_CPL_STS_1_27.desired)
     `RAL_FIELD_CP_1(IP_SB_CPL_STS_1_27, IP_SB_CPL_STS_1_27.desired, 0)
     `RAL_FIELD_CP(IP_SB_CPL_STS_1_28, IP_SB_CPL_STS_1_28.desired)
     `RAL_FIELD_CP_1(IP_SB_CPL_STS_1_28, IP_SB_CPL_STS_1_28.desired, 0)
     `RAL_FIELD_CP(IP_SB_CPL_STS_1_29, IP_SB_CPL_STS_1_29.desired)
     `RAL_FIELD_CP_1(IP_SB_CPL_STS_1_29, IP_SB_CPL_STS_1_29.desired, 0)
     `RAL_FIELD_CP(IP_SB_CPL_STS_1_30, IP_SB_CPL_STS_1_30.desired)
     `RAL_FIELD_CP_1(IP_SB_CPL_STS_1_30, IP_SB_CPL_STS_1_30.desired, 0)
     `RAL_FIELD_CP(IP_SB_CPL_STS_1_31, IP_SB_CPL_STS_1_31.desired)
     `RAL_FIELD_CP_1(IP_SB_CPL_STS_1_31, IP_SB_CPL_STS_1_31.desired, 0)
  endgroup
  covergroup actual_cg @(cov_ev);
     option.per_instance = 1;
     option.name = {sla_ral_file::regfilename,".",regname};
     `RAL_FIELD_CP(IP_SB_CPL_STS_1_0, IP_SB_CPL_STS_1_0.actual)
     `RAL_FIELD_CP_1(IP_SB_CPL_STS_1_0, IP_SB_CPL_STS_1_0.actual, 0)
     `RAL_FIELD_CP(IP_SB_CPL_STS_1_1, IP_SB_CPL_STS_1_1.actual)
     `RAL_FIELD_CP_1(IP_SB_CPL_STS_1_1, IP_SB_CPL_STS_1_1.actual, 0)
     `RAL_FIELD_CP(IP_SB_CPL_STS_1_2, IP_SB_CPL_STS_1_2.actual)
     `RAL_FIELD_CP_1(IP_SB_CPL_STS_1_2, IP_SB_CPL_STS_1_2.actual, 0)
     `RAL_FIELD_CP(IP_SB_CPL_STS_1_3, IP_SB_CPL_STS_1_3.actual)
     `RAL_FIELD_CP_1(IP_SB_CPL_STS_1_3, IP_SB_CPL_STS_1_3.actual, 0)
     `RAL_FIELD_CP(IP_SB_CPL_STS_1_4, IP_SB_CPL_STS_1_4.actual)
     `RAL_FIELD_CP_1(IP_SB_CPL_STS_1_4, IP_SB_CPL_STS_1_4.actual, 0)
     `RAL_FIELD_CP(IP_SB_CPL_STS_1_5, IP_SB_CPL_STS_1_5.actual)
     `RAL_FIELD_CP_1(IP_SB_CPL_STS_1_5, IP_SB_CPL_STS_1_5.actual, 0)
     `RAL_FIELD_CP(IP_SB_CPL_STS_1_6, IP_SB_CPL_STS_1_6.actual)
     `RAL_FIELD_CP_1(IP_SB_CPL_STS_1_6, IP_SB_CPL_STS_1_6.actual, 0)
     `RAL_FIELD_CP(IP_SB_CPL_STS_1_7, IP_SB_CPL_STS_1_7.actual)
     `RAL_FIELD_CP_1(IP_SB_CPL_STS_1_7, IP_SB_CPL_STS_1_7.actual, 0)
     `RAL_FIELD_CP(IP_SB_CPL_STS_1_8, IP_SB_CPL_STS_1_8.actual)
     `RAL_FIELD_CP_1(IP_SB_CPL_STS_1_8, IP_SB_CPL_STS_1_8.actual, 0)
     `RAL_FIELD_CP(IP_SB_CPL_STS_1_9, IP_SB_CPL_STS_1_9.actual)
     `RAL_FIELD_CP_1(IP_SB_CPL_STS_1_9, IP_SB_CPL_STS_1_9.actual, 0)
     `RAL_FIELD_CP(IP_SB_CPL_STS_1_10, IP_SB_CPL_STS_1_10.actual)
     `RAL_FIELD_CP_1(IP_SB_CPL_STS_1_10, IP_SB_CPL_STS_1_10.actual, 0)
     `RAL_FIELD_CP(IP_SB_CPL_STS_1_11, IP_SB_CPL_STS_1_11.actual)
     `RAL_FIELD_CP_1(IP_SB_CPL_STS_1_11, IP_SB_CPL_STS_1_11.actual, 0)
     `RAL_FIELD_CP(IP_SB_CPL_STS_1_12, IP_SB_CPL_STS_1_12.actual)
     `RAL_FIELD_CP_1(IP_SB_CPL_STS_1_12, IP_SB_CPL_STS_1_12.actual, 0)
     `RAL_FIELD_CP(IP_SB_CPL_STS_1_13, IP_SB_CPL_STS_1_13.actual)
     `RAL_FIELD_CP_1(IP_SB_CPL_STS_1_13, IP_SB_CPL_STS_1_13.actual, 0)
     `RAL_FIELD_CP(IP_SB_CPL_STS_1_14, IP_SB_CPL_STS_1_14.actual)
     `RAL_FIELD_CP_1(IP_SB_CPL_STS_1_14, IP_SB_CPL_STS_1_14.actual, 0)
     `RAL_FIELD_CP(IP_SB_CPL_STS_1_15, IP_SB_CPL_STS_1_15.actual)
     `RAL_FIELD_CP_1(IP_SB_CPL_STS_1_15, IP_SB_CPL_STS_1_15.actual, 0)
     `RAL_FIELD_CP(IP_SB_CPL_STS_1_16, IP_SB_CPL_STS_1_16.actual)
     `RAL_FIELD_CP_1(IP_SB_CPL_STS_1_16, IP_SB_CPL_STS_1_16.actual, 0)
     `RAL_FIELD_CP(IP_SB_CPL_STS_1_17, IP_SB_CPL_STS_1_17.actual)
     `RAL_FIELD_CP_1(IP_SB_CPL_STS_1_17, IP_SB_CPL_STS_1_17.actual, 0)
     `RAL_FIELD_CP(IP_SB_CPL_STS_1_18, IP_SB_CPL_STS_1_18.actual)
     `RAL_FIELD_CP_1(IP_SB_CPL_STS_1_18, IP_SB_CPL_STS_1_18.actual, 0)
     `RAL_FIELD_CP(IP_SB_CPL_STS_1_19, IP_SB_CPL_STS_1_19.actual)
     `RAL_FIELD_CP_1(IP_SB_CPL_STS_1_19, IP_SB_CPL_STS_1_19.actual, 0)
     `RAL_FIELD_CP(IP_SB_CPL_STS_1_20, IP_SB_CPL_STS_1_20.actual)
     `RAL_FIELD_CP_1(IP_SB_CPL_STS_1_20, IP_SB_CPL_STS_1_20.actual, 0)
     `RAL_FIELD_CP(IP_SB_CPL_STS_1_21, IP_SB_CPL_STS_1_21.actual)
     `RAL_FIELD_CP_1(IP_SB_CPL_STS_1_21, IP_SB_CPL_STS_1_21.actual, 0)
     `RAL_FIELD_CP(IP_SB_CPL_STS_1_22, IP_SB_CPL_STS_1_22.actual)
     `RAL_FIELD_CP_1(IP_SB_CPL_STS_1_22, IP_SB_CPL_STS_1_22.actual, 0)
     `RAL_FIELD_CP(IP_SB_CPL_STS_1_23, IP_SB_CPL_STS_1_23.actual)
     `RAL_FIELD_CP_1(IP_SB_CPL_STS_1_23, IP_SB_CPL_STS_1_23.actual, 0)
     `RAL_FIELD_CP(IP_SB_CPL_STS_1_24, IP_SB_CPL_STS_1_24.actual)
     `RAL_FIELD_CP_1(IP_SB_CPL_STS_1_24, IP_SB_CPL_STS_1_24.actual, 0)
     `RAL_FIELD_CP(IP_SB_CPL_STS_1_25, IP_SB_CPL_STS_1_25.actual)
     `RAL_FIELD_CP_1(IP_SB_CPL_STS_1_25, IP_SB_CPL_STS_1_25.actual, 0)
     `RAL_FIELD_CP(IP_SB_CPL_STS_1_26, IP_SB_CPL_STS_1_26.actual)
     `RAL_FIELD_CP_1(IP_SB_CPL_STS_1_26, IP_SB_CPL_STS_1_26.actual, 0)
     `RAL_FIELD_CP(IP_SB_CPL_STS_1_27, IP_SB_CPL_STS_1_27.actual)
     `RAL_FIELD_CP_1(IP_SB_CPL_STS_1_27, IP_SB_CPL_STS_1_27.actual, 0)
     `RAL_FIELD_CP(IP_SB_CPL_STS_1_28, IP_SB_CPL_STS_1_28.actual)
     `RAL_FIELD_CP_1(IP_SB_CPL_STS_1_28, IP_SB_CPL_STS_1_28.actual, 0)
     `RAL_FIELD_CP(IP_SB_CPL_STS_1_29, IP_SB_CPL_STS_1_29.actual)
     `RAL_FIELD_CP_1(IP_SB_CPL_STS_1_29, IP_SB_CPL_STS_1_29.actual, 0)
     `RAL_FIELD_CP(IP_SB_CPL_STS_1_30, IP_SB_CPL_STS_1_30.actual)
     `RAL_FIELD_CP_1(IP_SB_CPL_STS_1_30, IP_SB_CPL_STS_1_30.actual, 0)
     `RAL_FIELD_CP(IP_SB_CPL_STS_1_31, IP_SB_CPL_STS_1_31.actual)
     `RAL_FIELD_CP_1(IP_SB_CPL_STS_1_31, IP_SB_CPL_STS_1_31.actual, 0)
  endgroup
  `endif

  // --------------------------
  // add constraint based on database legal values
  // --------------------------
  function void build();
    IP_SB_CPL_STS_1_0 = new("IP_SB_CPL_STS_1_0", "RW/1C/V", 1, 0, {"IP_SB_CPL_STS_1.IP_SB_CPL_STS_1_0"});
    IP_SB_CPL_STS_1_0.set_powerwell("primary");
    IP_SB_CPL_STS_1_0.set_rand_mode(0);
   IP_SB_CPL_STS_1_0.set_reset_signame("pmu_rst_b");
    void'(add_field( IP_SB_CPL_STS_1_0 ));

    IP_SB_CPL_STS_1_1 = new("IP_SB_CPL_STS_1_1", "RW/1C/V", 1, 1, {"IP_SB_CPL_STS_1.IP_SB_CPL_STS_1_1"});
    IP_SB_CPL_STS_1_1.set_powerwell("primary");
    IP_SB_CPL_STS_1_1.set_rand_mode(0);
   IP_SB_CPL_STS_1_1.set_reset_signame("pmu_rst_b");
    void'(add_field( IP_SB_CPL_STS_1_1 ));

    IP_SB_CPL_STS_1_2 = new("IP_SB_CPL_STS_1_2", "RW/1C/V", 1, 2, {"IP_SB_CPL_STS_1.IP_SB_CPL_STS_1_2"});
    IP_SB_CPL_STS_1_2.set_powerwell("primary");
    IP_SB_CPL_STS_1_2.set_rand_mode(0);
   IP_SB_CPL_STS_1_2.set_reset_signame("pmu_rst_b");
    void'(add_field( IP_SB_CPL_STS_1_2 ));

    IP_SB_CPL_STS_1_3 = new("IP_SB_CPL_STS_1_3", "RW/1C/V", 1, 3, {"IP_SB_CPL_STS_1.IP_SB_CPL_STS_1_3"});
    IP_SB_CPL_STS_1_3.set_powerwell("primary");
    IP_SB_CPL_STS_1_3.set_rand_mode(0);
   IP_SB_CPL_STS_1_3.set_reset_signame("pmu_rst_b");
    void'(add_field( IP_SB_CPL_STS_1_3 ));

    IP_SB_CPL_STS_1_4 = new("IP_SB_CPL_STS_1_4", "RW/1C/V", 1, 4, {"IP_SB_CPL_STS_1.IP_SB_CPL_STS_1_4"});
    IP_SB_CPL_STS_1_4.set_powerwell("primary");
    IP_SB_CPL_STS_1_4.set_rand_mode(0);
   IP_SB_CPL_STS_1_4.set_reset_signame("pmu_rst_b");
    void'(add_field( IP_SB_CPL_STS_1_4 ));

    IP_SB_CPL_STS_1_5 = new("IP_SB_CPL_STS_1_5", "RW/1C/V", 1, 5, {"IP_SB_CPL_STS_1.IP_SB_CPL_STS_1_5"});
    IP_SB_CPL_STS_1_5.set_powerwell("primary");
    IP_SB_CPL_STS_1_5.set_rand_mode(0);
   IP_SB_CPL_STS_1_5.set_reset_signame("pmu_rst_b");
    void'(add_field( IP_SB_CPL_STS_1_5 ));

    IP_SB_CPL_STS_1_6 = new("IP_SB_CPL_STS_1_6", "RW/1C/V", 1, 6, {"IP_SB_CPL_STS_1.IP_SB_CPL_STS_1_6"});
    IP_SB_CPL_STS_1_6.set_powerwell("primary");
    IP_SB_CPL_STS_1_6.set_rand_mode(0);
   IP_SB_CPL_STS_1_6.set_reset_signame("pmu_rst_b");
    void'(add_field( IP_SB_CPL_STS_1_6 ));

    IP_SB_CPL_STS_1_7 = new("IP_SB_CPL_STS_1_7", "RW/1C/V", 1, 7, {"IP_SB_CPL_STS_1.IP_SB_CPL_STS_1_7"});
    IP_SB_CPL_STS_1_7.set_powerwell("primary");
    IP_SB_CPL_STS_1_7.set_rand_mode(0);
   IP_SB_CPL_STS_1_7.set_reset_signame("pmu_rst_b");
    void'(add_field( IP_SB_CPL_STS_1_7 ));

    IP_SB_CPL_STS_1_8 = new("IP_SB_CPL_STS_1_8", "RW/1C/V", 1, 8, {"IP_SB_CPL_STS_1.IP_SB_CPL_STS_1_8"});
    IP_SB_CPL_STS_1_8.set_powerwell("primary");
    IP_SB_CPL_STS_1_8.set_rand_mode(0);
   IP_SB_CPL_STS_1_8.set_reset_signame("pmu_rst_b");
    void'(add_field( IP_SB_CPL_STS_1_8 ));

    IP_SB_CPL_STS_1_9 = new("IP_SB_CPL_STS_1_9", "RW/1C/V", 1, 9, {"IP_SB_CPL_STS_1.IP_SB_CPL_STS_1_9"});
    IP_SB_CPL_STS_1_9.set_powerwell("primary");
    IP_SB_CPL_STS_1_9.set_rand_mode(0);
   IP_SB_CPL_STS_1_9.set_reset_signame("pmu_rst_b");
    void'(add_field( IP_SB_CPL_STS_1_9 ));

    IP_SB_CPL_STS_1_10 = new("IP_SB_CPL_STS_1_10", "RW/1C/V", 1, 10, {"IP_SB_CPL_STS_1.IP_SB_CPL_STS_1_10"});
    IP_SB_CPL_STS_1_10.set_powerwell("primary");
    IP_SB_CPL_STS_1_10.set_rand_mode(0);
   IP_SB_CPL_STS_1_10.set_reset_signame("pmu_rst_b");
    void'(add_field( IP_SB_CPL_STS_1_10 ));

    IP_SB_CPL_STS_1_11 = new("IP_SB_CPL_STS_1_11", "RW/1C/V", 1, 11, {"IP_SB_CPL_STS_1.IP_SB_CPL_STS_1_11"});
    IP_SB_CPL_STS_1_11.set_powerwell("primary");
    IP_SB_CPL_STS_1_11.set_rand_mode(0);
   IP_SB_CPL_STS_1_11.set_reset_signame("pmu_rst_b");
    void'(add_field( IP_SB_CPL_STS_1_11 ));

    IP_SB_CPL_STS_1_12 = new("IP_SB_CPL_STS_1_12", "RW/1C/V", 1, 12, {"IP_SB_CPL_STS_1.IP_SB_CPL_STS_1_12"});
    IP_SB_CPL_STS_1_12.set_powerwell("primary");
    IP_SB_CPL_STS_1_12.set_rand_mode(0);
   IP_SB_CPL_STS_1_12.set_reset_signame("pmu_rst_b");
    void'(add_field( IP_SB_CPL_STS_1_12 ));

    IP_SB_CPL_STS_1_13 = new("IP_SB_CPL_STS_1_13", "RW/1C/V", 1, 13, {"IP_SB_CPL_STS_1.IP_SB_CPL_STS_1_13"});
    IP_SB_CPL_STS_1_13.set_powerwell("primary");
    IP_SB_CPL_STS_1_13.set_rand_mode(0);
   IP_SB_CPL_STS_1_13.set_reset_signame("pmu_rst_b");
    void'(add_field( IP_SB_CPL_STS_1_13 ));

    IP_SB_CPL_STS_1_14 = new("IP_SB_CPL_STS_1_14", "RW/1C/V", 1, 14, {"IP_SB_CPL_STS_1.IP_SB_CPL_STS_1_14"});
    IP_SB_CPL_STS_1_14.set_powerwell("primary");
    IP_SB_CPL_STS_1_14.set_rand_mode(0);
   IP_SB_CPL_STS_1_14.set_reset_signame("pmu_rst_b");
    void'(add_field( IP_SB_CPL_STS_1_14 ));

    IP_SB_CPL_STS_1_15 = new("IP_SB_CPL_STS_1_15", "RW/1C/V", 1, 15, {"IP_SB_CPL_STS_1.IP_SB_CPL_STS_1_15"});
    IP_SB_CPL_STS_1_15.set_powerwell("primary");
    IP_SB_CPL_STS_1_15.set_rand_mode(0);
   IP_SB_CPL_STS_1_15.set_reset_signame("pmu_rst_b");
    void'(add_field( IP_SB_CPL_STS_1_15 ));

    IP_SB_CPL_STS_1_16 = new("IP_SB_CPL_STS_1_16", "RW/1C/V", 1, 16, {"IP_SB_CPL_STS_1.IP_SB_CPL_STS_1_16"});
    IP_SB_CPL_STS_1_16.set_powerwell("primary");
    IP_SB_CPL_STS_1_16.set_rand_mode(0);
   IP_SB_CPL_STS_1_16.set_reset_signame("pmu_rst_b");
    void'(add_field( IP_SB_CPL_STS_1_16 ));

    IP_SB_CPL_STS_1_17 = new("IP_SB_CPL_STS_1_17", "RW/1C/V", 1, 17, {"IP_SB_CPL_STS_1.IP_SB_CPL_STS_1_17"});
    IP_SB_CPL_STS_1_17.set_powerwell("primary");
    IP_SB_CPL_STS_1_17.set_rand_mode(0);
   IP_SB_CPL_STS_1_17.set_reset_signame("pmu_rst_b");
    void'(add_field( IP_SB_CPL_STS_1_17 ));

    IP_SB_CPL_STS_1_18 = new("IP_SB_CPL_STS_1_18", "RW/1C/V", 1, 18, {"IP_SB_CPL_STS_1.IP_SB_CPL_STS_1_18"});
    IP_SB_CPL_STS_1_18.set_powerwell("primary");
    IP_SB_CPL_STS_1_18.set_rand_mode(0);
   IP_SB_CPL_STS_1_18.set_reset_signame("pmu_rst_b");
    void'(add_field( IP_SB_CPL_STS_1_18 ));

    IP_SB_CPL_STS_1_19 = new("IP_SB_CPL_STS_1_19", "RW/1C/V", 1, 19, {"IP_SB_CPL_STS_1.IP_SB_CPL_STS_1_19"});
    IP_SB_CPL_STS_1_19.set_powerwell("primary");
    IP_SB_CPL_STS_1_19.set_rand_mode(0);
   IP_SB_CPL_STS_1_19.set_reset_signame("pmu_rst_b");
    void'(add_field( IP_SB_CPL_STS_1_19 ));

    IP_SB_CPL_STS_1_20 = new("IP_SB_CPL_STS_1_20", "RW/1C/V", 1, 20, {"IP_SB_CPL_STS_1.IP_SB_CPL_STS_1_20"});
    IP_SB_CPL_STS_1_20.set_powerwell("primary");
    IP_SB_CPL_STS_1_20.set_rand_mode(0);
   IP_SB_CPL_STS_1_20.set_reset_signame("pmu_rst_b");
    void'(add_field( IP_SB_CPL_STS_1_20 ));

    IP_SB_CPL_STS_1_21 = new("IP_SB_CPL_STS_1_21", "RW/1C/V", 1, 21, {"IP_SB_CPL_STS_1.IP_SB_CPL_STS_1_21"});
    IP_SB_CPL_STS_1_21.set_powerwell("primary");
    IP_SB_CPL_STS_1_21.set_rand_mode(0);
   IP_SB_CPL_STS_1_21.set_reset_signame("pmu_rst_b");
    void'(add_field( IP_SB_CPL_STS_1_21 ));

    IP_SB_CPL_STS_1_22 = new("IP_SB_CPL_STS_1_22", "RW/1C/V", 1, 22, {"IP_SB_CPL_STS_1.IP_SB_CPL_STS_1_22"});
    IP_SB_CPL_STS_1_22.set_powerwell("primary");
    IP_SB_CPL_STS_1_22.set_rand_mode(0);
   IP_SB_CPL_STS_1_22.set_reset_signame("pmu_rst_b");
    void'(add_field( IP_SB_CPL_STS_1_22 ));

    IP_SB_CPL_STS_1_23 = new("IP_SB_CPL_STS_1_23", "RW/1C/V", 1, 23, {"IP_SB_CPL_STS_1.IP_SB_CPL_STS_1_23"});
    IP_SB_CPL_STS_1_23.set_powerwell("primary");
    IP_SB_CPL_STS_1_23.set_rand_mode(0);
   IP_SB_CPL_STS_1_23.set_reset_signame("pmu_rst_b");
    void'(add_field( IP_SB_CPL_STS_1_23 ));

    IP_SB_CPL_STS_1_24 = new("IP_SB_CPL_STS_1_24", "RW/1C/V", 1, 24, {"IP_SB_CPL_STS_1.IP_SB_CPL_STS_1_24"});
    IP_SB_CPL_STS_1_24.set_powerwell("primary");
    IP_SB_CPL_STS_1_24.set_rand_mode(0);
   IP_SB_CPL_STS_1_24.set_reset_signame("pmu_rst_b");
    void'(add_field( IP_SB_CPL_STS_1_24 ));

    IP_SB_CPL_STS_1_25 = new("IP_SB_CPL_STS_1_25", "RW/1C/V", 1, 25, {"IP_SB_CPL_STS_1.IP_SB_CPL_STS_1_25"});
    IP_SB_CPL_STS_1_25.set_powerwell("primary");
    IP_SB_CPL_STS_1_25.set_rand_mode(0);
   IP_SB_CPL_STS_1_25.set_reset_signame("pmu_rst_b");
    void'(add_field( IP_SB_CPL_STS_1_25 ));

    IP_SB_CPL_STS_1_26 = new("IP_SB_CPL_STS_1_26", "RW/1C/V", 1, 26, {"IP_SB_CPL_STS_1.IP_SB_CPL_STS_1_26"});
    IP_SB_CPL_STS_1_26.set_powerwell("primary");
    IP_SB_CPL_STS_1_26.set_rand_mode(0);
   IP_SB_CPL_STS_1_26.set_reset_signame("pmu_rst_b");
    void'(add_field( IP_SB_CPL_STS_1_26 ));

    IP_SB_CPL_STS_1_27 = new("IP_SB_CPL_STS_1_27", "RW/1C/V", 1, 27, {"IP_SB_CPL_STS_1.IP_SB_CPL_STS_1_27"});
    IP_SB_CPL_STS_1_27.set_powerwell("primary");
    IP_SB_CPL_STS_1_27.set_rand_mode(0);
   IP_SB_CPL_STS_1_27.set_reset_signame("pmu_rst_b");
    void'(add_field( IP_SB_CPL_STS_1_27 ));

    IP_SB_CPL_STS_1_28 = new("IP_SB_CPL_STS_1_28", "RW/1C/V", 1, 28, {"IP_SB_CPL_STS_1.IP_SB_CPL_STS_1_28"});
    IP_SB_CPL_STS_1_28.set_powerwell("primary");
    IP_SB_CPL_STS_1_28.set_rand_mode(0);
   IP_SB_CPL_STS_1_28.set_reset_signame("pmu_rst_b");
    void'(add_field( IP_SB_CPL_STS_1_28 ));

    IP_SB_CPL_STS_1_29 = new("IP_SB_CPL_STS_1_29", "RW/1C/V", 1, 29, {"IP_SB_CPL_STS_1.IP_SB_CPL_STS_1_29"});
    IP_SB_CPL_STS_1_29.set_powerwell("primary");
    IP_SB_CPL_STS_1_29.set_rand_mode(0);
   IP_SB_CPL_STS_1_29.set_reset_signame("pmu_rst_b");
    void'(add_field( IP_SB_CPL_STS_1_29 ));

    IP_SB_CPL_STS_1_30 = new("IP_SB_CPL_STS_1_30", "RW/1C/V", 1, 30, {"IP_SB_CPL_STS_1.IP_SB_CPL_STS_1_30"});
    IP_SB_CPL_STS_1_30.set_powerwell("primary");
    IP_SB_CPL_STS_1_30.set_rand_mode(0);
   IP_SB_CPL_STS_1_30.set_reset_signame("pmu_rst_b");
    void'(add_field( IP_SB_CPL_STS_1_30 ));

    IP_SB_CPL_STS_1_31 = new("IP_SB_CPL_STS_1_31", "RW/1C/V", 1, 31, {"IP_SB_CPL_STS_1.IP_SB_CPL_STS_1_31"});
    IP_SB_CPL_STS_1_31.set_powerwell("primary");
    IP_SB_CPL_STS_1_31.set_rand_mode(0);
   IP_SB_CPL_STS_1_31.set_reset_signame("pmu_rst_b");
    void'(add_field( IP_SB_CPL_STS_1_31 ));

  endfunction


`DEFINE_NONE_SCOPE_REG_API(pmu_mmr_IP_SB_CPL_STS_1_reg) 
endclass : pmu_mmr_IP_SB_CPL_STS_1_reg

// ================================================

class pmu_mmr_SBI_MSTR_CTL_P_reg extends sla_ral_reg;

  // --------------------------
  sla_ral_field SEND_PC_SB;
  sla_ral_field PC_ADDR_VLD;
  sla_ral_field PC_DW0_VLD;
  sla_ral_field PC_DW1_VLD;
  sla_ral_field PC_EXHDR_VLD;
  sla_ral_field PC_FW_SRC_PID_VLD;
  sla_ral_field PC_FW_RS;
  sla_ral_field PC_MSG_DIS;

  // --------------------------
  `ovm_object_utils(pmu_mmr_SBI_MSTR_CTL_P_reg)

  // --------------------------
  function new(
                string         name = "",
                `ifdef SLA_RAL_COVERAGE
                sla_ral_coverage_t cov_t = COVERAGE_OFF,
                `endif
                int            bus_num=0,
                int            dev_num=0,
                int            func_num=0,
                int            offset=0,
                int            size=0,
                sla_ral_data_t reset_val=0,
                boolean_t      mon_enabled = SLA_FALSE
             );
      super.new();
      set_space_addr("CFG",undef);
      set_space_addr("MEM",undef);
      set_space_addr("IO",undef);
      set_space_addr("MSG",undef);
      set_space_addr("CREG",undef); 
      set_space_addr("LT_MEM",undef);    
 build();

      `ifdef SLA_RAL_COVERAGE
      `CONFIG_RAL_COVERAGE(regname, COVERAGE_ON, FD_RW_SAMPLE)
      if(_coverage_type != COVERAGE_OFF) begin
        if(_sample_type inside {FD_RO_SAMPLE, RO_SAMPLE})
        `CREATE_OP_SAMPLE_COV(RAL_WRITE)
        else if(_sample_type inside {FD_WO_SAMPLE, WO_SAMPLE})
        `CREATE_OP_SAMPLE_COV(RAL_READ)
        else
        `CREATE_OP_SAMPLE_COV(RAL_NONE)
        `CREATE_DESIRED_COV
        `CREATE_ACTUAL_COV
      end
      `endif

  endfunction

  `ifdef SLA_RAL_COVERAGE
  `CREATE_RAL_OP_COVERGROUP
  covergroup desired_cg @(cov_ev);
     option.per_instance = 1;
     option.name = {sla_ral_file::regfilename,".",regname};
     `RAL_FIELD_CP(SEND_PC_SB, SEND_PC_SB.desired)
     `RAL_FIELD_CP_1(SEND_PC_SB, SEND_PC_SB.desired, 0)
     `RAL_FIELD_CP(PC_ADDR_VLD, PC_ADDR_VLD.desired)
     `RAL_FIELD_CP_1(PC_ADDR_VLD, PC_ADDR_VLD.desired, 0)
     `RAL_FIELD_CP(PC_DW0_VLD, PC_DW0_VLD.desired)
     `RAL_FIELD_CP_1(PC_DW0_VLD, PC_DW0_VLD.desired, 0)
     `RAL_FIELD_CP(PC_DW1_VLD, PC_DW1_VLD.desired)
     `RAL_FIELD_CP_1(PC_DW1_VLD, PC_DW1_VLD.desired, 0)
     `RAL_FIELD_CP(PC_EXHDR_VLD, PC_EXHDR_VLD.desired)
     `RAL_FIELD_CP_1(PC_EXHDR_VLD, PC_EXHDR_VLD.desired, 0)
     `RAL_FIELD_CP(PC_FW_SRC_PID_VLD, PC_FW_SRC_PID_VLD.desired)
     `RAL_FIELD_CP_1(PC_FW_SRC_PID_VLD, PC_FW_SRC_PID_VLD.desired, 0)
     `RAL_FIELD_CP(PC_FW_RS, PC_FW_RS.desired)
     `RAL_FIELD_CP_1(PC_FW_RS, PC_FW_RS.desired, 0)
     `RAL_FIELD_CP(PC_MSG_DIS, PC_MSG_DIS.desired)
     `RAL_FIELD_CP_1(PC_MSG_DIS, PC_MSG_DIS.desired, 0)
  endgroup
  covergroup actual_cg @(cov_ev);
     option.per_instance = 1;
     option.name = {sla_ral_file::regfilename,".",regname};
     `RAL_FIELD_CP(SEND_PC_SB, SEND_PC_SB.actual)
     `RAL_FIELD_CP_1(SEND_PC_SB, SEND_PC_SB.actual, 0)
     `RAL_FIELD_CP(PC_ADDR_VLD, PC_ADDR_VLD.actual)
     `RAL_FIELD_CP_1(PC_ADDR_VLD, PC_ADDR_VLD.actual, 0)
     `RAL_FIELD_CP(PC_DW0_VLD, PC_DW0_VLD.actual)
     `RAL_FIELD_CP_1(PC_DW0_VLD, PC_DW0_VLD.actual, 0)
     `RAL_FIELD_CP(PC_DW1_VLD, PC_DW1_VLD.actual)
     `RAL_FIELD_CP_1(PC_DW1_VLD, PC_DW1_VLD.actual, 0)
     `RAL_FIELD_CP(PC_EXHDR_VLD, PC_EXHDR_VLD.actual)
     `RAL_FIELD_CP_1(PC_EXHDR_VLD, PC_EXHDR_VLD.actual, 0)
     `RAL_FIELD_CP(PC_FW_SRC_PID_VLD, PC_FW_SRC_PID_VLD.actual)
     `RAL_FIELD_CP_1(PC_FW_SRC_PID_VLD, PC_FW_SRC_PID_VLD.actual, 0)
     `RAL_FIELD_CP(PC_FW_RS, PC_FW_RS.actual)
     `RAL_FIELD_CP_1(PC_FW_RS, PC_FW_RS.actual, 0)
     `RAL_FIELD_CP(PC_MSG_DIS, PC_MSG_DIS.actual)
     `RAL_FIELD_CP_1(PC_MSG_DIS, PC_MSG_DIS.actual, 0)
  endgroup
  `endif

  // --------------------------
  // add constraint based on database legal values
  // --------------------------
  function void build();
    SEND_PC_SB = new("SEND_PC_SB", "RW/1S/V", 1, 0, {"SBI_MSTR_CTL_P.SEND_PC_SB"});
    SEND_PC_SB.set_powerwell("primary");
    SEND_PC_SB.set_rand_mode(0);
   SEND_PC_SB.set_reset_signame("pmu_rst_b");
    void'(add_field( SEND_PC_SB ));

    PC_ADDR_VLD = new("PC_ADDR_VLD", "RW", 1, 1, {"SBI_MSTR_CTL_P.PC_ADDR_VLD"});
    PC_ADDR_VLD.set_powerwell("primary");
    PC_ADDR_VLD.set_rand_mode(0);
   PC_ADDR_VLD.set_reset_signame("pmu_rst_b");
    void'(add_field( PC_ADDR_VLD ));

    PC_DW0_VLD = new("PC_DW0_VLD", "RW", 1, 2, {"SBI_MSTR_CTL_P.PC_DW0_VLD"});
    PC_DW0_VLD.set_powerwell("primary");
    PC_DW0_VLD.set_rand_mode(0);
   PC_DW0_VLD.set_reset_signame("pmu_rst_b");
    void'(add_field( PC_DW0_VLD ));

    PC_DW1_VLD = new("PC_DW1_VLD", "RW", 1, 3, {"SBI_MSTR_CTL_P.PC_DW1_VLD"});
    PC_DW1_VLD.set_powerwell("primary");
    PC_DW1_VLD.set_rand_mode(0);
   PC_DW1_VLD.set_reset_signame("pmu_rst_b");
    void'(add_field( PC_DW1_VLD ));

    PC_EXHDR_VLD = new("PC_EXHDR_VLD", "RW", 1, 4, {"SBI_MSTR_CTL_P.PC_EXHDR_VLD"});
    PC_EXHDR_VLD.set_powerwell("primary");
    PC_EXHDR_VLD.set_rand_mode(0);
   PC_EXHDR_VLD.set_reset_signame("pmu_rst_b");
    void'(add_field( PC_EXHDR_VLD ));

    PC_FW_SRC_PID_VLD = new("PC_FW_SRC_PID_VLD", "RW", 1, 5, {"SBI_MSTR_CTL_P.PC_FW_SRC_PID_VLD"});
    PC_FW_SRC_PID_VLD.set_powerwell("primary");
    PC_FW_SRC_PID_VLD.set_rand_mode(0);
   PC_FW_SRC_PID_VLD.set_reset_signame("pmu_rst_b");
    void'(add_field( PC_FW_SRC_PID_VLD ));

    PC_FW_RS = new("PC_FW_RS", "RW", 1, 6, {"SBI_MSTR_CTL_P.PC_FW_RS"});
    PC_FW_RS.set_powerwell("primary");
    PC_FW_RS.set_rand_mode(0);
   PC_FW_RS.set_reset_signame("pmu_rst_b");
    void'(add_field( PC_FW_RS ));

    PC_MSG_DIS = new("PC_MSG_DIS", "RW", 1, 7, {"SBI_MSTR_CTL_P.PC_MSG_DIS"});
    PC_MSG_DIS.set_powerwell("primary");
    PC_MSG_DIS.set_rand_mode(0);
   PC_MSG_DIS.set_reset_signame("pmu_rst_b");
    void'(add_field( PC_MSG_DIS ));

  endfunction


`DEFINE_NONE_SCOPE_REG_API(pmu_mmr_SBI_MSTR_CTL_P_reg) 
endclass : pmu_mmr_SBI_MSTR_CTL_P_reg

// ================================================

class pmu_mmr_SBI_MSTR_HDR_P_reg extends sla_ral_reg;

  // --------------------------
  sla_ral_field DEST_PID_PC_SB;
  sla_ral_field SRC_PID_PC_SB;
  sla_ral_field OPCODE_PC_SB;
  sla_ral_field TAG_PC_SB;
  sla_ral_field PC_MSG_HDR_FLD_1;
  sla_ral_field EH_PC_SB;

  // --------------------------
  `ovm_object_utils(pmu_mmr_SBI_MSTR_HDR_P_reg)

  // --------------------------
  function new(
                string         name = "",
                `ifdef SLA_RAL_COVERAGE
                sla_ral_coverage_t cov_t = COVERAGE_OFF,
                `endif
                int            bus_num=0,
                int            dev_num=0,
                int            func_num=0,
                int            offset=0,
                int            size=0,
                sla_ral_data_t reset_val=0,
                boolean_t      mon_enabled = SLA_FALSE
             );
      super.new();
      set_space_addr("CFG",undef);
      set_space_addr("MEM",undef);
      set_space_addr("IO",undef);
      set_space_addr("MSG",undef);
      set_space_addr("CREG",undef); 
      set_space_addr("LT_MEM",undef);    
 build();

      `ifdef SLA_RAL_COVERAGE
      `CONFIG_RAL_COVERAGE(regname, COVERAGE_ON, FD_RW_SAMPLE)
      if(_coverage_type != COVERAGE_OFF) begin
        if(_sample_type inside {FD_RO_SAMPLE, RO_SAMPLE})
        `CREATE_OP_SAMPLE_COV(RAL_WRITE)
        else if(_sample_type inside {FD_WO_SAMPLE, WO_SAMPLE})
        `CREATE_OP_SAMPLE_COV(RAL_READ)
        else
        `CREATE_OP_SAMPLE_COV(RAL_NONE)
        `CREATE_DESIRED_COV
        `CREATE_ACTUAL_COV
      end
      `endif

  endfunction

  `ifdef SLA_RAL_COVERAGE
  `CREATE_RAL_OP_COVERGROUP
  covergroup desired_cg @(cov_ev);
     option.per_instance = 1;
     option.name = {sla_ral_file::regfilename,".",regname};
     `RAL_FIELD_CP(DEST_PID_PC_SB, DEST_PID_PC_SB.desired)
     `RAL_FIELD_CP_8(DEST_PID_PC_SB, DEST_PID_PC_SB.desired, 0,1,2,3,4,5,6,7)
     `RAL_FIELD_CP(SRC_PID_PC_SB, SRC_PID_PC_SB.desired)
     `RAL_FIELD_CP_8(SRC_PID_PC_SB, SRC_PID_PC_SB.desired, 0,1,2,3,4,5,6,7)
     `RAL_FIELD_CP(OPCODE_PC_SB, OPCODE_PC_SB.desired)
     `RAL_FIELD_CP_8(OPCODE_PC_SB, OPCODE_PC_SB.desired, 0,1,2,3,4,5,6,7)
     `RAL_FIELD_CP(TAG_PC_SB, TAG_PC_SB.desired)
     `RAL_FIELD_CP_3(TAG_PC_SB, TAG_PC_SB.desired, 0,1,2)
     `RAL_FIELD_CP(PC_MSG_HDR_FLD_1, PC_MSG_HDR_FLD_1.desired)
     `RAL_FIELD_CP_4(PC_MSG_HDR_FLD_1, PC_MSG_HDR_FLD_1.desired, 0,1,2,3)
     `RAL_FIELD_CP(EH_PC_SB, EH_PC_SB.desired)
     `RAL_FIELD_CP_1(EH_PC_SB, EH_PC_SB.desired, 0)
  endgroup
  covergroup actual_cg @(cov_ev);
     option.per_instance = 1;
     option.name = {sla_ral_file::regfilename,".",regname};
     `RAL_FIELD_CP(DEST_PID_PC_SB, DEST_PID_PC_SB.actual)
     `RAL_FIELD_CP_8(DEST_PID_PC_SB, DEST_PID_PC_SB.actual, 0,1,2,3,4,5,6,7)
     `RAL_FIELD_CP(SRC_PID_PC_SB, SRC_PID_PC_SB.actual)
     `RAL_FIELD_CP_8(SRC_PID_PC_SB, SRC_PID_PC_SB.actual, 0,1,2,3,4,5,6,7)
     `RAL_FIELD_CP(OPCODE_PC_SB, OPCODE_PC_SB.actual)
     `RAL_FIELD_CP_8(OPCODE_PC_SB, OPCODE_PC_SB.actual, 0,1,2,3,4,5,6,7)
     `RAL_FIELD_CP(TAG_PC_SB, TAG_PC_SB.actual)
     `RAL_FIELD_CP_3(TAG_PC_SB, TAG_PC_SB.actual, 0,1,2)
     `RAL_FIELD_CP(PC_MSG_HDR_FLD_1, PC_MSG_HDR_FLD_1.actual)
     `RAL_FIELD_CP_4(PC_MSG_HDR_FLD_1, PC_MSG_HDR_FLD_1.actual, 0,1,2,3)
     `RAL_FIELD_CP(EH_PC_SB, EH_PC_SB.actual)
     `RAL_FIELD_CP_1(EH_PC_SB, EH_PC_SB.actual, 0)
  endgroup
  `endif

  // --------------------------
  // add constraint based on database legal values
  // --------------------------
  function void build();
    DEST_PID_PC_SB = new("DEST_PID_PC_SB", "RW", 8, 0, {"SBI_MSTR_HDR_P.DEST_PID_PC_SB"});
    DEST_PID_PC_SB.set_powerwell("primary");
    DEST_PID_PC_SB.set_rand_mode(0);
   DEST_PID_PC_SB.set_reset_signame("pmu_rst_b");
    void'(add_field( DEST_PID_PC_SB ));

    SRC_PID_PC_SB = new("SRC_PID_PC_SB", "RW", 8, 8, {"SBI_MSTR_HDR_P.SRC_PID_PC_SB"});
    SRC_PID_PC_SB.set_powerwell("primary");
    SRC_PID_PC_SB.set_rand_mode(0);
   SRC_PID_PC_SB.set_reset_signame("pmu_rst_b");
    void'(add_field( SRC_PID_PC_SB ));

    OPCODE_PC_SB = new("OPCODE_PC_SB", "RW", 8, 16, {"SBI_MSTR_HDR_P.OPCODE_PC_SB"});
    OPCODE_PC_SB.set_powerwell("primary");
    OPCODE_PC_SB.set_rand_mode(0);
   OPCODE_PC_SB.set_reset_signame("pmu_rst_b");
    void'(add_field( OPCODE_PC_SB ));

    TAG_PC_SB = new("TAG_PC_SB", "RW", 3, 24, {"SBI_MSTR_HDR_P.TAG_PC_SB"});
    TAG_PC_SB.set_powerwell("primary");
    TAG_PC_SB.set_rand_mode(0);
   TAG_PC_SB.set_reset_signame("pmu_rst_b");
    void'(add_field( TAG_PC_SB ));

    PC_MSG_HDR_FLD_1 = new("PC_MSG_HDR_FLD_1", "RW", 4, 27, {"SBI_MSTR_HDR_P.PC_MSG_HDR_FLD_1"});
    PC_MSG_HDR_FLD_1.set_powerwell("primary");
    PC_MSG_HDR_FLD_1.set_rand_mode(0);
   PC_MSG_HDR_FLD_1.set_reset_signame("pmu_rst_b");
    void'(add_field( PC_MSG_HDR_FLD_1 ));

    EH_PC_SB = new("EH_PC_SB", "RW", 1, 31, {"SBI_MSTR_HDR_P.EH_PC_SB"});
    EH_PC_SB.set_powerwell("primary");
    EH_PC_SB.set_rand_mode(0);
   EH_PC_SB.set_reset_signame("pmu_rst_b");
    void'(add_field( EH_PC_SB ));

  endfunction


`DEFINE_NONE_SCOPE_REG_API(pmu_mmr_SBI_MSTR_HDR_P_reg) 
endclass : pmu_mmr_SBI_MSTR_HDR_P_reg

// ================================================

class pmu_mmr_SBI_MSTR_EH_P_reg extends sla_ral_reg;

  // --------------------------
  sla_ral_field EXHDR_PC_SB;

  // --------------------------
  `ovm_object_utils(pmu_mmr_SBI_MSTR_EH_P_reg)

  // --------------------------
  function new(
                string         name = "",
                `ifdef SLA_RAL_COVERAGE
                sla_ral_coverage_t cov_t = COVERAGE_OFF,
                `endif
                int            bus_num=0,
                int            dev_num=0,
                int            func_num=0,
                int            offset=0,
                int            size=0,
                sla_ral_data_t reset_val=0,
                boolean_t      mon_enabled = SLA_FALSE
             );
      super.new();
      set_space_addr("CFG",undef);
      set_space_addr("MEM",undef);
      set_space_addr("IO",undef);
      set_space_addr("MSG",undef);
      set_space_addr("CREG",undef); 
      set_space_addr("LT_MEM",undef);    
 build();

      `ifdef SLA_RAL_COVERAGE
      `CONFIG_RAL_COVERAGE(regname, COVERAGE_ON, FD_RW_SAMPLE)
      if(_coverage_type != COVERAGE_OFF) begin
        if(_sample_type inside {FD_RO_SAMPLE, RO_SAMPLE})
        `CREATE_OP_SAMPLE_COV(RAL_WRITE)
        else if(_sample_type inside {FD_WO_SAMPLE, WO_SAMPLE})
        `CREATE_OP_SAMPLE_COV(RAL_READ)
        else
        `CREATE_OP_SAMPLE_COV(RAL_NONE)
        `CREATE_DESIRED_COV
        `CREATE_ACTUAL_COV
      end
      `endif

  endfunction

  `ifdef SLA_RAL_COVERAGE
  `CREATE_RAL_OP_COVERGROUP
  covergroup desired_cg @(cov_ev);
     option.per_instance = 1;
     option.name = {sla_ral_file::regfilename,".",regname};
     `RAL_FIELD_CP(EXHDR_PC_SB, EXHDR_PC_SB.desired)
     `RAL_FIELD_CP_16(EXHDR_PC_SB, EXHDR_PC_SB.desired, 0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)
     `RAL_FIELD_CP_16(EXHDR_PC_SB, EXHDR_PC_SB.desired, 16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31)
  endgroup
  covergroup actual_cg @(cov_ev);
     option.per_instance = 1;
     option.name = {sla_ral_file::regfilename,".",regname};
     `RAL_FIELD_CP(EXHDR_PC_SB, EXHDR_PC_SB.actual)
     `RAL_FIELD_CP_16(EXHDR_PC_SB, EXHDR_PC_SB.actual, 0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)
     `RAL_FIELD_CP_16(EXHDR_PC_SB, EXHDR_PC_SB.actual, 16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31)
  endgroup
  `endif

  // --------------------------
  // add constraint based on database legal values
  // --------------------------
  function void build();
    EXHDR_PC_SB = new("EXHDR_PC_SB", "RW", 32, 0, {"SBI_MSTR_EH_P.EXHDR_PC_SB"});
    EXHDR_PC_SB.set_powerwell("primary");
    EXHDR_PC_SB.set_rand_mode(0);
   EXHDR_PC_SB.set_reset_signame("pmu_rst_b");
    void'(add_field( EXHDR_PC_SB ));

  endfunction


`DEFINE_NONE_SCOPE_REG_API(pmu_mmr_SBI_MSTR_EH_P_reg) 
endclass : pmu_mmr_SBI_MSTR_EH_P_reg

// ================================================

class pmu_mmr_SBI_MSTR_ADDR_P_reg extends sla_ral_reg;

  // --------------------------
  sla_ral_field ADDR_PC_SB;

  // --------------------------
  `ovm_object_utils(pmu_mmr_SBI_MSTR_ADDR_P_reg)

  // --------------------------
  function new(
                string         name = "",
                `ifdef SLA_RAL_COVERAGE
                sla_ral_coverage_t cov_t = COVERAGE_OFF,
                `endif
                int            bus_num=0,
                int            dev_num=0,
                int            func_num=0,
                int            offset=0,
                int            size=0,
                sla_ral_data_t reset_val=0,
                boolean_t      mon_enabled = SLA_FALSE
             );
      super.new();
      set_space_addr("CFG",undef);
      set_space_addr("MEM",undef);
      set_space_addr("IO",undef);
      set_space_addr("MSG",undef);
      set_space_addr("CREG",undef); 
      set_space_addr("LT_MEM",undef);    
 build();

      `ifdef SLA_RAL_COVERAGE
      `CONFIG_RAL_COVERAGE(regname, COVERAGE_ON, FD_RW_SAMPLE)
      if(_coverage_type != COVERAGE_OFF) begin
        if(_sample_type inside {FD_RO_SAMPLE, RO_SAMPLE})
        `CREATE_OP_SAMPLE_COV(RAL_WRITE)
        else if(_sample_type inside {FD_WO_SAMPLE, WO_SAMPLE})
        `CREATE_OP_SAMPLE_COV(RAL_READ)
        else
        `CREATE_OP_SAMPLE_COV(RAL_NONE)
        `CREATE_DESIRED_COV
        `CREATE_ACTUAL_COV
      end
      `endif

  endfunction

  `ifdef SLA_RAL_COVERAGE
  `CREATE_RAL_OP_COVERGROUP
  covergroup desired_cg @(cov_ev);
     option.per_instance = 1;
     option.name = {sla_ral_file::regfilename,".",regname};
     `RAL_FIELD_CP(ADDR_PC_SB, ADDR_PC_SB.desired)
     `RAL_FIELD_CP_16(ADDR_PC_SB, ADDR_PC_SB.desired, 0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)
     `RAL_FIELD_CP_16(ADDR_PC_SB, ADDR_PC_SB.desired, 16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31)
  endgroup
  covergroup actual_cg @(cov_ev);
     option.per_instance = 1;
     option.name = {sla_ral_file::regfilename,".",regname};
     `RAL_FIELD_CP(ADDR_PC_SB, ADDR_PC_SB.actual)
     `RAL_FIELD_CP_16(ADDR_PC_SB, ADDR_PC_SB.actual, 0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)
     `RAL_FIELD_CP_16(ADDR_PC_SB, ADDR_PC_SB.actual, 16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31)
  endgroup
  `endif

  // --------------------------
  // add constraint based on database legal values
  // --------------------------
  function void build();
    ADDR_PC_SB = new("ADDR_PC_SB", "RW", 32, 0, {"SBI_MSTR_ADDR_P.ADDR_PC_SB"});
    ADDR_PC_SB.set_powerwell("primary");
    ADDR_PC_SB.set_rand_mode(0);
   ADDR_PC_SB.set_reset_signame("pmu_rst_b");
    void'(add_field( ADDR_PC_SB ));

  endfunction


`DEFINE_NONE_SCOPE_REG_API(pmu_mmr_SBI_MSTR_ADDR_P_reg) 
endclass : pmu_mmr_SBI_MSTR_ADDR_P_reg

// ================================================

class pmu_mmr_SBI_MSTR_DW0_P_reg extends sla_ral_reg;

  // --------------------------
  sla_ral_field DW0_PC_SB;

  // --------------------------
  `ovm_object_utils(pmu_mmr_SBI_MSTR_DW0_P_reg)

  // --------------------------
  function new(
                string         name = "",
                `ifdef SLA_RAL_COVERAGE
                sla_ral_coverage_t cov_t = COVERAGE_OFF,
                `endif
                int            bus_num=0,
                int            dev_num=0,
                int            func_num=0,
                int            offset=0,
                int            size=0,
                sla_ral_data_t reset_val=0,
                boolean_t      mon_enabled = SLA_FALSE
             );
      super.new();
      set_space_addr("CFG",undef);
      set_space_addr("MEM",undef);
      set_space_addr("IO",undef);
      set_space_addr("MSG",undef);
      set_space_addr("CREG",undef); 
      set_space_addr("LT_MEM",undef);    
 build();

      `ifdef SLA_RAL_COVERAGE
      `CONFIG_RAL_COVERAGE(regname, COVERAGE_ON, FD_RW_SAMPLE)
      if(_coverage_type != COVERAGE_OFF) begin
        if(_sample_type inside {FD_RO_SAMPLE, RO_SAMPLE})
        `CREATE_OP_SAMPLE_COV(RAL_WRITE)
        else if(_sample_type inside {FD_WO_SAMPLE, WO_SAMPLE})
        `CREATE_OP_SAMPLE_COV(RAL_READ)
        else
        `CREATE_OP_SAMPLE_COV(RAL_NONE)
        `CREATE_DESIRED_COV
        `CREATE_ACTUAL_COV
      end
      `endif

  endfunction

  `ifdef SLA_RAL_COVERAGE
  `CREATE_RAL_OP_COVERGROUP
  covergroup desired_cg @(cov_ev);
     option.per_instance = 1;
     option.name = {sla_ral_file::regfilename,".",regname};
     `RAL_FIELD_CP(DW0_PC_SB, DW0_PC_SB.desired)
     `RAL_FIELD_CP_16(DW0_PC_SB, DW0_PC_SB.desired, 0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)
     `RAL_FIELD_CP_16(DW0_PC_SB, DW0_PC_SB.desired, 16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31)
  endgroup
  covergroup actual_cg @(cov_ev);
     option.per_instance = 1;
     option.name = {sla_ral_file::regfilename,".",regname};
     `RAL_FIELD_CP(DW0_PC_SB, DW0_PC_SB.actual)
     `RAL_FIELD_CP_16(DW0_PC_SB, DW0_PC_SB.actual, 0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)
     `RAL_FIELD_CP_16(DW0_PC_SB, DW0_PC_SB.actual, 16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31)
  endgroup
  `endif

  // --------------------------
  // add constraint based on database legal values
  // --------------------------
  function void build();
    DW0_PC_SB = new("DW0_PC_SB", "RW", 32, 0, {"SBI_MSTR_DW0_P.DW0_PC_SB"});
    DW0_PC_SB.set_powerwell("primary");
    DW0_PC_SB.set_rand_mode(0);
   DW0_PC_SB.set_reset_signame("pmu_rst_b");
    void'(add_field( DW0_PC_SB ));

  endfunction


`DEFINE_NONE_SCOPE_REG_API(pmu_mmr_SBI_MSTR_DW0_P_reg) 
endclass : pmu_mmr_SBI_MSTR_DW0_P_reg

// ================================================

class pmu_mmr_SBI_MSTR_DW1_P_reg extends sla_ral_reg;

  // --------------------------
  sla_ral_field DW1_PC_SB;

  // --------------------------
  `ovm_object_utils(pmu_mmr_SBI_MSTR_DW1_P_reg)

  // --------------------------
  function new(
                string         name = "",
                `ifdef SLA_RAL_COVERAGE
                sla_ral_coverage_t cov_t = COVERAGE_OFF,
                `endif
                int            bus_num=0,
                int            dev_num=0,
                int            func_num=0,
                int            offset=0,
                int            size=0,
                sla_ral_data_t reset_val=0,
                boolean_t      mon_enabled = SLA_FALSE
             );
      super.new();
      set_space_addr("CFG",undef);
      set_space_addr("MEM",undef);
      set_space_addr("IO",undef);
      set_space_addr("MSG",undef);
      set_space_addr("CREG",undef); 
      set_space_addr("LT_MEM",undef);    
 build();

      `ifdef SLA_RAL_COVERAGE
      `CONFIG_RAL_COVERAGE(regname, COVERAGE_ON, FD_RW_SAMPLE)
      if(_coverage_type != COVERAGE_OFF) begin
        if(_sample_type inside {FD_RO_SAMPLE, RO_SAMPLE})
        `CREATE_OP_SAMPLE_COV(RAL_WRITE)
        else if(_sample_type inside {FD_WO_SAMPLE, WO_SAMPLE})
        `CREATE_OP_SAMPLE_COV(RAL_READ)
        else
        `CREATE_OP_SAMPLE_COV(RAL_NONE)
        `CREATE_DESIRED_COV
        `CREATE_ACTUAL_COV
      end
      `endif

  endfunction

  `ifdef SLA_RAL_COVERAGE
  `CREATE_RAL_OP_COVERGROUP
  covergroup desired_cg @(cov_ev);
     option.per_instance = 1;
     option.name = {sla_ral_file::regfilename,".",regname};
     `RAL_FIELD_CP(DW1_PC_SB, DW1_PC_SB.desired)
     `RAL_FIELD_CP_16(DW1_PC_SB, DW1_PC_SB.desired, 0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)
     `RAL_FIELD_CP_16(DW1_PC_SB, DW1_PC_SB.desired, 16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31)
  endgroup
  covergroup actual_cg @(cov_ev);
     option.per_instance = 1;
     option.name = {sla_ral_file::regfilename,".",regname};
     `RAL_FIELD_CP(DW1_PC_SB, DW1_PC_SB.actual)
     `RAL_FIELD_CP_16(DW1_PC_SB, DW1_PC_SB.actual, 0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)
     `RAL_FIELD_CP_16(DW1_PC_SB, DW1_PC_SB.actual, 16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31)
  endgroup
  `endif

  // --------------------------
  // add constraint based on database legal values
  // --------------------------
  function void build();
    DW1_PC_SB = new("DW1_PC_SB", "RW", 32, 0, {"SBI_MSTR_DW1_P.DW1_PC_SB"});
    DW1_PC_SB.set_powerwell("primary");
    DW1_PC_SB.set_rand_mode(0);
   DW1_PC_SB.set_reset_signame("pmu_rst_b");
    void'(add_field( DW1_PC_SB ));

  endfunction


`DEFINE_NONE_SCOPE_REG_API(pmu_mmr_SBI_MSTR_DW1_P_reg) 
endclass : pmu_mmr_SBI_MSTR_DW1_P_reg

// ================================================

class pmu_mmr_SBI_MSTR_CTL_NP_reg extends sla_ral_reg;

  // --------------------------
  sla_ral_field SEND_NP_SB;
  sla_ral_field NP_ADDR_VLD;
  sla_ral_field NP_DW0_VLD;
  sla_ral_field NP_DW1_VLD;
  sla_ral_field NP_EXHDR_VLD;
  sla_ral_field NP_BCAST_AGG_CPL;
  sla_ral_field NP_FW_SRC_PID_VLD;
  sla_ral_field NP_MSG_DIS;

  // --------------------------
  `ovm_object_utils(pmu_mmr_SBI_MSTR_CTL_NP_reg)

  // --------------------------
  function new(
                string         name = "",
                `ifdef SLA_RAL_COVERAGE
                sla_ral_coverage_t cov_t = COVERAGE_OFF,
                `endif
                int            bus_num=0,
                int            dev_num=0,
                int            func_num=0,
                int            offset=0,
                int            size=0,
                sla_ral_data_t reset_val=0,
                boolean_t      mon_enabled = SLA_FALSE
             );
      super.new();
      set_space_addr("CFG",undef);
      set_space_addr("MEM",undef);
      set_space_addr("IO",undef);
      set_space_addr("MSG",undef);
      set_space_addr("CREG",undef); 
      set_space_addr("LT_MEM",undef);    
 build();

      `ifdef SLA_RAL_COVERAGE
      `CONFIG_RAL_COVERAGE(regname, COVERAGE_ON, FD_RW_SAMPLE)
      if(_coverage_type != COVERAGE_OFF) begin
        if(_sample_type inside {FD_RO_SAMPLE, RO_SAMPLE})
        `CREATE_OP_SAMPLE_COV(RAL_WRITE)
        else if(_sample_type inside {FD_WO_SAMPLE, WO_SAMPLE})
        `CREATE_OP_SAMPLE_COV(RAL_READ)
        else
        `CREATE_OP_SAMPLE_COV(RAL_NONE)
        `CREATE_DESIRED_COV
        `CREATE_ACTUAL_COV
      end
      `endif

  endfunction

  `ifdef SLA_RAL_COVERAGE
  `CREATE_RAL_OP_COVERGROUP
  covergroup desired_cg @(cov_ev);
     option.per_instance = 1;
     option.name = {sla_ral_file::regfilename,".",regname};
     `RAL_FIELD_CP(SEND_NP_SB, SEND_NP_SB.desired)
     `RAL_FIELD_CP_1(SEND_NP_SB, SEND_NP_SB.desired, 0)
     `RAL_FIELD_CP(NP_ADDR_VLD, NP_ADDR_VLD.desired)
     `RAL_FIELD_CP_1(NP_ADDR_VLD, NP_ADDR_VLD.desired, 0)
     `RAL_FIELD_CP(NP_DW0_VLD, NP_DW0_VLD.desired)
     `RAL_FIELD_CP_1(NP_DW0_VLD, NP_DW0_VLD.desired, 0)
     `RAL_FIELD_CP(NP_DW1_VLD, NP_DW1_VLD.desired)
     `RAL_FIELD_CP_1(NP_DW1_VLD, NP_DW1_VLD.desired, 0)
     `RAL_FIELD_CP(NP_EXHDR_VLD, NP_EXHDR_VLD.desired)
     `RAL_FIELD_CP_1(NP_EXHDR_VLD, NP_EXHDR_VLD.desired, 0)
     `RAL_FIELD_CP(NP_BCAST_AGG_CPL, NP_BCAST_AGG_CPL.desired)
     `RAL_FIELD_CP_1(NP_BCAST_AGG_CPL, NP_BCAST_AGG_CPL.desired, 0)
     `RAL_FIELD_CP(NP_FW_SRC_PID_VLD, NP_FW_SRC_PID_VLD.desired)
     `RAL_FIELD_CP_1(NP_FW_SRC_PID_VLD, NP_FW_SRC_PID_VLD.desired, 0)
     `RAL_FIELD_CP(NP_MSG_DIS, NP_MSG_DIS.desired)
     `RAL_FIELD_CP_1(NP_MSG_DIS, NP_MSG_DIS.desired, 0)
  endgroup
  covergroup actual_cg @(cov_ev);
     option.per_instance = 1;
     option.name = {sla_ral_file::regfilename,".",regname};
     `RAL_FIELD_CP(SEND_NP_SB, SEND_NP_SB.actual)
     `RAL_FIELD_CP_1(SEND_NP_SB, SEND_NP_SB.actual, 0)
     `RAL_FIELD_CP(NP_ADDR_VLD, NP_ADDR_VLD.actual)
     `RAL_FIELD_CP_1(NP_ADDR_VLD, NP_ADDR_VLD.actual, 0)
     `RAL_FIELD_CP(NP_DW0_VLD, NP_DW0_VLD.actual)
     `RAL_FIELD_CP_1(NP_DW0_VLD, NP_DW0_VLD.actual, 0)
     `RAL_FIELD_CP(NP_DW1_VLD, NP_DW1_VLD.actual)
     `RAL_FIELD_CP_1(NP_DW1_VLD, NP_DW1_VLD.actual, 0)
     `RAL_FIELD_CP(NP_EXHDR_VLD, NP_EXHDR_VLD.actual)
     `RAL_FIELD_CP_1(NP_EXHDR_VLD, NP_EXHDR_VLD.actual, 0)
     `RAL_FIELD_CP(NP_BCAST_AGG_CPL, NP_BCAST_AGG_CPL.actual)
     `RAL_FIELD_CP_1(NP_BCAST_AGG_CPL, NP_BCAST_AGG_CPL.actual, 0)
     `RAL_FIELD_CP(NP_FW_SRC_PID_VLD, NP_FW_SRC_PID_VLD.actual)
     `RAL_FIELD_CP_1(NP_FW_SRC_PID_VLD, NP_FW_SRC_PID_VLD.actual, 0)
     `RAL_FIELD_CP(NP_MSG_DIS, NP_MSG_DIS.actual)
     `RAL_FIELD_CP_1(NP_MSG_DIS, NP_MSG_DIS.actual, 0)
  endgroup
  `endif

  // --------------------------
  // add constraint based on database legal values
  // --------------------------
  function void build();
    SEND_NP_SB = new("SEND_NP_SB", "RW/1S/V", 1, 0, {"SBI_MSTR_CTL_NP.SEND_NP_SB"});
    SEND_NP_SB.set_powerwell("primary");
    SEND_NP_SB.set_rand_mode(0);
   SEND_NP_SB.set_reset_signame("pmu_rst_b");
    void'(add_field( SEND_NP_SB ));

    NP_ADDR_VLD = new("NP_ADDR_VLD", "RW", 1, 1, {"SBI_MSTR_CTL_NP.NP_ADDR_VLD"});
    NP_ADDR_VLD.set_powerwell("primary");
    NP_ADDR_VLD.set_rand_mode(0);
   NP_ADDR_VLD.set_reset_signame("pmu_rst_b");
    void'(add_field( NP_ADDR_VLD ));

    NP_DW0_VLD = new("NP_DW0_VLD", "RW", 1, 2, {"SBI_MSTR_CTL_NP.NP_DW0_VLD"});
    NP_DW0_VLD.set_powerwell("primary");
    NP_DW0_VLD.set_rand_mode(0);
   NP_DW0_VLD.set_reset_signame("pmu_rst_b");
    void'(add_field( NP_DW0_VLD ));

    NP_DW1_VLD = new("NP_DW1_VLD", "RW", 1, 3, {"SBI_MSTR_CTL_NP.NP_DW1_VLD"});
    NP_DW1_VLD.set_powerwell("primary");
    NP_DW1_VLD.set_rand_mode(0);
   NP_DW1_VLD.set_reset_signame("pmu_rst_b");
    void'(add_field( NP_DW1_VLD ));

    NP_EXHDR_VLD = new("NP_EXHDR_VLD", "RW", 1, 4, {"SBI_MSTR_CTL_NP.NP_EXHDR_VLD"});
    NP_EXHDR_VLD.set_powerwell("primary");
    NP_EXHDR_VLD.set_rand_mode(0);
   NP_EXHDR_VLD.set_reset_signame("pmu_rst_b");
    void'(add_field( NP_EXHDR_VLD ));

    NP_BCAST_AGG_CPL = new("NP_BCAST_AGG_CPL", "RW", 1, 5, {"SBI_MSTR_CTL_NP.NP_BCAST_AGG_CPL"});
    NP_BCAST_AGG_CPL.set_powerwell("primary");
    NP_BCAST_AGG_CPL.set_rand_mode(0);
   NP_BCAST_AGG_CPL.set_reset_signame("pmu_rst_b");
    void'(add_field( NP_BCAST_AGG_CPL ));

    NP_FW_SRC_PID_VLD = new("NP_FW_SRC_PID_VLD", "RW", 1, 6, {"SBI_MSTR_CTL_NP.NP_FW_SRC_PID_VLD"});
    NP_FW_SRC_PID_VLD.set_powerwell("primary");
    NP_FW_SRC_PID_VLD.set_rand_mode(0);
   NP_FW_SRC_PID_VLD.set_reset_signame("pmu_rst_b");
    void'(add_field( NP_FW_SRC_PID_VLD ));

    NP_MSG_DIS = new("NP_MSG_DIS", "RW", 1, 7, {"SBI_MSTR_CTL_NP.NP_MSG_DIS"});
    NP_MSG_DIS.set_powerwell("primary");
    NP_MSG_DIS.set_rand_mode(0);
   NP_MSG_DIS.set_reset_signame("pmu_rst_b");
    void'(add_field( NP_MSG_DIS ));

  endfunction


`DEFINE_NONE_SCOPE_REG_API(pmu_mmr_SBI_MSTR_CTL_NP_reg) 
endclass : pmu_mmr_SBI_MSTR_CTL_NP_reg

// ================================================

class pmu_mmr_SBI_MSTR_STS_NP_reg extends sla_ral_reg;

  // --------------------------
  sla_ral_field NP_IN_PROG;
  sla_ral_field CPL_MSG_AVAIL;
  sla_ral_field CPL_DVLD_SB;
  sla_ral_field CPL_LEN_ERR_STS;
  sla_ral_field RSVD_7_4;

  // --------------------------
  `ovm_object_utils(pmu_mmr_SBI_MSTR_STS_NP_reg)

  // --------------------------
  function new(
                string         name = "",
                `ifdef SLA_RAL_COVERAGE
                sla_ral_coverage_t cov_t = COVERAGE_OFF,
                `endif
                int            bus_num=0,
                int            dev_num=0,
                int            func_num=0,
                int            offset=0,
                int            size=0,
                sla_ral_data_t reset_val=0,
                boolean_t      mon_enabled = SLA_FALSE
             );
      super.new();
      set_space_addr("CFG",undef);
      set_space_addr("MEM",undef);
      set_space_addr("IO",undef);
      set_space_addr("MSG",undef);
      set_space_addr("CREG",undef); 
      set_space_addr("LT_MEM",undef);    
 build();

      `ifdef SLA_RAL_COVERAGE
      `CONFIG_RAL_COVERAGE(regname, COVERAGE_ON, FD_RW_SAMPLE)
      if(_coverage_type != COVERAGE_OFF) begin
        if(_sample_type inside {FD_RO_SAMPLE, RO_SAMPLE})
        `CREATE_OP_SAMPLE_COV(RAL_WRITE)
        else if(_sample_type inside {FD_WO_SAMPLE, WO_SAMPLE})
        `CREATE_OP_SAMPLE_COV(RAL_READ)
        else
        `CREATE_OP_SAMPLE_COV(RAL_NONE)
        `CREATE_DESIRED_COV
        `CREATE_ACTUAL_COV
      end
      `endif

  endfunction

  `ifdef SLA_RAL_COVERAGE
  `CREATE_RAL_OP_COVERGROUP
  covergroup desired_cg @(cov_ev);
     option.per_instance = 1;
     option.name = {sla_ral_file::regfilename,".",regname};
     `RAL_FIELD_CP(NP_IN_PROG, NP_IN_PROG.desired)
     `RAL_FIELD_CP_1(NP_IN_PROG, NP_IN_PROG.desired, 0)
     `RAL_FIELD_CP(CPL_MSG_AVAIL, CPL_MSG_AVAIL.desired)
     `RAL_FIELD_CP_1(CPL_MSG_AVAIL, CPL_MSG_AVAIL.desired, 0)
     `RAL_FIELD_CP(CPL_DVLD_SB, CPL_DVLD_SB.desired)
     `RAL_FIELD_CP_1(CPL_DVLD_SB, CPL_DVLD_SB.desired, 0)
     `RAL_FIELD_CP(CPL_LEN_ERR_STS, CPL_LEN_ERR_STS.desired)
     `RAL_FIELD_CP_1(CPL_LEN_ERR_STS, CPL_LEN_ERR_STS.desired, 0)
     `RAL_FIELD_CP(RSVD_7_4, RSVD_7_4.desired)
     `RAL_FIELD_CP_4(RSVD_7_4, RSVD_7_4.desired, 0,1,2,3)
  endgroup
  covergroup actual_cg @(cov_ev);
     option.per_instance = 1;
     option.name = {sla_ral_file::regfilename,".",regname};
     `RAL_FIELD_CP(NP_IN_PROG, NP_IN_PROG.actual)
     `RAL_FIELD_CP_1(NP_IN_PROG, NP_IN_PROG.actual, 0)
     `RAL_FIELD_CP(CPL_MSG_AVAIL, CPL_MSG_AVAIL.actual)
     `RAL_FIELD_CP_1(CPL_MSG_AVAIL, CPL_MSG_AVAIL.actual, 0)
     `RAL_FIELD_CP(CPL_DVLD_SB, CPL_DVLD_SB.actual)
     `RAL_FIELD_CP_1(CPL_DVLD_SB, CPL_DVLD_SB.actual, 0)
     `RAL_FIELD_CP(CPL_LEN_ERR_STS, CPL_LEN_ERR_STS.actual)
     `RAL_FIELD_CP_1(CPL_LEN_ERR_STS, CPL_LEN_ERR_STS.actual, 0)
     `RAL_FIELD_CP(RSVD_7_4, RSVD_7_4.actual)
     `RAL_FIELD_CP_4(RSVD_7_4, RSVD_7_4.actual, 0,1,2,3)
  endgroup
  `endif

  // --------------------------
  // add constraint based on database legal values
  // --------------------------
  function void build();
    NP_IN_PROG = new("NP_IN_PROG", "RO/V", 1, 0, {"SBI_MSTR_STS_NP.NP_IN_PROG"});
    NP_IN_PROG.set_powerwell("primary");
    NP_IN_PROG.set_rand_mode(0);
    void'(add_field( NP_IN_PROG ));

    CPL_MSG_AVAIL = new("CPL_MSG_AVAIL", "RW/1C/V", 1, 1, {"SBI_MSTR_STS_NP.CPL_MSG_AVAIL"});
    CPL_MSG_AVAIL.set_powerwell("primary");
    CPL_MSG_AVAIL.set_rand_mode(0);
   CPL_MSG_AVAIL.set_reset_signame("pmu_rst_b");
    void'(add_field( CPL_MSG_AVAIL ));

    CPL_DVLD_SB = new("CPL_DVLD_SB", "RO/V", 1, 2, {"SBI_MSTR_STS_NP.CPL_DVLD_SB"});
    CPL_DVLD_SB.set_powerwell("primary");
    CPL_DVLD_SB.set_rand_mode(0);
    void'(add_field( CPL_DVLD_SB ));

    CPL_LEN_ERR_STS = new("CPL_LEN_ERR_STS", "RW/1C/V", 1, 3, {"SBI_MSTR_STS_NP.CPL_LEN_ERR_STS"});
    CPL_LEN_ERR_STS.set_powerwell("primary");
    CPL_LEN_ERR_STS.set_rand_mode(0);
   CPL_LEN_ERR_STS.set_reset_signame("pmu_rst_b");
    void'(add_field( CPL_LEN_ERR_STS ));

    RSVD_7_4 = new("RSVD_7_4", "RO", 4, 4, {"SBI_MSTR_STS_NP.RSVD_7_4"});
    RSVD_7_4.set_powerwell("primary");
    RSVD_7_4.set_rand_mode(0);
    void'(add_field( RSVD_7_4 ));

  endfunction


`DEFINE_NONE_SCOPE_REG_API(pmu_mmr_SBI_MSTR_STS_NP_reg) 
endclass : pmu_mmr_SBI_MSTR_STS_NP_reg

// ================================================

class pmu_mmr_SBI_MSTR_HDR_NP_reg extends sla_ral_reg;

  // --------------------------
  sla_ral_field DEST_PID_NP_SB;
  sla_ral_field SRC_PID_NP_SB;
  sla_ral_field OPCODE_NP_SB;
  sla_ral_field TAG_NP_SB;
  sla_ral_field NP_MSG_HDR_FLD_1;
  sla_ral_field EH_NP_SB;

  // --------------------------
  `ovm_object_utils(pmu_mmr_SBI_MSTR_HDR_NP_reg)

  // --------------------------
  function new(
                string         name = "",
                `ifdef SLA_RAL_COVERAGE
                sla_ral_coverage_t cov_t = COVERAGE_OFF,
                `endif
                int            bus_num=0,
                int            dev_num=0,
                int            func_num=0,
                int            offset=0,
                int            size=0,
                sla_ral_data_t reset_val=0,
                boolean_t      mon_enabled = SLA_FALSE
             );
      super.new();
      set_space_addr("CFG",undef);
      set_space_addr("MEM",undef);
      set_space_addr("IO",undef);
      set_space_addr("MSG",undef);
      set_space_addr("CREG",undef); 
      set_space_addr("LT_MEM",undef);    
 build();

      `ifdef SLA_RAL_COVERAGE
      `CONFIG_RAL_COVERAGE(regname, COVERAGE_ON, FD_RW_SAMPLE)
      if(_coverage_type != COVERAGE_OFF) begin
        if(_sample_type inside {FD_RO_SAMPLE, RO_SAMPLE})
        `CREATE_OP_SAMPLE_COV(RAL_WRITE)
        else if(_sample_type inside {FD_WO_SAMPLE, WO_SAMPLE})
        `CREATE_OP_SAMPLE_COV(RAL_READ)
        else
        `CREATE_OP_SAMPLE_COV(RAL_NONE)
        `CREATE_DESIRED_COV
        `CREATE_ACTUAL_COV
      end
      `endif

  endfunction

  `ifdef SLA_RAL_COVERAGE
  `CREATE_RAL_OP_COVERGROUP
  covergroup desired_cg @(cov_ev);
     option.per_instance = 1;
     option.name = {sla_ral_file::regfilename,".",regname};
     `RAL_FIELD_CP(DEST_PID_NP_SB, DEST_PID_NP_SB.desired)
     `RAL_FIELD_CP_8(DEST_PID_NP_SB, DEST_PID_NP_SB.desired, 0,1,2,3,4,5,6,7)
     `RAL_FIELD_CP(SRC_PID_NP_SB, SRC_PID_NP_SB.desired)
     `RAL_FIELD_CP_8(SRC_PID_NP_SB, SRC_PID_NP_SB.desired, 0,1,2,3,4,5,6,7)
     `RAL_FIELD_CP(OPCODE_NP_SB, OPCODE_NP_SB.desired)
     `RAL_FIELD_CP_8(OPCODE_NP_SB, OPCODE_NP_SB.desired, 0,1,2,3,4,5,6,7)
     `RAL_FIELD_CP(TAG_NP_SB, TAG_NP_SB.desired)
     `RAL_FIELD_CP_3(TAG_NP_SB, TAG_NP_SB.desired, 0,1,2)
     `RAL_FIELD_CP(NP_MSG_HDR_FLD_1, NP_MSG_HDR_FLD_1.desired)
     `RAL_FIELD_CP_4(NP_MSG_HDR_FLD_1, NP_MSG_HDR_FLD_1.desired, 0,1,2,3)
     `RAL_FIELD_CP(EH_NP_SB, EH_NP_SB.desired)
     `RAL_FIELD_CP_1(EH_NP_SB, EH_NP_SB.desired, 0)
  endgroup
  covergroup actual_cg @(cov_ev);
     option.per_instance = 1;
     option.name = {sla_ral_file::regfilename,".",regname};
     `RAL_FIELD_CP(DEST_PID_NP_SB, DEST_PID_NP_SB.actual)
     `RAL_FIELD_CP_8(DEST_PID_NP_SB, DEST_PID_NP_SB.actual, 0,1,2,3,4,5,6,7)
     `RAL_FIELD_CP(SRC_PID_NP_SB, SRC_PID_NP_SB.actual)
     `RAL_FIELD_CP_8(SRC_PID_NP_SB, SRC_PID_NP_SB.actual, 0,1,2,3,4,5,6,7)
     `RAL_FIELD_CP(OPCODE_NP_SB, OPCODE_NP_SB.actual)
     `RAL_FIELD_CP_8(OPCODE_NP_SB, OPCODE_NP_SB.actual, 0,1,2,3,4,5,6,7)
     `RAL_FIELD_CP(TAG_NP_SB, TAG_NP_SB.actual)
     `RAL_FIELD_CP_3(TAG_NP_SB, TAG_NP_SB.actual, 0,1,2)
     `RAL_FIELD_CP(NP_MSG_HDR_FLD_1, NP_MSG_HDR_FLD_1.actual)
     `RAL_FIELD_CP_4(NP_MSG_HDR_FLD_1, NP_MSG_HDR_FLD_1.actual, 0,1,2,3)
     `RAL_FIELD_CP(EH_NP_SB, EH_NP_SB.actual)
     `RAL_FIELD_CP_1(EH_NP_SB, EH_NP_SB.actual, 0)
  endgroup
  `endif

  // --------------------------
  // add constraint based on database legal values
  // --------------------------
  function void build();
    DEST_PID_NP_SB = new("DEST_PID_NP_SB", "RW", 8, 0, {"SBI_MSTR_HDR_NP.DEST_PID_NP_SB"});
    DEST_PID_NP_SB.set_powerwell("primary");
    DEST_PID_NP_SB.set_rand_mode(0);
   DEST_PID_NP_SB.set_reset_signame("pmu_rst_b");
    void'(add_field( DEST_PID_NP_SB ));

    SRC_PID_NP_SB = new("SRC_PID_NP_SB", "RW", 8, 8, {"SBI_MSTR_HDR_NP.SRC_PID_NP_SB"});
    SRC_PID_NP_SB.set_powerwell("primary");
    SRC_PID_NP_SB.set_rand_mode(0);
   SRC_PID_NP_SB.set_reset_signame("pmu_rst_b");
    void'(add_field( SRC_PID_NP_SB ));

    OPCODE_NP_SB = new("OPCODE_NP_SB", "RW", 8, 16, {"SBI_MSTR_HDR_NP.OPCODE_NP_SB"});
    OPCODE_NP_SB.set_powerwell("primary");
    OPCODE_NP_SB.set_rand_mode(0);
   OPCODE_NP_SB.set_reset_signame("pmu_rst_b");
    void'(add_field( OPCODE_NP_SB ));

    TAG_NP_SB = new("TAG_NP_SB", "RW", 3, 24, {"SBI_MSTR_HDR_NP.TAG_NP_SB"});
    TAG_NP_SB.set_powerwell("primary");
    TAG_NP_SB.set_rand_mode(0);
   TAG_NP_SB.set_reset_signame("pmu_rst_b");
    void'(add_field( TAG_NP_SB ));

    NP_MSG_HDR_FLD_1 = new("NP_MSG_HDR_FLD_1", "RW", 4, 27, {"SBI_MSTR_HDR_NP.NP_MSG_HDR_FLD_1"});
    NP_MSG_HDR_FLD_1.set_powerwell("primary");
    NP_MSG_HDR_FLD_1.set_rand_mode(0);
   NP_MSG_HDR_FLD_1.set_reset_signame("pmu_rst_b");
    void'(add_field( NP_MSG_HDR_FLD_1 ));

    EH_NP_SB = new("EH_NP_SB", "RW", 1, 31, {"SBI_MSTR_HDR_NP.EH_NP_SB"});
    EH_NP_SB.set_powerwell("primary");
    EH_NP_SB.set_rand_mode(0);
   EH_NP_SB.set_reset_signame("pmu_rst_b");
    void'(add_field( EH_NP_SB ));

  endfunction


`DEFINE_NONE_SCOPE_REG_API(pmu_mmr_SBI_MSTR_HDR_NP_reg) 
endclass : pmu_mmr_SBI_MSTR_HDR_NP_reg

// ================================================

class pmu_mmr_SBI_MSTR_EH_NP_reg extends sla_ral_reg;

  // --------------------------
  sla_ral_field EXHDR_NP_SB;

  // --------------------------
  `ovm_object_utils(pmu_mmr_SBI_MSTR_EH_NP_reg)

  // --------------------------
  function new(
                string         name = "",
                `ifdef SLA_RAL_COVERAGE
                sla_ral_coverage_t cov_t = COVERAGE_OFF,
                `endif
                int            bus_num=0,
                int            dev_num=0,
                int            func_num=0,
                int            offset=0,
                int            size=0,
                sla_ral_data_t reset_val=0,
                boolean_t      mon_enabled = SLA_FALSE
             );
      super.new();
      set_space_addr("CFG",undef);
      set_space_addr("MEM",undef);
      set_space_addr("IO",undef);
      set_space_addr("MSG",undef);
      set_space_addr("CREG",undef); 
      set_space_addr("LT_MEM",undef);    
 build();

      `ifdef SLA_RAL_COVERAGE
      `CONFIG_RAL_COVERAGE(regname, COVERAGE_ON, FD_RW_SAMPLE)
      if(_coverage_type != COVERAGE_OFF) begin
        if(_sample_type inside {FD_RO_SAMPLE, RO_SAMPLE})
        `CREATE_OP_SAMPLE_COV(RAL_WRITE)
        else if(_sample_type inside {FD_WO_SAMPLE, WO_SAMPLE})
        `CREATE_OP_SAMPLE_COV(RAL_READ)
        else
        `CREATE_OP_SAMPLE_COV(RAL_NONE)
        `CREATE_DESIRED_COV
        `CREATE_ACTUAL_COV
      end
      `endif

  endfunction

  `ifdef SLA_RAL_COVERAGE
  `CREATE_RAL_OP_COVERGROUP
  covergroup desired_cg @(cov_ev);
     option.per_instance = 1;
     option.name = {sla_ral_file::regfilename,".",regname};
     `RAL_FIELD_CP(EXHDR_NP_SB, EXHDR_NP_SB.desired)
     `RAL_FIELD_CP_16(EXHDR_NP_SB, EXHDR_NP_SB.desired, 0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)
     `RAL_FIELD_CP_16(EXHDR_NP_SB, EXHDR_NP_SB.desired, 16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31)
  endgroup
  covergroup actual_cg @(cov_ev);
     option.per_instance = 1;
     option.name = {sla_ral_file::regfilename,".",regname};
     `RAL_FIELD_CP(EXHDR_NP_SB, EXHDR_NP_SB.actual)
     `RAL_FIELD_CP_16(EXHDR_NP_SB, EXHDR_NP_SB.actual, 0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)
     `RAL_FIELD_CP_16(EXHDR_NP_SB, EXHDR_NP_SB.actual, 16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31)
  endgroup
  `endif

  // --------------------------
  // add constraint based on database legal values
  // --------------------------
  function void build();
    EXHDR_NP_SB = new("EXHDR_NP_SB", "RW", 32, 0, {"SBI_MSTR_EH_NP.EXHDR_NP_SB"});
    EXHDR_NP_SB.set_powerwell("primary");
    EXHDR_NP_SB.set_rand_mode(0);
   EXHDR_NP_SB.set_reset_signame("pmu_rst_b");
    void'(add_field( EXHDR_NP_SB ));

  endfunction


`DEFINE_NONE_SCOPE_REG_API(pmu_mmr_SBI_MSTR_EH_NP_reg) 
endclass : pmu_mmr_SBI_MSTR_EH_NP_reg

// ================================================

class pmu_mmr_SBI_MSTR_ADDR_NP_reg extends sla_ral_reg;

  // --------------------------
  sla_ral_field ADDR_NP_SB;

  // --------------------------
  `ovm_object_utils(pmu_mmr_SBI_MSTR_ADDR_NP_reg)

  // --------------------------
  function new(
                string         name = "",
                `ifdef SLA_RAL_COVERAGE
                sla_ral_coverage_t cov_t = COVERAGE_OFF,
                `endif
                int            bus_num=0,
                int            dev_num=0,
                int            func_num=0,
                int            offset=0,
                int            size=0,
                sla_ral_data_t reset_val=0,
                boolean_t      mon_enabled = SLA_FALSE
             );
      super.new();
      set_space_addr("CFG",undef);
      set_space_addr("MEM",undef);
      set_space_addr("IO",undef);
      set_space_addr("MSG",undef);
      set_space_addr("CREG",undef); 
      set_space_addr("LT_MEM",undef);    
 build();

      `ifdef SLA_RAL_COVERAGE
      `CONFIG_RAL_COVERAGE(regname, COVERAGE_ON, FD_RW_SAMPLE)
      if(_coverage_type != COVERAGE_OFF) begin
        if(_sample_type inside {FD_RO_SAMPLE, RO_SAMPLE})
        `CREATE_OP_SAMPLE_COV(RAL_WRITE)
        else if(_sample_type inside {FD_WO_SAMPLE, WO_SAMPLE})
        `CREATE_OP_SAMPLE_COV(RAL_READ)
        else
        `CREATE_OP_SAMPLE_COV(RAL_NONE)
        `CREATE_DESIRED_COV
        `CREATE_ACTUAL_COV
      end
      `endif

  endfunction

  `ifdef SLA_RAL_COVERAGE
  `CREATE_RAL_OP_COVERGROUP
  covergroup desired_cg @(cov_ev);
     option.per_instance = 1;
     option.name = {sla_ral_file::regfilename,".",regname};
     `RAL_FIELD_CP(ADDR_NP_SB, ADDR_NP_SB.desired)
     `RAL_FIELD_CP_16(ADDR_NP_SB, ADDR_NP_SB.desired, 0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)
     `RAL_FIELD_CP_16(ADDR_NP_SB, ADDR_NP_SB.desired, 16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31)
  endgroup
  covergroup actual_cg @(cov_ev);
     option.per_instance = 1;
     option.name = {sla_ral_file::regfilename,".",regname};
     `RAL_FIELD_CP(ADDR_NP_SB, ADDR_NP_SB.actual)
     `RAL_FIELD_CP_16(ADDR_NP_SB, ADDR_NP_SB.actual, 0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)
     `RAL_FIELD_CP_16(ADDR_NP_SB, ADDR_NP_SB.actual, 16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31)
  endgroup
  `endif

  // --------------------------
  // add constraint based on database legal values
  // --------------------------
  function void build();
    ADDR_NP_SB = new("ADDR_NP_SB", "RW", 32, 0, {"SBI_MSTR_ADDR_NP.ADDR_NP_SB"});
    ADDR_NP_SB.set_powerwell("primary");
    ADDR_NP_SB.set_rand_mode(0);
   ADDR_NP_SB.set_reset_signame("pmu_rst_b");
    void'(add_field( ADDR_NP_SB ));

  endfunction


`DEFINE_NONE_SCOPE_REG_API(pmu_mmr_SBI_MSTR_ADDR_NP_reg) 
endclass : pmu_mmr_SBI_MSTR_ADDR_NP_reg

// ================================================

class pmu_mmr_SBI_MSTR_DW0_NP_reg extends sla_ral_reg;

  // --------------------------
  sla_ral_field DW0_NP_SB;

  // --------------------------
  `ovm_object_utils(pmu_mmr_SBI_MSTR_DW0_NP_reg)

  // --------------------------
  function new(
                string         name = "",
                `ifdef SLA_RAL_COVERAGE
                sla_ral_coverage_t cov_t = COVERAGE_OFF,
                `endif
                int            bus_num=0,
                int            dev_num=0,
                int            func_num=0,
                int            offset=0,
                int            size=0,
                sla_ral_data_t reset_val=0,
                boolean_t      mon_enabled = SLA_FALSE
             );
      super.new();
      set_space_addr("CFG",undef);
      set_space_addr("MEM",undef);
      set_space_addr("IO",undef);
      set_space_addr("MSG",undef);
      set_space_addr("CREG",undef); 
      set_space_addr("LT_MEM",undef);    
 build();

      `ifdef SLA_RAL_COVERAGE
      `CONFIG_RAL_COVERAGE(regname, COVERAGE_ON, FD_RW_SAMPLE)
      if(_coverage_type != COVERAGE_OFF) begin
        if(_sample_type inside {FD_RO_SAMPLE, RO_SAMPLE})
        `CREATE_OP_SAMPLE_COV(RAL_WRITE)
        else if(_sample_type inside {FD_WO_SAMPLE, WO_SAMPLE})
        `CREATE_OP_SAMPLE_COV(RAL_READ)
        else
        `CREATE_OP_SAMPLE_COV(RAL_NONE)
        `CREATE_DESIRED_COV
        `CREATE_ACTUAL_COV
      end
      `endif

  endfunction

  `ifdef SLA_RAL_COVERAGE
  `CREATE_RAL_OP_COVERGROUP
  covergroup desired_cg @(cov_ev);
     option.per_instance = 1;
     option.name = {sla_ral_file::regfilename,".",regname};
     `RAL_FIELD_CP(DW0_NP_SB, DW0_NP_SB.desired)
     `RAL_FIELD_CP_16(DW0_NP_SB, DW0_NP_SB.desired, 0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)
     `RAL_FIELD_CP_16(DW0_NP_SB, DW0_NP_SB.desired, 16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31)
  endgroup
  covergroup actual_cg @(cov_ev);
     option.per_instance = 1;
     option.name = {sla_ral_file::regfilename,".",regname};
     `RAL_FIELD_CP(DW0_NP_SB, DW0_NP_SB.actual)
     `RAL_FIELD_CP_16(DW0_NP_SB, DW0_NP_SB.actual, 0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)
     `RAL_FIELD_CP_16(DW0_NP_SB, DW0_NP_SB.actual, 16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31)
  endgroup
  `endif

  // --------------------------
  // add constraint based on database legal values
  // --------------------------
  function void build();
    DW0_NP_SB = new("DW0_NP_SB", "RW", 32, 0, {"SBI_MSTR_DW0_NP.DW0_NP_SB"});
    DW0_NP_SB.set_powerwell("primary");
    DW0_NP_SB.set_rand_mode(0);
   DW0_NP_SB.set_reset_signame("pmu_rst_b");
    void'(add_field( DW0_NP_SB ));

  endfunction


`DEFINE_NONE_SCOPE_REG_API(pmu_mmr_SBI_MSTR_DW0_NP_reg) 
endclass : pmu_mmr_SBI_MSTR_DW0_NP_reg

// ================================================

class pmu_mmr_SBI_MSTR_DW1_NP_reg extends sla_ral_reg;

  // --------------------------
  sla_ral_field DW1_NP_SB;

  // --------------------------
  `ovm_object_utils(pmu_mmr_SBI_MSTR_DW1_NP_reg)

  // --------------------------
  function new(
                string         name = "",
                `ifdef SLA_RAL_COVERAGE
                sla_ral_coverage_t cov_t = COVERAGE_OFF,
                `endif
                int            bus_num=0,
                int            dev_num=0,
                int            func_num=0,
                int            offset=0,
                int            size=0,
                sla_ral_data_t reset_val=0,
                boolean_t      mon_enabled = SLA_FALSE
             );
      super.new();
      set_space_addr("CFG",undef);
      set_space_addr("MEM",undef);
      set_space_addr("IO",undef);
      set_space_addr("MSG",undef);
      set_space_addr("CREG",undef); 
      set_space_addr("LT_MEM",undef);    
 build();

      `ifdef SLA_RAL_COVERAGE
      `CONFIG_RAL_COVERAGE(regname, COVERAGE_ON, FD_RW_SAMPLE)
      if(_coverage_type != COVERAGE_OFF) begin
        if(_sample_type inside {FD_RO_SAMPLE, RO_SAMPLE})
        `CREATE_OP_SAMPLE_COV(RAL_WRITE)
        else if(_sample_type inside {FD_WO_SAMPLE, WO_SAMPLE})
        `CREATE_OP_SAMPLE_COV(RAL_READ)
        else
        `CREATE_OP_SAMPLE_COV(RAL_NONE)
        `CREATE_DESIRED_COV
        `CREATE_ACTUAL_COV
      end
      `endif

  endfunction

  `ifdef SLA_RAL_COVERAGE
  `CREATE_RAL_OP_COVERGROUP
  covergroup desired_cg @(cov_ev);
     option.per_instance = 1;
     option.name = {sla_ral_file::regfilename,".",regname};
     `RAL_FIELD_CP(DW1_NP_SB, DW1_NP_SB.desired)
     `RAL_FIELD_CP_16(DW1_NP_SB, DW1_NP_SB.desired, 0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)
     `RAL_FIELD_CP_16(DW1_NP_SB, DW1_NP_SB.desired, 16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31)
  endgroup
  covergroup actual_cg @(cov_ev);
     option.per_instance = 1;
     option.name = {sla_ral_file::regfilename,".",regname};
     `RAL_FIELD_CP(DW1_NP_SB, DW1_NP_SB.actual)
     `RAL_FIELD_CP_16(DW1_NP_SB, DW1_NP_SB.actual, 0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)
     `RAL_FIELD_CP_16(DW1_NP_SB, DW1_NP_SB.actual, 16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31)
  endgroup
  `endif

  // --------------------------
  // add constraint based on database legal values
  // --------------------------
  function void build();
    DW1_NP_SB = new("DW1_NP_SB", "RW", 32, 0, {"SBI_MSTR_DW1_NP.DW1_NP_SB"});
    DW1_NP_SB.set_powerwell("primary");
    DW1_NP_SB.set_rand_mode(0);
   DW1_NP_SB.set_reset_signame("pmu_rst_b");
    void'(add_field( DW1_NP_SB ));

  endfunction


`DEFINE_NONE_SCOPE_REG_API(pmu_mmr_SBI_MSTR_DW1_NP_reg) 
endclass : pmu_mmr_SBI_MSTR_DW1_NP_reg

// ================================================

class pmu_mmr_SBI_MSTR_CPL_HDR_reg extends sla_ral_reg;

  // --------------------------
  sla_ral_field CPL_HDR_SB;

  // --------------------------
  `ovm_object_utils(pmu_mmr_SBI_MSTR_CPL_HDR_reg)

  // --------------------------
  function new(
                string         name = "",
                `ifdef SLA_RAL_COVERAGE
                sla_ral_coverage_t cov_t = COVERAGE_OFF,
                `endif
                int            bus_num=0,
                int            dev_num=0,
                int            func_num=0,
                int            offset=0,
                int            size=0,
                sla_ral_data_t reset_val=0,
                boolean_t      mon_enabled = SLA_FALSE
             );
      super.new();
      set_space_addr("CFG",undef);
      set_space_addr("MEM",undef);
      set_space_addr("IO",undef);
      set_space_addr("MSG",undef);
      set_space_addr("CREG",undef); 
      set_space_addr("LT_MEM",undef);    
 build();

      `ifdef SLA_RAL_COVERAGE
      `CONFIG_RAL_COVERAGE(regname, COVERAGE_ON, FD_RW_SAMPLE)
      if(_coverage_type != COVERAGE_OFF) begin
        if(_sample_type inside {FD_RO_SAMPLE, RO_SAMPLE})
        `CREATE_OP_SAMPLE_COV(RAL_WRITE)
        else if(_sample_type inside {FD_WO_SAMPLE, WO_SAMPLE})
        `CREATE_OP_SAMPLE_COV(RAL_READ)
        else
        `CREATE_OP_SAMPLE_COV(RAL_NONE)
        `CREATE_DESIRED_COV
        `CREATE_ACTUAL_COV
      end
      `endif

  endfunction

  `ifdef SLA_RAL_COVERAGE
  `CREATE_RAL_OP_COVERGROUP
  covergroup desired_cg @(cov_ev);
     option.per_instance = 1;
     option.name = {sla_ral_file::regfilename,".",regname};
     `RAL_FIELD_CP(CPL_HDR_SB, CPL_HDR_SB.desired)
     `RAL_FIELD_CP_16(CPL_HDR_SB, CPL_HDR_SB.desired, 0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)
     `RAL_FIELD_CP_16(CPL_HDR_SB, CPL_HDR_SB.desired, 16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31)
  endgroup
  covergroup actual_cg @(cov_ev);
     option.per_instance = 1;
     option.name = {sla_ral_file::regfilename,".",regname};
     `RAL_FIELD_CP(CPL_HDR_SB, CPL_HDR_SB.actual)
     `RAL_FIELD_CP_16(CPL_HDR_SB, CPL_HDR_SB.actual, 0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)
     `RAL_FIELD_CP_16(CPL_HDR_SB, CPL_HDR_SB.actual, 16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31)
  endgroup
  `endif

  // --------------------------
  // add constraint based on database legal values
  // --------------------------
  function void build();
    CPL_HDR_SB = new("CPL_HDR_SB", "RO/V", 32, 0, {"SBI_MSTR_CPL_HDR.CPL_HDR_SB"});
    CPL_HDR_SB.set_powerwell("primary");
    CPL_HDR_SB.set_rand_mode(0);
    void'(add_field( CPL_HDR_SB ));

  endfunction


`DEFINE_NONE_SCOPE_REG_API(pmu_mmr_SBI_MSTR_CPL_HDR_reg) 
endclass : pmu_mmr_SBI_MSTR_CPL_HDR_reg

// ================================================

class pmu_mmr_SBI_MSTR_CPL_EH_reg extends sla_ral_reg;

  // --------------------------
  sla_ral_field CPL_EH_SB;

  // --------------------------
  `ovm_object_utils(pmu_mmr_SBI_MSTR_CPL_EH_reg)

  // --------------------------
  function new(
                string         name = "",
                `ifdef SLA_RAL_COVERAGE
                sla_ral_coverage_t cov_t = COVERAGE_OFF,
                `endif
                int            bus_num=0,
                int            dev_num=0,
                int            func_num=0,
                int            offset=0,
                int            size=0,
                sla_ral_data_t reset_val=0,
                boolean_t      mon_enabled = SLA_FALSE
             );
      super.new();
      set_space_addr("CFG",undef);
      set_space_addr("MEM",undef);
      set_space_addr("IO",undef);
      set_space_addr("MSG",undef);
      set_space_addr("CREG",undef); 
      set_space_addr("LT_MEM",undef);    
 build();

      `ifdef SLA_RAL_COVERAGE
      `CONFIG_RAL_COVERAGE(regname, COVERAGE_ON, FD_RW_SAMPLE)
      if(_coverage_type != COVERAGE_OFF) begin
        if(_sample_type inside {FD_RO_SAMPLE, RO_SAMPLE})
        `CREATE_OP_SAMPLE_COV(RAL_WRITE)
        else if(_sample_type inside {FD_WO_SAMPLE, WO_SAMPLE})
        `CREATE_OP_SAMPLE_COV(RAL_READ)
        else
        `CREATE_OP_SAMPLE_COV(RAL_NONE)
        `CREATE_DESIRED_COV
        `CREATE_ACTUAL_COV
      end
      `endif

  endfunction

  `ifdef SLA_RAL_COVERAGE
  `CREATE_RAL_OP_COVERGROUP
  covergroup desired_cg @(cov_ev);
     option.per_instance = 1;
     option.name = {sla_ral_file::regfilename,".",regname};
     `RAL_FIELD_CP(CPL_EH_SB, CPL_EH_SB.desired)
     `RAL_FIELD_CP_16(CPL_EH_SB, CPL_EH_SB.desired, 0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)
     `RAL_FIELD_CP_16(CPL_EH_SB, CPL_EH_SB.desired, 16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31)
  endgroup
  covergroup actual_cg @(cov_ev);
     option.per_instance = 1;
     option.name = {sla_ral_file::regfilename,".",regname};
     `RAL_FIELD_CP(CPL_EH_SB, CPL_EH_SB.actual)
     `RAL_FIELD_CP_16(CPL_EH_SB, CPL_EH_SB.actual, 0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)
     `RAL_FIELD_CP_16(CPL_EH_SB, CPL_EH_SB.actual, 16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31)
  endgroup
  `endif

  // --------------------------
  // add constraint based on database legal values
  // --------------------------
  function void build();
    CPL_EH_SB = new("CPL_EH_SB", "RO/V", 32, 0, {"SBI_MSTR_CPL_EH.CPL_EH_SB"});
    CPL_EH_SB.set_powerwell("primary");
    CPL_EH_SB.set_rand_mode(0);
    void'(add_field( CPL_EH_SB ));

  endfunction


`DEFINE_NONE_SCOPE_REG_API(pmu_mmr_SBI_MSTR_CPL_EH_reg) 
endclass : pmu_mmr_SBI_MSTR_CPL_EH_reg

// ================================================

class pmu_mmr_SBI_MSTR_CPL_DATA_reg extends sla_ral_reg;

  // --------------------------
  sla_ral_field CPL_DATA_SB;

  // --------------------------
  `ovm_object_utils(pmu_mmr_SBI_MSTR_CPL_DATA_reg)

  // --------------------------
  function new(
                string         name = "",
                `ifdef SLA_RAL_COVERAGE
                sla_ral_coverage_t cov_t = COVERAGE_OFF,
                `endif
                int            bus_num=0,
                int            dev_num=0,
                int            func_num=0,
                int            offset=0,
                int            size=0,
                sla_ral_data_t reset_val=0,
                boolean_t      mon_enabled = SLA_FALSE
             );
      super.new();
      set_space_addr("CFG",undef);
      set_space_addr("MEM",undef);
      set_space_addr("IO",undef);
      set_space_addr("MSG",undef);
      set_space_addr("CREG",undef); 
      set_space_addr("LT_MEM",undef);    
 build();

      `ifdef SLA_RAL_COVERAGE
      `CONFIG_RAL_COVERAGE(regname, COVERAGE_ON, FD_RW_SAMPLE)
      if(_coverage_type != COVERAGE_OFF) begin
        if(_sample_type inside {FD_RO_SAMPLE, RO_SAMPLE})
        `CREATE_OP_SAMPLE_COV(RAL_WRITE)
        else if(_sample_type inside {FD_WO_SAMPLE, WO_SAMPLE})
        `CREATE_OP_SAMPLE_COV(RAL_READ)
        else
        `CREATE_OP_SAMPLE_COV(RAL_NONE)
        `CREATE_DESIRED_COV
        `CREATE_ACTUAL_COV
      end
      `endif

  endfunction

  `ifdef SLA_RAL_COVERAGE
  `CREATE_RAL_OP_COVERGROUP
  covergroup desired_cg @(cov_ev);
     option.per_instance = 1;
     option.name = {sla_ral_file::regfilename,".",regname};
     `RAL_FIELD_CP(CPL_DATA_SB, CPL_DATA_SB.desired)
     `RAL_FIELD_CP_16(CPL_DATA_SB, CPL_DATA_SB.desired, 0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)
     `RAL_FIELD_CP_16(CPL_DATA_SB, CPL_DATA_SB.desired, 16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31)
  endgroup
  covergroup actual_cg @(cov_ev);
     option.per_instance = 1;
     option.name = {sla_ral_file::regfilename,".",regname};
     `RAL_FIELD_CP(CPL_DATA_SB, CPL_DATA_SB.actual)
     `RAL_FIELD_CP_16(CPL_DATA_SB, CPL_DATA_SB.actual, 0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)
     `RAL_FIELD_CP_16(CPL_DATA_SB, CPL_DATA_SB.actual, 16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31)
  endgroup
  `endif

  // --------------------------
  // add constraint based on database legal values
  // --------------------------
  function void build();
    CPL_DATA_SB = new("CPL_DATA_SB", "RO/V", 32, 0, {"SBI_MSTR_CPL_DATA.CPL_DATA_SB"});
    CPL_DATA_SB.set_powerwell("primary");
    CPL_DATA_SB.set_rand_mode(0);
    void'(add_field( CPL_DATA_SB ));

  endfunction


`DEFINE_NONE_SCOPE_REG_API(pmu_mmr_SBI_MSTR_CPL_DATA_reg) 
endclass : pmu_mmr_SBI_MSTR_CPL_DATA_reg

// ================================================

class pmu_mmr_SBI_IMSG_FIFO_STS_reg extends sla_ral_reg;

  // --------------------------
  sla_ral_field IMSG_FIFO_MSG_AVAIL;
  sla_ral_field IMSG_FIFO_MSG_LEN;
  sla_ral_field IMSG_FIFO_MSG_TYP;
  sla_ral_field IMSG_FIFO_NUM_AVAIL;

  // --------------------------
  `ovm_object_utils(pmu_mmr_SBI_IMSG_FIFO_STS_reg)

  // --------------------------
  function new(
                string         name = "",
                `ifdef SLA_RAL_COVERAGE
                sla_ral_coverage_t cov_t = COVERAGE_OFF,
                `endif
                int            bus_num=0,
                int            dev_num=0,
                int            func_num=0,
                int            offset=0,
                int            size=0,
                sla_ral_data_t reset_val=0,
                boolean_t      mon_enabled = SLA_FALSE
             );
      super.new();
      set_space_addr("CFG",undef);
      set_space_addr("MEM",undef);
      set_space_addr("IO",undef);
      set_space_addr("MSG",undef);
      set_space_addr("CREG",undef); 
      set_space_addr("LT_MEM",undef);    
 build();

      `ifdef SLA_RAL_COVERAGE
      `CONFIG_RAL_COVERAGE(regname, COVERAGE_ON, FD_RW_SAMPLE)
      if(_coverage_type != COVERAGE_OFF) begin
        if(_sample_type inside {FD_RO_SAMPLE, RO_SAMPLE})
        `CREATE_OP_SAMPLE_COV(RAL_WRITE)
        else if(_sample_type inside {FD_WO_SAMPLE, WO_SAMPLE})
        `CREATE_OP_SAMPLE_COV(RAL_READ)
        else
        `CREATE_OP_SAMPLE_COV(RAL_NONE)
        `CREATE_DESIRED_COV
        `CREATE_ACTUAL_COV
      end
      `endif

  endfunction

  `ifdef SLA_RAL_COVERAGE
  `CREATE_RAL_OP_COVERGROUP
  covergroup desired_cg @(cov_ev);
     option.per_instance = 1;
     option.name = {sla_ral_file::regfilename,".",regname};
     `RAL_FIELD_CP(IMSG_FIFO_MSG_AVAIL, IMSG_FIFO_MSG_AVAIL.desired)
     `RAL_FIELD_CP_1(IMSG_FIFO_MSG_AVAIL, IMSG_FIFO_MSG_AVAIL.desired, 0)
     `RAL_FIELD_CP(IMSG_FIFO_MSG_LEN, IMSG_FIFO_MSG_LEN.desired)
     `RAL_FIELD_CP_3(IMSG_FIFO_MSG_LEN, IMSG_FIFO_MSG_LEN.desired, 0,1,2)
     `RAL_FIELD_CP(IMSG_FIFO_MSG_TYP, IMSG_FIFO_MSG_TYP.desired)
     `RAL_FIELD_CP_1(IMSG_FIFO_MSG_TYP, IMSG_FIFO_MSG_TYP.desired, 0)
     `RAL_FIELD_CP(IMSG_FIFO_NUM_AVAIL, IMSG_FIFO_NUM_AVAIL.desired)
     `RAL_FIELD_CP_3(IMSG_FIFO_NUM_AVAIL, IMSG_FIFO_NUM_AVAIL.desired, 0,1,2)
  endgroup
  covergroup actual_cg @(cov_ev);
     option.per_instance = 1;
     option.name = {sla_ral_file::regfilename,".",regname};
     `RAL_FIELD_CP(IMSG_FIFO_MSG_AVAIL, IMSG_FIFO_MSG_AVAIL.actual)
     `RAL_FIELD_CP_1(IMSG_FIFO_MSG_AVAIL, IMSG_FIFO_MSG_AVAIL.actual, 0)
     `RAL_FIELD_CP(IMSG_FIFO_MSG_LEN, IMSG_FIFO_MSG_LEN.actual)
     `RAL_FIELD_CP_3(IMSG_FIFO_MSG_LEN, IMSG_FIFO_MSG_LEN.actual, 0,1,2)
     `RAL_FIELD_CP(IMSG_FIFO_MSG_TYP, IMSG_FIFO_MSG_TYP.actual)
     `RAL_FIELD_CP_1(IMSG_FIFO_MSG_TYP, IMSG_FIFO_MSG_TYP.actual, 0)
     `RAL_FIELD_CP(IMSG_FIFO_NUM_AVAIL, IMSG_FIFO_NUM_AVAIL.actual)
     `RAL_FIELD_CP_3(IMSG_FIFO_NUM_AVAIL, IMSG_FIFO_NUM_AVAIL.actual, 0,1,2)
  endgroup
  `endif

  // --------------------------
  // add constraint based on database legal values
  // --------------------------
  function void build();
    IMSG_FIFO_MSG_AVAIL = new("IMSG_FIFO_MSG_AVAIL", "RO/V", 1, 0, {"SBI_IMSG_FIFO_STS.IMSG_FIFO_MSG_AVAIL"});
    IMSG_FIFO_MSG_AVAIL.set_powerwell("primary");
    IMSG_FIFO_MSG_AVAIL.set_rand_mode(0);
    void'(add_field( IMSG_FIFO_MSG_AVAIL ));

    IMSG_FIFO_MSG_LEN = new("IMSG_FIFO_MSG_LEN", "RO/V", 3, 1, {"SBI_IMSG_FIFO_STS.IMSG_FIFO_MSG_LEN"});
    IMSG_FIFO_MSG_LEN.set_powerwell("primary");
    IMSG_FIFO_MSG_LEN.set_rand_mode(0);
    void'(add_field( IMSG_FIFO_MSG_LEN ));

    IMSG_FIFO_MSG_TYP = new("IMSG_FIFO_MSG_TYP", "RO/V", 1, 4, {"SBI_IMSG_FIFO_STS.IMSG_FIFO_MSG_TYP"});
    IMSG_FIFO_MSG_TYP.set_powerwell("primary");
    IMSG_FIFO_MSG_TYP.set_rand_mode(0);
    void'(add_field( IMSG_FIFO_MSG_TYP ));

    IMSG_FIFO_NUM_AVAIL = new("IMSG_FIFO_NUM_AVAIL", "RO/V", 3, 5, {"SBI_IMSG_FIFO_STS.IMSG_FIFO_NUM_AVAIL"});
    IMSG_FIFO_NUM_AVAIL.set_powerwell("primary");
    IMSG_FIFO_NUM_AVAIL.set_rand_mode(0);
    void'(add_field( IMSG_FIFO_NUM_AVAIL ));

  endfunction


`DEFINE_NONE_SCOPE_REG_API(pmu_mmr_SBI_IMSG_FIFO_STS_reg) 
endclass : pmu_mmr_SBI_IMSG_FIFO_STS_reg

// ================================================

class pmu_mmr_SBI_IMSG_FIFO_CTL_reg extends sla_ral_reg;

  // --------------------------
  sla_ral_field IMSG_FIFO_MSG_GET;
  sla_ral_field RSVD_6_1;
  sla_ral_field IMSG_FIFO_MSG_DROP;

  // --------------------------
  `ovm_object_utils(pmu_mmr_SBI_IMSG_FIFO_CTL_reg)

  // --------------------------
  function new(
                string         name = "",
                `ifdef SLA_RAL_COVERAGE
                sla_ral_coverage_t cov_t = COVERAGE_OFF,
                `endif
                int            bus_num=0,
                int            dev_num=0,
                int            func_num=0,
                int            offset=0,
                int            size=0,
                sla_ral_data_t reset_val=0,
                boolean_t      mon_enabled = SLA_FALSE
             );
      super.new();
      set_space_addr("CFG",undef);
      set_space_addr("MEM",undef);
      set_space_addr("IO",undef);
      set_space_addr("MSG",undef);
      set_space_addr("CREG",undef); 
      set_space_addr("LT_MEM",undef);    
 build();

      `ifdef SLA_RAL_COVERAGE
      `CONFIG_RAL_COVERAGE(regname, COVERAGE_ON, FD_RW_SAMPLE)
      if(_coverage_type != COVERAGE_OFF) begin
        if(_sample_type inside {FD_RO_SAMPLE, RO_SAMPLE})
        `CREATE_OP_SAMPLE_COV(RAL_WRITE)
        else if(_sample_type inside {FD_WO_SAMPLE, WO_SAMPLE})
        `CREATE_OP_SAMPLE_COV(RAL_READ)
        else
        `CREATE_OP_SAMPLE_COV(RAL_NONE)
        `CREATE_DESIRED_COV
        `CREATE_ACTUAL_COV
      end
      `endif

  endfunction

  `ifdef SLA_RAL_COVERAGE
  `CREATE_RAL_OP_COVERGROUP
  covergroup desired_cg @(cov_ev);
     option.per_instance = 1;
     option.name = {sla_ral_file::regfilename,".",regname};
     `RAL_FIELD_CP(IMSG_FIFO_MSG_GET, IMSG_FIFO_MSG_GET.desired)
     `RAL_FIELD_CP_1(IMSG_FIFO_MSG_GET, IMSG_FIFO_MSG_GET.desired, 0)
     `RAL_FIELD_CP(RSVD_6_1, RSVD_6_1.desired)
     `RAL_FIELD_CP_6(RSVD_6_1, RSVD_6_1.desired, 0,1,2,3,4,5)
     `RAL_FIELD_CP(IMSG_FIFO_MSG_DROP, IMSG_FIFO_MSG_DROP.desired)
     `RAL_FIELD_CP_1(IMSG_FIFO_MSG_DROP, IMSG_FIFO_MSG_DROP.desired, 0)
  endgroup
  covergroup actual_cg @(cov_ev);
     option.per_instance = 1;
     option.name = {sla_ral_file::regfilename,".",regname};
     `RAL_FIELD_CP(IMSG_FIFO_MSG_GET, IMSG_FIFO_MSG_GET.actual)
     `RAL_FIELD_CP_1(IMSG_FIFO_MSG_GET, IMSG_FIFO_MSG_GET.actual, 0)
     `RAL_FIELD_CP(RSVD_6_1, RSVD_6_1.actual)
     `RAL_FIELD_CP_6(RSVD_6_1, RSVD_6_1.actual, 0,1,2,3,4,5)
     `RAL_FIELD_CP(IMSG_FIFO_MSG_DROP, IMSG_FIFO_MSG_DROP.actual)
     `RAL_FIELD_CP_1(IMSG_FIFO_MSG_DROP, IMSG_FIFO_MSG_DROP.actual, 0)
  endgroup
  `endif

  // --------------------------
  // add constraint based on database legal values
  // --------------------------
  function void build();
    IMSG_FIFO_MSG_GET = new("IMSG_FIFO_MSG_GET", "RW/1S/V", 1, 0, {"SBI_IMSG_FIFO_CTL.IMSG_FIFO_MSG_GET"});
    IMSG_FIFO_MSG_GET.set_powerwell("primary");
    IMSG_FIFO_MSG_GET.set_rand_mode(0);
   IMSG_FIFO_MSG_GET.set_reset_signame("pmu_rst_b");
    void'(add_field( IMSG_FIFO_MSG_GET ));

    RSVD_6_1 = new("RSVD_6_1", "RO", 6, 1, {"SBI_IMSG_FIFO_CTL.RSVD_6_1"});
    RSVD_6_1.set_powerwell("primary");
    RSVD_6_1.set_rand_mode(0);
    void'(add_field( RSVD_6_1 ));

    IMSG_FIFO_MSG_DROP = new("IMSG_FIFO_MSG_DROP", "RW", 1, 7, {"SBI_IMSG_FIFO_CTL.IMSG_FIFO_MSG_DROP"});
    IMSG_FIFO_MSG_DROP.set_powerwell("primary");
    IMSG_FIFO_MSG_DROP.set_rand_mode(0);
   IMSG_FIFO_MSG_DROP.set_reset_signame("pmu_rst_b");
    void'(add_field( IMSG_FIFO_MSG_DROP ));

  endfunction


`DEFINE_NONE_SCOPE_REG_API(pmu_mmr_SBI_IMSG_FIFO_CTL_reg) 
endclass : pmu_mmr_SBI_IMSG_FIFO_CTL_reg

// ================================================

class pmu_mmr_SBI_IMSG_FIFO_DATA_DW0_reg extends sla_ral_reg;

  // --------------------------
  sla_ral_field IMSG_FIFO_DATA_DW0;

  // --------------------------
  `ovm_object_utils(pmu_mmr_SBI_IMSG_FIFO_DATA_DW0_reg)

  // --------------------------
  function new(
                string         name = "",
                `ifdef SLA_RAL_COVERAGE
                sla_ral_coverage_t cov_t = COVERAGE_OFF,
                `endif
                int            bus_num=0,
                int            dev_num=0,
                int            func_num=0,
                int            offset=0,
                int            size=0,
                sla_ral_data_t reset_val=0,
                boolean_t      mon_enabled = SLA_FALSE
             );
      super.new();
      set_space_addr("CFG",undef);
      set_space_addr("MEM",undef);
      set_space_addr("IO",undef);
      set_space_addr("MSG",undef);
      set_space_addr("CREG",undef); 
      set_space_addr("LT_MEM",undef);    
 build();

      `ifdef SLA_RAL_COVERAGE
      `CONFIG_RAL_COVERAGE(regname, COVERAGE_ON, FD_RW_SAMPLE)
      if(_coverage_type != COVERAGE_OFF) begin
        if(_sample_type inside {FD_RO_SAMPLE, RO_SAMPLE})
        `CREATE_OP_SAMPLE_COV(RAL_WRITE)
        else if(_sample_type inside {FD_WO_SAMPLE, WO_SAMPLE})
        `CREATE_OP_SAMPLE_COV(RAL_READ)
        else
        `CREATE_OP_SAMPLE_COV(RAL_NONE)
        `CREATE_DESIRED_COV
        `CREATE_ACTUAL_COV
      end
      `endif

  endfunction

  `ifdef SLA_RAL_COVERAGE
  `CREATE_RAL_OP_COVERGROUP
  covergroup desired_cg @(cov_ev);
     option.per_instance = 1;
     option.name = {sla_ral_file::regfilename,".",regname};
     `RAL_FIELD_CP(IMSG_FIFO_DATA_DW0, IMSG_FIFO_DATA_DW0.desired)
     `RAL_FIELD_CP_16(IMSG_FIFO_DATA_DW0, IMSG_FIFO_DATA_DW0.desired, 0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)
     `RAL_FIELD_CP_16(IMSG_FIFO_DATA_DW0, IMSG_FIFO_DATA_DW0.desired, 16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31)
  endgroup
  covergroup actual_cg @(cov_ev);
     option.per_instance = 1;
     option.name = {sla_ral_file::regfilename,".",regname};
     `RAL_FIELD_CP(IMSG_FIFO_DATA_DW0, IMSG_FIFO_DATA_DW0.actual)
     `RAL_FIELD_CP_16(IMSG_FIFO_DATA_DW0, IMSG_FIFO_DATA_DW0.actual, 0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)
     `RAL_FIELD_CP_16(IMSG_FIFO_DATA_DW0, IMSG_FIFO_DATA_DW0.actual, 16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31)
  endgroup
  `endif

  // --------------------------
  // add constraint based on database legal values
  // --------------------------
  function void build();
    IMSG_FIFO_DATA_DW0 = new("IMSG_FIFO_DATA_DW0", "RO/V", 32, 0, {"SBI_IMSG_FIFO_DATA_DW0.IMSG_FIFO_DATA_DW0"});
    IMSG_FIFO_DATA_DW0.set_powerwell("primary");
    IMSG_FIFO_DATA_DW0.set_rand_mode(0);
    void'(add_field( IMSG_FIFO_DATA_DW0 ));

  endfunction


`DEFINE_NONE_SCOPE_REG_API(pmu_mmr_SBI_IMSG_FIFO_DATA_DW0_reg) 
endclass : pmu_mmr_SBI_IMSG_FIFO_DATA_DW0_reg

// ================================================

class pmu_mmr_SBI_IMSG_FIFO_DATA_DW1_reg extends sla_ral_reg;

  // --------------------------
  sla_ral_field IMSG_FIFO_DATA_DW1;

  // --------------------------
  `ovm_object_utils(pmu_mmr_SBI_IMSG_FIFO_DATA_DW1_reg)

  // --------------------------
  function new(
                string         name = "",
                `ifdef SLA_RAL_COVERAGE
                sla_ral_coverage_t cov_t = COVERAGE_OFF,
                `endif
                int            bus_num=0,
                int            dev_num=0,
                int            func_num=0,
                int            offset=0,
                int            size=0,
                sla_ral_data_t reset_val=0,
                boolean_t      mon_enabled = SLA_FALSE
             );
      super.new();
      set_space_addr("CFG",undef);
      set_space_addr("MEM",undef);
      set_space_addr("IO",undef);
      set_space_addr("MSG",undef);
      set_space_addr("CREG",undef); 
      set_space_addr("LT_MEM",undef);    
 build();

      `ifdef SLA_RAL_COVERAGE
      `CONFIG_RAL_COVERAGE(regname, COVERAGE_ON, FD_RW_SAMPLE)
      if(_coverage_type != COVERAGE_OFF) begin
        if(_sample_type inside {FD_RO_SAMPLE, RO_SAMPLE})
        `CREATE_OP_SAMPLE_COV(RAL_WRITE)
        else if(_sample_type inside {FD_WO_SAMPLE, WO_SAMPLE})
        `CREATE_OP_SAMPLE_COV(RAL_READ)
        else
        `CREATE_OP_SAMPLE_COV(RAL_NONE)
        `CREATE_DESIRED_COV
        `CREATE_ACTUAL_COV
      end
      `endif

  endfunction

  `ifdef SLA_RAL_COVERAGE
  `CREATE_RAL_OP_COVERGROUP
  covergroup desired_cg @(cov_ev);
     option.per_instance = 1;
     option.name = {sla_ral_file::regfilename,".",regname};
     `RAL_FIELD_CP(IMSG_FIFO_DATA_DW1, IMSG_FIFO_DATA_DW1.desired)
     `RAL_FIELD_CP_16(IMSG_FIFO_DATA_DW1, IMSG_FIFO_DATA_DW1.desired, 0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)
     `RAL_FIELD_CP_16(IMSG_FIFO_DATA_DW1, IMSG_FIFO_DATA_DW1.desired, 16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31)
  endgroup
  covergroup actual_cg @(cov_ev);
     option.per_instance = 1;
     option.name = {sla_ral_file::regfilename,".",regname};
     `RAL_FIELD_CP(IMSG_FIFO_DATA_DW1, IMSG_FIFO_DATA_DW1.actual)
     `RAL_FIELD_CP_16(IMSG_FIFO_DATA_DW1, IMSG_FIFO_DATA_DW1.actual, 0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)
     `RAL_FIELD_CP_16(IMSG_FIFO_DATA_DW1, IMSG_FIFO_DATA_DW1.actual, 16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31)
  endgroup
  `endif

  // --------------------------
  // add constraint based on database legal values
  // --------------------------
  function void build();
    IMSG_FIFO_DATA_DW1 = new("IMSG_FIFO_DATA_DW1", "RO/V", 32, 0, {"SBI_IMSG_FIFO_DATA_DW1.IMSG_FIFO_DATA_DW1"});
    IMSG_FIFO_DATA_DW1.set_powerwell("primary");
    IMSG_FIFO_DATA_DW1.set_rand_mode(0);
    void'(add_field( IMSG_FIFO_DATA_DW1 ));

  endfunction


`DEFINE_NONE_SCOPE_REG_API(pmu_mmr_SBI_IMSG_FIFO_DATA_DW1_reg) 
endclass : pmu_mmr_SBI_IMSG_FIFO_DATA_DW1_reg

// ================================================

class pmu_mmr_SBI_IMSG_FIFO_DATA_DW2_reg extends sla_ral_reg;

  // --------------------------
  sla_ral_field IMSG_FIFO_DATA_DW2;

  // --------------------------
  `ovm_object_utils(pmu_mmr_SBI_IMSG_FIFO_DATA_DW2_reg)

  // --------------------------
  function new(
                string         name = "",
                `ifdef SLA_RAL_COVERAGE
                sla_ral_coverage_t cov_t = COVERAGE_OFF,
                `endif
                int            bus_num=0,
                int            dev_num=0,
                int            func_num=0,
                int            offset=0,
                int            size=0,
                sla_ral_data_t reset_val=0,
                boolean_t      mon_enabled = SLA_FALSE
             );
      super.new();
      set_space_addr("CFG",undef);
      set_space_addr("MEM",undef);
      set_space_addr("IO",undef);
      set_space_addr("MSG",undef);
      set_space_addr("CREG",undef); 
      set_space_addr("LT_MEM",undef);    
 build();

      `ifdef SLA_RAL_COVERAGE
      `CONFIG_RAL_COVERAGE(regname, COVERAGE_ON, FD_RW_SAMPLE)
      if(_coverage_type != COVERAGE_OFF) begin
        if(_sample_type inside {FD_RO_SAMPLE, RO_SAMPLE})
        `CREATE_OP_SAMPLE_COV(RAL_WRITE)
        else if(_sample_type inside {FD_WO_SAMPLE, WO_SAMPLE})
        `CREATE_OP_SAMPLE_COV(RAL_READ)
        else
        `CREATE_OP_SAMPLE_COV(RAL_NONE)
        `CREATE_DESIRED_COV
        `CREATE_ACTUAL_COV
      end
      `endif

  endfunction

  `ifdef SLA_RAL_COVERAGE
  `CREATE_RAL_OP_COVERGROUP
  covergroup desired_cg @(cov_ev);
     option.per_instance = 1;
     option.name = {sla_ral_file::regfilename,".",regname};
     `RAL_FIELD_CP(IMSG_FIFO_DATA_DW2, IMSG_FIFO_DATA_DW2.desired)
     `RAL_FIELD_CP_16(IMSG_FIFO_DATA_DW2, IMSG_FIFO_DATA_DW2.desired, 0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)
     `RAL_FIELD_CP_16(IMSG_FIFO_DATA_DW2, IMSG_FIFO_DATA_DW2.desired, 16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31)
  endgroup
  covergroup actual_cg @(cov_ev);
     option.per_instance = 1;
     option.name = {sla_ral_file::regfilename,".",regname};
     `RAL_FIELD_CP(IMSG_FIFO_DATA_DW2, IMSG_FIFO_DATA_DW2.actual)
     `RAL_FIELD_CP_16(IMSG_FIFO_DATA_DW2, IMSG_FIFO_DATA_DW2.actual, 0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)
     `RAL_FIELD_CP_16(IMSG_FIFO_DATA_DW2, IMSG_FIFO_DATA_DW2.actual, 16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31)
  endgroup
  `endif

  // --------------------------
  // add constraint based on database legal values
  // --------------------------
  function void build();
    IMSG_FIFO_DATA_DW2 = new("IMSG_FIFO_DATA_DW2", "RO/V", 32, 0, {"SBI_IMSG_FIFO_DATA_DW2.IMSG_FIFO_DATA_DW2"});
    IMSG_FIFO_DATA_DW2.set_powerwell("primary");
    IMSG_FIFO_DATA_DW2.set_rand_mode(0);
    void'(add_field( IMSG_FIFO_DATA_DW2 ));

  endfunction


`DEFINE_NONE_SCOPE_REG_API(pmu_mmr_SBI_IMSG_FIFO_DATA_DW2_reg) 
endclass : pmu_mmr_SBI_IMSG_FIFO_DATA_DW2_reg

// ================================================

class pmu_mmr_SBI_IMSG_FIFO_DATA_DW3_reg extends sla_ral_reg;

  // --------------------------
  sla_ral_field IMSG_FIFO_DATA_DW3;

  // --------------------------
  `ovm_object_utils(pmu_mmr_SBI_IMSG_FIFO_DATA_DW3_reg)

  // --------------------------
  function new(
                string         name = "",
                `ifdef SLA_RAL_COVERAGE
                sla_ral_coverage_t cov_t = COVERAGE_OFF,
                `endif
                int            bus_num=0,
                int            dev_num=0,
                int            func_num=0,
                int            offset=0,
                int            size=0,
                sla_ral_data_t reset_val=0,
                boolean_t      mon_enabled = SLA_FALSE
             );
      super.new();
      set_space_addr("CFG",undef);
      set_space_addr("MEM",undef);
      set_space_addr("IO",undef);
      set_space_addr("MSG",undef);
      set_space_addr("CREG",undef); 
      set_space_addr("LT_MEM",undef);    
 build();

      `ifdef SLA_RAL_COVERAGE
      `CONFIG_RAL_COVERAGE(regname, COVERAGE_ON, FD_RW_SAMPLE)
      if(_coverage_type != COVERAGE_OFF) begin
        if(_sample_type inside {FD_RO_SAMPLE, RO_SAMPLE})
        `CREATE_OP_SAMPLE_COV(RAL_WRITE)
        else if(_sample_type inside {FD_WO_SAMPLE, WO_SAMPLE})
        `CREATE_OP_SAMPLE_COV(RAL_READ)
        else
        `CREATE_OP_SAMPLE_COV(RAL_NONE)
        `CREATE_DESIRED_COV
        `CREATE_ACTUAL_COV
      end
      `endif

  endfunction

  `ifdef SLA_RAL_COVERAGE
  `CREATE_RAL_OP_COVERGROUP
  covergroup desired_cg @(cov_ev);
     option.per_instance = 1;
     option.name = {sla_ral_file::regfilename,".",regname};
     `RAL_FIELD_CP(IMSG_FIFO_DATA_DW3, IMSG_FIFO_DATA_DW3.desired)
     `RAL_FIELD_CP_16(IMSG_FIFO_DATA_DW3, IMSG_FIFO_DATA_DW3.desired, 0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)
     `RAL_FIELD_CP_16(IMSG_FIFO_DATA_DW3, IMSG_FIFO_DATA_DW3.desired, 16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31)
  endgroup
  covergroup actual_cg @(cov_ev);
     option.per_instance = 1;
     option.name = {sla_ral_file::regfilename,".",regname};
     `RAL_FIELD_CP(IMSG_FIFO_DATA_DW3, IMSG_FIFO_DATA_DW3.actual)
     `RAL_FIELD_CP_16(IMSG_FIFO_DATA_DW3, IMSG_FIFO_DATA_DW3.actual, 0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)
     `RAL_FIELD_CP_16(IMSG_FIFO_DATA_DW3, IMSG_FIFO_DATA_DW3.actual, 16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31)
  endgroup
  `endif

  // --------------------------
  // add constraint based on database legal values
  // --------------------------
  function void build();
    IMSG_FIFO_DATA_DW3 = new("IMSG_FIFO_DATA_DW3", "RO/V", 32, 0, {"SBI_IMSG_FIFO_DATA_DW3.IMSG_FIFO_DATA_DW3"});
    IMSG_FIFO_DATA_DW3.set_powerwell("primary");
    IMSG_FIFO_DATA_DW3.set_rand_mode(0);
    void'(add_field( IMSG_FIFO_DATA_DW3 ));

  endfunction


`DEFINE_NONE_SCOPE_REG_API(pmu_mmr_SBI_IMSG_FIFO_DATA_DW3_reg) 
endclass : pmu_mmr_SBI_IMSG_FIFO_DATA_DW3_reg

// ================================================

class pmu_mmr_TSRSP_CTL_reg extends sla_ral_reg;

  // --------------------------
  sla_ral_field ART_EN;
  sla_ral_field TSRSP_EN;
  sla_ral_field RSVD_7_2;
  sla_ral_field ART_RST;
  sla_ral_field SNAPSHOT_ART;
  sla_ral_field RSVD_31_10;

  // --------------------------
  `ovm_object_utils(pmu_mmr_TSRSP_CTL_reg)

  // --------------------------
  function new(
                string         name = "",
                `ifdef SLA_RAL_COVERAGE
                sla_ral_coverage_t cov_t = COVERAGE_OFF,
                `endif
                int            bus_num=0,
                int            dev_num=0,
                int            func_num=0,
                int            offset=0,
                int            size=0,
                sla_ral_data_t reset_val=0,
                boolean_t      mon_enabled = SLA_FALSE
             );
      super.new();
      set_space_addr("CFG",undef);
      set_space_addr("MEM",undef);
      set_space_addr("IO",undef);
      set_space_addr("MSG",undef);
      set_space_addr("CREG",undef); 
      set_space_addr("LT_MEM",undef);    
 build();

      `ifdef SLA_RAL_COVERAGE
      `CONFIG_RAL_COVERAGE(regname, COVERAGE_ON, FD_RW_SAMPLE)
      if(_coverage_type != COVERAGE_OFF) begin
        if(_sample_type inside {FD_RO_SAMPLE, RO_SAMPLE})
        `CREATE_OP_SAMPLE_COV(RAL_WRITE)
        else if(_sample_type inside {FD_WO_SAMPLE, WO_SAMPLE})
        `CREATE_OP_SAMPLE_COV(RAL_READ)
        else
        `CREATE_OP_SAMPLE_COV(RAL_NONE)
        `CREATE_DESIRED_COV
        `CREATE_ACTUAL_COV
      end
      `endif

  endfunction

  `ifdef SLA_RAL_COVERAGE
  `CREATE_RAL_OP_COVERGROUP
  covergroup desired_cg @(cov_ev);
     option.per_instance = 1;
     option.name = {sla_ral_file::regfilename,".",regname};
     `RAL_FIELD_CP(ART_EN, ART_EN.desired)
     `RAL_FIELD_CP_1(ART_EN, ART_EN.desired, 0)
     `RAL_FIELD_CP(TSRSP_EN, TSRSP_EN.desired)
     `RAL_FIELD_CP_1(TSRSP_EN, TSRSP_EN.desired, 0)
     `RAL_FIELD_CP(RSVD_7_2, RSVD_7_2.desired)
     `RAL_FIELD_CP_6(RSVD_7_2, RSVD_7_2.desired, 0,1,2,3,4,5)
     `RAL_FIELD_CP(ART_RST, ART_RST.desired)
     `RAL_FIELD_CP_1(ART_RST, ART_RST.desired, 0)
     `RAL_FIELD_CP(SNAPSHOT_ART, SNAPSHOT_ART.desired)
     `RAL_FIELD_CP_1(SNAPSHOT_ART, SNAPSHOT_ART.desired, 0)
     `RAL_FIELD_CP(RSVD_31_10, RSVD_31_10.desired)
     `RAL_FIELD_CP_16(RSVD_31_10, RSVD_31_10.desired, 0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)
     `RAL_FIELD_CP_6(RSVD_31_10, RSVD_31_10.desired, 16,17,18,19,20,21)
  endgroup
  covergroup actual_cg @(cov_ev);
     option.per_instance = 1;
     option.name = {sla_ral_file::regfilename,".",regname};
     `RAL_FIELD_CP(ART_EN, ART_EN.actual)
     `RAL_FIELD_CP_1(ART_EN, ART_EN.actual, 0)
     `RAL_FIELD_CP(TSRSP_EN, TSRSP_EN.actual)
     `RAL_FIELD_CP_1(TSRSP_EN, TSRSP_EN.actual, 0)
     `RAL_FIELD_CP(RSVD_7_2, RSVD_7_2.actual)
     `RAL_FIELD_CP_6(RSVD_7_2, RSVD_7_2.actual, 0,1,2,3,4,5)
     `RAL_FIELD_CP(ART_RST, ART_RST.actual)
     `RAL_FIELD_CP_1(ART_RST, ART_RST.actual, 0)
     `RAL_FIELD_CP(SNAPSHOT_ART, SNAPSHOT_ART.actual)
     `RAL_FIELD_CP_1(SNAPSHOT_ART, SNAPSHOT_ART.actual, 0)
     `RAL_FIELD_CP(RSVD_31_10, RSVD_31_10.actual)
     `RAL_FIELD_CP_16(RSVD_31_10, RSVD_31_10.actual, 0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)
     `RAL_FIELD_CP_6(RSVD_31_10, RSVD_31_10.actual, 16,17,18,19,20,21)
  endgroup
  `endif

  // --------------------------
  // add constraint based on database legal values
  // --------------------------
  function void build();
    ART_EN = new("ART_EN", "RW", 1, 0, {"TSRSP_CTL.ART_EN"});
    ART_EN.set_powerwell("primary");
    ART_EN.set_rand_mode(0);
   ART_EN.set_reset_signame("pmu_rst_b");
    void'(add_field( ART_EN ));

    TSRSP_EN = new("TSRSP_EN", "RW", 1, 1, {"TSRSP_CTL.TSRSP_EN"});
    TSRSP_EN.set_powerwell("primary");
    TSRSP_EN.set_rand_mode(0);
   TSRSP_EN.set_reset_signame("pmu_rst_b");
    void'(add_field( TSRSP_EN ));

    RSVD_7_2 = new("RSVD_7_2", "RO", 6, 2, {"TSRSP_CTL.RSVD_7_2"});
    RSVD_7_2.set_powerwell("primary");
    RSVD_7_2.set_rand_mode(0);
   RSVD_7_2.set_reset_signame("pmu_rst_b");
    void'(add_field( RSVD_7_2 ));

    ART_RST = new("ART_RST", "WO", 1, 8, {"TSRSP_CTL.ART_RST"});
    ART_RST.set_powerwell("primary");
    ART_RST.set_rand_mode(0);
   ART_RST.set_reset_signame("pmu_rst_b");
    void'(add_field( ART_RST ));

    SNAPSHOT_ART = new("SNAPSHOT_ART", "WO", 1, 9, {"TSRSP_CTL.SNAPSHOT_ART"});
    SNAPSHOT_ART.set_powerwell("primary");
    SNAPSHOT_ART.set_rand_mode(0);
   SNAPSHOT_ART.set_reset_signame("pmu_rst_b");
    void'(add_field( SNAPSHOT_ART ));

    RSVD_31_10 = new("RSVD_31_10", "RO", 22, 10, {"TSRSP_CTL.RSVD_31_10"});
    RSVD_31_10.set_powerwell("primary");
    RSVD_31_10.set_rand_mode(0);
   RSVD_31_10.set_reset_signame("pmu_rst_b");
    void'(add_field( RSVD_31_10 ));

  endfunction


`DEFINE_NONE_SCOPE_REG_API(pmu_mmr_TSRSP_CTL_reg) 
endclass : pmu_mmr_TSRSP_CTL_reg

// ================================================

class pmu_mmr_TSRSP_STS_reg extends sla_ral_reg;

  // --------------------------
  sla_ral_field CPL_UC;
  sla_ral_field CPL_UR;
  sla_ral_field SAI_ERR;
  sla_ral_field FLAG_Q_FULL;
  sla_ral_field FLAG_MSG_DIS;
  sla_ral_field FLAG_UNSUP_OPS;
  sla_ral_field RSVD_7_6;
  sla_ral_field TSRSP_INPROG;
  sla_ral_field RSVD_11_9;
  sla_ral_field REQ_AT;
  sla_ral_field REQ_ST;
  sla_ral_field REQ_SID;
  sla_ral_field REQ_MAD;
  sla_ral_field REQ_TBC;

  // --------------------------
  `ovm_object_utils(pmu_mmr_TSRSP_STS_reg)

  // --------------------------
  function new(
                string         name = "",
                `ifdef SLA_RAL_COVERAGE
                sla_ral_coverage_t cov_t = COVERAGE_OFF,
                `endif
                int            bus_num=0,
                int            dev_num=0,
                int            func_num=0,
                int            offset=0,
                int            size=0,
                sla_ral_data_t reset_val=0,
                boolean_t      mon_enabled = SLA_FALSE
             );
      super.new();
      set_space_addr("CFG",undef);
      set_space_addr("MEM",undef);
      set_space_addr("IO",undef);
      set_space_addr("MSG",undef);
      set_space_addr("CREG",undef); 
      set_space_addr("LT_MEM",undef);    
 build();

      `ifdef SLA_RAL_COVERAGE
      `CONFIG_RAL_COVERAGE(regname, COVERAGE_ON, FD_RW_SAMPLE)
      if(_coverage_type != COVERAGE_OFF) begin
        if(_sample_type inside {FD_RO_SAMPLE, RO_SAMPLE})
        `CREATE_OP_SAMPLE_COV(RAL_WRITE)
        else if(_sample_type inside {FD_WO_SAMPLE, WO_SAMPLE})
        `CREATE_OP_SAMPLE_COV(RAL_READ)
        else
        `CREATE_OP_SAMPLE_COV(RAL_NONE)
        `CREATE_DESIRED_COV
        `CREATE_ACTUAL_COV
      end
      `endif

  endfunction

  `ifdef SLA_RAL_COVERAGE
  `CREATE_RAL_OP_COVERGROUP
  covergroup desired_cg @(cov_ev);
     option.per_instance = 1;
     option.name = {sla_ral_file::regfilename,".",regname};
     `RAL_FIELD_CP(CPL_UC, CPL_UC.desired)
     `RAL_FIELD_CP_1(CPL_UC, CPL_UC.desired, 0)
     `RAL_FIELD_CP(CPL_UR, CPL_UR.desired)
     `RAL_FIELD_CP_1(CPL_UR, CPL_UR.desired, 0)
     `RAL_FIELD_CP(SAI_ERR, SAI_ERR.desired)
     `RAL_FIELD_CP_1(SAI_ERR, SAI_ERR.desired, 0)
     `RAL_FIELD_CP(FLAG_Q_FULL, FLAG_Q_FULL.desired)
     `RAL_FIELD_CP_1(FLAG_Q_FULL, FLAG_Q_FULL.desired, 0)
     `RAL_FIELD_CP(FLAG_MSG_DIS, FLAG_MSG_DIS.desired)
     `RAL_FIELD_CP_1(FLAG_MSG_DIS, FLAG_MSG_DIS.desired, 0)
     `RAL_FIELD_CP(FLAG_UNSUP_OPS, FLAG_UNSUP_OPS.desired)
     `RAL_FIELD_CP_1(FLAG_UNSUP_OPS, FLAG_UNSUP_OPS.desired, 0)
     `RAL_FIELD_CP(RSVD_7_6, RSVD_7_6.desired)
     `RAL_FIELD_CP_2(RSVD_7_6, RSVD_7_6.desired, 0,1)
     `RAL_FIELD_CP(TSRSP_INPROG, TSRSP_INPROG.desired)
     `RAL_FIELD_CP_1(TSRSP_INPROG, TSRSP_INPROG.desired, 0)
     `RAL_FIELD_CP(RSVD_11_9, RSVD_11_9.desired)
     `RAL_FIELD_CP_3(RSVD_11_9, RSVD_11_9.desired, 0,1,2)
     `RAL_FIELD_CP(REQ_AT, REQ_AT.desired)
     `RAL_FIELD_CP_3(REQ_AT, REQ_AT.desired, 0,1,2)
     `RAL_FIELD_CP(REQ_ST, REQ_ST.desired)
     `RAL_FIELD_CP_1(REQ_ST, REQ_ST.desired, 0)
     `RAL_FIELD_CP(REQ_SID, REQ_SID.desired)
     `RAL_FIELD_CP_8(REQ_SID, REQ_SID.desired, 0,1,2,3,4,5,6,7)
     `RAL_FIELD_CP(REQ_MAD, REQ_MAD.desired)
     `RAL_FIELD_CP_4(REQ_MAD, REQ_MAD.desired, 0,1,2,3)
     `RAL_FIELD_CP(REQ_TBC, REQ_TBC.desired)
     `RAL_FIELD_CP_4(REQ_TBC, REQ_TBC.desired, 0,1,2,3)
  endgroup
  covergroup actual_cg @(cov_ev);
     option.per_instance = 1;
     option.name = {sla_ral_file::regfilename,".",regname};
     `RAL_FIELD_CP(CPL_UC, CPL_UC.actual)
     `RAL_FIELD_CP_1(CPL_UC, CPL_UC.actual, 0)
     `RAL_FIELD_CP(CPL_UR, CPL_UR.actual)
     `RAL_FIELD_CP_1(CPL_UR, CPL_UR.actual, 0)
     `RAL_FIELD_CP(SAI_ERR, SAI_ERR.actual)
     `RAL_FIELD_CP_1(SAI_ERR, SAI_ERR.actual, 0)
     `RAL_FIELD_CP(FLAG_Q_FULL, FLAG_Q_FULL.actual)
     `RAL_FIELD_CP_1(FLAG_Q_FULL, FLAG_Q_FULL.actual, 0)
     `RAL_FIELD_CP(FLAG_MSG_DIS, FLAG_MSG_DIS.actual)
     `RAL_FIELD_CP_1(FLAG_MSG_DIS, FLAG_MSG_DIS.actual, 0)
     `RAL_FIELD_CP(FLAG_UNSUP_OPS, FLAG_UNSUP_OPS.actual)
     `RAL_FIELD_CP_1(FLAG_UNSUP_OPS, FLAG_UNSUP_OPS.actual, 0)
     `RAL_FIELD_CP(RSVD_7_6, RSVD_7_6.actual)
     `RAL_FIELD_CP_2(RSVD_7_6, RSVD_7_6.actual, 0,1)
     `RAL_FIELD_CP(TSRSP_INPROG, TSRSP_INPROG.actual)
     `RAL_FIELD_CP_1(TSRSP_INPROG, TSRSP_INPROG.actual, 0)
     `RAL_FIELD_CP(RSVD_11_9, RSVD_11_9.actual)
     `RAL_FIELD_CP_3(RSVD_11_9, RSVD_11_9.actual, 0,1,2)
     `RAL_FIELD_CP(REQ_AT, REQ_AT.actual)
     `RAL_FIELD_CP_3(REQ_AT, REQ_AT.actual, 0,1,2)
     `RAL_FIELD_CP(REQ_ST, REQ_ST.actual)
     `RAL_FIELD_CP_1(REQ_ST, REQ_ST.actual, 0)
     `RAL_FIELD_CP(REQ_SID, REQ_SID.actual)
     `RAL_FIELD_CP_8(REQ_SID, REQ_SID.actual, 0,1,2,3,4,5,6,7)
     `RAL_FIELD_CP(REQ_MAD, REQ_MAD.actual)
     `RAL_FIELD_CP_4(REQ_MAD, REQ_MAD.actual, 0,1,2,3)
     `RAL_FIELD_CP(REQ_TBC, REQ_TBC.actual)
     `RAL_FIELD_CP_4(REQ_TBC, REQ_TBC.actual, 0,1,2,3)
  endgroup
  `endif

  // --------------------------
  // add constraint based on database legal values
  // --------------------------
  function void build();
    CPL_UC = new("CPL_UC", "RW/1C/V", 1, 0, {"TSRSP_STS.CPL_UC"});
    CPL_UC.set_powerwell("primary");
    CPL_UC.set_rand_mode(0);
   CPL_UC.set_reset_signame("pmu_rst_b");
    void'(add_field( CPL_UC ));

    CPL_UR = new("CPL_UR", "RW/1C/V", 1, 1, {"TSRSP_STS.CPL_UR"});
    CPL_UR.set_powerwell("primary");
    CPL_UR.set_rand_mode(0);
   CPL_UR.set_reset_signame("pmu_rst_b");
    void'(add_field( CPL_UR ));

    SAI_ERR = new("SAI_ERR", "RW/1C/V", 1, 2, {"TSRSP_STS.SAI_ERR"});
    SAI_ERR.set_powerwell("primary");
    SAI_ERR.set_rand_mode(0);
   SAI_ERR.set_reset_signame("pmu_rst_b");
    void'(add_field( SAI_ERR ));

    FLAG_Q_FULL = new("FLAG_Q_FULL", "RW/1C/V", 1, 3, {"TSRSP_STS.FLAG_Q_FULL"});
    FLAG_Q_FULL.set_powerwell("primary");
    FLAG_Q_FULL.set_rand_mode(0);
   FLAG_Q_FULL.set_reset_signame("pmu_rst_b");
    void'(add_field( FLAG_Q_FULL ));

    FLAG_MSG_DIS = new("FLAG_MSG_DIS", "RW/1C/V", 1, 4, {"TSRSP_STS.FLAG_MSG_DIS"});
    FLAG_MSG_DIS.set_powerwell("primary");
    FLAG_MSG_DIS.set_rand_mode(0);
   FLAG_MSG_DIS.set_reset_signame("pmu_rst_b");
    void'(add_field( FLAG_MSG_DIS ));

    FLAG_UNSUP_OPS = new("FLAG_UNSUP_OPS", "RW/1C/V", 1, 5, {"TSRSP_STS.FLAG_UNSUP_OPS"});
    FLAG_UNSUP_OPS.set_powerwell("primary");
    FLAG_UNSUP_OPS.set_rand_mode(0);
   FLAG_UNSUP_OPS.set_reset_signame("pmu_rst_b");
    void'(add_field( FLAG_UNSUP_OPS ));

    RSVD_7_6 = new("RSVD_7_6", "RO", 2, 6, {"TSRSP_STS.RSVD_7_6"});
    RSVD_7_6.set_powerwell("primary");
    RSVD_7_6.set_rand_mode(0);
   RSVD_7_6.set_reset_signame("pmu_rst_b");
    void'(add_field( RSVD_7_6 ));

    TSRSP_INPROG = new("TSRSP_INPROG", "RO/V", 1, 8, {"TSRSP_STS.TSRSP_INPROG"});
    TSRSP_INPROG.set_powerwell("primary");
    TSRSP_INPROG.set_rand_mode(0);
   TSRSP_INPROG.set_reset_signame("pmu_rst_b");
    void'(add_field( TSRSP_INPROG ));

    RSVD_11_9 = new("RSVD_11_9", "RO", 3, 9, {"TSRSP_STS.RSVD_11_9"});
    RSVD_11_9.set_powerwell("primary");
    RSVD_11_9.set_rand_mode(0);
   RSVD_11_9.set_reset_signame("pmu_rst_b");
    void'(add_field( RSVD_11_9 ));

    REQ_AT = new("REQ_AT", "RO/V", 3, 12, {"TSRSP_STS.REQ_AT"});
    REQ_AT.set_powerwell("primary");
    REQ_AT.set_rand_mode(0);
   REQ_AT.set_reset_signame("pmu_rst_b");
    void'(add_field( REQ_AT ));

    REQ_ST = new("REQ_ST", "RO/V", 1, 15, {"TSRSP_STS.REQ_ST"});
    REQ_ST.set_powerwell("primary");
    REQ_ST.set_rand_mode(0);
   REQ_ST.set_reset_signame("pmu_rst_b");
    void'(add_field( REQ_ST ));

    REQ_SID = new("REQ_SID", "RO/V", 8, 16, {"TSRSP_STS.REQ_SID"});
    REQ_SID.set_powerwell("primary");
    REQ_SID.set_rand_mode(0);
   REQ_SID.set_reset_signame("pmu_rst_b");
    void'(add_field( REQ_SID ));

    REQ_MAD = new("REQ_MAD", "RO/V", 4, 24, {"TSRSP_STS.REQ_MAD"});
    REQ_MAD.set_powerwell("primary");
    REQ_MAD.set_rand_mode(0);
   REQ_MAD.set_reset_signame("pmu_rst_b");
    void'(add_field( REQ_MAD ));

    REQ_TBC = new("REQ_TBC", "RO/V", 4, 28, {"TSRSP_STS.REQ_TBC"});
    REQ_TBC.set_powerwell("primary");
    REQ_TBC.set_rand_mode(0);
   REQ_TBC.set_reset_signame("pmu_rst_b");
    void'(add_field( REQ_TBC ));

  endfunction


`DEFINE_NONE_SCOPE_REG_API(pmu_mmr_TSRSP_STS_reg) 
endclass : pmu_mmr_TSRSP_STS_reg

// ================================================

class pmu_mmr_TSRSP_CPL_STS_reg extends sla_ral_reg;

  // --------------------------
  sla_ral_field RSVD_7_0;
  sla_ral_field SRCID;
  sla_ral_field OPCODE;
  sla_ral_field TAG;
  sla_ral_field RSP;

  // --------------------------
  `ovm_object_utils(pmu_mmr_TSRSP_CPL_STS_reg)

  // --------------------------
  function new(
                string         name = "",
                `ifdef SLA_RAL_COVERAGE
                sla_ral_coverage_t cov_t = COVERAGE_OFF,
                `endif
                int            bus_num=0,
                int            dev_num=0,
                int            func_num=0,
                int            offset=0,
                int            size=0,
                sla_ral_data_t reset_val=0,
                boolean_t      mon_enabled = SLA_FALSE
             );
      super.new();
      set_space_addr("CFG",undef);
      set_space_addr("MEM",undef);
      set_space_addr("IO",undef);
      set_space_addr("MSG",undef);
      set_space_addr("CREG",undef); 
      set_space_addr("LT_MEM",undef);    
 build();

      `ifdef SLA_RAL_COVERAGE
      `CONFIG_RAL_COVERAGE(regname, COVERAGE_ON, FD_RW_SAMPLE)
      if(_coverage_type != COVERAGE_OFF) begin
        if(_sample_type inside {FD_RO_SAMPLE, RO_SAMPLE})
        `CREATE_OP_SAMPLE_COV(RAL_WRITE)
        else if(_sample_type inside {FD_WO_SAMPLE, WO_SAMPLE})
        `CREATE_OP_SAMPLE_COV(RAL_READ)
        else
        `CREATE_OP_SAMPLE_COV(RAL_NONE)
        `CREATE_DESIRED_COV
        `CREATE_ACTUAL_COV
      end
      `endif

  endfunction

  `ifdef SLA_RAL_COVERAGE
  `CREATE_RAL_OP_COVERGROUP
  covergroup desired_cg @(cov_ev);
     option.per_instance = 1;
     option.name = {sla_ral_file::regfilename,".",regname};
     `RAL_FIELD_CP(RSVD_7_0, RSVD_7_0.desired)
     `RAL_FIELD_CP_8(RSVD_7_0, RSVD_7_0.desired, 0,1,2,3,4,5,6,7)
     `RAL_FIELD_CP(SRCID, SRCID.desired)
     `RAL_FIELD_CP_8(SRCID, SRCID.desired, 0,1,2,3,4,5,6,7)
     `RAL_FIELD_CP(OPCODE, OPCODE.desired)
     `RAL_FIELD_CP_8(OPCODE, OPCODE.desired, 0,1,2,3,4,5,6,7)
     `RAL_FIELD_CP(TAG, TAG.desired)
     `RAL_FIELD_CP_3(TAG, TAG.desired, 0,1,2)
     `RAL_FIELD_CP(RSP, RSP.desired)
     `RAL_FIELD_CP_2(RSP, RSP.desired, 0,1)
  endgroup
  covergroup actual_cg @(cov_ev);
     option.per_instance = 1;
     option.name = {sla_ral_file::regfilename,".",regname};
     `RAL_FIELD_CP(RSVD_7_0, RSVD_7_0.actual)
     `RAL_FIELD_CP_8(RSVD_7_0, RSVD_7_0.actual, 0,1,2,3,4,5,6,7)
     `RAL_FIELD_CP(SRCID, SRCID.actual)
     `RAL_FIELD_CP_8(SRCID, SRCID.actual, 0,1,2,3,4,5,6,7)
     `RAL_FIELD_CP(OPCODE, OPCODE.actual)
     `RAL_FIELD_CP_8(OPCODE, OPCODE.actual, 0,1,2,3,4,5,6,7)
     `RAL_FIELD_CP(TAG, TAG.actual)
     `RAL_FIELD_CP_3(TAG, TAG.actual, 0,1,2)
     `RAL_FIELD_CP(RSP, RSP.actual)
     `RAL_FIELD_CP_2(RSP, RSP.actual, 0,1)
  endgroup
  `endif

  // --------------------------
  // add constraint based on database legal values
  // --------------------------
  function void build();
    RSVD_7_0 = new("RSVD_7_0", "RO", 8, 0, {"TSRSP_CPL_STS.RSVD_7_0"});
    RSVD_7_0.set_powerwell("primary");
    RSVD_7_0.set_rand_mode(0);
   RSVD_7_0.set_reset_signame("pmu_rst_b");
    void'(add_field( RSVD_7_0 ));

    SRCID = new("SRCID", "RO/V", 8, 8, {"TSRSP_CPL_STS.SRCID"});
    SRCID.set_powerwell("primary");
    SRCID.set_rand_mode(0);
   SRCID.set_reset_signame("pmu_rst_b");
    void'(add_field( SRCID ));

    OPCODE = new("OPCODE", "RO/V", 8, 16, {"TSRSP_CPL_STS.OPCODE"});
    OPCODE.set_powerwell("primary");
    OPCODE.set_rand_mode(0);
   OPCODE.set_reset_signame("pmu_rst_b");
    void'(add_field( OPCODE ));

    TAG = new("TAG", "RO/V", 3, 24, {"TSRSP_CPL_STS.TAG"});
    TAG.set_powerwell("primary");
    TAG.set_rand_mode(0);
   TAG.set_reset_signame("pmu_rst_b");
    void'(add_field( TAG ));

    RSP = new("RSP", "RO/V", 2, 27, {"TSRSP_CPL_STS.RSP"});
    RSP.set_powerwell("primary");
    RSP.set_rand_mode(0);
   RSP.set_reset_signame("pmu_rst_b");
    void'(add_field( RSP ));

  endfunction


`DEFINE_NONE_SCOPE_REG_API(pmu_mmr_TSRSP_CPL_STS_reg) 
endclass : pmu_mmr_TSRSP_CPL_STS_reg

// ================================================

class pmu_mmr_TSRSP_ART_SNAPSHOT_LO_reg extends sla_ral_reg;

  // --------------------------
  sla_ral_field TSRSP_ART_SNAPSHOT_LO;

  // --------------------------
  `ovm_object_utils(pmu_mmr_TSRSP_ART_SNAPSHOT_LO_reg)

  // --------------------------
  function new(
                string         name = "",
                `ifdef SLA_RAL_COVERAGE
                sla_ral_coverage_t cov_t = COVERAGE_OFF,
                `endif
                int            bus_num=0,
                int            dev_num=0,
                int            func_num=0,
                int            offset=0,
                int            size=0,
                sla_ral_data_t reset_val=0,
                boolean_t      mon_enabled = SLA_FALSE
             );
      super.new();
      set_space_addr("CFG",undef);
      set_space_addr("MEM",undef);
      set_space_addr("IO",undef);
      set_space_addr("MSG",undef);
      set_space_addr("CREG",undef); 
      set_space_addr("LT_MEM",undef);    
 build();

      `ifdef SLA_RAL_COVERAGE
      `CONFIG_RAL_COVERAGE(regname, COVERAGE_ON, FD_RW_SAMPLE)
      if(_coverage_type != COVERAGE_OFF) begin
        if(_sample_type inside {FD_RO_SAMPLE, RO_SAMPLE})
        `CREATE_OP_SAMPLE_COV(RAL_WRITE)
        else if(_sample_type inside {FD_WO_SAMPLE, WO_SAMPLE})
        `CREATE_OP_SAMPLE_COV(RAL_READ)
        else
        `CREATE_OP_SAMPLE_COV(RAL_NONE)
        `CREATE_DESIRED_COV
        `CREATE_ACTUAL_COV
      end
      `endif

  endfunction

  `ifdef SLA_RAL_COVERAGE
  `CREATE_RAL_OP_COVERGROUP
  covergroup desired_cg @(cov_ev);
     option.per_instance = 1;
     option.name = {sla_ral_file::regfilename,".",regname};
     `RAL_FIELD_CP(TSRSP_ART_SNAPSHOT_LO, TSRSP_ART_SNAPSHOT_LO.desired)
     `RAL_FIELD_CP_16(TSRSP_ART_SNAPSHOT_LO, TSRSP_ART_SNAPSHOT_LO.desired, 0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)
     `RAL_FIELD_CP_16(TSRSP_ART_SNAPSHOT_LO, TSRSP_ART_SNAPSHOT_LO.desired, 16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31)
  endgroup
  covergroup actual_cg @(cov_ev);
     option.per_instance = 1;
     option.name = {sla_ral_file::regfilename,".",regname};
     `RAL_FIELD_CP(TSRSP_ART_SNAPSHOT_LO, TSRSP_ART_SNAPSHOT_LO.actual)
     `RAL_FIELD_CP_16(TSRSP_ART_SNAPSHOT_LO, TSRSP_ART_SNAPSHOT_LO.actual, 0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)
     `RAL_FIELD_CP_16(TSRSP_ART_SNAPSHOT_LO, TSRSP_ART_SNAPSHOT_LO.actual, 16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31)
  endgroup
  `endif

  // --------------------------
  // add constraint based on database legal values
  // --------------------------
  function void build();
    TSRSP_ART_SNAPSHOT_LO = new("TSRSP_ART_SNAPSHOT_LO", "RO/V", 32, 0, {"TSRSP_ART_SNAPSHOT_LO.TSRSP_ART_SNAPSHOT_LO"});
    TSRSP_ART_SNAPSHOT_LO.set_powerwell("primary");
    TSRSP_ART_SNAPSHOT_LO.set_rand_mode(0);
   TSRSP_ART_SNAPSHOT_LO.set_reset_signame("pmu_rst_b");
    void'(add_field( TSRSP_ART_SNAPSHOT_LO ));

  endfunction


`DEFINE_NONE_SCOPE_REG_API(pmu_mmr_TSRSP_ART_SNAPSHOT_LO_reg) 
endclass : pmu_mmr_TSRSP_ART_SNAPSHOT_LO_reg

// ================================================

class pmu_mmr_TSRSP_ART_SNAPSHOT_HI_reg extends sla_ral_reg;

  // --------------------------
  sla_ral_field TSRSP_ART_SNAPSHOT_HI;

  // --------------------------
  `ovm_object_utils(pmu_mmr_TSRSP_ART_SNAPSHOT_HI_reg)

  // --------------------------
  function new(
                string         name = "",
                `ifdef SLA_RAL_COVERAGE
                sla_ral_coverage_t cov_t = COVERAGE_OFF,
                `endif
                int            bus_num=0,
                int            dev_num=0,
                int            func_num=0,
                int            offset=0,
                int            size=0,
                sla_ral_data_t reset_val=0,
                boolean_t      mon_enabled = SLA_FALSE
             );
      super.new();
      set_space_addr("CFG",undef);
      set_space_addr("MEM",undef);
      set_space_addr("IO",undef);
      set_space_addr("MSG",undef);
      set_space_addr("CREG",undef); 
      set_space_addr("LT_MEM",undef);    
 build();

      `ifdef SLA_RAL_COVERAGE
      `CONFIG_RAL_COVERAGE(regname, COVERAGE_ON, FD_RW_SAMPLE)
      if(_coverage_type != COVERAGE_OFF) begin
        if(_sample_type inside {FD_RO_SAMPLE, RO_SAMPLE})
        `CREATE_OP_SAMPLE_COV(RAL_WRITE)
        else if(_sample_type inside {FD_WO_SAMPLE, WO_SAMPLE})
        `CREATE_OP_SAMPLE_COV(RAL_READ)
        else
        `CREATE_OP_SAMPLE_COV(RAL_NONE)
        `CREATE_DESIRED_COV
        `CREATE_ACTUAL_COV
      end
      `endif

  endfunction

  `ifdef SLA_RAL_COVERAGE
  `CREATE_RAL_OP_COVERGROUP
  covergroup desired_cg @(cov_ev);
     option.per_instance = 1;
     option.name = {sla_ral_file::regfilename,".",regname};
     `RAL_FIELD_CP(TSRSP_ART_SNAPSHOT_HI, TSRSP_ART_SNAPSHOT_HI.desired)
     `RAL_FIELD_CP_16(TSRSP_ART_SNAPSHOT_HI, TSRSP_ART_SNAPSHOT_HI.desired, 0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)
     `RAL_FIELD_CP_16(TSRSP_ART_SNAPSHOT_HI, TSRSP_ART_SNAPSHOT_HI.desired, 16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31)
  endgroup
  covergroup actual_cg @(cov_ev);
     option.per_instance = 1;
     option.name = {sla_ral_file::regfilename,".",regname};
     `RAL_FIELD_CP(TSRSP_ART_SNAPSHOT_HI, TSRSP_ART_SNAPSHOT_HI.actual)
     `RAL_FIELD_CP_16(TSRSP_ART_SNAPSHOT_HI, TSRSP_ART_SNAPSHOT_HI.actual, 0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)
     `RAL_FIELD_CP_16(TSRSP_ART_SNAPSHOT_HI, TSRSP_ART_SNAPSHOT_HI.actual, 16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31)
  endgroup
  `endif

  // --------------------------
  // add constraint based on database legal values
  // --------------------------
  function void build();
    TSRSP_ART_SNAPSHOT_HI = new("TSRSP_ART_SNAPSHOT_HI", "RO/V", 32, 0, {"TSRSP_ART_SNAPSHOT_HI.TSRSP_ART_SNAPSHOT_HI"});
    TSRSP_ART_SNAPSHOT_HI.set_powerwell("primary");
    TSRSP_ART_SNAPSHOT_HI.set_rand_mode(0);
   TSRSP_ART_SNAPSHOT_HI.set_reset_signame("pmu_rst_b");
    void'(add_field( TSRSP_ART_SNAPSHOT_HI ));

  endfunction


`DEFINE_NONE_SCOPE_REG_API(pmu_mmr_TSRSP_ART_SNAPSHOT_HI_reg) 
endclass : pmu_mmr_TSRSP_ART_SNAPSHOT_HI_reg

// ================================================

class pmu_mmr_TS_SERIAL_CTL_reg extends sla_ral_reg;

  // --------------------------
  sla_ral_field SERIAL_TIMESYNC_START;
  sla_ral_field RSVD_7_1;
  sla_ral_field SERIAL_TIMESYNC_OFFSET;
  sla_ral_field SERIAL_CTRL_WORD;

  // --------------------------
  `ovm_object_utils(pmu_mmr_TS_SERIAL_CTL_reg)

  // --------------------------
  function new(
                string         name = "",
                `ifdef SLA_RAL_COVERAGE
                sla_ral_coverage_t cov_t = COVERAGE_OFF,
                `endif
                int            bus_num=0,
                int            dev_num=0,
                int            func_num=0,
                int            offset=0,
                int            size=0,
                sla_ral_data_t reset_val=0,
                boolean_t      mon_enabled = SLA_FALSE
             );
      super.new();
      set_space_addr("CFG",undef);
      set_space_addr("MEM",undef);
      set_space_addr("IO",undef);
      set_space_addr("MSG",undef);
      set_space_addr("CREG",undef); 
      set_space_addr("LT_MEM",undef);    
 build();

      `ifdef SLA_RAL_COVERAGE
      `CONFIG_RAL_COVERAGE(regname, COVERAGE_ON, FD_RW_SAMPLE)
      if(_coverage_type != COVERAGE_OFF) begin
        if(_sample_type inside {FD_RO_SAMPLE, RO_SAMPLE})
        `CREATE_OP_SAMPLE_COV(RAL_WRITE)
        else if(_sample_type inside {FD_WO_SAMPLE, WO_SAMPLE})
        `CREATE_OP_SAMPLE_COV(RAL_READ)
        else
        `CREATE_OP_SAMPLE_COV(RAL_NONE)
        `CREATE_DESIRED_COV
        `CREATE_ACTUAL_COV
      end
      `endif

  endfunction

  `ifdef SLA_RAL_COVERAGE
  `CREATE_RAL_OP_COVERGROUP
  covergroup desired_cg @(cov_ev);
     option.per_instance = 1;
     option.name = {sla_ral_file::regfilename,".",regname};
     `RAL_FIELD_CP(SERIAL_TIMESYNC_START, SERIAL_TIMESYNC_START.desired)
     `RAL_FIELD_CP_1(SERIAL_TIMESYNC_START, SERIAL_TIMESYNC_START.desired, 0)
     `RAL_FIELD_CP(RSVD_7_1, RSVD_7_1.desired)
     `RAL_FIELD_CP_7(RSVD_7_1, RSVD_7_1.desired, 0,1,2,3,4,5,6)
     `RAL_FIELD_CP(SERIAL_TIMESYNC_OFFSET, SERIAL_TIMESYNC_OFFSET.desired)
     `RAL_FIELD_CP_8(SERIAL_TIMESYNC_OFFSET, SERIAL_TIMESYNC_OFFSET.desired, 0,1,2,3,4,5,6,7)
     `RAL_FIELD_CP(SERIAL_CTRL_WORD, SERIAL_CTRL_WORD.desired)
     `RAL_FIELD_CP_16(SERIAL_CTRL_WORD, SERIAL_CTRL_WORD.desired, 0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)
  endgroup
  covergroup actual_cg @(cov_ev);
     option.per_instance = 1;
     option.name = {sla_ral_file::regfilename,".",regname};
     `RAL_FIELD_CP(SERIAL_TIMESYNC_START, SERIAL_TIMESYNC_START.actual)
     `RAL_FIELD_CP_1(SERIAL_TIMESYNC_START, SERIAL_TIMESYNC_START.actual, 0)
     `RAL_FIELD_CP(RSVD_7_1, RSVD_7_1.actual)
     `RAL_FIELD_CP_7(RSVD_7_1, RSVD_7_1.actual, 0,1,2,3,4,5,6)
     `RAL_FIELD_CP(SERIAL_TIMESYNC_OFFSET, SERIAL_TIMESYNC_OFFSET.actual)
     `RAL_FIELD_CP_8(SERIAL_TIMESYNC_OFFSET, SERIAL_TIMESYNC_OFFSET.actual, 0,1,2,3,4,5,6,7)
     `RAL_FIELD_CP(SERIAL_CTRL_WORD, SERIAL_CTRL_WORD.actual)
     `RAL_FIELD_CP_16(SERIAL_CTRL_WORD, SERIAL_CTRL_WORD.actual, 0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)
  endgroup
  `endif

  // --------------------------
  // add constraint based on database legal values
  // --------------------------
  function void build();
    SERIAL_TIMESYNC_START = new("SERIAL_TIMESYNC_START", "WO", 1, 0, {"TS_SERIAL_CTL.SERIAL_TIMESYNC_START"});
    SERIAL_TIMESYNC_START.set_powerwell("primary");
    SERIAL_TIMESYNC_START.set_rand_mode(0);
   SERIAL_TIMESYNC_START.set_reset_signame("pmu_rst_b");
    void'(add_field( SERIAL_TIMESYNC_START ));

    RSVD_7_1 = new("RSVD_7_1", "RO", 7, 1, {"TS_SERIAL_CTL.RSVD_7_1"});
    RSVD_7_1.set_powerwell("primary");
    RSVD_7_1.set_rand_mode(0);
   RSVD_7_1.set_reset_signame("pmu_rst_b");
    void'(add_field( RSVD_7_1 ));

    SERIAL_TIMESYNC_OFFSET = new("SERIAL_TIMESYNC_OFFSET", "RW", 8, 8, {"TS_SERIAL_CTL.SERIAL_TIMESYNC_OFFSET"});
    SERIAL_TIMESYNC_OFFSET.set_powerwell("primary");
    SERIAL_TIMESYNC_OFFSET.set_rand_mode(0);
   SERIAL_TIMESYNC_OFFSET.set_reset_signame("pmu_rst_b");
    void'(add_field( SERIAL_TIMESYNC_OFFSET ));

    SERIAL_CTRL_WORD = new("SERIAL_CTRL_WORD", "RW", 16, 16, {"TS_SERIAL_CTL.SERIAL_CTRL_WORD"});
    SERIAL_CTRL_WORD.set_powerwell("primary");
    SERIAL_CTRL_WORD.set_rand_mode(0);
   SERIAL_CTRL_WORD.set_reset_signame("pmu_rst_b");
    void'(add_field( SERIAL_CTRL_WORD ));

  endfunction


`DEFINE_NONE_SCOPE_REG_API(pmu_mmr_TS_SERIAL_CTL_reg) 
endclass : pmu_mmr_TS_SERIAL_CTL_reg

// ================================================

class pmu_mmr_PM_CORE_VID_reg extends sla_ral_reg;

  // --------------------------
  sla_ral_field CORE_VID_STS;
  sla_ral_field CORE_VID_TGT;
  sla_ral_field RSVD_6_4;
  sla_ral_field UPDATE_CORE_VID;

  // --------------------------
  `ovm_object_utils(pmu_mmr_PM_CORE_VID_reg)

  // --------------------------
  function new(
                string         name = "",
                `ifdef SLA_RAL_COVERAGE
                sla_ral_coverage_t cov_t = COVERAGE_OFF,
                `endif
                int            bus_num=0,
                int            dev_num=0,
                int            func_num=0,
                int            offset=0,
                int            size=0,
                sla_ral_data_t reset_val=0,
                boolean_t      mon_enabled = SLA_FALSE
             );
      super.new();
      set_space_addr("CFG",undef);
      set_space_addr("MEM",undef);
      set_space_addr("IO",undef);
      set_space_addr("MSG",undef);
      set_space_addr("CREG",undef); 
      set_space_addr("LT_MEM",undef);    
 build();

      `ifdef SLA_RAL_COVERAGE
      `CONFIG_RAL_COVERAGE(regname, COVERAGE_ON, FD_RW_SAMPLE)
      if(_coverage_type != COVERAGE_OFF) begin
        if(_sample_type inside {FD_RO_SAMPLE, RO_SAMPLE})
        `CREATE_OP_SAMPLE_COV(RAL_WRITE)
        else if(_sample_type inside {FD_WO_SAMPLE, WO_SAMPLE})
        `CREATE_OP_SAMPLE_COV(RAL_READ)
        else
        `CREATE_OP_SAMPLE_COV(RAL_NONE)
        `CREATE_DESIRED_COV
        `CREATE_ACTUAL_COV
      end
      `endif

  endfunction

  `ifdef SLA_RAL_COVERAGE
  `CREATE_RAL_OP_COVERGROUP
  covergroup desired_cg @(cov_ev);
     option.per_instance = 1;
     option.name = {sla_ral_file::regfilename,".",regname};
     `RAL_FIELD_CP(CORE_VID_STS, CORE_VID_STS.desired)
     `RAL_FIELD_CP_2(CORE_VID_STS, CORE_VID_STS.desired, 0,1)
     `RAL_FIELD_CP(CORE_VID_TGT, CORE_VID_TGT.desired)
     `RAL_FIELD_CP_2(CORE_VID_TGT, CORE_VID_TGT.desired, 0,1)
     `RAL_FIELD_CP(RSVD_6_4, RSVD_6_4.desired)
     `RAL_FIELD_CP_3(RSVD_6_4, RSVD_6_4.desired, 0,1,2)
     `RAL_FIELD_CP(UPDATE_CORE_VID, UPDATE_CORE_VID.desired)
     `RAL_FIELD_CP_1(UPDATE_CORE_VID, UPDATE_CORE_VID.desired, 0)
  endgroup
  covergroup actual_cg @(cov_ev);
     option.per_instance = 1;
     option.name = {sla_ral_file::regfilename,".",regname};
     `RAL_FIELD_CP(CORE_VID_STS, CORE_VID_STS.actual)
     `RAL_FIELD_CP_2(CORE_VID_STS, CORE_VID_STS.actual, 0,1)
     `RAL_FIELD_CP(CORE_VID_TGT, CORE_VID_TGT.actual)
     `RAL_FIELD_CP_2(CORE_VID_TGT, CORE_VID_TGT.actual, 0,1)
     `RAL_FIELD_CP(RSVD_6_4, RSVD_6_4.actual)
     `RAL_FIELD_CP_3(RSVD_6_4, RSVD_6_4.actual, 0,1,2)
     `RAL_FIELD_CP(UPDATE_CORE_VID, UPDATE_CORE_VID.actual)
     `RAL_FIELD_CP_1(UPDATE_CORE_VID, UPDATE_CORE_VID.actual, 0)
  endgroup
  `endif

  // --------------------------
  // add constraint based on database legal values
  // --------------------------
  function void build();
    CORE_VID_STS = new("CORE_VID_STS", "RO/V", 2, 0, {"PM_CORE_VID.CORE_VID_STS"});
    CORE_VID_STS.set_powerwell("primary");
    CORE_VID_STS.set_rand_mode(0);
    void'(add_field( CORE_VID_STS ));

    CORE_VID_TGT = new("CORE_VID_TGT", "RW", 2, 2, {"PM_CORE_VID.CORE_VID_TGT"});
    CORE_VID_TGT.set_powerwell("primary");
    CORE_VID_TGT.set_rand_mode(0);
   CORE_VID_TGT.set_reset_signame("pmu_rst_b");
    void'(add_field( CORE_VID_TGT ));

    RSVD_6_4 = new("RSVD_6_4", "RO", 3, 4, {"PM_CORE_VID.RSVD_6_4"});
    RSVD_6_4.set_powerwell("primary");
    RSVD_6_4.set_rand_mode(0);
    void'(add_field( RSVD_6_4 ));

    UPDATE_CORE_VID = new("UPDATE_CORE_VID", "RW", 1, 7, {"PM_CORE_VID.UPDATE_CORE_VID"});
    UPDATE_CORE_VID.set_powerwell("primary");
    UPDATE_CORE_VID.set_rand_mode(0);
   UPDATE_CORE_VID.set_reset_signame("pmu_rst_b");
    void'(add_field( UPDATE_CORE_VID ));

  endfunction


`DEFINE_NONE_SCOPE_REG_API(pmu_mmr_PM_CORE_VID_reg) 
endclass : pmu_mmr_PM_CORE_VID_reg

// ================================================

class pmu_mmr_DTS0_TEMP_reg extends sla_ral_reg;

  // --------------------------
  sla_ral_field TEMP0;
  sla_ral_field TEMP1;
  sla_ral_field TEMP2;
  sla_ral_field RSVD_28_27;
  sla_ral_field VALID2;
  sla_ral_field VALID1;
  sla_ral_field VALID0;

  // --------------------------
  `ovm_object_utils(pmu_mmr_DTS0_TEMP_reg)

  // --------------------------
  function new(
                string         name = "",
                `ifdef SLA_RAL_COVERAGE
                sla_ral_coverage_t cov_t = COVERAGE_OFF,
                `endif
                int            bus_num=0,
                int            dev_num=0,
                int            func_num=0,
                int            offset=0,
                int            size=0,
                sla_ral_data_t reset_val=0,
                boolean_t      mon_enabled = SLA_FALSE
             );
      super.new();
      set_space_addr("CFG",undef);
      set_space_addr("MEM",undef);
      set_space_addr("IO",undef);
      set_space_addr("MSG",undef);
      set_space_addr("CREG",undef); 
      set_space_addr("LT_MEM",undef);    
 build();

      `ifdef SLA_RAL_COVERAGE
      `CONFIG_RAL_COVERAGE(regname, COVERAGE_ON, FD_RW_SAMPLE)
      if(_coverage_type != COVERAGE_OFF) begin
        if(_sample_type inside {FD_RO_SAMPLE, RO_SAMPLE})
        `CREATE_OP_SAMPLE_COV(RAL_WRITE)
        else if(_sample_type inside {FD_WO_SAMPLE, WO_SAMPLE})
        `CREATE_OP_SAMPLE_COV(RAL_READ)
        else
        `CREATE_OP_SAMPLE_COV(RAL_NONE)
        `CREATE_DESIRED_COV
        `CREATE_ACTUAL_COV
      end
      `endif

  endfunction

  `ifdef SLA_RAL_COVERAGE
  `CREATE_RAL_OP_COVERGROUP
  covergroup desired_cg @(cov_ev);
     option.per_instance = 1;
     option.name = {sla_ral_file::regfilename,".",regname};
     `RAL_FIELD_CP(TEMP0, TEMP0.desired)
     `RAL_FIELD_CP_9(TEMP0, TEMP0.desired, 0,1,2,3,4,5,6,7,8)
     `RAL_FIELD_CP(TEMP1, TEMP1.desired)
     `RAL_FIELD_CP_9(TEMP1, TEMP1.desired, 0,1,2,3,4,5,6,7,8)
     `RAL_FIELD_CP(TEMP2, TEMP2.desired)
     `RAL_FIELD_CP_9(TEMP2, TEMP2.desired, 0,1,2,3,4,5,6,7,8)
     `RAL_FIELD_CP(RSVD_28_27, RSVD_28_27.desired)
     `RAL_FIELD_CP_2(RSVD_28_27, RSVD_28_27.desired, 0,1)
     `RAL_FIELD_CP(VALID2, VALID2.desired)
     `RAL_FIELD_CP_1(VALID2, VALID2.desired, 0)
     `RAL_FIELD_CP(VALID1, VALID1.desired)
     `RAL_FIELD_CP_1(VALID1, VALID1.desired, 0)
     `RAL_FIELD_CP(VALID0, VALID0.desired)
     `RAL_FIELD_CP_1(VALID0, VALID0.desired, 0)
  endgroup
  covergroup actual_cg @(cov_ev);
     option.per_instance = 1;
     option.name = {sla_ral_file::regfilename,".",regname};
     `RAL_FIELD_CP(TEMP0, TEMP0.actual)
     `RAL_FIELD_CP_9(TEMP0, TEMP0.actual, 0,1,2,3,4,5,6,7,8)
     `RAL_FIELD_CP(TEMP1, TEMP1.actual)
     `RAL_FIELD_CP_9(TEMP1, TEMP1.actual, 0,1,2,3,4,5,6,7,8)
     `RAL_FIELD_CP(TEMP2, TEMP2.actual)
     `RAL_FIELD_CP_9(TEMP2, TEMP2.actual, 0,1,2,3,4,5,6,7,8)
     `RAL_FIELD_CP(RSVD_28_27, RSVD_28_27.actual)
     `RAL_FIELD_CP_2(RSVD_28_27, RSVD_28_27.actual, 0,1)
     `RAL_FIELD_CP(VALID2, VALID2.actual)
     `RAL_FIELD_CP_1(VALID2, VALID2.actual, 0)
     `RAL_FIELD_CP(VALID1, VALID1.actual)
     `RAL_FIELD_CP_1(VALID1, VALID1.actual, 0)
     `RAL_FIELD_CP(VALID0, VALID0.actual)
     `RAL_FIELD_CP_1(VALID0, VALID0.actual, 0)
  endgroup
  `endif

  // --------------------------
  // add constraint based on database legal values
  // --------------------------
  function void build();
    TEMP0 = new("TEMP0", "RO/V", 9, 0, {"DTS0_TEMP.TEMP0"});
    TEMP0.set_powerwell("primary");
    TEMP0.set_rand_mode(0);
   TEMP0.set_reset_signame("pmu_rst_b");
    void'(add_field( TEMP0 ));

    TEMP1 = new("TEMP1", "RO/V", 9, 9, {"DTS0_TEMP.TEMP1"});
    TEMP1.set_powerwell("primary");
    TEMP1.set_rand_mode(0);
   TEMP1.set_reset_signame("pmu_rst_b");
    void'(add_field( TEMP1 ));

    TEMP2 = new("TEMP2", "RO/V", 9, 18, {"DTS0_TEMP.TEMP2"});
    TEMP2.set_powerwell("primary");
    TEMP2.set_rand_mode(0);
   TEMP2.set_reset_signame("pmu_rst_b");
    void'(add_field( TEMP2 ));

    RSVD_28_27 = new("RSVD_28_27", "RO", 2, 27, {"DTS0_TEMP.RSVD_28_27"});
    RSVD_28_27.set_powerwell("primary");
    RSVD_28_27.set_rand_mode(0);
    void'(add_field( RSVD_28_27 ));

    VALID2 = new("VALID2", "RO/V", 1, 29, {"DTS0_TEMP.VALID2"});
    VALID2.set_powerwell("primary");
    VALID2.set_rand_mode(0);
   VALID2.set_reset_signame("pmu_rst_b");
    void'(add_field( VALID2 ));

    VALID1 = new("VALID1", "RO/V", 1, 30, {"DTS0_TEMP.VALID1"});
    VALID1.set_powerwell("primary");
    VALID1.set_rand_mode(0);
   VALID1.set_reset_signame("pmu_rst_b");
    void'(add_field( VALID1 ));

    VALID0 = new("VALID0", "RO/V", 1, 31, {"DTS0_TEMP.VALID0"});
    VALID0.set_powerwell("primary");
    VALID0.set_rand_mode(0);
   VALID0.set_reset_signame("pmu_rst_b");
    void'(add_field( VALID0 ));

  endfunction


`DEFINE_NONE_SCOPE_REG_API(pmu_mmr_DTS0_TEMP_reg) 
endclass : pmu_mmr_DTS0_TEMP_reg

// ================================================

class pmu_mmr_DTS1_TEMP_reg extends sla_ral_reg;

  // --------------------------
  sla_ral_field TEMP0;
  sla_ral_field TEMP1;
  sla_ral_field TEMP2;
  sla_ral_field RSVD_28_27;
  sla_ral_field VALID2;
  sla_ral_field VALID1;
  sla_ral_field VALID0;

  // --------------------------
  `ovm_object_utils(pmu_mmr_DTS1_TEMP_reg)

  // --------------------------
  function new(
                string         name = "",
                `ifdef SLA_RAL_COVERAGE
                sla_ral_coverage_t cov_t = COVERAGE_OFF,
                `endif
                int            bus_num=0,
                int            dev_num=0,
                int            func_num=0,
                int            offset=0,
                int            size=0,
                sla_ral_data_t reset_val=0,
                boolean_t      mon_enabled = SLA_FALSE
             );
      super.new();
      set_space_addr("CFG",undef);
      set_space_addr("MEM",undef);
      set_space_addr("IO",undef);
      set_space_addr("MSG",undef);
      set_space_addr("CREG",undef); 
      set_space_addr("LT_MEM",undef);    
 build();

      `ifdef SLA_RAL_COVERAGE
      `CONFIG_RAL_COVERAGE(regname, COVERAGE_ON, FD_RW_SAMPLE)
      if(_coverage_type != COVERAGE_OFF) begin
        if(_sample_type inside {FD_RO_SAMPLE, RO_SAMPLE})
        `CREATE_OP_SAMPLE_COV(RAL_WRITE)
        else if(_sample_type inside {FD_WO_SAMPLE, WO_SAMPLE})
        `CREATE_OP_SAMPLE_COV(RAL_READ)
        else
        `CREATE_OP_SAMPLE_COV(RAL_NONE)
        `CREATE_DESIRED_COV
        `CREATE_ACTUAL_COV
      end
      `endif

  endfunction

  `ifdef SLA_RAL_COVERAGE
  `CREATE_RAL_OP_COVERGROUP
  covergroup desired_cg @(cov_ev);
     option.per_instance = 1;
     option.name = {sla_ral_file::regfilename,".",regname};
     `RAL_FIELD_CP(TEMP0, TEMP0.desired)
     `RAL_FIELD_CP_9(TEMP0, TEMP0.desired, 0,1,2,3,4,5,6,7,8)
     `RAL_FIELD_CP(TEMP1, TEMP1.desired)
     `RAL_FIELD_CP_9(TEMP1, TEMP1.desired, 0,1,2,3,4,5,6,7,8)
     `RAL_FIELD_CP(TEMP2, TEMP2.desired)
     `RAL_FIELD_CP_9(TEMP2, TEMP2.desired, 0,1,2,3,4,5,6,7,8)
     `RAL_FIELD_CP(RSVD_28_27, RSVD_28_27.desired)
     `RAL_FIELD_CP_2(RSVD_28_27, RSVD_28_27.desired, 0,1)
     `RAL_FIELD_CP(VALID2, VALID2.desired)
     `RAL_FIELD_CP_1(VALID2, VALID2.desired, 0)
     `RAL_FIELD_CP(VALID1, VALID1.desired)
     `RAL_FIELD_CP_1(VALID1, VALID1.desired, 0)
     `RAL_FIELD_CP(VALID0, VALID0.desired)
     `RAL_FIELD_CP_1(VALID0, VALID0.desired, 0)
  endgroup
  covergroup actual_cg @(cov_ev);
     option.per_instance = 1;
     option.name = {sla_ral_file::regfilename,".",regname};
     `RAL_FIELD_CP(TEMP0, TEMP0.actual)
     `RAL_FIELD_CP_9(TEMP0, TEMP0.actual, 0,1,2,3,4,5,6,7,8)
     `RAL_FIELD_CP(TEMP1, TEMP1.actual)
     `RAL_FIELD_CP_9(TEMP1, TEMP1.actual, 0,1,2,3,4,5,6,7,8)
     `RAL_FIELD_CP(TEMP2, TEMP2.actual)
     `RAL_FIELD_CP_9(TEMP2, TEMP2.actual, 0,1,2,3,4,5,6,7,8)
     `RAL_FIELD_CP(RSVD_28_27, RSVD_28_27.actual)
     `RAL_FIELD_CP_2(RSVD_28_27, RSVD_28_27.actual, 0,1)
     `RAL_FIELD_CP(VALID2, VALID2.actual)
     `RAL_FIELD_CP_1(VALID2, VALID2.actual, 0)
     `RAL_FIELD_CP(VALID1, VALID1.actual)
     `RAL_FIELD_CP_1(VALID1, VALID1.actual, 0)
     `RAL_FIELD_CP(VALID0, VALID0.actual)
     `RAL_FIELD_CP_1(VALID0, VALID0.actual, 0)
  endgroup
  `endif

  // --------------------------
  // add constraint based on database legal values
  // --------------------------
  function void build();
    TEMP0 = new("TEMP0", "RO/V", 9, 0, {"DTS1_TEMP.TEMP0"});
    TEMP0.set_powerwell("primary");
    TEMP0.set_rand_mode(0);
   TEMP0.set_reset_signame("pmu_rst_b");
    void'(add_field( TEMP0 ));

    TEMP1 = new("TEMP1", "RO/V", 9, 9, {"DTS1_TEMP.TEMP1"});
    TEMP1.set_powerwell("primary");
    TEMP1.set_rand_mode(0);
   TEMP1.set_reset_signame("pmu_rst_b");
    void'(add_field( TEMP1 ));

    TEMP2 = new("TEMP2", "RO/V", 9, 18, {"DTS1_TEMP.TEMP2"});
    TEMP2.set_powerwell("primary");
    TEMP2.set_rand_mode(0);
   TEMP2.set_reset_signame("pmu_rst_b");
    void'(add_field( TEMP2 ));

    RSVD_28_27 = new("RSVD_28_27", "RO", 2, 27, {"DTS1_TEMP.RSVD_28_27"});
    RSVD_28_27.set_powerwell("primary");
    RSVD_28_27.set_rand_mode(0);
    void'(add_field( RSVD_28_27 ));

    VALID2 = new("VALID2", "RO/V", 1, 29, {"DTS1_TEMP.VALID2"});
    VALID2.set_powerwell("primary");
    VALID2.set_rand_mode(0);
   VALID2.set_reset_signame("pmu_rst_b");
    void'(add_field( VALID2 ));

    VALID1 = new("VALID1", "RO/V", 1, 30, {"DTS1_TEMP.VALID1"});
    VALID1.set_powerwell("primary");
    VALID1.set_rand_mode(0);
   VALID1.set_reset_signame("pmu_rst_b");
    void'(add_field( VALID1 ));

    VALID0 = new("VALID0", "RO/V", 1, 31, {"DTS1_TEMP.VALID0"});
    VALID0.set_powerwell("primary");
    VALID0.set_rand_mode(0);
   VALID0.set_reset_signame("pmu_rst_b");
    void'(add_field( VALID0 ));

  endfunction


`DEFINE_NONE_SCOPE_REG_API(pmu_mmr_DTS1_TEMP_reg) 
endclass : pmu_mmr_DTS1_TEMP_reg

// ================================================

class pmu_mmr_DTS2_TEMP_reg extends sla_ral_reg;

  // --------------------------
  sla_ral_field TEMP0;
  sla_ral_field TEMP1;
  sla_ral_field TEMP2;
  sla_ral_field RSVD_28_27;
  sla_ral_field VALID2;
  sla_ral_field VALID1;
  sla_ral_field VALID0;

  // --------------------------
  `ovm_object_utils(pmu_mmr_DTS2_TEMP_reg)

  // --------------------------
  function new(
                string         name = "",
                `ifdef SLA_RAL_COVERAGE
                sla_ral_coverage_t cov_t = COVERAGE_OFF,
                `endif
                int            bus_num=0,
                int            dev_num=0,
                int            func_num=0,
                int            offset=0,
                int            size=0,
                sla_ral_data_t reset_val=0,
                boolean_t      mon_enabled = SLA_FALSE
             );
      super.new();
      set_space_addr("CFG",undef);
      set_space_addr("MEM",undef);
      set_space_addr("IO",undef);
      set_space_addr("MSG",undef);
      set_space_addr("CREG",undef); 
      set_space_addr("LT_MEM",undef);    
 build();

      `ifdef SLA_RAL_COVERAGE
      `CONFIG_RAL_COVERAGE(regname, COVERAGE_ON, FD_RW_SAMPLE)
      if(_coverage_type != COVERAGE_OFF) begin
        if(_sample_type inside {FD_RO_SAMPLE, RO_SAMPLE})
        `CREATE_OP_SAMPLE_COV(RAL_WRITE)
        else if(_sample_type inside {FD_WO_SAMPLE, WO_SAMPLE})
        `CREATE_OP_SAMPLE_COV(RAL_READ)
        else
        `CREATE_OP_SAMPLE_COV(RAL_NONE)
        `CREATE_DESIRED_COV
        `CREATE_ACTUAL_COV
      end
      `endif

  endfunction

  `ifdef SLA_RAL_COVERAGE
  `CREATE_RAL_OP_COVERGROUP
  covergroup desired_cg @(cov_ev);
     option.per_instance = 1;
     option.name = {sla_ral_file::regfilename,".",regname};
     `RAL_FIELD_CP(TEMP0, TEMP0.desired)
     `RAL_FIELD_CP_9(TEMP0, TEMP0.desired, 0,1,2,3,4,5,6,7,8)
     `RAL_FIELD_CP(TEMP1, TEMP1.desired)
     `RAL_FIELD_CP_9(TEMP1, TEMP1.desired, 0,1,2,3,4,5,6,7,8)
     `RAL_FIELD_CP(TEMP2, TEMP2.desired)
     `RAL_FIELD_CP_9(TEMP2, TEMP2.desired, 0,1,2,3,4,5,6,7,8)
     `RAL_FIELD_CP(RSVD_28_27, RSVD_28_27.desired)
     `RAL_FIELD_CP_2(RSVD_28_27, RSVD_28_27.desired, 0,1)
     `RAL_FIELD_CP(VALID2, VALID2.desired)
     `RAL_FIELD_CP_1(VALID2, VALID2.desired, 0)
     `RAL_FIELD_CP(VALID1, VALID1.desired)
     `RAL_FIELD_CP_1(VALID1, VALID1.desired, 0)
     `RAL_FIELD_CP(VALID0, VALID0.desired)
     `RAL_FIELD_CP_1(VALID0, VALID0.desired, 0)
  endgroup
  covergroup actual_cg @(cov_ev);
     option.per_instance = 1;
     option.name = {sla_ral_file::regfilename,".",regname};
     `RAL_FIELD_CP(TEMP0, TEMP0.actual)
     `RAL_FIELD_CP_9(TEMP0, TEMP0.actual, 0,1,2,3,4,5,6,7,8)
     `RAL_FIELD_CP(TEMP1, TEMP1.actual)
     `RAL_FIELD_CP_9(TEMP1, TEMP1.actual, 0,1,2,3,4,5,6,7,8)
     `RAL_FIELD_CP(TEMP2, TEMP2.actual)
     `RAL_FIELD_CP_9(TEMP2, TEMP2.actual, 0,1,2,3,4,5,6,7,8)
     `RAL_FIELD_CP(RSVD_28_27, RSVD_28_27.actual)
     `RAL_FIELD_CP_2(RSVD_28_27, RSVD_28_27.actual, 0,1)
     `RAL_FIELD_CP(VALID2, VALID2.actual)
     `RAL_FIELD_CP_1(VALID2, VALID2.actual, 0)
     `RAL_FIELD_CP(VALID1, VALID1.actual)
     `RAL_FIELD_CP_1(VALID1, VALID1.actual, 0)
     `RAL_FIELD_CP(VALID0, VALID0.actual)
     `RAL_FIELD_CP_1(VALID0, VALID0.actual, 0)
  endgroup
  `endif

  // --------------------------
  // add constraint based on database legal values
  // --------------------------
  function void build();
    TEMP0 = new("TEMP0", "RO/V", 9, 0, {"DTS2_TEMP.TEMP0"});
    TEMP0.set_powerwell("primary");
    TEMP0.set_rand_mode(0);
   TEMP0.set_reset_signame("pmu_rst_b");
    void'(add_field( TEMP0 ));

    TEMP1 = new("TEMP1", "RO/V", 9, 9, {"DTS2_TEMP.TEMP1"});
    TEMP1.set_powerwell("primary");
    TEMP1.set_rand_mode(0);
   TEMP1.set_reset_signame("pmu_rst_b");
    void'(add_field( TEMP1 ));

    TEMP2 = new("TEMP2", "RO/V", 9, 18, {"DTS2_TEMP.TEMP2"});
    TEMP2.set_powerwell("primary");
    TEMP2.set_rand_mode(0);
   TEMP2.set_reset_signame("pmu_rst_b");
    void'(add_field( TEMP2 ));

    RSVD_28_27 = new("RSVD_28_27", "RO", 2, 27, {"DTS2_TEMP.RSVD_28_27"});
    RSVD_28_27.set_powerwell("primary");
    RSVD_28_27.set_rand_mode(0);
    void'(add_field( RSVD_28_27 ));

    VALID2 = new("VALID2", "RO/V", 1, 29, {"DTS2_TEMP.VALID2"});
    VALID2.set_powerwell("primary");
    VALID2.set_rand_mode(0);
   VALID2.set_reset_signame("pmu_rst_b");
    void'(add_field( VALID2 ));

    VALID1 = new("VALID1", "RO/V", 1, 30, {"DTS2_TEMP.VALID1"});
    VALID1.set_powerwell("primary");
    VALID1.set_rand_mode(0);
   VALID1.set_reset_signame("pmu_rst_b");
    void'(add_field( VALID1 ));

    VALID0 = new("VALID0", "RO/V", 1, 31, {"DTS2_TEMP.VALID0"});
    VALID0.set_powerwell("primary");
    VALID0.set_rand_mode(0);
   VALID0.set_reset_signame("pmu_rst_b");
    void'(add_field( VALID0 ));

  endfunction


`DEFINE_NONE_SCOPE_REG_API(pmu_mmr_DTS2_TEMP_reg) 
endclass : pmu_mmr_DTS2_TEMP_reg

// ================================================

class pmu_mmr_SEQENG_STS_reg extends sla_ral_reg;

  // --------------------------
  sla_ral_field BOOT_START;
  sla_ral_field TRIG_PEND1;
  sla_ral_field TRIG_PEND2;
  sla_ral_field TRIG_PEND3;
  sla_ral_field TRIG_PEND4;
  sla_ral_field TRIG_PEND5;
  sla_ral_field TRIG_PEND6;
  sla_ral_field TRIG_PEND7;
  sla_ral_field TRIG_PEND8;
  sla_ral_field TRIG_PEND9;
  sla_ral_field TRIG_PEND10;
  sla_ral_field TRIG_PEND11;
  sla_ral_field TRIG_PEND12;
  sla_ral_field TRIG_PEND13;
  sla_ral_field TRIG_PEND14;
  sla_ral_field PMETO_PEND;
  sla_ral_field BOOT_DONE;
  sla_ral_field TRIG_DONE1;
  sla_ral_field TRIG_DONE2;
  sla_ral_field TRIG_DONE3;
  sla_ral_field TRIG_DONE4;
  sla_ral_field TRIG_DONE5;
  sla_ral_field TRIG_DONE6;
  sla_ral_field TRIG_DONE7;
  sla_ral_field TRIG_DONE8;
  sla_ral_field TRIG_DONE9;
  sla_ral_field TRIG_DONE10;
  sla_ral_field TRIG_DONE11;
  sla_ral_field TRIG_DONE12;
  sla_ral_field TRIG_DONE13;
  sla_ral_field TRIG_DONE14;
  sla_ral_field PMETO_DONE;

  // --------------------------
  `ovm_object_utils(pmu_mmr_SEQENG_STS_reg)

  // --------------------------
  function new(
                string         name = "",
                `ifdef SLA_RAL_COVERAGE
                sla_ral_coverage_t cov_t = COVERAGE_OFF,
                `endif
                int            bus_num=0,
                int            dev_num=0,
                int            func_num=0,
                int            offset=0,
                int            size=0,
                sla_ral_data_t reset_val=0,
                boolean_t      mon_enabled = SLA_FALSE
             );
      super.new();
      set_space_addr("CFG",undef);
      set_space_addr("MEM",undef);
      set_space_addr("IO",undef);
      set_space_addr("MSG",undef);
      set_space_addr("CREG",undef); 
      set_space_addr("LT_MEM",undef);    
 build();

      `ifdef SLA_RAL_COVERAGE
      `CONFIG_RAL_COVERAGE(regname, COVERAGE_ON, FD_RW_SAMPLE)
      if(_coverage_type != COVERAGE_OFF) begin
        if(_sample_type inside {FD_RO_SAMPLE, RO_SAMPLE})
        `CREATE_OP_SAMPLE_COV(RAL_WRITE)
        else if(_sample_type inside {FD_WO_SAMPLE, WO_SAMPLE})
        `CREATE_OP_SAMPLE_COV(RAL_READ)
        else
        `CREATE_OP_SAMPLE_COV(RAL_NONE)
        `CREATE_DESIRED_COV
        `CREATE_ACTUAL_COV
      end
      `endif

  endfunction

  `ifdef SLA_RAL_COVERAGE
  `CREATE_RAL_OP_COVERGROUP
  covergroup desired_cg @(cov_ev);
     option.per_instance = 1;
     option.name = {sla_ral_file::regfilename,".",regname};
     `RAL_FIELD_CP(BOOT_START, BOOT_START.desired)
     `RAL_FIELD_CP_1(BOOT_START, BOOT_START.desired, 0)
     `RAL_FIELD_CP(TRIG_PEND1, TRIG_PEND1.desired)
     `RAL_FIELD_CP_1(TRIG_PEND1, TRIG_PEND1.desired, 0)
     `RAL_FIELD_CP(TRIG_PEND2, TRIG_PEND2.desired)
     `RAL_FIELD_CP_1(TRIG_PEND2, TRIG_PEND2.desired, 0)
     `RAL_FIELD_CP(TRIG_PEND3, TRIG_PEND3.desired)
     `RAL_FIELD_CP_1(TRIG_PEND3, TRIG_PEND3.desired, 0)
     `RAL_FIELD_CP(TRIG_PEND4, TRIG_PEND4.desired)
     `RAL_FIELD_CP_1(TRIG_PEND4, TRIG_PEND4.desired, 0)
     `RAL_FIELD_CP(TRIG_PEND5, TRIG_PEND5.desired)
     `RAL_FIELD_CP_1(TRIG_PEND5, TRIG_PEND5.desired, 0)
     `RAL_FIELD_CP(TRIG_PEND6, TRIG_PEND6.desired)
     `RAL_FIELD_CP_1(TRIG_PEND6, TRIG_PEND6.desired, 0)
     `RAL_FIELD_CP(TRIG_PEND7, TRIG_PEND7.desired)
     `RAL_FIELD_CP_1(TRIG_PEND7, TRIG_PEND7.desired, 0)
     `RAL_FIELD_CP(TRIG_PEND8, TRIG_PEND8.desired)
     `RAL_FIELD_CP_1(TRIG_PEND8, TRIG_PEND8.desired, 0)
     `RAL_FIELD_CP(TRIG_PEND9, TRIG_PEND9.desired)
     `RAL_FIELD_CP_1(TRIG_PEND9, TRIG_PEND9.desired, 0)
     `RAL_FIELD_CP(TRIG_PEND10, TRIG_PEND10.desired)
     `RAL_FIELD_CP_1(TRIG_PEND10, TRIG_PEND10.desired, 0)
     `RAL_FIELD_CP(TRIG_PEND11, TRIG_PEND11.desired)
     `RAL_FIELD_CP_1(TRIG_PEND11, TRIG_PEND11.desired, 0)
     `RAL_FIELD_CP(TRIG_PEND12, TRIG_PEND12.desired)
     `RAL_FIELD_CP_1(TRIG_PEND12, TRIG_PEND12.desired, 0)
     `RAL_FIELD_CP(TRIG_PEND13, TRIG_PEND13.desired)
     `RAL_FIELD_CP_1(TRIG_PEND13, TRIG_PEND13.desired, 0)
     `RAL_FIELD_CP(TRIG_PEND14, TRIG_PEND14.desired)
     `RAL_FIELD_CP_1(TRIG_PEND14, TRIG_PEND14.desired, 0)
     `RAL_FIELD_CP(PMETO_PEND, PMETO_PEND.desired)
     `RAL_FIELD_CP_1(PMETO_PEND, PMETO_PEND.desired, 0)
     `RAL_FIELD_CP(BOOT_DONE, BOOT_DONE.desired)
     `RAL_FIELD_CP_1(BOOT_DONE, BOOT_DONE.desired, 0)
     `RAL_FIELD_CP(TRIG_DONE1, TRIG_DONE1.desired)
     `RAL_FIELD_CP_1(TRIG_DONE1, TRIG_DONE1.desired, 0)
     `RAL_FIELD_CP(TRIG_DONE2, TRIG_DONE2.desired)
     `RAL_FIELD_CP_1(TRIG_DONE2, TRIG_DONE2.desired, 0)
     `RAL_FIELD_CP(TRIG_DONE3, TRIG_DONE3.desired)
     `RAL_FIELD_CP_1(TRIG_DONE3, TRIG_DONE3.desired, 0)
     `RAL_FIELD_CP(TRIG_DONE4, TRIG_DONE4.desired)
     `RAL_FIELD_CP_1(TRIG_DONE4, TRIG_DONE4.desired, 0)
     `RAL_FIELD_CP(TRIG_DONE5, TRIG_DONE5.desired)
     `RAL_FIELD_CP_1(TRIG_DONE5, TRIG_DONE5.desired, 0)
     `RAL_FIELD_CP(TRIG_DONE6, TRIG_DONE6.desired)
     `RAL_FIELD_CP_1(TRIG_DONE6, TRIG_DONE6.desired, 0)
     `RAL_FIELD_CP(TRIG_DONE7, TRIG_DONE7.desired)
     `RAL_FIELD_CP_1(TRIG_DONE7, TRIG_DONE7.desired, 0)
     `RAL_FIELD_CP(TRIG_DONE8, TRIG_DONE8.desired)
     `RAL_FIELD_CP_1(TRIG_DONE8, TRIG_DONE8.desired, 0)
     `RAL_FIELD_CP(TRIG_DONE9, TRIG_DONE9.desired)
     `RAL_FIELD_CP_1(TRIG_DONE9, TRIG_DONE9.desired, 0)
     `RAL_FIELD_CP(TRIG_DONE10, TRIG_DONE10.desired)
     `RAL_FIELD_CP_1(TRIG_DONE10, TRIG_DONE10.desired, 0)
     `RAL_FIELD_CP(TRIG_DONE11, TRIG_DONE11.desired)
     `RAL_FIELD_CP_1(TRIG_DONE11, TRIG_DONE11.desired, 0)
     `RAL_FIELD_CP(TRIG_DONE12, TRIG_DONE12.desired)
     `RAL_FIELD_CP_1(TRIG_DONE12, TRIG_DONE12.desired, 0)
     `RAL_FIELD_CP(TRIG_DONE13, TRIG_DONE13.desired)
     `RAL_FIELD_CP_1(TRIG_DONE13, TRIG_DONE13.desired, 0)
     `RAL_FIELD_CP(TRIG_DONE14, TRIG_DONE14.desired)
     `RAL_FIELD_CP_1(TRIG_DONE14, TRIG_DONE14.desired, 0)
     `RAL_FIELD_CP(PMETO_DONE, PMETO_DONE.desired)
     `RAL_FIELD_CP_1(PMETO_DONE, PMETO_DONE.desired, 0)
  endgroup
  covergroup actual_cg @(cov_ev);
     option.per_instance = 1;
     option.name = {sla_ral_file::regfilename,".",regname};
     `RAL_FIELD_CP(BOOT_START, BOOT_START.actual)
     `RAL_FIELD_CP_1(BOOT_START, BOOT_START.actual, 0)
     `RAL_FIELD_CP(TRIG_PEND1, TRIG_PEND1.actual)
     `RAL_FIELD_CP_1(TRIG_PEND1, TRIG_PEND1.actual, 0)
     `RAL_FIELD_CP(TRIG_PEND2, TRIG_PEND2.actual)
     `RAL_FIELD_CP_1(TRIG_PEND2, TRIG_PEND2.actual, 0)
     `RAL_FIELD_CP(TRIG_PEND3, TRIG_PEND3.actual)
     `RAL_FIELD_CP_1(TRIG_PEND3, TRIG_PEND3.actual, 0)
     `RAL_FIELD_CP(TRIG_PEND4, TRIG_PEND4.actual)
     `RAL_FIELD_CP_1(TRIG_PEND4, TRIG_PEND4.actual, 0)
     `RAL_FIELD_CP(TRIG_PEND5, TRIG_PEND5.actual)
     `RAL_FIELD_CP_1(TRIG_PEND5, TRIG_PEND5.actual, 0)
     `RAL_FIELD_CP(TRIG_PEND6, TRIG_PEND6.actual)
     `RAL_FIELD_CP_1(TRIG_PEND6, TRIG_PEND6.actual, 0)
     `RAL_FIELD_CP(TRIG_PEND7, TRIG_PEND7.actual)
     `RAL_FIELD_CP_1(TRIG_PEND7, TRIG_PEND7.actual, 0)
     `RAL_FIELD_CP(TRIG_PEND8, TRIG_PEND8.actual)
     `RAL_FIELD_CP_1(TRIG_PEND8, TRIG_PEND8.actual, 0)
     `RAL_FIELD_CP(TRIG_PEND9, TRIG_PEND9.actual)
     `RAL_FIELD_CP_1(TRIG_PEND9, TRIG_PEND9.actual, 0)
     `RAL_FIELD_CP(TRIG_PEND10, TRIG_PEND10.actual)
     `RAL_FIELD_CP_1(TRIG_PEND10, TRIG_PEND10.actual, 0)
     `RAL_FIELD_CP(TRIG_PEND11, TRIG_PEND11.actual)
     `RAL_FIELD_CP_1(TRIG_PEND11, TRIG_PEND11.actual, 0)
     `RAL_FIELD_CP(TRIG_PEND12, TRIG_PEND12.actual)
     `RAL_FIELD_CP_1(TRIG_PEND12, TRIG_PEND12.actual, 0)
     `RAL_FIELD_CP(TRIG_PEND13, TRIG_PEND13.actual)
     `RAL_FIELD_CP_1(TRIG_PEND13, TRIG_PEND13.actual, 0)
     `RAL_FIELD_CP(TRIG_PEND14, TRIG_PEND14.actual)
     `RAL_FIELD_CP_1(TRIG_PEND14, TRIG_PEND14.actual, 0)
     `RAL_FIELD_CP(PMETO_PEND, PMETO_PEND.actual)
     `RAL_FIELD_CP_1(PMETO_PEND, PMETO_PEND.actual, 0)
     `RAL_FIELD_CP(BOOT_DONE, BOOT_DONE.actual)
     `RAL_FIELD_CP_1(BOOT_DONE, BOOT_DONE.actual, 0)
     `RAL_FIELD_CP(TRIG_DONE1, TRIG_DONE1.actual)
     `RAL_FIELD_CP_1(TRIG_DONE1, TRIG_DONE1.actual, 0)
     `RAL_FIELD_CP(TRIG_DONE2, TRIG_DONE2.actual)
     `RAL_FIELD_CP_1(TRIG_DONE2, TRIG_DONE2.actual, 0)
     `RAL_FIELD_CP(TRIG_DONE3, TRIG_DONE3.actual)
     `RAL_FIELD_CP_1(TRIG_DONE3, TRIG_DONE3.actual, 0)
     `RAL_FIELD_CP(TRIG_DONE4, TRIG_DONE4.actual)
     `RAL_FIELD_CP_1(TRIG_DONE4, TRIG_DONE4.actual, 0)
     `RAL_FIELD_CP(TRIG_DONE5, TRIG_DONE5.actual)
     `RAL_FIELD_CP_1(TRIG_DONE5, TRIG_DONE5.actual, 0)
     `RAL_FIELD_CP(TRIG_DONE6, TRIG_DONE6.actual)
     `RAL_FIELD_CP_1(TRIG_DONE6, TRIG_DONE6.actual, 0)
     `RAL_FIELD_CP(TRIG_DONE7, TRIG_DONE7.actual)
     `RAL_FIELD_CP_1(TRIG_DONE7, TRIG_DONE7.actual, 0)
     `RAL_FIELD_CP(TRIG_DONE8, TRIG_DONE8.actual)
     `RAL_FIELD_CP_1(TRIG_DONE8, TRIG_DONE8.actual, 0)
     `RAL_FIELD_CP(TRIG_DONE9, TRIG_DONE9.actual)
     `RAL_FIELD_CP_1(TRIG_DONE9, TRIG_DONE9.actual, 0)
     `RAL_FIELD_CP(TRIG_DONE10, TRIG_DONE10.actual)
     `RAL_FIELD_CP_1(TRIG_DONE10, TRIG_DONE10.actual, 0)
     `RAL_FIELD_CP(TRIG_DONE11, TRIG_DONE11.actual)
     `RAL_FIELD_CP_1(TRIG_DONE11, TRIG_DONE11.actual, 0)
     `RAL_FIELD_CP(TRIG_DONE12, TRIG_DONE12.actual)
     `RAL_FIELD_CP_1(TRIG_DONE12, TRIG_DONE12.actual, 0)
     `RAL_FIELD_CP(TRIG_DONE13, TRIG_DONE13.actual)
     `RAL_FIELD_CP_1(TRIG_DONE13, TRIG_DONE13.actual, 0)
     `RAL_FIELD_CP(TRIG_DONE14, TRIG_DONE14.actual)
     `RAL_FIELD_CP_1(TRIG_DONE14, TRIG_DONE14.actual, 0)
     `RAL_FIELD_CP(PMETO_DONE, PMETO_DONE.actual)
     `RAL_FIELD_CP_1(PMETO_DONE, PMETO_DONE.actual, 0)
  endgroup
  `endif

  // --------------------------
  // add constraint based on database legal values
  // --------------------------
  function void build();
    BOOT_START = new("BOOT_START", "RW/V", 1, 0, {"SEQENG_STS.BOOT_START"});
    BOOT_START.set_powerwell("primary");
    BOOT_START.set_rand_mode(0);
   BOOT_START.set_reset_signame("pmu_rst_b");
    void'(add_field( BOOT_START ));

    TRIG_PEND1 = new("TRIG_PEND1", "RW/V", 1, 1, {"SEQENG_STS.TRIG_PEND1"});
    TRIG_PEND1.set_powerwell("primary");
    TRIG_PEND1.set_rand_mode(0);
   TRIG_PEND1.set_reset_signame("pmu_rst_b");
    void'(add_field( TRIG_PEND1 ));

    TRIG_PEND2 = new("TRIG_PEND2", "RW/V", 1, 2, {"SEQENG_STS.TRIG_PEND2"});
    TRIG_PEND2.set_powerwell("primary");
    TRIG_PEND2.set_rand_mode(0);
   TRIG_PEND2.set_reset_signame("pmu_rst_b");
    void'(add_field( TRIG_PEND2 ));

    TRIG_PEND3 = new("TRIG_PEND3", "RW/V", 1, 3, {"SEQENG_STS.TRIG_PEND3"});
    TRIG_PEND3.set_powerwell("primary");
    TRIG_PEND3.set_rand_mode(0);
   TRIG_PEND3.set_reset_signame("pmu_rst_b");
    void'(add_field( TRIG_PEND3 ));

    TRIG_PEND4 = new("TRIG_PEND4", "RW/V", 1, 4, {"SEQENG_STS.TRIG_PEND4"});
    TRIG_PEND4.set_powerwell("primary");
    TRIG_PEND4.set_rand_mode(0);
   TRIG_PEND4.set_reset_signame("pmu_rst_b");
    void'(add_field( TRIG_PEND4 ));

    TRIG_PEND5 = new("TRIG_PEND5", "RW/V", 1, 5, {"SEQENG_STS.TRIG_PEND5"});
    TRIG_PEND5.set_powerwell("primary");
    TRIG_PEND5.set_rand_mode(0);
   TRIG_PEND5.set_reset_signame("pmu_rst_b");
    void'(add_field( TRIG_PEND5 ));

    TRIG_PEND6 = new("TRIG_PEND6", "RW/V", 1, 6, {"SEQENG_STS.TRIG_PEND6"});
    TRIG_PEND6.set_powerwell("primary");
    TRIG_PEND6.set_rand_mode(0);
   TRIG_PEND6.set_reset_signame("pmu_rst_b");
    void'(add_field( TRIG_PEND6 ));

    TRIG_PEND7 = new("TRIG_PEND7", "RW/V", 1, 7, {"SEQENG_STS.TRIG_PEND7"});
    TRIG_PEND7.set_powerwell("primary");
    TRIG_PEND7.set_rand_mode(0);
   TRIG_PEND7.set_reset_signame("pmu_rst_b");
    void'(add_field( TRIG_PEND7 ));

    TRIG_PEND8 = new("TRIG_PEND8", "RW/V", 1, 8, {"SEQENG_STS.TRIG_PEND8"});
    TRIG_PEND8.set_powerwell("primary");
    TRIG_PEND8.set_rand_mode(0);
   TRIG_PEND8.set_reset_signame("pmu_rst_b");
    void'(add_field( TRIG_PEND8 ));

    TRIG_PEND9 = new("TRIG_PEND9", "RW/V", 1, 9, {"SEQENG_STS.TRIG_PEND9"});
    TRIG_PEND9.set_powerwell("primary");
    TRIG_PEND9.set_rand_mode(0);
   TRIG_PEND9.set_reset_signame("pmu_rst_b");
    void'(add_field( TRIG_PEND9 ));

    TRIG_PEND10 = new("TRIG_PEND10", "RW/V", 1, 10, {"SEQENG_STS.TRIG_PEND10"});
    TRIG_PEND10.set_powerwell("primary");
    TRIG_PEND10.set_rand_mode(0);
   TRIG_PEND10.set_reset_signame("pmu_rst_b");
    void'(add_field( TRIG_PEND10 ));

    TRIG_PEND11 = new("TRIG_PEND11", "RW/V", 1, 11, {"SEQENG_STS.TRIG_PEND11"});
    TRIG_PEND11.set_powerwell("primary");
    TRIG_PEND11.set_rand_mode(0);
   TRIG_PEND11.set_reset_signame("pmu_rst_b");
    void'(add_field( TRIG_PEND11 ));

    TRIG_PEND12 = new("TRIG_PEND12", "RW/V", 1, 12, {"SEQENG_STS.TRIG_PEND12"});
    TRIG_PEND12.set_powerwell("primary");
    TRIG_PEND12.set_rand_mode(0);
   TRIG_PEND12.set_reset_signame("pmu_rst_b");
    void'(add_field( TRIG_PEND12 ));

    TRIG_PEND13 = new("TRIG_PEND13", "RW/V", 1, 13, {"SEQENG_STS.TRIG_PEND13"});
    TRIG_PEND13.set_powerwell("primary");
    TRIG_PEND13.set_rand_mode(0);
   TRIG_PEND13.set_reset_signame("pmu_rst_b");
    void'(add_field( TRIG_PEND13 ));

    TRIG_PEND14 = new("TRIG_PEND14", "RW/V", 1, 14, {"SEQENG_STS.TRIG_PEND14"});
    TRIG_PEND14.set_powerwell("primary");
    TRIG_PEND14.set_rand_mode(0);
   TRIG_PEND14.set_reset_signame("pmu_rst_b");
    void'(add_field( TRIG_PEND14 ));

    PMETO_PEND = new("PMETO_PEND", "RW/V", 1, 15, {"SEQENG_STS.PMETO_PEND"});
    PMETO_PEND.set_powerwell("primary");
    PMETO_PEND.set_rand_mode(0);
   PMETO_PEND.set_reset_signame("pmu_rst_b");
    void'(add_field( PMETO_PEND ));

    BOOT_DONE = new("BOOT_DONE", "RW/V", 1, 16, {"SEQENG_STS.BOOT_DONE"});
    BOOT_DONE.set_powerwell("primary");
    BOOT_DONE.set_rand_mode(0);
   BOOT_DONE.set_reset_signame("pmu_rst_b");
    void'(add_field( BOOT_DONE ));

    TRIG_DONE1 = new("TRIG_DONE1", "RW/V", 1, 17, {"SEQENG_STS.TRIG_DONE1"});
    TRIG_DONE1.set_powerwell("primary");
    TRIG_DONE1.set_rand_mode(0);
   TRIG_DONE1.set_reset_signame("pmu_rst_b");
    void'(add_field( TRIG_DONE1 ));

    TRIG_DONE2 = new("TRIG_DONE2", "RW/V", 1, 18, {"SEQENG_STS.TRIG_DONE2"});
    TRIG_DONE2.set_powerwell("primary");
    TRIG_DONE2.set_rand_mode(0);
   TRIG_DONE2.set_reset_signame("pmu_rst_b");
    void'(add_field( TRIG_DONE2 ));

    TRIG_DONE3 = new("TRIG_DONE3", "RW/V", 1, 19, {"SEQENG_STS.TRIG_DONE3"});
    TRIG_DONE3.set_powerwell("primary");
    TRIG_DONE3.set_rand_mode(0);
   TRIG_DONE3.set_reset_signame("pmu_rst_b");
    void'(add_field( TRIG_DONE3 ));

    TRIG_DONE4 = new("TRIG_DONE4", "RW/V", 1, 20, {"SEQENG_STS.TRIG_DONE4"});
    TRIG_DONE4.set_powerwell("primary");
    TRIG_DONE4.set_rand_mode(0);
   TRIG_DONE4.set_reset_signame("pmu_rst_b");
    void'(add_field( TRIG_DONE4 ));

    TRIG_DONE5 = new("TRIG_DONE5", "RW/V", 1, 21, {"SEQENG_STS.TRIG_DONE5"});
    TRIG_DONE5.set_powerwell("primary");
    TRIG_DONE5.set_rand_mode(0);
   TRIG_DONE5.set_reset_signame("pmu_rst_b");
    void'(add_field( TRIG_DONE5 ));

    TRIG_DONE6 = new("TRIG_DONE6", "RW/V", 1, 22, {"SEQENG_STS.TRIG_DONE6"});
    TRIG_DONE6.set_powerwell("primary");
    TRIG_DONE6.set_rand_mode(0);
   TRIG_DONE6.set_reset_signame("pmu_rst_b");
    void'(add_field( TRIG_DONE6 ));

    TRIG_DONE7 = new("TRIG_DONE7", "RW/V", 1, 23, {"SEQENG_STS.TRIG_DONE7"});
    TRIG_DONE7.set_powerwell("primary");
    TRIG_DONE7.set_rand_mode(0);
   TRIG_DONE7.set_reset_signame("pmu_rst_b");
    void'(add_field( TRIG_DONE7 ));

    TRIG_DONE8 = new("TRIG_DONE8", "RW/V", 1, 24, {"SEQENG_STS.TRIG_DONE8"});
    TRIG_DONE8.set_powerwell("primary");
    TRIG_DONE8.set_rand_mode(0);
   TRIG_DONE8.set_reset_signame("pmu_rst_b");
    void'(add_field( TRIG_DONE8 ));

    TRIG_DONE9 = new("TRIG_DONE9", "RW/V", 1, 25, {"SEQENG_STS.TRIG_DONE9"});
    TRIG_DONE9.set_powerwell("primary");
    TRIG_DONE9.set_rand_mode(0);
   TRIG_DONE9.set_reset_signame("pmu_rst_b");
    void'(add_field( TRIG_DONE9 ));

    TRIG_DONE10 = new("TRIG_DONE10", "RW/V", 1, 26, {"SEQENG_STS.TRIG_DONE10"});
    TRIG_DONE10.set_powerwell("primary");
    TRIG_DONE10.set_rand_mode(0);
   TRIG_DONE10.set_reset_signame("pmu_rst_b");
    void'(add_field( TRIG_DONE10 ));

    TRIG_DONE11 = new("TRIG_DONE11", "RW/V", 1, 27, {"SEQENG_STS.TRIG_DONE11"});
    TRIG_DONE11.set_powerwell("primary");
    TRIG_DONE11.set_rand_mode(0);
   TRIG_DONE11.set_reset_signame("pmu_rst_b");
    void'(add_field( TRIG_DONE11 ));

    TRIG_DONE12 = new("TRIG_DONE12", "RW/V", 1, 28, {"SEQENG_STS.TRIG_DONE12"});
    TRIG_DONE12.set_powerwell("primary");
    TRIG_DONE12.set_rand_mode(0);
   TRIG_DONE12.set_reset_signame("pmu_rst_b");
    void'(add_field( TRIG_DONE12 ));

    TRIG_DONE13 = new("TRIG_DONE13", "RW/V", 1, 29, {"SEQENG_STS.TRIG_DONE13"});
    TRIG_DONE13.set_powerwell("primary");
    TRIG_DONE13.set_rand_mode(0);
   TRIG_DONE13.set_reset_signame("pmu_rst_b");
    void'(add_field( TRIG_DONE13 ));

    TRIG_DONE14 = new("TRIG_DONE14", "RW/V", 1, 30, {"SEQENG_STS.TRIG_DONE14"});
    TRIG_DONE14.set_powerwell("primary");
    TRIG_DONE14.set_rand_mode(0);
   TRIG_DONE14.set_reset_signame("pmu_rst_b");
    void'(add_field( TRIG_DONE14 ));

    PMETO_DONE = new("PMETO_DONE", "RW/V", 1, 31, {"SEQENG_STS.PMETO_DONE"});
    PMETO_DONE.set_powerwell("primary");
    PMETO_DONE.set_rand_mode(0);
   PMETO_DONE.set_reset_signame("pmu_rst_b");
    void'(add_field( PMETO_DONE ));

  endfunction


`DEFINE_NONE_SCOPE_REG_API(pmu_mmr_SEQENG_STS_reg) 
endclass : pmu_mmr_SEQENG_STS_reg

// ================================================

class pmu_mmr_SEQENG_STS_1_reg extends sla_ral_reg;

  // --------------------------
  sla_ral_field BOOT_START_WIP;
  sla_ral_field TRIG_WIP1;
  sla_ral_field TRIG_WIP2;
  sla_ral_field TRIG_WIP3;
  sla_ral_field TRIG_WIP4;
  sla_ral_field TRIG_WIP5;
  sla_ral_field TRIG_WIP6;
  sla_ral_field TRIG_WIP7;
  sla_ral_field TRIG_WIP8;
  sla_ral_field TRIG_WIP9;
  sla_ral_field TRIG_WIP10;
  sla_ral_field TRIG_WIP11;
  sla_ral_field TRIG_WIP12;
  sla_ral_field TRIG_WIP13;
  sla_ral_field TRIG_WIP14;
  sla_ral_field PMETO_WIP;
  sla_ral_field CPL_SEQ_ID;
  sla_ral_field CPL_SEQ_LN;

  // --------------------------
  `ovm_object_utils(pmu_mmr_SEQENG_STS_1_reg)

  // --------------------------
  function new(
                string         name = "",
                `ifdef SLA_RAL_COVERAGE
                sla_ral_coverage_t cov_t = COVERAGE_OFF,
                `endif
                int            bus_num=0,
                int            dev_num=0,
                int            func_num=0,
                int            offset=0,
                int            size=0,
                sla_ral_data_t reset_val=0,
                boolean_t      mon_enabled = SLA_FALSE
             );
      super.new();
      set_space_addr("CFG",undef);
      set_space_addr("MEM",undef);
      set_space_addr("IO",undef);
      set_space_addr("MSG",undef);
      set_space_addr("CREG",undef); 
      set_space_addr("LT_MEM",undef);    
 build();

      `ifdef SLA_RAL_COVERAGE
      `CONFIG_RAL_COVERAGE(regname, COVERAGE_ON, FD_RW_SAMPLE)
      if(_coverage_type != COVERAGE_OFF) begin
        if(_sample_type inside {FD_RO_SAMPLE, RO_SAMPLE})
        `CREATE_OP_SAMPLE_COV(RAL_WRITE)
        else if(_sample_type inside {FD_WO_SAMPLE, WO_SAMPLE})
        `CREATE_OP_SAMPLE_COV(RAL_READ)
        else
        `CREATE_OP_SAMPLE_COV(RAL_NONE)
        `CREATE_DESIRED_COV
        `CREATE_ACTUAL_COV
      end
      `endif

  endfunction

  `ifdef SLA_RAL_COVERAGE
  `CREATE_RAL_OP_COVERGROUP
  covergroup desired_cg @(cov_ev);
     option.per_instance = 1;
     option.name = {sla_ral_file::regfilename,".",regname};
     `RAL_FIELD_CP(BOOT_START_WIP, BOOT_START_WIP.desired)
     `RAL_FIELD_CP_1(BOOT_START_WIP, BOOT_START_WIP.desired, 0)
     `RAL_FIELD_CP(TRIG_WIP1, TRIG_WIP1.desired)
     `RAL_FIELD_CP_1(TRIG_WIP1, TRIG_WIP1.desired, 0)
     `RAL_FIELD_CP(TRIG_WIP2, TRIG_WIP2.desired)
     `RAL_FIELD_CP_1(TRIG_WIP2, TRIG_WIP2.desired, 0)
     `RAL_FIELD_CP(TRIG_WIP3, TRIG_WIP3.desired)
     `RAL_FIELD_CP_1(TRIG_WIP3, TRIG_WIP3.desired, 0)
     `RAL_FIELD_CP(TRIG_WIP4, TRIG_WIP4.desired)
     `RAL_FIELD_CP_1(TRIG_WIP4, TRIG_WIP4.desired, 0)
     `RAL_FIELD_CP(TRIG_WIP5, TRIG_WIP5.desired)
     `RAL_FIELD_CP_1(TRIG_WIP5, TRIG_WIP5.desired, 0)
     `RAL_FIELD_CP(TRIG_WIP6, TRIG_WIP6.desired)
     `RAL_FIELD_CP_1(TRIG_WIP6, TRIG_WIP6.desired, 0)
     `RAL_FIELD_CP(TRIG_WIP7, TRIG_WIP7.desired)
     `RAL_FIELD_CP_1(TRIG_WIP7, TRIG_WIP7.desired, 0)
     `RAL_FIELD_CP(TRIG_WIP8, TRIG_WIP8.desired)
     `RAL_FIELD_CP_1(TRIG_WIP8, TRIG_WIP8.desired, 0)
     `RAL_FIELD_CP(TRIG_WIP9, TRIG_WIP9.desired)
     `RAL_FIELD_CP_1(TRIG_WIP9, TRIG_WIP9.desired, 0)
     `RAL_FIELD_CP(TRIG_WIP10, TRIG_WIP10.desired)
     `RAL_FIELD_CP_1(TRIG_WIP10, TRIG_WIP10.desired, 0)
     `RAL_FIELD_CP(TRIG_WIP11, TRIG_WIP11.desired)
     `RAL_FIELD_CP_1(TRIG_WIP11, TRIG_WIP11.desired, 0)
     `RAL_FIELD_CP(TRIG_WIP12, TRIG_WIP12.desired)
     `RAL_FIELD_CP_1(TRIG_WIP12, TRIG_WIP12.desired, 0)
     `RAL_FIELD_CP(TRIG_WIP13, TRIG_WIP13.desired)
     `RAL_FIELD_CP_1(TRIG_WIP13, TRIG_WIP13.desired, 0)
     `RAL_FIELD_CP(TRIG_WIP14, TRIG_WIP14.desired)
     `RAL_FIELD_CP_1(TRIG_WIP14, TRIG_WIP14.desired, 0)
     `RAL_FIELD_CP(PMETO_WIP, PMETO_WIP.desired)
     `RAL_FIELD_CP_1(PMETO_WIP, PMETO_WIP.desired, 0)
     `RAL_FIELD_CP(CPL_SEQ_ID, CPL_SEQ_ID.desired)
     `RAL_FIELD_CP_5(CPL_SEQ_ID, CPL_SEQ_ID.desired, 0,1,2,3,4)
     `RAL_FIELD_CP(CPL_SEQ_LN, CPL_SEQ_LN.desired)
     `RAL_FIELD_CP_11(CPL_SEQ_LN, CPL_SEQ_LN.desired, 0,1,2,3,4,5,6,7,8,9,10)
  endgroup
  covergroup actual_cg @(cov_ev);
     option.per_instance = 1;
     option.name = {sla_ral_file::regfilename,".",regname};
     `RAL_FIELD_CP(BOOT_START_WIP, BOOT_START_WIP.actual)
     `RAL_FIELD_CP_1(BOOT_START_WIP, BOOT_START_WIP.actual, 0)
     `RAL_FIELD_CP(TRIG_WIP1, TRIG_WIP1.actual)
     `RAL_FIELD_CP_1(TRIG_WIP1, TRIG_WIP1.actual, 0)
     `RAL_FIELD_CP(TRIG_WIP2, TRIG_WIP2.actual)
     `RAL_FIELD_CP_1(TRIG_WIP2, TRIG_WIP2.actual, 0)
     `RAL_FIELD_CP(TRIG_WIP3, TRIG_WIP3.actual)
     `RAL_FIELD_CP_1(TRIG_WIP3, TRIG_WIP3.actual, 0)
     `RAL_FIELD_CP(TRIG_WIP4, TRIG_WIP4.actual)
     `RAL_FIELD_CP_1(TRIG_WIP4, TRIG_WIP4.actual, 0)
     `RAL_FIELD_CP(TRIG_WIP5, TRIG_WIP5.actual)
     `RAL_FIELD_CP_1(TRIG_WIP5, TRIG_WIP5.actual, 0)
     `RAL_FIELD_CP(TRIG_WIP6, TRIG_WIP6.actual)
     `RAL_FIELD_CP_1(TRIG_WIP6, TRIG_WIP6.actual, 0)
     `RAL_FIELD_CP(TRIG_WIP7, TRIG_WIP7.actual)
     `RAL_FIELD_CP_1(TRIG_WIP7, TRIG_WIP7.actual, 0)
     `RAL_FIELD_CP(TRIG_WIP8, TRIG_WIP8.actual)
     `RAL_FIELD_CP_1(TRIG_WIP8, TRIG_WIP8.actual, 0)
     `RAL_FIELD_CP(TRIG_WIP9, TRIG_WIP9.actual)
     `RAL_FIELD_CP_1(TRIG_WIP9, TRIG_WIP9.actual, 0)
     `RAL_FIELD_CP(TRIG_WIP10, TRIG_WIP10.actual)
     `RAL_FIELD_CP_1(TRIG_WIP10, TRIG_WIP10.actual, 0)
     `RAL_FIELD_CP(TRIG_WIP11, TRIG_WIP11.actual)
     `RAL_FIELD_CP_1(TRIG_WIP11, TRIG_WIP11.actual, 0)
     `RAL_FIELD_CP(TRIG_WIP12, TRIG_WIP12.actual)
     `RAL_FIELD_CP_1(TRIG_WIP12, TRIG_WIP12.actual, 0)
     `RAL_FIELD_CP(TRIG_WIP13, TRIG_WIP13.actual)
     `RAL_FIELD_CP_1(TRIG_WIP13, TRIG_WIP13.actual, 0)
     `RAL_FIELD_CP(TRIG_WIP14, TRIG_WIP14.actual)
     `RAL_FIELD_CP_1(TRIG_WIP14, TRIG_WIP14.actual, 0)
     `RAL_FIELD_CP(PMETO_WIP, PMETO_WIP.actual)
     `RAL_FIELD_CP_1(PMETO_WIP, PMETO_WIP.actual, 0)
     `RAL_FIELD_CP(CPL_SEQ_ID, CPL_SEQ_ID.actual)
     `RAL_FIELD_CP_5(CPL_SEQ_ID, CPL_SEQ_ID.actual, 0,1,2,3,4)
     `RAL_FIELD_CP(CPL_SEQ_LN, CPL_SEQ_LN.actual)
     `RAL_FIELD_CP_11(CPL_SEQ_LN, CPL_SEQ_LN.actual, 0,1,2,3,4,5,6,7,8,9,10)
  endgroup
  `endif

  // --------------------------
  // add constraint based on database legal values
  // --------------------------
  function void build();
    BOOT_START_WIP = new("BOOT_START_WIP", "RW", 1, 0, {"SEQENG_STS_1.BOOT_START_WIP"});
    BOOT_START_WIP.set_powerwell("primary");
    BOOT_START_WIP.set_rand_mode(0);
   BOOT_START_WIP.set_reset_signame("pmu_rst_b");
    void'(add_field( BOOT_START_WIP ));

    TRIG_WIP1 = new("TRIG_WIP1", "RW", 1, 1, {"SEQENG_STS_1.TRIG_WIP1"});
    TRIG_WIP1.set_powerwell("primary");
    TRIG_WIP1.set_rand_mode(0);
   TRIG_WIP1.set_reset_signame("pmu_rst_b");
    void'(add_field( TRIG_WIP1 ));

    TRIG_WIP2 = new("TRIG_WIP2", "RW", 1, 2, {"SEQENG_STS_1.TRIG_WIP2"});
    TRIG_WIP2.set_powerwell("primary");
    TRIG_WIP2.set_rand_mode(0);
   TRIG_WIP2.set_reset_signame("pmu_rst_b");
    void'(add_field( TRIG_WIP2 ));

    TRIG_WIP3 = new("TRIG_WIP3", "RW", 1, 3, {"SEQENG_STS_1.TRIG_WIP3"});
    TRIG_WIP3.set_powerwell("primary");
    TRIG_WIP3.set_rand_mode(0);
   TRIG_WIP3.set_reset_signame("pmu_rst_b");
    void'(add_field( TRIG_WIP3 ));

    TRIG_WIP4 = new("TRIG_WIP4", "RW", 1, 4, {"SEQENG_STS_1.TRIG_WIP4"});
    TRIG_WIP4.set_powerwell("primary");
    TRIG_WIP4.set_rand_mode(0);
   TRIG_WIP4.set_reset_signame("pmu_rst_b");
    void'(add_field( TRIG_WIP4 ));

    TRIG_WIP5 = new("TRIG_WIP5", "RW", 1, 5, {"SEQENG_STS_1.TRIG_WIP5"});
    TRIG_WIP5.set_powerwell("primary");
    TRIG_WIP5.set_rand_mode(0);
   TRIG_WIP5.set_reset_signame("pmu_rst_b");
    void'(add_field( TRIG_WIP5 ));

    TRIG_WIP6 = new("TRIG_WIP6", "RW", 1, 6, {"SEQENG_STS_1.TRIG_WIP6"});
    TRIG_WIP6.set_powerwell("primary");
    TRIG_WIP6.set_rand_mode(0);
   TRIG_WIP6.set_reset_signame("pmu_rst_b");
    void'(add_field( TRIG_WIP6 ));

    TRIG_WIP7 = new("TRIG_WIP7", "RW", 1, 7, {"SEQENG_STS_1.TRIG_WIP7"});
    TRIG_WIP7.set_powerwell("primary");
    TRIG_WIP7.set_rand_mode(0);
   TRIG_WIP7.set_reset_signame("pmu_rst_b");
    void'(add_field( TRIG_WIP7 ));

    TRIG_WIP8 = new("TRIG_WIP8", "RW", 1, 8, {"SEQENG_STS_1.TRIG_WIP8"});
    TRIG_WIP8.set_powerwell("primary");
    TRIG_WIP8.set_rand_mode(0);
   TRIG_WIP8.set_reset_signame("pmu_rst_b");
    void'(add_field( TRIG_WIP8 ));

    TRIG_WIP9 = new("TRIG_WIP9", "RW", 1, 9, {"SEQENG_STS_1.TRIG_WIP9"});
    TRIG_WIP9.set_powerwell("primary");
    TRIG_WIP9.set_rand_mode(0);
   TRIG_WIP9.set_reset_signame("pmu_rst_b");
    void'(add_field( TRIG_WIP9 ));

    TRIG_WIP10 = new("TRIG_WIP10", "RW", 1, 10, {"SEQENG_STS_1.TRIG_WIP10"});
    TRIG_WIP10.set_powerwell("primary");
    TRIG_WIP10.set_rand_mode(0);
   TRIG_WIP10.set_reset_signame("pmu_rst_b");
    void'(add_field( TRIG_WIP10 ));

    TRIG_WIP11 = new("TRIG_WIP11", "RW", 1, 11, {"SEQENG_STS_1.TRIG_WIP11"});
    TRIG_WIP11.set_powerwell("primary");
    TRIG_WIP11.set_rand_mode(0);
   TRIG_WIP11.set_reset_signame("pmu_rst_b");
    void'(add_field( TRIG_WIP11 ));

    TRIG_WIP12 = new("TRIG_WIP12", "RW", 1, 12, {"SEQENG_STS_1.TRIG_WIP12"});
    TRIG_WIP12.set_powerwell("primary");
    TRIG_WIP12.set_rand_mode(0);
   TRIG_WIP12.set_reset_signame("pmu_rst_b");
    void'(add_field( TRIG_WIP12 ));

    TRIG_WIP13 = new("TRIG_WIP13", "RW", 1, 13, {"SEQENG_STS_1.TRIG_WIP13"});
    TRIG_WIP13.set_powerwell("primary");
    TRIG_WIP13.set_rand_mode(0);
   TRIG_WIP13.set_reset_signame("pmu_rst_b");
    void'(add_field( TRIG_WIP13 ));

    TRIG_WIP14 = new("TRIG_WIP14", "RW", 1, 14, {"SEQENG_STS_1.TRIG_WIP14"});
    TRIG_WIP14.set_powerwell("primary");
    TRIG_WIP14.set_rand_mode(0);
   TRIG_WIP14.set_reset_signame("pmu_rst_b");
    void'(add_field( TRIG_WIP14 ));

    PMETO_WIP = new("PMETO_WIP", "RW", 1, 15, {"SEQENG_STS_1.PMETO_WIP"});
    PMETO_WIP.set_powerwell("primary");
    PMETO_WIP.set_rand_mode(0);
   PMETO_WIP.set_reset_signame("pmu_rst_b");
    void'(add_field( PMETO_WIP ));

    CPL_SEQ_ID = new("CPL_SEQ_ID", "RO/V", 5, 16, {"SEQENG_STS_1.CPL_SEQ_ID"});
    CPL_SEQ_ID.set_powerwell("primary");
    CPL_SEQ_ID.set_rand_mode(0);
    void'(add_field( CPL_SEQ_ID ));

    CPL_SEQ_LN = new("CPL_SEQ_LN", "RO/V", 11, 21, {"SEQENG_STS_1.CPL_SEQ_LN"});
    CPL_SEQ_LN.set_powerwell("primary");
    CPL_SEQ_LN.set_rand_mode(0);
    void'(add_field( CPL_SEQ_LN ));

  endfunction


`DEFINE_NONE_SCOPE_REG_API(pmu_mmr_SEQENG_STS_1_reg) 
endclass : pmu_mmr_SEQENG_STS_1_reg

// ================================================

class pmu_mmr_SEQENG_EVENTS_reg extends sla_ral_reg;

  // --------------------------
  sla_ral_field BOOT_START_EVENT;
  sla_ral_field TRIG_EVENT1;
  sla_ral_field TRIG_EVENT2;
  sla_ral_field TRIG_EVENT3;
  sla_ral_field TRIG_EVENT4;
  sla_ral_field TRIG_EVENT5;
  sla_ral_field TRIG_EVENTi6;
  sla_ral_field TRIG_EVENT7;
  sla_ral_field TRIG_EVENT8;
  sla_ral_field TRIG_EVENT9;
  sla_ral_field TRIG_EVENT10;
  sla_ral_field TRIG_EVENT11;
  sla_ral_field TRIG_EVENT12;
  sla_ral_field TRIG_EVENT13;
  sla_ral_field TRIG_EVENT14;
  sla_ral_field PME_TO_EVENT;
  sla_ral_field RSVD_31_15;

  // --------------------------
  `ovm_object_utils(pmu_mmr_SEQENG_EVENTS_reg)

  // --------------------------
  function new(
                string         name = "",
                `ifdef SLA_RAL_COVERAGE
                sla_ral_coverage_t cov_t = COVERAGE_OFF,
                `endif
                int            bus_num=0,
                int            dev_num=0,
                int            func_num=0,
                int            offset=0,
                int            size=0,
                sla_ral_data_t reset_val=0,
                boolean_t      mon_enabled = SLA_FALSE
             );
      super.new();
      set_space_addr("CFG",undef);
      set_space_addr("MEM",undef);
      set_space_addr("IO",undef);
      set_space_addr("MSG",undef);
      set_space_addr("CREG",undef); 
      set_space_addr("LT_MEM",undef);    
 build();

      `ifdef SLA_RAL_COVERAGE
      `CONFIG_RAL_COVERAGE(regname, COVERAGE_ON, FD_RW_SAMPLE)
      if(_coverage_type != COVERAGE_OFF) begin
        if(_sample_type inside {FD_RO_SAMPLE, RO_SAMPLE})
        `CREATE_OP_SAMPLE_COV(RAL_WRITE)
        else if(_sample_type inside {FD_WO_SAMPLE, WO_SAMPLE})
        `CREATE_OP_SAMPLE_COV(RAL_READ)
        else
        `CREATE_OP_SAMPLE_COV(RAL_NONE)
        `CREATE_DESIRED_COV
        `CREATE_ACTUAL_COV
      end
      `endif

  endfunction

  `ifdef SLA_RAL_COVERAGE
  `CREATE_RAL_OP_COVERGROUP
  covergroup desired_cg @(cov_ev);
     option.per_instance = 1;
     option.name = {sla_ral_file::regfilename,".",regname};
     `RAL_FIELD_CP(BOOT_START_EVENT, BOOT_START_EVENT.desired)
     `RAL_FIELD_CP_1(BOOT_START_EVENT, BOOT_START_EVENT.desired, 0)
     `RAL_FIELD_CP(TRIG_EVENT1, TRIG_EVENT1.desired)
     `RAL_FIELD_CP_1(TRIG_EVENT1, TRIG_EVENT1.desired, 0)
     `RAL_FIELD_CP(TRIG_EVENT2, TRIG_EVENT2.desired)
     `RAL_FIELD_CP_1(TRIG_EVENT2, TRIG_EVENT2.desired, 0)
     `RAL_FIELD_CP(TRIG_EVENT3, TRIG_EVENT3.desired)
     `RAL_FIELD_CP_1(TRIG_EVENT3, TRIG_EVENT3.desired, 0)
     `RAL_FIELD_CP(TRIG_EVENT4, TRIG_EVENT4.desired)
     `RAL_FIELD_CP_1(TRIG_EVENT4, TRIG_EVENT4.desired, 0)
     `RAL_FIELD_CP(TRIG_EVENT5, TRIG_EVENT5.desired)
     `RAL_FIELD_CP_1(TRIG_EVENT5, TRIG_EVENT5.desired, 0)
     `RAL_FIELD_CP(TRIG_EVENTi6, TRIG_EVENTi6.desired)
     `RAL_FIELD_CP_1(TRIG_EVENTi6, TRIG_EVENTi6.desired, 0)
     `RAL_FIELD_CP(TRIG_EVENT7, TRIG_EVENT7.desired)
     `RAL_FIELD_CP_1(TRIG_EVENT7, TRIG_EVENT7.desired, 0)
     `RAL_FIELD_CP(TRIG_EVENT8, TRIG_EVENT8.desired)
     `RAL_FIELD_CP_1(TRIG_EVENT8, TRIG_EVENT8.desired, 0)
     `RAL_FIELD_CP(TRIG_EVENT9, TRIG_EVENT9.desired)
     `RAL_FIELD_CP_1(TRIG_EVENT9, TRIG_EVENT9.desired, 0)
     `RAL_FIELD_CP(TRIG_EVENT10, TRIG_EVENT10.desired)
     `RAL_FIELD_CP_1(TRIG_EVENT10, TRIG_EVENT10.desired, 0)
     `RAL_FIELD_CP(TRIG_EVENT11, TRIG_EVENT11.desired)
     `RAL_FIELD_CP_1(TRIG_EVENT11, TRIG_EVENT11.desired, 0)
     `RAL_FIELD_CP(TRIG_EVENT12, TRIG_EVENT12.desired)
     `RAL_FIELD_CP_1(TRIG_EVENT12, TRIG_EVENT12.desired, 0)
     `RAL_FIELD_CP(TRIG_EVENT13, TRIG_EVENT13.desired)
     `RAL_FIELD_CP_1(TRIG_EVENT13, TRIG_EVENT13.desired, 0)
     `RAL_FIELD_CP(TRIG_EVENT14, TRIG_EVENT14.desired)
     `RAL_FIELD_CP_1(TRIG_EVENT14, TRIG_EVENT14.desired, 0)
     `RAL_FIELD_CP(PME_TO_EVENT, PME_TO_EVENT.desired)
     `RAL_FIELD_CP_1(PME_TO_EVENT, PME_TO_EVENT.desired, 0)
     `RAL_FIELD_CP(RSVD_31_15, RSVD_31_15.desired)
     `RAL_FIELD_CP_16(RSVD_31_15, RSVD_31_15.desired, 0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)
  endgroup
  covergroup actual_cg @(cov_ev);
     option.per_instance = 1;
     option.name = {sla_ral_file::regfilename,".",regname};
     `RAL_FIELD_CP(BOOT_START_EVENT, BOOT_START_EVENT.actual)
     `RAL_FIELD_CP_1(BOOT_START_EVENT, BOOT_START_EVENT.actual, 0)
     `RAL_FIELD_CP(TRIG_EVENT1, TRIG_EVENT1.actual)
     `RAL_FIELD_CP_1(TRIG_EVENT1, TRIG_EVENT1.actual, 0)
     `RAL_FIELD_CP(TRIG_EVENT2, TRIG_EVENT2.actual)
     `RAL_FIELD_CP_1(TRIG_EVENT2, TRIG_EVENT2.actual, 0)
     `RAL_FIELD_CP(TRIG_EVENT3, TRIG_EVENT3.actual)
     `RAL_FIELD_CP_1(TRIG_EVENT3, TRIG_EVENT3.actual, 0)
     `RAL_FIELD_CP(TRIG_EVENT4, TRIG_EVENT4.actual)
     `RAL_FIELD_CP_1(TRIG_EVENT4, TRIG_EVENT4.actual, 0)
     `RAL_FIELD_CP(TRIG_EVENT5, TRIG_EVENT5.actual)
     `RAL_FIELD_CP_1(TRIG_EVENT5, TRIG_EVENT5.actual, 0)
     `RAL_FIELD_CP(TRIG_EVENTi6, TRIG_EVENTi6.actual)
     `RAL_FIELD_CP_1(TRIG_EVENTi6, TRIG_EVENTi6.actual, 0)
     `RAL_FIELD_CP(TRIG_EVENT7, TRIG_EVENT7.actual)
     `RAL_FIELD_CP_1(TRIG_EVENT7, TRIG_EVENT7.actual, 0)
     `RAL_FIELD_CP(TRIG_EVENT8, TRIG_EVENT8.actual)
     `RAL_FIELD_CP_1(TRIG_EVENT8, TRIG_EVENT8.actual, 0)
     `RAL_FIELD_CP(TRIG_EVENT9, TRIG_EVENT9.actual)
     `RAL_FIELD_CP_1(TRIG_EVENT9, TRIG_EVENT9.actual, 0)
     `RAL_FIELD_CP(TRIG_EVENT10, TRIG_EVENT10.actual)
     `RAL_FIELD_CP_1(TRIG_EVENT10, TRIG_EVENT10.actual, 0)
     `RAL_FIELD_CP(TRIG_EVENT11, TRIG_EVENT11.actual)
     `RAL_FIELD_CP_1(TRIG_EVENT11, TRIG_EVENT11.actual, 0)
     `RAL_FIELD_CP(TRIG_EVENT12, TRIG_EVENT12.actual)
     `RAL_FIELD_CP_1(TRIG_EVENT12, TRIG_EVENT12.actual, 0)
     `RAL_FIELD_CP(TRIG_EVENT13, TRIG_EVENT13.actual)
     `RAL_FIELD_CP_1(TRIG_EVENT13, TRIG_EVENT13.actual, 0)
     `RAL_FIELD_CP(TRIG_EVENT14, TRIG_EVENT14.actual)
     `RAL_FIELD_CP_1(TRIG_EVENT14, TRIG_EVENT14.actual, 0)
     `RAL_FIELD_CP(PME_TO_EVENT, PME_TO_EVENT.actual)
     `RAL_FIELD_CP_1(PME_TO_EVENT, PME_TO_EVENT.actual, 0)
     `RAL_FIELD_CP(RSVD_31_15, RSVD_31_15.actual)
     `RAL_FIELD_CP_16(RSVD_31_15, RSVD_31_15.actual, 0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)
  endgroup
  `endif

  // --------------------------
  // add constraint based on database legal values
  // --------------------------
  function void build();
    BOOT_START_EVENT = new("BOOT_START_EVENT", "RO/V", 1, 0, {"SEQENG_EVENTS.BOOT_START_EVENT"});
    BOOT_START_EVENT.set_powerwell("primary");
    BOOT_START_EVENT.set_rand_mode(0);
   BOOT_START_EVENT.set_reset_signame("pmu_rst_b");
    void'(add_field( BOOT_START_EVENT ));

    TRIG_EVENT1 = new("TRIG_EVENT1", "RO/V", 1, 1, {"SEQENG_EVENTS.TRIG_EVENT1"});
    TRIG_EVENT1.set_powerwell("primary");
    TRIG_EVENT1.set_rand_mode(0);
   TRIG_EVENT1.set_reset_signame("pmu_rst_b");
    void'(add_field( TRIG_EVENT1 ));

    TRIG_EVENT2 = new("TRIG_EVENT2", "RO/V", 1, 2, {"SEQENG_EVENTS.TRIG_EVENT2"});
    TRIG_EVENT2.set_powerwell("primary");
    TRIG_EVENT2.set_rand_mode(0);
   TRIG_EVENT2.set_reset_signame("pmu_rst_b");
    void'(add_field( TRIG_EVENT2 ));

    TRIG_EVENT3 = new("TRIG_EVENT3", "RO/V", 1, 3, {"SEQENG_EVENTS.TRIG_EVENT3"});
    TRIG_EVENT3.set_powerwell("primary");
    TRIG_EVENT3.set_rand_mode(0);
   TRIG_EVENT3.set_reset_signame("pmu_rst_b");
    void'(add_field( TRIG_EVENT3 ));

    TRIG_EVENT4 = new("TRIG_EVENT4", "RO/V", 1, 4, {"SEQENG_EVENTS.TRIG_EVENT4"});
    TRIG_EVENT4.set_powerwell("primary");
    TRIG_EVENT4.set_rand_mode(0);
   TRIG_EVENT4.set_reset_signame("pmu_rst_b");
    void'(add_field( TRIG_EVENT4 ));

    TRIG_EVENT5 = new("TRIG_EVENT5", "RO/V", 1, 5, {"SEQENG_EVENTS.TRIG_EVENT5"});
    TRIG_EVENT5.set_powerwell("primary");
    TRIG_EVENT5.set_rand_mode(0);
   TRIG_EVENT5.set_reset_signame("pmu_rst_b");
    void'(add_field( TRIG_EVENT5 ));

    TRIG_EVENTi6 = new("TRIG_EVENTi6", "RO/V", 1, 6, {"SEQENG_EVENTS.TRIG_EVENTi6"});
    TRIG_EVENTi6.set_powerwell("primary");
    TRIG_EVENTi6.set_rand_mode(0);
   TRIG_EVENTi6.set_reset_signame("pmu_rst_b");
    void'(add_field( TRIG_EVENTi6 ));

    TRIG_EVENT7 = new("TRIG_EVENT7", "RO/V", 1, 7, {"SEQENG_EVENTS.TRIG_EVENT7"});
    TRIG_EVENT7.set_powerwell("primary");
    TRIG_EVENT7.set_rand_mode(0);
   TRIG_EVENT7.set_reset_signame("pmu_rst_b");
    void'(add_field( TRIG_EVENT7 ));

    TRIG_EVENT8 = new("TRIG_EVENT8", "RO/V", 1, 8, {"SEQENG_EVENTS.TRIG_EVENT8"});
    TRIG_EVENT8.set_powerwell("primary");
    TRIG_EVENT8.set_rand_mode(0);
   TRIG_EVENT8.set_reset_signame("pmu_rst_b");
    void'(add_field( TRIG_EVENT8 ));

    TRIG_EVENT9 = new("TRIG_EVENT9", "RO/V", 1, 9, {"SEQENG_EVENTS.TRIG_EVENT9"});
    TRIG_EVENT9.set_powerwell("primary");
    TRIG_EVENT9.set_rand_mode(0);
   TRIG_EVENT9.set_reset_signame("pmu_rst_b");
    void'(add_field( TRIG_EVENT9 ));

    TRIG_EVENT10 = new("TRIG_EVENT10", "RO/V", 1, 10, {"SEQENG_EVENTS.TRIG_EVENT10"});
    TRIG_EVENT10.set_powerwell("primary");
    TRIG_EVENT10.set_rand_mode(0);
   TRIG_EVENT10.set_reset_signame("pmu_rst_b");
    void'(add_field( TRIG_EVENT10 ));

    TRIG_EVENT11 = new("TRIG_EVENT11", "RO/V", 1, 11, {"SEQENG_EVENTS.TRIG_EVENT11"});
    TRIG_EVENT11.set_powerwell("primary");
    TRIG_EVENT11.set_rand_mode(0);
   TRIG_EVENT11.set_reset_signame("pmu_rst_b");
    void'(add_field( TRIG_EVENT11 ));

    TRIG_EVENT12 = new("TRIG_EVENT12", "RO/V", 1, 12, {"SEQENG_EVENTS.TRIG_EVENT12"});
    TRIG_EVENT12.set_powerwell("primary");
    TRIG_EVENT12.set_rand_mode(0);
   TRIG_EVENT12.set_reset_signame("pmu_rst_b");
    void'(add_field( TRIG_EVENT12 ));

    TRIG_EVENT13 = new("TRIG_EVENT13", "RO/V", 1, 13, {"SEQENG_EVENTS.TRIG_EVENT13"});
    TRIG_EVENT13.set_powerwell("primary");
    TRIG_EVENT13.set_rand_mode(0);
   TRIG_EVENT13.set_reset_signame("pmu_rst_b");
    void'(add_field( TRIG_EVENT13 ));

    TRIG_EVENT14 = new("TRIG_EVENT14", "RO/V", 1, 14, {"SEQENG_EVENTS.TRIG_EVENT14"});
    TRIG_EVENT14.set_powerwell("primary");
    TRIG_EVENT14.set_rand_mode(0);
   TRIG_EVENT14.set_reset_signame("pmu_rst_b");
    void'(add_field( TRIG_EVENT14 ));

    PME_TO_EVENT = new("PME_TO_EVENT", "RO/V", 1, 15, {"SEQENG_EVENTS.PME_TO_EVENT"});
    PME_TO_EVENT.set_powerwell("primary");
    PME_TO_EVENT.set_rand_mode(0);
   PME_TO_EVENT.set_reset_signame("pmu_rst_b");
    void'(add_field( PME_TO_EVENT ));

    RSVD_31_15 = new("RSVD_31_15", "RO", 16, 16, {"SEQENG_EVENTS.RSVD_31_15"});
    RSVD_31_15.set_powerwell("primary");
    RSVD_31_15.set_rand_mode(0);
    void'(add_field( RSVD_31_15 ));

  endfunction


`DEFINE_NONE_SCOPE_REG_API(pmu_mmr_SEQENG_EVENTS_reg) 
endclass : pmu_mmr_SEQENG_EVENTS_reg

// ================================================

class pmu_mmr_SEQENG_EVT_POL_reg extends sla_ral_reg;

  // --------------------------
  sla_ral_field TRIG_EVT_POL;
  sla_ral_field RSVD_31_16;

  // --------------------------
  `ovm_object_utils(pmu_mmr_SEQENG_EVT_POL_reg)

  // --------------------------
  function new(
                string         name = "",
                `ifdef SLA_RAL_COVERAGE
                sla_ral_coverage_t cov_t = COVERAGE_OFF,
                `endif
                int            bus_num=0,
                int            dev_num=0,
                int            func_num=0,
                int            offset=0,
                int            size=0,
                sla_ral_data_t reset_val=0,
                boolean_t      mon_enabled = SLA_FALSE
             );
      super.new();
      set_space_addr("CFG",undef);
      set_space_addr("MEM",undef);
      set_space_addr("IO",undef);
      set_space_addr("MSG",undef);
      set_space_addr("CREG",undef); 
      set_space_addr("LT_MEM",undef);    
 build();

      `ifdef SLA_RAL_COVERAGE
      `CONFIG_RAL_COVERAGE(regname, COVERAGE_ON, FD_RW_SAMPLE)
      if(_coverage_type != COVERAGE_OFF) begin
        if(_sample_type inside {FD_RO_SAMPLE, RO_SAMPLE})
        `CREATE_OP_SAMPLE_COV(RAL_WRITE)
        else if(_sample_type inside {FD_WO_SAMPLE, WO_SAMPLE})
        `CREATE_OP_SAMPLE_COV(RAL_READ)
        else
        `CREATE_OP_SAMPLE_COV(RAL_NONE)
        `CREATE_DESIRED_COV
        `CREATE_ACTUAL_COV
      end
      `endif

  endfunction

  `ifdef SLA_RAL_COVERAGE
  `CREATE_RAL_OP_COVERGROUP
  covergroup desired_cg @(cov_ev);
     option.per_instance = 1;
     option.name = {sla_ral_file::regfilename,".",regname};
     `RAL_FIELD_CP(TRIG_EVT_POL, TRIG_EVT_POL.desired)
     `RAL_FIELD_CP_16(TRIG_EVT_POL, TRIG_EVT_POL.desired, 0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)
     `RAL_FIELD_CP(RSVD_31_16, RSVD_31_16.desired)
     `RAL_FIELD_CP_16(RSVD_31_16, RSVD_31_16.desired, 0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)
  endgroup
  covergroup actual_cg @(cov_ev);
     option.per_instance = 1;
     option.name = {sla_ral_file::regfilename,".",regname};
     `RAL_FIELD_CP(TRIG_EVT_POL, TRIG_EVT_POL.actual)
     `RAL_FIELD_CP_16(TRIG_EVT_POL, TRIG_EVT_POL.actual, 0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)
     `RAL_FIELD_CP(RSVD_31_16, RSVD_31_16.actual)
     `RAL_FIELD_CP_16(RSVD_31_16, RSVD_31_16.actual, 0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)
  endgroup
  `endif

  // --------------------------
  // add constraint based on database legal values
  // --------------------------
  function void build();
    TRIG_EVT_POL = new("TRIG_EVT_POL", "RW", 16, 0, {"SEQENG_EVT_POL.TRIG_EVT_POL"});
    TRIG_EVT_POL.set_powerwell("primary");
    TRIG_EVT_POL.set_rand_mode(0);
   TRIG_EVT_POL.set_reset_signame("pmu_rst_b");
    void'(add_field( TRIG_EVT_POL ));

    RSVD_31_16 = new("RSVD_31_16", "RO", 16, 16, {"SEQENG_EVT_POL.RSVD_31_16"});
    RSVD_31_16.set_powerwell("primary");
    RSVD_31_16.set_rand_mode(0);
    void'(add_field( RSVD_31_16 ));

  endfunction


`DEFINE_NONE_SCOPE_REG_API(pmu_mmr_SEQENG_EVT_POL_reg) 
endclass : pmu_mmr_SEQENG_EVT_POL_reg

// ================================================

class pmu_mmr_FUSE_PULL_STS_reg extends sla_ral_reg;

  // --------------------------
  sla_ral_field FP_FUSE_ERR;
  sla_ral_field RSVD_7_1;
  sla_ral_field FP_FUSE_ERR_TYP;
  sla_ral_field FP_STRP_ERR;
  sla_ral_field RSVD_23_17;
  sla_ral_field FP_STRP_ERR_TYP;

  // --------------------------
  `ovm_object_utils(pmu_mmr_FUSE_PULL_STS_reg)

  // --------------------------
  function new(
                string         name = "",
                `ifdef SLA_RAL_COVERAGE
                sla_ral_coverage_t cov_t = COVERAGE_OFF,
                `endif
                int            bus_num=0,
                int            dev_num=0,
                int            func_num=0,
                int            offset=0,
                int            size=0,
                sla_ral_data_t reset_val=0,
                boolean_t      mon_enabled = SLA_FALSE
             );
      super.new();
      set_space_addr("CFG",undef);
      set_space_addr("MEM",undef);
      set_space_addr("IO",undef);
      set_space_addr("MSG",undef);
      set_space_addr("CREG",undef); 
      set_space_addr("LT_MEM",undef);    
 build();

      `ifdef SLA_RAL_COVERAGE
      `CONFIG_RAL_COVERAGE(regname, COVERAGE_ON, FD_RW_SAMPLE)
      if(_coverage_type != COVERAGE_OFF) begin
        if(_sample_type inside {FD_RO_SAMPLE, RO_SAMPLE})
        `CREATE_OP_SAMPLE_COV(RAL_WRITE)
        else if(_sample_type inside {FD_WO_SAMPLE, WO_SAMPLE})
        `CREATE_OP_SAMPLE_COV(RAL_READ)
        else
        `CREATE_OP_SAMPLE_COV(RAL_NONE)
        `CREATE_DESIRED_COV
        `CREATE_ACTUAL_COV
      end
      `endif

  endfunction

  `ifdef SLA_RAL_COVERAGE
  `CREATE_RAL_OP_COVERGROUP
  covergroup desired_cg @(cov_ev);
     option.per_instance = 1;
     option.name = {sla_ral_file::regfilename,".",regname};
     `RAL_FIELD_CP(FP_FUSE_ERR, FP_FUSE_ERR.desired)
     `RAL_FIELD_CP_1(FP_FUSE_ERR, FP_FUSE_ERR.desired, 0)
     `RAL_FIELD_CP(RSVD_7_1, RSVD_7_1.desired)
     `RAL_FIELD_CP_7(RSVD_7_1, RSVD_7_1.desired, 0,1,2,3,4,5,6)
     `RAL_FIELD_CP(FP_FUSE_ERR_TYP, FP_FUSE_ERR_TYP.desired)
     `RAL_FIELD_CP_8(FP_FUSE_ERR_TYP, FP_FUSE_ERR_TYP.desired, 0,1,2,3,4,5,6,7)
     `RAL_FIELD_CP(FP_STRP_ERR, FP_STRP_ERR.desired)
     `RAL_FIELD_CP_1(FP_STRP_ERR, FP_STRP_ERR.desired, 0)
     `RAL_FIELD_CP(RSVD_23_17, RSVD_23_17.desired)
     `RAL_FIELD_CP_7(RSVD_23_17, RSVD_23_17.desired, 0,1,2,3,4,5,6)
     `RAL_FIELD_CP(FP_STRP_ERR_TYP, FP_STRP_ERR_TYP.desired)
     `RAL_FIELD_CP_8(FP_STRP_ERR_TYP, FP_STRP_ERR_TYP.desired, 0,1,2,3,4,5,6,7)
  endgroup
  covergroup actual_cg @(cov_ev);
     option.per_instance = 1;
     option.name = {sla_ral_file::regfilename,".",regname};
     `RAL_FIELD_CP(FP_FUSE_ERR, FP_FUSE_ERR.actual)
     `RAL_FIELD_CP_1(FP_FUSE_ERR, FP_FUSE_ERR.actual, 0)
     `RAL_FIELD_CP(RSVD_7_1, RSVD_7_1.actual)
     `RAL_FIELD_CP_7(RSVD_7_1, RSVD_7_1.actual, 0,1,2,3,4,5,6)
     `RAL_FIELD_CP(FP_FUSE_ERR_TYP, FP_FUSE_ERR_TYP.actual)
     `RAL_FIELD_CP_8(FP_FUSE_ERR_TYP, FP_FUSE_ERR_TYP.actual, 0,1,2,3,4,5,6,7)
     `RAL_FIELD_CP(FP_STRP_ERR, FP_STRP_ERR.actual)
     `RAL_FIELD_CP_1(FP_STRP_ERR, FP_STRP_ERR.actual, 0)
     `RAL_FIELD_CP(RSVD_23_17, RSVD_23_17.actual)
     `RAL_FIELD_CP_7(RSVD_23_17, RSVD_23_17.actual, 0,1,2,3,4,5,6)
     `RAL_FIELD_CP(FP_STRP_ERR_TYP, FP_STRP_ERR_TYP.actual)
     `RAL_FIELD_CP_8(FP_STRP_ERR_TYP, FP_STRP_ERR_TYP.actual, 0,1,2,3,4,5,6,7)
  endgroup
  `endif

  // --------------------------
  // add constraint based on database legal values
  // --------------------------
  function void build();
    FP_FUSE_ERR = new("FP_FUSE_ERR", "RO/V", 1, 0, {"FUSE_PULL_STS.FP_FUSE_ERR"});
    FP_FUSE_ERR.set_powerwell("primary");
    FP_FUSE_ERR.set_rand_mode(0);
    void'(add_field( FP_FUSE_ERR ));

    RSVD_7_1 = new("RSVD_7_1", "RO", 7, 1, {"FUSE_PULL_STS.RSVD_7_1"});
    RSVD_7_1.set_powerwell("primary");
    RSVD_7_1.set_rand_mode(0);
    void'(add_field( RSVD_7_1 ));

    FP_FUSE_ERR_TYP = new("FP_FUSE_ERR_TYP", "RO/V", 8, 8, {"FUSE_PULL_STS.FP_FUSE_ERR_TYP"});
    FP_FUSE_ERR_TYP.set_powerwell("primary");
    FP_FUSE_ERR_TYP.set_rand_mode(0);
    void'(add_field( FP_FUSE_ERR_TYP ));

    FP_STRP_ERR = new("FP_STRP_ERR", "RO/V", 1, 16, {"FUSE_PULL_STS.FP_STRP_ERR"});
    FP_STRP_ERR.set_powerwell("primary");
    FP_STRP_ERR.set_rand_mode(0);
    void'(add_field( FP_STRP_ERR ));

    RSVD_23_17 = new("RSVD_23_17", "RO", 7, 17, {"FUSE_PULL_STS.RSVD_23_17"});
    RSVD_23_17.set_powerwell("primary");
    RSVD_23_17.set_rand_mode(0);
    void'(add_field( RSVD_23_17 ));

    FP_STRP_ERR_TYP = new("FP_STRP_ERR_TYP", "RO/V", 8, 24, {"FUSE_PULL_STS.FP_STRP_ERR_TYP"});
    FP_STRP_ERR_TYP.set_powerwell("primary");
    FP_STRP_ERR_TYP.set_rand_mode(0);
    void'(add_field( FP_STRP_ERR_TYP ));

  endfunction


`DEFINE_NONE_SCOPE_REG_API(pmu_mmr_FUSE_PULL_STS_reg) 
endclass : pmu_mmr_FUSE_PULL_STS_reg

// ================================================

class pmu_mmr_FW_CPL_STS_reg extends sla_ral_reg;

  // --------------------------
  sla_ral_field FW_CPL_ERR;
  sla_ral_field CPL_SEQ_ID;
  sla_ral_field CPL_SEQ_LN;
  sla_ral_field RSVD_31_18;

  // --------------------------
  `ovm_object_utils(pmu_mmr_FW_CPL_STS_reg)

  // --------------------------
  function new(
                string         name = "",
                `ifdef SLA_RAL_COVERAGE
                sla_ral_coverage_t cov_t = COVERAGE_OFF,
                `endif
                int            bus_num=0,
                int            dev_num=0,
                int            func_num=0,
                int            offset=0,
                int            size=0,
                sla_ral_data_t reset_val=0,
                boolean_t      mon_enabled = SLA_FALSE
             );
      super.new();
      set_space_addr("CFG",undef);
      set_space_addr("MEM",undef);
      set_space_addr("IO",undef);
      set_space_addr("MSG",undef);
      set_space_addr("CREG",undef); 
      set_space_addr("LT_MEM",undef);    
 build();

      `ifdef SLA_RAL_COVERAGE
      `CONFIG_RAL_COVERAGE(regname, COVERAGE_ON, FD_RW_SAMPLE)
      if(_coverage_type != COVERAGE_OFF) begin
        if(_sample_type inside {FD_RO_SAMPLE, RO_SAMPLE})
        `CREATE_OP_SAMPLE_COV(RAL_WRITE)
        else if(_sample_type inside {FD_WO_SAMPLE, WO_SAMPLE})
        `CREATE_OP_SAMPLE_COV(RAL_READ)
        else
        `CREATE_OP_SAMPLE_COV(RAL_NONE)
        `CREATE_DESIRED_COV
        `CREATE_ACTUAL_COV
      end
      `endif

  endfunction

  `ifdef SLA_RAL_COVERAGE
  `CREATE_RAL_OP_COVERGROUP
  covergroup desired_cg @(cov_ev);
     option.per_instance = 1;
     option.name = {sla_ral_file::regfilename,".",regname};
     `RAL_FIELD_CP(FW_CPL_ERR, FW_CPL_ERR.desired)
     `RAL_FIELD_CP_2(FW_CPL_ERR, FW_CPL_ERR.desired, 0,1)
     `RAL_FIELD_CP(CPL_SEQ_ID, CPL_SEQ_ID.desired)
     `RAL_FIELD_CP_5(CPL_SEQ_ID, CPL_SEQ_ID.desired, 0,1,2,3,4)
     `RAL_FIELD_CP(CPL_SEQ_LN, CPL_SEQ_LN.desired)
     `RAL_FIELD_CP_9(CPL_SEQ_LN, CPL_SEQ_LN.desired, 0,1,2,3,4,5,6,7,8)
     `RAL_FIELD_CP(RSVD_31_18, RSVD_31_18.desired)
     `RAL_FIELD_CP_16(RSVD_31_18, RSVD_31_18.desired, 0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)
  endgroup
  covergroup actual_cg @(cov_ev);
     option.per_instance = 1;
     option.name = {sla_ral_file::regfilename,".",regname};
     `RAL_FIELD_CP(FW_CPL_ERR, FW_CPL_ERR.actual)
     `RAL_FIELD_CP_2(FW_CPL_ERR, FW_CPL_ERR.actual, 0,1)
     `RAL_FIELD_CP(CPL_SEQ_ID, CPL_SEQ_ID.actual)
     `RAL_FIELD_CP_5(CPL_SEQ_ID, CPL_SEQ_ID.actual, 0,1,2,3,4)
     `RAL_FIELD_CP(CPL_SEQ_LN, CPL_SEQ_LN.actual)
     `RAL_FIELD_CP_9(CPL_SEQ_LN, CPL_SEQ_LN.actual, 0,1,2,3,4,5,6,7,8)
     `RAL_FIELD_CP(RSVD_31_18, RSVD_31_18.actual)
     `RAL_FIELD_CP_16(RSVD_31_18, RSVD_31_18.actual, 0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)
  endgroup
  `endif

  // --------------------------
  // add constraint based on database legal values
  // --------------------------
  function void build();
    FW_CPL_ERR = new("FW_CPL_ERR", "RW/1C/V", 2, 0, {"FW_CPL_STS.FW_CPL_ERR"});
    FW_CPL_ERR.set_powerwell("primary");
    FW_CPL_ERR.set_rand_mode(0);
   FW_CPL_ERR.set_reset_signame("pmu_rst_b");
    void'(add_field( FW_CPL_ERR ));

    CPL_SEQ_ID = new("CPL_SEQ_ID", "RW/1C/V", 5, 2, {"FW_CPL_STS.CPL_SEQ_ID"});
    CPL_SEQ_ID.set_powerwell("primary");
    CPL_SEQ_ID.set_rand_mode(0);
   CPL_SEQ_ID.set_reset_signame("pmu_rst_b");
    void'(add_field( CPL_SEQ_ID ));

    CPL_SEQ_LN = new("CPL_SEQ_LN", "RW/1C/V", 9, 7, {"FW_CPL_STS.CPL_SEQ_LN"});
    CPL_SEQ_LN.set_powerwell("primary");
    CPL_SEQ_LN.set_rand_mode(0);
   CPL_SEQ_LN.set_reset_signame("pmu_rst_b");
    void'(add_field( CPL_SEQ_LN ));

    RSVD_31_18 = new("RSVD_31_18", "RO", 16, 16, {"FW_CPL_STS.RSVD_31_18"});
    RSVD_31_18.set_powerwell("primary");
    RSVD_31_18.set_rand_mode(0);
    void'(add_field( RSVD_31_18 ));

  endfunction


`DEFINE_NONE_SCOPE_REG_API(pmu_mmr_FW_CPL_STS_reg) 
endclass : pmu_mmr_FW_CPL_STS_reg

// ================================================

class pmu_mmr_PTCH_CPL_STS_reg extends sla_ral_reg;

  // --------------------------
  sla_ral_field PTCH_FTCH_ERR;
  sla_ral_field PTCH_FTCH_ADDR;
  sla_ral_field PTCH_FTCH_SEQ_ID;
  sla_ral_field PTCH_FTCH_SEQ_LN;

  // --------------------------
  `ovm_object_utils(pmu_mmr_PTCH_CPL_STS_reg)

  // --------------------------
  function new(
                string         name = "",
                `ifdef SLA_RAL_COVERAGE
                sla_ral_coverage_t cov_t = COVERAGE_OFF,
                `endif
                int            bus_num=0,
                int            dev_num=0,
                int            func_num=0,
                int            offset=0,
                int            size=0,
                sla_ral_data_t reset_val=0,
                boolean_t      mon_enabled = SLA_FALSE
             );
      super.new();
      set_space_addr("CFG",undef);
      set_space_addr("MEM",undef);
      set_space_addr("IO",undef);
      set_space_addr("MSG",undef);
      set_space_addr("CREG",undef); 
      set_space_addr("LT_MEM",undef);    
 build();

      `ifdef SLA_RAL_COVERAGE
      `CONFIG_RAL_COVERAGE(regname, COVERAGE_ON, FD_RW_SAMPLE)
      if(_coverage_type != COVERAGE_OFF) begin
        if(_sample_type inside {FD_RO_SAMPLE, RO_SAMPLE})
        `CREATE_OP_SAMPLE_COV(RAL_WRITE)
        else if(_sample_type inside {FD_WO_SAMPLE, WO_SAMPLE})
        `CREATE_OP_SAMPLE_COV(RAL_READ)
        else
        `CREATE_OP_SAMPLE_COV(RAL_NONE)
        `CREATE_DESIRED_COV
        `CREATE_ACTUAL_COV
      end
      `endif

  endfunction

  `ifdef SLA_RAL_COVERAGE
  `CREATE_RAL_OP_COVERGROUP
  covergroup desired_cg @(cov_ev);
     option.per_instance = 1;
     option.name = {sla_ral_file::regfilename,".",regname};
     `RAL_FIELD_CP(PTCH_FTCH_ERR, PTCH_FTCH_ERR.desired)
     `RAL_FIELD_CP_2(PTCH_FTCH_ERR, PTCH_FTCH_ERR.desired, 0,1)
     `RAL_FIELD_CP(PTCH_FTCH_ADDR, PTCH_FTCH_ADDR.desired)
     `RAL_FIELD_CP_16(PTCH_FTCH_ADDR, PTCH_FTCH_ADDR.desired, 0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)
     `RAL_FIELD_CP(PTCH_FTCH_SEQ_ID, PTCH_FTCH_SEQ_ID.desired)
     `RAL_FIELD_CP_5(PTCH_FTCH_SEQ_ID, PTCH_FTCH_SEQ_ID.desired, 0,1,2,3,4)
     `RAL_FIELD_CP(PTCH_FTCH_SEQ_LN, PTCH_FTCH_SEQ_LN.desired)
     `RAL_FIELD_CP_9(PTCH_FTCH_SEQ_LN, PTCH_FTCH_SEQ_LN.desired, 0,1,2,3,4,5,6,7,8)
  endgroup
  covergroup actual_cg @(cov_ev);
     option.per_instance = 1;
     option.name = {sla_ral_file::regfilename,".",regname};
     `RAL_FIELD_CP(PTCH_FTCH_ERR, PTCH_FTCH_ERR.actual)
     `RAL_FIELD_CP_2(PTCH_FTCH_ERR, PTCH_FTCH_ERR.actual, 0,1)
     `RAL_FIELD_CP(PTCH_FTCH_ADDR, PTCH_FTCH_ADDR.actual)
     `RAL_FIELD_CP_16(PTCH_FTCH_ADDR, PTCH_FTCH_ADDR.actual, 0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)
     `RAL_FIELD_CP(PTCH_FTCH_SEQ_ID, PTCH_FTCH_SEQ_ID.actual)
     `RAL_FIELD_CP_5(PTCH_FTCH_SEQ_ID, PTCH_FTCH_SEQ_ID.actual, 0,1,2,3,4)
     `RAL_FIELD_CP(PTCH_FTCH_SEQ_LN, PTCH_FTCH_SEQ_LN.actual)
     `RAL_FIELD_CP_9(PTCH_FTCH_SEQ_LN, PTCH_FTCH_SEQ_LN.actual, 0,1,2,3,4,5,6,7,8)
  endgroup
  `endif

  // --------------------------
  // add constraint based on database legal values
  // --------------------------
  function void build();
    PTCH_FTCH_ERR = new("PTCH_FTCH_ERR", "RW/1C/V", 2, 0, {"PTCH_CPL_STS.PTCH_FTCH_ERR"});
    PTCH_FTCH_ERR.set_powerwell("primary");
    PTCH_FTCH_ERR.set_rand_mode(0);
   PTCH_FTCH_ERR.set_reset_signame("pmu_rst_b");
    void'(add_field( PTCH_FTCH_ERR ));

    PTCH_FTCH_ADDR = new("PTCH_FTCH_ADDR", "RW/1C/V", 16, 2, {"PTCH_CPL_STS.PTCH_FTCH_ADDR"});
    PTCH_FTCH_ADDR.set_powerwell("primary");
    PTCH_FTCH_ADDR.set_rand_mode(0);
   PTCH_FTCH_ADDR.set_reset_signame("pmu_rst_b");
    void'(add_field( PTCH_FTCH_ADDR ));

    PTCH_FTCH_SEQ_ID = new("PTCH_FTCH_SEQ_ID", "RW/1C/V", 5, 18, {"PTCH_CPL_STS.PTCH_FTCH_SEQ_ID"});
    PTCH_FTCH_SEQ_ID.set_powerwell("primary");
    PTCH_FTCH_SEQ_ID.set_rand_mode(0);
   PTCH_FTCH_SEQ_ID.set_reset_signame("pmu_rst_b");
    void'(add_field( PTCH_FTCH_SEQ_ID ));

    PTCH_FTCH_SEQ_LN = new("PTCH_FTCH_SEQ_LN", "RW/1C/V", 9, 23, {"PTCH_CPL_STS.PTCH_FTCH_SEQ_LN"});
    PTCH_FTCH_SEQ_LN.set_powerwell("primary");
    PTCH_FTCH_SEQ_LN.set_rand_mode(0);
   PTCH_FTCH_SEQ_LN.set_reset_signame("pmu_rst_b");
    void'(add_field( PTCH_FTCH_SEQ_LN ));

  endfunction


`DEFINE_NONE_SCOPE_REG_API(pmu_mmr_PTCH_CPL_STS_reg) 
endclass : pmu_mmr_PTCH_CPL_STS_reg

// ================================================

class pmu_mmr_CH_CPL_STS_reg extends sla_ral_reg;

  // --------------------------
  sla_ral_field FIRST_ERR;
  sla_ral_field FIRST_ERR_SRC;
  sla_ral_field LAST_ERR;
  sla_ral_field LAST_ERR_SRC;

  // --------------------------
  `ovm_object_utils(pmu_mmr_CH_CPL_STS_reg)

  // --------------------------
  function new(
                string         name = "",
                `ifdef SLA_RAL_COVERAGE
                sla_ral_coverage_t cov_t = COVERAGE_OFF,
                `endif
                int            bus_num=0,
                int            dev_num=0,
                int            func_num=0,
                int            offset=0,
                int            size=0,
                sla_ral_data_t reset_val=0,
                boolean_t      mon_enabled = SLA_FALSE
             );
      super.new();
      set_space_addr("CFG",undef);
      set_space_addr("MEM",undef);
      set_space_addr("IO",undef);
      set_space_addr("MSG",undef);
      set_space_addr("CREG",undef); 
      set_space_addr("LT_MEM",undef);    
 build();

      `ifdef SLA_RAL_COVERAGE
      `CONFIG_RAL_COVERAGE(regname, COVERAGE_ON, FD_RW_SAMPLE)
      if(_coverage_type != COVERAGE_OFF) begin
        if(_sample_type inside {FD_RO_SAMPLE, RO_SAMPLE})
        `CREATE_OP_SAMPLE_COV(RAL_WRITE)
        else if(_sample_type inside {FD_WO_SAMPLE, WO_SAMPLE})
        `CREATE_OP_SAMPLE_COV(RAL_READ)
        else
        `CREATE_OP_SAMPLE_COV(RAL_NONE)
        `CREATE_DESIRED_COV
        `CREATE_ACTUAL_COV
      end
      `endif

  endfunction

  `ifdef SLA_RAL_COVERAGE
  `CREATE_RAL_OP_COVERGROUP
  covergroup desired_cg @(cov_ev);
     option.per_instance = 1;
     option.name = {sla_ral_file::regfilename,".",regname};
     `RAL_FIELD_CP(FIRST_ERR, FIRST_ERR.desired)
     `RAL_FIELD_CP_2(FIRST_ERR, FIRST_ERR.desired, 0,1)
     `RAL_FIELD_CP(FIRST_ERR_SRC, FIRST_ERR_SRC.desired)
     `RAL_FIELD_CP_8(FIRST_ERR_SRC, FIRST_ERR_SRC.desired, 0,1,2,3,4,5,6,7)
     `RAL_FIELD_CP(LAST_ERR, LAST_ERR.desired)
     `RAL_FIELD_CP_2(LAST_ERR, LAST_ERR.desired, 0,1)
     `RAL_FIELD_CP(LAST_ERR_SRC, LAST_ERR_SRC.desired)
     `RAL_FIELD_CP_8(LAST_ERR_SRC, LAST_ERR_SRC.desired, 0,1,2,3,4,5,6,7)
  endgroup
  covergroup actual_cg @(cov_ev);
     option.per_instance = 1;
     option.name = {sla_ral_file::regfilename,".",regname};
     `RAL_FIELD_CP(FIRST_ERR, FIRST_ERR.actual)
     `RAL_FIELD_CP_2(FIRST_ERR, FIRST_ERR.actual, 0,1)
     `RAL_FIELD_CP(FIRST_ERR_SRC, FIRST_ERR_SRC.actual)
     `RAL_FIELD_CP_8(FIRST_ERR_SRC, FIRST_ERR_SRC.actual, 0,1,2,3,4,5,6,7)
     `RAL_FIELD_CP(LAST_ERR, LAST_ERR.actual)
     `RAL_FIELD_CP_2(LAST_ERR, LAST_ERR.actual, 0,1)
     `RAL_FIELD_CP(LAST_ERR_SRC, LAST_ERR_SRC.actual)
     `RAL_FIELD_CP_8(LAST_ERR_SRC, LAST_ERR_SRC.actual, 0,1,2,3,4,5,6,7)
  endgroup
  `endif

  // --------------------------
  // add constraint based on database legal values
  // --------------------------
  function void build();
    FIRST_ERR = new("FIRST_ERR", "RW/1C/V", 2, 0, {"CH_CPL_STS.FIRST_ERR"});
    FIRST_ERR.set_powerwell("primary");
    FIRST_ERR.set_rand_mode(0);
   FIRST_ERR.set_reset_signame("pmu_rst_b");
    void'(add_field( FIRST_ERR ));

    FIRST_ERR_SRC = new("FIRST_ERR_SRC", "RW/1C/V", 8, 8, {"CH_CPL_STS.FIRST_ERR_SRC"});
    FIRST_ERR_SRC.set_powerwell("primary");
    FIRST_ERR_SRC.set_rand_mode(0);
   FIRST_ERR_SRC.set_reset_signame("pmu_rst_b");
    void'(add_field( FIRST_ERR_SRC ));

    LAST_ERR = new("LAST_ERR", "RW/1C/V", 2, 16, {"CH_CPL_STS.LAST_ERR"});
    LAST_ERR.set_powerwell("primary");
    LAST_ERR.set_rand_mode(0);
   LAST_ERR.set_reset_signame("pmu_rst_b");
    void'(add_field( LAST_ERR ));

    LAST_ERR_SRC = new("LAST_ERR_SRC", "RW/1C/V", 8, 24, {"CH_CPL_STS.LAST_ERR_SRC"});
    LAST_ERR_SRC.set_powerwell("primary");
    LAST_ERR_SRC.set_rand_mode(0);
   LAST_ERR_SRC.set_reset_signame("pmu_rst_b");
    void'(add_field( LAST_ERR_SRC ));

  endfunction


`DEFINE_NONE_SCOPE_REG_API(pmu_mmr_CH_CPL_STS_reg) 
endclass : pmu_mmr_CH_CPL_STS_reg

// ================================================

class pmu_mmr_IPRDY_STS_reg extends sla_ral_reg;

  // --------------------------
  sla_ral_field FIRST_ERR;
  sla_ral_field FIRST_ERR_SRC;
  sla_ral_field LAST_ERR;
  sla_ral_field LAST_ERR_SRC;

  // --------------------------
  `ovm_object_utils(pmu_mmr_IPRDY_STS_reg)

  // --------------------------
  function new(
                string         name = "",
                `ifdef SLA_RAL_COVERAGE
                sla_ral_coverage_t cov_t = COVERAGE_OFF,
                `endif
                int            bus_num=0,
                int            dev_num=0,
                int            func_num=0,
                int            offset=0,
                int            size=0,
                sla_ral_data_t reset_val=0,
                boolean_t      mon_enabled = SLA_FALSE
             );
      super.new();
      set_space_addr("CFG",undef);
      set_space_addr("MEM",undef);
      set_space_addr("IO",undef);
      set_space_addr("MSG",undef);
      set_space_addr("CREG",undef); 
      set_space_addr("LT_MEM",undef);    
 build();

      `ifdef SLA_RAL_COVERAGE
      `CONFIG_RAL_COVERAGE(regname, COVERAGE_ON, FD_RW_SAMPLE)
      if(_coverage_type != COVERAGE_OFF) begin
        if(_sample_type inside {FD_RO_SAMPLE, RO_SAMPLE})
        `CREATE_OP_SAMPLE_COV(RAL_WRITE)
        else if(_sample_type inside {FD_WO_SAMPLE, WO_SAMPLE})
        `CREATE_OP_SAMPLE_COV(RAL_READ)
        else
        `CREATE_OP_SAMPLE_COV(RAL_NONE)
        `CREATE_DESIRED_COV
        `CREATE_ACTUAL_COV
      end
      `endif

  endfunction

  `ifdef SLA_RAL_COVERAGE
  `CREATE_RAL_OP_COVERGROUP
  covergroup desired_cg @(cov_ev);
     option.per_instance = 1;
     option.name = {sla_ral_file::regfilename,".",regname};
     `RAL_FIELD_CP(FIRST_ERR, FIRST_ERR.desired)
     `RAL_FIELD_CP_2(FIRST_ERR, FIRST_ERR.desired, 0,1)
     `RAL_FIELD_CP(FIRST_ERR_SRC, FIRST_ERR_SRC.desired)
     `RAL_FIELD_CP_8(FIRST_ERR_SRC, FIRST_ERR_SRC.desired, 0,1,2,3,4,5,6,7)
     `RAL_FIELD_CP(LAST_ERR, LAST_ERR.desired)
     `RAL_FIELD_CP_2(LAST_ERR, LAST_ERR.desired, 0,1)
     `RAL_FIELD_CP(LAST_ERR_SRC, LAST_ERR_SRC.desired)
     `RAL_FIELD_CP_8(LAST_ERR_SRC, LAST_ERR_SRC.desired, 0,1,2,3,4,5,6,7)
  endgroup
  covergroup actual_cg @(cov_ev);
     option.per_instance = 1;
     option.name = {sla_ral_file::regfilename,".",regname};
     `RAL_FIELD_CP(FIRST_ERR, FIRST_ERR.actual)
     `RAL_FIELD_CP_2(FIRST_ERR, FIRST_ERR.actual, 0,1)
     `RAL_FIELD_CP(FIRST_ERR_SRC, FIRST_ERR_SRC.actual)
     `RAL_FIELD_CP_8(FIRST_ERR_SRC, FIRST_ERR_SRC.actual, 0,1,2,3,4,5,6,7)
     `RAL_FIELD_CP(LAST_ERR, LAST_ERR.actual)
     `RAL_FIELD_CP_2(LAST_ERR, LAST_ERR.actual, 0,1)
     `RAL_FIELD_CP(LAST_ERR_SRC, LAST_ERR_SRC.actual)
     `RAL_FIELD_CP_8(LAST_ERR_SRC, LAST_ERR_SRC.actual, 0,1,2,3,4,5,6,7)
  endgroup
  `endif

  // --------------------------
  // add constraint based on database legal values
  // --------------------------
  function void build();
    FIRST_ERR = new("FIRST_ERR", "RW/1C/V", 2, 0, {"IPRDY_STS.FIRST_ERR"});
    FIRST_ERR.set_powerwell("primary");
    FIRST_ERR.set_rand_mode(0);
   FIRST_ERR.set_reset_signame("pmu_rst_b");
    void'(add_field( FIRST_ERR ));

    FIRST_ERR_SRC = new("FIRST_ERR_SRC", "RW/1C/V", 8, 8, {"IPRDY_STS.FIRST_ERR_SRC"});
    FIRST_ERR_SRC.set_powerwell("primary");
    FIRST_ERR_SRC.set_rand_mode(0);
   FIRST_ERR_SRC.set_reset_signame("pmu_rst_b");
    void'(add_field( FIRST_ERR_SRC ));

    LAST_ERR = new("LAST_ERR", "RW/1C/V", 2, 16, {"IPRDY_STS.LAST_ERR"});
    LAST_ERR.set_powerwell("primary");
    LAST_ERR.set_rand_mode(0);
   LAST_ERR.set_reset_signame("pmu_rst_b");
    void'(add_field( LAST_ERR ));

    LAST_ERR_SRC = new("LAST_ERR_SRC", "RW/1C/V", 8, 24, {"IPRDY_STS.LAST_ERR_SRC"});
    LAST_ERR_SRC.set_powerwell("primary");
    LAST_ERR_SRC.set_rand_mode(0);
   LAST_ERR_SRC.set_reset_signame("pmu_rst_b");
    void'(add_field( LAST_ERR_SRC ));

  endfunction


`DEFINE_NONE_SCOPE_REG_API(pmu_mmr_IPRDY_STS_reg) 
endclass : pmu_mmr_IPRDY_STS_reg

// ================================================

class pmu_mmr_BPRP_ACK_STS_reg extends sla_ral_reg;

  // --------------------------
  sla_ral_field FIRST_ERR;
  sla_ral_field FIRST_ERR_SRC;
  sla_ral_field LAST_ERR;
  sla_ral_field LAST_ERR_SRC;

  // --------------------------
  `ovm_object_utils(pmu_mmr_BPRP_ACK_STS_reg)

  // --------------------------
  function new(
                string         name = "",
                `ifdef SLA_RAL_COVERAGE
                sla_ral_coverage_t cov_t = COVERAGE_OFF,
                `endif
                int            bus_num=0,
                int            dev_num=0,
                int            func_num=0,
                int            offset=0,
                int            size=0,
                sla_ral_data_t reset_val=0,
                boolean_t      mon_enabled = SLA_FALSE
             );
      super.new();
      set_space_addr("CFG",undef);
      set_space_addr("MEM",undef);
      set_space_addr("IO",undef);
      set_space_addr("MSG",undef);
      set_space_addr("CREG",undef); 
      set_space_addr("LT_MEM",undef);    
 build();

      `ifdef SLA_RAL_COVERAGE
      `CONFIG_RAL_COVERAGE(regname, COVERAGE_ON, FD_RW_SAMPLE)
      if(_coverage_type != COVERAGE_OFF) begin
        if(_sample_type inside {FD_RO_SAMPLE, RO_SAMPLE})
        `CREATE_OP_SAMPLE_COV(RAL_WRITE)
        else if(_sample_type inside {FD_WO_SAMPLE, WO_SAMPLE})
        `CREATE_OP_SAMPLE_COV(RAL_READ)
        else
        `CREATE_OP_SAMPLE_COV(RAL_NONE)
        `CREATE_DESIRED_COV
        `CREATE_ACTUAL_COV
      end
      `endif

  endfunction

  `ifdef SLA_RAL_COVERAGE
  `CREATE_RAL_OP_COVERGROUP
  covergroup desired_cg @(cov_ev);
     option.per_instance = 1;
     option.name = {sla_ral_file::regfilename,".",regname};
     `RAL_FIELD_CP(FIRST_ERR, FIRST_ERR.desired)
     `RAL_FIELD_CP_2(FIRST_ERR, FIRST_ERR.desired, 0,1)
     `RAL_FIELD_CP(FIRST_ERR_SRC, FIRST_ERR_SRC.desired)
     `RAL_FIELD_CP_8(FIRST_ERR_SRC, FIRST_ERR_SRC.desired, 0,1,2,3,4,5,6,7)
     `RAL_FIELD_CP(LAST_ERR, LAST_ERR.desired)
     `RAL_FIELD_CP_2(LAST_ERR, LAST_ERR.desired, 0,1)
     `RAL_FIELD_CP(LAST_ERR_SRC, LAST_ERR_SRC.desired)
     `RAL_FIELD_CP_8(LAST_ERR_SRC, LAST_ERR_SRC.desired, 0,1,2,3,4,5,6,7)
  endgroup
  covergroup actual_cg @(cov_ev);
     option.per_instance = 1;
     option.name = {sla_ral_file::regfilename,".",regname};
     `RAL_FIELD_CP(FIRST_ERR, FIRST_ERR.actual)
     `RAL_FIELD_CP_2(FIRST_ERR, FIRST_ERR.actual, 0,1)
     `RAL_FIELD_CP(FIRST_ERR_SRC, FIRST_ERR_SRC.actual)
     `RAL_FIELD_CP_8(FIRST_ERR_SRC, FIRST_ERR_SRC.actual, 0,1,2,3,4,5,6,7)
     `RAL_FIELD_CP(LAST_ERR, LAST_ERR.actual)
     `RAL_FIELD_CP_2(LAST_ERR, LAST_ERR.actual, 0,1)
     `RAL_FIELD_CP(LAST_ERR_SRC, LAST_ERR_SRC.actual)
     `RAL_FIELD_CP_8(LAST_ERR_SRC, LAST_ERR_SRC.actual, 0,1,2,3,4,5,6,7)
  endgroup
  `endif

  // --------------------------
  // add constraint based on database legal values
  // --------------------------
  function void build();
    FIRST_ERR = new("FIRST_ERR", "RW/1C/V", 2, 0, {"BPRP_ACK_STS.FIRST_ERR"});
    FIRST_ERR.set_powerwell("primary");
    FIRST_ERR.set_rand_mode(0);
   FIRST_ERR.set_reset_signame("pmu_rst_b");
    void'(add_field( FIRST_ERR ));

    FIRST_ERR_SRC = new("FIRST_ERR_SRC", "RW/1C/V", 8, 8, {"BPRP_ACK_STS.FIRST_ERR_SRC"});
    FIRST_ERR_SRC.set_powerwell("primary");
    FIRST_ERR_SRC.set_rand_mode(0);
   FIRST_ERR_SRC.set_reset_signame("pmu_rst_b");
    void'(add_field( FIRST_ERR_SRC ));

    LAST_ERR = new("LAST_ERR", "RW/1C/V", 2, 16, {"BPRP_ACK_STS.LAST_ERR"});
    LAST_ERR.set_powerwell("primary");
    LAST_ERR.set_rand_mode(0);
   LAST_ERR.set_reset_signame("pmu_rst_b");
    void'(add_field( LAST_ERR ));

    LAST_ERR_SRC = new("LAST_ERR_SRC", "RW/1C/V", 8, 24, {"BPRP_ACK_STS.LAST_ERR_SRC"});
    LAST_ERR_SRC.set_powerwell("primary");
    LAST_ERR_SRC.set_rand_mode(0);
   LAST_ERR_SRC.set_reset_signame("pmu_rst_b");
    void'(add_field( LAST_ERR_SRC ));

  endfunction


`DEFINE_NONE_SCOPE_REG_API(pmu_mmr_BPRP_ACK_STS_reg) 
endclass : pmu_mmr_BPRP_ACK_STS_reg

// ================================================

class pmu_mmr_PMETO_STS_reg extends sla_ral_reg;

  // --------------------------
  sla_ral_field FIRST_ERR;
  sla_ral_field FIRST_ERR_SRC;
  sla_ral_field LAST_ERR;
  sla_ral_field LAST_ERR_SRC;

  // --------------------------
  `ovm_object_utils(pmu_mmr_PMETO_STS_reg)

  // --------------------------
  function new(
                string         name = "",
                `ifdef SLA_RAL_COVERAGE
                sla_ral_coverage_t cov_t = COVERAGE_OFF,
                `endif
                int            bus_num=0,
                int            dev_num=0,
                int            func_num=0,
                int            offset=0,
                int            size=0,
                sla_ral_data_t reset_val=0,
                boolean_t      mon_enabled = SLA_FALSE
             );
      super.new();
      set_space_addr("CFG",undef);
      set_space_addr("MEM",undef);
      set_space_addr("IO",undef);
      set_space_addr("MSG",undef);
      set_space_addr("CREG",undef); 
      set_space_addr("LT_MEM",undef);    
 build();

      `ifdef SLA_RAL_COVERAGE
      `CONFIG_RAL_COVERAGE(regname, COVERAGE_ON, FD_RW_SAMPLE)
      if(_coverage_type != COVERAGE_OFF) begin
        if(_sample_type inside {FD_RO_SAMPLE, RO_SAMPLE})
        `CREATE_OP_SAMPLE_COV(RAL_WRITE)
        else if(_sample_type inside {FD_WO_SAMPLE, WO_SAMPLE})
        `CREATE_OP_SAMPLE_COV(RAL_READ)
        else
        `CREATE_OP_SAMPLE_COV(RAL_NONE)
        `CREATE_DESIRED_COV
        `CREATE_ACTUAL_COV
      end
      `endif

  endfunction

  `ifdef SLA_RAL_COVERAGE
  `CREATE_RAL_OP_COVERGROUP
  covergroup desired_cg @(cov_ev);
     option.per_instance = 1;
     option.name = {sla_ral_file::regfilename,".",regname};
     `RAL_FIELD_CP(FIRST_ERR, FIRST_ERR.desired)
     `RAL_FIELD_CP_3(FIRST_ERR, FIRST_ERR.desired, 0,1,2)
     `RAL_FIELD_CP(FIRST_ERR_SRC, FIRST_ERR_SRC.desired)
     `RAL_FIELD_CP_8(FIRST_ERR_SRC, FIRST_ERR_SRC.desired, 0,1,2,3,4,5,6,7)
     `RAL_FIELD_CP(LAST_ERR, LAST_ERR.desired)
     `RAL_FIELD_CP_3(LAST_ERR, LAST_ERR.desired, 0,1,2)
     `RAL_FIELD_CP(LAST_ERR_SRC, LAST_ERR_SRC.desired)
     `RAL_FIELD_CP_8(LAST_ERR_SRC, LAST_ERR_SRC.desired, 0,1,2,3,4,5,6,7)
  endgroup
  covergroup actual_cg @(cov_ev);
     option.per_instance = 1;
     option.name = {sla_ral_file::regfilename,".",regname};
     `RAL_FIELD_CP(FIRST_ERR, FIRST_ERR.actual)
     `RAL_FIELD_CP_3(FIRST_ERR, FIRST_ERR.actual, 0,1,2)
     `RAL_FIELD_CP(FIRST_ERR_SRC, FIRST_ERR_SRC.actual)
     `RAL_FIELD_CP_8(FIRST_ERR_SRC, FIRST_ERR_SRC.actual, 0,1,2,3,4,5,6,7)
     `RAL_FIELD_CP(LAST_ERR, LAST_ERR.actual)
     `RAL_FIELD_CP_3(LAST_ERR, LAST_ERR.actual, 0,1,2)
     `RAL_FIELD_CP(LAST_ERR_SRC, LAST_ERR_SRC.actual)
     `RAL_FIELD_CP_8(LAST_ERR_SRC, LAST_ERR_SRC.actual, 0,1,2,3,4,5,6,7)
  endgroup
  `endif

  // --------------------------
  // add constraint based on database legal values
  // --------------------------
  function void build();
    FIRST_ERR = new("FIRST_ERR", "RW/1C/V", 3, 0, {"PMETO_STS.FIRST_ERR"});
    FIRST_ERR.set_powerwell("primary");
    FIRST_ERR.set_rand_mode(0);
   FIRST_ERR.set_reset_signame("pmu_rst_b");
    void'(add_field( FIRST_ERR ));

    FIRST_ERR_SRC = new("FIRST_ERR_SRC", "RW/1C/V", 8, 8, {"PMETO_STS.FIRST_ERR_SRC"});
    FIRST_ERR_SRC.set_powerwell("primary");
    FIRST_ERR_SRC.set_rand_mode(0);
   FIRST_ERR_SRC.set_reset_signame("pmu_rst_b");
    void'(add_field( FIRST_ERR_SRC ));

    LAST_ERR = new("LAST_ERR", "RW/1C/V", 3, 16, {"PMETO_STS.LAST_ERR"});
    LAST_ERR.set_powerwell("primary");
    LAST_ERR.set_rand_mode(0);
   LAST_ERR.set_reset_signame("pmu_rst_b");
    void'(add_field( LAST_ERR ));

    LAST_ERR_SRC = new("LAST_ERR_SRC", "RW/1C/V", 8, 24, {"PMETO_STS.LAST_ERR_SRC"});
    LAST_ERR_SRC.set_powerwell("primary");
    LAST_ERR_SRC.set_rand_mode(0);
   LAST_ERR_SRC.set_reset_signame("pmu_rst_b");
    void'(add_field( LAST_ERR_SRC ));

  endfunction


`DEFINE_NONE_SCOPE_REG_API(pmu_mmr_PMETO_STS_reg) 
endclass : pmu_mmr_PMETO_STS_reg

// ================================================

class pmu_mmr_TELEM_STS_reg extends sla_ral_reg;

  // --------------------------
  sla_ral_field FIRST_ERR;
  sla_ral_field FIRST_ERR_SRC;
  sla_ral_field LAST_ERR;
  sla_ral_field LAST_ERR_SRC;

  // --------------------------
  `ovm_object_utils(pmu_mmr_TELEM_STS_reg)

  // --------------------------
  function new(
                string         name = "",
                `ifdef SLA_RAL_COVERAGE
                sla_ral_coverage_t cov_t = COVERAGE_OFF,
                `endif
                int            bus_num=0,
                int            dev_num=0,
                int            func_num=0,
                int            offset=0,
                int            size=0,
                sla_ral_data_t reset_val=0,
                boolean_t      mon_enabled = SLA_FALSE
             );
      super.new();
      set_space_addr("CFG",undef);
      set_space_addr("MEM",undef);
      set_space_addr("IO",undef);
      set_space_addr("MSG",undef);
      set_space_addr("CREG",undef); 
      set_space_addr("LT_MEM",undef);    
 build();

      `ifdef SLA_RAL_COVERAGE
      `CONFIG_RAL_COVERAGE(regname, COVERAGE_ON, FD_RW_SAMPLE)
      if(_coverage_type != COVERAGE_OFF) begin
        if(_sample_type inside {FD_RO_SAMPLE, RO_SAMPLE})
        `CREATE_OP_SAMPLE_COV(RAL_WRITE)
        else if(_sample_type inside {FD_WO_SAMPLE, WO_SAMPLE})
        `CREATE_OP_SAMPLE_COV(RAL_READ)
        else
        `CREATE_OP_SAMPLE_COV(RAL_NONE)
        `CREATE_DESIRED_COV
        `CREATE_ACTUAL_COV
      end
      `endif

  endfunction

  `ifdef SLA_RAL_COVERAGE
  `CREATE_RAL_OP_COVERGROUP
  covergroup desired_cg @(cov_ev);
     option.per_instance = 1;
     option.name = {sla_ral_file::regfilename,".",regname};
     `RAL_FIELD_CP(FIRST_ERR, FIRST_ERR.desired)
     `RAL_FIELD_CP_2(FIRST_ERR, FIRST_ERR.desired, 0,1)
     `RAL_FIELD_CP(FIRST_ERR_SRC, FIRST_ERR_SRC.desired)
     `RAL_FIELD_CP_8(FIRST_ERR_SRC, FIRST_ERR_SRC.desired, 0,1,2,3,4,5,6,7)
     `RAL_FIELD_CP(LAST_ERR, LAST_ERR.desired)
     `RAL_FIELD_CP_2(LAST_ERR, LAST_ERR.desired, 0,1)
     `RAL_FIELD_CP(LAST_ERR_SRC, LAST_ERR_SRC.desired)
     `RAL_FIELD_CP_8(LAST_ERR_SRC, LAST_ERR_SRC.desired, 0,1,2,3,4,5,6,7)
  endgroup
  covergroup actual_cg @(cov_ev);
     option.per_instance = 1;
     option.name = {sla_ral_file::regfilename,".",regname};
     `RAL_FIELD_CP(FIRST_ERR, FIRST_ERR.actual)
     `RAL_FIELD_CP_2(FIRST_ERR, FIRST_ERR.actual, 0,1)
     `RAL_FIELD_CP(FIRST_ERR_SRC, FIRST_ERR_SRC.actual)
     `RAL_FIELD_CP_8(FIRST_ERR_SRC, FIRST_ERR_SRC.actual, 0,1,2,3,4,5,6,7)
     `RAL_FIELD_CP(LAST_ERR, LAST_ERR.actual)
     `RAL_FIELD_CP_2(LAST_ERR, LAST_ERR.actual, 0,1)
     `RAL_FIELD_CP(LAST_ERR_SRC, LAST_ERR_SRC.actual)
     `RAL_FIELD_CP_8(LAST_ERR_SRC, LAST_ERR_SRC.actual, 0,1,2,3,4,5,6,7)
  endgroup
  `endif

  // --------------------------
  // add constraint based on database legal values
  // --------------------------
  function void build();
    FIRST_ERR = new("FIRST_ERR", "RW/1C/V", 2, 0, {"TELEM_STS.FIRST_ERR"});
    FIRST_ERR.set_powerwell("primary");
    FIRST_ERR.set_rand_mode(0);
   FIRST_ERR.set_reset_signame("pmu_rst_b");
    void'(add_field( FIRST_ERR ));

    FIRST_ERR_SRC = new("FIRST_ERR_SRC", "RW/1C/V", 8, 8, {"TELEM_STS.FIRST_ERR_SRC"});
    FIRST_ERR_SRC.set_powerwell("primary");
    FIRST_ERR_SRC.set_rand_mode(0);
   FIRST_ERR_SRC.set_reset_signame("pmu_rst_b");
    void'(add_field( FIRST_ERR_SRC ));

    LAST_ERR = new("LAST_ERR", "RW/1C/V", 2, 16, {"TELEM_STS.LAST_ERR"});
    LAST_ERR.set_powerwell("primary");
    LAST_ERR.set_rand_mode(0);
   LAST_ERR.set_reset_signame("pmu_rst_b");
    void'(add_field( LAST_ERR ));

    LAST_ERR_SRC = new("LAST_ERR_SRC", "RW/1C/V", 8, 24, {"TELEM_STS.LAST_ERR_SRC"});
    LAST_ERR_SRC.set_powerwell("primary");
    LAST_ERR_SRC.set_rand_mode(0);
   LAST_ERR_SRC.set_reset_signame("pmu_rst_b");
    void'(add_field( LAST_ERR_SRC ));

  endfunction


`DEFINE_NONE_SCOPE_REG_API(pmu_mmr_TELEM_STS_reg) 
endclass : pmu_mmr_TELEM_STS_reg

// ================================================

class pmu_mmr_SB_EP_DBG_CTL_reg extends sla_ral_reg;

  // --------------------------
  sla_ral_field ISM_FRC_NOT_IDL;
  sla_ral_field ISM_FRC_IDL;
  sla_ral_field RSVD_2;
  sla_ral_field SC_CG_OVR;
  sla_ral_field FRC_CREDIT_REQ;
  sla_ral_field SBCG_EN;
  sla_ral_field RSVD_7_6;
  sla_ral_field PMSB_ISM_FRC_NOT_IDL;
  sla_ral_field PMSB_ISM_FRC_IDL;
  sla_ral_field RSVD_10;
  sla_ral_field PMSB_SC_CG_OVR;
  sla_ral_field PMSB_FRC_CREDIT_REQ;
  sla_ral_field RSVD_15_13;

  // --------------------------
  `ovm_object_utils(pmu_mmr_SB_EP_DBG_CTL_reg)

  // --------------------------
  function new(
                string         name = "",
                `ifdef SLA_RAL_COVERAGE
                sla_ral_coverage_t cov_t = COVERAGE_OFF,
                `endif
                int            bus_num=0,
                int            dev_num=0,
                int            func_num=0,
                int            offset=0,
                int            size=0,
                sla_ral_data_t reset_val=0,
                boolean_t      mon_enabled = SLA_FALSE
             );
      super.new();
      set_space_addr("CFG",undef);
      set_space_addr("MEM",undef);
      set_space_addr("IO",undef);
      set_space_addr("MSG",undef);
      set_space_addr("CREG",undef); 
      set_space_addr("LT_MEM",undef);    
 build();

      `ifdef SLA_RAL_COVERAGE
      `CONFIG_RAL_COVERAGE(regname, COVERAGE_ON, FD_RW_SAMPLE)
      if(_coverage_type != COVERAGE_OFF) begin
        if(_sample_type inside {FD_RO_SAMPLE, RO_SAMPLE})
        `CREATE_OP_SAMPLE_COV(RAL_WRITE)
        else if(_sample_type inside {FD_WO_SAMPLE, WO_SAMPLE})
        `CREATE_OP_SAMPLE_COV(RAL_READ)
        else
        `CREATE_OP_SAMPLE_COV(RAL_NONE)
        `CREATE_DESIRED_COV
        `CREATE_ACTUAL_COV
      end
      `endif

  endfunction

  `ifdef SLA_RAL_COVERAGE
  `CREATE_RAL_OP_COVERGROUP
  covergroup desired_cg @(cov_ev);
     option.per_instance = 1;
     option.name = {sla_ral_file::regfilename,".",regname};
     `RAL_FIELD_CP(ISM_FRC_NOT_IDL, ISM_FRC_NOT_IDL.desired)
     `RAL_FIELD_CP_1(ISM_FRC_NOT_IDL, ISM_FRC_NOT_IDL.desired, 0)
     `RAL_FIELD_CP(ISM_FRC_IDL, ISM_FRC_IDL.desired)
     `RAL_FIELD_CP_1(ISM_FRC_IDL, ISM_FRC_IDL.desired, 0)
     `RAL_FIELD_CP(RSVD_2, RSVD_2.desired)
     `RAL_FIELD_CP_1(RSVD_2, RSVD_2.desired, 0)
     `RAL_FIELD_CP(SC_CG_OVR, SC_CG_OVR.desired)
     `RAL_FIELD_CP_1(SC_CG_OVR, SC_CG_OVR.desired, 0)
     `RAL_FIELD_CP(FRC_CREDIT_REQ, FRC_CREDIT_REQ.desired)
     `RAL_FIELD_CP_1(FRC_CREDIT_REQ, FRC_CREDIT_REQ.desired, 0)
     `RAL_FIELD_CP(SBCG_EN, SBCG_EN.desired)
     `RAL_FIELD_CP_1(SBCG_EN, SBCG_EN.desired, 0)
     `RAL_FIELD_CP(RSVD_7_6, RSVD_7_6.desired)
     `RAL_FIELD_CP_2(RSVD_7_6, RSVD_7_6.desired, 0,1)
     `RAL_FIELD_CP(PMSB_ISM_FRC_NOT_IDL, PMSB_ISM_FRC_NOT_IDL.desired)
     `RAL_FIELD_CP_1(PMSB_ISM_FRC_NOT_IDL, PMSB_ISM_FRC_NOT_IDL.desired, 0)
     `RAL_FIELD_CP(PMSB_ISM_FRC_IDL, PMSB_ISM_FRC_IDL.desired)
     `RAL_FIELD_CP_1(PMSB_ISM_FRC_IDL, PMSB_ISM_FRC_IDL.desired, 0)
     `RAL_FIELD_CP(RSVD_10, RSVD_10.desired)
     `RAL_FIELD_CP_1(RSVD_10, RSVD_10.desired, 0)
     `RAL_FIELD_CP(PMSB_SC_CG_OVR, PMSB_SC_CG_OVR.desired)
     `RAL_FIELD_CP_1(PMSB_SC_CG_OVR, PMSB_SC_CG_OVR.desired, 0)
     `RAL_FIELD_CP(PMSB_FRC_CREDIT_REQ, PMSB_FRC_CREDIT_REQ.desired)
     `RAL_FIELD_CP_1(PMSB_FRC_CREDIT_REQ, PMSB_FRC_CREDIT_REQ.desired, 0)
     `RAL_FIELD_CP(RSVD_15_13, RSVD_15_13.desired)
     `RAL_FIELD_CP_3(RSVD_15_13, RSVD_15_13.desired, 0,1,2)
  endgroup
  covergroup actual_cg @(cov_ev);
     option.per_instance = 1;
     option.name = {sla_ral_file::regfilename,".",regname};
     `RAL_FIELD_CP(ISM_FRC_NOT_IDL, ISM_FRC_NOT_IDL.actual)
     `RAL_FIELD_CP_1(ISM_FRC_NOT_IDL, ISM_FRC_NOT_IDL.actual, 0)
     `RAL_FIELD_CP(ISM_FRC_IDL, ISM_FRC_IDL.actual)
     `RAL_FIELD_CP_1(ISM_FRC_IDL, ISM_FRC_IDL.actual, 0)
     `RAL_FIELD_CP(RSVD_2, RSVD_2.actual)
     `RAL_FIELD_CP_1(RSVD_2, RSVD_2.actual, 0)
     `RAL_FIELD_CP(SC_CG_OVR, SC_CG_OVR.actual)
     `RAL_FIELD_CP_1(SC_CG_OVR, SC_CG_OVR.actual, 0)
     `RAL_FIELD_CP(FRC_CREDIT_REQ, FRC_CREDIT_REQ.actual)
     `RAL_FIELD_CP_1(FRC_CREDIT_REQ, FRC_CREDIT_REQ.actual, 0)
     `RAL_FIELD_CP(SBCG_EN, SBCG_EN.actual)
     `RAL_FIELD_CP_1(SBCG_EN, SBCG_EN.actual, 0)
     `RAL_FIELD_CP(RSVD_7_6, RSVD_7_6.actual)
     `RAL_FIELD_CP_2(RSVD_7_6, RSVD_7_6.actual, 0,1)
     `RAL_FIELD_CP(PMSB_ISM_FRC_NOT_IDL, PMSB_ISM_FRC_NOT_IDL.actual)
     `RAL_FIELD_CP_1(PMSB_ISM_FRC_NOT_IDL, PMSB_ISM_FRC_NOT_IDL.actual, 0)
     `RAL_FIELD_CP(PMSB_ISM_FRC_IDL, PMSB_ISM_FRC_IDL.actual)
     `RAL_FIELD_CP_1(PMSB_ISM_FRC_IDL, PMSB_ISM_FRC_IDL.actual, 0)
     `RAL_FIELD_CP(RSVD_10, RSVD_10.actual)
     `RAL_FIELD_CP_1(RSVD_10, RSVD_10.actual, 0)
     `RAL_FIELD_CP(PMSB_SC_CG_OVR, PMSB_SC_CG_OVR.actual)
     `RAL_FIELD_CP_1(PMSB_SC_CG_OVR, PMSB_SC_CG_OVR.actual, 0)
     `RAL_FIELD_CP(PMSB_FRC_CREDIT_REQ, PMSB_FRC_CREDIT_REQ.actual)
     `RAL_FIELD_CP_1(PMSB_FRC_CREDIT_REQ, PMSB_FRC_CREDIT_REQ.actual, 0)
     `RAL_FIELD_CP(RSVD_15_13, RSVD_15_13.actual)
     `RAL_FIELD_CP_3(RSVD_15_13, RSVD_15_13.actual, 0,1,2)
  endgroup
  `endif

  // --------------------------
  // add constraint based on database legal values
  // --------------------------
  function void build();
    ISM_FRC_NOT_IDL = new("ISM_FRC_NOT_IDL", "RW", 1, 0, {"SB_EP_DBG_CTL.ISM_FRC_NOT_IDL"});
    ISM_FRC_NOT_IDL.set_powerwell("primary");
    ISM_FRC_NOT_IDL.set_rand_mode(0);
   ISM_FRC_NOT_IDL.set_reset_signame("pmu_rst_b");
    void'(add_field( ISM_FRC_NOT_IDL ));

    ISM_FRC_IDL = new("ISM_FRC_IDL", "RW", 1, 1, {"SB_EP_DBG_CTL.ISM_FRC_IDL"});
    ISM_FRC_IDL.set_powerwell("primary");
    ISM_FRC_IDL.set_rand_mode(0);
   ISM_FRC_IDL.set_reset_signame("pmu_rst_b");
    void'(add_field( ISM_FRC_IDL ));

    RSVD_2 = new("RSVD_2", "RO", 1, 2, {"SB_EP_DBG_CTL.RSVD_2"});
    RSVD_2.set_powerwell("primary");
    RSVD_2.set_rand_mode(0);
    void'(add_field( RSVD_2 ));

    SC_CG_OVR = new("SC_CG_OVR", "RW", 1, 3, {"SB_EP_DBG_CTL.SC_CG_OVR"});
    SC_CG_OVR.set_powerwell("primary");
    SC_CG_OVR.set_rand_mode(0);
   SC_CG_OVR.set_reset_signame("pmu_rst_b");
    void'(add_field( SC_CG_OVR ));

    FRC_CREDIT_REQ = new("FRC_CREDIT_REQ", "RW", 1, 4, {"SB_EP_DBG_CTL.FRC_CREDIT_REQ"});
    FRC_CREDIT_REQ.set_powerwell("primary");
    FRC_CREDIT_REQ.set_rand_mode(0);
   FRC_CREDIT_REQ.set_reset_signame("pmu_rst_b");
    void'(add_field( FRC_CREDIT_REQ ));

    SBCG_EN = new("SBCG_EN", "RW", 1, 5, {"SB_EP_DBG_CTL.SBCG_EN"});
    SBCG_EN.set_powerwell("primary");
    SBCG_EN.set_rand_mode(0);
   SBCG_EN.set_reset_signame("pmu_rst_b");
    void'(add_field( SBCG_EN ));

    RSVD_7_6 = new("RSVD_7_6", "RO", 2, 6, {"SB_EP_DBG_CTL.RSVD_7_6"});
    RSVD_7_6.set_powerwell("primary");
    RSVD_7_6.set_rand_mode(0);
    void'(add_field( RSVD_7_6 ));

    PMSB_ISM_FRC_NOT_IDL = new("PMSB_ISM_FRC_NOT_IDL", "RW", 1, 8, {"SB_EP_DBG_CTL.PMSB_ISM_FRC_NOT_IDL"});
    PMSB_ISM_FRC_NOT_IDL.set_powerwell("primary");
    PMSB_ISM_FRC_NOT_IDL.set_rand_mode(0);
   PMSB_ISM_FRC_NOT_IDL.set_reset_signame("pmu_rst_b");
    void'(add_field( PMSB_ISM_FRC_NOT_IDL ));

    PMSB_ISM_FRC_IDL = new("PMSB_ISM_FRC_IDL", "RW", 1, 9, {"SB_EP_DBG_CTL.PMSB_ISM_FRC_IDL"});
    PMSB_ISM_FRC_IDL.set_powerwell("primary");
    PMSB_ISM_FRC_IDL.set_rand_mode(0);
   PMSB_ISM_FRC_IDL.set_reset_signame("pmu_rst_b");
    void'(add_field( PMSB_ISM_FRC_IDL ));

    RSVD_10 = new("RSVD_10", "RO", 1, 10, {"SB_EP_DBG_CTL.RSVD_10"});
    RSVD_10.set_powerwell("primary");
    RSVD_10.set_rand_mode(0);
    void'(add_field( RSVD_10 ));

    PMSB_SC_CG_OVR = new("PMSB_SC_CG_OVR", "RW", 1, 11, {"SB_EP_DBG_CTL.PMSB_SC_CG_OVR"});
    PMSB_SC_CG_OVR.set_powerwell("primary");
    PMSB_SC_CG_OVR.set_rand_mode(0);
   PMSB_SC_CG_OVR.set_reset_signame("pmu_rst_b");
    void'(add_field( PMSB_SC_CG_OVR ));

    PMSB_FRC_CREDIT_REQ = new("PMSB_FRC_CREDIT_REQ", "RW", 1, 12, {"SB_EP_DBG_CTL.PMSB_FRC_CREDIT_REQ"});
    PMSB_FRC_CREDIT_REQ.set_powerwell("primary");
    PMSB_FRC_CREDIT_REQ.set_rand_mode(0);
   PMSB_FRC_CREDIT_REQ.set_reset_signame("pmu_rst_b");
    void'(add_field( PMSB_FRC_CREDIT_REQ ));

    RSVD_15_13 = new("RSVD_15_13", "RO", 3, 13, {"SB_EP_DBG_CTL.RSVD_15_13"});
    RSVD_15_13.set_powerwell("primary");
    RSVD_15_13.set_rand_mode(0);
    void'(add_field( RSVD_15_13 ));

  endfunction


`DEFINE_NONE_SCOPE_REG_API(pmu_mmr_SB_EP_DBG_CTL_reg) 
endclass : pmu_mmr_SB_EP_DBG_CTL_reg

// ================================================

class pmu_mmr_SURVIVABILITY_CTL_0_reg extends sla_ral_reg;

  // --------------------------
  sla_ral_field CHK_TMR_EN;
  sla_ral_field CHK_FORCE;
  sla_ral_field HLT_RESUME;
  sla_ral_field SURV_CTL;
  sla_ral_field CHK_EXP_CNT;

  // --------------------------
  `ovm_object_utils(pmu_mmr_SURVIVABILITY_CTL_0_reg)

  // --------------------------
  function new(
                string         name = "",
                `ifdef SLA_RAL_COVERAGE
                sla_ral_coverage_t cov_t = COVERAGE_OFF,
                `endif
                int            bus_num=0,
                int            dev_num=0,
                int            func_num=0,
                int            offset=0,
                int            size=0,
                sla_ral_data_t reset_val=0,
                boolean_t      mon_enabled = SLA_FALSE
             );
      super.new();
      set_space_addr("CFG",undef);
      set_space_addr("MEM",undef);
      set_space_addr("IO",undef);
      set_space_addr("MSG",undef);
      set_space_addr("CREG",undef); 
      set_space_addr("LT_MEM",undef);    
 build();

      `ifdef SLA_RAL_COVERAGE
      `CONFIG_RAL_COVERAGE(regname, COVERAGE_ON, FD_RW_SAMPLE)
      if(_coverage_type != COVERAGE_OFF) begin
        if(_sample_type inside {FD_RO_SAMPLE, RO_SAMPLE})
        `CREATE_OP_SAMPLE_COV(RAL_WRITE)
        else if(_sample_type inside {FD_WO_SAMPLE, WO_SAMPLE})
        `CREATE_OP_SAMPLE_COV(RAL_READ)
        else
        `CREATE_OP_SAMPLE_COV(RAL_NONE)
        `CREATE_DESIRED_COV
        `CREATE_ACTUAL_COV
      end
      `endif

  endfunction

  `ifdef SLA_RAL_COVERAGE
  `CREATE_RAL_OP_COVERGROUP
  covergroup desired_cg @(cov_ev);
     option.per_instance = 1;
     option.name = {sla_ral_file::regfilename,".",regname};
     `RAL_FIELD_CP(CHK_TMR_EN, CHK_TMR_EN.desired)
     `RAL_FIELD_CP_1(CHK_TMR_EN, CHK_TMR_EN.desired, 0)
     `RAL_FIELD_CP(CHK_FORCE, CHK_FORCE.desired)
     `RAL_FIELD_CP_1(CHK_FORCE, CHK_FORCE.desired, 0)
     `RAL_FIELD_CP(HLT_RESUME, HLT_RESUME.desired)
     `RAL_FIELD_CP_1(HLT_RESUME, HLT_RESUME.desired, 0)
     `RAL_FIELD_CP(SURV_CTL, SURV_CTL.desired)
     `RAL_FIELD_CP_13(SURV_CTL, SURV_CTL.desired, 0,1,2,3,4,5,6,7,8,9,10,11,12)
     `RAL_FIELD_CP(CHK_EXP_CNT, CHK_EXP_CNT.desired)
     `RAL_FIELD_CP_16(CHK_EXP_CNT, CHK_EXP_CNT.desired, 0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)
  endgroup
  covergroup actual_cg @(cov_ev);
     option.per_instance = 1;
     option.name = {sla_ral_file::regfilename,".",regname};
     `RAL_FIELD_CP(CHK_TMR_EN, CHK_TMR_EN.actual)
     `RAL_FIELD_CP_1(CHK_TMR_EN, CHK_TMR_EN.actual, 0)
     `RAL_FIELD_CP(CHK_FORCE, CHK_FORCE.actual)
     `RAL_FIELD_CP_1(CHK_FORCE, CHK_FORCE.actual, 0)
     `RAL_FIELD_CP(HLT_RESUME, HLT_RESUME.actual)
     `RAL_FIELD_CP_1(HLT_RESUME, HLT_RESUME.actual, 0)
     `RAL_FIELD_CP(SURV_CTL, SURV_CTL.actual)
     `RAL_FIELD_CP_13(SURV_CTL, SURV_CTL.actual, 0,1,2,3,4,5,6,7,8,9,10,11,12)
     `RAL_FIELD_CP(CHK_EXP_CNT, CHK_EXP_CNT.actual)
     `RAL_FIELD_CP_16(CHK_EXP_CNT, CHK_EXP_CNT.actual, 0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)
  endgroup
  `endif

  // --------------------------
  // add constraint based on database legal values
  // --------------------------
  function void build();
    CHK_TMR_EN = new("CHK_TMR_EN", "RW/V", 1, 0, {"SURVIVABILITY_CTL_0.CHK_TMR_EN"});
    CHK_TMR_EN.set_powerwell("primary");
    CHK_TMR_EN.set_rand_mode(0);
   CHK_TMR_EN.set_reset_signame("pmu_rst_b");
    void'(add_field( CHK_TMR_EN ));

    CHK_FORCE = new("CHK_FORCE", "RW/V", 1, 1, {"SURVIVABILITY_CTL_0.CHK_FORCE"});
    CHK_FORCE.set_powerwell("primary");
    CHK_FORCE.set_rand_mode(0);
   CHK_FORCE.set_reset_signame("pmu_rst_b");
    void'(add_field( CHK_FORCE ));

    HLT_RESUME = new("HLT_RESUME", "RW/V", 1, 2, {"SURVIVABILITY_CTL_0.HLT_RESUME"});
    HLT_RESUME.set_powerwell("primary");
    HLT_RESUME.set_rand_mode(0);
   HLT_RESUME.set_reset_signame("pmu_rst_b");
    void'(add_field( HLT_RESUME ));

    SURV_CTL = new("SURV_CTL", "RW/V", 13, 3, {"SURVIVABILITY_CTL_0.SURV_CTL"});
    SURV_CTL.set_powerwell("primary");
    SURV_CTL.set_rand_mode(0);
   SURV_CTL.set_reset_signame("pmu_rst_b");
    void'(add_field( SURV_CTL ));

    CHK_EXP_CNT = new("CHK_EXP_CNT", "RW/V", 16, 16, {"SURVIVABILITY_CTL_0.CHK_EXP_CNT"});
    CHK_EXP_CNT.set_powerwell("primary");
    CHK_EXP_CNT.set_rand_mode(0);
   CHK_EXP_CNT.set_reset_signame("pmu_rst_b");
    void'(add_field( CHK_EXP_CNT ));

  endfunction


`DEFINE_NONE_SCOPE_REG_API(pmu_mmr_SURVIVABILITY_CTL_0_reg) 
endclass : pmu_mmr_SURVIVABILITY_CTL_0_reg

// ================================================

class pmu_mmr_SURVIVABILITY_CTL_1_reg extends sla_ral_reg;

  // --------------------------
  sla_ral_field SURV_CTL;

  // --------------------------
  `ovm_object_utils(pmu_mmr_SURVIVABILITY_CTL_1_reg)

  // --------------------------
  function new(
                string         name = "",
                `ifdef SLA_RAL_COVERAGE
                sla_ral_coverage_t cov_t = COVERAGE_OFF,
                `endif
                int            bus_num=0,
                int            dev_num=0,
                int            func_num=0,
                int            offset=0,
                int            size=0,
                sla_ral_data_t reset_val=0,
                boolean_t      mon_enabled = SLA_FALSE
             );
      super.new();
      set_space_addr("CFG",undef);
      set_space_addr("MEM",undef);
      set_space_addr("IO",undef);
      set_space_addr("MSG",undef);
      set_space_addr("CREG",undef); 
      set_space_addr("LT_MEM",undef);    
 build();

      `ifdef SLA_RAL_COVERAGE
      `CONFIG_RAL_COVERAGE(regname, COVERAGE_ON, FD_RW_SAMPLE)
      if(_coverage_type != COVERAGE_OFF) begin
        if(_sample_type inside {FD_RO_SAMPLE, RO_SAMPLE})
        `CREATE_OP_SAMPLE_COV(RAL_WRITE)
        else if(_sample_type inside {FD_WO_SAMPLE, WO_SAMPLE})
        `CREATE_OP_SAMPLE_COV(RAL_READ)
        else
        `CREATE_OP_SAMPLE_COV(RAL_NONE)
        `CREATE_DESIRED_COV
        `CREATE_ACTUAL_COV
      end
      `endif

  endfunction

  `ifdef SLA_RAL_COVERAGE
  `CREATE_RAL_OP_COVERGROUP
  covergroup desired_cg @(cov_ev);
     option.per_instance = 1;
     option.name = {sla_ral_file::regfilename,".",regname};
     `RAL_FIELD_CP(SURV_CTL, SURV_CTL.desired)
     `RAL_FIELD_CP_16(SURV_CTL, SURV_CTL.desired, 0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)
     `RAL_FIELD_CP_16(SURV_CTL, SURV_CTL.desired, 16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31)
  endgroup
  covergroup actual_cg @(cov_ev);
     option.per_instance = 1;
     option.name = {sla_ral_file::regfilename,".",regname};
     `RAL_FIELD_CP(SURV_CTL, SURV_CTL.actual)
     `RAL_FIELD_CP_16(SURV_CTL, SURV_CTL.actual, 0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)
     `RAL_FIELD_CP_16(SURV_CTL, SURV_CTL.actual, 16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31)
  endgroup
  `endif

  // --------------------------
  // add constraint based on database legal values
  // --------------------------
  function void build();
    SURV_CTL = new("SURV_CTL", "RW/V", 32, 0, {"SURVIVABILITY_CTL_1.SURV_CTL"});
    SURV_CTL.set_powerwell("primary");
    SURV_CTL.set_rand_mode(0);
   SURV_CTL.set_reset_signame("pmu_rst_b");
    void'(add_field( SURV_CTL ));

  endfunction


`DEFINE_NONE_SCOPE_REG_API(pmu_mmr_SURVIVABILITY_CTL_1_reg) 
endclass : pmu_mmr_SURVIVABILITY_CTL_1_reg

// ================================================

class pmu_mmr_SURVIVABILITY_STS_0_reg extends sla_ral_reg;

  // --------------------------
  sla_ral_field CHK_TIMER_FIRED;
  sla_ral_field SURV_STS;

  // --------------------------
  `ovm_object_utils(pmu_mmr_SURVIVABILITY_STS_0_reg)

  // --------------------------
  function new(
                string         name = "",
                `ifdef SLA_RAL_COVERAGE
                sla_ral_coverage_t cov_t = COVERAGE_OFF,
                `endif
                int            bus_num=0,
                int            dev_num=0,
                int            func_num=0,
                int            offset=0,
                int            size=0,
                sla_ral_data_t reset_val=0,
                boolean_t      mon_enabled = SLA_FALSE
             );
      super.new();
      set_space_addr("CFG",undef);
      set_space_addr("MEM",undef);
      set_space_addr("IO",undef);
      set_space_addr("MSG",undef);
      set_space_addr("CREG",undef); 
      set_space_addr("LT_MEM",undef);    
 build();

      `ifdef SLA_RAL_COVERAGE
      `CONFIG_RAL_COVERAGE(regname, COVERAGE_ON, FD_RW_SAMPLE)
      if(_coverage_type != COVERAGE_OFF) begin
        if(_sample_type inside {FD_RO_SAMPLE, RO_SAMPLE})
        `CREATE_OP_SAMPLE_COV(RAL_WRITE)
        else if(_sample_type inside {FD_WO_SAMPLE, WO_SAMPLE})
        `CREATE_OP_SAMPLE_COV(RAL_READ)
        else
        `CREATE_OP_SAMPLE_COV(RAL_NONE)
        `CREATE_DESIRED_COV
        `CREATE_ACTUAL_COV
      end
      `endif

  endfunction

  `ifdef SLA_RAL_COVERAGE
  `CREATE_RAL_OP_COVERGROUP
  covergroup desired_cg @(cov_ev);
     option.per_instance = 1;
     option.name = {sla_ral_file::regfilename,".",regname};
     `RAL_FIELD_CP(CHK_TIMER_FIRED, CHK_TIMER_FIRED.desired)
     `RAL_FIELD_CP_1(CHK_TIMER_FIRED, CHK_TIMER_FIRED.desired, 0)
     `RAL_FIELD_CP(SURV_STS, SURV_STS.desired)
     `RAL_FIELD_CP_16(SURV_STS, SURV_STS.desired, 0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)
     `RAL_FIELD_CP_15(SURV_STS, SURV_STS.desired, 16,17,18,19,20,21,22,23,24,25,26,27,28,29,30)
  endgroup
  covergroup actual_cg @(cov_ev);
     option.per_instance = 1;
     option.name = {sla_ral_file::regfilename,".",regname};
     `RAL_FIELD_CP(CHK_TIMER_FIRED, CHK_TIMER_FIRED.actual)
     `RAL_FIELD_CP_1(CHK_TIMER_FIRED, CHK_TIMER_FIRED.actual, 0)
     `RAL_FIELD_CP(SURV_STS, SURV_STS.actual)
     `RAL_FIELD_CP_16(SURV_STS, SURV_STS.actual, 0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)
     `RAL_FIELD_CP_15(SURV_STS, SURV_STS.actual, 16,17,18,19,20,21,22,23,24,25,26,27,28,29,30)
  endgroup
  `endif

  // --------------------------
  // add constraint based on database legal values
  // --------------------------
  function void build();
    CHK_TIMER_FIRED = new("CHK_TIMER_FIRED", "RW/1C/V", 1, 0, {"SURVIVABILITY_STS_0.CHK_TIMER_FIRED"});
    CHK_TIMER_FIRED.set_powerwell("primary");
    CHK_TIMER_FIRED.set_rand_mode(0);
   CHK_TIMER_FIRED.set_reset_signame("pmu_rst_b");
    void'(add_field( CHK_TIMER_FIRED ));

    SURV_STS = new("SURV_STS", "RW/1C/V", 31, 1, {"SURVIVABILITY_STS_0.SURV_STS"});
    SURV_STS.set_powerwell("primary");
    SURV_STS.set_rand_mode(0);
   SURV_STS.set_reset_signame("pmu_rst_b");
    void'(add_field( SURV_STS ));

  endfunction


`DEFINE_NONE_SCOPE_REG_API(pmu_mmr_SURVIVABILITY_STS_0_reg) 
endclass : pmu_mmr_SURVIVABILITY_STS_0_reg

// ================================================

class pmu_mmr_SURVIVABILITY_STS_1_reg extends sla_ral_reg;

  // --------------------------
  sla_ral_field SURV_STS;

  // --------------------------
  `ovm_object_utils(pmu_mmr_SURVIVABILITY_STS_1_reg)

  // --------------------------
  function new(
                string         name = "",
                `ifdef SLA_RAL_COVERAGE
                sla_ral_coverage_t cov_t = COVERAGE_OFF,
                `endif
                int            bus_num=0,
                int            dev_num=0,
                int            func_num=0,
                int            offset=0,
                int            size=0,
                sla_ral_data_t reset_val=0,
                boolean_t      mon_enabled = SLA_FALSE
             );
      super.new();
      set_space_addr("CFG",undef);
      set_space_addr("MEM",undef);
      set_space_addr("IO",undef);
      set_space_addr("MSG",undef);
      set_space_addr("CREG",undef); 
      set_space_addr("LT_MEM",undef);    
 build();

      `ifdef SLA_RAL_COVERAGE
      `CONFIG_RAL_COVERAGE(regname, COVERAGE_ON, FD_RW_SAMPLE)
      if(_coverage_type != COVERAGE_OFF) begin
        if(_sample_type inside {FD_RO_SAMPLE, RO_SAMPLE})
        `CREATE_OP_SAMPLE_COV(RAL_WRITE)
        else if(_sample_type inside {FD_WO_SAMPLE, WO_SAMPLE})
        `CREATE_OP_SAMPLE_COV(RAL_READ)
        else
        `CREATE_OP_SAMPLE_COV(RAL_NONE)
        `CREATE_DESIRED_COV
        `CREATE_ACTUAL_COV
      end
      `endif

  endfunction

  `ifdef SLA_RAL_COVERAGE
  `CREATE_RAL_OP_COVERGROUP
  covergroup desired_cg @(cov_ev);
     option.per_instance = 1;
     option.name = {sla_ral_file::regfilename,".",regname};
     `RAL_FIELD_CP(SURV_STS, SURV_STS.desired)
     `RAL_FIELD_CP_16(SURV_STS, SURV_STS.desired, 0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)
     `RAL_FIELD_CP_16(SURV_STS, SURV_STS.desired, 16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31)
  endgroup
  covergroup actual_cg @(cov_ev);
     option.per_instance = 1;
     option.name = {sla_ral_file::regfilename,".",regname};
     `RAL_FIELD_CP(SURV_STS, SURV_STS.actual)
     `RAL_FIELD_CP_16(SURV_STS, SURV_STS.actual, 0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)
     `RAL_FIELD_CP_16(SURV_STS, SURV_STS.actual, 16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31)
  endgroup
  `endif

  // --------------------------
  // add constraint based on database legal values
  // --------------------------
  function void build();
    SURV_STS = new("SURV_STS", "RW/1C/V", 32, 0, {"SURVIVABILITY_STS_1.SURV_STS"});
    SURV_STS.set_powerwell("primary");
    SURV_STS.set_rand_mode(0);
   SURV_STS.set_reset_signame("pmu_rst_b");
    void'(add_field( SURV_STS ));

  endfunction


`DEFINE_NONE_SCOPE_REG_API(pmu_mmr_SURVIVABILITY_STS_1_reg) 
endclass : pmu_mmr_SURVIVABILITY_STS_1_reg

// ================================================

class pmu_mmr_IP_FORCE_FET_EN_OVR_EN_0_reg extends sla_ral_reg;

  // --------------------------
  sla_ral_field FET_EN_B_OVR_EN;

  // --------------------------
  `ovm_object_utils(pmu_mmr_IP_FORCE_FET_EN_OVR_EN_0_reg)

  // --------------------------
  function new(
                string         name = "",
                `ifdef SLA_RAL_COVERAGE
                sla_ral_coverage_t cov_t = COVERAGE_OFF,
                `endif
                int            bus_num=0,
                int            dev_num=0,
                int            func_num=0,
                int            offset=0,
                int            size=0,
                sla_ral_data_t reset_val=0,
                boolean_t      mon_enabled = SLA_FALSE
             );
      super.new();
      set_space_addr("CFG",undef);
      set_space_addr("MEM",undef);
      set_space_addr("IO",undef);
      set_space_addr("MSG",undef);
      set_space_addr("CREG",undef); 
      set_space_addr("LT_MEM",undef);    
 build();

      `ifdef SLA_RAL_COVERAGE
      `CONFIG_RAL_COVERAGE(regname, COVERAGE_ON, FD_RW_SAMPLE)
      if(_coverage_type != COVERAGE_OFF) begin
        if(_sample_type inside {FD_RO_SAMPLE, RO_SAMPLE})
        `CREATE_OP_SAMPLE_COV(RAL_WRITE)
        else if(_sample_type inside {FD_WO_SAMPLE, WO_SAMPLE})
        `CREATE_OP_SAMPLE_COV(RAL_READ)
        else
        `CREATE_OP_SAMPLE_COV(RAL_NONE)
        `CREATE_DESIRED_COV
        `CREATE_ACTUAL_COV
      end
      `endif

  endfunction

  `ifdef SLA_RAL_COVERAGE
  `CREATE_RAL_OP_COVERGROUP
  covergroup desired_cg @(cov_ev);
     option.per_instance = 1;
     option.name = {sla_ral_file::regfilename,".",regname};
     `RAL_FIELD_CP(FET_EN_B_OVR_EN, FET_EN_B_OVR_EN.desired)
     `RAL_FIELD_CP_16(FET_EN_B_OVR_EN, FET_EN_B_OVR_EN.desired, 0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)
     `RAL_FIELD_CP_16(FET_EN_B_OVR_EN, FET_EN_B_OVR_EN.desired, 16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31)
  endgroup
  covergroup actual_cg @(cov_ev);
     option.per_instance = 1;
     option.name = {sla_ral_file::regfilename,".",regname};
     `RAL_FIELD_CP(FET_EN_B_OVR_EN, FET_EN_B_OVR_EN.actual)
     `RAL_FIELD_CP_16(FET_EN_B_OVR_EN, FET_EN_B_OVR_EN.actual, 0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)
     `RAL_FIELD_CP_16(FET_EN_B_OVR_EN, FET_EN_B_OVR_EN.actual, 16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31)
  endgroup
  `endif

  // --------------------------
  // add constraint based on database legal values
  // --------------------------
  function void build();
    FET_EN_B_OVR_EN = new("FET_EN_B_OVR_EN", "RW", 32, 0, {"IP_FORCE_FET_EN_OVR_EN_0.FET_EN_B_OVR_EN"});
    FET_EN_B_OVR_EN.set_powerwell("primary");
    FET_EN_B_OVR_EN.set_rand_mode(0);
   FET_EN_B_OVR_EN.set_reset_signame("pmu_rst_b");
    void'(add_field( FET_EN_B_OVR_EN ));

  endfunction


`DEFINE_NONE_SCOPE_REG_API(pmu_mmr_IP_FORCE_FET_EN_OVR_EN_0_reg) 
endclass : pmu_mmr_IP_FORCE_FET_EN_OVR_EN_0_reg

// ================================================

class pmu_mmr_IP_FORCE_FET_EN_OVR_EN_1_reg extends sla_ral_reg;

  // --------------------------
  sla_ral_field FET_EN_B_OVR_EN;

  // --------------------------
  `ovm_object_utils(pmu_mmr_IP_FORCE_FET_EN_OVR_EN_1_reg)

  // --------------------------
  function new(
                string         name = "",
                `ifdef SLA_RAL_COVERAGE
                sla_ral_coverage_t cov_t = COVERAGE_OFF,
                `endif
                int            bus_num=0,
                int            dev_num=0,
                int            func_num=0,
                int            offset=0,
                int            size=0,
                sla_ral_data_t reset_val=0,
                boolean_t      mon_enabled = SLA_FALSE
             );
      super.new();
      set_space_addr("CFG",undef);
      set_space_addr("MEM",undef);
      set_space_addr("IO",undef);
      set_space_addr("MSG",undef);
      set_space_addr("CREG",undef); 
      set_space_addr("LT_MEM",undef);    
 build();

      `ifdef SLA_RAL_COVERAGE
      `CONFIG_RAL_COVERAGE(regname, COVERAGE_ON, FD_RW_SAMPLE)
      if(_coverage_type != COVERAGE_OFF) begin
        if(_sample_type inside {FD_RO_SAMPLE, RO_SAMPLE})
        `CREATE_OP_SAMPLE_COV(RAL_WRITE)
        else if(_sample_type inside {FD_WO_SAMPLE, WO_SAMPLE})
        `CREATE_OP_SAMPLE_COV(RAL_READ)
        else
        `CREATE_OP_SAMPLE_COV(RAL_NONE)
        `CREATE_DESIRED_COV
        `CREATE_ACTUAL_COV
      end
      `endif

  endfunction

  `ifdef SLA_RAL_COVERAGE
  `CREATE_RAL_OP_COVERGROUP
  covergroup desired_cg @(cov_ev);
     option.per_instance = 1;
     option.name = {sla_ral_file::regfilename,".",regname};
     `RAL_FIELD_CP(FET_EN_B_OVR_EN, FET_EN_B_OVR_EN.desired)
     `RAL_FIELD_CP_16(FET_EN_B_OVR_EN, FET_EN_B_OVR_EN.desired, 0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)
     `RAL_FIELD_CP_16(FET_EN_B_OVR_EN, FET_EN_B_OVR_EN.desired, 16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31)
  endgroup
  covergroup actual_cg @(cov_ev);
     option.per_instance = 1;
     option.name = {sla_ral_file::regfilename,".",regname};
     `RAL_FIELD_CP(FET_EN_B_OVR_EN, FET_EN_B_OVR_EN.actual)
     `RAL_FIELD_CP_16(FET_EN_B_OVR_EN, FET_EN_B_OVR_EN.actual, 0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)
     `RAL_FIELD_CP_16(FET_EN_B_OVR_EN, FET_EN_B_OVR_EN.actual, 16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31)
  endgroup
  `endif

  // --------------------------
  // add constraint based on database legal values
  // --------------------------
  function void build();
    FET_EN_B_OVR_EN = new("FET_EN_B_OVR_EN", "RW", 32, 0, {"IP_FORCE_FET_EN_OVR_EN_1.FET_EN_B_OVR_EN"});
    FET_EN_B_OVR_EN.set_powerwell("primary");
    FET_EN_B_OVR_EN.set_rand_mode(0);
   FET_EN_B_OVR_EN.set_reset_signame("pmu_rst_b");
    void'(add_field( FET_EN_B_OVR_EN ));

  endfunction


`DEFINE_NONE_SCOPE_REG_API(pmu_mmr_IP_FORCE_FET_EN_OVR_EN_1_reg) 
endclass : pmu_mmr_IP_FORCE_FET_EN_OVR_EN_1_reg

// ================================================

class pmu_mmr_IP_FORCE_FET_EN_OVR_VAL_0_reg extends sla_ral_reg;

  // --------------------------
  sla_ral_field FET_EN_B_OVR_VAL;

  // --------------------------
  `ovm_object_utils(pmu_mmr_IP_FORCE_FET_EN_OVR_VAL_0_reg)

  // --------------------------
  function new(
                string         name = "",
                `ifdef SLA_RAL_COVERAGE
                sla_ral_coverage_t cov_t = COVERAGE_OFF,
                `endif
                int            bus_num=0,
                int            dev_num=0,
                int            func_num=0,
                int            offset=0,
                int            size=0,
                sla_ral_data_t reset_val=0,
                boolean_t      mon_enabled = SLA_FALSE
             );
      super.new();
      set_space_addr("CFG",undef);
      set_space_addr("MEM",undef);
      set_space_addr("IO",undef);
      set_space_addr("MSG",undef);
      set_space_addr("CREG",undef); 
      set_space_addr("LT_MEM",undef);    
 build();

      `ifdef SLA_RAL_COVERAGE
      `CONFIG_RAL_COVERAGE(regname, COVERAGE_ON, FD_RW_SAMPLE)
      if(_coverage_type != COVERAGE_OFF) begin
        if(_sample_type inside {FD_RO_SAMPLE, RO_SAMPLE})
        `CREATE_OP_SAMPLE_COV(RAL_WRITE)
        else if(_sample_type inside {FD_WO_SAMPLE, WO_SAMPLE})
        `CREATE_OP_SAMPLE_COV(RAL_READ)
        else
        `CREATE_OP_SAMPLE_COV(RAL_NONE)
        `CREATE_DESIRED_COV
        `CREATE_ACTUAL_COV
      end
      `endif

  endfunction

  `ifdef SLA_RAL_COVERAGE
  `CREATE_RAL_OP_COVERGROUP
  covergroup desired_cg @(cov_ev);
     option.per_instance = 1;
     option.name = {sla_ral_file::regfilename,".",regname};
     `RAL_FIELD_CP(FET_EN_B_OVR_VAL, FET_EN_B_OVR_VAL.desired)
     `RAL_FIELD_CP_16(FET_EN_B_OVR_VAL, FET_EN_B_OVR_VAL.desired, 0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)
     `RAL_FIELD_CP_16(FET_EN_B_OVR_VAL, FET_EN_B_OVR_VAL.desired, 16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31)
  endgroup
  covergroup actual_cg @(cov_ev);
     option.per_instance = 1;
     option.name = {sla_ral_file::regfilename,".",regname};
     `RAL_FIELD_CP(FET_EN_B_OVR_VAL, FET_EN_B_OVR_VAL.actual)
     `RAL_FIELD_CP_16(FET_EN_B_OVR_VAL, FET_EN_B_OVR_VAL.actual, 0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)
     `RAL_FIELD_CP_16(FET_EN_B_OVR_VAL, FET_EN_B_OVR_VAL.actual, 16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31)
  endgroup
  `endif

  // --------------------------
  // add constraint based on database legal values
  // --------------------------
  function void build();
    FET_EN_B_OVR_VAL = new("FET_EN_B_OVR_VAL", "RW", 32, 0, {"IP_FORCE_FET_EN_OVR_VAL_0.FET_EN_B_OVR_VAL"});
    FET_EN_B_OVR_VAL.set_powerwell("primary");
    FET_EN_B_OVR_VAL.set_rand_mode(0);
   FET_EN_B_OVR_VAL.set_reset_signame("pmu_rst_b");
    void'(add_field( FET_EN_B_OVR_VAL ));

  endfunction


`DEFINE_NONE_SCOPE_REG_API(pmu_mmr_IP_FORCE_FET_EN_OVR_VAL_0_reg) 
endclass : pmu_mmr_IP_FORCE_FET_EN_OVR_VAL_0_reg

// ================================================

class pmu_mmr_IP_FORCE_FET_EN_OVR_VAL_1_reg extends sla_ral_reg;

  // --------------------------
  sla_ral_field FET_EN_B_OVR_VAL;

  // --------------------------
  `ovm_object_utils(pmu_mmr_IP_FORCE_FET_EN_OVR_VAL_1_reg)

  // --------------------------
  function new(
                string         name = "",
                `ifdef SLA_RAL_COVERAGE
                sla_ral_coverage_t cov_t = COVERAGE_OFF,
                `endif
                int            bus_num=0,
                int            dev_num=0,
                int            func_num=0,
                int            offset=0,
                int            size=0,
                sla_ral_data_t reset_val=0,
                boolean_t      mon_enabled = SLA_FALSE
             );
      super.new();
      set_space_addr("CFG",undef);
      set_space_addr("MEM",undef);
      set_space_addr("IO",undef);
      set_space_addr("MSG",undef);
      set_space_addr("CREG",undef); 
      set_space_addr("LT_MEM",undef);    
 build();

      `ifdef SLA_RAL_COVERAGE
      `CONFIG_RAL_COVERAGE(regname, COVERAGE_ON, FD_RW_SAMPLE)
      if(_coverage_type != COVERAGE_OFF) begin
        if(_sample_type inside {FD_RO_SAMPLE, RO_SAMPLE})
        `CREATE_OP_SAMPLE_COV(RAL_WRITE)
        else if(_sample_type inside {FD_WO_SAMPLE, WO_SAMPLE})
        `CREATE_OP_SAMPLE_COV(RAL_READ)
        else
        `CREATE_OP_SAMPLE_COV(RAL_NONE)
        `CREATE_DESIRED_COV
        `CREATE_ACTUAL_COV
      end
      `endif

  endfunction

  `ifdef SLA_RAL_COVERAGE
  `CREATE_RAL_OP_COVERGROUP
  covergroup desired_cg @(cov_ev);
     option.per_instance = 1;
     option.name = {sla_ral_file::regfilename,".",regname};
     `RAL_FIELD_CP(FET_EN_B_OVR_VAL, FET_EN_B_OVR_VAL.desired)
     `RAL_FIELD_CP_16(FET_EN_B_OVR_VAL, FET_EN_B_OVR_VAL.desired, 0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)
     `RAL_FIELD_CP_16(FET_EN_B_OVR_VAL, FET_EN_B_OVR_VAL.desired, 16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31)
  endgroup
  covergroup actual_cg @(cov_ev);
     option.per_instance = 1;
     option.name = {sla_ral_file::regfilename,".",regname};
     `RAL_FIELD_CP(FET_EN_B_OVR_VAL, FET_EN_B_OVR_VAL.actual)
     `RAL_FIELD_CP_16(FET_EN_B_OVR_VAL, FET_EN_B_OVR_VAL.actual, 0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)
     `RAL_FIELD_CP_16(FET_EN_B_OVR_VAL, FET_EN_B_OVR_VAL.actual, 16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31)
  endgroup
  `endif

  // --------------------------
  // add constraint based on database legal values
  // --------------------------
  function void build();
    FET_EN_B_OVR_VAL = new("FET_EN_B_OVR_VAL", "RW", 32, 0, {"IP_FORCE_FET_EN_OVR_VAL_1.FET_EN_B_OVR_VAL"});
    FET_EN_B_OVR_VAL.set_powerwell("primary");
    FET_EN_B_OVR_VAL.set_rand_mode(0);
   FET_EN_B_OVR_VAL.set_reset_signame("pmu_rst_b");
    void'(add_field( FET_EN_B_OVR_VAL ));

  endfunction


`DEFINE_NONE_SCOPE_REG_API(pmu_mmr_IP_FORCE_FET_EN_OVR_VAL_1_reg) 
endclass : pmu_mmr_IP_FORCE_FET_EN_OVR_VAL_1_reg

// ================================================

class pmu_mmr_GEN_CTL_0_reg extends sla_ral_reg;

  // --------------------------
  sla_ral_field GEN_CTL_0;
  sla_ral_field GEN_CTL_1;
  sla_ral_field GEN_CTL_2;
  sla_ral_field GEN_CTL_3;
  sla_ral_field GEN_CTL_4;
  sla_ral_field GEN_CTL_5;
  sla_ral_field GEN_CTL_6;
  sla_ral_field GEN_CTL_7;
  sla_ral_field GEN_CTL_8;
  sla_ral_field GEN_CTL_9;
  sla_ral_field GEN_CTL_10;
  sla_ral_field GEN_CTL_11;
  sla_ral_field GEN_CTL_12;
  sla_ral_field GEN_CTL_13;
  sla_ral_field GEN_CTL_14;
  sla_ral_field GEN_CTL_15;
  sla_ral_field GEN_CTL_16;
  sla_ral_field GEN_CTL_17;
  sla_ral_field GEN_CTL_18;
  sla_ral_field GEN_CTL_19;
  sla_ral_field GEN_CTL_20;
  sla_ral_field GEN_CTL_21;
  sla_ral_field GEN_CTL_22;
  sla_ral_field GEN_CTL_23;
  sla_ral_field GEN_CTL_24;
  sla_ral_field GEN_CTL_25;
  sla_ral_field GEN_CTL_26;
  sla_ral_field GEN_CTL_27;
  sla_ral_field GEN_CTL_28;
  sla_ral_field GEN_CTL_29;
  sla_ral_field GEN_CTL_30;
  sla_ral_field GEN_CTL_31;

  // --------------------------
  `ovm_object_utils(pmu_mmr_GEN_CTL_0_reg)

  // --------------------------
  function new(
                string         name = "",
                `ifdef SLA_RAL_COVERAGE
                sla_ral_coverage_t cov_t = COVERAGE_OFF,
                `endif
                int            bus_num=0,
                int            dev_num=0,
                int            func_num=0,
                int            offset=0,
                int            size=0,
                sla_ral_data_t reset_val=0,
                boolean_t      mon_enabled = SLA_FALSE
             );
      super.new();
      set_space_addr("CFG",undef);
      set_space_addr("MEM",undef);
      set_space_addr("IO",undef);
      set_space_addr("MSG",undef);
      set_space_addr("CREG",undef); 
      set_space_addr("LT_MEM",undef);    
 build();

      `ifdef SLA_RAL_COVERAGE
      `CONFIG_RAL_COVERAGE(regname, COVERAGE_ON, FD_RW_SAMPLE)
      if(_coverage_type != COVERAGE_OFF) begin
        if(_sample_type inside {FD_RO_SAMPLE, RO_SAMPLE})
        `CREATE_OP_SAMPLE_COV(RAL_WRITE)
        else if(_sample_type inside {FD_WO_SAMPLE, WO_SAMPLE})
        `CREATE_OP_SAMPLE_COV(RAL_READ)
        else
        `CREATE_OP_SAMPLE_COV(RAL_NONE)
        `CREATE_DESIRED_COV
        `CREATE_ACTUAL_COV
      end
      `endif

  endfunction

  `ifdef SLA_RAL_COVERAGE
  `CREATE_RAL_OP_COVERGROUP
  covergroup desired_cg @(cov_ev);
     option.per_instance = 1;
     option.name = {sla_ral_file::regfilename,".",regname};
     `RAL_FIELD_CP(GEN_CTL_0, GEN_CTL_0.desired)
     `RAL_FIELD_CP_1(GEN_CTL_0, GEN_CTL_0.desired, 0)
     `RAL_FIELD_CP(GEN_CTL_1, GEN_CTL_1.desired)
     `RAL_FIELD_CP_1(GEN_CTL_1, GEN_CTL_1.desired, 0)
     `RAL_FIELD_CP(GEN_CTL_2, GEN_CTL_2.desired)
     `RAL_FIELD_CP_1(GEN_CTL_2, GEN_CTL_2.desired, 0)
     `RAL_FIELD_CP(GEN_CTL_3, GEN_CTL_3.desired)
     `RAL_FIELD_CP_1(GEN_CTL_3, GEN_CTL_3.desired, 0)
     `RAL_FIELD_CP(GEN_CTL_4, GEN_CTL_4.desired)
     `RAL_FIELD_CP_1(GEN_CTL_4, GEN_CTL_4.desired, 0)
     `RAL_FIELD_CP(GEN_CTL_5, GEN_CTL_5.desired)
     `RAL_FIELD_CP_1(GEN_CTL_5, GEN_CTL_5.desired, 0)
     `RAL_FIELD_CP(GEN_CTL_6, GEN_CTL_6.desired)
     `RAL_FIELD_CP_1(GEN_CTL_6, GEN_CTL_6.desired, 0)
     `RAL_FIELD_CP(GEN_CTL_7, GEN_CTL_7.desired)
     `RAL_FIELD_CP_1(GEN_CTL_7, GEN_CTL_7.desired, 0)
     `RAL_FIELD_CP(GEN_CTL_8, GEN_CTL_8.desired)
     `RAL_FIELD_CP_1(GEN_CTL_8, GEN_CTL_8.desired, 0)
     `RAL_FIELD_CP(GEN_CTL_9, GEN_CTL_9.desired)
     `RAL_FIELD_CP_1(GEN_CTL_9, GEN_CTL_9.desired, 0)
     `RAL_FIELD_CP(GEN_CTL_10, GEN_CTL_10.desired)
     `RAL_FIELD_CP_1(GEN_CTL_10, GEN_CTL_10.desired, 0)
     `RAL_FIELD_CP(GEN_CTL_11, GEN_CTL_11.desired)
     `RAL_FIELD_CP_1(GEN_CTL_11, GEN_CTL_11.desired, 0)
     `RAL_FIELD_CP(GEN_CTL_12, GEN_CTL_12.desired)
     `RAL_FIELD_CP_1(GEN_CTL_12, GEN_CTL_12.desired, 0)
     `RAL_FIELD_CP(GEN_CTL_13, GEN_CTL_13.desired)
     `RAL_FIELD_CP_1(GEN_CTL_13, GEN_CTL_13.desired, 0)
     `RAL_FIELD_CP(GEN_CTL_14, GEN_CTL_14.desired)
     `RAL_FIELD_CP_1(GEN_CTL_14, GEN_CTL_14.desired, 0)
     `RAL_FIELD_CP(GEN_CTL_15, GEN_CTL_15.desired)
     `RAL_FIELD_CP_1(GEN_CTL_15, GEN_CTL_15.desired, 0)
     `RAL_FIELD_CP(GEN_CTL_16, GEN_CTL_16.desired)
     `RAL_FIELD_CP_1(GEN_CTL_16, GEN_CTL_16.desired, 0)
     `RAL_FIELD_CP(GEN_CTL_17, GEN_CTL_17.desired)
     `RAL_FIELD_CP_1(GEN_CTL_17, GEN_CTL_17.desired, 0)
     `RAL_FIELD_CP(GEN_CTL_18, GEN_CTL_18.desired)
     `RAL_FIELD_CP_1(GEN_CTL_18, GEN_CTL_18.desired, 0)
     `RAL_FIELD_CP(GEN_CTL_19, GEN_CTL_19.desired)
     `RAL_FIELD_CP_1(GEN_CTL_19, GEN_CTL_19.desired, 0)
     `RAL_FIELD_CP(GEN_CTL_20, GEN_CTL_20.desired)
     `RAL_FIELD_CP_1(GEN_CTL_20, GEN_CTL_20.desired, 0)
     `RAL_FIELD_CP(GEN_CTL_21, GEN_CTL_21.desired)
     `RAL_FIELD_CP_1(GEN_CTL_21, GEN_CTL_21.desired, 0)
     `RAL_FIELD_CP(GEN_CTL_22, GEN_CTL_22.desired)
     `RAL_FIELD_CP_1(GEN_CTL_22, GEN_CTL_22.desired, 0)
     `RAL_FIELD_CP(GEN_CTL_23, GEN_CTL_23.desired)
     `RAL_FIELD_CP_1(GEN_CTL_23, GEN_CTL_23.desired, 0)
     `RAL_FIELD_CP(GEN_CTL_24, GEN_CTL_24.desired)
     `RAL_FIELD_CP_1(GEN_CTL_24, GEN_CTL_24.desired, 0)
     `RAL_FIELD_CP(GEN_CTL_25, GEN_CTL_25.desired)
     `RAL_FIELD_CP_1(GEN_CTL_25, GEN_CTL_25.desired, 0)
     `RAL_FIELD_CP(GEN_CTL_26, GEN_CTL_26.desired)
     `RAL_FIELD_CP_1(GEN_CTL_26, GEN_CTL_26.desired, 0)
     `RAL_FIELD_CP(GEN_CTL_27, GEN_CTL_27.desired)
     `RAL_FIELD_CP_1(GEN_CTL_27, GEN_CTL_27.desired, 0)
     `RAL_FIELD_CP(GEN_CTL_28, GEN_CTL_28.desired)
     `RAL_FIELD_CP_1(GEN_CTL_28, GEN_CTL_28.desired, 0)
     `RAL_FIELD_CP(GEN_CTL_29, GEN_CTL_29.desired)
     `RAL_FIELD_CP_1(GEN_CTL_29, GEN_CTL_29.desired, 0)
     `RAL_FIELD_CP(GEN_CTL_30, GEN_CTL_30.desired)
     `RAL_FIELD_CP_1(GEN_CTL_30, GEN_CTL_30.desired, 0)
     `RAL_FIELD_CP(GEN_CTL_31, GEN_CTL_31.desired)
     `RAL_FIELD_CP_1(GEN_CTL_31, GEN_CTL_31.desired, 0)
  endgroup
  covergroup actual_cg @(cov_ev);
     option.per_instance = 1;
     option.name = {sla_ral_file::regfilename,".",regname};
     `RAL_FIELD_CP(GEN_CTL_0, GEN_CTL_0.actual)
     `RAL_FIELD_CP_1(GEN_CTL_0, GEN_CTL_0.actual, 0)
     `RAL_FIELD_CP(GEN_CTL_1, GEN_CTL_1.actual)
     `RAL_FIELD_CP_1(GEN_CTL_1, GEN_CTL_1.actual, 0)
     `RAL_FIELD_CP(GEN_CTL_2, GEN_CTL_2.actual)
     `RAL_FIELD_CP_1(GEN_CTL_2, GEN_CTL_2.actual, 0)
     `RAL_FIELD_CP(GEN_CTL_3, GEN_CTL_3.actual)
     `RAL_FIELD_CP_1(GEN_CTL_3, GEN_CTL_3.actual, 0)
     `RAL_FIELD_CP(GEN_CTL_4, GEN_CTL_4.actual)
     `RAL_FIELD_CP_1(GEN_CTL_4, GEN_CTL_4.actual, 0)
     `RAL_FIELD_CP(GEN_CTL_5, GEN_CTL_5.actual)
     `RAL_FIELD_CP_1(GEN_CTL_5, GEN_CTL_5.actual, 0)
     `RAL_FIELD_CP(GEN_CTL_6, GEN_CTL_6.actual)
     `RAL_FIELD_CP_1(GEN_CTL_6, GEN_CTL_6.actual, 0)
     `RAL_FIELD_CP(GEN_CTL_7, GEN_CTL_7.actual)
     `RAL_FIELD_CP_1(GEN_CTL_7, GEN_CTL_7.actual, 0)
     `RAL_FIELD_CP(GEN_CTL_8, GEN_CTL_8.actual)
     `RAL_FIELD_CP_1(GEN_CTL_8, GEN_CTL_8.actual, 0)
     `RAL_FIELD_CP(GEN_CTL_9, GEN_CTL_9.actual)
     `RAL_FIELD_CP_1(GEN_CTL_9, GEN_CTL_9.actual, 0)
     `RAL_FIELD_CP(GEN_CTL_10, GEN_CTL_10.actual)
     `RAL_FIELD_CP_1(GEN_CTL_10, GEN_CTL_10.actual, 0)
     `RAL_FIELD_CP(GEN_CTL_11, GEN_CTL_11.actual)
     `RAL_FIELD_CP_1(GEN_CTL_11, GEN_CTL_11.actual, 0)
     `RAL_FIELD_CP(GEN_CTL_12, GEN_CTL_12.actual)
     `RAL_FIELD_CP_1(GEN_CTL_12, GEN_CTL_12.actual, 0)
     `RAL_FIELD_CP(GEN_CTL_13, GEN_CTL_13.actual)
     `RAL_FIELD_CP_1(GEN_CTL_13, GEN_CTL_13.actual, 0)
     `RAL_FIELD_CP(GEN_CTL_14, GEN_CTL_14.actual)
     `RAL_FIELD_CP_1(GEN_CTL_14, GEN_CTL_14.actual, 0)
     `RAL_FIELD_CP(GEN_CTL_15, GEN_CTL_15.actual)
     `RAL_FIELD_CP_1(GEN_CTL_15, GEN_CTL_15.actual, 0)
     `RAL_FIELD_CP(GEN_CTL_16, GEN_CTL_16.actual)
     `RAL_FIELD_CP_1(GEN_CTL_16, GEN_CTL_16.actual, 0)
     `RAL_FIELD_CP(GEN_CTL_17, GEN_CTL_17.actual)
     `RAL_FIELD_CP_1(GEN_CTL_17, GEN_CTL_17.actual, 0)
     `RAL_FIELD_CP(GEN_CTL_18, GEN_CTL_18.actual)
     `RAL_FIELD_CP_1(GEN_CTL_18, GEN_CTL_18.actual, 0)
     `RAL_FIELD_CP(GEN_CTL_19, GEN_CTL_19.actual)
     `RAL_FIELD_CP_1(GEN_CTL_19, GEN_CTL_19.actual, 0)
     `RAL_FIELD_CP(GEN_CTL_20, GEN_CTL_20.actual)
     `RAL_FIELD_CP_1(GEN_CTL_20, GEN_CTL_20.actual, 0)
     `RAL_FIELD_CP(GEN_CTL_21, GEN_CTL_21.actual)
     `RAL_FIELD_CP_1(GEN_CTL_21, GEN_CTL_21.actual, 0)
     `RAL_FIELD_CP(GEN_CTL_22, GEN_CTL_22.actual)
     `RAL_FIELD_CP_1(GEN_CTL_22, GEN_CTL_22.actual, 0)
     `RAL_FIELD_CP(GEN_CTL_23, GEN_CTL_23.actual)
     `RAL_FIELD_CP_1(GEN_CTL_23, GEN_CTL_23.actual, 0)
     `RAL_FIELD_CP(GEN_CTL_24, GEN_CTL_24.actual)
     `RAL_FIELD_CP_1(GEN_CTL_24, GEN_CTL_24.actual, 0)
     `RAL_FIELD_CP(GEN_CTL_25, GEN_CTL_25.actual)
     `RAL_FIELD_CP_1(GEN_CTL_25, GEN_CTL_25.actual, 0)
     `RAL_FIELD_CP(GEN_CTL_26, GEN_CTL_26.actual)
     `RAL_FIELD_CP_1(GEN_CTL_26, GEN_CTL_26.actual, 0)
     `RAL_FIELD_CP(GEN_CTL_27, GEN_CTL_27.actual)
     `RAL_FIELD_CP_1(GEN_CTL_27, GEN_CTL_27.actual, 0)
     `RAL_FIELD_CP(GEN_CTL_28, GEN_CTL_28.actual)
     `RAL_FIELD_CP_1(GEN_CTL_28, GEN_CTL_28.actual, 0)
     `RAL_FIELD_CP(GEN_CTL_29, GEN_CTL_29.actual)
     `RAL_FIELD_CP_1(GEN_CTL_29, GEN_CTL_29.actual, 0)
     `RAL_FIELD_CP(GEN_CTL_30, GEN_CTL_30.actual)
     `RAL_FIELD_CP_1(GEN_CTL_30, GEN_CTL_30.actual, 0)
     `RAL_FIELD_CP(GEN_CTL_31, GEN_CTL_31.actual)
     `RAL_FIELD_CP_1(GEN_CTL_31, GEN_CTL_31.actual, 0)
  endgroup
  `endif

  // --------------------------
  // add constraint based on database legal values
  // --------------------------
  function void build();
    GEN_CTL_0 = new("GEN_CTL_0", "RW/V", 1, 0, {"GEN_CTL_0.GEN_CTL_0"});
    GEN_CTL_0.set_powerwell("primary");
    GEN_CTL_0.set_rand_mode(0);
   GEN_CTL_0.set_reset_signame("pmu_rst_b");
    void'(add_field( GEN_CTL_0 ));

    GEN_CTL_1 = new("GEN_CTL_1", "RW/V", 1, 1, {"GEN_CTL_0.GEN_CTL_1"});
    GEN_CTL_1.set_powerwell("primary");
    GEN_CTL_1.set_rand_mode(0);
   GEN_CTL_1.set_reset_signame("pmu_rst_b");
    void'(add_field( GEN_CTL_1 ));

    GEN_CTL_2 = new("GEN_CTL_2", "RW/V", 1, 2, {"GEN_CTL_0.GEN_CTL_2"});
    GEN_CTL_2.set_powerwell("primary");
    GEN_CTL_2.set_rand_mode(0);
   GEN_CTL_2.set_reset_signame("pmu_rst_b");
    void'(add_field( GEN_CTL_2 ));

    GEN_CTL_3 = new("GEN_CTL_3", "RW/V", 1, 3, {"GEN_CTL_0.GEN_CTL_3"});
    GEN_CTL_3.set_powerwell("primary");
    GEN_CTL_3.set_rand_mode(0);
   GEN_CTL_3.set_reset_signame("pmu_rst_b");
    void'(add_field( GEN_CTL_3 ));

    GEN_CTL_4 = new("GEN_CTL_4", "RW/V", 1, 4, {"GEN_CTL_0.GEN_CTL_4"});
    GEN_CTL_4.set_powerwell("primary");
    GEN_CTL_4.set_rand_mode(0);
   GEN_CTL_4.set_reset_signame("pmu_rst_b");
    void'(add_field( GEN_CTL_4 ));

    GEN_CTL_5 = new("GEN_CTL_5", "RW/V", 1, 5, {"GEN_CTL_0.GEN_CTL_5"});
    GEN_CTL_5.set_powerwell("primary");
    GEN_CTL_5.set_rand_mode(0);
   GEN_CTL_5.set_reset_signame("pmu_rst_b");
    void'(add_field( GEN_CTL_5 ));

    GEN_CTL_6 = new("GEN_CTL_6", "RW/V", 1, 6, {"GEN_CTL_0.GEN_CTL_6"});
    GEN_CTL_6.set_powerwell("primary");
    GEN_CTL_6.set_rand_mode(0);
   GEN_CTL_6.set_reset_signame("pmu_rst_b");
    void'(add_field( GEN_CTL_6 ));

    GEN_CTL_7 = new("GEN_CTL_7", "RW/V", 1, 7, {"GEN_CTL_0.GEN_CTL_7"});
    GEN_CTL_7.set_powerwell("primary");
    GEN_CTL_7.set_rand_mode(0);
   GEN_CTL_7.set_reset_signame("pmu_rst_b");
    void'(add_field( GEN_CTL_7 ));

    GEN_CTL_8 = new("GEN_CTL_8", "RW/V", 1, 8, {"GEN_CTL_0.GEN_CTL_8"});
    GEN_CTL_8.set_powerwell("primary");
    GEN_CTL_8.set_rand_mode(0);
   GEN_CTL_8.set_reset_signame("pmu_rst_b");
    void'(add_field( GEN_CTL_8 ));

    GEN_CTL_9 = new("GEN_CTL_9", "RW/V", 1, 9, {"GEN_CTL_0.GEN_CTL_9"});
    GEN_CTL_9.set_powerwell("primary");
    GEN_CTL_9.set_rand_mode(0);
   GEN_CTL_9.set_reset_signame("pmu_rst_b");
    void'(add_field( GEN_CTL_9 ));

    GEN_CTL_10 = new("GEN_CTL_10", "RW/V", 1, 10, {"GEN_CTL_0.GEN_CTL_10"});
    GEN_CTL_10.set_powerwell("primary");
    GEN_CTL_10.set_rand_mode(0);
   GEN_CTL_10.set_reset_signame("pmu_rst_b");
    void'(add_field( GEN_CTL_10 ));

    GEN_CTL_11 = new("GEN_CTL_11", "RW/V", 1, 11, {"GEN_CTL_0.GEN_CTL_11"});
    GEN_CTL_11.set_powerwell("primary");
    GEN_CTL_11.set_rand_mode(0);
   GEN_CTL_11.set_reset_signame("pmu_rst_b");
    void'(add_field( GEN_CTL_11 ));

    GEN_CTL_12 = new("GEN_CTL_12", "RW/V", 1, 12, {"GEN_CTL_0.GEN_CTL_12"});
    GEN_CTL_12.set_powerwell("primary");
    GEN_CTL_12.set_rand_mode(0);
   GEN_CTL_12.set_reset_signame("pmu_rst_b");
    void'(add_field( GEN_CTL_12 ));

    GEN_CTL_13 = new("GEN_CTL_13", "RW/V", 1, 13, {"GEN_CTL_0.GEN_CTL_13"});
    GEN_CTL_13.set_powerwell("primary");
    GEN_CTL_13.set_rand_mode(0);
   GEN_CTL_13.set_reset_signame("pmu_rst_b");
    void'(add_field( GEN_CTL_13 ));

    GEN_CTL_14 = new("GEN_CTL_14", "RW/V", 1, 14, {"GEN_CTL_0.GEN_CTL_14"});
    GEN_CTL_14.set_powerwell("primary");
    GEN_CTL_14.set_rand_mode(0);
   GEN_CTL_14.set_reset_signame("pmu_rst_b");
    void'(add_field( GEN_CTL_14 ));

    GEN_CTL_15 = new("GEN_CTL_15", "RW/V", 1, 15, {"GEN_CTL_0.GEN_CTL_15"});
    GEN_CTL_15.set_powerwell("primary");
    GEN_CTL_15.set_rand_mode(0);
   GEN_CTL_15.set_reset_signame("pmu_rst_b");
    void'(add_field( GEN_CTL_15 ));

    GEN_CTL_16 = new("GEN_CTL_16", "RW/V", 1, 16, {"GEN_CTL_0.GEN_CTL_16"});
    GEN_CTL_16.set_powerwell("primary");
    GEN_CTL_16.set_rand_mode(0);
   GEN_CTL_16.set_reset_signame("pmu_rst_b");
    void'(add_field( GEN_CTL_16 ));

    GEN_CTL_17 = new("GEN_CTL_17", "RW/V", 1, 17, {"GEN_CTL_0.GEN_CTL_17"});
    GEN_CTL_17.set_powerwell("primary");
    GEN_CTL_17.set_rand_mode(0);
   GEN_CTL_17.set_reset_signame("pmu_rst_b");
    void'(add_field( GEN_CTL_17 ));

    GEN_CTL_18 = new("GEN_CTL_18", "RW/V", 1, 18, {"GEN_CTL_0.GEN_CTL_18"});
    GEN_CTL_18.set_powerwell("primary");
    GEN_CTL_18.set_rand_mode(0);
   GEN_CTL_18.set_reset_signame("pmu_rst_b");
    void'(add_field( GEN_CTL_18 ));

    GEN_CTL_19 = new("GEN_CTL_19", "RW/V", 1, 19, {"GEN_CTL_0.GEN_CTL_19"});
    GEN_CTL_19.set_powerwell("primary");
    GEN_CTL_19.set_rand_mode(0);
   GEN_CTL_19.set_reset_signame("pmu_rst_b");
    void'(add_field( GEN_CTL_19 ));

    GEN_CTL_20 = new("GEN_CTL_20", "RW/V", 1, 20, {"GEN_CTL_0.GEN_CTL_20"});
    GEN_CTL_20.set_powerwell("primary");
    GEN_CTL_20.set_rand_mode(0);
   GEN_CTL_20.set_reset_signame("pmu_rst_b");
    void'(add_field( GEN_CTL_20 ));

    GEN_CTL_21 = new("GEN_CTL_21", "RW/V", 1, 21, {"GEN_CTL_0.GEN_CTL_21"});
    GEN_CTL_21.set_powerwell("primary");
    GEN_CTL_21.set_rand_mode(0);
   GEN_CTL_21.set_reset_signame("pmu_rst_b");
    void'(add_field( GEN_CTL_21 ));

    GEN_CTL_22 = new("GEN_CTL_22", "RW/V", 1, 22, {"GEN_CTL_0.GEN_CTL_22"});
    GEN_CTL_22.set_powerwell("primary");
    GEN_CTL_22.set_rand_mode(0);
   GEN_CTL_22.set_reset_signame("pmu_rst_b");
    void'(add_field( GEN_CTL_22 ));

    GEN_CTL_23 = new("GEN_CTL_23", "RW/V", 1, 23, {"GEN_CTL_0.GEN_CTL_23"});
    GEN_CTL_23.set_powerwell("primary");
    GEN_CTL_23.set_rand_mode(0);
   GEN_CTL_23.set_reset_signame("pmu_rst_b");
    void'(add_field( GEN_CTL_23 ));

    GEN_CTL_24 = new("GEN_CTL_24", "RW/V", 1, 24, {"GEN_CTL_0.GEN_CTL_24"});
    GEN_CTL_24.set_powerwell("primary");
    GEN_CTL_24.set_rand_mode(0);
   GEN_CTL_24.set_reset_signame("pmu_rst_b");
    void'(add_field( GEN_CTL_24 ));

    GEN_CTL_25 = new("GEN_CTL_25", "RW/V", 1, 25, {"GEN_CTL_0.GEN_CTL_25"});
    GEN_CTL_25.set_powerwell("primary");
    GEN_CTL_25.set_rand_mode(0);
   GEN_CTL_25.set_reset_signame("pmu_rst_b");
    void'(add_field( GEN_CTL_25 ));

    GEN_CTL_26 = new("GEN_CTL_26", "RW/V", 1, 26, {"GEN_CTL_0.GEN_CTL_26"});
    GEN_CTL_26.set_powerwell("primary");
    GEN_CTL_26.set_rand_mode(0);
   GEN_CTL_26.set_reset_signame("pmu_rst_b");
    void'(add_field( GEN_CTL_26 ));

    GEN_CTL_27 = new("GEN_CTL_27", "RW/V", 1, 27, {"GEN_CTL_0.GEN_CTL_27"});
    GEN_CTL_27.set_powerwell("primary");
    GEN_CTL_27.set_rand_mode(0);
   GEN_CTL_27.set_reset_signame("pmu_rst_b");
    void'(add_field( GEN_CTL_27 ));

    GEN_CTL_28 = new("GEN_CTL_28", "RW/V", 1, 28, {"GEN_CTL_0.GEN_CTL_28"});
    GEN_CTL_28.set_powerwell("primary");
    GEN_CTL_28.set_rand_mode(0);
   GEN_CTL_28.set_reset_signame("pmu_rst_b");
    void'(add_field( GEN_CTL_28 ));

    GEN_CTL_29 = new("GEN_CTL_29", "RW/V", 1, 29, {"GEN_CTL_0.GEN_CTL_29"});
    GEN_CTL_29.set_powerwell("primary");
    GEN_CTL_29.set_rand_mode(0);
   GEN_CTL_29.set_reset_signame("pmu_rst_b");
    void'(add_field( GEN_CTL_29 ));

    GEN_CTL_30 = new("GEN_CTL_30", "RW/V", 1, 30, {"GEN_CTL_0.GEN_CTL_30"});
    GEN_CTL_30.set_powerwell("primary");
    GEN_CTL_30.set_rand_mode(0);
   GEN_CTL_30.set_reset_signame("pmu_rst_b");
    void'(add_field( GEN_CTL_30 ));

    GEN_CTL_31 = new("GEN_CTL_31", "RW/V", 1, 31, {"GEN_CTL_0.GEN_CTL_31"});
    GEN_CTL_31.set_powerwell("primary");
    GEN_CTL_31.set_rand_mode(0);
   GEN_CTL_31.set_reset_signame("pmu_rst_b");
    void'(add_field( GEN_CTL_31 ));

  endfunction


`DEFINE_NONE_SCOPE_REG_API(pmu_mmr_GEN_CTL_0_reg) 
endclass : pmu_mmr_GEN_CTL_0_reg

// ================================================

class pmu_mmr_GEN_CTL_1_reg extends sla_ral_reg;

  // --------------------------
  sla_ral_field GEN_CTL_0;
  sla_ral_field GEN_CTL_1;
  sla_ral_field GEN_CTL_2;
  sla_ral_field GEN_CTL_3;
  sla_ral_field GEN_CTL_4;
  sla_ral_field GEN_CTL_5;
  sla_ral_field GEN_CTL_6;
  sla_ral_field GEN_CTL_7;
  sla_ral_field GEN_CTL_8;
  sla_ral_field GEN_CTL_9;
  sla_ral_field GEN_CTL_10;
  sla_ral_field GEN_CTL_11;
  sla_ral_field GEN_CTL_12;
  sla_ral_field GEN_CTL_13;
  sla_ral_field GEN_CTL_14;
  sla_ral_field GEN_CTL_15;
  sla_ral_field GEN_CTL_16;
  sla_ral_field GEN_CTL_17;
  sla_ral_field GEN_CTL_18;
  sla_ral_field GEN_CTL_19;
  sla_ral_field GEN_CTL_20;
  sla_ral_field GEN_CTL_21;
  sla_ral_field GEN_CTL_22;
  sla_ral_field GEN_CTL_23;
  sla_ral_field GEN_CTL_24;
  sla_ral_field GEN_CTL_25;
  sla_ral_field GEN_CTL_26;
  sla_ral_field GEN_CTL_27;
  sla_ral_field GEN_CTL_28;
  sla_ral_field GEN_CTL_29;
  sla_ral_field GEN_CTL_30;
  sla_ral_field GEN_CTL_31;

  // --------------------------
  `ovm_object_utils(pmu_mmr_GEN_CTL_1_reg)

  // --------------------------
  function new(
                string         name = "",
                `ifdef SLA_RAL_COVERAGE
                sla_ral_coverage_t cov_t = COVERAGE_OFF,
                `endif
                int            bus_num=0,
                int            dev_num=0,
                int            func_num=0,
                int            offset=0,
                int            size=0,
                sla_ral_data_t reset_val=0,
                boolean_t      mon_enabled = SLA_FALSE
             );
      super.new();
      set_space_addr("CFG",undef);
      set_space_addr("MEM",undef);
      set_space_addr("IO",undef);
      set_space_addr("MSG",undef);
      set_space_addr("CREG",undef); 
      set_space_addr("LT_MEM",undef);    
 build();

      `ifdef SLA_RAL_COVERAGE
      `CONFIG_RAL_COVERAGE(regname, COVERAGE_ON, FD_RW_SAMPLE)
      if(_coverage_type != COVERAGE_OFF) begin
        if(_sample_type inside {FD_RO_SAMPLE, RO_SAMPLE})
        `CREATE_OP_SAMPLE_COV(RAL_WRITE)
        else if(_sample_type inside {FD_WO_SAMPLE, WO_SAMPLE})
        `CREATE_OP_SAMPLE_COV(RAL_READ)
        else
        `CREATE_OP_SAMPLE_COV(RAL_NONE)
        `CREATE_DESIRED_COV
        `CREATE_ACTUAL_COV
      end
      `endif

  endfunction

  `ifdef SLA_RAL_COVERAGE
  `CREATE_RAL_OP_COVERGROUP
  covergroup desired_cg @(cov_ev);
     option.per_instance = 1;
     option.name = {sla_ral_file::regfilename,".",regname};
     `RAL_FIELD_CP(GEN_CTL_0, GEN_CTL_0.desired)
     `RAL_FIELD_CP_1(GEN_CTL_0, GEN_CTL_0.desired, 0)
     `RAL_FIELD_CP(GEN_CTL_1, GEN_CTL_1.desired)
     `RAL_FIELD_CP_1(GEN_CTL_1, GEN_CTL_1.desired, 0)
     `RAL_FIELD_CP(GEN_CTL_2, GEN_CTL_2.desired)
     `RAL_FIELD_CP_1(GEN_CTL_2, GEN_CTL_2.desired, 0)
     `RAL_FIELD_CP(GEN_CTL_3, GEN_CTL_3.desired)
     `RAL_FIELD_CP_1(GEN_CTL_3, GEN_CTL_3.desired, 0)
     `RAL_FIELD_CP(GEN_CTL_4, GEN_CTL_4.desired)
     `RAL_FIELD_CP_1(GEN_CTL_4, GEN_CTL_4.desired, 0)
     `RAL_FIELD_CP(GEN_CTL_5, GEN_CTL_5.desired)
     `RAL_FIELD_CP_1(GEN_CTL_5, GEN_CTL_5.desired, 0)
     `RAL_FIELD_CP(GEN_CTL_6, GEN_CTL_6.desired)
     `RAL_FIELD_CP_1(GEN_CTL_6, GEN_CTL_6.desired, 0)
     `RAL_FIELD_CP(GEN_CTL_7, GEN_CTL_7.desired)
     `RAL_FIELD_CP_1(GEN_CTL_7, GEN_CTL_7.desired, 0)
     `RAL_FIELD_CP(GEN_CTL_8, GEN_CTL_8.desired)
     `RAL_FIELD_CP_1(GEN_CTL_8, GEN_CTL_8.desired, 0)
     `RAL_FIELD_CP(GEN_CTL_9, GEN_CTL_9.desired)
     `RAL_FIELD_CP_1(GEN_CTL_9, GEN_CTL_9.desired, 0)
     `RAL_FIELD_CP(GEN_CTL_10, GEN_CTL_10.desired)
     `RAL_FIELD_CP_1(GEN_CTL_10, GEN_CTL_10.desired, 0)
     `RAL_FIELD_CP(GEN_CTL_11, GEN_CTL_11.desired)
     `RAL_FIELD_CP_1(GEN_CTL_11, GEN_CTL_11.desired, 0)
     `RAL_FIELD_CP(GEN_CTL_12, GEN_CTL_12.desired)
     `RAL_FIELD_CP_1(GEN_CTL_12, GEN_CTL_12.desired, 0)
     `RAL_FIELD_CP(GEN_CTL_13, GEN_CTL_13.desired)
     `RAL_FIELD_CP_1(GEN_CTL_13, GEN_CTL_13.desired, 0)
     `RAL_FIELD_CP(GEN_CTL_14, GEN_CTL_14.desired)
     `RAL_FIELD_CP_1(GEN_CTL_14, GEN_CTL_14.desired, 0)
     `RAL_FIELD_CP(GEN_CTL_15, GEN_CTL_15.desired)
     `RAL_FIELD_CP_1(GEN_CTL_15, GEN_CTL_15.desired, 0)
     `RAL_FIELD_CP(GEN_CTL_16, GEN_CTL_16.desired)
     `RAL_FIELD_CP_1(GEN_CTL_16, GEN_CTL_16.desired, 0)
     `RAL_FIELD_CP(GEN_CTL_17, GEN_CTL_17.desired)
     `RAL_FIELD_CP_1(GEN_CTL_17, GEN_CTL_17.desired, 0)
     `RAL_FIELD_CP(GEN_CTL_18, GEN_CTL_18.desired)
     `RAL_FIELD_CP_1(GEN_CTL_18, GEN_CTL_18.desired, 0)
     `RAL_FIELD_CP(GEN_CTL_19, GEN_CTL_19.desired)
     `RAL_FIELD_CP_1(GEN_CTL_19, GEN_CTL_19.desired, 0)
     `RAL_FIELD_CP(GEN_CTL_20, GEN_CTL_20.desired)
     `RAL_FIELD_CP_1(GEN_CTL_20, GEN_CTL_20.desired, 0)
     `RAL_FIELD_CP(GEN_CTL_21, GEN_CTL_21.desired)
     `RAL_FIELD_CP_1(GEN_CTL_21, GEN_CTL_21.desired, 0)
     `RAL_FIELD_CP(GEN_CTL_22, GEN_CTL_22.desired)
     `RAL_FIELD_CP_1(GEN_CTL_22, GEN_CTL_22.desired, 0)
     `RAL_FIELD_CP(GEN_CTL_23, GEN_CTL_23.desired)
     `RAL_FIELD_CP_1(GEN_CTL_23, GEN_CTL_23.desired, 0)
     `RAL_FIELD_CP(GEN_CTL_24, GEN_CTL_24.desired)
     `RAL_FIELD_CP_1(GEN_CTL_24, GEN_CTL_24.desired, 0)
     `RAL_FIELD_CP(GEN_CTL_25, GEN_CTL_25.desired)
     `RAL_FIELD_CP_1(GEN_CTL_25, GEN_CTL_25.desired, 0)
     `RAL_FIELD_CP(GEN_CTL_26, GEN_CTL_26.desired)
     `RAL_FIELD_CP_1(GEN_CTL_26, GEN_CTL_26.desired, 0)
     `RAL_FIELD_CP(GEN_CTL_27, GEN_CTL_27.desired)
     `RAL_FIELD_CP_1(GEN_CTL_27, GEN_CTL_27.desired, 0)
     `RAL_FIELD_CP(GEN_CTL_28, GEN_CTL_28.desired)
     `RAL_FIELD_CP_1(GEN_CTL_28, GEN_CTL_28.desired, 0)
     `RAL_FIELD_CP(GEN_CTL_29, GEN_CTL_29.desired)
     `RAL_FIELD_CP_1(GEN_CTL_29, GEN_CTL_29.desired, 0)
     `RAL_FIELD_CP(GEN_CTL_30, GEN_CTL_30.desired)
     `RAL_FIELD_CP_1(GEN_CTL_30, GEN_CTL_30.desired, 0)
     `RAL_FIELD_CP(GEN_CTL_31, GEN_CTL_31.desired)
     `RAL_FIELD_CP_1(GEN_CTL_31, GEN_CTL_31.desired, 0)
  endgroup
  covergroup actual_cg @(cov_ev);
     option.per_instance = 1;
     option.name = {sla_ral_file::regfilename,".",regname};
     `RAL_FIELD_CP(GEN_CTL_0, GEN_CTL_0.actual)
     `RAL_FIELD_CP_1(GEN_CTL_0, GEN_CTL_0.actual, 0)
     `RAL_FIELD_CP(GEN_CTL_1, GEN_CTL_1.actual)
     `RAL_FIELD_CP_1(GEN_CTL_1, GEN_CTL_1.actual, 0)
     `RAL_FIELD_CP(GEN_CTL_2, GEN_CTL_2.actual)
     `RAL_FIELD_CP_1(GEN_CTL_2, GEN_CTL_2.actual, 0)
     `RAL_FIELD_CP(GEN_CTL_3, GEN_CTL_3.actual)
     `RAL_FIELD_CP_1(GEN_CTL_3, GEN_CTL_3.actual, 0)
     `RAL_FIELD_CP(GEN_CTL_4, GEN_CTL_4.actual)
     `RAL_FIELD_CP_1(GEN_CTL_4, GEN_CTL_4.actual, 0)
     `RAL_FIELD_CP(GEN_CTL_5, GEN_CTL_5.actual)
     `RAL_FIELD_CP_1(GEN_CTL_5, GEN_CTL_5.actual, 0)
     `RAL_FIELD_CP(GEN_CTL_6, GEN_CTL_6.actual)
     `RAL_FIELD_CP_1(GEN_CTL_6, GEN_CTL_6.actual, 0)
     `RAL_FIELD_CP(GEN_CTL_7, GEN_CTL_7.actual)
     `RAL_FIELD_CP_1(GEN_CTL_7, GEN_CTL_7.actual, 0)
     `RAL_FIELD_CP(GEN_CTL_8, GEN_CTL_8.actual)
     `RAL_FIELD_CP_1(GEN_CTL_8, GEN_CTL_8.actual, 0)
     `RAL_FIELD_CP(GEN_CTL_9, GEN_CTL_9.actual)
     `RAL_FIELD_CP_1(GEN_CTL_9, GEN_CTL_9.actual, 0)
     `RAL_FIELD_CP(GEN_CTL_10, GEN_CTL_10.actual)
     `RAL_FIELD_CP_1(GEN_CTL_10, GEN_CTL_10.actual, 0)
     `RAL_FIELD_CP(GEN_CTL_11, GEN_CTL_11.actual)
     `RAL_FIELD_CP_1(GEN_CTL_11, GEN_CTL_11.actual, 0)
     `RAL_FIELD_CP(GEN_CTL_12, GEN_CTL_12.actual)
     `RAL_FIELD_CP_1(GEN_CTL_12, GEN_CTL_12.actual, 0)
     `RAL_FIELD_CP(GEN_CTL_13, GEN_CTL_13.actual)
     `RAL_FIELD_CP_1(GEN_CTL_13, GEN_CTL_13.actual, 0)
     `RAL_FIELD_CP(GEN_CTL_14, GEN_CTL_14.actual)
     `RAL_FIELD_CP_1(GEN_CTL_14, GEN_CTL_14.actual, 0)
     `RAL_FIELD_CP(GEN_CTL_15, GEN_CTL_15.actual)
     `RAL_FIELD_CP_1(GEN_CTL_15, GEN_CTL_15.actual, 0)
     `RAL_FIELD_CP(GEN_CTL_16, GEN_CTL_16.actual)
     `RAL_FIELD_CP_1(GEN_CTL_16, GEN_CTL_16.actual, 0)
     `RAL_FIELD_CP(GEN_CTL_17, GEN_CTL_17.actual)
     `RAL_FIELD_CP_1(GEN_CTL_17, GEN_CTL_17.actual, 0)
     `RAL_FIELD_CP(GEN_CTL_18, GEN_CTL_18.actual)
     `RAL_FIELD_CP_1(GEN_CTL_18, GEN_CTL_18.actual, 0)
     `RAL_FIELD_CP(GEN_CTL_19, GEN_CTL_19.actual)
     `RAL_FIELD_CP_1(GEN_CTL_19, GEN_CTL_19.actual, 0)
     `RAL_FIELD_CP(GEN_CTL_20, GEN_CTL_20.actual)
     `RAL_FIELD_CP_1(GEN_CTL_20, GEN_CTL_20.actual, 0)
     `RAL_FIELD_CP(GEN_CTL_21, GEN_CTL_21.actual)
     `RAL_FIELD_CP_1(GEN_CTL_21, GEN_CTL_21.actual, 0)
     `RAL_FIELD_CP(GEN_CTL_22, GEN_CTL_22.actual)
     `RAL_FIELD_CP_1(GEN_CTL_22, GEN_CTL_22.actual, 0)
     `RAL_FIELD_CP(GEN_CTL_23, GEN_CTL_23.actual)
     `RAL_FIELD_CP_1(GEN_CTL_23, GEN_CTL_23.actual, 0)
     `RAL_FIELD_CP(GEN_CTL_24, GEN_CTL_24.actual)
     `RAL_FIELD_CP_1(GEN_CTL_24, GEN_CTL_24.actual, 0)
     `RAL_FIELD_CP(GEN_CTL_25, GEN_CTL_25.actual)
     `RAL_FIELD_CP_1(GEN_CTL_25, GEN_CTL_25.actual, 0)
     `RAL_FIELD_CP(GEN_CTL_26, GEN_CTL_26.actual)
     `RAL_FIELD_CP_1(GEN_CTL_26, GEN_CTL_26.actual, 0)
     `RAL_FIELD_CP(GEN_CTL_27, GEN_CTL_27.actual)
     `RAL_FIELD_CP_1(GEN_CTL_27, GEN_CTL_27.actual, 0)
     `RAL_FIELD_CP(GEN_CTL_28, GEN_CTL_28.actual)
     `RAL_FIELD_CP_1(GEN_CTL_28, GEN_CTL_28.actual, 0)
     `RAL_FIELD_CP(GEN_CTL_29, GEN_CTL_29.actual)
     `RAL_FIELD_CP_1(GEN_CTL_29, GEN_CTL_29.actual, 0)
     `RAL_FIELD_CP(GEN_CTL_30, GEN_CTL_30.actual)
     `RAL_FIELD_CP_1(GEN_CTL_30, GEN_CTL_30.actual, 0)
     `RAL_FIELD_CP(GEN_CTL_31, GEN_CTL_31.actual)
     `RAL_FIELD_CP_1(GEN_CTL_31, GEN_CTL_31.actual, 0)
  endgroup
  `endif

  // --------------------------
  // add constraint based on database legal values
  // --------------------------
  function void build();
    GEN_CTL_0 = new("GEN_CTL_0", "RW/V", 1, 0, {"GEN_CTL_1.GEN_CTL_0"});
    GEN_CTL_0.set_powerwell("primary");
    GEN_CTL_0.set_rand_mode(0);
   GEN_CTL_0.set_reset_signame("pmu_rst_b");
    void'(add_field( GEN_CTL_0 ));

    GEN_CTL_1 = new("GEN_CTL_1", "RW/V", 1, 1, {"GEN_CTL_1.GEN_CTL_1"});
    GEN_CTL_1.set_powerwell("primary");
    GEN_CTL_1.set_rand_mode(0);
   GEN_CTL_1.set_reset_signame("pmu_rst_b");
    void'(add_field( GEN_CTL_1 ));

    GEN_CTL_2 = new("GEN_CTL_2", "RW/V", 1, 2, {"GEN_CTL_1.GEN_CTL_2"});
    GEN_CTL_2.set_powerwell("primary");
    GEN_CTL_2.set_rand_mode(0);
   GEN_CTL_2.set_reset_signame("pmu_rst_b");
    void'(add_field( GEN_CTL_2 ));

    GEN_CTL_3 = new("GEN_CTL_3", "RW/V", 1, 3, {"GEN_CTL_1.GEN_CTL_3"});
    GEN_CTL_3.set_powerwell("primary");
    GEN_CTL_3.set_rand_mode(0);
   GEN_CTL_3.set_reset_signame("pmu_rst_b");
    void'(add_field( GEN_CTL_3 ));

    GEN_CTL_4 = new("GEN_CTL_4", "RW/V", 1, 4, {"GEN_CTL_1.GEN_CTL_4"});
    GEN_CTL_4.set_powerwell("primary");
    GEN_CTL_4.set_rand_mode(0);
   GEN_CTL_4.set_reset_signame("pmu_rst_b");
    void'(add_field( GEN_CTL_4 ));

    GEN_CTL_5 = new("GEN_CTL_5", "RW/V", 1, 5, {"GEN_CTL_1.GEN_CTL_5"});
    GEN_CTL_5.set_powerwell("primary");
    GEN_CTL_5.set_rand_mode(0);
   GEN_CTL_5.set_reset_signame("pmu_rst_b");
    void'(add_field( GEN_CTL_5 ));

    GEN_CTL_6 = new("GEN_CTL_6", "RW/V", 1, 6, {"GEN_CTL_1.GEN_CTL_6"});
    GEN_CTL_6.set_powerwell("primary");
    GEN_CTL_6.set_rand_mode(0);
   GEN_CTL_6.set_reset_signame("pmu_rst_b");
    void'(add_field( GEN_CTL_6 ));

    GEN_CTL_7 = new("GEN_CTL_7", "RW/V", 1, 7, {"GEN_CTL_1.GEN_CTL_7"});
    GEN_CTL_7.set_powerwell("primary");
    GEN_CTL_7.set_rand_mode(0);
   GEN_CTL_7.set_reset_signame("pmu_rst_b");
    void'(add_field( GEN_CTL_7 ));

    GEN_CTL_8 = new("GEN_CTL_8", "RW/V", 1, 8, {"GEN_CTL_1.GEN_CTL_8"});
    GEN_CTL_8.set_powerwell("primary");
    GEN_CTL_8.set_rand_mode(0);
   GEN_CTL_8.set_reset_signame("pmu_rst_b");
    void'(add_field( GEN_CTL_8 ));

    GEN_CTL_9 = new("GEN_CTL_9", "RW/V", 1, 9, {"GEN_CTL_1.GEN_CTL_9"});
    GEN_CTL_9.set_powerwell("primary");
    GEN_CTL_9.set_rand_mode(0);
   GEN_CTL_9.set_reset_signame("pmu_rst_b");
    void'(add_field( GEN_CTL_9 ));

    GEN_CTL_10 = new("GEN_CTL_10", "RW/V", 1, 10, {"GEN_CTL_1.GEN_CTL_10"});
    GEN_CTL_10.set_powerwell("primary");
    GEN_CTL_10.set_rand_mode(0);
   GEN_CTL_10.set_reset_signame("pmu_rst_b");
    void'(add_field( GEN_CTL_10 ));

    GEN_CTL_11 = new("GEN_CTL_11", "RW/V", 1, 11, {"GEN_CTL_1.GEN_CTL_11"});
    GEN_CTL_11.set_powerwell("primary");
    GEN_CTL_11.set_rand_mode(0);
   GEN_CTL_11.set_reset_signame("pmu_rst_b");
    void'(add_field( GEN_CTL_11 ));

    GEN_CTL_12 = new("GEN_CTL_12", "RW/V", 1, 12, {"GEN_CTL_1.GEN_CTL_12"});
    GEN_CTL_12.set_powerwell("primary");
    GEN_CTL_12.set_rand_mode(0);
   GEN_CTL_12.set_reset_signame("pmu_rst_b");
    void'(add_field( GEN_CTL_12 ));

    GEN_CTL_13 = new("GEN_CTL_13", "RW/V", 1, 13, {"GEN_CTL_1.GEN_CTL_13"});
    GEN_CTL_13.set_powerwell("primary");
    GEN_CTL_13.set_rand_mode(0);
   GEN_CTL_13.set_reset_signame("pmu_rst_b");
    void'(add_field( GEN_CTL_13 ));

    GEN_CTL_14 = new("GEN_CTL_14", "RW/V", 1, 14, {"GEN_CTL_1.GEN_CTL_14"});
    GEN_CTL_14.set_powerwell("primary");
    GEN_CTL_14.set_rand_mode(0);
   GEN_CTL_14.set_reset_signame("pmu_rst_b");
    void'(add_field( GEN_CTL_14 ));

    GEN_CTL_15 = new("GEN_CTL_15", "RW/V", 1, 15, {"GEN_CTL_1.GEN_CTL_15"});
    GEN_CTL_15.set_powerwell("primary");
    GEN_CTL_15.set_rand_mode(0);
   GEN_CTL_15.set_reset_signame("pmu_rst_b");
    void'(add_field( GEN_CTL_15 ));

    GEN_CTL_16 = new("GEN_CTL_16", "RW/V", 1, 16, {"GEN_CTL_1.GEN_CTL_16"});
    GEN_CTL_16.set_powerwell("primary");
    GEN_CTL_16.set_rand_mode(0);
   GEN_CTL_16.set_reset_signame("pmu_rst_b");
    void'(add_field( GEN_CTL_16 ));

    GEN_CTL_17 = new("GEN_CTL_17", "RW/V", 1, 17, {"GEN_CTL_1.GEN_CTL_17"});
    GEN_CTL_17.set_powerwell("primary");
    GEN_CTL_17.set_rand_mode(0);
   GEN_CTL_17.set_reset_signame("pmu_rst_b");
    void'(add_field( GEN_CTL_17 ));

    GEN_CTL_18 = new("GEN_CTL_18", "RW/V", 1, 18, {"GEN_CTL_1.GEN_CTL_18"});
    GEN_CTL_18.set_powerwell("primary");
    GEN_CTL_18.set_rand_mode(0);
   GEN_CTL_18.set_reset_signame("pmu_rst_b");
    void'(add_field( GEN_CTL_18 ));

    GEN_CTL_19 = new("GEN_CTL_19", "RW/V", 1, 19, {"GEN_CTL_1.GEN_CTL_19"});
    GEN_CTL_19.set_powerwell("primary");
    GEN_CTL_19.set_rand_mode(0);
   GEN_CTL_19.set_reset_signame("pmu_rst_b");
    void'(add_field( GEN_CTL_19 ));

    GEN_CTL_20 = new("GEN_CTL_20", "RW/V", 1, 20, {"GEN_CTL_1.GEN_CTL_20"});
    GEN_CTL_20.set_powerwell("primary");
    GEN_CTL_20.set_rand_mode(0);
   GEN_CTL_20.set_reset_signame("pmu_rst_b");
    void'(add_field( GEN_CTL_20 ));

    GEN_CTL_21 = new("GEN_CTL_21", "RW/V", 1, 21, {"GEN_CTL_1.GEN_CTL_21"});
    GEN_CTL_21.set_powerwell("primary");
    GEN_CTL_21.set_rand_mode(0);
   GEN_CTL_21.set_reset_signame("pmu_rst_b");
    void'(add_field( GEN_CTL_21 ));

    GEN_CTL_22 = new("GEN_CTL_22", "RW/V", 1, 22, {"GEN_CTL_1.GEN_CTL_22"});
    GEN_CTL_22.set_powerwell("primary");
    GEN_CTL_22.set_rand_mode(0);
   GEN_CTL_22.set_reset_signame("pmu_rst_b");
    void'(add_field( GEN_CTL_22 ));

    GEN_CTL_23 = new("GEN_CTL_23", "RW/V", 1, 23, {"GEN_CTL_1.GEN_CTL_23"});
    GEN_CTL_23.set_powerwell("primary");
    GEN_CTL_23.set_rand_mode(0);
   GEN_CTL_23.set_reset_signame("pmu_rst_b");
    void'(add_field( GEN_CTL_23 ));

    GEN_CTL_24 = new("GEN_CTL_24", "RW/V", 1, 24, {"GEN_CTL_1.GEN_CTL_24"});
    GEN_CTL_24.set_powerwell("primary");
    GEN_CTL_24.set_rand_mode(0);
   GEN_CTL_24.set_reset_signame("pmu_rst_b");
    void'(add_field( GEN_CTL_24 ));

    GEN_CTL_25 = new("GEN_CTL_25", "RW/V", 1, 25, {"GEN_CTL_1.GEN_CTL_25"});
    GEN_CTL_25.set_powerwell("primary");
    GEN_CTL_25.set_rand_mode(0);
   GEN_CTL_25.set_reset_signame("pmu_rst_b");
    void'(add_field( GEN_CTL_25 ));

    GEN_CTL_26 = new("GEN_CTL_26", "RW/V", 1, 26, {"GEN_CTL_1.GEN_CTL_26"});
    GEN_CTL_26.set_powerwell("primary");
    GEN_CTL_26.set_rand_mode(0);
   GEN_CTL_26.set_reset_signame("pmu_rst_b");
    void'(add_field( GEN_CTL_26 ));

    GEN_CTL_27 = new("GEN_CTL_27", "RW/V", 1, 27, {"GEN_CTL_1.GEN_CTL_27"});
    GEN_CTL_27.set_powerwell("primary");
    GEN_CTL_27.set_rand_mode(0);
   GEN_CTL_27.set_reset_signame("pmu_rst_b");
    void'(add_field( GEN_CTL_27 ));

    GEN_CTL_28 = new("GEN_CTL_28", "RW/V", 1, 28, {"GEN_CTL_1.GEN_CTL_28"});
    GEN_CTL_28.set_powerwell("primary");
    GEN_CTL_28.set_rand_mode(0);
   GEN_CTL_28.set_reset_signame("pmu_rst_b");
    void'(add_field( GEN_CTL_28 ));

    GEN_CTL_29 = new("GEN_CTL_29", "RW/V", 1, 29, {"GEN_CTL_1.GEN_CTL_29"});
    GEN_CTL_29.set_powerwell("primary");
    GEN_CTL_29.set_rand_mode(0);
   GEN_CTL_29.set_reset_signame("pmu_rst_b");
    void'(add_field( GEN_CTL_29 ));

    GEN_CTL_30 = new("GEN_CTL_30", "RW/V", 1, 30, {"GEN_CTL_1.GEN_CTL_30"});
    GEN_CTL_30.set_powerwell("primary");
    GEN_CTL_30.set_rand_mode(0);
   GEN_CTL_30.set_reset_signame("pmu_rst_b");
    void'(add_field( GEN_CTL_30 ));

    GEN_CTL_31 = new("GEN_CTL_31", "RW/V", 1, 31, {"GEN_CTL_1.GEN_CTL_31"});
    GEN_CTL_31.set_powerwell("primary");
    GEN_CTL_31.set_rand_mode(0);
   GEN_CTL_31.set_reset_signame("pmu_rst_b");
    void'(add_field( GEN_CTL_31 ));

  endfunction


`DEFINE_NONE_SCOPE_REG_API(pmu_mmr_GEN_CTL_1_reg) 
endclass : pmu_mmr_GEN_CTL_1_reg

// ================================================

class pmu_mmr_GEN_STS_0_reg extends sla_ral_reg;

  // --------------------------
  sla_ral_field PHY_PMU_GLBL_STABLE;
  sla_ral_field GEN_STS;

  // --------------------------
  `ovm_object_utils(pmu_mmr_GEN_STS_0_reg)

  // --------------------------
  function new(
                string         name = "",
                `ifdef SLA_RAL_COVERAGE
                sla_ral_coverage_t cov_t = COVERAGE_OFF,
                `endif
                int            bus_num=0,
                int            dev_num=0,
                int            func_num=0,
                int            offset=0,
                int            size=0,
                sla_ral_data_t reset_val=0,
                boolean_t      mon_enabled = SLA_FALSE
             );
      super.new();
      set_space_addr("CFG",undef);
      set_space_addr("MEM",undef);
      set_space_addr("IO",undef);
      set_space_addr("MSG",undef);
      set_space_addr("CREG",undef); 
      set_space_addr("LT_MEM",undef);    
 build();

      `ifdef SLA_RAL_COVERAGE
      `CONFIG_RAL_COVERAGE(regname, COVERAGE_ON, FD_RW_SAMPLE)
      if(_coverage_type != COVERAGE_OFF) begin
        if(_sample_type inside {FD_RO_SAMPLE, RO_SAMPLE})
        `CREATE_OP_SAMPLE_COV(RAL_WRITE)
        else if(_sample_type inside {FD_WO_SAMPLE, WO_SAMPLE})
        `CREATE_OP_SAMPLE_COV(RAL_READ)
        else
        `CREATE_OP_SAMPLE_COV(RAL_NONE)
        `CREATE_DESIRED_COV
        `CREATE_ACTUAL_COV
      end
      `endif

  endfunction

  `ifdef SLA_RAL_COVERAGE
  `CREATE_RAL_OP_COVERGROUP
  covergroup desired_cg @(cov_ev);
     option.per_instance = 1;
     option.name = {sla_ral_file::regfilename,".",regname};
     `RAL_FIELD_CP(PHY_PMU_GLBL_STABLE, PHY_PMU_GLBL_STABLE.desired)
     `RAL_FIELD_CP_4(PHY_PMU_GLBL_STABLE, PHY_PMU_GLBL_STABLE.desired, 0,1,2,3)
     `RAL_FIELD_CP(GEN_STS, GEN_STS.desired)
     `RAL_FIELD_CP_16(GEN_STS, GEN_STS.desired, 0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)
     `RAL_FIELD_CP_12(GEN_STS, GEN_STS.desired, 16,17,18,19,20,21,22,23,24,25,26,27)
  endgroup
  covergroup actual_cg @(cov_ev);
     option.per_instance = 1;
     option.name = {sla_ral_file::regfilename,".",regname};
     `RAL_FIELD_CP(PHY_PMU_GLBL_STABLE, PHY_PMU_GLBL_STABLE.actual)
     `RAL_FIELD_CP_4(PHY_PMU_GLBL_STABLE, PHY_PMU_GLBL_STABLE.actual, 0,1,2,3)
     `RAL_FIELD_CP(GEN_STS, GEN_STS.actual)
     `RAL_FIELD_CP_16(GEN_STS, GEN_STS.actual, 0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)
     `RAL_FIELD_CP_12(GEN_STS, GEN_STS.actual, 16,17,18,19,20,21,22,23,24,25,26,27)
  endgroup
  `endif

  // --------------------------
  // add constraint based on database legal values
  // --------------------------
  function void build();
    PHY_PMU_GLBL_STABLE = new("PHY_PMU_GLBL_STABLE", "RO/V", 4, 0, {"GEN_STS_0.PHY_PMU_GLBL_STABLE"});
    PHY_PMU_GLBL_STABLE.set_powerwell("primary");
    PHY_PMU_GLBL_STABLE.set_rand_mode(0);
    void'(add_field( PHY_PMU_GLBL_STABLE ));

    GEN_STS = new("GEN_STS", "RO/V", 28, 4, {"GEN_STS_0.GEN_STS"});
    GEN_STS.set_powerwell("primary");
    GEN_STS.set_rand_mode(0);
    void'(add_field( GEN_STS ));

  endfunction


`DEFINE_NONE_SCOPE_REG_API(pmu_mmr_GEN_STS_0_reg) 
endclass : pmu_mmr_GEN_STS_0_reg

// ================================================

class pmu_mmr_GEN_STS_1_reg extends sla_ral_reg;

  // --------------------------
  sla_ral_field GEN_STS;

  // --------------------------
  `ovm_object_utils(pmu_mmr_GEN_STS_1_reg)

  // --------------------------
  function new(
                string         name = "",
                `ifdef SLA_RAL_COVERAGE
                sla_ral_coverage_t cov_t = COVERAGE_OFF,
                `endif
                int            bus_num=0,
                int            dev_num=0,
                int            func_num=0,
                int            offset=0,
                int            size=0,
                sla_ral_data_t reset_val=0,
                boolean_t      mon_enabled = SLA_FALSE
             );
      super.new();
      set_space_addr("CFG",undef);
      set_space_addr("MEM",undef);
      set_space_addr("IO",undef);
      set_space_addr("MSG",undef);
      set_space_addr("CREG",undef); 
      set_space_addr("LT_MEM",undef);    
 build();

      `ifdef SLA_RAL_COVERAGE
      `CONFIG_RAL_COVERAGE(regname, COVERAGE_ON, FD_RW_SAMPLE)
      if(_coverage_type != COVERAGE_OFF) begin
        if(_sample_type inside {FD_RO_SAMPLE, RO_SAMPLE})
        `CREATE_OP_SAMPLE_COV(RAL_WRITE)
        else if(_sample_type inside {FD_WO_SAMPLE, WO_SAMPLE})
        `CREATE_OP_SAMPLE_COV(RAL_READ)
        else
        `CREATE_OP_SAMPLE_COV(RAL_NONE)
        `CREATE_DESIRED_COV
        `CREATE_ACTUAL_COV
      end
      `endif

  endfunction

  `ifdef SLA_RAL_COVERAGE
  `CREATE_RAL_OP_COVERGROUP
  covergroup desired_cg @(cov_ev);
     option.per_instance = 1;
     option.name = {sla_ral_file::regfilename,".",regname};
     `RAL_FIELD_CP(GEN_STS, GEN_STS.desired)
     `RAL_FIELD_CP_16(GEN_STS, GEN_STS.desired, 0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)
     `RAL_FIELD_CP_16(GEN_STS, GEN_STS.desired, 16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31)
  endgroup
  covergroup actual_cg @(cov_ev);
     option.per_instance = 1;
     option.name = {sla_ral_file::regfilename,".",regname};
     `RAL_FIELD_CP(GEN_STS, GEN_STS.actual)
     `RAL_FIELD_CP_16(GEN_STS, GEN_STS.actual, 0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)
     `RAL_FIELD_CP_16(GEN_STS, GEN_STS.actual, 16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31)
  endgroup
  `endif

  // --------------------------
  // add constraint based on database legal values
  // --------------------------
  function void build();
    GEN_STS = new("GEN_STS", "RO/V", 32, 0, {"GEN_STS_1.GEN_STS"});
    GEN_STS.set_powerwell("primary");
    GEN_STS.set_rand_mode(0);
    void'(add_field( GEN_STS ));

  endfunction


`DEFINE_NONE_SCOPE_REG_API(pmu_mmr_GEN_STS_1_reg) 
endclass : pmu_mmr_GEN_STS_1_reg

// ================================================

class pmu_mmr_PMU_FUSE_0_reg extends sla_ral_reg;

  // --------------------------
  sla_ral_field FUSE_DW0;

  // --------------------------
  `ovm_object_utils(pmu_mmr_PMU_FUSE_0_reg)

  // --------------------------
  function new(
                string         name = "",
                `ifdef SLA_RAL_COVERAGE
                sla_ral_coverage_t cov_t = COVERAGE_OFF,
                `endif
                int            bus_num=0,
                int            dev_num=0,
                int            func_num=0,
                int            offset=0,
                int            size=0,
                sla_ral_data_t reset_val=0,
                boolean_t      mon_enabled = SLA_FALSE
             );
      super.new();
      set_space_addr("CFG",undef);
      set_space_addr("MEM",undef);
      set_space_addr("IO",undef);
      set_space_addr("MSG",undef);
      set_space_addr("CREG",undef); 
      set_space_addr("LT_MEM",undef);    
 build();

      `ifdef SLA_RAL_COVERAGE
      `CONFIG_RAL_COVERAGE(regname, COVERAGE_ON, FD_RW_SAMPLE)
      if(_coverage_type != COVERAGE_OFF) begin
        if(_sample_type inside {FD_RO_SAMPLE, RO_SAMPLE})
        `CREATE_OP_SAMPLE_COV(RAL_WRITE)
        else if(_sample_type inside {FD_WO_SAMPLE, WO_SAMPLE})
        `CREATE_OP_SAMPLE_COV(RAL_READ)
        else
        `CREATE_OP_SAMPLE_COV(RAL_NONE)
        `CREATE_DESIRED_COV
        `CREATE_ACTUAL_COV
      end
      `endif

  endfunction

  `ifdef SLA_RAL_COVERAGE
  `CREATE_RAL_OP_COVERGROUP
  covergroup desired_cg @(cov_ev);
     option.per_instance = 1;
     option.name = {sla_ral_file::regfilename,".",regname};
     `RAL_FIELD_CP(FUSE_DW0, FUSE_DW0.desired)
     `RAL_FIELD_CP_16(FUSE_DW0, FUSE_DW0.desired, 0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)
     `RAL_FIELD_CP_16(FUSE_DW0, FUSE_DW0.desired, 16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31)
  endgroup
  covergroup actual_cg @(cov_ev);
     option.per_instance = 1;
     option.name = {sla_ral_file::regfilename,".",regname};
     `RAL_FIELD_CP(FUSE_DW0, FUSE_DW0.actual)
     `RAL_FIELD_CP_16(FUSE_DW0, FUSE_DW0.actual, 0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)
     `RAL_FIELD_CP_16(FUSE_DW0, FUSE_DW0.actual, 16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31)
  endgroup
  `endif

  // --------------------------
  // add constraint based on database legal values
  // --------------------------
  function void build();
    FUSE_DW0 = new("FUSE_DW0", "RO/V", 32, 0, {"PMU_FUSE_0.FUSE_DW0"});
    FUSE_DW0.set_powerwell("primary");
    FUSE_DW0.set_rand_mode(0);
    void'(add_field( FUSE_DW0 ));

  endfunction


`DEFINE_NONE_SCOPE_REG_API(pmu_mmr_PMU_FUSE_0_reg) 
endclass : pmu_mmr_PMU_FUSE_0_reg

// ================================================

class pmu_mmr_PMU_FUSE_1_reg extends sla_ral_reg;

  // --------------------------
  sla_ral_field FUSE_DW1;

  // --------------------------
  `ovm_object_utils(pmu_mmr_PMU_FUSE_1_reg)

  // --------------------------
  function new(
                string         name = "",
                `ifdef SLA_RAL_COVERAGE
                sla_ral_coverage_t cov_t = COVERAGE_OFF,
                `endif
                int            bus_num=0,
                int            dev_num=0,
                int            func_num=0,
                int            offset=0,
                int            size=0,
                sla_ral_data_t reset_val=0,
                boolean_t      mon_enabled = SLA_FALSE
             );
      super.new();
      set_space_addr("CFG",undef);
      set_space_addr("MEM",undef);
      set_space_addr("IO",undef);
      set_space_addr("MSG",undef);
      set_space_addr("CREG",undef); 
      set_space_addr("LT_MEM",undef);    
 build();

      `ifdef SLA_RAL_COVERAGE
      `CONFIG_RAL_COVERAGE(regname, COVERAGE_ON, FD_RW_SAMPLE)
      if(_coverage_type != COVERAGE_OFF) begin
        if(_sample_type inside {FD_RO_SAMPLE, RO_SAMPLE})
        `CREATE_OP_SAMPLE_COV(RAL_WRITE)
        else if(_sample_type inside {FD_WO_SAMPLE, WO_SAMPLE})
        `CREATE_OP_SAMPLE_COV(RAL_READ)
        else
        `CREATE_OP_SAMPLE_COV(RAL_NONE)
        `CREATE_DESIRED_COV
        `CREATE_ACTUAL_COV
      end
      `endif

  endfunction

  `ifdef SLA_RAL_COVERAGE
  `CREATE_RAL_OP_COVERGROUP
  covergroup desired_cg @(cov_ev);
     option.per_instance = 1;
     option.name = {sla_ral_file::regfilename,".",regname};
     `RAL_FIELD_CP(FUSE_DW1, FUSE_DW1.desired)
     `RAL_FIELD_CP_16(FUSE_DW1, FUSE_DW1.desired, 0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)
     `RAL_FIELD_CP_16(FUSE_DW1, FUSE_DW1.desired, 16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31)
  endgroup
  covergroup actual_cg @(cov_ev);
     option.per_instance = 1;
     option.name = {sla_ral_file::regfilename,".",regname};
     `RAL_FIELD_CP(FUSE_DW1, FUSE_DW1.actual)
     `RAL_FIELD_CP_16(FUSE_DW1, FUSE_DW1.actual, 0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)
     `RAL_FIELD_CP_16(FUSE_DW1, FUSE_DW1.actual, 16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31)
  endgroup
  `endif

  // --------------------------
  // add constraint based on database legal values
  // --------------------------
  function void build();
    FUSE_DW1 = new("FUSE_DW1", "RO/V", 32, 0, {"PMU_FUSE_1.FUSE_DW1"});
    FUSE_DW1.set_powerwell("primary");
    FUSE_DW1.set_rand_mode(0);
    void'(add_field( FUSE_DW1 ));

  endfunction


`DEFINE_NONE_SCOPE_REG_API(pmu_mmr_PMU_FUSE_1_reg) 
endclass : pmu_mmr_PMU_FUSE_1_reg

// ================================================

class pmu_mmr_PMU_FUSE_2_reg extends sla_ral_reg;

  // --------------------------
  sla_ral_field FUSE_DW2;

  // --------------------------
  `ovm_object_utils(pmu_mmr_PMU_FUSE_2_reg)

  // --------------------------
  function new(
                string         name = "",
                `ifdef SLA_RAL_COVERAGE
                sla_ral_coverage_t cov_t = COVERAGE_OFF,
                `endif
                int            bus_num=0,
                int            dev_num=0,
                int            func_num=0,
                int            offset=0,
                int            size=0,
                sla_ral_data_t reset_val=0,
                boolean_t      mon_enabled = SLA_FALSE
             );
      super.new();
      set_space_addr("CFG",undef);
      set_space_addr("MEM",undef);
      set_space_addr("IO",undef);
      set_space_addr("MSG",undef);
      set_space_addr("CREG",undef); 
      set_space_addr("LT_MEM",undef);    
 build();

      `ifdef SLA_RAL_COVERAGE
      `CONFIG_RAL_COVERAGE(regname, COVERAGE_ON, FD_RW_SAMPLE)
      if(_coverage_type != COVERAGE_OFF) begin
        if(_sample_type inside {FD_RO_SAMPLE, RO_SAMPLE})
        `CREATE_OP_SAMPLE_COV(RAL_WRITE)
        else if(_sample_type inside {FD_WO_SAMPLE, WO_SAMPLE})
        `CREATE_OP_SAMPLE_COV(RAL_READ)
        else
        `CREATE_OP_SAMPLE_COV(RAL_NONE)
        `CREATE_DESIRED_COV
        `CREATE_ACTUAL_COV
      end
      `endif

  endfunction

  `ifdef SLA_RAL_COVERAGE
  `CREATE_RAL_OP_COVERGROUP
  covergroup desired_cg @(cov_ev);
     option.per_instance = 1;
     option.name = {sla_ral_file::regfilename,".",regname};
     `RAL_FIELD_CP(FUSE_DW2, FUSE_DW2.desired)
     `RAL_FIELD_CP_16(FUSE_DW2, FUSE_DW2.desired, 0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)
     `RAL_FIELD_CP_16(FUSE_DW2, FUSE_DW2.desired, 16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31)
  endgroup
  covergroup actual_cg @(cov_ev);
     option.per_instance = 1;
     option.name = {sla_ral_file::regfilename,".",regname};
     `RAL_FIELD_CP(FUSE_DW2, FUSE_DW2.actual)
     `RAL_FIELD_CP_16(FUSE_DW2, FUSE_DW2.actual, 0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)
     `RAL_FIELD_CP_16(FUSE_DW2, FUSE_DW2.actual, 16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31)
  endgroup
  `endif

  // --------------------------
  // add constraint based on database legal values
  // --------------------------
  function void build();
    FUSE_DW2 = new("FUSE_DW2", "RO/V", 32, 0, {"PMU_FUSE_2.FUSE_DW2"});
    FUSE_DW2.set_powerwell("primary");
    FUSE_DW2.set_rand_mode(0);
    void'(add_field( FUSE_DW2 ));

  endfunction


`DEFINE_NONE_SCOPE_REG_API(pmu_mmr_PMU_FUSE_2_reg) 
endclass : pmu_mmr_PMU_FUSE_2_reg

// ================================================

class pmu_mmr_PMU_FUSE_3_reg extends sla_ral_reg;

  // --------------------------
  sla_ral_field FUSE_DW3;

  // --------------------------
  `ovm_object_utils(pmu_mmr_PMU_FUSE_3_reg)

  // --------------------------
  function new(
                string         name = "",
                `ifdef SLA_RAL_COVERAGE
                sla_ral_coverage_t cov_t = COVERAGE_OFF,
                `endif
                int            bus_num=0,
                int            dev_num=0,
                int            func_num=0,
                int            offset=0,
                int            size=0,
                sla_ral_data_t reset_val=0,
                boolean_t      mon_enabled = SLA_FALSE
             );
      super.new();
      set_space_addr("CFG",undef);
      set_space_addr("MEM",undef);
      set_space_addr("IO",undef);
      set_space_addr("MSG",undef);
      set_space_addr("CREG",undef); 
      set_space_addr("LT_MEM",undef);    
 build();

      `ifdef SLA_RAL_COVERAGE
      `CONFIG_RAL_COVERAGE(regname, COVERAGE_ON, FD_RW_SAMPLE)
      if(_coverage_type != COVERAGE_OFF) begin
        if(_sample_type inside {FD_RO_SAMPLE, RO_SAMPLE})
        `CREATE_OP_SAMPLE_COV(RAL_WRITE)
        else if(_sample_type inside {FD_WO_SAMPLE, WO_SAMPLE})
        `CREATE_OP_SAMPLE_COV(RAL_READ)
        else
        `CREATE_OP_SAMPLE_COV(RAL_NONE)
        `CREATE_DESIRED_COV
        `CREATE_ACTUAL_COV
      end
      `endif

  endfunction

  `ifdef SLA_RAL_COVERAGE
  `CREATE_RAL_OP_COVERGROUP
  covergroup desired_cg @(cov_ev);
     option.per_instance = 1;
     option.name = {sla_ral_file::regfilename,".",regname};
     `RAL_FIELD_CP(FUSE_DW3, FUSE_DW3.desired)
     `RAL_FIELD_CP_16(FUSE_DW3, FUSE_DW3.desired, 0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)
     `RAL_FIELD_CP_16(FUSE_DW3, FUSE_DW3.desired, 16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31)
  endgroup
  covergroup actual_cg @(cov_ev);
     option.per_instance = 1;
     option.name = {sla_ral_file::regfilename,".",regname};
     `RAL_FIELD_CP(FUSE_DW3, FUSE_DW3.actual)
     `RAL_FIELD_CP_16(FUSE_DW3, FUSE_DW3.actual, 0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)
     `RAL_FIELD_CP_16(FUSE_DW3, FUSE_DW3.actual, 16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31)
  endgroup
  `endif

  // --------------------------
  // add constraint based on database legal values
  // --------------------------
  function void build();
    FUSE_DW3 = new("FUSE_DW3", "RO/V", 32, 0, {"PMU_FUSE_3.FUSE_DW3"});
    FUSE_DW3.set_powerwell("primary");
    FUSE_DW3.set_rand_mode(0);
    void'(add_field( FUSE_DW3 ));

  endfunction


`DEFINE_NONE_SCOPE_REG_API(pmu_mmr_PMU_FUSE_3_reg) 
endclass : pmu_mmr_PMU_FUSE_3_reg

// ================================================

class pmu_mmr_PMU_FUSE_4_reg extends sla_ral_reg;

  // --------------------------
  sla_ral_field FUSE_DW4;

  // --------------------------
  `ovm_object_utils(pmu_mmr_PMU_FUSE_4_reg)

  // --------------------------
  function new(
                string         name = "",
                `ifdef SLA_RAL_COVERAGE
                sla_ral_coverage_t cov_t = COVERAGE_OFF,
                `endif
                int            bus_num=0,
                int            dev_num=0,
                int            func_num=0,
                int            offset=0,
                int            size=0,
                sla_ral_data_t reset_val=0,
                boolean_t      mon_enabled = SLA_FALSE
             );
      super.new();
      set_space_addr("CFG",undef);
      set_space_addr("MEM",undef);
      set_space_addr("IO",undef);
      set_space_addr("MSG",undef);
      set_space_addr("CREG",undef); 
      set_space_addr("LT_MEM",undef);    
 build();

      `ifdef SLA_RAL_COVERAGE
      `CONFIG_RAL_COVERAGE(regname, COVERAGE_ON, FD_RW_SAMPLE)
      if(_coverage_type != COVERAGE_OFF) begin
        if(_sample_type inside {FD_RO_SAMPLE, RO_SAMPLE})
        `CREATE_OP_SAMPLE_COV(RAL_WRITE)
        else if(_sample_type inside {FD_WO_SAMPLE, WO_SAMPLE})
        `CREATE_OP_SAMPLE_COV(RAL_READ)
        else
        `CREATE_OP_SAMPLE_COV(RAL_NONE)
        `CREATE_DESIRED_COV
        `CREATE_ACTUAL_COV
      end
      `endif

  endfunction

  `ifdef SLA_RAL_COVERAGE
  `CREATE_RAL_OP_COVERGROUP
  covergroup desired_cg @(cov_ev);
     option.per_instance = 1;
     option.name = {sla_ral_file::regfilename,".",regname};
     `RAL_FIELD_CP(FUSE_DW4, FUSE_DW4.desired)
     `RAL_FIELD_CP_16(FUSE_DW4, FUSE_DW4.desired, 0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)
     `RAL_FIELD_CP_16(FUSE_DW4, FUSE_DW4.desired, 16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31)
  endgroup
  covergroup actual_cg @(cov_ev);
     option.per_instance = 1;
     option.name = {sla_ral_file::regfilename,".",regname};
     `RAL_FIELD_CP(FUSE_DW4, FUSE_DW4.actual)
     `RAL_FIELD_CP_16(FUSE_DW4, FUSE_DW4.actual, 0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)
     `RAL_FIELD_CP_16(FUSE_DW4, FUSE_DW4.actual, 16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31)
  endgroup
  `endif

  // --------------------------
  // add constraint based on database legal values
  // --------------------------
  function void build();
    FUSE_DW4 = new("FUSE_DW4", "RO/V", 32, 0, {"PMU_FUSE_4.FUSE_DW4"});
    FUSE_DW4.set_powerwell("primary");
    FUSE_DW4.set_rand_mode(0);
    void'(add_field( FUSE_DW4 ));

  endfunction


`DEFINE_NONE_SCOPE_REG_API(pmu_mmr_PMU_FUSE_4_reg) 
endclass : pmu_mmr_PMU_FUSE_4_reg

// ================================================

class pmu_mmr_PMU_FUSE_5_reg extends sla_ral_reg;

  // --------------------------
  sla_ral_field FUSE_DW5;

  // --------------------------
  `ovm_object_utils(pmu_mmr_PMU_FUSE_5_reg)

  // --------------------------
  function new(
                string         name = "",
                `ifdef SLA_RAL_COVERAGE
                sla_ral_coverage_t cov_t = COVERAGE_OFF,
                `endif
                int            bus_num=0,
                int            dev_num=0,
                int            func_num=0,
                int            offset=0,
                int            size=0,
                sla_ral_data_t reset_val=0,
                boolean_t      mon_enabled = SLA_FALSE
             );
      super.new();
      set_space_addr("CFG",undef);
      set_space_addr("MEM",undef);
      set_space_addr("IO",undef);
      set_space_addr("MSG",undef);
      set_space_addr("CREG",undef); 
      set_space_addr("LT_MEM",undef);    
 build();

      `ifdef SLA_RAL_COVERAGE
      `CONFIG_RAL_COVERAGE(regname, COVERAGE_ON, FD_RW_SAMPLE)
      if(_coverage_type != COVERAGE_OFF) begin
        if(_sample_type inside {FD_RO_SAMPLE, RO_SAMPLE})
        `CREATE_OP_SAMPLE_COV(RAL_WRITE)
        else if(_sample_type inside {FD_WO_SAMPLE, WO_SAMPLE})
        `CREATE_OP_SAMPLE_COV(RAL_READ)
        else
        `CREATE_OP_SAMPLE_COV(RAL_NONE)
        `CREATE_DESIRED_COV
        `CREATE_ACTUAL_COV
      end
      `endif

  endfunction

  `ifdef SLA_RAL_COVERAGE
  `CREATE_RAL_OP_COVERGROUP
  covergroup desired_cg @(cov_ev);
     option.per_instance = 1;
     option.name = {sla_ral_file::regfilename,".",regname};
     `RAL_FIELD_CP(FUSE_DW5, FUSE_DW5.desired)
     `RAL_FIELD_CP_16(FUSE_DW5, FUSE_DW5.desired, 0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)
     `RAL_FIELD_CP_16(FUSE_DW5, FUSE_DW5.desired, 16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31)
  endgroup
  covergroup actual_cg @(cov_ev);
     option.per_instance = 1;
     option.name = {sla_ral_file::regfilename,".",regname};
     `RAL_FIELD_CP(FUSE_DW5, FUSE_DW5.actual)
     `RAL_FIELD_CP_16(FUSE_DW5, FUSE_DW5.actual, 0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)
     `RAL_FIELD_CP_16(FUSE_DW5, FUSE_DW5.actual, 16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31)
  endgroup
  `endif

  // --------------------------
  // add constraint based on database legal values
  // --------------------------
  function void build();
    FUSE_DW5 = new("FUSE_DW5", "RO/V", 32, 0, {"PMU_FUSE_5.FUSE_DW5"});
    FUSE_DW5.set_powerwell("primary");
    FUSE_DW5.set_rand_mode(0);
    void'(add_field( FUSE_DW5 ));

  endfunction


`DEFINE_NONE_SCOPE_REG_API(pmu_mmr_PMU_FUSE_5_reg) 
endclass : pmu_mmr_PMU_FUSE_5_reg

// ================================================

class pmu_mmr_PMU_FUSE_6_reg extends sla_ral_reg;

  // --------------------------
  sla_ral_field FUSE_DW6;

  // --------------------------
  `ovm_object_utils(pmu_mmr_PMU_FUSE_6_reg)

  // --------------------------
  function new(
                string         name = "",
                `ifdef SLA_RAL_COVERAGE
                sla_ral_coverage_t cov_t = COVERAGE_OFF,
                `endif
                int            bus_num=0,
                int            dev_num=0,
                int            func_num=0,
                int            offset=0,
                int            size=0,
                sla_ral_data_t reset_val=0,
                boolean_t      mon_enabled = SLA_FALSE
             );
      super.new();
      set_space_addr("CFG",undef);
      set_space_addr("MEM",undef);
      set_space_addr("IO",undef);
      set_space_addr("MSG",undef);
      set_space_addr("CREG",undef); 
      set_space_addr("LT_MEM",undef);    
 build();

      `ifdef SLA_RAL_COVERAGE
      `CONFIG_RAL_COVERAGE(regname, COVERAGE_ON, FD_RW_SAMPLE)
      if(_coverage_type != COVERAGE_OFF) begin
        if(_sample_type inside {FD_RO_SAMPLE, RO_SAMPLE})
        `CREATE_OP_SAMPLE_COV(RAL_WRITE)
        else if(_sample_type inside {FD_WO_SAMPLE, WO_SAMPLE})
        `CREATE_OP_SAMPLE_COV(RAL_READ)
        else
        `CREATE_OP_SAMPLE_COV(RAL_NONE)
        `CREATE_DESIRED_COV
        `CREATE_ACTUAL_COV
      end
      `endif

  endfunction

  `ifdef SLA_RAL_COVERAGE
  `CREATE_RAL_OP_COVERGROUP
  covergroup desired_cg @(cov_ev);
     option.per_instance = 1;
     option.name = {sla_ral_file::regfilename,".",regname};
     `RAL_FIELD_CP(FUSE_DW6, FUSE_DW6.desired)
     `RAL_FIELD_CP_16(FUSE_DW6, FUSE_DW6.desired, 0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)
     `RAL_FIELD_CP_16(FUSE_DW6, FUSE_DW6.desired, 16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31)
  endgroup
  covergroup actual_cg @(cov_ev);
     option.per_instance = 1;
     option.name = {sla_ral_file::regfilename,".",regname};
     `RAL_FIELD_CP(FUSE_DW6, FUSE_DW6.actual)
     `RAL_FIELD_CP_16(FUSE_DW6, FUSE_DW6.actual, 0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)
     `RAL_FIELD_CP_16(FUSE_DW6, FUSE_DW6.actual, 16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31)
  endgroup
  `endif

  // --------------------------
  // add constraint based on database legal values
  // --------------------------
  function void build();
    FUSE_DW6 = new("FUSE_DW6", "RO/V", 32, 0, {"PMU_FUSE_6.FUSE_DW6"});
    FUSE_DW6.set_powerwell("primary");
    FUSE_DW6.set_rand_mode(0);
    void'(add_field( FUSE_DW6 ));

  endfunction


`DEFINE_NONE_SCOPE_REG_API(pmu_mmr_PMU_FUSE_6_reg) 
endclass : pmu_mmr_PMU_FUSE_6_reg

// ================================================

class pmu_mmr_PMU_FUSE_7_reg extends sla_ral_reg;

  // --------------------------
  sla_ral_field FUSE_DW7;

  // --------------------------
  `ovm_object_utils(pmu_mmr_PMU_FUSE_7_reg)

  // --------------------------
  function new(
                string         name = "",
                `ifdef SLA_RAL_COVERAGE
                sla_ral_coverage_t cov_t = COVERAGE_OFF,
                `endif
                int            bus_num=0,
                int            dev_num=0,
                int            func_num=0,
                int            offset=0,
                int            size=0,
                sla_ral_data_t reset_val=0,
                boolean_t      mon_enabled = SLA_FALSE
             );
      super.new();
      set_space_addr("CFG",undef);
      set_space_addr("MEM",undef);
      set_space_addr("IO",undef);
      set_space_addr("MSG",undef);
      set_space_addr("CREG",undef); 
      set_space_addr("LT_MEM",undef);    
 build();

      `ifdef SLA_RAL_COVERAGE
      `CONFIG_RAL_COVERAGE(regname, COVERAGE_ON, FD_RW_SAMPLE)
      if(_coverage_type != COVERAGE_OFF) begin
        if(_sample_type inside {FD_RO_SAMPLE, RO_SAMPLE})
        `CREATE_OP_SAMPLE_COV(RAL_WRITE)
        else if(_sample_type inside {FD_WO_SAMPLE, WO_SAMPLE})
        `CREATE_OP_SAMPLE_COV(RAL_READ)
        else
        `CREATE_OP_SAMPLE_COV(RAL_NONE)
        `CREATE_DESIRED_COV
        `CREATE_ACTUAL_COV
      end
      `endif

  endfunction

  `ifdef SLA_RAL_COVERAGE
  `CREATE_RAL_OP_COVERGROUP
  covergroup desired_cg @(cov_ev);
     option.per_instance = 1;
     option.name = {sla_ral_file::regfilename,".",regname};
     `RAL_FIELD_CP(FUSE_DW7, FUSE_DW7.desired)
     `RAL_FIELD_CP_16(FUSE_DW7, FUSE_DW7.desired, 0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)
     `RAL_FIELD_CP_16(FUSE_DW7, FUSE_DW7.desired, 16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31)
  endgroup
  covergroup actual_cg @(cov_ev);
     option.per_instance = 1;
     option.name = {sla_ral_file::regfilename,".",regname};
     `RAL_FIELD_CP(FUSE_DW7, FUSE_DW7.actual)
     `RAL_FIELD_CP_16(FUSE_DW7, FUSE_DW7.actual, 0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)
     `RAL_FIELD_CP_16(FUSE_DW7, FUSE_DW7.actual, 16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31)
  endgroup
  `endif

  // --------------------------
  // add constraint based on database legal values
  // --------------------------
  function void build();
    FUSE_DW7 = new("FUSE_DW7", "RO/V", 32, 0, {"PMU_FUSE_7.FUSE_DW7"});
    FUSE_DW7.set_powerwell("primary");
    FUSE_DW7.set_rand_mode(0);
    void'(add_field( FUSE_DW7 ));

  endfunction


`DEFINE_NONE_SCOPE_REG_API(pmu_mmr_PMU_FUSE_7_reg) 
endclass : pmu_mmr_PMU_FUSE_7_reg

// ================================================

class pmu_mmr_PMU_FUSE_8_reg extends sla_ral_reg;

  // --------------------------
  sla_ral_field FUSE_DW8;

  // --------------------------
  `ovm_object_utils(pmu_mmr_PMU_FUSE_8_reg)

  // --------------------------
  function new(
                string         name = "",
                `ifdef SLA_RAL_COVERAGE
                sla_ral_coverage_t cov_t = COVERAGE_OFF,
                `endif
                int            bus_num=0,
                int            dev_num=0,
                int            func_num=0,
                int            offset=0,
                int            size=0,
                sla_ral_data_t reset_val=0,
                boolean_t      mon_enabled = SLA_FALSE
             );
      super.new();
      set_space_addr("CFG",undef);
      set_space_addr("MEM",undef);
      set_space_addr("IO",undef);
      set_space_addr("MSG",undef);
      set_space_addr("CREG",undef); 
      set_space_addr("LT_MEM",undef);    
 build();

      `ifdef SLA_RAL_COVERAGE
      `CONFIG_RAL_COVERAGE(regname, COVERAGE_ON, FD_RW_SAMPLE)
      if(_coverage_type != COVERAGE_OFF) begin
        if(_sample_type inside {FD_RO_SAMPLE, RO_SAMPLE})
        `CREATE_OP_SAMPLE_COV(RAL_WRITE)
        else if(_sample_type inside {FD_WO_SAMPLE, WO_SAMPLE})
        `CREATE_OP_SAMPLE_COV(RAL_READ)
        else
        `CREATE_OP_SAMPLE_COV(RAL_NONE)
        `CREATE_DESIRED_COV
        `CREATE_ACTUAL_COV
      end
      `endif

  endfunction

  `ifdef SLA_RAL_COVERAGE
  `CREATE_RAL_OP_COVERGROUP
  covergroup desired_cg @(cov_ev);
     option.per_instance = 1;
     option.name = {sla_ral_file::regfilename,".",regname};
     `RAL_FIELD_CP(FUSE_DW8, FUSE_DW8.desired)
     `RAL_FIELD_CP_16(FUSE_DW8, FUSE_DW8.desired, 0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)
     `RAL_FIELD_CP_16(FUSE_DW8, FUSE_DW8.desired, 16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31)
  endgroup
  covergroup actual_cg @(cov_ev);
     option.per_instance = 1;
     option.name = {sla_ral_file::regfilename,".",regname};
     `RAL_FIELD_CP(FUSE_DW8, FUSE_DW8.actual)
     `RAL_FIELD_CP_16(FUSE_DW8, FUSE_DW8.actual, 0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)
     `RAL_FIELD_CP_16(FUSE_DW8, FUSE_DW8.actual, 16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31)
  endgroup
  `endif

  // --------------------------
  // add constraint based on database legal values
  // --------------------------
  function void build();
    FUSE_DW8 = new("FUSE_DW8", "RO/V", 32, 0, {"PMU_FUSE_8.FUSE_DW8"});
    FUSE_DW8.set_powerwell("primary");
    FUSE_DW8.set_rand_mode(0);
    void'(add_field( FUSE_DW8 ));

  endfunction


`DEFINE_NONE_SCOPE_REG_API(pmu_mmr_PMU_FUSE_8_reg) 
endclass : pmu_mmr_PMU_FUSE_8_reg

// ================================================

class pmu_mmr_PMU_FUSE_9_reg extends sla_ral_reg;

  // --------------------------
  sla_ral_field FUSE_DW9;

  // --------------------------
  `ovm_object_utils(pmu_mmr_PMU_FUSE_9_reg)

  // --------------------------
  function new(
                string         name = "",
                `ifdef SLA_RAL_COVERAGE
                sla_ral_coverage_t cov_t = COVERAGE_OFF,
                `endif
                int            bus_num=0,
                int            dev_num=0,
                int            func_num=0,
                int            offset=0,
                int            size=0,
                sla_ral_data_t reset_val=0,
                boolean_t      mon_enabled = SLA_FALSE
             );
      super.new();
      set_space_addr("CFG",undef);
      set_space_addr("MEM",undef);
      set_space_addr("IO",undef);
      set_space_addr("MSG",undef);
      set_space_addr("CREG",undef); 
      set_space_addr("LT_MEM",undef);    
 build();

      `ifdef SLA_RAL_COVERAGE
      `CONFIG_RAL_COVERAGE(regname, COVERAGE_ON, FD_RW_SAMPLE)
      if(_coverage_type != COVERAGE_OFF) begin
        if(_sample_type inside {FD_RO_SAMPLE, RO_SAMPLE})
        `CREATE_OP_SAMPLE_COV(RAL_WRITE)
        else if(_sample_type inside {FD_WO_SAMPLE, WO_SAMPLE})
        `CREATE_OP_SAMPLE_COV(RAL_READ)
        else
        `CREATE_OP_SAMPLE_COV(RAL_NONE)
        `CREATE_DESIRED_COV
        `CREATE_ACTUAL_COV
      end
      `endif

  endfunction

  `ifdef SLA_RAL_COVERAGE
  `CREATE_RAL_OP_COVERGROUP
  covergroup desired_cg @(cov_ev);
     option.per_instance = 1;
     option.name = {sla_ral_file::regfilename,".",regname};
     `RAL_FIELD_CP(FUSE_DW9, FUSE_DW9.desired)
     `RAL_FIELD_CP_16(FUSE_DW9, FUSE_DW9.desired, 0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)
     `RAL_FIELD_CP_16(FUSE_DW9, FUSE_DW9.desired, 16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31)
  endgroup
  covergroup actual_cg @(cov_ev);
     option.per_instance = 1;
     option.name = {sla_ral_file::regfilename,".",regname};
     `RAL_FIELD_CP(FUSE_DW9, FUSE_DW9.actual)
     `RAL_FIELD_CP_16(FUSE_DW9, FUSE_DW9.actual, 0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)
     `RAL_FIELD_CP_16(FUSE_DW9, FUSE_DW9.actual, 16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31)
  endgroup
  `endif

  // --------------------------
  // add constraint based on database legal values
  // --------------------------
  function void build();
    FUSE_DW9 = new("FUSE_DW9", "RO/V", 32, 0, {"PMU_FUSE_9.FUSE_DW9"});
    FUSE_DW9.set_powerwell("primary");
    FUSE_DW9.set_rand_mode(0);
    void'(add_field( FUSE_DW9 ));

  endfunction


`DEFINE_NONE_SCOPE_REG_API(pmu_mmr_PMU_FUSE_9_reg) 
endclass : pmu_mmr_PMU_FUSE_9_reg

// ================================================

class pmu_mmr_PMU_FUSE_10_reg extends sla_ral_reg;

  // --------------------------
  sla_ral_field FUSE_DW10;

  // --------------------------
  `ovm_object_utils(pmu_mmr_PMU_FUSE_10_reg)

  // --------------------------
  function new(
                string         name = "",
                `ifdef SLA_RAL_COVERAGE
                sla_ral_coverage_t cov_t = COVERAGE_OFF,
                `endif
                int            bus_num=0,
                int            dev_num=0,
                int            func_num=0,
                int            offset=0,
                int            size=0,
                sla_ral_data_t reset_val=0,
                boolean_t      mon_enabled = SLA_FALSE
             );
      super.new();
      set_space_addr("CFG",undef);
      set_space_addr("MEM",undef);
      set_space_addr("IO",undef);
      set_space_addr("MSG",undef);
      set_space_addr("CREG",undef); 
      set_space_addr("LT_MEM",undef);    
 build();

      `ifdef SLA_RAL_COVERAGE
      `CONFIG_RAL_COVERAGE(regname, COVERAGE_ON, FD_RW_SAMPLE)
      if(_coverage_type != COVERAGE_OFF) begin
        if(_sample_type inside {FD_RO_SAMPLE, RO_SAMPLE})
        `CREATE_OP_SAMPLE_COV(RAL_WRITE)
        else if(_sample_type inside {FD_WO_SAMPLE, WO_SAMPLE})
        `CREATE_OP_SAMPLE_COV(RAL_READ)
        else
        `CREATE_OP_SAMPLE_COV(RAL_NONE)
        `CREATE_DESIRED_COV
        `CREATE_ACTUAL_COV
      end
      `endif

  endfunction

  `ifdef SLA_RAL_COVERAGE
  `CREATE_RAL_OP_COVERGROUP
  covergroup desired_cg @(cov_ev);
     option.per_instance = 1;
     option.name = {sla_ral_file::regfilename,".",regname};
     `RAL_FIELD_CP(FUSE_DW10, FUSE_DW10.desired)
     `RAL_FIELD_CP_16(FUSE_DW10, FUSE_DW10.desired, 0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)
     `RAL_FIELD_CP_16(FUSE_DW10, FUSE_DW10.desired, 16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31)
  endgroup
  covergroup actual_cg @(cov_ev);
     option.per_instance = 1;
     option.name = {sla_ral_file::regfilename,".",regname};
     `RAL_FIELD_CP(FUSE_DW10, FUSE_DW10.actual)
     `RAL_FIELD_CP_16(FUSE_DW10, FUSE_DW10.actual, 0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)
     `RAL_FIELD_CP_16(FUSE_DW10, FUSE_DW10.actual, 16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31)
  endgroup
  `endif

  // --------------------------
  // add constraint based on database legal values
  // --------------------------
  function void build();
    FUSE_DW10 = new("FUSE_DW10", "RO/V", 32, 0, {"PMU_FUSE_10.FUSE_DW10"});
    FUSE_DW10.set_powerwell("primary");
    FUSE_DW10.set_rand_mode(0);
    void'(add_field( FUSE_DW10 ));

  endfunction


`DEFINE_NONE_SCOPE_REG_API(pmu_mmr_PMU_FUSE_10_reg) 
endclass : pmu_mmr_PMU_FUSE_10_reg

// ================================================

class pmu_mmr_PMU_FUSE_11_reg extends sla_ral_reg;

  // --------------------------
  sla_ral_field FUSE_DW11;

  // --------------------------
  `ovm_object_utils(pmu_mmr_PMU_FUSE_11_reg)

  // --------------------------
  function new(
                string         name = "",
                `ifdef SLA_RAL_COVERAGE
                sla_ral_coverage_t cov_t = COVERAGE_OFF,
                `endif
                int            bus_num=0,
                int            dev_num=0,
                int            func_num=0,
                int            offset=0,
                int            size=0,
                sla_ral_data_t reset_val=0,
                boolean_t      mon_enabled = SLA_FALSE
             );
      super.new();
      set_space_addr("CFG",undef);
      set_space_addr("MEM",undef);
      set_space_addr("IO",undef);
      set_space_addr("MSG",undef);
      set_space_addr("CREG",undef); 
      set_space_addr("LT_MEM",undef);    
 build();

      `ifdef SLA_RAL_COVERAGE
      `CONFIG_RAL_COVERAGE(regname, COVERAGE_ON, FD_RW_SAMPLE)
      if(_coverage_type != COVERAGE_OFF) begin
        if(_sample_type inside {FD_RO_SAMPLE, RO_SAMPLE})
        `CREATE_OP_SAMPLE_COV(RAL_WRITE)
        else if(_sample_type inside {FD_WO_SAMPLE, WO_SAMPLE})
        `CREATE_OP_SAMPLE_COV(RAL_READ)
        else
        `CREATE_OP_SAMPLE_COV(RAL_NONE)
        `CREATE_DESIRED_COV
        `CREATE_ACTUAL_COV
      end
      `endif

  endfunction

  `ifdef SLA_RAL_COVERAGE
  `CREATE_RAL_OP_COVERGROUP
  covergroup desired_cg @(cov_ev);
     option.per_instance = 1;
     option.name = {sla_ral_file::regfilename,".",regname};
     `RAL_FIELD_CP(FUSE_DW11, FUSE_DW11.desired)
     `RAL_FIELD_CP_16(FUSE_DW11, FUSE_DW11.desired, 0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)
     `RAL_FIELD_CP_16(FUSE_DW11, FUSE_DW11.desired, 16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31)
  endgroup
  covergroup actual_cg @(cov_ev);
     option.per_instance = 1;
     option.name = {sla_ral_file::regfilename,".",regname};
     `RAL_FIELD_CP(FUSE_DW11, FUSE_DW11.actual)
     `RAL_FIELD_CP_16(FUSE_DW11, FUSE_DW11.actual, 0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)
     `RAL_FIELD_CP_16(FUSE_DW11, FUSE_DW11.actual, 16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31)
  endgroup
  `endif

  // --------------------------
  // add constraint based on database legal values
  // --------------------------
  function void build();
    FUSE_DW11 = new("FUSE_DW11", "RO/V", 32, 0, {"PMU_FUSE_11.FUSE_DW11"});
    FUSE_DW11.set_powerwell("primary");
    FUSE_DW11.set_rand_mode(0);
    void'(add_field( FUSE_DW11 ));

  endfunction


`DEFINE_NONE_SCOPE_REG_API(pmu_mmr_PMU_FUSE_11_reg) 
endclass : pmu_mmr_PMU_FUSE_11_reg

// ================================================

class pmu_mmr_PMU_FUSE_12_reg extends sla_ral_reg;

  // --------------------------
  sla_ral_field FUSE_DW12;

  // --------------------------
  `ovm_object_utils(pmu_mmr_PMU_FUSE_12_reg)

  // --------------------------
  function new(
                string         name = "",
                `ifdef SLA_RAL_COVERAGE
                sla_ral_coverage_t cov_t = COVERAGE_OFF,
                `endif
                int            bus_num=0,
                int            dev_num=0,
                int            func_num=0,
                int            offset=0,
                int            size=0,
                sla_ral_data_t reset_val=0,
                boolean_t      mon_enabled = SLA_FALSE
             );
      super.new();
      set_space_addr("CFG",undef);
      set_space_addr("MEM",undef);
      set_space_addr("IO",undef);
      set_space_addr("MSG",undef);
      set_space_addr("CREG",undef); 
      set_space_addr("LT_MEM",undef);    
 build();

      `ifdef SLA_RAL_COVERAGE
      `CONFIG_RAL_COVERAGE(regname, COVERAGE_ON, FD_RW_SAMPLE)
      if(_coverage_type != COVERAGE_OFF) begin
        if(_sample_type inside {FD_RO_SAMPLE, RO_SAMPLE})
        `CREATE_OP_SAMPLE_COV(RAL_WRITE)
        else if(_sample_type inside {FD_WO_SAMPLE, WO_SAMPLE})
        `CREATE_OP_SAMPLE_COV(RAL_READ)
        else
        `CREATE_OP_SAMPLE_COV(RAL_NONE)
        `CREATE_DESIRED_COV
        `CREATE_ACTUAL_COV
      end
      `endif

  endfunction

  `ifdef SLA_RAL_COVERAGE
  `CREATE_RAL_OP_COVERGROUP
  covergroup desired_cg @(cov_ev);
     option.per_instance = 1;
     option.name = {sla_ral_file::regfilename,".",regname};
     `RAL_FIELD_CP(FUSE_DW12, FUSE_DW12.desired)
     `RAL_FIELD_CP_16(FUSE_DW12, FUSE_DW12.desired, 0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)
     `RAL_FIELD_CP_16(FUSE_DW12, FUSE_DW12.desired, 16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31)
  endgroup
  covergroup actual_cg @(cov_ev);
     option.per_instance = 1;
     option.name = {sla_ral_file::regfilename,".",regname};
     `RAL_FIELD_CP(FUSE_DW12, FUSE_DW12.actual)
     `RAL_FIELD_CP_16(FUSE_DW12, FUSE_DW12.actual, 0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)
     `RAL_FIELD_CP_16(FUSE_DW12, FUSE_DW12.actual, 16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31)
  endgroup
  `endif

  // --------------------------
  // add constraint based on database legal values
  // --------------------------
  function void build();
    FUSE_DW12 = new("FUSE_DW12", "RO/V", 32, 0, {"PMU_FUSE_12.FUSE_DW12"});
    FUSE_DW12.set_powerwell("primary");
    FUSE_DW12.set_rand_mode(0);
    void'(add_field( FUSE_DW12 ));

  endfunction


`DEFINE_NONE_SCOPE_REG_API(pmu_mmr_PMU_FUSE_12_reg) 
endclass : pmu_mmr_PMU_FUSE_12_reg

// ================================================

class pmu_mmr_PMU_FUSE_13_reg extends sla_ral_reg;

  // --------------------------
  sla_ral_field FUSE_DW13;

  // --------------------------
  `ovm_object_utils(pmu_mmr_PMU_FUSE_13_reg)

  // --------------------------
  function new(
                string         name = "",
                `ifdef SLA_RAL_COVERAGE
                sla_ral_coverage_t cov_t = COVERAGE_OFF,
                `endif
                int            bus_num=0,
                int            dev_num=0,
                int            func_num=0,
                int            offset=0,
                int            size=0,
                sla_ral_data_t reset_val=0,
                boolean_t      mon_enabled = SLA_FALSE
             );
      super.new();
      set_space_addr("CFG",undef);
      set_space_addr("MEM",undef);
      set_space_addr("IO",undef);
      set_space_addr("MSG",undef);
      set_space_addr("CREG",undef); 
      set_space_addr("LT_MEM",undef);    
 build();

      `ifdef SLA_RAL_COVERAGE
      `CONFIG_RAL_COVERAGE(regname, COVERAGE_ON, FD_RW_SAMPLE)
      if(_coverage_type != COVERAGE_OFF) begin
        if(_sample_type inside {FD_RO_SAMPLE, RO_SAMPLE})
        `CREATE_OP_SAMPLE_COV(RAL_WRITE)
        else if(_sample_type inside {FD_WO_SAMPLE, WO_SAMPLE})
        `CREATE_OP_SAMPLE_COV(RAL_READ)
        else
        `CREATE_OP_SAMPLE_COV(RAL_NONE)
        `CREATE_DESIRED_COV
        `CREATE_ACTUAL_COV
      end
      `endif

  endfunction

  `ifdef SLA_RAL_COVERAGE
  `CREATE_RAL_OP_COVERGROUP
  covergroup desired_cg @(cov_ev);
     option.per_instance = 1;
     option.name = {sla_ral_file::regfilename,".",regname};
     `RAL_FIELD_CP(FUSE_DW13, FUSE_DW13.desired)
     `RAL_FIELD_CP_16(FUSE_DW13, FUSE_DW13.desired, 0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)
     `RAL_FIELD_CP_16(FUSE_DW13, FUSE_DW13.desired, 16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31)
  endgroup
  covergroup actual_cg @(cov_ev);
     option.per_instance = 1;
     option.name = {sla_ral_file::regfilename,".",regname};
     `RAL_FIELD_CP(FUSE_DW13, FUSE_DW13.actual)
     `RAL_FIELD_CP_16(FUSE_DW13, FUSE_DW13.actual, 0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)
     `RAL_FIELD_CP_16(FUSE_DW13, FUSE_DW13.actual, 16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31)
  endgroup
  `endif

  // --------------------------
  // add constraint based on database legal values
  // --------------------------
  function void build();
    FUSE_DW13 = new("FUSE_DW13", "RO/V", 32, 0, {"PMU_FUSE_13.FUSE_DW13"});
    FUSE_DW13.set_powerwell("primary");
    FUSE_DW13.set_rand_mode(0);
    void'(add_field( FUSE_DW13 ));

  endfunction


`DEFINE_NONE_SCOPE_REG_API(pmu_mmr_PMU_FUSE_13_reg) 
endclass : pmu_mmr_PMU_FUSE_13_reg

// ================================================

class pmu_mmr_PMU_FUSE_14_reg extends sla_ral_reg;

  // --------------------------
  sla_ral_field FUSE_DW14;

  // --------------------------
  `ovm_object_utils(pmu_mmr_PMU_FUSE_14_reg)

  // --------------------------
  function new(
                string         name = "",
                `ifdef SLA_RAL_COVERAGE
                sla_ral_coverage_t cov_t = COVERAGE_OFF,
                `endif
                int            bus_num=0,
                int            dev_num=0,
                int            func_num=0,
                int            offset=0,
                int            size=0,
                sla_ral_data_t reset_val=0,
                boolean_t      mon_enabled = SLA_FALSE
             );
      super.new();
      set_space_addr("CFG",undef);
      set_space_addr("MEM",undef);
      set_space_addr("IO",undef);
      set_space_addr("MSG",undef);
      set_space_addr("CREG",undef); 
      set_space_addr("LT_MEM",undef);    
 build();

      `ifdef SLA_RAL_COVERAGE
      `CONFIG_RAL_COVERAGE(regname, COVERAGE_ON, FD_RW_SAMPLE)
      if(_coverage_type != COVERAGE_OFF) begin
        if(_sample_type inside {FD_RO_SAMPLE, RO_SAMPLE})
        `CREATE_OP_SAMPLE_COV(RAL_WRITE)
        else if(_sample_type inside {FD_WO_SAMPLE, WO_SAMPLE})
        `CREATE_OP_SAMPLE_COV(RAL_READ)
        else
        `CREATE_OP_SAMPLE_COV(RAL_NONE)
        `CREATE_DESIRED_COV
        `CREATE_ACTUAL_COV
      end
      `endif

  endfunction

  `ifdef SLA_RAL_COVERAGE
  `CREATE_RAL_OP_COVERGROUP
  covergroup desired_cg @(cov_ev);
     option.per_instance = 1;
     option.name = {sla_ral_file::regfilename,".",regname};
     `RAL_FIELD_CP(FUSE_DW14, FUSE_DW14.desired)
     `RAL_FIELD_CP_16(FUSE_DW14, FUSE_DW14.desired, 0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)
     `RAL_FIELD_CP_16(FUSE_DW14, FUSE_DW14.desired, 16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31)
  endgroup
  covergroup actual_cg @(cov_ev);
     option.per_instance = 1;
     option.name = {sla_ral_file::regfilename,".",regname};
     `RAL_FIELD_CP(FUSE_DW14, FUSE_DW14.actual)
     `RAL_FIELD_CP_16(FUSE_DW14, FUSE_DW14.actual, 0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)
     `RAL_FIELD_CP_16(FUSE_DW14, FUSE_DW14.actual, 16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31)
  endgroup
  `endif

  // --------------------------
  // add constraint based on database legal values
  // --------------------------
  function void build();
    FUSE_DW14 = new("FUSE_DW14", "RO/V", 32, 0, {"PMU_FUSE_14.FUSE_DW14"});
    FUSE_DW14.set_powerwell("primary");
    FUSE_DW14.set_rand_mode(0);
    void'(add_field( FUSE_DW14 ));

  endfunction


`DEFINE_NONE_SCOPE_REG_API(pmu_mmr_PMU_FUSE_14_reg) 
endclass : pmu_mmr_PMU_FUSE_14_reg

// ================================================

class pmu_mmr_PMU_FUSE_15_reg extends sla_ral_reg;

  // --------------------------
  sla_ral_field FUSE_DW15;

  // --------------------------
  `ovm_object_utils(pmu_mmr_PMU_FUSE_15_reg)

  // --------------------------
  function new(
                string         name = "",
                `ifdef SLA_RAL_COVERAGE
                sla_ral_coverage_t cov_t = COVERAGE_OFF,
                `endif
                int            bus_num=0,
                int            dev_num=0,
                int            func_num=0,
                int            offset=0,
                int            size=0,
                sla_ral_data_t reset_val=0,
                boolean_t      mon_enabled = SLA_FALSE
             );
      super.new();
      set_space_addr("CFG",undef);
      set_space_addr("MEM",undef);
      set_space_addr("IO",undef);
      set_space_addr("MSG",undef);
      set_space_addr("CREG",undef); 
      set_space_addr("LT_MEM",undef);    
 build();

      `ifdef SLA_RAL_COVERAGE
      `CONFIG_RAL_COVERAGE(regname, COVERAGE_ON, FD_RW_SAMPLE)
      if(_coverage_type != COVERAGE_OFF) begin
        if(_sample_type inside {FD_RO_SAMPLE, RO_SAMPLE})
        `CREATE_OP_SAMPLE_COV(RAL_WRITE)
        else if(_sample_type inside {FD_WO_SAMPLE, WO_SAMPLE})
        `CREATE_OP_SAMPLE_COV(RAL_READ)
        else
        `CREATE_OP_SAMPLE_COV(RAL_NONE)
        `CREATE_DESIRED_COV
        `CREATE_ACTUAL_COV
      end
      `endif

  endfunction

  `ifdef SLA_RAL_COVERAGE
  `CREATE_RAL_OP_COVERGROUP
  covergroup desired_cg @(cov_ev);
     option.per_instance = 1;
     option.name = {sla_ral_file::regfilename,".",regname};
     `RAL_FIELD_CP(FUSE_DW15, FUSE_DW15.desired)
     `RAL_FIELD_CP_16(FUSE_DW15, FUSE_DW15.desired, 0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)
     `RAL_FIELD_CP_16(FUSE_DW15, FUSE_DW15.desired, 16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31)
  endgroup
  covergroup actual_cg @(cov_ev);
     option.per_instance = 1;
     option.name = {sla_ral_file::regfilename,".",regname};
     `RAL_FIELD_CP(FUSE_DW15, FUSE_DW15.actual)
     `RAL_FIELD_CP_16(FUSE_DW15, FUSE_DW15.actual, 0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)
     `RAL_FIELD_CP_16(FUSE_DW15, FUSE_DW15.actual, 16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31)
  endgroup
  `endif

  // --------------------------
  // add constraint based on database legal values
  // --------------------------
  function void build();
    FUSE_DW15 = new("FUSE_DW15", "RO/V", 32, 0, {"PMU_FUSE_15.FUSE_DW15"});
    FUSE_DW15.set_powerwell("primary");
    FUSE_DW15.set_rand_mode(0);
    void'(add_field( FUSE_DW15 ));

  endfunction


`DEFINE_NONE_SCOPE_REG_API(pmu_mmr_PMU_FUSE_15_reg) 
endclass : pmu_mmr_PMU_FUSE_15_reg

// ================================================

class pmu_mmr_PMU_STRAP_0_reg extends sla_ral_reg;

  // --------------------------
  sla_ral_field SOFT_STRAP_DW0;

  // --------------------------
  `ovm_object_utils(pmu_mmr_PMU_STRAP_0_reg)

  // --------------------------
  function new(
                string         name = "",
                `ifdef SLA_RAL_COVERAGE
                sla_ral_coverage_t cov_t = COVERAGE_OFF,
                `endif
                int            bus_num=0,
                int            dev_num=0,
                int            func_num=0,
                int            offset=0,
                int            size=0,
                sla_ral_data_t reset_val=0,
                boolean_t      mon_enabled = SLA_FALSE
             );
      super.new();
      set_space_addr("CFG",undef);
      set_space_addr("MEM",undef);
      set_space_addr("IO",undef);
      set_space_addr("MSG",undef);
      set_space_addr("CREG",undef); 
      set_space_addr("LT_MEM",undef);    
 build();

      `ifdef SLA_RAL_COVERAGE
      `CONFIG_RAL_COVERAGE(regname, COVERAGE_ON, FD_RW_SAMPLE)
      if(_coverage_type != COVERAGE_OFF) begin
        if(_sample_type inside {FD_RO_SAMPLE, RO_SAMPLE})
        `CREATE_OP_SAMPLE_COV(RAL_WRITE)
        else if(_sample_type inside {FD_WO_SAMPLE, WO_SAMPLE})
        `CREATE_OP_SAMPLE_COV(RAL_READ)
        else
        `CREATE_OP_SAMPLE_COV(RAL_NONE)
        `CREATE_DESIRED_COV
        `CREATE_ACTUAL_COV
      end
      `endif

  endfunction

  `ifdef SLA_RAL_COVERAGE
  `CREATE_RAL_OP_COVERGROUP
  covergroup desired_cg @(cov_ev);
     option.per_instance = 1;
     option.name = {sla_ral_file::regfilename,".",regname};
     `RAL_FIELD_CP(SOFT_STRAP_DW0, SOFT_STRAP_DW0.desired)
     `RAL_FIELD_CP_16(SOFT_STRAP_DW0, SOFT_STRAP_DW0.desired, 0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)
     `RAL_FIELD_CP_16(SOFT_STRAP_DW0, SOFT_STRAP_DW0.desired, 16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31)
  endgroup
  covergroup actual_cg @(cov_ev);
     option.per_instance = 1;
     option.name = {sla_ral_file::regfilename,".",regname};
     `RAL_FIELD_CP(SOFT_STRAP_DW0, SOFT_STRAP_DW0.actual)
     `RAL_FIELD_CP_16(SOFT_STRAP_DW0, SOFT_STRAP_DW0.actual, 0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)
     `RAL_FIELD_CP_16(SOFT_STRAP_DW0, SOFT_STRAP_DW0.actual, 16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31)
  endgroup
  `endif

  // --------------------------
  // add constraint based on database legal values
  // --------------------------
  function void build();
    SOFT_STRAP_DW0 = new("SOFT_STRAP_DW0", "RO/V", 32, 0, {"PMU_STRAP_0.SOFT_STRAP_DW0"});
    SOFT_STRAP_DW0.set_powerwell("primary");
    SOFT_STRAP_DW0.set_rand_mode(0);
    void'(add_field( SOFT_STRAP_DW0 ));

  endfunction


`DEFINE_NONE_SCOPE_REG_API(pmu_mmr_PMU_STRAP_0_reg) 
endclass : pmu_mmr_PMU_STRAP_0_reg

// ================================================

class pmu_mmr_PMU_STRAP_1_reg extends sla_ral_reg;

  // --------------------------
  sla_ral_field SOFT_STRAP_DW1;

  // --------------------------
  `ovm_object_utils(pmu_mmr_PMU_STRAP_1_reg)

  // --------------------------
  function new(
                string         name = "",
                `ifdef SLA_RAL_COVERAGE
                sla_ral_coverage_t cov_t = COVERAGE_OFF,
                `endif
                int            bus_num=0,
                int            dev_num=0,
                int            func_num=0,
                int            offset=0,
                int            size=0,
                sla_ral_data_t reset_val=0,
                boolean_t      mon_enabled = SLA_FALSE
             );
      super.new();
      set_space_addr("CFG",undef);
      set_space_addr("MEM",undef);
      set_space_addr("IO",undef);
      set_space_addr("MSG",undef);
      set_space_addr("CREG",undef); 
      set_space_addr("LT_MEM",undef);    
 build();

      `ifdef SLA_RAL_COVERAGE
      `CONFIG_RAL_COVERAGE(regname, COVERAGE_ON, FD_RW_SAMPLE)
      if(_coverage_type != COVERAGE_OFF) begin
        if(_sample_type inside {FD_RO_SAMPLE, RO_SAMPLE})
        `CREATE_OP_SAMPLE_COV(RAL_WRITE)
        else if(_sample_type inside {FD_WO_SAMPLE, WO_SAMPLE})
        `CREATE_OP_SAMPLE_COV(RAL_READ)
        else
        `CREATE_OP_SAMPLE_COV(RAL_NONE)
        `CREATE_DESIRED_COV
        `CREATE_ACTUAL_COV
      end
      `endif

  endfunction

  `ifdef SLA_RAL_COVERAGE
  `CREATE_RAL_OP_COVERGROUP
  covergroup desired_cg @(cov_ev);
     option.per_instance = 1;
     option.name = {sla_ral_file::regfilename,".",regname};
     `RAL_FIELD_CP(SOFT_STRAP_DW1, SOFT_STRAP_DW1.desired)
     `RAL_FIELD_CP_16(SOFT_STRAP_DW1, SOFT_STRAP_DW1.desired, 0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)
     `RAL_FIELD_CP_16(SOFT_STRAP_DW1, SOFT_STRAP_DW1.desired, 16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31)
  endgroup
  covergroup actual_cg @(cov_ev);
     option.per_instance = 1;
     option.name = {sla_ral_file::regfilename,".",regname};
     `RAL_FIELD_CP(SOFT_STRAP_DW1, SOFT_STRAP_DW1.actual)
     `RAL_FIELD_CP_16(SOFT_STRAP_DW1, SOFT_STRAP_DW1.actual, 0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)
     `RAL_FIELD_CP_16(SOFT_STRAP_DW1, SOFT_STRAP_DW1.actual, 16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31)
  endgroup
  `endif

  // --------------------------
  // add constraint based on database legal values
  // --------------------------
  function void build();
    SOFT_STRAP_DW1 = new("SOFT_STRAP_DW1", "RO/V", 32, 0, {"PMU_STRAP_1.SOFT_STRAP_DW1"});
    SOFT_STRAP_DW1.set_powerwell("primary");
    SOFT_STRAP_DW1.set_rand_mode(0);
    void'(add_field( SOFT_STRAP_DW1 ));

  endfunction


`DEFINE_NONE_SCOPE_REG_API(pmu_mmr_PMU_STRAP_1_reg) 
endclass : pmu_mmr_PMU_STRAP_1_reg

// ================================================

class pmu_mmr_PMU_STRAP_2_reg extends sla_ral_reg;

  // --------------------------
  sla_ral_field SOFT_STRAP_DW2;

  // --------------------------
  `ovm_object_utils(pmu_mmr_PMU_STRAP_2_reg)

  // --------------------------
  function new(
                string         name = "",
                `ifdef SLA_RAL_COVERAGE
                sla_ral_coverage_t cov_t = COVERAGE_OFF,
                `endif
                int            bus_num=0,
                int            dev_num=0,
                int            func_num=0,
                int            offset=0,
                int            size=0,
                sla_ral_data_t reset_val=0,
                boolean_t      mon_enabled = SLA_FALSE
             );
      super.new();
      set_space_addr("CFG",undef);
      set_space_addr("MEM",undef);
      set_space_addr("IO",undef);
      set_space_addr("MSG",undef);
      set_space_addr("CREG",undef); 
      set_space_addr("LT_MEM",undef);    
 build();

      `ifdef SLA_RAL_COVERAGE
      `CONFIG_RAL_COVERAGE(regname, COVERAGE_ON, FD_RW_SAMPLE)
      if(_coverage_type != COVERAGE_OFF) begin
        if(_sample_type inside {FD_RO_SAMPLE, RO_SAMPLE})
        `CREATE_OP_SAMPLE_COV(RAL_WRITE)
        else if(_sample_type inside {FD_WO_SAMPLE, WO_SAMPLE})
        `CREATE_OP_SAMPLE_COV(RAL_READ)
        else
        `CREATE_OP_SAMPLE_COV(RAL_NONE)
        `CREATE_DESIRED_COV
        `CREATE_ACTUAL_COV
      end
      `endif

  endfunction

  `ifdef SLA_RAL_COVERAGE
  `CREATE_RAL_OP_COVERGROUP
  covergroup desired_cg @(cov_ev);
     option.per_instance = 1;
     option.name = {sla_ral_file::regfilename,".",regname};
     `RAL_FIELD_CP(SOFT_STRAP_DW2, SOFT_STRAP_DW2.desired)
     `RAL_FIELD_CP_16(SOFT_STRAP_DW2, SOFT_STRAP_DW2.desired, 0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)
     `RAL_FIELD_CP_16(SOFT_STRAP_DW2, SOFT_STRAP_DW2.desired, 16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31)
  endgroup
  covergroup actual_cg @(cov_ev);
     option.per_instance = 1;
     option.name = {sla_ral_file::regfilename,".",regname};
     `RAL_FIELD_CP(SOFT_STRAP_DW2, SOFT_STRAP_DW2.actual)
     `RAL_FIELD_CP_16(SOFT_STRAP_DW2, SOFT_STRAP_DW2.actual, 0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)
     `RAL_FIELD_CP_16(SOFT_STRAP_DW2, SOFT_STRAP_DW2.actual, 16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31)
  endgroup
  `endif

  // --------------------------
  // add constraint based on database legal values
  // --------------------------
  function void build();
    SOFT_STRAP_DW2 = new("SOFT_STRAP_DW2", "RO/V", 32, 0, {"PMU_STRAP_2.SOFT_STRAP_DW2"});
    SOFT_STRAP_DW2.set_powerwell("primary");
    SOFT_STRAP_DW2.set_rand_mode(0);
    void'(add_field( SOFT_STRAP_DW2 ));

  endfunction


`DEFINE_NONE_SCOPE_REG_API(pmu_mmr_PMU_STRAP_2_reg) 
endclass : pmu_mmr_PMU_STRAP_2_reg

// ================================================

class pmu_mmr_PMU_STRAP_3_reg extends sla_ral_reg;

  // --------------------------
  sla_ral_field SOFT_STRAP_DW3;

  // --------------------------
  `ovm_object_utils(pmu_mmr_PMU_STRAP_3_reg)

  // --------------------------
  function new(
                string         name = "",
                `ifdef SLA_RAL_COVERAGE
                sla_ral_coverage_t cov_t = COVERAGE_OFF,
                `endif
                int            bus_num=0,
                int            dev_num=0,
                int            func_num=0,
                int            offset=0,
                int            size=0,
                sla_ral_data_t reset_val=0,
                boolean_t      mon_enabled = SLA_FALSE
             );
      super.new();
      set_space_addr("CFG",undef);
      set_space_addr("MEM",undef);
      set_space_addr("IO",undef);
      set_space_addr("MSG",undef);
      set_space_addr("CREG",undef); 
      set_space_addr("LT_MEM",undef);    
 build();

      `ifdef SLA_RAL_COVERAGE
      `CONFIG_RAL_COVERAGE(regname, COVERAGE_ON, FD_RW_SAMPLE)
      if(_coverage_type != COVERAGE_OFF) begin
        if(_sample_type inside {FD_RO_SAMPLE, RO_SAMPLE})
        `CREATE_OP_SAMPLE_COV(RAL_WRITE)
        else if(_sample_type inside {FD_WO_SAMPLE, WO_SAMPLE})
        `CREATE_OP_SAMPLE_COV(RAL_READ)
        else
        `CREATE_OP_SAMPLE_COV(RAL_NONE)
        `CREATE_DESIRED_COV
        `CREATE_ACTUAL_COV
      end
      `endif

  endfunction

  `ifdef SLA_RAL_COVERAGE
  `CREATE_RAL_OP_COVERGROUP
  covergroup desired_cg @(cov_ev);
     option.per_instance = 1;
     option.name = {sla_ral_file::regfilename,".",regname};
     `RAL_FIELD_CP(SOFT_STRAP_DW3, SOFT_STRAP_DW3.desired)
     `RAL_FIELD_CP_16(SOFT_STRAP_DW3, SOFT_STRAP_DW3.desired, 0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)
     `RAL_FIELD_CP_16(SOFT_STRAP_DW3, SOFT_STRAP_DW3.desired, 16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31)
  endgroup
  covergroup actual_cg @(cov_ev);
     option.per_instance = 1;
     option.name = {sla_ral_file::regfilename,".",regname};
     `RAL_FIELD_CP(SOFT_STRAP_DW3, SOFT_STRAP_DW3.actual)
     `RAL_FIELD_CP_16(SOFT_STRAP_DW3, SOFT_STRAP_DW3.actual, 0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)
     `RAL_FIELD_CP_16(SOFT_STRAP_DW3, SOFT_STRAP_DW3.actual, 16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31)
  endgroup
  `endif

  // --------------------------
  // add constraint based on database legal values
  // --------------------------
  function void build();
    SOFT_STRAP_DW3 = new("SOFT_STRAP_DW3", "RO/V", 32, 0, {"PMU_STRAP_3.SOFT_STRAP_DW3"});
    SOFT_STRAP_DW3.set_powerwell("primary");
    SOFT_STRAP_DW3.set_rand_mode(0);
    void'(add_field( SOFT_STRAP_DW3 ));

  endfunction


`DEFINE_NONE_SCOPE_REG_API(pmu_mmr_PMU_STRAP_3_reg) 
endclass : pmu_mmr_PMU_STRAP_3_reg

// ================================================

class pmu_mmr_PMU_SAI_READ_POLICY0_HI_reg extends sla_ral_reg;

  // --------------------------
  sla_ral_field read_policy;

  // --------------------------
  `ovm_object_utils(pmu_mmr_PMU_SAI_READ_POLICY0_HI_reg)

  // --------------------------
  function new(
                string         name = "",
                `ifdef SLA_RAL_COVERAGE
                sla_ral_coverage_t cov_t = COVERAGE_OFF,
                `endif
                int            bus_num=0,
                int            dev_num=0,
                int            func_num=0,
                int            offset=0,
                int            size=0,
                sla_ral_data_t reset_val=0,
                boolean_t      mon_enabled = SLA_FALSE
             );
      super.new();
      set_space_addr("CFG",undef);
      set_space_addr("MEM",undef);
      set_space_addr("IO",undef);
      set_space_addr("MSG",undef);
      set_space_addr("CREG",undef); 
      set_space_addr("LT_MEM",undef);    
 build();

      `ifdef SLA_RAL_COVERAGE
      `CONFIG_RAL_COVERAGE(regname, COVERAGE_ON, FD_RW_SAMPLE)
      if(_coverage_type != COVERAGE_OFF) begin
        if(_sample_type inside {FD_RO_SAMPLE, RO_SAMPLE})
        `CREATE_OP_SAMPLE_COV(RAL_WRITE)
        else if(_sample_type inside {FD_WO_SAMPLE, WO_SAMPLE})
        `CREATE_OP_SAMPLE_COV(RAL_READ)
        else
        `CREATE_OP_SAMPLE_COV(RAL_NONE)
        `CREATE_DESIRED_COV
        `CREATE_ACTUAL_COV
      end
      `endif

  endfunction

  `ifdef SLA_RAL_COVERAGE
  `CREATE_RAL_OP_COVERGROUP
  covergroup desired_cg @(cov_ev);
     option.per_instance = 1;
     option.name = {sla_ral_file::regfilename,".",regname};
     `RAL_FIELD_CP(read_policy, read_policy.desired)
     `RAL_FIELD_CP_16(read_policy, read_policy.desired, 0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)
     `RAL_FIELD_CP_16(read_policy, read_policy.desired, 16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31)
  endgroup
  covergroup actual_cg @(cov_ev);
     option.per_instance = 1;
     option.name = {sla_ral_file::regfilename,".",regname};
     `RAL_FIELD_CP(read_policy, read_policy.actual)
     `RAL_FIELD_CP_16(read_policy, read_policy.actual, 0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)
     `RAL_FIELD_CP_16(read_policy, read_policy.actual, 16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31)
  endgroup
  `endif

  // --------------------------
  // add constraint based on database legal values
  // --------------------------
  function void build();
    read_policy = new("read_policy", "RW", 32, 0, {"PMU_SAI_READ_POLICY0_HI.read_policy"});
    read_policy.set_rand_mode(0);
   read_policy.set_reset_signame("pmu_rst_b");
    void'(add_field( read_policy ));

  endfunction


`DEFINE_NONE_SCOPE_REG_API(pmu_mmr_PMU_SAI_READ_POLICY0_HI_reg) 
endclass : pmu_mmr_PMU_SAI_READ_POLICY0_HI_reg

// ================================================

class pmu_mmr_PMU_SAI_READ_POLICY0_LO_reg extends sla_ral_reg;

  // --------------------------
  sla_ral_field read_policy;

  // --------------------------
  `ovm_object_utils(pmu_mmr_PMU_SAI_READ_POLICY0_LO_reg)

  // --------------------------
  function new(
                string         name = "",
                `ifdef SLA_RAL_COVERAGE
                sla_ral_coverage_t cov_t = COVERAGE_OFF,
                `endif
                int            bus_num=0,
                int            dev_num=0,
                int            func_num=0,
                int            offset=0,
                int            size=0,
                sla_ral_data_t reset_val=0,
                boolean_t      mon_enabled = SLA_FALSE
             );
      super.new();
      set_space_addr("CFG",undef);
      set_space_addr("MEM",undef);
      set_space_addr("IO",undef);
      set_space_addr("MSG",undef);
      set_space_addr("CREG",undef); 
      set_space_addr("LT_MEM",undef);    
 build();

      `ifdef SLA_RAL_COVERAGE
      `CONFIG_RAL_COVERAGE(regname, COVERAGE_ON, FD_RW_SAMPLE)
      if(_coverage_type != COVERAGE_OFF) begin
        if(_sample_type inside {FD_RO_SAMPLE, RO_SAMPLE})
        `CREATE_OP_SAMPLE_COV(RAL_WRITE)
        else if(_sample_type inside {FD_WO_SAMPLE, WO_SAMPLE})
        `CREATE_OP_SAMPLE_COV(RAL_READ)
        else
        `CREATE_OP_SAMPLE_COV(RAL_NONE)
        `CREATE_DESIRED_COV
        `CREATE_ACTUAL_COV
      end
      `endif

  endfunction

  `ifdef SLA_RAL_COVERAGE
  `CREATE_RAL_OP_COVERGROUP
  covergroup desired_cg @(cov_ev);
     option.per_instance = 1;
     option.name = {sla_ral_file::regfilename,".",regname};
     `RAL_FIELD_CP(read_policy, read_policy.desired)
     `RAL_FIELD_CP_16(read_policy, read_policy.desired, 0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)
     `RAL_FIELD_CP_16(read_policy, read_policy.desired, 16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31)
  endgroup
  covergroup actual_cg @(cov_ev);
     option.per_instance = 1;
     option.name = {sla_ral_file::regfilename,".",regname};
     `RAL_FIELD_CP(read_policy, read_policy.actual)
     `RAL_FIELD_CP_16(read_policy, read_policy.actual, 0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)
     `RAL_FIELD_CP_16(read_policy, read_policy.actual, 16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31)
  endgroup
  `endif

  // --------------------------
  // add constraint based on database legal values
  // --------------------------
  function void build();
    read_policy = new("read_policy", "RW", 32, 0, {"PMU_SAI_READ_POLICY0_LO.read_policy"});
    read_policy.set_rand_mode(0);
   read_policy.set_reset_signame("pmu_rst_b");
    void'(add_field( read_policy ));

  endfunction


`DEFINE_NONE_SCOPE_REG_API(pmu_mmr_PMU_SAI_READ_POLICY0_LO_reg) 
endclass : pmu_mmr_PMU_SAI_READ_POLICY0_LO_reg

// ================================================

class pmu_mmr_PMU_SAI_READ_POLICY1_HI_reg extends sla_ral_reg;

  // --------------------------
  sla_ral_field read_policy;

  // --------------------------
  `ovm_object_utils(pmu_mmr_PMU_SAI_READ_POLICY1_HI_reg)

  // --------------------------
  function new(
                string         name = "",
                `ifdef SLA_RAL_COVERAGE
                sla_ral_coverage_t cov_t = COVERAGE_OFF,
                `endif
                int            bus_num=0,
                int            dev_num=0,
                int            func_num=0,
                int            offset=0,
                int            size=0,
                sla_ral_data_t reset_val=0,
                boolean_t      mon_enabled = SLA_FALSE
             );
      super.new();
      set_space_addr("CFG",undef);
      set_space_addr("MEM",undef);
      set_space_addr("IO",undef);
      set_space_addr("MSG",undef);
      set_space_addr("CREG",undef); 
      set_space_addr("LT_MEM",undef);    
 build();

      `ifdef SLA_RAL_COVERAGE
      `CONFIG_RAL_COVERAGE(regname, COVERAGE_ON, FD_RW_SAMPLE)
      if(_coverage_type != COVERAGE_OFF) begin
        if(_sample_type inside {FD_RO_SAMPLE, RO_SAMPLE})
        `CREATE_OP_SAMPLE_COV(RAL_WRITE)
        else if(_sample_type inside {FD_WO_SAMPLE, WO_SAMPLE})
        `CREATE_OP_SAMPLE_COV(RAL_READ)
        else
        `CREATE_OP_SAMPLE_COV(RAL_NONE)
        `CREATE_DESIRED_COV
        `CREATE_ACTUAL_COV
      end
      `endif

  endfunction

  `ifdef SLA_RAL_COVERAGE
  `CREATE_RAL_OP_COVERGROUP
  covergroup desired_cg @(cov_ev);
     option.per_instance = 1;
     option.name = {sla_ral_file::regfilename,".",regname};
     `RAL_FIELD_CP(read_policy, read_policy.desired)
     `RAL_FIELD_CP_16(read_policy, read_policy.desired, 0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)
     `RAL_FIELD_CP_16(read_policy, read_policy.desired, 16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31)
  endgroup
  covergroup actual_cg @(cov_ev);
     option.per_instance = 1;
     option.name = {sla_ral_file::regfilename,".",regname};
     `RAL_FIELD_CP(read_policy, read_policy.actual)
     `RAL_FIELD_CP_16(read_policy, read_policy.actual, 0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)
     `RAL_FIELD_CP_16(read_policy, read_policy.actual, 16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31)
  endgroup
  `endif

  // --------------------------
  // add constraint based on database legal values
  // --------------------------
  function void build();
    read_policy = new("read_policy", "RW", 32, 0, {"PMU_SAI_READ_POLICY1_HI.read_policy"});
    read_policy.set_rand_mode(0);
   read_policy.set_reset_signame("pmu_rst_b");
    void'(add_field( read_policy ));

  endfunction


`DEFINE_NONE_SCOPE_REG_API(pmu_mmr_PMU_SAI_READ_POLICY1_HI_reg) 
endclass : pmu_mmr_PMU_SAI_READ_POLICY1_HI_reg

// ================================================

class pmu_mmr_PMU_SAI_READ_POLICY1_LO_reg extends sla_ral_reg;

  // --------------------------
  sla_ral_field read_policy;

  // --------------------------
  `ovm_object_utils(pmu_mmr_PMU_SAI_READ_POLICY1_LO_reg)

  // --------------------------
  function new(
                string         name = "",
                `ifdef SLA_RAL_COVERAGE
                sla_ral_coverage_t cov_t = COVERAGE_OFF,
                `endif
                int            bus_num=0,
                int            dev_num=0,
                int            func_num=0,
                int            offset=0,
                int            size=0,
                sla_ral_data_t reset_val=0,
                boolean_t      mon_enabled = SLA_FALSE
             );
      super.new();
      set_space_addr("CFG",undef);
      set_space_addr("MEM",undef);
      set_space_addr("IO",undef);
      set_space_addr("MSG",undef);
      set_space_addr("CREG",undef); 
      set_space_addr("LT_MEM",undef);    
 build();

      `ifdef SLA_RAL_COVERAGE
      `CONFIG_RAL_COVERAGE(regname, COVERAGE_ON, FD_RW_SAMPLE)
      if(_coverage_type != COVERAGE_OFF) begin
        if(_sample_type inside {FD_RO_SAMPLE, RO_SAMPLE})
        `CREATE_OP_SAMPLE_COV(RAL_WRITE)
        else if(_sample_type inside {FD_WO_SAMPLE, WO_SAMPLE})
        `CREATE_OP_SAMPLE_COV(RAL_READ)
        else
        `CREATE_OP_SAMPLE_COV(RAL_NONE)
        `CREATE_DESIRED_COV
        `CREATE_ACTUAL_COV
      end
      `endif

  endfunction

  `ifdef SLA_RAL_COVERAGE
  `CREATE_RAL_OP_COVERGROUP
  covergroup desired_cg @(cov_ev);
     option.per_instance = 1;
     option.name = {sla_ral_file::regfilename,".",regname};
     `RAL_FIELD_CP(read_policy, read_policy.desired)
     `RAL_FIELD_CP_16(read_policy, read_policy.desired, 0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)
     `RAL_FIELD_CP_16(read_policy, read_policy.desired, 16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31)
  endgroup
  covergroup actual_cg @(cov_ev);
     option.per_instance = 1;
     option.name = {sla_ral_file::regfilename,".",regname};
     `RAL_FIELD_CP(read_policy, read_policy.actual)
     `RAL_FIELD_CP_16(read_policy, read_policy.actual, 0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)
     `RAL_FIELD_CP_16(read_policy, read_policy.actual, 16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31)
  endgroup
  `endif

  // --------------------------
  // add constraint based on database legal values
  // --------------------------
  function void build();
    read_policy = new("read_policy", "RW", 32, 0, {"PMU_SAI_READ_POLICY1_LO.read_policy"});
    read_policy.set_rand_mode(0);
   read_policy.set_reset_signame("pmu_rst_b");
    void'(add_field( read_policy ));

  endfunction


`DEFINE_NONE_SCOPE_REG_API(pmu_mmr_PMU_SAI_READ_POLICY1_LO_reg) 
endclass : pmu_mmr_PMU_SAI_READ_POLICY1_LO_reg

// ================================================

class pmu_mmr_PMU_SAI_READ_POLICY2_HI_reg extends sla_ral_reg;

  // --------------------------
  sla_ral_field read_policy;

  // --------------------------
  `ovm_object_utils(pmu_mmr_PMU_SAI_READ_POLICY2_HI_reg)

  // --------------------------
  function new(
                string         name = "",
                `ifdef SLA_RAL_COVERAGE
                sla_ral_coverage_t cov_t = COVERAGE_OFF,
                `endif
                int            bus_num=0,
                int            dev_num=0,
                int            func_num=0,
                int            offset=0,
                int            size=0,
                sla_ral_data_t reset_val=0,
                boolean_t      mon_enabled = SLA_FALSE
             );
      super.new();
      set_space_addr("CFG",undef);
      set_space_addr("MEM",undef);
      set_space_addr("IO",undef);
      set_space_addr("MSG",undef);
      set_space_addr("CREG",undef); 
      set_space_addr("LT_MEM",undef);    
 build();

      `ifdef SLA_RAL_COVERAGE
      `CONFIG_RAL_COVERAGE(regname, COVERAGE_ON, FD_RW_SAMPLE)
      if(_coverage_type != COVERAGE_OFF) begin
        if(_sample_type inside {FD_RO_SAMPLE, RO_SAMPLE})
        `CREATE_OP_SAMPLE_COV(RAL_WRITE)
        else if(_sample_type inside {FD_WO_SAMPLE, WO_SAMPLE})
        `CREATE_OP_SAMPLE_COV(RAL_READ)
        else
        `CREATE_OP_SAMPLE_COV(RAL_NONE)
        `CREATE_DESIRED_COV
        `CREATE_ACTUAL_COV
      end
      `endif

  endfunction

  `ifdef SLA_RAL_COVERAGE
  `CREATE_RAL_OP_COVERGROUP
  covergroup desired_cg @(cov_ev);
     option.per_instance = 1;
     option.name = {sla_ral_file::regfilename,".",regname};
     `RAL_FIELD_CP(read_policy, read_policy.desired)
     `RAL_FIELD_CP_16(read_policy, read_policy.desired, 0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)
     `RAL_FIELD_CP_16(read_policy, read_policy.desired, 16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31)
  endgroup
  covergroup actual_cg @(cov_ev);
     option.per_instance = 1;
     option.name = {sla_ral_file::regfilename,".",regname};
     `RAL_FIELD_CP(read_policy, read_policy.actual)
     `RAL_FIELD_CP_16(read_policy, read_policy.actual, 0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)
     `RAL_FIELD_CP_16(read_policy, read_policy.actual, 16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31)
  endgroup
  `endif

  // --------------------------
  // add constraint based on database legal values
  // --------------------------
  function void build();
    read_policy = new("read_policy", "RW", 32, 0, {"PMU_SAI_READ_POLICY2_HI.read_policy"});
    read_policy.set_rand_mode(0);
   read_policy.set_reset_signame("pmu_rst_b");
    void'(add_field( read_policy ));

  endfunction


`DEFINE_NONE_SCOPE_REG_API(pmu_mmr_PMU_SAI_READ_POLICY2_HI_reg) 
endclass : pmu_mmr_PMU_SAI_READ_POLICY2_HI_reg

// ================================================

class pmu_mmr_PMU_SAI_READ_POLICY2_LO_reg extends sla_ral_reg;

  // --------------------------
  sla_ral_field read_policy;

  // --------------------------
  `ovm_object_utils(pmu_mmr_PMU_SAI_READ_POLICY2_LO_reg)

  // --------------------------
  function new(
                string         name = "",
                `ifdef SLA_RAL_COVERAGE
                sla_ral_coverage_t cov_t = COVERAGE_OFF,
                `endif
                int            bus_num=0,
                int            dev_num=0,
                int            func_num=0,
                int            offset=0,
                int            size=0,
                sla_ral_data_t reset_val=0,
                boolean_t      mon_enabled = SLA_FALSE
             );
      super.new();
      set_space_addr("CFG",undef);
      set_space_addr("MEM",undef);
      set_space_addr("IO",undef);
      set_space_addr("MSG",undef);
      set_space_addr("CREG",undef); 
      set_space_addr("LT_MEM",undef);    
 build();

      `ifdef SLA_RAL_COVERAGE
      `CONFIG_RAL_COVERAGE(regname, COVERAGE_ON, FD_RW_SAMPLE)
      if(_coverage_type != COVERAGE_OFF) begin
        if(_sample_type inside {FD_RO_SAMPLE, RO_SAMPLE})
        `CREATE_OP_SAMPLE_COV(RAL_WRITE)
        else if(_sample_type inside {FD_WO_SAMPLE, WO_SAMPLE})
        `CREATE_OP_SAMPLE_COV(RAL_READ)
        else
        `CREATE_OP_SAMPLE_COV(RAL_NONE)
        `CREATE_DESIRED_COV
        `CREATE_ACTUAL_COV
      end
      `endif

  endfunction

  `ifdef SLA_RAL_COVERAGE
  `CREATE_RAL_OP_COVERGROUP
  covergroup desired_cg @(cov_ev);
     option.per_instance = 1;
     option.name = {sla_ral_file::regfilename,".",regname};
     `RAL_FIELD_CP(read_policy, read_policy.desired)
     `RAL_FIELD_CP_16(read_policy, read_policy.desired, 0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)
     `RAL_FIELD_CP_16(read_policy, read_policy.desired, 16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31)
  endgroup
  covergroup actual_cg @(cov_ev);
     option.per_instance = 1;
     option.name = {sla_ral_file::regfilename,".",regname};
     `RAL_FIELD_CP(read_policy, read_policy.actual)
     `RAL_FIELD_CP_16(read_policy, read_policy.actual, 0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)
     `RAL_FIELD_CP_16(read_policy, read_policy.actual, 16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31)
  endgroup
  `endif

  // --------------------------
  // add constraint based on database legal values
  // --------------------------
  function void build();
    read_policy = new("read_policy", "RW", 32, 0, {"PMU_SAI_READ_POLICY2_LO.read_policy"});
    read_policy.set_rand_mode(0);
   read_policy.set_reset_signame("pmu_rst_b");
    void'(add_field( read_policy ));

  endfunction


`DEFINE_NONE_SCOPE_REG_API(pmu_mmr_PMU_SAI_READ_POLICY2_LO_reg) 
endclass : pmu_mmr_PMU_SAI_READ_POLICY2_LO_reg

// ================================================

class pmu_mmr_PMU_SAI_WRITE_POLICY0_HI_reg extends sla_ral_reg;

  // --------------------------
  sla_ral_field write_policy;

  // --------------------------
  `ovm_object_utils(pmu_mmr_PMU_SAI_WRITE_POLICY0_HI_reg)

  // --------------------------
  function new(
                string         name = "",
                `ifdef SLA_RAL_COVERAGE
                sla_ral_coverage_t cov_t = COVERAGE_OFF,
                `endif
                int            bus_num=0,
                int            dev_num=0,
                int            func_num=0,
                int            offset=0,
                int            size=0,
                sla_ral_data_t reset_val=0,
                boolean_t      mon_enabled = SLA_FALSE
             );
      super.new();
      set_space_addr("CFG",undef);
      set_space_addr("MEM",undef);
      set_space_addr("IO",undef);
      set_space_addr("MSG",undef);
      set_space_addr("CREG",undef); 
      set_space_addr("LT_MEM",undef);    
 build();

      `ifdef SLA_RAL_COVERAGE
      `CONFIG_RAL_COVERAGE(regname, COVERAGE_ON, FD_RW_SAMPLE)
      if(_coverage_type != COVERAGE_OFF) begin
        if(_sample_type inside {FD_RO_SAMPLE, RO_SAMPLE})
        `CREATE_OP_SAMPLE_COV(RAL_WRITE)
        else if(_sample_type inside {FD_WO_SAMPLE, WO_SAMPLE})
        `CREATE_OP_SAMPLE_COV(RAL_READ)
        else
        `CREATE_OP_SAMPLE_COV(RAL_NONE)
        `CREATE_DESIRED_COV
        `CREATE_ACTUAL_COV
      end
      `endif

  endfunction

  `ifdef SLA_RAL_COVERAGE
  `CREATE_RAL_OP_COVERGROUP
  covergroup desired_cg @(cov_ev);
     option.per_instance = 1;
     option.name = {sla_ral_file::regfilename,".",regname};
     `RAL_FIELD_CP(write_policy, write_policy.desired)
     `RAL_FIELD_CP_16(write_policy, write_policy.desired, 0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)
     `RAL_FIELD_CP_16(write_policy, write_policy.desired, 16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31)
  endgroup
  covergroup actual_cg @(cov_ev);
     option.per_instance = 1;
     option.name = {sla_ral_file::regfilename,".",regname};
     `RAL_FIELD_CP(write_policy, write_policy.actual)
     `RAL_FIELD_CP_16(write_policy, write_policy.actual, 0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)
     `RAL_FIELD_CP_16(write_policy, write_policy.actual, 16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31)
  endgroup
  `endif

  // --------------------------
  // add constraint based on database legal values
  // --------------------------
  function void build();
    write_policy = new("write_policy", "RW", 32, 0, {"PMU_SAI_WRITE_POLICY0_HI.write_policy"});
    write_policy.set_rand_mode(0);
   write_policy.set_reset_signame("pmu_rst_b");
    void'(add_field( write_policy ));

  endfunction


`DEFINE_NONE_SCOPE_REG_API(pmu_mmr_PMU_SAI_WRITE_POLICY0_HI_reg) 
endclass : pmu_mmr_PMU_SAI_WRITE_POLICY0_HI_reg

// ================================================

class pmu_mmr_PMU_SAI_WRITE_POLICY0_LO_reg extends sla_ral_reg;

  // --------------------------
  sla_ral_field write_policy;

  // --------------------------
  `ovm_object_utils(pmu_mmr_PMU_SAI_WRITE_POLICY0_LO_reg)

  // --------------------------
  function new(
                string         name = "",
                `ifdef SLA_RAL_COVERAGE
                sla_ral_coverage_t cov_t = COVERAGE_OFF,
                `endif
                int            bus_num=0,
                int            dev_num=0,
                int            func_num=0,
                int            offset=0,
                int            size=0,
                sla_ral_data_t reset_val=0,
                boolean_t      mon_enabled = SLA_FALSE
             );
      super.new();
      set_space_addr("CFG",undef);
      set_space_addr("MEM",undef);
      set_space_addr("IO",undef);
      set_space_addr("MSG",undef);
      set_space_addr("CREG",undef); 
      set_space_addr("LT_MEM",undef);    
 build();

      `ifdef SLA_RAL_COVERAGE
      `CONFIG_RAL_COVERAGE(regname, COVERAGE_ON, FD_RW_SAMPLE)
      if(_coverage_type != COVERAGE_OFF) begin
        if(_sample_type inside {FD_RO_SAMPLE, RO_SAMPLE})
        `CREATE_OP_SAMPLE_COV(RAL_WRITE)
        else if(_sample_type inside {FD_WO_SAMPLE, WO_SAMPLE})
        `CREATE_OP_SAMPLE_COV(RAL_READ)
        else
        `CREATE_OP_SAMPLE_COV(RAL_NONE)
        `CREATE_DESIRED_COV
        `CREATE_ACTUAL_COV
      end
      `endif

  endfunction

  `ifdef SLA_RAL_COVERAGE
  `CREATE_RAL_OP_COVERGROUP
  covergroup desired_cg @(cov_ev);
     option.per_instance = 1;
     option.name = {sla_ral_file::regfilename,".",regname};
     `RAL_FIELD_CP(write_policy, write_policy.desired)
     `RAL_FIELD_CP_16(write_policy, write_policy.desired, 0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)
     `RAL_FIELD_CP_16(write_policy, write_policy.desired, 16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31)
  endgroup
  covergroup actual_cg @(cov_ev);
     option.per_instance = 1;
     option.name = {sla_ral_file::regfilename,".",regname};
     `RAL_FIELD_CP(write_policy, write_policy.actual)
     `RAL_FIELD_CP_16(write_policy, write_policy.actual, 0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)
     `RAL_FIELD_CP_16(write_policy, write_policy.actual, 16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31)
  endgroup
  `endif

  // --------------------------
  // add constraint based on database legal values
  // --------------------------
  function void build();
    write_policy = new("write_policy", "RW", 32, 0, {"PMU_SAI_WRITE_POLICY0_LO.write_policy"});
    write_policy.set_rand_mode(0);
   write_policy.set_reset_signame("pmu_rst_b");
    void'(add_field( write_policy ));

  endfunction


`DEFINE_NONE_SCOPE_REG_API(pmu_mmr_PMU_SAI_WRITE_POLICY0_LO_reg) 
endclass : pmu_mmr_PMU_SAI_WRITE_POLICY0_LO_reg

// ================================================

class pmu_mmr_PMU_SAI_WRITE_POLICY1_HI_reg extends sla_ral_reg;

  // --------------------------
  sla_ral_field write_policy;

  // --------------------------
  `ovm_object_utils(pmu_mmr_PMU_SAI_WRITE_POLICY1_HI_reg)

  // --------------------------
  function new(
                string         name = "",
                `ifdef SLA_RAL_COVERAGE
                sla_ral_coverage_t cov_t = COVERAGE_OFF,
                `endif
                int            bus_num=0,
                int            dev_num=0,
                int            func_num=0,
                int            offset=0,
                int            size=0,
                sla_ral_data_t reset_val=0,
                boolean_t      mon_enabled = SLA_FALSE
             );
      super.new();
      set_space_addr("CFG",undef);
      set_space_addr("MEM",undef);
      set_space_addr("IO",undef);
      set_space_addr("MSG",undef);
      set_space_addr("CREG",undef); 
      set_space_addr("LT_MEM",undef);    
 build();

      `ifdef SLA_RAL_COVERAGE
      `CONFIG_RAL_COVERAGE(regname, COVERAGE_ON, FD_RW_SAMPLE)
      if(_coverage_type != COVERAGE_OFF) begin
        if(_sample_type inside {FD_RO_SAMPLE, RO_SAMPLE})
        `CREATE_OP_SAMPLE_COV(RAL_WRITE)
        else if(_sample_type inside {FD_WO_SAMPLE, WO_SAMPLE})
        `CREATE_OP_SAMPLE_COV(RAL_READ)
        else
        `CREATE_OP_SAMPLE_COV(RAL_NONE)
        `CREATE_DESIRED_COV
        `CREATE_ACTUAL_COV
      end
      `endif

  endfunction

  `ifdef SLA_RAL_COVERAGE
  `CREATE_RAL_OP_COVERGROUP
  covergroup desired_cg @(cov_ev);
     option.per_instance = 1;
     option.name = {sla_ral_file::regfilename,".",regname};
     `RAL_FIELD_CP(write_policy, write_policy.desired)
     `RAL_FIELD_CP_16(write_policy, write_policy.desired, 0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)
     `RAL_FIELD_CP_16(write_policy, write_policy.desired, 16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31)
  endgroup
  covergroup actual_cg @(cov_ev);
     option.per_instance = 1;
     option.name = {sla_ral_file::regfilename,".",regname};
     `RAL_FIELD_CP(write_policy, write_policy.actual)
     `RAL_FIELD_CP_16(write_policy, write_policy.actual, 0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)
     `RAL_FIELD_CP_16(write_policy, write_policy.actual, 16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31)
  endgroup
  `endif

  // --------------------------
  // add constraint based on database legal values
  // --------------------------
  function void build();
    write_policy = new("write_policy", "RW", 32, 0, {"PMU_SAI_WRITE_POLICY1_HI.write_policy"});
    write_policy.set_rand_mode(0);
   write_policy.set_reset_signame("pmu_rst_b");
    void'(add_field( write_policy ));

  endfunction


`DEFINE_NONE_SCOPE_REG_API(pmu_mmr_PMU_SAI_WRITE_POLICY1_HI_reg) 
endclass : pmu_mmr_PMU_SAI_WRITE_POLICY1_HI_reg

// ================================================

class pmu_mmr_PMU_SAI_WRITE_POLICY1_LO_reg extends sla_ral_reg;

  // --------------------------
  sla_ral_field write_policy;

  // --------------------------
  `ovm_object_utils(pmu_mmr_PMU_SAI_WRITE_POLICY1_LO_reg)

  // --------------------------
  function new(
                string         name = "",
                `ifdef SLA_RAL_COVERAGE
                sla_ral_coverage_t cov_t = COVERAGE_OFF,
                `endif
                int            bus_num=0,
                int            dev_num=0,
                int            func_num=0,
                int            offset=0,
                int            size=0,
                sla_ral_data_t reset_val=0,
                boolean_t      mon_enabled = SLA_FALSE
             );
      super.new();
      set_space_addr("CFG",undef);
      set_space_addr("MEM",undef);
      set_space_addr("IO",undef);
      set_space_addr("MSG",undef);
      set_space_addr("CREG",undef); 
      set_space_addr("LT_MEM",undef);    
 build();

      `ifdef SLA_RAL_COVERAGE
      `CONFIG_RAL_COVERAGE(regname, COVERAGE_ON, FD_RW_SAMPLE)
      if(_coverage_type != COVERAGE_OFF) begin
        if(_sample_type inside {FD_RO_SAMPLE, RO_SAMPLE})
        `CREATE_OP_SAMPLE_COV(RAL_WRITE)
        else if(_sample_type inside {FD_WO_SAMPLE, WO_SAMPLE})
        `CREATE_OP_SAMPLE_COV(RAL_READ)
        else
        `CREATE_OP_SAMPLE_COV(RAL_NONE)
        `CREATE_DESIRED_COV
        `CREATE_ACTUAL_COV
      end
      `endif

  endfunction

  `ifdef SLA_RAL_COVERAGE
  `CREATE_RAL_OP_COVERGROUP
  covergroup desired_cg @(cov_ev);
     option.per_instance = 1;
     option.name = {sla_ral_file::regfilename,".",regname};
     `RAL_FIELD_CP(write_policy, write_policy.desired)
     `RAL_FIELD_CP_16(write_policy, write_policy.desired, 0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)
     `RAL_FIELD_CP_16(write_policy, write_policy.desired, 16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31)
  endgroup
  covergroup actual_cg @(cov_ev);
     option.per_instance = 1;
     option.name = {sla_ral_file::regfilename,".",regname};
     `RAL_FIELD_CP(write_policy, write_policy.actual)
     `RAL_FIELD_CP_16(write_policy, write_policy.actual, 0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)
     `RAL_FIELD_CP_16(write_policy, write_policy.actual, 16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31)
  endgroup
  `endif

  // --------------------------
  // add constraint based on database legal values
  // --------------------------
  function void build();
    write_policy = new("write_policy", "RW", 32, 0, {"PMU_SAI_WRITE_POLICY1_LO.write_policy"});
    write_policy.set_rand_mode(0);
   write_policy.set_reset_signame("pmu_rst_b");
    void'(add_field( write_policy ));

  endfunction


`DEFINE_NONE_SCOPE_REG_API(pmu_mmr_PMU_SAI_WRITE_POLICY1_LO_reg) 
endclass : pmu_mmr_PMU_SAI_WRITE_POLICY1_LO_reg

// ================================================

class pmu_mmr_PMU_SAI_WRITE_POLICY2_HI_reg extends sla_ral_reg;

  // --------------------------
  sla_ral_field write_policy;

  // --------------------------
  `ovm_object_utils(pmu_mmr_PMU_SAI_WRITE_POLICY2_HI_reg)

  // --------------------------
  function new(
                string         name = "",
                `ifdef SLA_RAL_COVERAGE
                sla_ral_coverage_t cov_t = COVERAGE_OFF,
                `endif
                int            bus_num=0,
                int            dev_num=0,
                int            func_num=0,
                int            offset=0,
                int            size=0,
                sla_ral_data_t reset_val=0,
                boolean_t      mon_enabled = SLA_FALSE
             );
      super.new();
      set_space_addr("CFG",undef);
      set_space_addr("MEM",undef);
      set_space_addr("IO",undef);
      set_space_addr("MSG",undef);
      set_space_addr("CREG",undef); 
      set_space_addr("LT_MEM",undef);    
 build();

      `ifdef SLA_RAL_COVERAGE
      `CONFIG_RAL_COVERAGE(regname, COVERAGE_ON, FD_RW_SAMPLE)
      if(_coverage_type != COVERAGE_OFF) begin
        if(_sample_type inside {FD_RO_SAMPLE, RO_SAMPLE})
        `CREATE_OP_SAMPLE_COV(RAL_WRITE)
        else if(_sample_type inside {FD_WO_SAMPLE, WO_SAMPLE})
        `CREATE_OP_SAMPLE_COV(RAL_READ)
        else
        `CREATE_OP_SAMPLE_COV(RAL_NONE)
        `CREATE_DESIRED_COV
        `CREATE_ACTUAL_COV
      end
      `endif

  endfunction

  `ifdef SLA_RAL_COVERAGE
  `CREATE_RAL_OP_COVERGROUP
  covergroup desired_cg @(cov_ev);
     option.per_instance = 1;
     option.name = {sla_ral_file::regfilename,".",regname};
     `RAL_FIELD_CP(write_policy, write_policy.desired)
     `RAL_FIELD_CP_16(write_policy, write_policy.desired, 0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)
     `RAL_FIELD_CP_16(write_policy, write_policy.desired, 16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31)
  endgroup
  covergroup actual_cg @(cov_ev);
     option.per_instance = 1;
     option.name = {sla_ral_file::regfilename,".",regname};
     `RAL_FIELD_CP(write_policy, write_policy.actual)
     `RAL_FIELD_CP_16(write_policy, write_policy.actual, 0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)
     `RAL_FIELD_CP_16(write_policy, write_policy.actual, 16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31)
  endgroup
  `endif

  // --------------------------
  // add constraint based on database legal values
  // --------------------------
  function void build();
    write_policy = new("write_policy", "RW", 32, 0, {"PMU_SAI_WRITE_POLICY2_HI.write_policy"});
    write_policy.set_rand_mode(0);
   write_policy.set_reset_signame("pmu_rst_b");
    void'(add_field( write_policy ));

  endfunction


`DEFINE_NONE_SCOPE_REG_API(pmu_mmr_PMU_SAI_WRITE_POLICY2_HI_reg) 
endclass : pmu_mmr_PMU_SAI_WRITE_POLICY2_HI_reg

// ================================================

class pmu_mmr_PMU_SAI_WRITE_POLICY2_LO_reg extends sla_ral_reg;

  // --------------------------
  sla_ral_field write_policy;

  // --------------------------
  `ovm_object_utils(pmu_mmr_PMU_SAI_WRITE_POLICY2_LO_reg)

  // --------------------------
  function new(
                string         name = "",
                `ifdef SLA_RAL_COVERAGE
                sla_ral_coverage_t cov_t = COVERAGE_OFF,
                `endif
                int            bus_num=0,
                int            dev_num=0,
                int            func_num=0,
                int            offset=0,
                int            size=0,
                sla_ral_data_t reset_val=0,
                boolean_t      mon_enabled = SLA_FALSE
             );
      super.new();
      set_space_addr("CFG",undef);
      set_space_addr("MEM",undef);
      set_space_addr("IO",undef);
      set_space_addr("MSG",undef);
      set_space_addr("CREG",undef); 
      set_space_addr("LT_MEM",undef);    
 build();

      `ifdef SLA_RAL_COVERAGE
      `CONFIG_RAL_COVERAGE(regname, COVERAGE_ON, FD_RW_SAMPLE)
      if(_coverage_type != COVERAGE_OFF) begin
        if(_sample_type inside {FD_RO_SAMPLE, RO_SAMPLE})
        `CREATE_OP_SAMPLE_COV(RAL_WRITE)
        else if(_sample_type inside {FD_WO_SAMPLE, WO_SAMPLE})
        `CREATE_OP_SAMPLE_COV(RAL_READ)
        else
        `CREATE_OP_SAMPLE_COV(RAL_NONE)
        `CREATE_DESIRED_COV
        `CREATE_ACTUAL_COV
      end
      `endif

  endfunction

  `ifdef SLA_RAL_COVERAGE
  `CREATE_RAL_OP_COVERGROUP
  covergroup desired_cg @(cov_ev);
     option.per_instance = 1;
     option.name = {sla_ral_file::regfilename,".",regname};
     `RAL_FIELD_CP(write_policy, write_policy.desired)
     `RAL_FIELD_CP_16(write_policy, write_policy.desired, 0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)
     `RAL_FIELD_CP_16(write_policy, write_policy.desired, 16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31)
  endgroup
  covergroup actual_cg @(cov_ev);
     option.per_instance = 1;
     option.name = {sla_ral_file::regfilename,".",regname};
     `RAL_FIELD_CP(write_policy, write_policy.actual)
     `RAL_FIELD_CP_16(write_policy, write_policy.actual, 0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)
     `RAL_FIELD_CP_16(write_policy, write_policy.actual, 16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31)
  endgroup
  `endif

  // --------------------------
  // add constraint based on database legal values
  // --------------------------
  function void build();
    write_policy = new("write_policy", "RW", 32, 0, {"PMU_SAI_WRITE_POLICY2_LO.write_policy"});
    write_policy.set_rand_mode(0);
   write_policy.set_reset_signame("pmu_rst_b");
    void'(add_field( write_policy ));

  endfunction


`DEFINE_NONE_SCOPE_REG_API(pmu_mmr_PMU_SAI_WRITE_POLICY2_LO_reg) 
endclass : pmu_mmr_PMU_SAI_WRITE_POLICY2_LO_reg

// ================================================

class pmu_mmr_PMU_SAI_CONTROL_POLICY0_HI_reg extends sla_ral_reg;

  // --------------------------
  sla_ral_field cp_policy;

  // --------------------------
  `ovm_object_utils(pmu_mmr_PMU_SAI_CONTROL_POLICY0_HI_reg)

  // --------------------------
  function new(
                string         name = "",
                `ifdef SLA_RAL_COVERAGE
                sla_ral_coverage_t cov_t = COVERAGE_OFF,
                `endif
                int            bus_num=0,
                int            dev_num=0,
                int            func_num=0,
                int            offset=0,
                int            size=0,
                sla_ral_data_t reset_val=0,
                boolean_t      mon_enabled = SLA_FALSE
             );
      super.new();
      set_space_addr("CFG",undef);
      set_space_addr("MEM",undef);
      set_space_addr("IO",undef);
      set_space_addr("MSG",undef);
      set_space_addr("CREG",undef); 
      set_space_addr("LT_MEM",undef);    
 build();

      `ifdef SLA_RAL_COVERAGE
      `CONFIG_RAL_COVERAGE(regname, COVERAGE_ON, FD_RW_SAMPLE)
      if(_coverage_type != COVERAGE_OFF) begin
        if(_sample_type inside {FD_RO_SAMPLE, RO_SAMPLE})
        `CREATE_OP_SAMPLE_COV(RAL_WRITE)
        else if(_sample_type inside {FD_WO_SAMPLE, WO_SAMPLE})
        `CREATE_OP_SAMPLE_COV(RAL_READ)
        else
        `CREATE_OP_SAMPLE_COV(RAL_NONE)
        `CREATE_DESIRED_COV
        `CREATE_ACTUAL_COV
      end
      `endif

  endfunction

  `ifdef SLA_RAL_COVERAGE
  `CREATE_RAL_OP_COVERGROUP
  covergroup desired_cg @(cov_ev);
     option.per_instance = 1;
     option.name = {sla_ral_file::regfilename,".",regname};
     `RAL_FIELD_CP(cp_policy, cp_policy.desired)
     `RAL_FIELD_CP_16(cp_policy, cp_policy.desired, 0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)
     `RAL_FIELD_CP_16(cp_policy, cp_policy.desired, 16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31)
  endgroup
  covergroup actual_cg @(cov_ev);
     option.per_instance = 1;
     option.name = {sla_ral_file::regfilename,".",regname};
     `RAL_FIELD_CP(cp_policy, cp_policy.actual)
     `RAL_FIELD_CP_16(cp_policy, cp_policy.actual, 0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)
     `RAL_FIELD_CP_16(cp_policy, cp_policy.actual, 16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31)
  endgroup
  `endif

  // --------------------------
  // add constraint based on database legal values
  // --------------------------
  function void build();
    cp_policy = new("cp_policy", "RW", 32, 0, {"PMU_SAI_CONTROL_POLICY0_HI.cp_policy"});
    cp_policy.set_powerwell("primary");
    cp_policy.set_rand_mode(0);
   cp_policy.set_reset_signame("pmu_rst_b");
    void'(add_field( cp_policy ));

  endfunction


`DEFINE_NONE_SCOPE_REG_API(pmu_mmr_PMU_SAI_CONTROL_POLICY0_HI_reg) 
endclass : pmu_mmr_PMU_SAI_CONTROL_POLICY0_HI_reg

// ================================================

class pmu_mmr_PMU_SAI_CONTROL_POLICY0_LO_reg extends sla_ral_reg;

  // --------------------------
  sla_ral_field cp_policy;

  // --------------------------
  `ovm_object_utils(pmu_mmr_PMU_SAI_CONTROL_POLICY0_LO_reg)

  // --------------------------
  function new(
                string         name = "",
                `ifdef SLA_RAL_COVERAGE
                sla_ral_coverage_t cov_t = COVERAGE_OFF,
                `endif
                int            bus_num=0,
                int            dev_num=0,
                int            func_num=0,
                int            offset=0,
                int            size=0,
                sla_ral_data_t reset_val=0,
                boolean_t      mon_enabled = SLA_FALSE
             );
      super.new();
      set_space_addr("CFG",undef);
      set_space_addr("MEM",undef);
      set_space_addr("IO",undef);
      set_space_addr("MSG",undef);
      set_space_addr("CREG",undef); 
      set_space_addr("LT_MEM",undef);    
 build();

      `ifdef SLA_RAL_COVERAGE
      `CONFIG_RAL_COVERAGE(regname, COVERAGE_ON, FD_RW_SAMPLE)
      if(_coverage_type != COVERAGE_OFF) begin
        if(_sample_type inside {FD_RO_SAMPLE, RO_SAMPLE})
        `CREATE_OP_SAMPLE_COV(RAL_WRITE)
        else if(_sample_type inside {FD_WO_SAMPLE, WO_SAMPLE})
        `CREATE_OP_SAMPLE_COV(RAL_READ)
        else
        `CREATE_OP_SAMPLE_COV(RAL_NONE)
        `CREATE_DESIRED_COV
        `CREATE_ACTUAL_COV
      end
      `endif

  endfunction

  `ifdef SLA_RAL_COVERAGE
  `CREATE_RAL_OP_COVERGROUP
  covergroup desired_cg @(cov_ev);
     option.per_instance = 1;
     option.name = {sla_ral_file::regfilename,".",regname};
     `RAL_FIELD_CP(cp_policy, cp_policy.desired)
     `RAL_FIELD_CP_16(cp_policy, cp_policy.desired, 0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)
     `RAL_FIELD_CP_16(cp_policy, cp_policy.desired, 16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31)
  endgroup
  covergroup actual_cg @(cov_ev);
     option.per_instance = 1;
     option.name = {sla_ral_file::regfilename,".",regname};
     `RAL_FIELD_CP(cp_policy, cp_policy.actual)
     `RAL_FIELD_CP_16(cp_policy, cp_policy.actual, 0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)
     `RAL_FIELD_CP_16(cp_policy, cp_policy.actual, 16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31)
  endgroup
  `endif

  // --------------------------
  // add constraint based on database legal values
  // --------------------------
  function void build();
    cp_policy = new("cp_policy", "RW", 32, 0, {"PMU_SAI_CONTROL_POLICY0_LO.cp_policy"});
    cp_policy.set_powerwell("primary");
    cp_policy.set_rand_mode(0);
   cp_policy.set_reset_signame("pmu_rst_b");
    void'(add_field( cp_policy ));

  endfunction


`DEFINE_NONE_SCOPE_REG_API(pmu_mmr_PMU_SAI_CONTROL_POLICY0_LO_reg) 
endclass : pmu_mmr_PMU_SAI_CONTROL_POLICY0_LO_reg

// ================================================

class pmu_mmr_PMU_SAI_CONTROL_POLICY1_HI_reg extends sla_ral_reg;

  // --------------------------
  sla_ral_field cp_policy;

  // --------------------------
  `ovm_object_utils(pmu_mmr_PMU_SAI_CONTROL_POLICY1_HI_reg)

  // --------------------------
  function new(
                string         name = "",
                `ifdef SLA_RAL_COVERAGE
                sla_ral_coverage_t cov_t = COVERAGE_OFF,
                `endif
                int            bus_num=0,
                int            dev_num=0,
                int            func_num=0,
                int            offset=0,
                int            size=0,
                sla_ral_data_t reset_val=0,
                boolean_t      mon_enabled = SLA_FALSE
             );
      super.new();
      set_space_addr("CFG",undef);
      set_space_addr("MEM",undef);
      set_space_addr("IO",undef);
      set_space_addr("MSG",undef);
      set_space_addr("CREG",undef); 
      set_space_addr("LT_MEM",undef);    
 build();

      `ifdef SLA_RAL_COVERAGE
      `CONFIG_RAL_COVERAGE(regname, COVERAGE_ON, FD_RW_SAMPLE)
      if(_coverage_type != COVERAGE_OFF) begin
        if(_sample_type inside {FD_RO_SAMPLE, RO_SAMPLE})
        `CREATE_OP_SAMPLE_COV(RAL_WRITE)
        else if(_sample_type inside {FD_WO_SAMPLE, WO_SAMPLE})
        `CREATE_OP_SAMPLE_COV(RAL_READ)
        else
        `CREATE_OP_SAMPLE_COV(RAL_NONE)
        `CREATE_DESIRED_COV
        `CREATE_ACTUAL_COV
      end
      `endif

  endfunction

  `ifdef SLA_RAL_COVERAGE
  `CREATE_RAL_OP_COVERGROUP
  covergroup desired_cg @(cov_ev);
     option.per_instance = 1;
     option.name = {sla_ral_file::regfilename,".",regname};
     `RAL_FIELD_CP(cp_policy, cp_policy.desired)
     `RAL_FIELD_CP_16(cp_policy, cp_policy.desired, 0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)
     `RAL_FIELD_CP_16(cp_policy, cp_policy.desired, 16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31)
  endgroup
  covergroup actual_cg @(cov_ev);
     option.per_instance = 1;
     option.name = {sla_ral_file::regfilename,".",regname};
     `RAL_FIELD_CP(cp_policy, cp_policy.actual)
     `RAL_FIELD_CP_16(cp_policy, cp_policy.actual, 0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)
     `RAL_FIELD_CP_16(cp_policy, cp_policy.actual, 16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31)
  endgroup
  `endif

  // --------------------------
  // add constraint based on database legal values
  // --------------------------
  function void build();
    cp_policy = new("cp_policy", "RW", 32, 0, {"PMU_SAI_CONTROL_POLICY1_HI.cp_policy"});
    cp_policy.set_powerwell("primary");
    cp_policy.set_rand_mode(0);
   cp_policy.set_reset_signame("pmu_rst_b");
    void'(add_field( cp_policy ));

  endfunction


`DEFINE_NONE_SCOPE_REG_API(pmu_mmr_PMU_SAI_CONTROL_POLICY1_HI_reg) 
endclass : pmu_mmr_PMU_SAI_CONTROL_POLICY1_HI_reg

// ================================================

class pmu_mmr_PMU_SAI_CONTROL_POLICY1_LO_reg extends sla_ral_reg;

  // --------------------------
  sla_ral_field cp_policy;

  // --------------------------
  `ovm_object_utils(pmu_mmr_PMU_SAI_CONTROL_POLICY1_LO_reg)

  // --------------------------
  function new(
                string         name = "",
                `ifdef SLA_RAL_COVERAGE
                sla_ral_coverage_t cov_t = COVERAGE_OFF,
                `endif
                int            bus_num=0,
                int            dev_num=0,
                int            func_num=0,
                int            offset=0,
                int            size=0,
                sla_ral_data_t reset_val=0,
                boolean_t      mon_enabled = SLA_FALSE
             );
      super.new();
      set_space_addr("CFG",undef);
      set_space_addr("MEM",undef);
      set_space_addr("IO",undef);
      set_space_addr("MSG",undef);
      set_space_addr("CREG",undef); 
      set_space_addr("LT_MEM",undef);    
 build();

      `ifdef SLA_RAL_COVERAGE
      `CONFIG_RAL_COVERAGE(regname, COVERAGE_ON, FD_RW_SAMPLE)
      if(_coverage_type != COVERAGE_OFF) begin
        if(_sample_type inside {FD_RO_SAMPLE, RO_SAMPLE})
        `CREATE_OP_SAMPLE_COV(RAL_WRITE)
        else if(_sample_type inside {FD_WO_SAMPLE, WO_SAMPLE})
        `CREATE_OP_SAMPLE_COV(RAL_READ)
        else
        `CREATE_OP_SAMPLE_COV(RAL_NONE)
        `CREATE_DESIRED_COV
        `CREATE_ACTUAL_COV
      end
      `endif

  endfunction

  `ifdef SLA_RAL_COVERAGE
  `CREATE_RAL_OP_COVERGROUP
  covergroup desired_cg @(cov_ev);
     option.per_instance = 1;
     option.name = {sla_ral_file::regfilename,".",regname};
     `RAL_FIELD_CP(cp_policy, cp_policy.desired)
     `RAL_FIELD_CP_16(cp_policy, cp_policy.desired, 0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)
     `RAL_FIELD_CP_16(cp_policy, cp_policy.desired, 16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31)
  endgroup
  covergroup actual_cg @(cov_ev);
     option.per_instance = 1;
     option.name = {sla_ral_file::regfilename,".",regname};
     `RAL_FIELD_CP(cp_policy, cp_policy.actual)
     `RAL_FIELD_CP_16(cp_policy, cp_policy.actual, 0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)
     `RAL_FIELD_CP_16(cp_policy, cp_policy.actual, 16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31)
  endgroup
  `endif

  // --------------------------
  // add constraint based on database legal values
  // --------------------------
  function void build();
    cp_policy = new("cp_policy", "RW", 32, 0, {"PMU_SAI_CONTROL_POLICY1_LO.cp_policy"});
    cp_policy.set_powerwell("primary");
    cp_policy.set_rand_mode(0);
   cp_policy.set_reset_signame("pmu_rst_b");
    void'(add_field( cp_policy ));

  endfunction


`DEFINE_NONE_SCOPE_REG_API(pmu_mmr_PMU_SAI_CONTROL_POLICY1_LO_reg) 
endclass : pmu_mmr_PMU_SAI_CONTROL_POLICY1_LO_reg

// ================================================

class pmu_mmr_PMU_SAI_CONTROL_POLICY2_HI_reg extends sla_ral_reg;

  // --------------------------
  sla_ral_field cp_policy;

  // --------------------------
  `ovm_object_utils(pmu_mmr_PMU_SAI_CONTROL_POLICY2_HI_reg)

  // --------------------------
  function new(
                string         name = "",
                `ifdef SLA_RAL_COVERAGE
                sla_ral_coverage_t cov_t = COVERAGE_OFF,
                `endif
                int            bus_num=0,
                int            dev_num=0,
                int            func_num=0,
                int            offset=0,
                int            size=0,
                sla_ral_data_t reset_val=0,
                boolean_t      mon_enabled = SLA_FALSE
             );
      super.new();
      set_space_addr("CFG",undef);
      set_space_addr("MEM",undef);
      set_space_addr("IO",undef);
      set_space_addr("MSG",undef);
      set_space_addr("CREG",undef); 
      set_space_addr("LT_MEM",undef);    
 build();

      `ifdef SLA_RAL_COVERAGE
      `CONFIG_RAL_COVERAGE(regname, COVERAGE_ON, FD_RW_SAMPLE)
      if(_coverage_type != COVERAGE_OFF) begin
        if(_sample_type inside {FD_RO_SAMPLE, RO_SAMPLE})
        `CREATE_OP_SAMPLE_COV(RAL_WRITE)
        else if(_sample_type inside {FD_WO_SAMPLE, WO_SAMPLE})
        `CREATE_OP_SAMPLE_COV(RAL_READ)
        else
        `CREATE_OP_SAMPLE_COV(RAL_NONE)
        `CREATE_DESIRED_COV
        `CREATE_ACTUAL_COV
      end
      `endif

  endfunction

  `ifdef SLA_RAL_COVERAGE
  `CREATE_RAL_OP_COVERGROUP
  covergroup desired_cg @(cov_ev);
     option.per_instance = 1;
     option.name = {sla_ral_file::regfilename,".",regname};
     `RAL_FIELD_CP(cp_policy, cp_policy.desired)
     `RAL_FIELD_CP_16(cp_policy, cp_policy.desired, 0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)
     `RAL_FIELD_CP_16(cp_policy, cp_policy.desired, 16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31)
  endgroup
  covergroup actual_cg @(cov_ev);
     option.per_instance = 1;
     option.name = {sla_ral_file::regfilename,".",regname};
     `RAL_FIELD_CP(cp_policy, cp_policy.actual)
     `RAL_FIELD_CP_16(cp_policy, cp_policy.actual, 0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)
     `RAL_FIELD_CP_16(cp_policy, cp_policy.actual, 16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31)
  endgroup
  `endif

  // --------------------------
  // add constraint based on database legal values
  // --------------------------
  function void build();
    cp_policy = new("cp_policy", "RW", 32, 0, {"PMU_SAI_CONTROL_POLICY2_HI.cp_policy"});
    cp_policy.set_powerwell("primary");
    cp_policy.set_rand_mode(0);
   cp_policy.set_reset_signame("pmu_rst_b");
    void'(add_field( cp_policy ));

  endfunction


`DEFINE_NONE_SCOPE_REG_API(pmu_mmr_PMU_SAI_CONTROL_POLICY2_HI_reg) 
endclass : pmu_mmr_PMU_SAI_CONTROL_POLICY2_HI_reg

// ================================================

class pmu_mmr_PMU_SAI_CONTROL_POLICY2_LO_reg extends sla_ral_reg;

  // --------------------------
  sla_ral_field cp_policy;

  // --------------------------
  `ovm_object_utils(pmu_mmr_PMU_SAI_CONTROL_POLICY2_LO_reg)

  // --------------------------
  function new(
                string         name = "",
                `ifdef SLA_RAL_COVERAGE
                sla_ral_coverage_t cov_t = COVERAGE_OFF,
                `endif
                int            bus_num=0,
                int            dev_num=0,
                int            func_num=0,
                int            offset=0,
                int            size=0,
                sla_ral_data_t reset_val=0,
                boolean_t      mon_enabled = SLA_FALSE
             );
      super.new();
      set_space_addr("CFG",undef);
      set_space_addr("MEM",undef);
      set_space_addr("IO",undef);
      set_space_addr("MSG",undef);
      set_space_addr("CREG",undef); 
      set_space_addr("LT_MEM",undef);    
 build();

      `ifdef SLA_RAL_COVERAGE
      `CONFIG_RAL_COVERAGE(regname, COVERAGE_ON, FD_RW_SAMPLE)
      if(_coverage_type != COVERAGE_OFF) begin
        if(_sample_type inside {FD_RO_SAMPLE, RO_SAMPLE})
        `CREATE_OP_SAMPLE_COV(RAL_WRITE)
        else if(_sample_type inside {FD_WO_SAMPLE, WO_SAMPLE})
        `CREATE_OP_SAMPLE_COV(RAL_READ)
        else
        `CREATE_OP_SAMPLE_COV(RAL_NONE)
        `CREATE_DESIRED_COV
        `CREATE_ACTUAL_COV
      end
      `endif

  endfunction

  `ifdef SLA_RAL_COVERAGE
  `CREATE_RAL_OP_COVERGROUP
  covergroup desired_cg @(cov_ev);
     option.per_instance = 1;
     option.name = {sla_ral_file::regfilename,".",regname};
     `RAL_FIELD_CP(cp_policy, cp_policy.desired)
     `RAL_FIELD_CP_16(cp_policy, cp_policy.desired, 0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)
     `RAL_FIELD_CP_16(cp_policy, cp_policy.desired, 16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31)
  endgroup
  covergroup actual_cg @(cov_ev);
     option.per_instance = 1;
     option.name = {sla_ral_file::regfilename,".",regname};
     `RAL_FIELD_CP(cp_policy, cp_policy.actual)
     `RAL_FIELD_CP_16(cp_policy, cp_policy.actual, 0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)
     `RAL_FIELD_CP_16(cp_policy, cp_policy.actual, 16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31)
  endgroup
  `endif

  // --------------------------
  // add constraint based on database legal values
  // --------------------------
  function void build();
    cp_policy = new("cp_policy", "RW", 32, 0, {"PMU_SAI_CONTROL_POLICY2_LO.cp_policy"});
    cp_policy.set_powerwell("primary");
    cp_policy.set_rand_mode(0);
   cp_policy.set_reset_signame("pmu_rst_b");
    void'(add_field( cp_policy ));

  endfunction


`DEFINE_NONE_SCOPE_REG_API(pmu_mmr_PMU_SAI_CONTROL_POLICY2_LO_reg) 
endclass : pmu_mmr_PMU_SAI_CONTROL_POLICY2_LO_reg

// ================================================

class pmu_mmr_file extends sla_ral_file;

  rand pmu_mmr_INT_STS_0_reg INT_STS_0;
  rand pmu_mmr_INT_STS_1_reg INT_STS_1;
  rand pmu_mmr_INT_EN_0_reg INT_EN_0;
  rand pmu_mmr_INT_EN_1_reg INT_EN_1;
  rand pmu_mmr_INT_CTRL_STS_reg INT_CTRL_STS;
  rand pmu_mmr_IP_ST_DIS_MASK_0_reg IP_ST_DIS_MASK_0;
  rand pmu_mmr_IP_ST_DIS_MASK_1_reg IP_ST_DIS_MASK_1;
  rand pmu_mmr_IP_RST_EN_0_reg IP_RST_EN_0;
  rand pmu_mmr_IP_RST_EN_1_reg IP_RST_EN_1;
  rand pmu_mmr_RST_CTRL_reg RST_CTRL;
  rand pmu_mmr_IP_RST_STS_0_reg IP_RST_STS_0;
  rand pmu_mmr_IP_RST_STS_1_reg IP_RST_STS_1;
  rand pmu_mmr_PWRGD_CTRL_reg PWRGD_CTRL;
  rand pmu_mmr_PWRGD_STS_reg PWRGD_STS;
  rand pmu_mmr_ISOL_CTRL_reg ISOL_CTRL;
  rand pmu_mmr_PLL_SIG_0_reg PLL_SIG_0;
  rand pmu_mmr_PG_CTL_reg PG_CTL;
  rand pmu_mmr_IP_PUG_TYPE_0_reg IP_PUG_TYPE_0;
  rand pmu_mmr_IP_PUG_TYPE_1_reg IP_PUG_TYPE_1;
  rand pmu_mmr_IP_PG_REQ_STS_0_reg IP_PG_REQ_STS_0;
  rand pmu_mmr_IP_PG_REQ_STS_1_reg IP_PG_REQ_STS_1;
  rand pmu_mmr_IP_PWR_STS_0_reg IP_PWR_STS_0;
  rand pmu_mmr_IP_PWR_STS_1_reg IP_PWR_STS_1;
  rand pmu_mmr_IP_FET_EN_ACK_STS_0_reg IP_FET_EN_ACK_STS_0;
  rand pmu_mmr_IP_FET_EN_ACK_STS_1_reg IP_FET_EN_ACK_STS_1;
  rand pmu_mmr_IP_STS_MASK_0_reg IP_STS_MASK_0;
  rand pmu_mmr_IP_STS_MASK_1_reg IP_STS_MASK_1;
  rand pmu_mmr_IP_WAKE_CTL_0_reg IP_WAKE_CTL_0;
  rand pmu_mmr_IP_WAKE_CTL_1_reg IP_WAKE_CTL_1;
  rand pmu_mmr_IP_SIDE_POK_STS_0_reg IP_SIDE_POK_STS_0;
  rand pmu_mmr_IP_SIDE_POK_STS_1_reg IP_SIDE_POK_STS_1;
  rand pmu_mmr_IP_PRIM_POK_STS_0_reg IP_PRIM_POK_STS_0;
  rand pmu_mmr_IP_PRIM_POK_STS_1_reg IP_PRIM_POK_STS_1;
  rand pmu_mmr_IP_READY_STS_0_reg IP_READY_STS_0;
  rand pmu_mmr_IP_READY_STS_1_reg IP_READY_STS_1;
  rand pmu_mmr_IP_MSG_EN_0_reg IP_MSG_EN_0;
  rand pmu_mmr_IP_MSG_EN_1_reg IP_MSG_EN_1;
  rand pmu_mmr_SBI_CHMSG_CTRL_reg SBI_CHMSG_CTRL;
  rand pmu_mmr_SBI_SETID_DW1_reg SBI_SETID_DW1;
  rand pmu_mmr_SBI_SETID_DW2_reg SBI_SETID_DW2;
  rand pmu_mmr_IP_BP_RP_ACK_STS_0_reg IP_BP_RP_ACK_STS_0;
  rand pmu_mmr_IP_BP_RP_ACK_STS_1_reg IP_BP_RP_ACK_STS_1;
  rand pmu_mmr_IP_SB_CPL_STS_MASK_0_reg IP_SB_CPL_STS_MASK_0;
  rand pmu_mmr_IP_SB_CPL_STS_MASK_1_reg IP_SB_CPL_STS_MASK_1;
  rand pmu_mmr_IP_SB_CPL_STS_0_reg IP_SB_CPL_STS_0;
  rand pmu_mmr_IP_SB_CPL_STS_1_reg IP_SB_CPL_STS_1;
  rand pmu_mmr_SBI_MSTR_CTL_P_reg SBI_MSTR_CTL_P;
  rand pmu_mmr_SBI_MSTR_HDR_P_reg SBI_MSTR_HDR_P;
  rand pmu_mmr_SBI_MSTR_EH_P_reg SBI_MSTR_EH_P;
  rand pmu_mmr_SBI_MSTR_ADDR_P_reg SBI_MSTR_ADDR_P;
  rand pmu_mmr_SBI_MSTR_DW0_P_reg SBI_MSTR_DW0_P;
  rand pmu_mmr_SBI_MSTR_DW1_P_reg SBI_MSTR_DW1_P;
  rand pmu_mmr_SBI_MSTR_CTL_NP_reg SBI_MSTR_CTL_NP;
  rand pmu_mmr_SBI_MSTR_STS_NP_reg SBI_MSTR_STS_NP;
  rand pmu_mmr_SBI_MSTR_HDR_NP_reg SBI_MSTR_HDR_NP;
  rand pmu_mmr_SBI_MSTR_EH_NP_reg SBI_MSTR_EH_NP;
  rand pmu_mmr_SBI_MSTR_ADDR_NP_reg SBI_MSTR_ADDR_NP;
  rand pmu_mmr_SBI_MSTR_DW0_NP_reg SBI_MSTR_DW0_NP;
  rand pmu_mmr_SBI_MSTR_DW1_NP_reg SBI_MSTR_DW1_NP;
  rand pmu_mmr_SBI_MSTR_CPL_HDR_reg SBI_MSTR_CPL_HDR;
  rand pmu_mmr_SBI_MSTR_CPL_EH_reg SBI_MSTR_CPL_EH;
  rand pmu_mmr_SBI_MSTR_CPL_DATA_reg SBI_MSTR_CPL_DATA;
  rand pmu_mmr_SBI_IMSG_FIFO_STS_reg SBI_IMSG_FIFO_STS;
  rand pmu_mmr_SBI_IMSG_FIFO_CTL_reg SBI_IMSG_FIFO_CTL;
  rand pmu_mmr_SBI_IMSG_FIFO_DATA_DW0_reg SBI_IMSG_FIFO_DATA_DW0;
  rand pmu_mmr_SBI_IMSG_FIFO_DATA_DW1_reg SBI_IMSG_FIFO_DATA_DW1;
  rand pmu_mmr_SBI_IMSG_FIFO_DATA_DW2_reg SBI_IMSG_FIFO_DATA_DW2;
  rand pmu_mmr_SBI_IMSG_FIFO_DATA_DW3_reg SBI_IMSG_FIFO_DATA_DW3;
  rand pmu_mmr_TSRSP_CTL_reg TSRSP_CTL;
  rand pmu_mmr_TSRSP_STS_reg TSRSP_STS;
  rand pmu_mmr_TSRSP_CPL_STS_reg TSRSP_CPL_STS;
  rand pmu_mmr_TSRSP_ART_SNAPSHOT_LO_reg TSRSP_ART_SNAPSHOT_LO;
  rand pmu_mmr_TSRSP_ART_SNAPSHOT_HI_reg TSRSP_ART_SNAPSHOT_HI;
  rand pmu_mmr_TS_SERIAL_CTL_reg TS_SERIAL_CTL;
  rand pmu_mmr_PM_CORE_VID_reg PM_CORE_VID;
  rand pmu_mmr_DTS0_TEMP_reg DTS0_TEMP;
  rand pmu_mmr_DTS1_TEMP_reg DTS1_TEMP;
  rand pmu_mmr_DTS2_TEMP_reg DTS2_TEMP;
  rand pmu_mmr_SEQENG_STS_reg SEQENG_STS;
  rand pmu_mmr_SEQENG_STS_1_reg SEQENG_STS_1;
  rand pmu_mmr_SEQENG_EVENTS_reg SEQENG_EVENTS;
  rand pmu_mmr_SEQENG_EVT_POL_reg SEQENG_EVT_POL;
  rand pmu_mmr_FUSE_PULL_STS_reg FUSE_PULL_STS;
  rand pmu_mmr_FW_CPL_STS_reg FW_CPL_STS;
  rand pmu_mmr_PTCH_CPL_STS_reg PTCH_CPL_STS;
  rand pmu_mmr_CH_CPL_STS_reg CH_CPL_STS;
  rand pmu_mmr_IPRDY_STS_reg IPRDY_STS;
  rand pmu_mmr_BPRP_ACK_STS_reg BPRP_ACK_STS;
  rand pmu_mmr_PMETO_STS_reg PMETO_STS;
  rand pmu_mmr_TELEM_STS_reg TELEM_STS;
  rand pmu_mmr_SB_EP_DBG_CTL_reg SB_EP_DBG_CTL;
  rand pmu_mmr_SURVIVABILITY_CTL_0_reg SURVIVABILITY_CTL_0;
  rand pmu_mmr_SURVIVABILITY_CTL_1_reg SURVIVABILITY_CTL_1;
  rand pmu_mmr_SURVIVABILITY_STS_0_reg SURVIVABILITY_STS_0;
  rand pmu_mmr_SURVIVABILITY_STS_1_reg SURVIVABILITY_STS_1;
  rand pmu_mmr_IP_FORCE_FET_EN_OVR_EN_0_reg IP_FORCE_FET_EN_OVR_EN_0;
  rand pmu_mmr_IP_FORCE_FET_EN_OVR_EN_1_reg IP_FORCE_FET_EN_OVR_EN_1;
  rand pmu_mmr_IP_FORCE_FET_EN_OVR_VAL_0_reg IP_FORCE_FET_EN_OVR_VAL_0;
  rand pmu_mmr_IP_FORCE_FET_EN_OVR_VAL_1_reg IP_FORCE_FET_EN_OVR_VAL_1;
  rand pmu_mmr_GEN_CTL_0_reg GEN_CTL_0;
  rand pmu_mmr_GEN_CTL_1_reg GEN_CTL_1;
  rand pmu_mmr_GEN_STS_0_reg GEN_STS_0;
  rand pmu_mmr_GEN_STS_1_reg GEN_STS_1;
  rand pmu_mmr_PMU_FUSE_0_reg PMU_FUSE_0;
  rand pmu_mmr_PMU_FUSE_1_reg PMU_FUSE_1;
  rand pmu_mmr_PMU_FUSE_2_reg PMU_FUSE_2;
  rand pmu_mmr_PMU_FUSE_3_reg PMU_FUSE_3;
  rand pmu_mmr_PMU_FUSE_4_reg PMU_FUSE_4;
  rand pmu_mmr_PMU_FUSE_5_reg PMU_FUSE_5;
  rand pmu_mmr_PMU_FUSE_6_reg PMU_FUSE_6;
  rand pmu_mmr_PMU_FUSE_7_reg PMU_FUSE_7;
  rand pmu_mmr_PMU_FUSE_8_reg PMU_FUSE_8;
  rand pmu_mmr_PMU_FUSE_9_reg PMU_FUSE_9;
  rand pmu_mmr_PMU_FUSE_10_reg PMU_FUSE_10;
  rand pmu_mmr_PMU_FUSE_11_reg PMU_FUSE_11;
  rand pmu_mmr_PMU_FUSE_12_reg PMU_FUSE_12;
  rand pmu_mmr_PMU_FUSE_13_reg PMU_FUSE_13;
  rand pmu_mmr_PMU_FUSE_14_reg PMU_FUSE_14;
  rand pmu_mmr_PMU_FUSE_15_reg PMU_FUSE_15;
  rand pmu_mmr_PMU_STRAP_0_reg PMU_STRAP_0;
  rand pmu_mmr_PMU_STRAP_1_reg PMU_STRAP_1;
  rand pmu_mmr_PMU_STRAP_2_reg PMU_STRAP_2;
  rand pmu_mmr_PMU_STRAP_3_reg PMU_STRAP_3;
  rand pmu_mmr_PMU_SAI_READ_POLICY0_HI_reg PMU_SAI_READ_POLICY0_HI;
  rand pmu_mmr_PMU_SAI_READ_POLICY0_LO_reg PMU_SAI_READ_POLICY0_LO;
  rand pmu_mmr_PMU_SAI_READ_POLICY1_HI_reg PMU_SAI_READ_POLICY1_HI;
  rand pmu_mmr_PMU_SAI_READ_POLICY1_LO_reg PMU_SAI_READ_POLICY1_LO;
  rand pmu_mmr_PMU_SAI_READ_POLICY2_HI_reg PMU_SAI_READ_POLICY2_HI;
  rand pmu_mmr_PMU_SAI_READ_POLICY2_LO_reg PMU_SAI_READ_POLICY2_LO;
  rand pmu_mmr_PMU_SAI_WRITE_POLICY0_HI_reg PMU_SAI_WRITE_POLICY0_HI;
  rand pmu_mmr_PMU_SAI_WRITE_POLICY0_LO_reg PMU_SAI_WRITE_POLICY0_LO;
  rand pmu_mmr_PMU_SAI_WRITE_POLICY1_HI_reg PMU_SAI_WRITE_POLICY1_HI;
  rand pmu_mmr_PMU_SAI_WRITE_POLICY1_LO_reg PMU_SAI_WRITE_POLICY1_LO;
  rand pmu_mmr_PMU_SAI_WRITE_POLICY2_HI_reg PMU_SAI_WRITE_POLICY2_HI;
  rand pmu_mmr_PMU_SAI_WRITE_POLICY2_LO_reg PMU_SAI_WRITE_POLICY2_LO;
  rand pmu_mmr_PMU_SAI_CONTROL_POLICY0_HI_reg PMU_SAI_CONTROL_POLICY0_HI;
  rand pmu_mmr_PMU_SAI_CONTROL_POLICY0_LO_reg PMU_SAI_CONTROL_POLICY0_LO;
  rand pmu_mmr_PMU_SAI_CONTROL_POLICY1_HI_reg PMU_SAI_CONTROL_POLICY1_HI;
  rand pmu_mmr_PMU_SAI_CONTROL_POLICY1_LO_reg PMU_SAI_CONTROL_POLICY1_LO;
  rand pmu_mmr_PMU_SAI_CONTROL_POLICY2_HI_reg PMU_SAI_CONTROL_POLICY2_HI;
  rand pmu_mmr_PMU_SAI_CONTROL_POLICY2_LO_reg PMU_SAI_CONTROL_POLICY2_LO;

  `ovm_component_utils(pmu_mmr_file)

  function new(string n, ovm_component p);
    super.new(n,p);
  endfunction

  // --------------------------
  function void build();
    regfilename = this.get_name();


    `ifdef SLA_RAL_COVERAGE
    sla_ral_reg::regname = "INT_STS_0";
    `endif
    INT_STS_0 = pmu_mmr_INT_STS_0_reg::type_id::create("INT_STS_0", this);
    INT_STS_0.set_cfg(16'h0, 16'h0, 16'h0, 4'h0, 32, 32'b00000000000000000000000000000000);
    INT_STS_0.set_space_addr("MSG", 4'h0);
    INT_STS_0.set_space_addr("CR-SB",4'h0); 
    INT_STS_0.set_space("MSG");
    INT_STS_0.set_msg_opcode("CR-SB");
    INT_STS_0.set_security_policy("security_Policy_group", "PMU_SAI_POLICY0");
    if ( $test$plusargs("INT_STS_0:dont_test") ) INT_STS_0.set_test_reg(1'b0);
    if (!add_reg( INT_STS_0 )) begin
      `sla_error(get_name(), ("could not add register INT_STS_0"));
    end
   INT_STS_0.SIDE_POK_ALL0_STS.set_paths({"INT_STS_0.SIDE_POK_ALL0_STS"});
   INT_STS_0.SIDE_POK_ALL1_STS.set_paths({"INT_STS_0.SIDE_POK_ALL1_STS"});
   INT_STS_0.PRIM_POK_ALL0_STS.set_paths({"INT_STS_0.PRIM_POK_ALL0_STS"});
   INT_STS_0.PRIM_POK_ALL1_STS.set_paths({"INT_STS_0.PRIM_POK_ALL1_STS"});
   INT_STS_0.IP_PWR_STS_ALL0_STS.set_paths({"INT_STS_0.IP_PWR_STS_ALL0_STS"});
   INT_STS_0.IP_PWR_STS_ALL1_STS.set_paths({"INT_STS_0.IP_PWR_STS_ALL1_STS"});
   INT_STS_0.IN_SB_MSG_AVAIL_STS.set_paths({"INT_STS_0.IN_SB_MSG_AVAIL_STS"});
   INT_STS_0.SEND_SB_PC_STS.set_paths({"INT_STS_0.SEND_SB_PC_STS"});
   INT_STS_0.SB_NP_CPL_AVAIL_STS.set_paths({"INT_STS_0.SB_NP_CPL_AVAIL_STS"});
   INT_STS_0.IN_SB_FIFO_FULL_STS.set_paths({"INT_STS_0.IN_SB_FIFO_FULL_STS"});
   INT_STS_0.SEND_SB_CHMSG_STS.set_paths({"INT_STS_0.SEND_SB_CHMSG_STS"});
   INT_STS_0.RSVD_13.set_paths({"INT_STS_0.RSVD_13"});
   INT_STS_0.SSC_PLL_VALID_STS.set_paths({"INT_STS_0.SSC_PLL_VALID_STS"});
   INT_STS_0.RSVD_15.set_paths({"INT_STS_0.RSVD_15"});
   INT_STS_0.CGU_PLL_VALID_STS.set_paths({"INT_STS_0.CGU_PLL_VALID_STS"});
   INT_STS_0.IP_SB_CPL_ALL1_STS.set_paths({"INT_STS_0.IP_SB_CPL_ALL1_STS"});
   INT_STS_0.SPIRDY_STS.set_paths({"INT_STS_0.SPIRDY_STS"});
   INT_STS_0.FUSE_LOAD_STS.set_paths({"INT_STS_0.FUSE_LOAD_STS"});
   INT_STS_0.IP_READY_ALL1_STS.set_paths({"INT_STS_0.IP_READY_ALL1_STS"});
   INT_STS_0.IP_BP_RP_ALL1_STS.set_paths({"INT_STS_0.IP_BP_RP_ALL1_STS"});
   INT_STS_0.RSVD_22.set_paths({"INT_STS_0.RSVD_22"});
   INT_STS_0.RSVD_23.set_paths({"INT_STS_0.RSVD_23"});
   INT_STS_0.FPGPOK_STS.set_paths({"INT_STS_0.FPGPOK_STS"});
   INT_STS_0.SETID_STS.set_paths({"INT_STS_0.SETID_STS"});
   INT_STS_0.IP_READY_ERR_STS.set_paths({"INT_STS_0.IP_READY_ERR_STS"});
   INT_STS_0.SB_CPL_ERR_STS.set_paths({"INT_STS_0.SB_CPL_ERR_STS"});
   INT_STS_0.RSVD_31_27.set_paths({"INT_STS_0.RSVD_31_27"});

    `ifdef SLA_RAL_COVERAGE
    sla_ral_reg::regname = "INT_STS_1";
    `endif
    INT_STS_1 = pmu_mmr_INT_STS_1_reg::type_id::create("INT_STS_1", this);
    INT_STS_1.set_cfg(16'h0, 16'h0, 16'h0, 4'h4, 32, 32'b00000000000000000000000000000000);
    INT_STS_1.set_space_addr("MSG", 4'h4);
    INT_STS_1.set_space_addr("CR-SB",4'h4); 
    INT_STS_1.set_space("MSG");
    INT_STS_1.set_msg_opcode("CR-SB");
    INT_STS_1.set_security_policy("security_Policy_group", "PMU_SAI_POLICY0");
    if ( $test$plusargs("INT_STS_1:dont_test") ) INT_STS_1.set_test_reg(1'b0);
    if (!add_reg( INT_STS_1 )) begin
      `sla_error(get_name(), ("could not add register INT_STS_1"));
    end
   INT_STS_1.RSVD_31_0.set_paths({"INT_STS_1.RSVD_31_0"});

    `ifdef SLA_RAL_COVERAGE
    sla_ral_reg::regname = "INT_EN_0";
    `endif
    INT_EN_0 = pmu_mmr_INT_EN_0_reg::type_id::create("INT_EN_0", this);
    INT_EN_0.set_cfg(16'h0, 16'h0, 16'h0, 8'h0C, 32, 32'b00000000000000000000000000000000);
    INT_EN_0.set_space_addr("MSG", 8'h0C);
    INT_EN_0.set_space_addr("CR-SB",8'h0C); 
    INT_EN_0.set_space("MSG");
    INT_EN_0.set_msg_opcode("CR-SB");
    INT_EN_0.set_security_policy("security_Policy_group", "PMU_SAI_POLICY0");
    if ( $test$plusargs("INT_EN_0:dont_test") ) INT_EN_0.set_test_reg(1'b0);
    if (!add_reg( INT_EN_0 )) begin
      `sla_error(get_name(), ("could not add register INT_EN_0"));
    end
   INT_EN_0.SIDE_POK_ALL0_EN.set_paths({"INT_EN_0.SIDE_POK_ALL0_EN"});
   INT_EN_0.SIDE_POK_ALL1_EN.set_paths({"INT_EN_0.SIDE_POK_ALL1_EN"});
   INT_EN_0.PRIM_POK_ALL0_EN.set_paths({"INT_EN_0.PRIM_POK_ALL0_EN"});
   INT_EN_0.PRIM_POK_ALL1_EN.set_paths({"INT_EN_0.PRIM_POK_ALL1_EN"});
   INT_EN_0.IP_PWR_STS_ALL0_EN.set_paths({"INT_EN_0.IP_PWR_STS_ALL0_EN"});
   INT_EN_0.IP_PWR_STS_ALL1_EN.set_paths({"INT_EN_0.IP_PWR_STS_ALL1_EN"});
   INT_EN_0.IN_SB_MSG_AVAIL_EN.set_paths({"INT_EN_0.IN_SB_MSG_AVAIL_EN"});
   INT_EN_0.SEND_SB_PC_EN.set_paths({"INT_EN_0.SEND_SB_PC_EN"});
   INT_EN_0.SB_NP_CPL_AVAIL_EN.set_paths({"INT_EN_0.SB_NP_CPL_AVAIL_EN"});
   INT_EN_0.IN_SB_FIFO_FULL_EN.set_paths({"INT_EN_0.IN_SB_FIFO_FULL_EN"});
   INT_EN_0.RSVD_12.set_paths({"INT_EN_0.RSVD_12"});
   INT_EN_0.RSVD_13.set_paths({"INT_EN_0.RSVD_13"});
   INT_EN_0.SSC_PLL_VALID_EN.set_paths({"INT_EN_0.SSC_PLL_VALID_EN"});
   INT_EN_0.RSVD_15.set_paths({"INT_EN_0.RSVD_15"});
   INT_EN_0.CGU_PLL_VALID_EN.set_paths({"INT_EN_0.CGU_PLL_VALID_EN"});
   INT_EN_0.IP_SB_CPL_ALL1_EN.set_paths({"INT_EN_0.IP_SB_CPL_ALL1_EN"});
   INT_EN_0.SPIRDY_EN.set_paths({"INT_EN_0.SPIRDY_EN"});
   INT_EN_0.FUSE_LOAD_EN.set_paths({"INT_EN_0.FUSE_LOAD_EN"});
   INT_EN_0.IP_READY_ALL1_EN.set_paths({"INT_EN_0.IP_READY_ALL1_EN"});
   INT_EN_0.IP_BP_RP_ALL1_EN.set_paths({"INT_EN_0.IP_BP_RP_ALL1_EN"});
   INT_EN_0.RSVD_22.set_paths({"INT_EN_0.RSVD_22"});
   INT_EN_0.RSVD_23.set_paths({"INT_EN_0.RSVD_23"});
   INT_EN_0.FPGPOK_EN.set_paths({"INT_EN_0.FPGPOK_EN"});
   INT_EN_0.SETID_EN.set_paths({"INT_EN_0.SETID_EN"});
   INT_EN_0.IP_READY_ERR_EN.set_paths({"INT_EN_0.IP_READY_ERR_EN"});
   INT_EN_0.RSVD_31_25.set_paths({"INT_EN_0.RSVD_31_25"});

    `ifdef SLA_RAL_COVERAGE
    sla_ral_reg::regname = "INT_EN_1";
    `endif
    INT_EN_1 = pmu_mmr_INT_EN_1_reg::type_id::create("INT_EN_1", this);
    INT_EN_1.set_cfg(16'h0, 16'h0, 16'h0, 8'h10, 32, 32'b00000000000000000000000000000000);
    INT_EN_1.set_space_addr("MSG", 8'h10);
    INT_EN_1.set_space_addr("CR-SB",8'h10); 
    INT_EN_1.set_space("MSG");
    INT_EN_1.set_msg_opcode("CR-SB");
    INT_EN_1.set_security_policy("security_Policy_group", "PMU_SAI_POLICY0");
    if ( $test$plusargs("INT_EN_1:dont_test") ) INT_EN_1.set_test_reg(1'b0);
    if (!add_reg( INT_EN_1 )) begin
      `sla_error(get_name(), ("could not add register INT_EN_1"));
    end
   INT_EN_1.RSVD_31_0.set_paths({"INT_EN_1.RSVD_31_0"});

    `ifdef SLA_RAL_COVERAGE
    sla_ral_reg::regname = "INT_CTRL_STS";
    `endif
    INT_CTRL_STS = pmu_mmr_INT_CTRL_STS_reg::type_id::create("INT_CTRL_STS", this);
    INT_CTRL_STS.set_cfg(16'h0, 16'h0, 16'h0, 8'h18, 32, 32'b00000000000000000000000000000000);
    INT_CTRL_STS.set_space_addr("MSG", 8'h18);
    INT_CTRL_STS.set_space_addr("CR-SB",8'h18); 
    INT_CTRL_STS.set_space("MSG");
    INT_CTRL_STS.set_msg_opcode("CR-SB");
    INT_CTRL_STS.set_security_policy("security_Policy_group", "PMU_SAI_POLICY0");
    if ( $test$plusargs("INT_CTRL_STS:dont_test") ) INT_CTRL_STS.set_test_reg(1'b0);
    if (!add_reg( INT_CTRL_STS )) begin
      `sla_error(get_name(), ("could not add register INT_CTRL_STS"));
    end
   INT_CTRL_STS.INT_ADDR.set_paths({"INT_CTRL_STS.INT_ADDR"});
   INT_CTRL_STS.INT_CMPL_UC_F.set_paths({"INT_CTRL_STS.INT_CMPL_UC_F"});
   INT_CTRL_STS.INT_CMPL_SAI_F.set_paths({"INT_CTRL_STS.INT_CMPL_SAI_F"});
   INT_CTRL_STS.INT_CMPL_UR.set_paths({"INT_CTRL_STS.INT_CMPL_UR"});

    `ifdef SLA_RAL_COVERAGE
    sla_ral_reg::regname = "IP_ST_DIS_MASK_0";
    `endif
    IP_ST_DIS_MASK_0 = pmu_mmr_IP_ST_DIS_MASK_0_reg::type_id::create("IP_ST_DIS_MASK_0", this);
    IP_ST_DIS_MASK_0.set_cfg(16'h0, 16'h0, 16'h0, 8'h20, 32, 32'b00000000000000000000000000000000);
    IP_ST_DIS_MASK_0.set_space_addr("MSG", 8'h20);
    IP_ST_DIS_MASK_0.set_space_addr("CR-SB",8'h20); 
    IP_ST_DIS_MASK_0.set_space("MSG");
    IP_ST_DIS_MASK_0.set_msg_opcode("CR-SB");
    IP_ST_DIS_MASK_0.set_security_policy("security_Policy_group", "PMU_SAI_POLICY0");
    if ( $test$plusargs("IP_ST_DIS_MASK_0:dont_test") ) IP_ST_DIS_MASK_0.set_test_reg(1'b0);
    if (!add_reg( IP_ST_DIS_MASK_0 )) begin
      `sla_error(get_name(), ("could not add register IP_ST_DIS_MASK_0"));
    end
   IP_ST_DIS_MASK_0.IP_ST_DIS_IND_0_0.set_paths({"IP_ST_DIS_MASK_0.IP_ST_DIS_IND_0_0"});
   IP_ST_DIS_MASK_0.IP_ST_DIS_IND_0_1.set_paths({"IP_ST_DIS_MASK_0.IP_ST_DIS_IND_0_1"});
   IP_ST_DIS_MASK_0.IP_ST_DIS_IND_0_2.set_paths({"IP_ST_DIS_MASK_0.IP_ST_DIS_IND_0_2"});
   IP_ST_DIS_MASK_0.IP_ST_DIS_IND_0_3.set_paths({"IP_ST_DIS_MASK_0.IP_ST_DIS_IND_0_3"});
   IP_ST_DIS_MASK_0.IP_ST_DIS_IND_0_4.set_paths({"IP_ST_DIS_MASK_0.IP_ST_DIS_IND_0_4"});
   IP_ST_DIS_MASK_0.IP_ST_DIS_IND_0_5.set_paths({"IP_ST_DIS_MASK_0.IP_ST_DIS_IND_0_5"});
   IP_ST_DIS_MASK_0.IP_ST_DIS_IND_0_6.set_paths({"IP_ST_DIS_MASK_0.IP_ST_DIS_IND_0_6"});
   IP_ST_DIS_MASK_0.IP_ST_DIS_IND_0_7.set_paths({"IP_ST_DIS_MASK_0.IP_ST_DIS_IND_0_7"});
   IP_ST_DIS_MASK_0.IP_ST_DIS_IND_0_8.set_paths({"IP_ST_DIS_MASK_0.IP_ST_DIS_IND_0_8"});
   IP_ST_DIS_MASK_0.IP_ST_DIS_IND_0_9.set_paths({"IP_ST_DIS_MASK_0.IP_ST_DIS_IND_0_9"});
   IP_ST_DIS_MASK_0.IP_ST_DIS_IND_0_10.set_paths({"IP_ST_DIS_MASK_0.IP_ST_DIS_IND_0_10"});
   IP_ST_DIS_MASK_0.IP_ST_DIS_IND_0_11.set_paths({"IP_ST_DIS_MASK_0.IP_ST_DIS_IND_0_11"});
   IP_ST_DIS_MASK_0.IP_ST_DIS_IND_0_12.set_paths({"IP_ST_DIS_MASK_0.IP_ST_DIS_IND_0_12"});
   IP_ST_DIS_MASK_0.IP_ST_DIS_IND_0_13.set_paths({"IP_ST_DIS_MASK_0.IP_ST_DIS_IND_0_13"});
   IP_ST_DIS_MASK_0.IP_ST_DIS_IND_0_14.set_paths({"IP_ST_DIS_MASK_0.IP_ST_DIS_IND_0_14"});
   IP_ST_DIS_MASK_0.IP_ST_DIS_IND_0_15.set_paths({"IP_ST_DIS_MASK_0.IP_ST_DIS_IND_0_15"});
   IP_ST_DIS_MASK_0.IP_ST_DIS_IND_0_16.set_paths({"IP_ST_DIS_MASK_0.IP_ST_DIS_IND_0_16"});
   IP_ST_DIS_MASK_0.IP_ST_DIS_IND_0_17.set_paths({"IP_ST_DIS_MASK_0.IP_ST_DIS_IND_0_17"});
   IP_ST_DIS_MASK_0.IP_ST_DIS_IND_0_18.set_paths({"IP_ST_DIS_MASK_0.IP_ST_DIS_IND_0_18"});
   IP_ST_DIS_MASK_0.IP_ST_DIS_IND_0_19.set_paths({"IP_ST_DIS_MASK_0.IP_ST_DIS_IND_0_19"});
   IP_ST_DIS_MASK_0.IP_ST_DIS_IND_0_20.set_paths({"IP_ST_DIS_MASK_0.IP_ST_DIS_IND_0_20"});
   IP_ST_DIS_MASK_0.IP_ST_DIS_IND_0_21.set_paths({"IP_ST_DIS_MASK_0.IP_ST_DIS_IND_0_21"});
   IP_ST_DIS_MASK_0.IP_ST_DIS_IND_0_22.set_paths({"IP_ST_DIS_MASK_0.IP_ST_DIS_IND_0_22"});
   IP_ST_DIS_MASK_0.IP_ST_DIS_IND_0_23.set_paths({"IP_ST_DIS_MASK_0.IP_ST_DIS_IND_0_23"});
   IP_ST_DIS_MASK_0.IP_ST_DIS_IND_0_24.set_paths({"IP_ST_DIS_MASK_0.IP_ST_DIS_IND_0_24"});
   IP_ST_DIS_MASK_0.IP_ST_DIS_IND_0_25.set_paths({"IP_ST_DIS_MASK_0.IP_ST_DIS_IND_0_25"});
   IP_ST_DIS_MASK_0.IP_ST_DIS_IND_0_26.set_paths({"IP_ST_DIS_MASK_0.IP_ST_DIS_IND_0_26"});
   IP_ST_DIS_MASK_0.IP_ST_DIS_IND_0_27.set_paths({"IP_ST_DIS_MASK_0.IP_ST_DIS_IND_0_27"});
   IP_ST_DIS_MASK_0.IP_ST_DIS_IND_0_28.set_paths({"IP_ST_DIS_MASK_0.IP_ST_DIS_IND_0_28"});
   IP_ST_DIS_MASK_0.IP_ST_DIS_IND_0_29.set_paths({"IP_ST_DIS_MASK_0.IP_ST_DIS_IND_0_29"});
   IP_ST_DIS_MASK_0.IP_ST_DIS_IND_0_30.set_paths({"IP_ST_DIS_MASK_0.IP_ST_DIS_IND_0_30"});
   IP_ST_DIS_MASK_0.IP_ST_DIS_IND_0_31.set_paths({"IP_ST_DIS_MASK_0.IP_ST_DIS_IND_0_31"});

    `ifdef SLA_RAL_COVERAGE
    sla_ral_reg::regname = "IP_ST_DIS_MASK_1";
    `endif
    IP_ST_DIS_MASK_1 = pmu_mmr_IP_ST_DIS_MASK_1_reg::type_id::create("IP_ST_DIS_MASK_1", this);
    IP_ST_DIS_MASK_1.set_cfg(16'h0, 16'h0, 16'h0, 8'h24, 32, 32'b00000000000000000000000000000000);
    IP_ST_DIS_MASK_1.set_space_addr("MSG", 8'h24);
    IP_ST_DIS_MASK_1.set_space_addr("CR-SB",8'h24); 
    IP_ST_DIS_MASK_1.set_space("MSG");
    IP_ST_DIS_MASK_1.set_msg_opcode("CR-SB");
    IP_ST_DIS_MASK_1.set_security_policy("security_Policy_group", "PMU_SAI_POLICY0");
    if ( $test$plusargs("IP_ST_DIS_MASK_1:dont_test") ) IP_ST_DIS_MASK_1.set_test_reg(1'b0);
    if (!add_reg( IP_ST_DIS_MASK_1 )) begin
      `sla_error(get_name(), ("could not add register IP_ST_DIS_MASK_1"));
    end
   IP_ST_DIS_MASK_1.IP_ST_DIS_IND_1_0.set_paths({"IP_ST_DIS_MASK_1.IP_ST_DIS_IND_1_0"});
   IP_ST_DIS_MASK_1.IP_ST_DIS_IND_1_1.set_paths({"IP_ST_DIS_MASK_1.IP_ST_DIS_IND_1_1"});
   IP_ST_DIS_MASK_1.IP_ST_DIS_IND_1_2.set_paths({"IP_ST_DIS_MASK_1.IP_ST_DIS_IND_1_2"});
   IP_ST_DIS_MASK_1.IP_ST_DIS_IND_1_3.set_paths({"IP_ST_DIS_MASK_1.IP_ST_DIS_IND_1_3"});
   IP_ST_DIS_MASK_1.IP_ST_DIS_IND_1_4.set_paths({"IP_ST_DIS_MASK_1.IP_ST_DIS_IND_1_4"});
   IP_ST_DIS_MASK_1.IP_ST_DIS_IND_1_5.set_paths({"IP_ST_DIS_MASK_1.IP_ST_DIS_IND_1_5"});
   IP_ST_DIS_MASK_1.IP_ST_DIS_IND_1_6.set_paths({"IP_ST_DIS_MASK_1.IP_ST_DIS_IND_1_6"});
   IP_ST_DIS_MASK_1.IP_ST_DIS_IND_1_7.set_paths({"IP_ST_DIS_MASK_1.IP_ST_DIS_IND_1_7"});
   IP_ST_DIS_MASK_1.IP_ST_DIS_IND_1_8.set_paths({"IP_ST_DIS_MASK_1.IP_ST_DIS_IND_1_8"});
   IP_ST_DIS_MASK_1.IP_ST_DIS_IND_1_9.set_paths({"IP_ST_DIS_MASK_1.IP_ST_DIS_IND_1_9"});
   IP_ST_DIS_MASK_1.IP_ST_DIS_IND_1_10.set_paths({"IP_ST_DIS_MASK_1.IP_ST_DIS_IND_1_10"});
   IP_ST_DIS_MASK_1.IP_ST_DIS_IND_1_11.set_paths({"IP_ST_DIS_MASK_1.IP_ST_DIS_IND_1_11"});
   IP_ST_DIS_MASK_1.IP_ST_DIS_IND_1_12.set_paths({"IP_ST_DIS_MASK_1.IP_ST_DIS_IND_1_12"});
   IP_ST_DIS_MASK_1.IP_ST_DIS_IND_1_13.set_paths({"IP_ST_DIS_MASK_1.IP_ST_DIS_IND_1_13"});
   IP_ST_DIS_MASK_1.IP_ST_DIS_IND_1_14.set_paths({"IP_ST_DIS_MASK_1.IP_ST_DIS_IND_1_14"});
   IP_ST_DIS_MASK_1.IP_ST_DIS_IND_1_15.set_paths({"IP_ST_DIS_MASK_1.IP_ST_DIS_IND_1_15"});
   IP_ST_DIS_MASK_1.IP_ST_DIS_IND_1_16.set_paths({"IP_ST_DIS_MASK_1.IP_ST_DIS_IND_1_16"});
   IP_ST_DIS_MASK_1.IP_ST_DIS_IND_1_17.set_paths({"IP_ST_DIS_MASK_1.IP_ST_DIS_IND_1_17"});
   IP_ST_DIS_MASK_1.IP_ST_DIS_IND_1_18.set_paths({"IP_ST_DIS_MASK_1.IP_ST_DIS_IND_1_18"});
   IP_ST_DIS_MASK_1.IP_ST_DIS_IND_1_19.set_paths({"IP_ST_DIS_MASK_1.IP_ST_DIS_IND_1_19"});
   IP_ST_DIS_MASK_1.IP_ST_DIS_IND_1_20.set_paths({"IP_ST_DIS_MASK_1.IP_ST_DIS_IND_1_20"});
   IP_ST_DIS_MASK_1.IP_ST_DIS_IND_1_21.set_paths({"IP_ST_DIS_MASK_1.IP_ST_DIS_IND_1_21"});
   IP_ST_DIS_MASK_1.IP_ST_DIS_IND_1_22.set_paths({"IP_ST_DIS_MASK_1.IP_ST_DIS_IND_1_22"});
   IP_ST_DIS_MASK_1.IP_ST_DIS_IND_1_23.set_paths({"IP_ST_DIS_MASK_1.IP_ST_DIS_IND_1_23"});
   IP_ST_DIS_MASK_1.IP_ST_DIS_IND_1_24.set_paths({"IP_ST_DIS_MASK_1.IP_ST_DIS_IND_1_24"});
   IP_ST_DIS_MASK_1.IP_ST_DIS_IND_1_25.set_paths({"IP_ST_DIS_MASK_1.IP_ST_DIS_IND_1_25"});
   IP_ST_DIS_MASK_1.IP_ST_DIS_IND_1_26.set_paths({"IP_ST_DIS_MASK_1.IP_ST_DIS_IND_1_26"});
   IP_ST_DIS_MASK_1.IP_ST_DIS_IND_1_27.set_paths({"IP_ST_DIS_MASK_1.IP_ST_DIS_IND_1_27"});
   IP_ST_DIS_MASK_1.IP_ST_DIS_IND_1_28.set_paths({"IP_ST_DIS_MASK_1.IP_ST_DIS_IND_1_28"});
   IP_ST_DIS_MASK_1.IP_ST_DIS_IND_1_29.set_paths({"IP_ST_DIS_MASK_1.IP_ST_DIS_IND_1_29"});
   IP_ST_DIS_MASK_1.IP_ST_DIS_IND_1_30.set_paths({"IP_ST_DIS_MASK_1.IP_ST_DIS_IND_1_30"});
   IP_ST_DIS_MASK_1.IP_ST_DIS_IND_1_31.set_paths({"IP_ST_DIS_MASK_1.IP_ST_DIS_IND_1_31"});

    `ifdef SLA_RAL_COVERAGE
    sla_ral_reg::regname = "IP_RST_EN_0";
    `endif
    IP_RST_EN_0 = pmu_mmr_IP_RST_EN_0_reg::type_id::create("IP_RST_EN_0", this);
    IP_RST_EN_0.set_cfg(16'h0, 16'h0, 16'h0, 8'h30, 32, 32'b00000000000000000000000000000000);
    IP_RST_EN_0.set_space_addr("MSG", 8'h30);
    IP_RST_EN_0.set_space_addr("CR-SB",8'h30); 
    IP_RST_EN_0.set_space("MSG");
    IP_RST_EN_0.set_msg_opcode("CR-SB");
    IP_RST_EN_0.set_security_policy("security_Policy_group", "PMU_SAI_POLICY0");
    if ( $test$plusargs("IP_RST_EN_0:dont_test") ) IP_RST_EN_0.set_test_reg(1'b0);
    if (!add_reg( IP_RST_EN_0 )) begin
      `sla_error(get_name(), ("could not add register IP_RST_EN_0"));
    end
   IP_RST_EN_0.RST_EN_0.set_paths({"IP_RST_EN_0.RST_EN_0"});

    `ifdef SLA_RAL_COVERAGE
    sla_ral_reg::regname = "IP_RST_EN_1";
    `endif
    IP_RST_EN_1 = pmu_mmr_IP_RST_EN_1_reg::type_id::create("IP_RST_EN_1", this);
    IP_RST_EN_1.set_cfg(16'h0, 16'h0, 16'h0, 8'h34, 32, 32'b00000000000000000000000000000000);
    IP_RST_EN_1.set_space_addr("MSG", 8'h34);
    IP_RST_EN_1.set_space_addr("CR-SB",8'h34); 
    IP_RST_EN_1.set_space("MSG");
    IP_RST_EN_1.set_msg_opcode("CR-SB");
    IP_RST_EN_1.set_security_policy("security_Policy_group", "PMU_SAI_POLICY0");
    if ( $test$plusargs("IP_RST_EN_1:dont_test") ) IP_RST_EN_1.set_test_reg(1'b0);
    if (!add_reg( IP_RST_EN_1 )) begin
      `sla_error(get_name(), ("could not add register IP_RST_EN_1"));
    end
   IP_RST_EN_1.RST_EN_1.set_paths({"IP_RST_EN_1.RST_EN_1"});

    `ifdef SLA_RAL_COVERAGE
    sla_ral_reg::regname = "RST_CTRL";
    `endif
    RST_CTRL = pmu_mmr_RST_CTRL_reg::type_id::create("RST_CTRL", this);
    RST_CTRL.set_cfg(16'h0, 16'h0, 16'h0, 8'h40, 32, 32'b00000000000000000000000000000000);
    RST_CTRL.set_space_addr("MSG", 8'h40);
    RST_CTRL.set_space_addr("CR-SB",8'h40); 
    RST_CTRL.set_space("MSG");
    RST_CTRL.set_msg_opcode("CR-SB");
    RST_CTRL.set_security_policy("security_Policy_group", "PMU_SAI_POLICY0");
    if ( $test$plusargs("RST_CTRL:dont_test") ) RST_CTRL.set_test_reg(1'b0);
    if (!add_reg( RST_CTRL )) begin
      `sla_error(get_name(), ("could not add register RST_CTRL"));
    end
   RST_CTRL.UPDATE.set_paths({"RST_CTRL.UPDATE"});
   RST_CTRL.RST_VALUE.set_paths({"RST_CTRL.RST_VALUE"});
   RST_CTRL.RSVD_3_2.set_paths({"RST_CTRL.RSVD_3_2"});
   RST_CTRL.RST_TYPE.set_paths({"RST_CTRL.RST_TYPE"});
   RST_CTRL.RSVD_31_7.set_paths({"RST_CTRL.RSVD_31_7"});

    `ifdef SLA_RAL_COVERAGE
    sla_ral_reg::regname = "IP_RST_STS_0";
    `endif
    IP_RST_STS_0 = pmu_mmr_IP_RST_STS_0_reg::type_id::create("IP_RST_STS_0", this);
    IP_RST_STS_0.set_cfg(16'h0, 16'h0, 16'h0, 8'h44, 32, 32'b00000000000000000000000000000000);
    IP_RST_STS_0.set_space_addr("MSG", 8'h44);
    IP_RST_STS_0.set_space_addr("CR-SB",8'h44); 
    IP_RST_STS_0.set_space("MSG");
    IP_RST_STS_0.set_msg_opcode("CR-SB");
    IP_RST_STS_0.set_security_policy("security_Policy_group", "PMU_SAI_POLICY0");
    if ( $test$plusargs("IP_RST_STS_0:dont_test") ) IP_RST_STS_0.set_test_reg(1'b0);
    if (!add_reg( IP_RST_STS_0 )) begin
      `sla_error(get_name(), ("could not add register IP_RST_STS_0"));
    end
   IP_RST_STS_0.RST_STS.set_paths({"IP_RST_STS_0.RST_STS"});

    `ifdef SLA_RAL_COVERAGE
    sla_ral_reg::regname = "IP_RST_STS_1";
    `endif
    IP_RST_STS_1 = pmu_mmr_IP_RST_STS_1_reg::type_id::create("IP_RST_STS_1", this);
    IP_RST_STS_1.set_cfg(16'h0, 16'h0, 16'h0, 8'h48, 32, 32'b00000000000000000000000000000000);
    IP_RST_STS_1.set_space_addr("MSG", 8'h48);
    IP_RST_STS_1.set_space_addr("CR-SB",8'h48); 
    IP_RST_STS_1.set_space("MSG");
    IP_RST_STS_1.set_msg_opcode("CR-SB");
    IP_RST_STS_1.set_security_policy("security_Policy_group", "PMU_SAI_POLICY0");
    if ( $test$plusargs("IP_RST_STS_1:dont_test") ) IP_RST_STS_1.set_test_reg(1'b0);
    if (!add_reg( IP_RST_STS_1 )) begin
      `sla_error(get_name(), ("could not add register IP_RST_STS_1"));
    end
   IP_RST_STS_1.RST_STS.set_paths({"IP_RST_STS_1.RST_STS"});

    `ifdef SLA_RAL_COVERAGE
    sla_ral_reg::regname = "PWRGD_CTRL";
    `endif
    PWRGD_CTRL = pmu_mmr_PWRGD_CTRL_reg::type_id::create("PWRGD_CTRL", this);
    PWRGD_CTRL.set_cfg(16'h0, 16'h0, 16'h0, 8'h5C, 32, 32'b00000000000000000000000000000000);
    PWRGD_CTRL.set_space_addr("MSG", 8'h5C);
    PWRGD_CTRL.set_space_addr("CR-SB",8'h5C); 
    PWRGD_CTRL.set_space("MSG");
    PWRGD_CTRL.set_msg_opcode("CR-SB");
    PWRGD_CTRL.set_security_policy("security_Policy_group", "PMU_SAI_POLICY0");
    if ( $test$plusargs("PWRGD_CTRL:dont_test") ) PWRGD_CTRL.set_test_reg(1'b0);
    if (!add_reg( PWRGD_CTRL )) begin
      `sla_error(get_name(), ("could not add register PWRGD_CTRL"));
    end
   PWRGD_CTRL.PMU_PHY_PWRGD_B_0.set_paths({"PWRGD_CTRL.PMU_PHY_PWRGD_B_0"});
   PWRGD_CTRL.PMU_PHY_PWRGD_B_1.set_paths({"PWRGD_CTRL.PMU_PHY_PWRGD_B_1"});
   PWRGD_CTRL.PMU_PHY_PWRGD_B_2.set_paths({"PWRGD_CTRL.PMU_PHY_PWRGD_B_2"});
   PWRGD_CTRL.PMU_PHY_PWRGD_B_3.set_paths({"PWRGD_CTRL.PMU_PHY_PWRGD_B_3"});
   PWRGD_CTRL.PWRGD_4.set_paths({"PWRGD_CTRL.PWRGD_4"});
   PWRGD_CTRL.PWRGD_5.set_paths({"PWRGD_CTRL.PWRGD_5"});
   PWRGD_CTRL.PWRGD_6.set_paths({"PWRGD_CTRL.PWRGD_6"});
   PWRGD_CTRL.PWRGD_7.set_paths({"PWRGD_CTRL.PWRGD_7"});
   PWRGD_CTRL.RSVD_31_8.set_paths({"PWRGD_CTRL.RSVD_31_8"});

    `ifdef SLA_RAL_COVERAGE
    sla_ral_reg::regname = "PWRGD_STS";
    `endif
    PWRGD_STS = pmu_mmr_PWRGD_STS_reg::type_id::create("PWRGD_STS", this);
    PWRGD_STS.set_cfg(16'h0, 16'h0, 16'h0, 8'h60, 32, 32'b00000000000000000000000000000000);
    PWRGD_STS.set_space_addr("MSG", 8'h60);
    PWRGD_STS.set_space_addr("CR-SB",8'h60); 
    PWRGD_STS.set_space("MSG");
    PWRGD_STS.set_msg_opcode("CR-SB");
    PWRGD_STS.set_security_policy("security_Policy_group", "PMU_SAI_POLICY0");
    if ( $test$plusargs("PWRGD_STS:dont_test") ) PWRGD_STS.set_test_reg(1'b0);
    if (!add_reg( PWRGD_STS )) begin
      `sla_error(get_name(), ("could not add register PWRGD_STS"));
    end
   PWRGD_STS.PHY_PMU_PWR_STS_0.set_paths({"PWRGD_STS.PHY_PMU_PWR_STS_0"});
   PWRGD_STS.PHY_PMU_PWR_STS_1.set_paths({"PWRGD_STS.PHY_PMU_PWR_STS_1"});
   PWRGD_STS.PHY_PMU_PWR_STS_2.set_paths({"PWRGD_STS.PHY_PMU_PWR_STS_2"});
   PWRGD_STS.PHY_PMU_PWR_STS_3.set_paths({"PWRGD_STS.PHY_PMU_PWR_STS_3"});
   PWRGD_STS.PWRSTS_4.set_paths({"PWRGD_STS.PWRSTS_4"});
   PWRGD_STS.PWRSTS_5.set_paths({"PWRGD_STS.PWRSTS_5"});
   PWRGD_STS.PWRSTS_6.set_paths({"PWRGD_STS.PWRSTS_6"});
   PWRGD_STS.PWRSTS_7.set_paths({"PWRGD_STS.PWRSTS_7"});
   PWRGD_STS.RSVD_31_8.set_paths({"PWRGD_STS.RSVD_31_8"});

    `ifdef SLA_RAL_COVERAGE
    sla_ral_reg::regname = "ISOL_CTRL";
    `endif
    ISOL_CTRL = pmu_mmr_ISOL_CTRL_reg::type_id::create("ISOL_CTRL", this);
    ISOL_CTRL.set_cfg(16'h0, 16'h0, 16'h0, 8'h64, 32, 32'b00000000000000000000000000000000);
    ISOL_CTRL.set_space_addr("MSG", 8'h64);
    ISOL_CTRL.set_space_addr("CR-SB",8'h64); 
    ISOL_CTRL.set_space("MSG");
    ISOL_CTRL.set_msg_opcode("CR-SB");
    ISOL_CTRL.set_security_policy("security_Policy_group", "PMU_SAI_POLICY0");
    if ( $test$plusargs("ISOL_CTRL:dont_test") ) ISOL_CTRL.set_test_reg(1'b0);
    if (!add_reg( ISOL_CTRL )) begin
      `sla_error(get_name(), ("could not add register ISOL_CTRL"));
    end
   ISOL_CTRL.ISOL_0.set_paths({"ISOL_CTRL.ISOL_0"});
   ISOL_CTRL.ISOL_1.set_paths({"ISOL_CTRL.ISOL_1"});
   ISOL_CTRL.ISOL_2.set_paths({"ISOL_CTRL.ISOL_2"});
   ISOL_CTRL.ISOL_3.set_paths({"ISOL_CTRL.ISOL_3"});
   ISOL_CTRL.ISOL_4.set_paths({"ISOL_CTRL.ISOL_4"});
   ISOL_CTRL.ISOL_5.set_paths({"ISOL_CTRL.ISOL_5"});
   ISOL_CTRL.ISOL_6.set_paths({"ISOL_CTRL.ISOL_6"});
   ISOL_CTRL.ISOL_7.set_paths({"ISOL_CTRL.ISOL_7"});
   ISOL_CTRL.RSVD_31_8.set_paths({"ISOL_CTRL.RSVD_31_8"});

    `ifdef SLA_RAL_COVERAGE
    sla_ral_reg::regname = "PLL_SIG_0";
    `endif
    PLL_SIG_0 = pmu_mmr_PLL_SIG_0_reg::type_id::create("PLL_SIG_0", this);
    PLL_SIG_0.set_cfg(16'h0, 16'h0, 16'h0, 8'h68, 32, 32'b00000000000000000000000000000000);
    PLL_SIG_0.set_space_addr("MSG", 8'h68);
    PLL_SIG_0.set_space_addr("CR-SB",8'h68); 
    PLL_SIG_0.set_space("MSG");
    PLL_SIG_0.set_msg_opcode("CR-SB");
    PLL_SIG_0.set_security_policy("security_Policy_group", "PMU_SAI_POLICY0");
    if ( $test$plusargs("PLL_SIG_0:dont_test") ) PLL_SIG_0.set_test_reg(1'b0);
    if (!add_reg( PLL_SIG_0 )) begin
      `sla_error(get_name(), ("could not add register PLL_SIG_0"));
    end
   PLL_SIG_0.CGU_PLL_VALID.set_paths({"PLL_SIG_0.CGU_PLL_VALID"});
   PLL_SIG_0.SSC_PLL_VALID.set_paths({"PLL_SIG_0.SSC_PLL_VALID"});
   PLL_SIG_0.PLL_VALID_2.set_paths({"PLL_SIG_0.PLL_VALID_2"});
   PLL_SIG_0.PLL_VALID_3.set_paths({"PLL_SIG_0.PLL_VALID_3"});
   PLL_SIG_0.PLL_VALID_4.set_paths({"PLL_SIG_0.PLL_VALID_4"});
   PLL_SIG_0.PLL_VALID_5.set_paths({"PLL_SIG_0.PLL_VALID_5"});
   PLL_SIG_0.PLL_VALID_6.set_paths({"PLL_SIG_0.PLL_VALID_6"});
   PLL_SIG_0.PLL_VALID_7.set_paths({"PLL_SIG_0.PLL_VALID_7"});
   PLL_SIG_0.PLL_VALID_8.set_paths({"PLL_SIG_0.PLL_VALID_8"});
   PLL_SIG_0.PLL_VALID_9.set_paths({"PLL_SIG_0.PLL_VALID_9"});
   PLL_SIG_0.PLL_VALID_10.set_paths({"PLL_SIG_0.PLL_VALID_10"});
   PLL_SIG_0.PLL_VALID_11.set_paths({"PLL_SIG_0.PLL_VALID_11"});
   PLL_SIG_0.PLL_VALID_12.set_paths({"PLL_SIG_0.PLL_VALID_12"});
   PLL_SIG_0.PLL_VALID_13.set_paths({"PLL_SIG_0.PLL_VALID_13"});
   PLL_SIG_0.PLL_VALID_14.set_paths({"PLL_SIG_0.PLL_VALID_14"});
   PLL_SIG_0.PLL_VALID_15.set_paths({"PLL_SIG_0.PLL_VALID_15"});
   PLL_SIG_0.CGU_PLL_EN.set_paths({"PLL_SIG_0.CGU_PLL_EN"});
   PLL_SIG_0.SSC_PLL_EN.set_paths({"PLL_SIG_0.SSC_PLL_EN"});
   PLL_SIG_0.PLL_EN_2.set_paths({"PLL_SIG_0.PLL_EN_2"});
   PLL_SIG_0.PLL_EN_3.set_paths({"PLL_SIG_0.PLL_EN_3"});
   PLL_SIG_0.PLL_EN_4.set_paths({"PLL_SIG_0.PLL_EN_4"});
   PLL_SIG_0.PLL_EN_5.set_paths({"PLL_SIG_0.PLL_EN_5"});
   PLL_SIG_0.PLL_EN_6.set_paths({"PLL_SIG_0.PLL_EN_6"});
   PLL_SIG_0.PLL_EN_7.set_paths({"PLL_SIG_0.PLL_EN_7"});
   PLL_SIG_0.PLL_EN_8.set_paths({"PLL_SIG_0.PLL_EN_8"});
   PLL_SIG_0.PLL_EN_9.set_paths({"PLL_SIG_0.PLL_EN_9"});
   PLL_SIG_0.PLL_EN_10.set_paths({"PLL_SIG_0.PLL_EN_10"});
   PLL_SIG_0.PLL_EN_11.set_paths({"PLL_SIG_0.PLL_EN_11"});
   PLL_SIG_0.PLL_EN_12.set_paths({"PLL_SIG_0.PLL_EN_12"});
   PLL_SIG_0.PLL_EN_13.set_paths({"PLL_SIG_0.PLL_EN_13"});
   PLL_SIG_0.PLL_EN_14.set_paths({"PLL_SIG_0.PLL_EN_14"});
   PLL_SIG_0.PLL_EN_15.set_paths({"PLL_SIG_0.PLL_EN_15"});

    `ifdef SLA_RAL_COVERAGE
    sla_ral_reg::regname = "PG_CTL";
    `endif
    PG_CTL = pmu_mmr_PG_CTL_reg::type_id::create("PG_CTL", this);
    PG_CTL.set_cfg(16'h0, 16'h0, 16'h0, 8'h7C, 32, 32'b00000000000000000000011001100110);
    PG_CTL.set_space_addr("MSG", 8'h7C);
    PG_CTL.set_space_addr("CR-SB",8'h7C); 
    PG_CTL.set_space("MSG");
    PG_CTL.set_msg_opcode("CR-SB");
    PG_CTL.set_security_policy("security_Policy_group", "PMU_SAI_POLICY0");
    if ( $test$plusargs("PG_CTL:dont_test") ) PG_CTL.set_test_reg(1'b0);
    if (!add_reg( PG_CTL )) begin
      `sla_error(get_name(), ("could not add register PG_CTL"));
    end
   PG_CTL.PG_PGACK_PFETEN_LTCY.set_paths({"PG_CTL.PG_PGACK_PFETEN_LTCY"});
   PG_CTL.PG_STALL_LTCY.set_paths({"PG_CTL.PG_STALL_LTCY"});
   PG_CTL.PUG_STALL_LTCY.set_paths({"PG_CTL.PUG_STALL_LTCY"});
   PG_CTL.IP_FAST_PWRUNGATE_LTCY.set_paths({"PG_CTL.IP_FAST_PWRUNGATE_LTCY"});
   PG_CTL.IP_SLOW_PWRUNGATE_LTCY.set_paths({"PG_CTL.IP_SLOW_PWRUNGATE_LTCY"});

    `ifdef SLA_RAL_COVERAGE
    sla_ral_reg::regname = "IP_PUG_TYPE_0";
    `endif
    IP_PUG_TYPE_0 = pmu_mmr_IP_PUG_TYPE_0_reg::type_id::create("IP_PUG_TYPE_0", this);
    IP_PUG_TYPE_0.set_cfg(16'h0, 16'h0, 16'h0, 8'h80, 32, 32'b00000000000000000000000000000000);
    IP_PUG_TYPE_0.set_space_addr("MSG", 8'h80);
    IP_PUG_TYPE_0.set_space_addr("CR-SB",8'h80); 
    IP_PUG_TYPE_0.set_space("MSG");
    IP_PUG_TYPE_0.set_msg_opcode("CR-SB");
    IP_PUG_TYPE_0.set_security_policy("security_Policy_group", "PMU_SAI_POLICY0");
    if ( $test$plusargs("IP_PUG_TYPE_0:dont_test") ) IP_PUG_TYPE_0.set_test_reg(1'b0);
    if (!add_reg( IP_PUG_TYPE_0 )) begin
      `sla_error(get_name(), ("could not add register IP_PUG_TYPE_0"));
    end
   IP_PUG_TYPE_0.PUG_TYPE.set_paths({"IP_PUG_TYPE_0.PUG_TYPE"});

    `ifdef SLA_RAL_COVERAGE
    sla_ral_reg::regname = "IP_PUG_TYPE_1";
    `endif
    IP_PUG_TYPE_1 = pmu_mmr_IP_PUG_TYPE_1_reg::type_id::create("IP_PUG_TYPE_1", this);
    IP_PUG_TYPE_1.set_cfg(16'h0, 16'h0, 16'h0, 8'h84, 32, 32'b00000000000000000000000000000000);
    IP_PUG_TYPE_1.set_space_addr("MSG", 8'h84);
    IP_PUG_TYPE_1.set_space_addr("CR-SB",8'h84); 
    IP_PUG_TYPE_1.set_space("MSG");
    IP_PUG_TYPE_1.set_msg_opcode("CR-SB");
    IP_PUG_TYPE_1.set_security_policy("security_Policy_group", "PMU_SAI_POLICY0");
    if ( $test$plusargs("IP_PUG_TYPE_1:dont_test") ) IP_PUG_TYPE_1.set_test_reg(1'b0);
    if (!add_reg( IP_PUG_TYPE_1 )) begin
      `sla_error(get_name(), ("could not add register IP_PUG_TYPE_1"));
    end
   IP_PUG_TYPE_1.PUG_TYPE.set_paths({"IP_PUG_TYPE_1.PUG_TYPE"});

    `ifdef SLA_RAL_COVERAGE
    sla_ral_reg::regname = "IP_PG_REQ_STS_0";
    `endif
    IP_PG_REQ_STS_0 = pmu_mmr_IP_PG_REQ_STS_0_reg::type_id::create("IP_PG_REQ_STS_0", this);
    IP_PG_REQ_STS_0.set_cfg(16'h0, 16'h0, 16'h0, 8'h90, 32, 32'b00000000000000000000000000000000);
    IP_PG_REQ_STS_0.set_space_addr("MSG", 8'h90);
    IP_PG_REQ_STS_0.set_space_addr("CR-SB",8'h90); 
    IP_PG_REQ_STS_0.set_space("MSG");
    IP_PG_REQ_STS_0.set_msg_opcode("CR-SB");
    IP_PG_REQ_STS_0.set_security_policy("security_Policy_group", "PMU_SAI_POLICY0");
    if ( $test$plusargs("IP_PG_REQ_STS_0:dont_test") ) IP_PG_REQ_STS_0.set_test_reg(1'b0);
    if (!add_reg( IP_PG_REQ_STS_0 )) begin
      `sla_error(get_name(), ("could not add register IP_PG_REQ_STS_0"));
    end
   IP_PG_REQ_STS_0.PG_REQ_STS.set_paths({"IP_PG_REQ_STS_0.PG_REQ_STS"});

    `ifdef SLA_RAL_COVERAGE
    sla_ral_reg::regname = "IP_PG_REQ_STS_1";
    `endif
    IP_PG_REQ_STS_1 = pmu_mmr_IP_PG_REQ_STS_1_reg::type_id::create("IP_PG_REQ_STS_1", this);
    IP_PG_REQ_STS_1.set_cfg(16'h0, 16'h0, 16'h0, 8'h94, 32, 32'b00000000000000000000000000000000);
    IP_PG_REQ_STS_1.set_space_addr("MSG", 8'h94);
    IP_PG_REQ_STS_1.set_space_addr("CR-SB",8'h94); 
    IP_PG_REQ_STS_1.set_space("MSG");
    IP_PG_REQ_STS_1.set_msg_opcode("CR-SB");
    IP_PG_REQ_STS_1.set_security_policy("security_Policy_group", "PMU_SAI_POLICY0");
    if ( $test$plusargs("IP_PG_REQ_STS_1:dont_test") ) IP_PG_REQ_STS_1.set_test_reg(1'b0);
    if (!add_reg( IP_PG_REQ_STS_1 )) begin
      `sla_error(get_name(), ("could not add register IP_PG_REQ_STS_1"));
    end
   IP_PG_REQ_STS_1.PG_REQ_STS.set_paths({"IP_PG_REQ_STS_1.PG_REQ_STS"});

    `ifdef SLA_RAL_COVERAGE
    sla_ral_reg::regname = "IP_PWR_STS_0";
    `endif
    IP_PWR_STS_0 = pmu_mmr_IP_PWR_STS_0_reg::type_id::create("IP_PWR_STS_0", this);
    IP_PWR_STS_0.set_cfg(16'h0, 16'h0, 16'h0, 12'h0A0, 32, 32'b00000000000000000000000000000000);
    IP_PWR_STS_0.set_space_addr("MSG", 12'h0A0);
    IP_PWR_STS_0.set_space_addr("CR-SB",12'h0A0); 
    IP_PWR_STS_0.set_space("MSG");
    IP_PWR_STS_0.set_msg_opcode("CR-SB");
    IP_PWR_STS_0.set_security_policy("security_Policy_group", "PMU_SAI_POLICY0");
    if ( $test$plusargs("IP_PWR_STS_0:dont_test") ) IP_PWR_STS_0.set_test_reg(1'b0);
    if (!add_reg( IP_PWR_STS_0 )) begin
      `sla_error(get_name(), ("could not add register IP_PWR_STS_0"));
    end
   IP_PWR_STS_0.IP_PWR_STS.set_paths({"IP_PWR_STS_0.IP_PWR_STS"});

    `ifdef SLA_RAL_COVERAGE
    sla_ral_reg::regname = "IP_PWR_STS_1";
    `endif
    IP_PWR_STS_1 = pmu_mmr_IP_PWR_STS_1_reg::type_id::create("IP_PWR_STS_1", this);
    IP_PWR_STS_1.set_cfg(16'h0, 16'h0, 16'h0, 12'h0A4, 32, 32'b00000000000000000000000000000000);
    IP_PWR_STS_1.set_space_addr("MSG", 12'h0A4);
    IP_PWR_STS_1.set_space_addr("CR-SB",12'h0A4); 
    IP_PWR_STS_1.set_space("MSG");
    IP_PWR_STS_1.set_msg_opcode("CR-SB");
    IP_PWR_STS_1.set_security_policy("security_Policy_group", "PMU_SAI_POLICY0");
    if ( $test$plusargs("IP_PWR_STS_1:dont_test") ) IP_PWR_STS_1.set_test_reg(1'b0);
    if (!add_reg( IP_PWR_STS_1 )) begin
      `sla_error(get_name(), ("could not add register IP_PWR_STS_1"));
    end
   IP_PWR_STS_1.IP_PWR_STS.set_paths({"IP_PWR_STS_1.IP_PWR_STS"});

    `ifdef SLA_RAL_COVERAGE
    sla_ral_reg::regname = "IP_FET_EN_ACK_STS_0";
    `endif
    IP_FET_EN_ACK_STS_0 = pmu_mmr_IP_FET_EN_ACK_STS_0_reg::type_id::create("IP_FET_EN_ACK_STS_0", this);
    IP_FET_EN_ACK_STS_0.set_cfg(16'h0, 16'h0, 16'h0, 12'h0B0, 32, 32'b00000000000000000000000000000000);
    IP_FET_EN_ACK_STS_0.set_space_addr("MSG", 12'h0B0);
    IP_FET_EN_ACK_STS_0.set_space_addr("CR-SB",12'h0B0); 
    IP_FET_EN_ACK_STS_0.set_space("MSG");
    IP_FET_EN_ACK_STS_0.set_msg_opcode("CR-SB");
    IP_FET_EN_ACK_STS_0.set_security_policy("security_Policy_group", "PMU_SAI_POLICY0");
    if ( $test$plusargs("IP_FET_EN_ACK_STS_0:dont_test") ) IP_FET_EN_ACK_STS_0.set_test_reg(1'b0);
    if (!add_reg( IP_FET_EN_ACK_STS_0 )) begin
      `sla_error(get_name(), ("could not add register IP_FET_EN_ACK_STS_0"));
    end
   IP_FET_EN_ACK_STS_0.FET_EN_ACK_STS.set_paths({"IP_FET_EN_ACK_STS_0.FET_EN_ACK_STS"});

    `ifdef SLA_RAL_COVERAGE
    sla_ral_reg::regname = "IP_FET_EN_ACK_STS_1";
    `endif
    IP_FET_EN_ACK_STS_1 = pmu_mmr_IP_FET_EN_ACK_STS_1_reg::type_id::create("IP_FET_EN_ACK_STS_1", this);
    IP_FET_EN_ACK_STS_1.set_cfg(16'h0, 16'h0, 16'h0, 12'h0B4, 32, 32'b00000000000000000000000000000000);
    IP_FET_EN_ACK_STS_1.set_space_addr("MSG", 12'h0B4);
    IP_FET_EN_ACK_STS_1.set_space_addr("CR-SB",12'h0B4); 
    IP_FET_EN_ACK_STS_1.set_space("MSG");
    IP_FET_EN_ACK_STS_1.set_msg_opcode("CR-SB");
    IP_FET_EN_ACK_STS_1.set_security_policy("security_Policy_group", "PMU_SAI_POLICY0");
    if ( $test$plusargs("IP_FET_EN_ACK_STS_1:dont_test") ) IP_FET_EN_ACK_STS_1.set_test_reg(1'b0);
    if (!add_reg( IP_FET_EN_ACK_STS_1 )) begin
      `sla_error(get_name(), ("could not add register IP_FET_EN_ACK_STS_1"));
    end
   IP_FET_EN_ACK_STS_1.FET_EN_ACK_STS.set_paths({"IP_FET_EN_ACK_STS_1.FET_EN_ACK_STS"});

    `ifdef SLA_RAL_COVERAGE
    sla_ral_reg::regname = "IP_STS_MASK_0";
    `endif
    IP_STS_MASK_0 = pmu_mmr_IP_STS_MASK_0_reg::type_id::create("IP_STS_MASK_0", this);
    IP_STS_MASK_0.set_cfg(16'h0, 16'h0, 16'h0, 12'h0C0, 32, 32'b00000000000000000000000000000000);
    IP_STS_MASK_0.set_space_addr("MSG", 12'h0C0);
    IP_STS_MASK_0.set_space_addr("CR-SB",12'h0C0); 
    IP_STS_MASK_0.set_space("MSG");
    IP_STS_MASK_0.set_msg_opcode("CR-SB");
    IP_STS_MASK_0.set_security_policy("security_Policy_group", "PMU_SAI_POLICY0");
    if ( $test$plusargs("IP_STS_MASK_0:dont_test") ) IP_STS_MASK_0.set_test_reg(1'b0);
    if (!add_reg( IP_STS_MASK_0 )) begin
      `sla_error(get_name(), ("could not add register IP_STS_MASK_0"));
    end
   IP_STS_MASK_0.IP_STS_MASK.set_paths({"IP_STS_MASK_0.IP_STS_MASK"});

    `ifdef SLA_RAL_COVERAGE
    sla_ral_reg::regname = "IP_STS_MASK_1";
    `endif
    IP_STS_MASK_1 = pmu_mmr_IP_STS_MASK_1_reg::type_id::create("IP_STS_MASK_1", this);
    IP_STS_MASK_1.set_cfg(16'h0, 16'h0, 16'h0, 12'h0C4, 32, 32'b00000000000000000000000000000000);
    IP_STS_MASK_1.set_space_addr("MSG", 12'h0C4);
    IP_STS_MASK_1.set_space_addr("CR-SB",12'h0C4); 
    IP_STS_MASK_1.set_space("MSG");
    IP_STS_MASK_1.set_msg_opcode("CR-SB");
    IP_STS_MASK_1.set_security_policy("security_Policy_group", "PMU_SAI_POLICY0");
    if ( $test$plusargs("IP_STS_MASK_1:dont_test") ) IP_STS_MASK_1.set_test_reg(1'b0);
    if (!add_reg( IP_STS_MASK_1 )) begin
      `sla_error(get_name(), ("could not add register IP_STS_MASK_1"));
    end
   IP_STS_MASK_1.IP_STS_MASK.set_paths({"IP_STS_MASK_1.IP_STS_MASK"});

    `ifdef SLA_RAL_COVERAGE
    sla_ral_reg::regname = "IP_WAKE_CTL_0";
    `endif
    IP_WAKE_CTL_0 = pmu_mmr_IP_WAKE_CTL_0_reg::type_id::create("IP_WAKE_CTL_0", this);
    IP_WAKE_CTL_0.set_cfg(16'h0, 16'h0, 16'h0, 12'h0D0, 32, 32'b00000000000000000000000000000000);
    IP_WAKE_CTL_0.set_space_addr("MSG", 12'h0D0);
    IP_WAKE_CTL_0.set_space_addr("CR-SB",12'h0D0); 
    IP_WAKE_CTL_0.set_space("MSG");
    IP_WAKE_CTL_0.set_msg_opcode("CR-SB");
    IP_WAKE_CTL_0.set_security_policy("security_Policy_group", "PMU_SAI_POLICY0");
    if ( $test$plusargs("IP_WAKE_CTL_0:dont_test") ) IP_WAKE_CTL_0.set_test_reg(1'b0);
    if (!add_reg( IP_WAKE_CTL_0 )) begin
      `sla_error(get_name(), ("could not add register IP_WAKE_CTL_0"));
    end
   IP_WAKE_CTL_0.DRV_PG_WAKE.set_paths({"IP_WAKE_CTL_0.DRV_PG_WAKE"});

    `ifdef SLA_RAL_COVERAGE
    sla_ral_reg::regname = "IP_WAKE_CTL_1";
    `endif
    IP_WAKE_CTL_1 = pmu_mmr_IP_WAKE_CTL_1_reg::type_id::create("IP_WAKE_CTL_1", this);
    IP_WAKE_CTL_1.set_cfg(16'h0, 16'h0, 16'h0, 12'h0D4, 32, 32'b00000000000000000000000000000000);
    IP_WAKE_CTL_1.set_space_addr("MSG", 12'h0D4);
    IP_WAKE_CTL_1.set_space_addr("CR-SB",12'h0D4); 
    IP_WAKE_CTL_1.set_space("MSG");
    IP_WAKE_CTL_1.set_msg_opcode("CR-SB");
    IP_WAKE_CTL_1.set_security_policy("security_Policy_group", "PMU_SAI_POLICY0");
    if ( $test$plusargs("IP_WAKE_CTL_1:dont_test") ) IP_WAKE_CTL_1.set_test_reg(1'b0);
    if (!add_reg( IP_WAKE_CTL_1 )) begin
      `sla_error(get_name(), ("could not add register IP_WAKE_CTL_1"));
    end
   IP_WAKE_CTL_1.DRV_PG_WAKE.set_paths({"IP_WAKE_CTL_1.DRV_PG_WAKE"});

    `ifdef SLA_RAL_COVERAGE
    sla_ral_reg::regname = "IP_SIDE_POK_STS_0";
    `endif
    IP_SIDE_POK_STS_0 = pmu_mmr_IP_SIDE_POK_STS_0_reg::type_id::create("IP_SIDE_POK_STS_0", this);
    IP_SIDE_POK_STS_0.set_cfg(16'h0, 16'h0, 16'h0, 12'h0E0, 32, 32'b00000000000000000000000000000000);
    IP_SIDE_POK_STS_0.set_space_addr("MSG", 12'h0E0);
    IP_SIDE_POK_STS_0.set_space_addr("CR-SB",12'h0E0); 
    IP_SIDE_POK_STS_0.set_space("MSG");
    IP_SIDE_POK_STS_0.set_msg_opcode("CR-SB");
    IP_SIDE_POK_STS_0.set_security_policy("security_Policy_group", "PMU_SAI_POLICY0");
    if ( $test$plusargs("IP_SIDE_POK_STS_0:dont_test") ) IP_SIDE_POK_STS_0.set_test_reg(1'b0);
    if (!add_reg( IP_SIDE_POK_STS_0 )) begin
      `sla_error(get_name(), ("could not add register IP_SIDE_POK_STS_0"));
    end
   IP_SIDE_POK_STS_0.IP_SIDE_POK.set_paths({"IP_SIDE_POK_STS_0.IP_SIDE_POK"});

    `ifdef SLA_RAL_COVERAGE
    sla_ral_reg::regname = "IP_SIDE_POK_STS_1";
    `endif
    IP_SIDE_POK_STS_1 = pmu_mmr_IP_SIDE_POK_STS_1_reg::type_id::create("IP_SIDE_POK_STS_1", this);
    IP_SIDE_POK_STS_1.set_cfg(16'h0, 16'h0, 16'h0, 12'h0E4, 32, 32'b00000000000000000000000000000000);
    IP_SIDE_POK_STS_1.set_space_addr("MSG", 12'h0E4);
    IP_SIDE_POK_STS_1.set_space_addr("CR-SB",12'h0E4); 
    IP_SIDE_POK_STS_1.set_space("MSG");
    IP_SIDE_POK_STS_1.set_msg_opcode("CR-SB");
    IP_SIDE_POK_STS_1.set_security_policy("security_Policy_group", "PMU_SAI_POLICY0");
    if ( $test$plusargs("IP_SIDE_POK_STS_1:dont_test") ) IP_SIDE_POK_STS_1.set_test_reg(1'b0);
    if (!add_reg( IP_SIDE_POK_STS_1 )) begin
      `sla_error(get_name(), ("could not add register IP_SIDE_POK_STS_1"));
    end
   IP_SIDE_POK_STS_1.IP_SIDE_POK.set_paths({"IP_SIDE_POK_STS_1.IP_SIDE_POK"});

    `ifdef SLA_RAL_COVERAGE
    sla_ral_reg::regname = "IP_PRIM_POK_STS_0";
    `endif
    IP_PRIM_POK_STS_0 = pmu_mmr_IP_PRIM_POK_STS_0_reg::type_id::create("IP_PRIM_POK_STS_0", this);
    IP_PRIM_POK_STS_0.set_cfg(16'h0, 16'h0, 16'h0, 12'h0F0, 32, 32'b00000000000000000000000000000000);
    IP_PRIM_POK_STS_0.set_space_addr("MSG", 12'h0F0);
    IP_PRIM_POK_STS_0.set_space_addr("CR-SB",12'h0F0); 
    IP_PRIM_POK_STS_0.set_space("MSG");
    IP_PRIM_POK_STS_0.set_msg_opcode("CR-SB");
    IP_PRIM_POK_STS_0.set_security_policy("security_Policy_group", "PMU_SAI_POLICY0");
    if ( $test$plusargs("IP_PRIM_POK_STS_0:dont_test") ) IP_PRIM_POK_STS_0.set_test_reg(1'b0);
    if (!add_reg( IP_PRIM_POK_STS_0 )) begin
      `sla_error(get_name(), ("could not add register IP_PRIM_POK_STS_0"));
    end
   IP_PRIM_POK_STS_0.IP_PRIM_POK.set_paths({"IP_PRIM_POK_STS_0.IP_PRIM_POK"});

    `ifdef SLA_RAL_COVERAGE
    sla_ral_reg::regname = "IP_PRIM_POK_STS_1";
    `endif
    IP_PRIM_POK_STS_1 = pmu_mmr_IP_PRIM_POK_STS_1_reg::type_id::create("IP_PRIM_POK_STS_1", this);
    IP_PRIM_POK_STS_1.set_cfg(16'h0, 16'h0, 16'h0, 12'h0F4, 32, 32'b00000000000000000000000000000000);
    IP_PRIM_POK_STS_1.set_space_addr("MSG", 12'h0F4);
    IP_PRIM_POK_STS_1.set_space_addr("CR-SB",12'h0F4); 
    IP_PRIM_POK_STS_1.set_space("MSG");
    IP_PRIM_POK_STS_1.set_msg_opcode("CR-SB");
    IP_PRIM_POK_STS_1.set_security_policy("security_Policy_group", "PMU_SAI_POLICY0");
    if ( $test$plusargs("IP_PRIM_POK_STS_1:dont_test") ) IP_PRIM_POK_STS_1.set_test_reg(1'b0);
    if (!add_reg( IP_PRIM_POK_STS_1 )) begin
      `sla_error(get_name(), ("could not add register IP_PRIM_POK_STS_1"));
    end
   IP_PRIM_POK_STS_1.IP_PRIM_POK.set_paths({"IP_PRIM_POK_STS_1.IP_PRIM_POK"});

    `ifdef SLA_RAL_COVERAGE
    sla_ral_reg::regname = "IP_READY_STS_0";
    `endif
    IP_READY_STS_0 = pmu_mmr_IP_READY_STS_0_reg::type_id::create("IP_READY_STS_0", this);
    IP_READY_STS_0.set_cfg(16'h0, 16'h0, 16'h0, 12'h100, 32, 32'b00000000000000000000000000000000);
    IP_READY_STS_0.set_space_addr("MSG", 12'h100);
    IP_READY_STS_0.set_space_addr("CR-SB",12'h100); 
    IP_READY_STS_0.set_space("MSG");
    IP_READY_STS_0.set_msg_opcode("CR-SB");
    IP_READY_STS_0.set_security_policy("security_Policy_group", "PMU_SAI_POLICY0");
    if ( $test$plusargs("IP_READY_STS_0:dont_test") ) IP_READY_STS_0.set_test_reg(1'b0);
    if (!add_reg( IP_READY_STS_0 )) begin
      `sla_error(get_name(), ("could not add register IP_READY_STS_0"));
    end
   IP_READY_STS_0.IP_READY_STS_0_0.set_paths({"IP_READY_STS_0.IP_READY_STS_0_0"});
   IP_READY_STS_0.IP_READY_STS_0_1.set_paths({"IP_READY_STS_0.IP_READY_STS_0_1"});
   IP_READY_STS_0.IP_READY_STS_0_2.set_paths({"IP_READY_STS_0.IP_READY_STS_0_2"});
   IP_READY_STS_0.IP_READY_STS_0_3.set_paths({"IP_READY_STS_0.IP_READY_STS_0_3"});
   IP_READY_STS_0.IP_READY_STS_0_4.set_paths({"IP_READY_STS_0.IP_READY_STS_0_4"});
   IP_READY_STS_0.IP_READY_STS_0_5.set_paths({"IP_READY_STS_0.IP_READY_STS_0_5"});
   IP_READY_STS_0.IP_READY_STS_0_6.set_paths({"IP_READY_STS_0.IP_READY_STS_0_6"});
   IP_READY_STS_0.IP_READY_STS_0_7.set_paths({"IP_READY_STS_0.IP_READY_STS_0_7"});
   IP_READY_STS_0.IP_READY_STS_0_8.set_paths({"IP_READY_STS_0.IP_READY_STS_0_8"});
   IP_READY_STS_0.IP_READY_STS_0_9.set_paths({"IP_READY_STS_0.IP_READY_STS_0_9"});
   IP_READY_STS_0.IP_READY_STS_0_10.set_paths({"IP_READY_STS_0.IP_READY_STS_0_10"});
   IP_READY_STS_0.IP_READY_STS_0_11.set_paths({"IP_READY_STS_0.IP_READY_STS_0_11"});
   IP_READY_STS_0.IP_READY_STS_0_12.set_paths({"IP_READY_STS_0.IP_READY_STS_0_12"});
   IP_READY_STS_0.IP_READY_STS_0_13.set_paths({"IP_READY_STS_0.IP_READY_STS_0_13"});
   IP_READY_STS_0.IP_READY_STS_0_14.set_paths({"IP_READY_STS_0.IP_READY_STS_0_14"});
   IP_READY_STS_0.IP_READY_STS_0_15.set_paths({"IP_READY_STS_0.IP_READY_STS_0_15"});
   IP_READY_STS_0.IP_READY_STS_0_16.set_paths({"IP_READY_STS_0.IP_READY_STS_0_16"});
   IP_READY_STS_0.IP_READY_STS_0_17.set_paths({"IP_READY_STS_0.IP_READY_STS_0_17"});
   IP_READY_STS_0.IP_READY_STS_0_18.set_paths({"IP_READY_STS_0.IP_READY_STS_0_18"});
   IP_READY_STS_0.IP_READY_STS_0_19.set_paths({"IP_READY_STS_0.IP_READY_STS_0_19"});
   IP_READY_STS_0.IP_READY_STS_0_20.set_paths({"IP_READY_STS_0.IP_READY_STS_0_20"});
   IP_READY_STS_0.IP_READY_STS_0_21.set_paths({"IP_READY_STS_0.IP_READY_STS_0_21"});
   IP_READY_STS_0.IP_READY_STS_0_22.set_paths({"IP_READY_STS_0.IP_READY_STS_0_22"});
   IP_READY_STS_0.IP_READY_STS_0_23.set_paths({"IP_READY_STS_0.IP_READY_STS_0_23"});
   IP_READY_STS_0.IP_READY_STS_0_24.set_paths({"IP_READY_STS_0.IP_READY_STS_0_24"});
   IP_READY_STS_0.IP_READY_STS_0_25.set_paths({"IP_READY_STS_0.IP_READY_STS_0_25"});
   IP_READY_STS_0.IP_READY_STS_0_26.set_paths({"IP_READY_STS_0.IP_READY_STS_0_26"});
   IP_READY_STS_0.IP_READY_STS_0_27.set_paths({"IP_READY_STS_0.IP_READY_STS_0_27"});
   IP_READY_STS_0.IP_READY_STS_0_28.set_paths({"IP_READY_STS_0.IP_READY_STS_0_28"});
   IP_READY_STS_0.IP_READY_STS_0_29.set_paths({"IP_READY_STS_0.IP_READY_STS_0_29"});
   IP_READY_STS_0.IP_READY_STS_0_30.set_paths({"IP_READY_STS_0.IP_READY_STS_0_30"});
   IP_READY_STS_0.IP_READY_STS_0_31.set_paths({"IP_READY_STS_0.IP_READY_STS_0_31"});

    `ifdef SLA_RAL_COVERAGE
    sla_ral_reg::regname = "IP_READY_STS_1";
    `endif
    IP_READY_STS_1 = pmu_mmr_IP_READY_STS_1_reg::type_id::create("IP_READY_STS_1", this);
    IP_READY_STS_1.set_cfg(16'h0, 16'h0, 16'h0, 12'h104, 32, 32'b00000000000000000000000000000000);
    IP_READY_STS_1.set_space_addr("MSG", 12'h104);
    IP_READY_STS_1.set_space_addr("CR-SB",12'h104); 
    IP_READY_STS_1.set_space("MSG");
    IP_READY_STS_1.set_msg_opcode("CR-SB");
    IP_READY_STS_1.set_security_policy("security_Policy_group", "PMU_SAI_POLICY0");
    if ( $test$plusargs("IP_READY_STS_1:dont_test") ) IP_READY_STS_1.set_test_reg(1'b0);
    if (!add_reg( IP_READY_STS_1 )) begin
      `sla_error(get_name(), ("could not add register IP_READY_STS_1"));
    end
   IP_READY_STS_1.IP_READY_STS_1_0.set_paths({"IP_READY_STS_1.IP_READY_STS_1_0"});
   IP_READY_STS_1.IP_READY_STS_1_1.set_paths({"IP_READY_STS_1.IP_READY_STS_1_1"});
   IP_READY_STS_1.IP_READY_STS_1_2.set_paths({"IP_READY_STS_1.IP_READY_STS_1_2"});
   IP_READY_STS_1.IP_READY_STS_1_3.set_paths({"IP_READY_STS_1.IP_READY_STS_1_3"});
   IP_READY_STS_1.IP_READY_STS_1_4.set_paths({"IP_READY_STS_1.IP_READY_STS_1_4"});
   IP_READY_STS_1.IP_READY_STS_1_5.set_paths({"IP_READY_STS_1.IP_READY_STS_1_5"});
   IP_READY_STS_1.IP_READY_STS_1_6.set_paths({"IP_READY_STS_1.IP_READY_STS_1_6"});
   IP_READY_STS_1.IP_READY_STS_1_7.set_paths({"IP_READY_STS_1.IP_READY_STS_1_7"});
   IP_READY_STS_1.IP_READY_STS_1_8.set_paths({"IP_READY_STS_1.IP_READY_STS_1_8"});
   IP_READY_STS_1.IP_READY_STS_1_9.set_paths({"IP_READY_STS_1.IP_READY_STS_1_9"});
   IP_READY_STS_1.IP_READY_STS_1_10.set_paths({"IP_READY_STS_1.IP_READY_STS_1_10"});
   IP_READY_STS_1.IP_READY_STS_1_11.set_paths({"IP_READY_STS_1.IP_READY_STS_1_11"});
   IP_READY_STS_1.IP_READY_STS_1_12.set_paths({"IP_READY_STS_1.IP_READY_STS_1_12"});
   IP_READY_STS_1.IP_READY_STS_1_13.set_paths({"IP_READY_STS_1.IP_READY_STS_1_13"});
   IP_READY_STS_1.IP_READY_STS_1_14.set_paths({"IP_READY_STS_1.IP_READY_STS_1_14"});
   IP_READY_STS_1.IP_READY_STS_1_15.set_paths({"IP_READY_STS_1.IP_READY_STS_1_15"});
   IP_READY_STS_1.IP_READY_STS_1_16.set_paths({"IP_READY_STS_1.IP_READY_STS_1_16"});
   IP_READY_STS_1.IP_READY_STS_1_17.set_paths({"IP_READY_STS_1.IP_READY_STS_1_17"});
   IP_READY_STS_1.IP_READY_STS_1_18.set_paths({"IP_READY_STS_1.IP_READY_STS_1_18"});
   IP_READY_STS_1.IP_READY_STS_1_19.set_paths({"IP_READY_STS_1.IP_READY_STS_1_19"});
   IP_READY_STS_1.IP_READY_STS_1_20.set_paths({"IP_READY_STS_1.IP_READY_STS_1_20"});
   IP_READY_STS_1.IP_READY_STS_1_21.set_paths({"IP_READY_STS_1.IP_READY_STS_1_21"});
   IP_READY_STS_1.IP_READY_STS_1_22.set_paths({"IP_READY_STS_1.IP_READY_STS_1_22"});
   IP_READY_STS_1.IP_READY_STS_1_23.set_paths({"IP_READY_STS_1.IP_READY_STS_1_23"});
   IP_READY_STS_1.IP_READY_STS_1_24.set_paths({"IP_READY_STS_1.IP_READY_STS_1_24"});
   IP_READY_STS_1.IP_READY_STS_1_25.set_paths({"IP_READY_STS_1.IP_READY_STS_1_25"});
   IP_READY_STS_1.IP_READY_STS_1_26.set_paths({"IP_READY_STS_1.IP_READY_STS_1_26"});
   IP_READY_STS_1.IP_READY_STS_1_27.set_paths({"IP_READY_STS_1.IP_READY_STS_1_27"});
   IP_READY_STS_1.IP_READY_STS_1_28.set_paths({"IP_READY_STS_1.IP_READY_STS_1_28"});
   IP_READY_STS_1.IP_READY_STS_1_29.set_paths({"IP_READY_STS_1.IP_READY_STS_1_29"});
   IP_READY_STS_1.IP_READY_STS_1_30.set_paths({"IP_READY_STS_1.IP_READY_STS_1_30"});
   IP_READY_STS_1.IP_READY_STS_1_31.set_paths({"IP_READY_STS_1.IP_READY_STS_1_31"});

    `ifdef SLA_RAL_COVERAGE
    sla_ral_reg::regname = "IP_MSG_EN_0";
    `endif
    IP_MSG_EN_0 = pmu_mmr_IP_MSG_EN_0_reg::type_id::create("IP_MSG_EN_0", this);
    IP_MSG_EN_0.set_cfg(16'h0, 16'h0, 16'h0, 12'h110, 32, 32'b00000000000000000000000000000000);
    IP_MSG_EN_0.set_space_addr("MSG", 12'h110);
    IP_MSG_EN_0.set_space_addr("CR-SB",12'h110); 
    IP_MSG_EN_0.set_space("MSG");
    IP_MSG_EN_0.set_msg_opcode("CR-SB");
    IP_MSG_EN_0.set_security_policy("security_Policy_group", "PMU_SAI_POLICY0");
    if ( $test$plusargs("IP_MSG_EN_0:dont_test") ) IP_MSG_EN_0.set_test_reg(1'b0);
    if (!add_reg( IP_MSG_EN_0 )) begin
      `sla_error(get_name(), ("could not add register IP_MSG_EN_0"));
    end
   IP_MSG_EN_0.SB_MSG_EN.set_paths({"IP_MSG_EN_0.SB_MSG_EN"});

    `ifdef SLA_RAL_COVERAGE
    sla_ral_reg::regname = "IP_MSG_EN_1";
    `endif
    IP_MSG_EN_1 = pmu_mmr_IP_MSG_EN_1_reg::type_id::create("IP_MSG_EN_1", this);
    IP_MSG_EN_1.set_cfg(16'h0, 16'h0, 16'h0, 12'h114, 32, 32'b00000000000000000000000000000000);
    IP_MSG_EN_1.set_space_addr("MSG", 12'h114);
    IP_MSG_EN_1.set_space_addr("CR-SB",12'h114); 
    IP_MSG_EN_1.set_space("MSG");
    IP_MSG_EN_1.set_msg_opcode("CR-SB");
    IP_MSG_EN_1.set_security_policy("security_Policy_group", "PMU_SAI_POLICY0");
    if ( $test$plusargs("IP_MSG_EN_1:dont_test") ) IP_MSG_EN_1.set_test_reg(1'b0);
    if (!add_reg( IP_MSG_EN_1 )) begin
      `sla_error(get_name(), ("could not add register IP_MSG_EN_1"));
    end
   IP_MSG_EN_1.SB_MSG_EN.set_paths({"IP_MSG_EN_1.SB_MSG_EN"});

    `ifdef SLA_RAL_COVERAGE
    sla_ral_reg::regname = "SBI_CHMSG_CTRL";
    `endif
    SBI_CHMSG_CTRL = pmu_mmr_SBI_CHMSG_CTRL_reg::type_id::create("SBI_CHMSG_CTRL", this);
    SBI_CHMSG_CTRL.set_cfg(16'h0, 16'h0, 16'h0, 12'h120, 32, 32'b00000000000000000000000000000000);
    SBI_CHMSG_CTRL.set_space_addr("MSG", 12'h120);
    SBI_CHMSG_CTRL.set_space_addr("CR-SB",12'h120); 
    SBI_CHMSG_CTRL.set_space("MSG");
    SBI_CHMSG_CTRL.set_msg_opcode("CR-SB");
    SBI_CHMSG_CTRL.set_security_policy("security_Policy_group", "PMU_SAI_POLICY0");
    if ( $test$plusargs("SBI_CHMSG_CTRL:dont_test") ) SBI_CHMSG_CTRL.set_test_reg(1'b0);
    if (!add_reg( SBI_CHMSG_CTRL )) begin
      `sla_error(get_name(), ("could not add register SBI_CHMSG_CTRL"));
    end
   SBI_CHMSG_CTRL.SEND_CHMSG_SB.set_paths({"SBI_CHMSG_CTRL.SEND_CHMSG_SB"});
   SBI_CHMSG_CTRL.FPGPOK_TYPE.set_paths({"SBI_CHMSG_CTRL.FPGPOK_TYPE"});
   SBI_CHMSG_CTRL.NO_EXH.set_paths({"SBI_CHMSG_CTRL.NO_EXH"});
   SBI_CHMSG_CTRL.BP_RP_TYPE.set_paths({"SBI_CHMSG_CTRL.BP_RP_TYPE"});
   SBI_CHMSG_CTRL.BP_RP_PREP_TYPE.set_paths({"SBI_CHMSG_CTRL.BP_RP_PREP_TYPE"});
   SBI_CHMSG_CTRL.BP_RP_PREP_ACK.set_paths({"SBI_CHMSG_CTRL.BP_RP_PREP_ACK"});
   SBI_CHMSG_CTRL.CHMSG_RS.set_paths({"SBI_CHMSG_CTRL.CHMSG_RS"});
   SBI_CHMSG_CTRL.CHMSG_MT.set_paths({"SBI_CHMSG_CTRL.CHMSG_MT"});

    `ifdef SLA_RAL_COVERAGE
    sla_ral_reg::regname = "SBI_SETID_DW1";
    `endif
    SBI_SETID_DW1 = pmu_mmr_SBI_SETID_DW1_reg::type_id::create("SBI_SETID_DW1", this);
    SBI_SETID_DW1.set_cfg(16'h0, 16'h0, 16'h0, 12'h124, 32, 32'b00000000000000000000000000000000);
    SBI_SETID_DW1.set_space_addr("MSG", 12'h124);
    SBI_SETID_DW1.set_space_addr("CR-SB",12'h124); 
    SBI_SETID_DW1.set_space("MSG");
    SBI_SETID_DW1.set_msg_opcode("CR-SB");
    SBI_SETID_DW1.set_security_policy("security_Policy_group", "PMU_SAI_POLICY0");
    if ( $test$plusargs("SBI_SETID_DW1:dont_test") ) SBI_SETID_DW1.set_test_reg(1'b0);
    if (!add_reg( SBI_SETID_DW1 )) begin
      `sla_error(get_name(), ("could not add register SBI_SETID_DW1"));
    end
   SBI_SETID_DW1.REVID.set_paths({"SBI_SETID_DW1.REVID"});
   SBI_SETID_DW1.RSVD_15_8.set_paths({"SBI_SETID_DW1.RSVD_15_8"});
   SBI_SETID_DW1.DEVID.set_paths({"SBI_SETID_DW1.DEVID"});

    `ifdef SLA_RAL_COVERAGE
    sla_ral_reg::regname = "SBI_SETID_DW2";
    `endif
    SBI_SETID_DW2 = pmu_mmr_SBI_SETID_DW2_reg::type_id::create("SBI_SETID_DW2", this);
    SBI_SETID_DW2.set_cfg(16'h0, 16'h0, 16'h0, 12'h128, 32, 32'b00000000000000000000000000000000);
    SBI_SETID_DW2.set_space_addr("MSG", 12'h128);
    SBI_SETID_DW2.set_space_addr("CR-SB",12'h128); 
    SBI_SETID_DW2.set_space("MSG");
    SBI_SETID_DW2.set_msg_opcode("CR-SB");
    SBI_SETID_DW2.set_security_policy("security_Policy_group", "PMU_SAI_POLICY0");
    if ( $test$plusargs("SBI_SETID_DW2:dont_test") ) SBI_SETID_DW2.set_test_reg(1'b0);
    if (!add_reg( SBI_SETID_DW2 )) begin
      `sla_error(get_name(), ("could not add register SBI_SETID_DW2"));
    end
   SBI_SETID_DW2.PPOP.set_paths({"SBI_SETID_DW2.PPOP"});
   SBI_SETID_DW2.MID.set_paths({"SBI_SETID_DW2.MID"});
   SBI_SETID_DW2.MSID.set_paths({"SBI_SETID_DW2.MSID"});
   SBI_SETID_DW2.DPOP.set_paths({"SBI_SETID_DW2.DPOP"});
   SBI_SETID_DW2.RSVD_31_28.set_paths({"SBI_SETID_DW2.RSVD_31_28"});

    `ifdef SLA_RAL_COVERAGE
    sla_ral_reg::regname = "IP_BP_RP_ACK_STS_0";
    `endif
    IP_BP_RP_ACK_STS_0 = pmu_mmr_IP_BP_RP_ACK_STS_0_reg::type_id::create("IP_BP_RP_ACK_STS_0", this);
    IP_BP_RP_ACK_STS_0.set_cfg(16'h0, 16'h0, 16'h0, 12'h130, 32, 32'b00000000000000000000000000000000);
    IP_BP_RP_ACK_STS_0.set_space_addr("MSG", 12'h130);
    IP_BP_RP_ACK_STS_0.set_space_addr("CR-SB",12'h130); 
    IP_BP_RP_ACK_STS_0.set_space("MSG");
    IP_BP_RP_ACK_STS_0.set_msg_opcode("CR-SB");
    IP_BP_RP_ACK_STS_0.set_security_policy("security_Policy_group", "PMU_SAI_POLICY0");
    if ( $test$plusargs("IP_BP_RP_ACK_STS_0:dont_test") ) IP_BP_RP_ACK_STS_0.set_test_reg(1'b0);
    if (!add_reg( IP_BP_RP_ACK_STS_0 )) begin
      `sla_error(get_name(), ("could not add register IP_BP_RP_ACK_STS_0"));
    end
   IP_BP_RP_ACK_STS_0.IP_BP_RP_ACK_STS_0_0.set_paths({"IP_BP_RP_ACK_STS_0.IP_BP_RP_ACK_STS_0_0"});
   IP_BP_RP_ACK_STS_0.IP_BP_RP_ACK_STS_0_1.set_paths({"IP_BP_RP_ACK_STS_0.IP_BP_RP_ACK_STS_0_1"});
   IP_BP_RP_ACK_STS_0.IP_BP_RP_ACK_STS_0_2.set_paths({"IP_BP_RP_ACK_STS_0.IP_BP_RP_ACK_STS_0_2"});
   IP_BP_RP_ACK_STS_0.IP_BP_RP_ACK_STS_0_3.set_paths({"IP_BP_RP_ACK_STS_0.IP_BP_RP_ACK_STS_0_3"});
   IP_BP_RP_ACK_STS_0.IP_BP_RP_ACK_STS_0_4.set_paths({"IP_BP_RP_ACK_STS_0.IP_BP_RP_ACK_STS_0_4"});
   IP_BP_RP_ACK_STS_0.IP_BP_RP_ACK_STS_0_5.set_paths({"IP_BP_RP_ACK_STS_0.IP_BP_RP_ACK_STS_0_5"});
   IP_BP_RP_ACK_STS_0.IP_BP_RP_ACK_STS_0_6.set_paths({"IP_BP_RP_ACK_STS_0.IP_BP_RP_ACK_STS_0_6"});
   IP_BP_RP_ACK_STS_0.IP_BP_RP_ACK_STS_0_7.set_paths({"IP_BP_RP_ACK_STS_0.IP_BP_RP_ACK_STS_0_7"});
   IP_BP_RP_ACK_STS_0.IP_BP_RP_ACK_STS_0_8.set_paths({"IP_BP_RP_ACK_STS_0.IP_BP_RP_ACK_STS_0_8"});
   IP_BP_RP_ACK_STS_0.IP_BP_RP_ACK_STS_0_9.set_paths({"IP_BP_RP_ACK_STS_0.IP_BP_RP_ACK_STS_0_9"});
   IP_BP_RP_ACK_STS_0.IP_BP_RP_ACK_STS_0_10.set_paths({"IP_BP_RP_ACK_STS_0.IP_BP_RP_ACK_STS_0_10"});
   IP_BP_RP_ACK_STS_0.IP_BP_RP_ACK_STS_0_11.set_paths({"IP_BP_RP_ACK_STS_0.IP_BP_RP_ACK_STS_0_11"});
   IP_BP_RP_ACK_STS_0.IP_BP_RP_ACK_STS_0_12.set_paths({"IP_BP_RP_ACK_STS_0.IP_BP_RP_ACK_STS_0_12"});
   IP_BP_RP_ACK_STS_0.IP_BP_RP_ACK_STS_0_13.set_paths({"IP_BP_RP_ACK_STS_0.IP_BP_RP_ACK_STS_0_13"});
   IP_BP_RP_ACK_STS_0.IP_BP_RP_ACK_STS_0_14.set_paths({"IP_BP_RP_ACK_STS_0.IP_BP_RP_ACK_STS_0_14"});
   IP_BP_RP_ACK_STS_0.IP_BP_RP_ACK_STS_0_15.set_paths({"IP_BP_RP_ACK_STS_0.IP_BP_RP_ACK_STS_0_15"});
   IP_BP_RP_ACK_STS_0.IP_BP_RP_ACK_STS_0_16.set_paths({"IP_BP_RP_ACK_STS_0.IP_BP_RP_ACK_STS_0_16"});
   IP_BP_RP_ACK_STS_0.IP_BP_RP_ACK_STS_0_17.set_paths({"IP_BP_RP_ACK_STS_0.IP_BP_RP_ACK_STS_0_17"});
   IP_BP_RP_ACK_STS_0.IP_BP_RP_ACK_STS_0_18.set_paths({"IP_BP_RP_ACK_STS_0.IP_BP_RP_ACK_STS_0_18"});
   IP_BP_RP_ACK_STS_0.IP_BP_RP_ACK_STS_0_19.set_paths({"IP_BP_RP_ACK_STS_0.IP_BP_RP_ACK_STS_0_19"});
   IP_BP_RP_ACK_STS_0.IP_BP_RP_ACK_STS_0_20.set_paths({"IP_BP_RP_ACK_STS_0.IP_BP_RP_ACK_STS_0_20"});
   IP_BP_RP_ACK_STS_0.IP_BP_RP_ACK_STS_0_21.set_paths({"IP_BP_RP_ACK_STS_0.IP_BP_RP_ACK_STS_0_21"});
   IP_BP_RP_ACK_STS_0.IP_BP_RP_ACK_STS_0_22.set_paths({"IP_BP_RP_ACK_STS_0.IP_BP_RP_ACK_STS_0_22"});
   IP_BP_RP_ACK_STS_0.IP_BP_RP_ACK_STS_0_23.set_paths({"IP_BP_RP_ACK_STS_0.IP_BP_RP_ACK_STS_0_23"});
   IP_BP_RP_ACK_STS_0.IP_BP_RP_ACK_STS_0_24.set_paths({"IP_BP_RP_ACK_STS_0.IP_BP_RP_ACK_STS_0_24"});
   IP_BP_RP_ACK_STS_0.IP_BP_RP_ACK_STS_0_25.set_paths({"IP_BP_RP_ACK_STS_0.IP_BP_RP_ACK_STS_0_25"});
   IP_BP_RP_ACK_STS_0.IP_BP_RP_ACK_STS_0_26.set_paths({"IP_BP_RP_ACK_STS_0.IP_BP_RP_ACK_STS_0_26"});
   IP_BP_RP_ACK_STS_0.IP_BP_RP_ACK_STS_0_27.set_paths({"IP_BP_RP_ACK_STS_0.IP_BP_RP_ACK_STS_0_27"});
   IP_BP_RP_ACK_STS_0.IP_BP_RP_ACK_STS_0_28.set_paths({"IP_BP_RP_ACK_STS_0.IP_BP_RP_ACK_STS_0_28"});
   IP_BP_RP_ACK_STS_0.IP_BP_RP_ACK_STS_0_29.set_paths({"IP_BP_RP_ACK_STS_0.IP_BP_RP_ACK_STS_0_29"});
   IP_BP_RP_ACK_STS_0.IP_BP_RP_ACK_STS_0_30.set_paths({"IP_BP_RP_ACK_STS_0.IP_BP_RP_ACK_STS_0_30"});
   IP_BP_RP_ACK_STS_0.IP_BP_RP_ACK_STS_0_31.set_paths({"IP_BP_RP_ACK_STS_0.IP_BP_RP_ACK_STS_0_31"});

    `ifdef SLA_RAL_COVERAGE
    sla_ral_reg::regname = "IP_BP_RP_ACK_STS_1";
    `endif
    IP_BP_RP_ACK_STS_1 = pmu_mmr_IP_BP_RP_ACK_STS_1_reg::type_id::create("IP_BP_RP_ACK_STS_1", this);
    IP_BP_RP_ACK_STS_1.set_cfg(16'h0, 16'h0, 16'h0, 12'h134, 32, 32'b00000000000000000000000000000000);
    IP_BP_RP_ACK_STS_1.set_space_addr("MSG", 12'h134);
    IP_BP_RP_ACK_STS_1.set_space_addr("CR-SB",12'h134); 
    IP_BP_RP_ACK_STS_1.set_space("MSG");
    IP_BP_RP_ACK_STS_1.set_msg_opcode("CR-SB");
    IP_BP_RP_ACK_STS_1.set_security_policy("security_Policy_group", "PMU_SAI_POLICY0");
    if ( $test$plusargs("IP_BP_RP_ACK_STS_1:dont_test") ) IP_BP_RP_ACK_STS_1.set_test_reg(1'b0);
    if (!add_reg( IP_BP_RP_ACK_STS_1 )) begin
      `sla_error(get_name(), ("could not add register IP_BP_RP_ACK_STS_1"));
    end
   IP_BP_RP_ACK_STS_1.IP_BP_RP_ACK_STS_1_0.set_paths({"IP_BP_RP_ACK_STS_1.IP_BP_RP_ACK_STS_1_0"});
   IP_BP_RP_ACK_STS_1.IP_BP_RP_ACK_STS_1_1.set_paths({"IP_BP_RP_ACK_STS_1.IP_BP_RP_ACK_STS_1_1"});
   IP_BP_RP_ACK_STS_1.IP_BP_RP_ACK_STS_1_2.set_paths({"IP_BP_RP_ACK_STS_1.IP_BP_RP_ACK_STS_1_2"});
   IP_BP_RP_ACK_STS_1.IP_BP_RP_ACK_STS_1_3.set_paths({"IP_BP_RP_ACK_STS_1.IP_BP_RP_ACK_STS_1_3"});
   IP_BP_RP_ACK_STS_1.IP_BP_RP_ACK_STS_1_4.set_paths({"IP_BP_RP_ACK_STS_1.IP_BP_RP_ACK_STS_1_4"});
   IP_BP_RP_ACK_STS_1.IP_BP_RP_ACK_STS_1_5.set_paths({"IP_BP_RP_ACK_STS_1.IP_BP_RP_ACK_STS_1_5"});
   IP_BP_RP_ACK_STS_1.IP_BP_RP_ACK_STS_1_6.set_paths({"IP_BP_RP_ACK_STS_1.IP_BP_RP_ACK_STS_1_6"});
   IP_BP_RP_ACK_STS_1.IP_BP_RP_ACK_STS_1_7.set_paths({"IP_BP_RP_ACK_STS_1.IP_BP_RP_ACK_STS_1_7"});
   IP_BP_RP_ACK_STS_1.IP_BP_RP_ACK_STS_1_8.set_paths({"IP_BP_RP_ACK_STS_1.IP_BP_RP_ACK_STS_1_8"});
   IP_BP_RP_ACK_STS_1.IP_BP_RP_ACK_STS_1_9.set_paths({"IP_BP_RP_ACK_STS_1.IP_BP_RP_ACK_STS_1_9"});
   IP_BP_RP_ACK_STS_1.IP_BP_RP_ACK_STS_1_10.set_paths({"IP_BP_RP_ACK_STS_1.IP_BP_RP_ACK_STS_1_10"});
   IP_BP_RP_ACK_STS_1.IP_BP_RP_ACK_STS_1_11.set_paths({"IP_BP_RP_ACK_STS_1.IP_BP_RP_ACK_STS_1_11"});
   IP_BP_RP_ACK_STS_1.IP_BP_RP_ACK_STS_1_12.set_paths({"IP_BP_RP_ACK_STS_1.IP_BP_RP_ACK_STS_1_12"});
   IP_BP_RP_ACK_STS_1.IP_BP_RP_ACK_STS_1_13.set_paths({"IP_BP_RP_ACK_STS_1.IP_BP_RP_ACK_STS_1_13"});
   IP_BP_RP_ACK_STS_1.IP_BP_RP_ACK_STS_1_14.set_paths({"IP_BP_RP_ACK_STS_1.IP_BP_RP_ACK_STS_1_14"});
   IP_BP_RP_ACK_STS_1.IP_BP_RP_ACK_STS_1_15.set_paths({"IP_BP_RP_ACK_STS_1.IP_BP_RP_ACK_STS_1_15"});
   IP_BP_RP_ACK_STS_1.IP_BP_RP_ACK_STS_1_16.set_paths({"IP_BP_RP_ACK_STS_1.IP_BP_RP_ACK_STS_1_16"});
   IP_BP_RP_ACK_STS_1.IP_BP_RP_ACK_STS_1_17.set_paths({"IP_BP_RP_ACK_STS_1.IP_BP_RP_ACK_STS_1_17"});
   IP_BP_RP_ACK_STS_1.IP_BP_RP_ACK_STS_1_18.set_paths({"IP_BP_RP_ACK_STS_1.IP_BP_RP_ACK_STS_1_18"});
   IP_BP_RP_ACK_STS_1.IP_BP_RP_ACK_STS_1_19.set_paths({"IP_BP_RP_ACK_STS_1.IP_BP_RP_ACK_STS_1_19"});
   IP_BP_RP_ACK_STS_1.IP_BP_RP_ACK_STS_1_20.set_paths({"IP_BP_RP_ACK_STS_1.IP_BP_RP_ACK_STS_1_20"});
   IP_BP_RP_ACK_STS_1.IP_BP_RP_ACK_STS_1_21.set_paths({"IP_BP_RP_ACK_STS_1.IP_BP_RP_ACK_STS_1_21"});
   IP_BP_RP_ACK_STS_1.IP_BP_RP_ACK_STS_1_22.set_paths({"IP_BP_RP_ACK_STS_1.IP_BP_RP_ACK_STS_1_22"});
   IP_BP_RP_ACK_STS_1.IP_BP_RP_ACK_STS_1_23.set_paths({"IP_BP_RP_ACK_STS_1.IP_BP_RP_ACK_STS_1_23"});
   IP_BP_RP_ACK_STS_1.IP_BP_RP_ACK_STS_1_24.set_paths({"IP_BP_RP_ACK_STS_1.IP_BP_RP_ACK_STS_1_24"});
   IP_BP_RP_ACK_STS_1.IP_BP_RP_ACK_STS_1_25.set_paths({"IP_BP_RP_ACK_STS_1.IP_BP_RP_ACK_STS_1_25"});
   IP_BP_RP_ACK_STS_1.IP_BP_RP_ACK_STS_1_26.set_paths({"IP_BP_RP_ACK_STS_1.IP_BP_RP_ACK_STS_1_26"});
   IP_BP_RP_ACK_STS_1.IP_BP_RP_ACK_STS_1_27.set_paths({"IP_BP_RP_ACK_STS_1.IP_BP_RP_ACK_STS_1_27"});
   IP_BP_RP_ACK_STS_1.IP_BP_RP_ACK_STS_1_28.set_paths({"IP_BP_RP_ACK_STS_1.IP_BP_RP_ACK_STS_1_28"});
   IP_BP_RP_ACK_STS_1.IP_BP_RP_ACK_STS_1_29.set_paths({"IP_BP_RP_ACK_STS_1.IP_BP_RP_ACK_STS_1_29"});
   IP_BP_RP_ACK_STS_1.IP_BP_RP_ACK_STS_1_30.set_paths({"IP_BP_RP_ACK_STS_1.IP_BP_RP_ACK_STS_1_30"});
   IP_BP_RP_ACK_STS_1.IP_BP_RP_ACK_STS_1_31.set_paths({"IP_BP_RP_ACK_STS_1.IP_BP_RP_ACK_STS_1_31"});

    `ifdef SLA_RAL_COVERAGE
    sla_ral_reg::regname = "IP_SB_CPL_STS_MASK_0";
    `endif
    IP_SB_CPL_STS_MASK_0 = pmu_mmr_IP_SB_CPL_STS_MASK_0_reg::type_id::create("IP_SB_CPL_STS_MASK_0", this);
    IP_SB_CPL_STS_MASK_0.set_cfg(16'h0, 16'h0, 16'h0, 12'h140, 32, 32'b00000000000000000000000000000000);
    IP_SB_CPL_STS_MASK_0.set_space_addr("MSG", 12'h140);
    IP_SB_CPL_STS_MASK_0.set_space_addr("CR-SB",12'h140); 
    IP_SB_CPL_STS_MASK_0.set_space("MSG");
    IP_SB_CPL_STS_MASK_0.set_msg_opcode("CR-SB");
    IP_SB_CPL_STS_MASK_0.set_security_policy("security_Policy_group", "PMU_SAI_POLICY0");
    if ( $test$plusargs("IP_SB_CPL_STS_MASK_0:dont_test") ) IP_SB_CPL_STS_MASK_0.set_test_reg(1'b0);
    if (!add_reg( IP_SB_CPL_STS_MASK_0 )) begin
      `sla_error(get_name(), ("could not add register IP_SB_CPL_STS_MASK_0"));
    end
   IP_SB_CPL_STS_MASK_0.IP_SB_CPL_STS_MASK.set_paths({"IP_SB_CPL_STS_MASK_0.IP_SB_CPL_STS_MASK"});

    `ifdef SLA_RAL_COVERAGE
    sla_ral_reg::regname = "IP_SB_CPL_STS_MASK_1";
    `endif
    IP_SB_CPL_STS_MASK_1 = pmu_mmr_IP_SB_CPL_STS_MASK_1_reg::type_id::create("IP_SB_CPL_STS_MASK_1", this);
    IP_SB_CPL_STS_MASK_1.set_cfg(16'h0, 16'h0, 16'h0, 12'h144, 32, 32'b00000000000000000000000000000000);
    IP_SB_CPL_STS_MASK_1.set_space_addr("MSG", 12'h144);
    IP_SB_CPL_STS_MASK_1.set_space_addr("CR-SB",12'h144); 
    IP_SB_CPL_STS_MASK_1.set_space("MSG");
    IP_SB_CPL_STS_MASK_1.set_msg_opcode("CR-SB");
    IP_SB_CPL_STS_MASK_1.set_security_policy("security_Policy_group", "PMU_SAI_POLICY0");
    if ( $test$plusargs("IP_SB_CPL_STS_MASK_1:dont_test") ) IP_SB_CPL_STS_MASK_1.set_test_reg(1'b0);
    if (!add_reg( IP_SB_CPL_STS_MASK_1 )) begin
      `sla_error(get_name(), ("could not add register IP_SB_CPL_STS_MASK_1"));
    end
   IP_SB_CPL_STS_MASK_1.IP_SB_CPL_STS_MASK.set_paths({"IP_SB_CPL_STS_MASK_1.IP_SB_CPL_STS_MASK"});

    `ifdef SLA_RAL_COVERAGE
    sla_ral_reg::regname = "IP_SB_CPL_STS_0";
    `endif
    IP_SB_CPL_STS_0 = pmu_mmr_IP_SB_CPL_STS_0_reg::type_id::create("IP_SB_CPL_STS_0", this);
    IP_SB_CPL_STS_0.set_cfg(16'h0, 16'h0, 16'h0, 12'h150, 32, 32'b00000000000000000000000000000000);
    IP_SB_CPL_STS_0.set_space_addr("MSG", 12'h150);
    IP_SB_CPL_STS_0.set_space_addr("CR-SB",12'h150); 
    IP_SB_CPL_STS_0.set_space("MSG");
    IP_SB_CPL_STS_0.set_msg_opcode("CR-SB");
    IP_SB_CPL_STS_0.set_security_policy("security_Policy_group", "PMU_SAI_POLICY0");
    if ( $test$plusargs("IP_SB_CPL_STS_0:dont_test") ) IP_SB_CPL_STS_0.set_test_reg(1'b0);
    if (!add_reg( IP_SB_CPL_STS_0 )) begin
      `sla_error(get_name(), ("could not add register IP_SB_CPL_STS_0"));
    end
   IP_SB_CPL_STS_0.IP_SB_CPL_STS_0_0.set_paths({"IP_SB_CPL_STS_0.IP_SB_CPL_STS_0_0"});
   IP_SB_CPL_STS_0.IP_SB_CPL_STS_0_1.set_paths({"IP_SB_CPL_STS_0.IP_SB_CPL_STS_0_1"});
   IP_SB_CPL_STS_0.IP_SB_CPL_STS_0_2.set_paths({"IP_SB_CPL_STS_0.IP_SB_CPL_STS_0_2"});
   IP_SB_CPL_STS_0.IP_SB_CPL_STS_0_3.set_paths({"IP_SB_CPL_STS_0.IP_SB_CPL_STS_0_3"});
   IP_SB_CPL_STS_0.IP_SB_CPL_STS_0_4.set_paths({"IP_SB_CPL_STS_0.IP_SB_CPL_STS_0_4"});
   IP_SB_CPL_STS_0.IP_SB_CPL_STS_0_5.set_paths({"IP_SB_CPL_STS_0.IP_SB_CPL_STS_0_5"});
   IP_SB_CPL_STS_0.IP_SB_CPL_STS_0_6.set_paths({"IP_SB_CPL_STS_0.IP_SB_CPL_STS_0_6"});
   IP_SB_CPL_STS_0.IP_SB_CPL_STS_0_7.set_paths({"IP_SB_CPL_STS_0.IP_SB_CPL_STS_0_7"});
   IP_SB_CPL_STS_0.IP_SB_CPL_STS_0_8.set_paths({"IP_SB_CPL_STS_0.IP_SB_CPL_STS_0_8"});
   IP_SB_CPL_STS_0.IP_SB_CPL_STS_0_9.set_paths({"IP_SB_CPL_STS_0.IP_SB_CPL_STS_0_9"});
   IP_SB_CPL_STS_0.IP_SB_CPL_STS_0_10.set_paths({"IP_SB_CPL_STS_0.IP_SB_CPL_STS_0_10"});
   IP_SB_CPL_STS_0.IP_SB_CPL_STS_0_11.set_paths({"IP_SB_CPL_STS_0.IP_SB_CPL_STS_0_11"});
   IP_SB_CPL_STS_0.IP_SB_CPL_STS_0_12.set_paths({"IP_SB_CPL_STS_0.IP_SB_CPL_STS_0_12"});
   IP_SB_CPL_STS_0.IP_SB_CPL_STS_0_13.set_paths({"IP_SB_CPL_STS_0.IP_SB_CPL_STS_0_13"});
   IP_SB_CPL_STS_0.IP_SB_CPL_STS_0_14.set_paths({"IP_SB_CPL_STS_0.IP_SB_CPL_STS_0_14"});
   IP_SB_CPL_STS_0.IP_SB_CPL_STS_0_15.set_paths({"IP_SB_CPL_STS_0.IP_SB_CPL_STS_0_15"});
   IP_SB_CPL_STS_0.IP_SB_CPL_STS_0_16.set_paths({"IP_SB_CPL_STS_0.IP_SB_CPL_STS_0_16"});
   IP_SB_CPL_STS_0.IP_SB_CPL_STS_0_17.set_paths({"IP_SB_CPL_STS_0.IP_SB_CPL_STS_0_17"});
   IP_SB_CPL_STS_0.IP_SB_CPL_STS_0_18.set_paths({"IP_SB_CPL_STS_0.IP_SB_CPL_STS_0_18"});
   IP_SB_CPL_STS_0.IP_SB_CPL_STS_0_19.set_paths({"IP_SB_CPL_STS_0.IP_SB_CPL_STS_0_19"});
   IP_SB_CPL_STS_0.IP_SB_CPL_STS_0_20.set_paths({"IP_SB_CPL_STS_0.IP_SB_CPL_STS_0_20"});
   IP_SB_CPL_STS_0.IP_SB_CPL_STS_0_21.set_paths({"IP_SB_CPL_STS_0.IP_SB_CPL_STS_0_21"});
   IP_SB_CPL_STS_0.IP_SB_CPL_STS_0_22.set_paths({"IP_SB_CPL_STS_0.IP_SB_CPL_STS_0_22"});
   IP_SB_CPL_STS_0.IP_SB_CPL_STS_0_23.set_paths({"IP_SB_CPL_STS_0.IP_SB_CPL_STS_0_23"});
   IP_SB_CPL_STS_0.IP_SB_CPL_STS_0_24.set_paths({"IP_SB_CPL_STS_0.IP_SB_CPL_STS_0_24"});
   IP_SB_CPL_STS_0.IP_SB_CPL_STS_0_25.set_paths({"IP_SB_CPL_STS_0.IP_SB_CPL_STS_0_25"});
   IP_SB_CPL_STS_0.IP_SB_CPL_STS_0_26.set_paths({"IP_SB_CPL_STS_0.IP_SB_CPL_STS_0_26"});
   IP_SB_CPL_STS_0.IP_SB_CPL_STS_0_27.set_paths({"IP_SB_CPL_STS_0.IP_SB_CPL_STS_0_27"});
   IP_SB_CPL_STS_0.IP_SB_CPL_STS_0_28.set_paths({"IP_SB_CPL_STS_0.IP_SB_CPL_STS_0_28"});
   IP_SB_CPL_STS_0.IP_SB_CPL_STS_0_29.set_paths({"IP_SB_CPL_STS_0.IP_SB_CPL_STS_0_29"});
   IP_SB_CPL_STS_0.IP_SB_CPL_STS_0_30.set_paths({"IP_SB_CPL_STS_0.IP_SB_CPL_STS_0_30"});
   IP_SB_CPL_STS_0.IP_SB_CPL_STS_0_31.set_paths({"IP_SB_CPL_STS_0.IP_SB_CPL_STS_0_31"});

    `ifdef SLA_RAL_COVERAGE
    sla_ral_reg::regname = "IP_SB_CPL_STS_1";
    `endif
    IP_SB_CPL_STS_1 = pmu_mmr_IP_SB_CPL_STS_1_reg::type_id::create("IP_SB_CPL_STS_1", this);
    IP_SB_CPL_STS_1.set_cfg(16'h0, 16'h0, 16'h0, 12'h154, 32, 32'b00000000000000000000000000000000);
    IP_SB_CPL_STS_1.set_space_addr("MSG", 12'h154);
    IP_SB_CPL_STS_1.set_space_addr("CR-SB",12'h154); 
    IP_SB_CPL_STS_1.set_space("MSG");
    IP_SB_CPL_STS_1.set_msg_opcode("CR-SB");
    IP_SB_CPL_STS_1.set_security_policy("security_Policy_group", "PMU_SAI_POLICY0");
    if ( $test$plusargs("IP_SB_CPL_STS_1:dont_test") ) IP_SB_CPL_STS_1.set_test_reg(1'b0);
    if (!add_reg( IP_SB_CPL_STS_1 )) begin
      `sla_error(get_name(), ("could not add register IP_SB_CPL_STS_1"));
    end
   IP_SB_CPL_STS_1.IP_SB_CPL_STS_1_0.set_paths({"IP_SB_CPL_STS_1.IP_SB_CPL_STS_1_0"});
   IP_SB_CPL_STS_1.IP_SB_CPL_STS_1_1.set_paths({"IP_SB_CPL_STS_1.IP_SB_CPL_STS_1_1"});
   IP_SB_CPL_STS_1.IP_SB_CPL_STS_1_2.set_paths({"IP_SB_CPL_STS_1.IP_SB_CPL_STS_1_2"});
   IP_SB_CPL_STS_1.IP_SB_CPL_STS_1_3.set_paths({"IP_SB_CPL_STS_1.IP_SB_CPL_STS_1_3"});
   IP_SB_CPL_STS_1.IP_SB_CPL_STS_1_4.set_paths({"IP_SB_CPL_STS_1.IP_SB_CPL_STS_1_4"});
   IP_SB_CPL_STS_1.IP_SB_CPL_STS_1_5.set_paths({"IP_SB_CPL_STS_1.IP_SB_CPL_STS_1_5"});
   IP_SB_CPL_STS_1.IP_SB_CPL_STS_1_6.set_paths({"IP_SB_CPL_STS_1.IP_SB_CPL_STS_1_6"});
   IP_SB_CPL_STS_1.IP_SB_CPL_STS_1_7.set_paths({"IP_SB_CPL_STS_1.IP_SB_CPL_STS_1_7"});
   IP_SB_CPL_STS_1.IP_SB_CPL_STS_1_8.set_paths({"IP_SB_CPL_STS_1.IP_SB_CPL_STS_1_8"});
   IP_SB_CPL_STS_1.IP_SB_CPL_STS_1_9.set_paths({"IP_SB_CPL_STS_1.IP_SB_CPL_STS_1_9"});
   IP_SB_CPL_STS_1.IP_SB_CPL_STS_1_10.set_paths({"IP_SB_CPL_STS_1.IP_SB_CPL_STS_1_10"});
   IP_SB_CPL_STS_1.IP_SB_CPL_STS_1_11.set_paths({"IP_SB_CPL_STS_1.IP_SB_CPL_STS_1_11"});
   IP_SB_CPL_STS_1.IP_SB_CPL_STS_1_12.set_paths({"IP_SB_CPL_STS_1.IP_SB_CPL_STS_1_12"});
   IP_SB_CPL_STS_1.IP_SB_CPL_STS_1_13.set_paths({"IP_SB_CPL_STS_1.IP_SB_CPL_STS_1_13"});
   IP_SB_CPL_STS_1.IP_SB_CPL_STS_1_14.set_paths({"IP_SB_CPL_STS_1.IP_SB_CPL_STS_1_14"});
   IP_SB_CPL_STS_1.IP_SB_CPL_STS_1_15.set_paths({"IP_SB_CPL_STS_1.IP_SB_CPL_STS_1_15"});
   IP_SB_CPL_STS_1.IP_SB_CPL_STS_1_16.set_paths({"IP_SB_CPL_STS_1.IP_SB_CPL_STS_1_16"});
   IP_SB_CPL_STS_1.IP_SB_CPL_STS_1_17.set_paths({"IP_SB_CPL_STS_1.IP_SB_CPL_STS_1_17"});
   IP_SB_CPL_STS_1.IP_SB_CPL_STS_1_18.set_paths({"IP_SB_CPL_STS_1.IP_SB_CPL_STS_1_18"});
   IP_SB_CPL_STS_1.IP_SB_CPL_STS_1_19.set_paths({"IP_SB_CPL_STS_1.IP_SB_CPL_STS_1_19"});
   IP_SB_CPL_STS_1.IP_SB_CPL_STS_1_20.set_paths({"IP_SB_CPL_STS_1.IP_SB_CPL_STS_1_20"});
   IP_SB_CPL_STS_1.IP_SB_CPL_STS_1_21.set_paths({"IP_SB_CPL_STS_1.IP_SB_CPL_STS_1_21"});
   IP_SB_CPL_STS_1.IP_SB_CPL_STS_1_22.set_paths({"IP_SB_CPL_STS_1.IP_SB_CPL_STS_1_22"});
   IP_SB_CPL_STS_1.IP_SB_CPL_STS_1_23.set_paths({"IP_SB_CPL_STS_1.IP_SB_CPL_STS_1_23"});
   IP_SB_CPL_STS_1.IP_SB_CPL_STS_1_24.set_paths({"IP_SB_CPL_STS_1.IP_SB_CPL_STS_1_24"});
   IP_SB_CPL_STS_1.IP_SB_CPL_STS_1_25.set_paths({"IP_SB_CPL_STS_1.IP_SB_CPL_STS_1_25"});
   IP_SB_CPL_STS_1.IP_SB_CPL_STS_1_26.set_paths({"IP_SB_CPL_STS_1.IP_SB_CPL_STS_1_26"});
   IP_SB_CPL_STS_1.IP_SB_CPL_STS_1_27.set_paths({"IP_SB_CPL_STS_1.IP_SB_CPL_STS_1_27"});
   IP_SB_CPL_STS_1.IP_SB_CPL_STS_1_28.set_paths({"IP_SB_CPL_STS_1.IP_SB_CPL_STS_1_28"});
   IP_SB_CPL_STS_1.IP_SB_CPL_STS_1_29.set_paths({"IP_SB_CPL_STS_1.IP_SB_CPL_STS_1_29"});
   IP_SB_CPL_STS_1.IP_SB_CPL_STS_1_30.set_paths({"IP_SB_CPL_STS_1.IP_SB_CPL_STS_1_30"});
   IP_SB_CPL_STS_1.IP_SB_CPL_STS_1_31.set_paths({"IP_SB_CPL_STS_1.IP_SB_CPL_STS_1_31"});

    `ifdef SLA_RAL_COVERAGE
    sla_ral_reg::regname = "SBI_MSTR_CTL_P";
    `endif
    SBI_MSTR_CTL_P = pmu_mmr_SBI_MSTR_CTL_P_reg::type_id::create("SBI_MSTR_CTL_P", this);
    SBI_MSTR_CTL_P.set_cfg(16'h0, 16'h0, 16'h0, 12'h400, 32, 32'b00000000000000000000000000000000);
    SBI_MSTR_CTL_P.set_space_addr("MSG", 12'h400);
    SBI_MSTR_CTL_P.set_space_addr("CR-SB",12'h400); 
    SBI_MSTR_CTL_P.set_space("MSG");
    SBI_MSTR_CTL_P.set_msg_opcode("CR-SB");
    SBI_MSTR_CTL_P.set_security_policy("security_Policy_group", "PMU_SAI_POLICY0");
    if ( $test$plusargs("SBI_MSTR_CTL_P:dont_test") ) SBI_MSTR_CTL_P.set_test_reg(1'b0);
    if (!add_reg( SBI_MSTR_CTL_P )) begin
      `sla_error(get_name(), ("could not add register SBI_MSTR_CTL_P"));
    end
   SBI_MSTR_CTL_P.SEND_PC_SB.set_paths({"SBI_MSTR_CTL_P.SEND_PC_SB"});
   SBI_MSTR_CTL_P.PC_ADDR_VLD.set_paths({"SBI_MSTR_CTL_P.PC_ADDR_VLD"});
   SBI_MSTR_CTL_P.PC_DW0_VLD.set_paths({"SBI_MSTR_CTL_P.PC_DW0_VLD"});
   SBI_MSTR_CTL_P.PC_DW1_VLD.set_paths({"SBI_MSTR_CTL_P.PC_DW1_VLD"});
   SBI_MSTR_CTL_P.PC_EXHDR_VLD.set_paths({"SBI_MSTR_CTL_P.PC_EXHDR_VLD"});
   SBI_MSTR_CTL_P.PC_FW_SRC_PID_VLD.set_paths({"SBI_MSTR_CTL_P.PC_FW_SRC_PID_VLD"});
   SBI_MSTR_CTL_P.PC_FW_RS.set_paths({"SBI_MSTR_CTL_P.PC_FW_RS"});
   SBI_MSTR_CTL_P.PC_MSG_DIS.set_paths({"SBI_MSTR_CTL_P.PC_MSG_DIS"});

    `ifdef SLA_RAL_COVERAGE
    sla_ral_reg::regname = "SBI_MSTR_HDR_P";
    `endif
    SBI_MSTR_HDR_P = pmu_mmr_SBI_MSTR_HDR_P_reg::type_id::create("SBI_MSTR_HDR_P", this);
    SBI_MSTR_HDR_P.set_cfg(16'h0, 16'h0, 16'h0, 12'h404, 32, 32'b00000000000000000101010100000000);
    SBI_MSTR_HDR_P.set_space_addr("MSG", 12'h404);
    SBI_MSTR_HDR_P.set_space_addr("CR-SB",12'h404); 
    SBI_MSTR_HDR_P.set_space("MSG");
    SBI_MSTR_HDR_P.set_msg_opcode("CR-SB");
    SBI_MSTR_HDR_P.set_security_policy("security_Policy_group", "PMU_SAI_POLICY0");
    if ( $test$plusargs("SBI_MSTR_HDR_P:dont_test") ) SBI_MSTR_HDR_P.set_test_reg(1'b0);
    if (!add_reg( SBI_MSTR_HDR_P )) begin
      `sla_error(get_name(), ("could not add register SBI_MSTR_HDR_P"));
    end
   SBI_MSTR_HDR_P.DEST_PID_PC_SB.set_paths({"SBI_MSTR_HDR_P.DEST_PID_PC_SB"});
   SBI_MSTR_HDR_P.SRC_PID_PC_SB.set_paths({"SBI_MSTR_HDR_P.SRC_PID_PC_SB"});
   SBI_MSTR_HDR_P.OPCODE_PC_SB.set_paths({"SBI_MSTR_HDR_P.OPCODE_PC_SB"});
   SBI_MSTR_HDR_P.TAG_PC_SB.set_paths({"SBI_MSTR_HDR_P.TAG_PC_SB"});
   SBI_MSTR_HDR_P.PC_MSG_HDR_FLD_1.set_paths({"SBI_MSTR_HDR_P.PC_MSG_HDR_FLD_1"});
   SBI_MSTR_HDR_P.EH_PC_SB.set_paths({"SBI_MSTR_HDR_P.EH_PC_SB"});

    `ifdef SLA_RAL_COVERAGE
    sla_ral_reg::regname = "SBI_MSTR_EH_P";
    `endif
    SBI_MSTR_EH_P = pmu_mmr_SBI_MSTR_EH_P_reg::type_id::create("SBI_MSTR_EH_P", this);
    SBI_MSTR_EH_P.set_cfg(16'h0, 16'h0, 16'h0, 12'h408, 32, 32'b00000000000000000000000000000000);
    SBI_MSTR_EH_P.set_space_addr("MSG", 12'h408);
    SBI_MSTR_EH_P.set_space_addr("CR-SB",12'h408); 
    SBI_MSTR_EH_P.set_space("MSG");
    SBI_MSTR_EH_P.set_msg_opcode("CR-SB");
    SBI_MSTR_EH_P.set_security_policy("security_Policy_group", "PMU_SAI_POLICY0");
    if ( $test$plusargs("SBI_MSTR_EH_P:dont_test") ) SBI_MSTR_EH_P.set_test_reg(1'b0);
    if (!add_reg( SBI_MSTR_EH_P )) begin
      `sla_error(get_name(), ("could not add register SBI_MSTR_EH_P"));
    end
   SBI_MSTR_EH_P.EXHDR_PC_SB.set_paths({"SBI_MSTR_EH_P.EXHDR_PC_SB"});

    `ifdef SLA_RAL_COVERAGE
    sla_ral_reg::regname = "SBI_MSTR_ADDR_P";
    `endif
    SBI_MSTR_ADDR_P = pmu_mmr_SBI_MSTR_ADDR_P_reg::type_id::create("SBI_MSTR_ADDR_P", this);
    SBI_MSTR_ADDR_P.set_cfg(16'h0, 16'h0, 16'h0, 12'h40C, 32, 32'b00000000000000000000000000000000);
    SBI_MSTR_ADDR_P.set_space_addr("MSG", 12'h40C);
    SBI_MSTR_ADDR_P.set_space_addr("CR-SB",12'h40C); 
    SBI_MSTR_ADDR_P.set_space("MSG");
    SBI_MSTR_ADDR_P.set_msg_opcode("CR-SB");
    SBI_MSTR_ADDR_P.set_security_policy("security_Policy_group", "PMU_SAI_POLICY0");
    if ( $test$plusargs("SBI_MSTR_ADDR_P:dont_test") ) SBI_MSTR_ADDR_P.set_test_reg(1'b0);
    if (!add_reg( SBI_MSTR_ADDR_P )) begin
      `sla_error(get_name(), ("could not add register SBI_MSTR_ADDR_P"));
    end
   SBI_MSTR_ADDR_P.ADDR_PC_SB.set_paths({"SBI_MSTR_ADDR_P.ADDR_PC_SB"});

    `ifdef SLA_RAL_COVERAGE
    sla_ral_reg::regname = "SBI_MSTR_DW0_P";
    `endif
    SBI_MSTR_DW0_P = pmu_mmr_SBI_MSTR_DW0_P_reg::type_id::create("SBI_MSTR_DW0_P", this);
    SBI_MSTR_DW0_P.set_cfg(16'h0, 16'h0, 16'h0, 12'h410, 32, 32'b00000000000000000000000000000000);
    SBI_MSTR_DW0_P.set_space_addr("MSG", 12'h410);
    SBI_MSTR_DW0_P.set_space_addr("CR-SB",12'h410); 
    SBI_MSTR_DW0_P.set_space("MSG");
    SBI_MSTR_DW0_P.set_msg_opcode("CR-SB");
    SBI_MSTR_DW0_P.set_security_policy("security_Policy_group", "PMU_SAI_POLICY0");
    if ( $test$plusargs("SBI_MSTR_DW0_P:dont_test") ) SBI_MSTR_DW0_P.set_test_reg(1'b0);
    if (!add_reg( SBI_MSTR_DW0_P )) begin
      `sla_error(get_name(), ("could not add register SBI_MSTR_DW0_P"));
    end
   SBI_MSTR_DW0_P.DW0_PC_SB.set_paths({"SBI_MSTR_DW0_P.DW0_PC_SB"});

    `ifdef SLA_RAL_COVERAGE
    sla_ral_reg::regname = "SBI_MSTR_DW1_P";
    `endif
    SBI_MSTR_DW1_P = pmu_mmr_SBI_MSTR_DW1_P_reg::type_id::create("SBI_MSTR_DW1_P", this);
    SBI_MSTR_DW1_P.set_cfg(16'h0, 16'h0, 16'h0, 12'h414, 32, 32'b00000000000000000000000000000000);
    SBI_MSTR_DW1_P.set_space_addr("MSG", 12'h414);
    SBI_MSTR_DW1_P.set_space_addr("CR-SB",12'h414); 
    SBI_MSTR_DW1_P.set_space("MSG");
    SBI_MSTR_DW1_P.set_msg_opcode("CR-SB");
    SBI_MSTR_DW1_P.set_security_policy("security_Policy_group", "PMU_SAI_POLICY0");
    if ( $test$plusargs("SBI_MSTR_DW1_P:dont_test") ) SBI_MSTR_DW1_P.set_test_reg(1'b0);
    if (!add_reg( SBI_MSTR_DW1_P )) begin
      `sla_error(get_name(), ("could not add register SBI_MSTR_DW1_P"));
    end
   SBI_MSTR_DW1_P.DW1_PC_SB.set_paths({"SBI_MSTR_DW1_P.DW1_PC_SB"});

    `ifdef SLA_RAL_COVERAGE
    sla_ral_reg::regname = "SBI_MSTR_CTL_NP";
    `endif
    SBI_MSTR_CTL_NP = pmu_mmr_SBI_MSTR_CTL_NP_reg::type_id::create("SBI_MSTR_CTL_NP", this);
    SBI_MSTR_CTL_NP.set_cfg(16'h0, 16'h0, 16'h0, 12'h420, 32, 32'b00000000000000000000000000000000);
    SBI_MSTR_CTL_NP.set_space_addr("MSG", 12'h420);
    SBI_MSTR_CTL_NP.set_space_addr("CR-SB",12'h420); 
    SBI_MSTR_CTL_NP.set_space("MSG");
    SBI_MSTR_CTL_NP.set_msg_opcode("CR-SB");
    SBI_MSTR_CTL_NP.set_security_policy("security_Policy_group", "PMU_SAI_POLICY0");
    if ( $test$plusargs("SBI_MSTR_CTL_NP:dont_test") ) SBI_MSTR_CTL_NP.set_test_reg(1'b0);
    if (!add_reg( SBI_MSTR_CTL_NP )) begin
      `sla_error(get_name(), ("could not add register SBI_MSTR_CTL_NP"));
    end
   SBI_MSTR_CTL_NP.SEND_NP_SB.set_paths({"SBI_MSTR_CTL_NP.SEND_NP_SB"});
   SBI_MSTR_CTL_NP.NP_ADDR_VLD.set_paths({"SBI_MSTR_CTL_NP.NP_ADDR_VLD"});
   SBI_MSTR_CTL_NP.NP_DW0_VLD.set_paths({"SBI_MSTR_CTL_NP.NP_DW0_VLD"});
   SBI_MSTR_CTL_NP.NP_DW1_VLD.set_paths({"SBI_MSTR_CTL_NP.NP_DW1_VLD"});
   SBI_MSTR_CTL_NP.NP_EXHDR_VLD.set_paths({"SBI_MSTR_CTL_NP.NP_EXHDR_VLD"});
   SBI_MSTR_CTL_NP.NP_BCAST_AGG_CPL.set_paths({"SBI_MSTR_CTL_NP.NP_BCAST_AGG_CPL"});
   SBI_MSTR_CTL_NP.NP_FW_SRC_PID_VLD.set_paths({"SBI_MSTR_CTL_NP.NP_FW_SRC_PID_VLD"});
   SBI_MSTR_CTL_NP.NP_MSG_DIS.set_paths({"SBI_MSTR_CTL_NP.NP_MSG_DIS"});

    `ifdef SLA_RAL_COVERAGE
    sla_ral_reg::regname = "SBI_MSTR_STS_NP";
    `endif
    SBI_MSTR_STS_NP = pmu_mmr_SBI_MSTR_STS_NP_reg::type_id::create("SBI_MSTR_STS_NP", this);
    SBI_MSTR_STS_NP.set_cfg(16'h0, 16'h0, 16'h0, 12'h424, 32, 32'b00000000000000000000000000000000);
    SBI_MSTR_STS_NP.set_space_addr("MSG", 12'h424);
    SBI_MSTR_STS_NP.set_space_addr("CR-SB",12'h424); 
    SBI_MSTR_STS_NP.set_space("MSG");
    SBI_MSTR_STS_NP.set_msg_opcode("CR-SB");
    SBI_MSTR_STS_NP.set_security_policy("security_Policy_group", "PMU_SAI_POLICY0");
    if ( $test$plusargs("SBI_MSTR_STS_NP:dont_test") ) SBI_MSTR_STS_NP.set_test_reg(1'b0);
    if (!add_reg( SBI_MSTR_STS_NP )) begin
      `sla_error(get_name(), ("could not add register SBI_MSTR_STS_NP"));
    end
   SBI_MSTR_STS_NP.NP_IN_PROG.set_paths({"SBI_MSTR_STS_NP.NP_IN_PROG"});
   SBI_MSTR_STS_NP.CPL_MSG_AVAIL.set_paths({"SBI_MSTR_STS_NP.CPL_MSG_AVAIL"});
   SBI_MSTR_STS_NP.CPL_DVLD_SB.set_paths({"SBI_MSTR_STS_NP.CPL_DVLD_SB"});
   SBI_MSTR_STS_NP.CPL_LEN_ERR_STS.set_paths({"SBI_MSTR_STS_NP.CPL_LEN_ERR_STS"});
   SBI_MSTR_STS_NP.RSVD_7_4.set_paths({"SBI_MSTR_STS_NP.RSVD_7_4"});

    `ifdef SLA_RAL_COVERAGE
    sla_ral_reg::regname = "SBI_MSTR_HDR_NP";
    `endif
    SBI_MSTR_HDR_NP = pmu_mmr_SBI_MSTR_HDR_NP_reg::type_id::create("SBI_MSTR_HDR_NP", this);
    SBI_MSTR_HDR_NP.set_cfg(16'h0, 16'h0, 16'h0, 12'h428, 32, 32'b00000000000000000101010100000000);
    SBI_MSTR_HDR_NP.set_space_addr("MSG", 12'h428);
    SBI_MSTR_HDR_NP.set_space_addr("CR-SB",12'h428); 
    SBI_MSTR_HDR_NP.set_space("MSG");
    SBI_MSTR_HDR_NP.set_msg_opcode("CR-SB");
    SBI_MSTR_HDR_NP.set_security_policy("security_Policy_group", "PMU_SAI_POLICY0");
    if ( $test$plusargs("SBI_MSTR_HDR_NP:dont_test") ) SBI_MSTR_HDR_NP.set_test_reg(1'b0);
    if (!add_reg( SBI_MSTR_HDR_NP )) begin
      `sla_error(get_name(), ("could not add register SBI_MSTR_HDR_NP"));
    end
   SBI_MSTR_HDR_NP.DEST_PID_NP_SB.set_paths({"SBI_MSTR_HDR_NP.DEST_PID_NP_SB"});
   SBI_MSTR_HDR_NP.SRC_PID_NP_SB.set_paths({"SBI_MSTR_HDR_NP.SRC_PID_NP_SB"});
   SBI_MSTR_HDR_NP.OPCODE_NP_SB.set_paths({"SBI_MSTR_HDR_NP.OPCODE_NP_SB"});
   SBI_MSTR_HDR_NP.TAG_NP_SB.set_paths({"SBI_MSTR_HDR_NP.TAG_NP_SB"});
   SBI_MSTR_HDR_NP.NP_MSG_HDR_FLD_1.set_paths({"SBI_MSTR_HDR_NP.NP_MSG_HDR_FLD_1"});
   SBI_MSTR_HDR_NP.EH_NP_SB.set_paths({"SBI_MSTR_HDR_NP.EH_NP_SB"});

    `ifdef SLA_RAL_COVERAGE
    sla_ral_reg::regname = "SBI_MSTR_EH_NP";
    `endif
    SBI_MSTR_EH_NP = pmu_mmr_SBI_MSTR_EH_NP_reg::type_id::create("SBI_MSTR_EH_NP", this);
    SBI_MSTR_EH_NP.set_cfg(16'h0, 16'h0, 16'h0, 12'h42C, 32, 32'b00000000000000000000000000000000);
    SBI_MSTR_EH_NP.set_space_addr("MSG", 12'h42C);
    SBI_MSTR_EH_NP.set_space_addr("CR-SB",12'h42C); 
    SBI_MSTR_EH_NP.set_space("MSG");
    SBI_MSTR_EH_NP.set_msg_opcode("CR-SB");
    SBI_MSTR_EH_NP.set_security_policy("security_Policy_group", "PMU_SAI_POLICY0");
    if ( $test$plusargs("SBI_MSTR_EH_NP:dont_test") ) SBI_MSTR_EH_NP.set_test_reg(1'b0);
    if (!add_reg( SBI_MSTR_EH_NP )) begin
      `sla_error(get_name(), ("could not add register SBI_MSTR_EH_NP"));
    end
   SBI_MSTR_EH_NP.EXHDR_NP_SB.set_paths({"SBI_MSTR_EH_NP.EXHDR_NP_SB"});

    `ifdef SLA_RAL_COVERAGE
    sla_ral_reg::regname = "SBI_MSTR_ADDR_NP";
    `endif
    SBI_MSTR_ADDR_NP = pmu_mmr_SBI_MSTR_ADDR_NP_reg::type_id::create("SBI_MSTR_ADDR_NP", this);
    SBI_MSTR_ADDR_NP.set_cfg(16'h0, 16'h0, 16'h0, 12'h430, 32, 32'b00000000000000000000000000000000);
    SBI_MSTR_ADDR_NP.set_space_addr("MSG", 12'h430);
    SBI_MSTR_ADDR_NP.set_space_addr("CR-SB",12'h430); 
    SBI_MSTR_ADDR_NP.set_space("MSG");
    SBI_MSTR_ADDR_NP.set_msg_opcode("CR-SB");
    SBI_MSTR_ADDR_NP.set_security_policy("security_Policy_group", "PMU_SAI_POLICY0");
    if ( $test$plusargs("SBI_MSTR_ADDR_NP:dont_test") ) SBI_MSTR_ADDR_NP.set_test_reg(1'b0);
    if (!add_reg( SBI_MSTR_ADDR_NP )) begin
      `sla_error(get_name(), ("could not add register SBI_MSTR_ADDR_NP"));
    end
   SBI_MSTR_ADDR_NP.ADDR_NP_SB.set_paths({"SBI_MSTR_ADDR_NP.ADDR_NP_SB"});

    `ifdef SLA_RAL_COVERAGE
    sla_ral_reg::regname = "SBI_MSTR_DW0_NP";
    `endif
    SBI_MSTR_DW0_NP = pmu_mmr_SBI_MSTR_DW0_NP_reg::type_id::create("SBI_MSTR_DW0_NP", this);
    SBI_MSTR_DW0_NP.set_cfg(16'h0, 16'h0, 16'h0, 12'h434, 32, 32'b00000000000000000000000000000000);
    SBI_MSTR_DW0_NP.set_space_addr("MSG", 12'h434);
    SBI_MSTR_DW0_NP.set_space_addr("CR-SB",12'h434); 
    SBI_MSTR_DW0_NP.set_space("MSG");
    SBI_MSTR_DW0_NP.set_msg_opcode("CR-SB");
    SBI_MSTR_DW0_NP.set_security_policy("security_Policy_group", "PMU_SAI_POLICY0");
    if ( $test$plusargs("SBI_MSTR_DW0_NP:dont_test") ) SBI_MSTR_DW0_NP.set_test_reg(1'b0);
    if (!add_reg( SBI_MSTR_DW0_NP )) begin
      `sla_error(get_name(), ("could not add register SBI_MSTR_DW0_NP"));
    end
   SBI_MSTR_DW0_NP.DW0_NP_SB.set_paths({"SBI_MSTR_DW0_NP.DW0_NP_SB"});

    `ifdef SLA_RAL_COVERAGE
    sla_ral_reg::regname = "SBI_MSTR_DW1_NP";
    `endif
    SBI_MSTR_DW1_NP = pmu_mmr_SBI_MSTR_DW1_NP_reg::type_id::create("SBI_MSTR_DW1_NP", this);
    SBI_MSTR_DW1_NP.set_cfg(16'h0, 16'h0, 16'h0, 12'h438, 32, 32'b00000000000000000000000000000000);
    SBI_MSTR_DW1_NP.set_space_addr("MSG", 12'h438);
    SBI_MSTR_DW1_NP.set_space_addr("CR-SB",12'h438); 
    SBI_MSTR_DW1_NP.set_space("MSG");
    SBI_MSTR_DW1_NP.set_msg_opcode("CR-SB");
    SBI_MSTR_DW1_NP.set_security_policy("security_Policy_group", "PMU_SAI_POLICY0");
    if ( $test$plusargs("SBI_MSTR_DW1_NP:dont_test") ) SBI_MSTR_DW1_NP.set_test_reg(1'b0);
    if (!add_reg( SBI_MSTR_DW1_NP )) begin
      `sla_error(get_name(), ("could not add register SBI_MSTR_DW1_NP"));
    end
   SBI_MSTR_DW1_NP.DW1_NP_SB.set_paths({"SBI_MSTR_DW1_NP.DW1_NP_SB"});

    `ifdef SLA_RAL_COVERAGE
    sla_ral_reg::regname = "SBI_MSTR_CPL_HDR";
    `endif
    SBI_MSTR_CPL_HDR = pmu_mmr_SBI_MSTR_CPL_HDR_reg::type_id::create("SBI_MSTR_CPL_HDR", this);
    SBI_MSTR_CPL_HDR.set_cfg(16'h0, 16'h0, 16'h0, 12'h440, 32, 32'b00000000000000000000000000000000);
    SBI_MSTR_CPL_HDR.set_space_addr("MSG", 12'h440);
    SBI_MSTR_CPL_HDR.set_space_addr("CR-SB",12'h440); 
    SBI_MSTR_CPL_HDR.set_space("MSG");
    SBI_MSTR_CPL_HDR.set_msg_opcode("CR-SB");
    SBI_MSTR_CPL_HDR.set_security_policy("security_Policy_group", "PMU_SAI_POLICY0");
    if ( $test$plusargs("SBI_MSTR_CPL_HDR:dont_test") ) SBI_MSTR_CPL_HDR.set_test_reg(1'b0);
    if (!add_reg( SBI_MSTR_CPL_HDR )) begin
      `sla_error(get_name(), ("could not add register SBI_MSTR_CPL_HDR"));
    end
   SBI_MSTR_CPL_HDR.CPL_HDR_SB.set_paths({"SBI_MSTR_CPL_HDR.CPL_HDR_SB"});

    `ifdef SLA_RAL_COVERAGE
    sla_ral_reg::regname = "SBI_MSTR_CPL_EH";
    `endif
    SBI_MSTR_CPL_EH = pmu_mmr_SBI_MSTR_CPL_EH_reg::type_id::create("SBI_MSTR_CPL_EH", this);
    SBI_MSTR_CPL_EH.set_cfg(16'h0, 16'h0, 16'h0, 12'h444, 32, 32'b00000000000000000000000000000000);
    SBI_MSTR_CPL_EH.set_space_addr("MSG", 12'h444);
    SBI_MSTR_CPL_EH.set_space_addr("CR-SB",12'h444); 
    SBI_MSTR_CPL_EH.set_space("MSG");
    SBI_MSTR_CPL_EH.set_msg_opcode("CR-SB");
    SBI_MSTR_CPL_EH.set_security_policy("security_Policy_group", "PMU_SAI_POLICY0");
    if ( $test$plusargs("SBI_MSTR_CPL_EH:dont_test") ) SBI_MSTR_CPL_EH.set_test_reg(1'b0);
    if (!add_reg( SBI_MSTR_CPL_EH )) begin
      `sla_error(get_name(), ("could not add register SBI_MSTR_CPL_EH"));
    end
   SBI_MSTR_CPL_EH.CPL_EH_SB.set_paths({"SBI_MSTR_CPL_EH.CPL_EH_SB"});

    `ifdef SLA_RAL_COVERAGE
    sla_ral_reg::regname = "SBI_MSTR_CPL_DATA";
    `endif
    SBI_MSTR_CPL_DATA = pmu_mmr_SBI_MSTR_CPL_DATA_reg::type_id::create("SBI_MSTR_CPL_DATA", this);
    SBI_MSTR_CPL_DATA.set_cfg(16'h0, 16'h0, 16'h0, 12'h448, 32, 32'b00000000000000000000000000000000);
    SBI_MSTR_CPL_DATA.set_space_addr("MSG", 12'h448);
    SBI_MSTR_CPL_DATA.set_space_addr("CR-SB",12'h448); 
    SBI_MSTR_CPL_DATA.set_space("MSG");
    SBI_MSTR_CPL_DATA.set_msg_opcode("CR-SB");
    SBI_MSTR_CPL_DATA.set_security_policy("security_Policy_group", "PMU_SAI_POLICY0");
    if ( $test$plusargs("SBI_MSTR_CPL_DATA:dont_test") ) SBI_MSTR_CPL_DATA.set_test_reg(1'b0);
    if (!add_reg( SBI_MSTR_CPL_DATA )) begin
      `sla_error(get_name(), ("could not add register SBI_MSTR_CPL_DATA"));
    end
   SBI_MSTR_CPL_DATA.CPL_DATA_SB.set_paths({"SBI_MSTR_CPL_DATA.CPL_DATA_SB"});

    `ifdef SLA_RAL_COVERAGE
    sla_ral_reg::regname = "SBI_IMSG_FIFO_STS";
    `endif
    SBI_IMSG_FIFO_STS = pmu_mmr_SBI_IMSG_FIFO_STS_reg::type_id::create("SBI_IMSG_FIFO_STS", this);
    SBI_IMSG_FIFO_STS.set_cfg(16'h0, 16'h0, 16'h0, 12'h450, 32, 32'b00000000000000000000000000000000);
    SBI_IMSG_FIFO_STS.set_space_addr("MSG", 12'h450);
    SBI_IMSG_FIFO_STS.set_space_addr("CR-SB",12'h450); 
    SBI_IMSG_FIFO_STS.set_space("MSG");
    SBI_IMSG_FIFO_STS.set_msg_opcode("CR-SB");
    SBI_IMSG_FIFO_STS.set_security_policy("security_Policy_group", "PMU_SAI_POLICY0");
    if ( $test$plusargs("SBI_IMSG_FIFO_STS:dont_test") ) SBI_IMSG_FIFO_STS.set_test_reg(1'b0);
    if (!add_reg( SBI_IMSG_FIFO_STS )) begin
      `sla_error(get_name(), ("could not add register SBI_IMSG_FIFO_STS"));
    end
   SBI_IMSG_FIFO_STS.IMSG_FIFO_MSG_AVAIL.set_paths({"SBI_IMSG_FIFO_STS.IMSG_FIFO_MSG_AVAIL"});
   SBI_IMSG_FIFO_STS.IMSG_FIFO_MSG_LEN.set_paths({"SBI_IMSG_FIFO_STS.IMSG_FIFO_MSG_LEN"});
   SBI_IMSG_FIFO_STS.IMSG_FIFO_MSG_TYP.set_paths({"SBI_IMSG_FIFO_STS.IMSG_FIFO_MSG_TYP"});
   SBI_IMSG_FIFO_STS.IMSG_FIFO_NUM_AVAIL.set_paths({"SBI_IMSG_FIFO_STS.IMSG_FIFO_NUM_AVAIL"});

    `ifdef SLA_RAL_COVERAGE
    sla_ral_reg::regname = "SBI_IMSG_FIFO_CTL";
    `endif
    SBI_IMSG_FIFO_CTL = pmu_mmr_SBI_IMSG_FIFO_CTL_reg::type_id::create("SBI_IMSG_FIFO_CTL", this);
    SBI_IMSG_FIFO_CTL.set_cfg(16'h0, 16'h0, 16'h0, 12'h454, 32, 32'b00000000000000000000000000000000);
    SBI_IMSG_FIFO_CTL.set_space_addr("MSG", 12'h454);
    SBI_IMSG_FIFO_CTL.set_space_addr("CR-SB",12'h454); 
    SBI_IMSG_FIFO_CTL.set_space("MSG");
    SBI_IMSG_FIFO_CTL.set_msg_opcode("CR-SB");
    SBI_IMSG_FIFO_CTL.set_security_policy("security_Policy_group", "PMU_SAI_POLICY0");
    if ( $test$plusargs("SBI_IMSG_FIFO_CTL:dont_test") ) SBI_IMSG_FIFO_CTL.set_test_reg(1'b0);
    if (!add_reg( SBI_IMSG_FIFO_CTL )) begin
      `sla_error(get_name(), ("could not add register SBI_IMSG_FIFO_CTL"));
    end
   SBI_IMSG_FIFO_CTL.IMSG_FIFO_MSG_GET.set_paths({"SBI_IMSG_FIFO_CTL.IMSG_FIFO_MSG_GET"});
   SBI_IMSG_FIFO_CTL.RSVD_6_1.set_paths({"SBI_IMSG_FIFO_CTL.RSVD_6_1"});
   SBI_IMSG_FIFO_CTL.IMSG_FIFO_MSG_DROP.set_paths({"SBI_IMSG_FIFO_CTL.IMSG_FIFO_MSG_DROP"});

    `ifdef SLA_RAL_COVERAGE
    sla_ral_reg::regname = "SBI_IMSG_FIFO_DATA_DW0";
    `endif
    SBI_IMSG_FIFO_DATA_DW0 = pmu_mmr_SBI_IMSG_FIFO_DATA_DW0_reg::type_id::create("SBI_IMSG_FIFO_DATA_DW0", this);
    SBI_IMSG_FIFO_DATA_DW0.set_cfg(16'h0, 16'h0, 16'h0, 12'h458, 32, 32'b00000000000000000000000000000000);
    SBI_IMSG_FIFO_DATA_DW0.set_space_addr("MSG", 12'h458);
    SBI_IMSG_FIFO_DATA_DW0.set_space_addr("CR-SB",12'h458); 
    SBI_IMSG_FIFO_DATA_DW0.set_space("MSG");
    SBI_IMSG_FIFO_DATA_DW0.set_msg_opcode("CR-SB");
    SBI_IMSG_FIFO_DATA_DW0.set_security_policy("security_Policy_group", "PMU_SAI_POLICY0");
    if ( $test$plusargs("SBI_IMSG_FIFO_DATA_DW0:dont_test") ) SBI_IMSG_FIFO_DATA_DW0.set_test_reg(1'b0);
    if (!add_reg( SBI_IMSG_FIFO_DATA_DW0 )) begin
      `sla_error(get_name(), ("could not add register SBI_IMSG_FIFO_DATA_DW0"));
    end
   SBI_IMSG_FIFO_DATA_DW0.IMSG_FIFO_DATA_DW0.set_paths({"SBI_IMSG_FIFO_DATA_DW0.IMSG_FIFO_DATA_DW0"});

    `ifdef SLA_RAL_COVERAGE
    sla_ral_reg::regname = "SBI_IMSG_FIFO_DATA_DW1";
    `endif
    SBI_IMSG_FIFO_DATA_DW1 = pmu_mmr_SBI_IMSG_FIFO_DATA_DW1_reg::type_id::create("SBI_IMSG_FIFO_DATA_DW1", this);
    SBI_IMSG_FIFO_DATA_DW1.set_cfg(16'h0, 16'h0, 16'h0, 12'h45C, 32, 32'b00000000000000000000000000000000);
    SBI_IMSG_FIFO_DATA_DW1.set_space_addr("MSG", 12'h45C);
    SBI_IMSG_FIFO_DATA_DW1.set_space_addr("CR-SB",12'h45C); 
    SBI_IMSG_FIFO_DATA_DW1.set_space("MSG");
    SBI_IMSG_FIFO_DATA_DW1.set_msg_opcode("CR-SB");
    SBI_IMSG_FIFO_DATA_DW1.set_security_policy("security_Policy_group", "PMU_SAI_POLICY0");
    if ( $test$plusargs("SBI_IMSG_FIFO_DATA_DW1:dont_test") ) SBI_IMSG_FIFO_DATA_DW1.set_test_reg(1'b0);
    if (!add_reg( SBI_IMSG_FIFO_DATA_DW1 )) begin
      `sla_error(get_name(), ("could not add register SBI_IMSG_FIFO_DATA_DW1"));
    end
   SBI_IMSG_FIFO_DATA_DW1.IMSG_FIFO_DATA_DW1.set_paths({"SBI_IMSG_FIFO_DATA_DW1.IMSG_FIFO_DATA_DW1"});

    `ifdef SLA_RAL_COVERAGE
    sla_ral_reg::regname = "SBI_IMSG_FIFO_DATA_DW2";
    `endif
    SBI_IMSG_FIFO_DATA_DW2 = pmu_mmr_SBI_IMSG_FIFO_DATA_DW2_reg::type_id::create("SBI_IMSG_FIFO_DATA_DW2", this);
    SBI_IMSG_FIFO_DATA_DW2.set_cfg(16'h0, 16'h0, 16'h0, 12'h460, 32, 32'b00000000000000000000000000000000);
    SBI_IMSG_FIFO_DATA_DW2.set_space_addr("MSG", 12'h460);
    SBI_IMSG_FIFO_DATA_DW2.set_space_addr("CR-SB",12'h460); 
    SBI_IMSG_FIFO_DATA_DW2.set_space("MSG");
    SBI_IMSG_FIFO_DATA_DW2.set_msg_opcode("CR-SB");
    SBI_IMSG_FIFO_DATA_DW2.set_security_policy("security_Policy_group", "PMU_SAI_POLICY0");
    if ( $test$plusargs("SBI_IMSG_FIFO_DATA_DW2:dont_test") ) SBI_IMSG_FIFO_DATA_DW2.set_test_reg(1'b0);
    if (!add_reg( SBI_IMSG_FIFO_DATA_DW2 )) begin
      `sla_error(get_name(), ("could not add register SBI_IMSG_FIFO_DATA_DW2"));
    end
   SBI_IMSG_FIFO_DATA_DW2.IMSG_FIFO_DATA_DW2.set_paths({"SBI_IMSG_FIFO_DATA_DW2.IMSG_FIFO_DATA_DW2"});

    `ifdef SLA_RAL_COVERAGE
    sla_ral_reg::regname = "SBI_IMSG_FIFO_DATA_DW3";
    `endif
    SBI_IMSG_FIFO_DATA_DW3 = pmu_mmr_SBI_IMSG_FIFO_DATA_DW3_reg::type_id::create("SBI_IMSG_FIFO_DATA_DW3", this);
    SBI_IMSG_FIFO_DATA_DW3.set_cfg(16'h0, 16'h0, 16'h0, 12'h464, 32, 32'b00000000000000000000000000000000);
    SBI_IMSG_FIFO_DATA_DW3.set_space_addr("MSG", 12'h464);
    SBI_IMSG_FIFO_DATA_DW3.set_space_addr("CR-SB",12'h464); 
    SBI_IMSG_FIFO_DATA_DW3.set_space("MSG");
    SBI_IMSG_FIFO_DATA_DW3.set_msg_opcode("CR-SB");
    SBI_IMSG_FIFO_DATA_DW3.set_security_policy("security_Policy_group", "PMU_SAI_POLICY0");
    if ( $test$plusargs("SBI_IMSG_FIFO_DATA_DW3:dont_test") ) SBI_IMSG_FIFO_DATA_DW3.set_test_reg(1'b0);
    if (!add_reg( SBI_IMSG_FIFO_DATA_DW3 )) begin
      `sla_error(get_name(), ("could not add register SBI_IMSG_FIFO_DATA_DW3"));
    end
   SBI_IMSG_FIFO_DATA_DW3.IMSG_FIFO_DATA_DW3.set_paths({"SBI_IMSG_FIFO_DATA_DW3.IMSG_FIFO_DATA_DW3"});

    `ifdef SLA_RAL_COVERAGE
    sla_ral_reg::regname = "TSRSP_CTL";
    `endif
    TSRSP_CTL = pmu_mmr_TSRSP_CTL_reg::type_id::create("TSRSP_CTL", this);
    TSRSP_CTL.set_cfg(16'h0, 16'h0, 16'h0, 12'h500, 32, 32'b00000000000000000000000000000001);
    TSRSP_CTL.set_space_addr("MSG", 12'h500);
    TSRSP_CTL.set_space_addr("CR-SB",12'h500); 
    TSRSP_CTL.set_space("MSG");
    TSRSP_CTL.set_msg_opcode("CR-SB");
    TSRSP_CTL.set_security_policy("security_Policy_group", "PMU_SAI_POLICY0");
    if ( $test$plusargs("TSRSP_CTL:dont_test") ) TSRSP_CTL.set_test_reg(1'b0);
    if (!add_reg( TSRSP_CTL )) begin
      `sla_error(get_name(), ("could not add register TSRSP_CTL"));
    end
   TSRSP_CTL.ART_EN.set_paths({"TSRSP_CTL.ART_EN"});
   TSRSP_CTL.TSRSP_EN.set_paths({"TSRSP_CTL.TSRSP_EN"});
   TSRSP_CTL.RSVD_7_2.set_paths({"TSRSP_CTL.RSVD_7_2"});
   TSRSP_CTL.ART_RST.set_paths({"TSRSP_CTL.ART_RST"});
   TSRSP_CTL.SNAPSHOT_ART.set_paths({"TSRSP_CTL.SNAPSHOT_ART"});
   TSRSP_CTL.RSVD_31_10.set_paths({"TSRSP_CTL.RSVD_31_10"});

    `ifdef SLA_RAL_COVERAGE
    sla_ral_reg::regname = "TSRSP_STS";
    `endif
    TSRSP_STS = pmu_mmr_TSRSP_STS_reg::type_id::create("TSRSP_STS", this);
    TSRSP_STS.set_cfg(16'h0, 16'h0, 16'h0, 12'h504, 32, 32'b000000000000000000000000000000000);
    TSRSP_STS.set_space_addr("MSG", 12'h504);
    TSRSP_STS.set_space_addr("CR-SB",12'h504); 
    TSRSP_STS.set_space("MSG");
    TSRSP_STS.set_msg_opcode("CR-SB");
    TSRSP_STS.set_security_policy("security_Policy_group", "PMU_SAI_POLICY0");
    if ( $test$plusargs("TSRSP_STS:dont_test") ) TSRSP_STS.set_test_reg(1'b0);
    if (!add_reg( TSRSP_STS )) begin
      `sla_error(get_name(), ("could not add register TSRSP_STS"));
    end
   TSRSP_STS.CPL_UC.set_paths({"TSRSP_STS.CPL_UC"});
   TSRSP_STS.CPL_UR.set_paths({"TSRSP_STS.CPL_UR"});
   TSRSP_STS.SAI_ERR.set_paths({"TSRSP_STS.SAI_ERR"});
   TSRSP_STS.FLAG_Q_FULL.set_paths({"TSRSP_STS.FLAG_Q_FULL"});
   TSRSP_STS.FLAG_MSG_DIS.set_paths({"TSRSP_STS.FLAG_MSG_DIS"});
   TSRSP_STS.FLAG_UNSUP_OPS.set_paths({"TSRSP_STS.FLAG_UNSUP_OPS"});
   TSRSP_STS.RSVD_7_6.set_paths({"TSRSP_STS.RSVD_7_6"});
   TSRSP_STS.TSRSP_INPROG.set_paths({"TSRSP_STS.TSRSP_INPROG"});
   TSRSP_STS.RSVD_11_9.set_paths({"TSRSP_STS.RSVD_11_9"});
   TSRSP_STS.REQ_AT.set_paths({"TSRSP_STS.REQ_AT"});
   TSRSP_STS.REQ_ST.set_paths({"TSRSP_STS.REQ_ST"});
   TSRSP_STS.REQ_SID.set_paths({"TSRSP_STS.REQ_SID"});
   TSRSP_STS.REQ_MAD.set_paths({"TSRSP_STS.REQ_MAD"});
   TSRSP_STS.REQ_TBC.set_paths({"TSRSP_STS.REQ_TBC"});

    `ifdef SLA_RAL_COVERAGE
    sla_ral_reg::regname = "TSRSP_CPL_STS";
    `endif
    TSRSP_CPL_STS = pmu_mmr_TSRSP_CPL_STS_reg::type_id::create("TSRSP_CPL_STS", this);
    TSRSP_CPL_STS.set_cfg(16'h0, 16'h0, 16'h0, 12'h508, 32, 32'b00000000000000000000000000000000);
    TSRSP_CPL_STS.set_space_addr("MSG", 12'h508);
    TSRSP_CPL_STS.set_space_addr("CR-SB",12'h508); 
    TSRSP_CPL_STS.set_space("MSG");
    TSRSP_CPL_STS.set_msg_opcode("CR-SB");
    TSRSP_CPL_STS.set_security_policy("security_Policy_group", "PMU_SAI_POLICY0");
    if ( $test$plusargs("TSRSP_CPL_STS:dont_test") ) TSRSP_CPL_STS.set_test_reg(1'b0);
    if (!add_reg( TSRSP_CPL_STS )) begin
      `sla_error(get_name(), ("could not add register TSRSP_CPL_STS"));
    end
   TSRSP_CPL_STS.RSVD_7_0.set_paths({"TSRSP_CPL_STS.RSVD_7_0"});
   TSRSP_CPL_STS.SRCID.set_paths({"TSRSP_CPL_STS.SRCID"});
   TSRSP_CPL_STS.OPCODE.set_paths({"TSRSP_CPL_STS.OPCODE"});
   TSRSP_CPL_STS.TAG.set_paths({"TSRSP_CPL_STS.TAG"});
   TSRSP_CPL_STS.RSP.set_paths({"TSRSP_CPL_STS.RSP"});

    `ifdef SLA_RAL_COVERAGE
    sla_ral_reg::regname = "TSRSP_ART_SNAPSHOT_LO";
    `endif
    TSRSP_ART_SNAPSHOT_LO = pmu_mmr_TSRSP_ART_SNAPSHOT_LO_reg::type_id::create("TSRSP_ART_SNAPSHOT_LO", this);
    TSRSP_ART_SNAPSHOT_LO.set_cfg(16'h0, 16'h0, 16'h0, 12'h50C, 32, 32'b00000000000000000000000000000000);
    TSRSP_ART_SNAPSHOT_LO.set_space_addr("MSG", 12'h50C);
    TSRSP_ART_SNAPSHOT_LO.set_space_addr("CR-SB",12'h50C); 
    TSRSP_ART_SNAPSHOT_LO.set_space("MSG");
    TSRSP_ART_SNAPSHOT_LO.set_msg_opcode("CR-SB");
    TSRSP_ART_SNAPSHOT_LO.set_security_policy("security_Policy_group", "PMU_SAI_POLICY0");
    if ( $test$plusargs("TSRSP_ART_SNAPSHOT_LO:dont_test") ) TSRSP_ART_SNAPSHOT_LO.set_test_reg(1'b0);
    if (!add_reg( TSRSP_ART_SNAPSHOT_LO )) begin
      `sla_error(get_name(), ("could not add register TSRSP_ART_SNAPSHOT_LO"));
    end
   TSRSP_ART_SNAPSHOT_LO.TSRSP_ART_SNAPSHOT_LO.set_paths({"TSRSP_ART_SNAPSHOT_LO.TSRSP_ART_SNAPSHOT_LO"});

    `ifdef SLA_RAL_COVERAGE
    sla_ral_reg::regname = "TSRSP_ART_SNAPSHOT_HI";
    `endif
    TSRSP_ART_SNAPSHOT_HI = pmu_mmr_TSRSP_ART_SNAPSHOT_HI_reg::type_id::create("TSRSP_ART_SNAPSHOT_HI", this);
    TSRSP_ART_SNAPSHOT_HI.set_cfg(16'h0, 16'h0, 16'h0, 12'h510, 32, 32'b00000000000000000000000000000000);
    TSRSP_ART_SNAPSHOT_HI.set_space_addr("MSG", 12'h510);
    TSRSP_ART_SNAPSHOT_HI.set_space_addr("CR-SB",12'h510); 
    TSRSP_ART_SNAPSHOT_HI.set_space("MSG");
    TSRSP_ART_SNAPSHOT_HI.set_msg_opcode("CR-SB");
    TSRSP_ART_SNAPSHOT_HI.set_security_policy("security_Policy_group", "PMU_SAI_POLICY0");
    if ( $test$plusargs("TSRSP_ART_SNAPSHOT_HI:dont_test") ) TSRSP_ART_SNAPSHOT_HI.set_test_reg(1'b0);
    if (!add_reg( TSRSP_ART_SNAPSHOT_HI )) begin
      `sla_error(get_name(), ("could not add register TSRSP_ART_SNAPSHOT_HI"));
    end
   TSRSP_ART_SNAPSHOT_HI.TSRSP_ART_SNAPSHOT_HI.set_paths({"TSRSP_ART_SNAPSHOT_HI.TSRSP_ART_SNAPSHOT_HI"});

    `ifdef SLA_RAL_COVERAGE
    sla_ral_reg::regname = "TS_SERIAL_CTL";
    `endif
    TS_SERIAL_CTL = pmu_mmr_TS_SERIAL_CTL_reg::type_id::create("TS_SERIAL_CTL", this);
    TS_SERIAL_CTL.set_cfg(16'h0, 16'h0, 16'h0, 12'h514, 32, 32'b00000000000000000000000000000000);
    TS_SERIAL_CTL.set_space_addr("MSG", 12'h514);
    TS_SERIAL_CTL.set_space_addr("CR-SB",12'h514); 
    TS_SERIAL_CTL.set_space("MSG");
    TS_SERIAL_CTL.set_msg_opcode("CR-SB");
    TS_SERIAL_CTL.set_security_policy("security_Policy_group", "PMU_SAI_POLICY0");
    if ( $test$plusargs("TS_SERIAL_CTL:dont_test") ) TS_SERIAL_CTL.set_test_reg(1'b0);
    if (!add_reg( TS_SERIAL_CTL )) begin
      `sla_error(get_name(), ("could not add register TS_SERIAL_CTL"));
    end
   TS_SERIAL_CTL.SERIAL_TIMESYNC_START.set_paths({"TS_SERIAL_CTL.SERIAL_TIMESYNC_START"});
   TS_SERIAL_CTL.RSVD_7_1.set_paths({"TS_SERIAL_CTL.RSVD_7_1"});
   TS_SERIAL_CTL.SERIAL_TIMESYNC_OFFSET.set_paths({"TS_SERIAL_CTL.SERIAL_TIMESYNC_OFFSET"});
   TS_SERIAL_CTL.SERIAL_CTRL_WORD.set_paths({"TS_SERIAL_CTL.SERIAL_CTRL_WORD"});

    `ifdef SLA_RAL_COVERAGE
    sla_ral_reg::regname = "PM_CORE_VID";
    `endif
    PM_CORE_VID = pmu_mmr_PM_CORE_VID_reg::type_id::create("PM_CORE_VID", this);
    PM_CORE_VID.set_cfg(16'h0, 16'h0, 16'h0, 12'h540, 32, 32'b00000000000000000000000000000000);
    PM_CORE_VID.set_space_addr("MSG", 12'h540);
    PM_CORE_VID.set_space_addr("CR-SB",12'h540); 
    PM_CORE_VID.set_space("MSG");
    PM_CORE_VID.set_msg_opcode("CR-SB");
    PM_CORE_VID.set_security_policy("security_Policy_group", "PMU_SAI_POLICY0");
    if ( $test$plusargs("PM_CORE_VID:dont_test") ) PM_CORE_VID.set_test_reg(1'b0);
    if (!add_reg( PM_CORE_VID )) begin
      `sla_error(get_name(), ("could not add register PM_CORE_VID"));
    end
   PM_CORE_VID.CORE_VID_STS.set_paths({"PM_CORE_VID.CORE_VID_STS"});
   PM_CORE_VID.CORE_VID_TGT.set_paths({"PM_CORE_VID.CORE_VID_TGT"});
   PM_CORE_VID.RSVD_6_4.set_paths({"PM_CORE_VID.RSVD_6_4"});
   PM_CORE_VID.UPDATE_CORE_VID.set_paths({"PM_CORE_VID.UPDATE_CORE_VID"});

    `ifdef SLA_RAL_COVERAGE
    sla_ral_reg::regname = "DTS0_TEMP";
    `endif
    DTS0_TEMP = pmu_mmr_DTS0_TEMP_reg::type_id::create("DTS0_TEMP", this);
    DTS0_TEMP.set_cfg(16'h0, 16'h0, 16'h0, 12'h544, 32, 32'b00000000000000000000000000000000);
    DTS0_TEMP.set_space_addr("MSG", 12'h544);
    DTS0_TEMP.set_space_addr("CR-SB",12'h544); 
    DTS0_TEMP.set_space("MSG");
    DTS0_TEMP.set_msg_opcode("CR-SB");
    DTS0_TEMP.set_security_policy("security_Policy_group", "PMU_SAI_POLICY2");
    if ( $test$plusargs("DTS0_TEMP:dont_test") ) DTS0_TEMP.set_test_reg(1'b0);
    if (!add_reg( DTS0_TEMP )) begin
      `sla_error(get_name(), ("could not add register DTS0_TEMP"));
    end
   DTS0_TEMP.TEMP0.set_paths({"DTS0_TEMP.TEMP0"});
   DTS0_TEMP.TEMP1.set_paths({"DTS0_TEMP.TEMP1"});
   DTS0_TEMP.TEMP2.set_paths({"DTS0_TEMP.TEMP2"});
   DTS0_TEMP.RSVD_28_27.set_paths({"DTS0_TEMP.RSVD_28_27"});
   DTS0_TEMP.VALID2.set_paths({"DTS0_TEMP.VALID2"});
   DTS0_TEMP.VALID1.set_paths({"DTS0_TEMP.VALID1"});
   DTS0_TEMP.VALID0.set_paths({"DTS0_TEMP.VALID0"});

    `ifdef SLA_RAL_COVERAGE
    sla_ral_reg::regname = "DTS1_TEMP";
    `endif
    DTS1_TEMP = pmu_mmr_DTS1_TEMP_reg::type_id::create("DTS1_TEMP", this);
    DTS1_TEMP.set_cfg(16'h0, 16'h0, 16'h0, 12'h548, 32, 32'b00000000000000000000000000000000);
    DTS1_TEMP.set_space_addr("MSG", 12'h548);
    DTS1_TEMP.set_space_addr("CR-SB",12'h548); 
    DTS1_TEMP.set_space("MSG");
    DTS1_TEMP.set_msg_opcode("CR-SB");
    DTS1_TEMP.set_security_policy("security_Policy_group", "PMU_SAI_POLICY2");
    if ( $test$plusargs("DTS1_TEMP:dont_test") ) DTS1_TEMP.set_test_reg(1'b0);
    if (!add_reg( DTS1_TEMP )) begin
      `sla_error(get_name(), ("could not add register DTS1_TEMP"));
    end
   DTS1_TEMP.TEMP0.set_paths({"DTS1_TEMP.TEMP0"});
   DTS1_TEMP.TEMP1.set_paths({"DTS1_TEMP.TEMP1"});
   DTS1_TEMP.TEMP2.set_paths({"DTS1_TEMP.TEMP2"});
   DTS1_TEMP.RSVD_28_27.set_paths({"DTS1_TEMP.RSVD_28_27"});
   DTS1_TEMP.VALID2.set_paths({"DTS1_TEMP.VALID2"});
   DTS1_TEMP.VALID1.set_paths({"DTS1_TEMP.VALID1"});
   DTS1_TEMP.VALID0.set_paths({"DTS1_TEMP.VALID0"});

    `ifdef SLA_RAL_COVERAGE
    sla_ral_reg::regname = "DTS2_TEMP";
    `endif
    DTS2_TEMP = pmu_mmr_DTS2_TEMP_reg::type_id::create("DTS2_TEMP", this);
    DTS2_TEMP.set_cfg(16'h0, 16'h0, 16'h0, 12'h54C, 32, 32'b00000000000000000000000000000000);
    DTS2_TEMP.set_space_addr("MSG", 12'h54C);
    DTS2_TEMP.set_space_addr("CR-SB",12'h54C); 
    DTS2_TEMP.set_space("MSG");
    DTS2_TEMP.set_msg_opcode("CR-SB");
    DTS2_TEMP.set_security_policy("security_Policy_group", "PMU_SAI_POLICY2");
    if ( $test$plusargs("DTS2_TEMP:dont_test") ) DTS2_TEMP.set_test_reg(1'b0);
    if (!add_reg( DTS2_TEMP )) begin
      `sla_error(get_name(), ("could not add register DTS2_TEMP"));
    end
   DTS2_TEMP.TEMP0.set_paths({"DTS2_TEMP.TEMP0"});
   DTS2_TEMP.TEMP1.set_paths({"DTS2_TEMP.TEMP1"});
   DTS2_TEMP.TEMP2.set_paths({"DTS2_TEMP.TEMP2"});
   DTS2_TEMP.RSVD_28_27.set_paths({"DTS2_TEMP.RSVD_28_27"});
   DTS2_TEMP.VALID2.set_paths({"DTS2_TEMP.VALID2"});
   DTS2_TEMP.VALID1.set_paths({"DTS2_TEMP.VALID1"});
   DTS2_TEMP.VALID0.set_paths({"DTS2_TEMP.VALID0"});

    `ifdef SLA_RAL_COVERAGE
    sla_ral_reg::regname = "SEQENG_STS";
    `endif
    SEQENG_STS = pmu_mmr_SEQENG_STS_reg::type_id::create("SEQENG_STS", this);
    SEQENG_STS.set_cfg(16'h0, 16'h0, 16'h0, 12'h600, 32, 32'b00000000000000000000000000000000);
    SEQENG_STS.set_space_addr("MSG", 12'h600);
    SEQENG_STS.set_space_addr("CR-SB",12'h600); 
    SEQENG_STS.set_space("MSG");
    SEQENG_STS.set_msg_opcode("CR-SB");
    SEQENG_STS.set_security_policy("security_Policy_group", "PMU_SAI_POLICY0");
    if ( $test$plusargs("SEQENG_STS:dont_test") ) SEQENG_STS.set_test_reg(1'b0);
    if (!add_reg( SEQENG_STS )) begin
      `sla_error(get_name(), ("could not add register SEQENG_STS"));
    end
   SEQENG_STS.BOOT_START.set_paths({"SEQENG_STS.BOOT_START"});
   SEQENG_STS.TRIG_PEND1.set_paths({"SEQENG_STS.TRIG_PEND1"});
   SEQENG_STS.TRIG_PEND2.set_paths({"SEQENG_STS.TRIG_PEND2"});
   SEQENG_STS.TRIG_PEND3.set_paths({"SEQENG_STS.TRIG_PEND3"});
   SEQENG_STS.TRIG_PEND4.set_paths({"SEQENG_STS.TRIG_PEND4"});
   SEQENG_STS.TRIG_PEND5.set_paths({"SEQENG_STS.TRIG_PEND5"});
   SEQENG_STS.TRIG_PEND6.set_paths({"SEQENG_STS.TRIG_PEND6"});
   SEQENG_STS.TRIG_PEND7.set_paths({"SEQENG_STS.TRIG_PEND7"});
   SEQENG_STS.TRIG_PEND8.set_paths({"SEQENG_STS.TRIG_PEND8"});
   SEQENG_STS.TRIG_PEND9.set_paths({"SEQENG_STS.TRIG_PEND9"});
   SEQENG_STS.TRIG_PEND10.set_paths({"SEQENG_STS.TRIG_PEND10"});
   SEQENG_STS.TRIG_PEND11.set_paths({"SEQENG_STS.TRIG_PEND11"});
   SEQENG_STS.TRIG_PEND12.set_paths({"SEQENG_STS.TRIG_PEND12"});
   SEQENG_STS.TRIG_PEND13.set_paths({"SEQENG_STS.TRIG_PEND13"});
   SEQENG_STS.TRIG_PEND14.set_paths({"SEQENG_STS.TRIG_PEND14"});
   SEQENG_STS.PMETO_PEND.set_paths({"SEQENG_STS.PMETO_PEND"});
   SEQENG_STS.BOOT_DONE.set_paths({"SEQENG_STS.BOOT_DONE"});
   SEQENG_STS.TRIG_DONE1.set_paths({"SEQENG_STS.TRIG_DONE1"});
   SEQENG_STS.TRIG_DONE2.set_paths({"SEQENG_STS.TRIG_DONE2"});
   SEQENG_STS.TRIG_DONE3.set_paths({"SEQENG_STS.TRIG_DONE3"});
   SEQENG_STS.TRIG_DONE4.set_paths({"SEQENG_STS.TRIG_DONE4"});
   SEQENG_STS.TRIG_DONE5.set_paths({"SEQENG_STS.TRIG_DONE5"});
   SEQENG_STS.TRIG_DONE6.set_paths({"SEQENG_STS.TRIG_DONE6"});
   SEQENG_STS.TRIG_DONE7.set_paths({"SEQENG_STS.TRIG_DONE7"});
   SEQENG_STS.TRIG_DONE8.set_paths({"SEQENG_STS.TRIG_DONE8"});
   SEQENG_STS.TRIG_DONE9.set_paths({"SEQENG_STS.TRIG_DONE9"});
   SEQENG_STS.TRIG_DONE10.set_paths({"SEQENG_STS.TRIG_DONE10"});
   SEQENG_STS.TRIG_DONE11.set_paths({"SEQENG_STS.TRIG_DONE11"});
   SEQENG_STS.TRIG_DONE12.set_paths({"SEQENG_STS.TRIG_DONE12"});
   SEQENG_STS.TRIG_DONE13.set_paths({"SEQENG_STS.TRIG_DONE13"});
   SEQENG_STS.TRIG_DONE14.set_paths({"SEQENG_STS.TRIG_DONE14"});
   SEQENG_STS.PMETO_DONE.set_paths({"SEQENG_STS.PMETO_DONE"});

    `ifdef SLA_RAL_COVERAGE
    sla_ral_reg::regname = "SEQENG_STS_1";
    `endif
    SEQENG_STS_1 = pmu_mmr_SEQENG_STS_1_reg::type_id::create("SEQENG_STS_1", this);
    SEQENG_STS_1.set_cfg(16'h0, 16'h0, 16'h0, 12'h604, 32, 32'b00000000000000000000000000000000);
    SEQENG_STS_1.set_space_addr("MSG", 12'h604);
    SEQENG_STS_1.set_space_addr("CR-SB",12'h604); 
    SEQENG_STS_1.set_space("MSG");
    SEQENG_STS_1.set_msg_opcode("CR-SB");
    SEQENG_STS_1.set_security_policy("security_Policy_group", "PMU_SAI_POLICY0");
    if ( $test$plusargs("SEQENG_STS_1:dont_test") ) SEQENG_STS_1.set_test_reg(1'b0);
    if (!add_reg( SEQENG_STS_1 )) begin
      `sla_error(get_name(), ("could not add register SEQENG_STS_1"));
    end
   SEQENG_STS_1.BOOT_START_WIP.set_paths({"SEQENG_STS_1.BOOT_START_WIP"});
   SEQENG_STS_1.TRIG_WIP1.set_paths({"SEQENG_STS_1.TRIG_WIP1"});
   SEQENG_STS_1.TRIG_WIP2.set_paths({"SEQENG_STS_1.TRIG_WIP2"});
   SEQENG_STS_1.TRIG_WIP3.set_paths({"SEQENG_STS_1.TRIG_WIP3"});
   SEQENG_STS_1.TRIG_WIP4.set_paths({"SEQENG_STS_1.TRIG_WIP4"});
   SEQENG_STS_1.TRIG_WIP5.set_paths({"SEQENG_STS_1.TRIG_WIP5"});
   SEQENG_STS_1.TRIG_WIP6.set_paths({"SEQENG_STS_1.TRIG_WIP6"});
   SEQENG_STS_1.TRIG_WIP7.set_paths({"SEQENG_STS_1.TRIG_WIP7"});
   SEQENG_STS_1.TRIG_WIP8.set_paths({"SEQENG_STS_1.TRIG_WIP8"});
   SEQENG_STS_1.TRIG_WIP9.set_paths({"SEQENG_STS_1.TRIG_WIP9"});
   SEQENG_STS_1.TRIG_WIP10.set_paths({"SEQENG_STS_1.TRIG_WIP10"});
   SEQENG_STS_1.TRIG_WIP11.set_paths({"SEQENG_STS_1.TRIG_WIP11"});
   SEQENG_STS_1.TRIG_WIP12.set_paths({"SEQENG_STS_1.TRIG_WIP12"});
   SEQENG_STS_1.TRIG_WIP13.set_paths({"SEQENG_STS_1.TRIG_WIP13"});
   SEQENG_STS_1.TRIG_WIP14.set_paths({"SEQENG_STS_1.TRIG_WIP14"});
   SEQENG_STS_1.PMETO_WIP.set_paths({"SEQENG_STS_1.PMETO_WIP"});
   SEQENG_STS_1.CPL_SEQ_ID.set_paths({"SEQENG_STS_1.CPL_SEQ_ID"});
   SEQENG_STS_1.CPL_SEQ_LN.set_paths({"SEQENG_STS_1.CPL_SEQ_LN"});

    `ifdef SLA_RAL_COVERAGE
    sla_ral_reg::regname = "SEQENG_EVENTS";
    `endif
    SEQENG_EVENTS = pmu_mmr_SEQENG_EVENTS_reg::type_id::create("SEQENG_EVENTS", this);
    SEQENG_EVENTS.set_cfg(16'h0, 16'h0, 16'h0, 12'h608, 32, 32'b00000000000000000000000000000000);
    SEQENG_EVENTS.set_space_addr("MSG", 12'h608);
    SEQENG_EVENTS.set_space_addr("CR-SB",12'h608); 
    SEQENG_EVENTS.set_space("MSG");
    SEQENG_EVENTS.set_msg_opcode("CR-SB");
    SEQENG_EVENTS.set_security_policy("security_Policy_group", "PMU_SAI_POLICY0");
    if ( $test$plusargs("SEQENG_EVENTS:dont_test") ) SEQENG_EVENTS.set_test_reg(1'b0);
    if (!add_reg( SEQENG_EVENTS )) begin
      `sla_error(get_name(), ("could not add register SEQENG_EVENTS"));
    end
   SEQENG_EVENTS.BOOT_START_EVENT.set_paths({"SEQENG_EVENTS.BOOT_START_EVENT"});
   SEQENG_EVENTS.TRIG_EVENT1.set_paths({"SEQENG_EVENTS.TRIG_EVENT1"});
   SEQENG_EVENTS.TRIG_EVENT2.set_paths({"SEQENG_EVENTS.TRIG_EVENT2"});
   SEQENG_EVENTS.TRIG_EVENT3.set_paths({"SEQENG_EVENTS.TRIG_EVENT3"});
   SEQENG_EVENTS.TRIG_EVENT4.set_paths({"SEQENG_EVENTS.TRIG_EVENT4"});
   SEQENG_EVENTS.TRIG_EVENT5.set_paths({"SEQENG_EVENTS.TRIG_EVENT5"});
   SEQENG_EVENTS.TRIG_EVENTi6.set_paths({"SEQENG_EVENTS.TRIG_EVENTi6"});
   SEQENG_EVENTS.TRIG_EVENT7.set_paths({"SEQENG_EVENTS.TRIG_EVENT7"});
   SEQENG_EVENTS.TRIG_EVENT8.set_paths({"SEQENG_EVENTS.TRIG_EVENT8"});
   SEQENG_EVENTS.TRIG_EVENT9.set_paths({"SEQENG_EVENTS.TRIG_EVENT9"});
   SEQENG_EVENTS.TRIG_EVENT10.set_paths({"SEQENG_EVENTS.TRIG_EVENT10"});
   SEQENG_EVENTS.TRIG_EVENT11.set_paths({"SEQENG_EVENTS.TRIG_EVENT11"});
   SEQENG_EVENTS.TRIG_EVENT12.set_paths({"SEQENG_EVENTS.TRIG_EVENT12"});
   SEQENG_EVENTS.TRIG_EVENT13.set_paths({"SEQENG_EVENTS.TRIG_EVENT13"});
   SEQENG_EVENTS.TRIG_EVENT14.set_paths({"SEQENG_EVENTS.TRIG_EVENT14"});
   SEQENG_EVENTS.PME_TO_EVENT.set_paths({"SEQENG_EVENTS.PME_TO_EVENT"});
   SEQENG_EVENTS.RSVD_31_15.set_paths({"SEQENG_EVENTS.RSVD_31_15"});

    `ifdef SLA_RAL_COVERAGE
    sla_ral_reg::regname = "SEQENG_EVT_POL";
    `endif
    SEQENG_EVT_POL = pmu_mmr_SEQENG_EVT_POL_reg::type_id::create("SEQENG_EVT_POL", this);
    SEQENG_EVT_POL.set_cfg(16'h0, 16'h0, 16'h0, 12'h60C, 32, 32'b00000000000000000000000000000000);
    SEQENG_EVT_POL.set_space_addr("MSG", 12'h60C);
    SEQENG_EVT_POL.set_space_addr("CR-SB",12'h60C); 
    SEQENG_EVT_POL.set_space("MSG");
    SEQENG_EVT_POL.set_msg_opcode("CR-SB");
    SEQENG_EVT_POL.set_security_policy("security_Policy_group", "PMU_SAI_POLICY0");
    if ( $test$plusargs("SEQENG_EVT_POL:dont_test") ) SEQENG_EVT_POL.set_test_reg(1'b0);
    if (!add_reg( SEQENG_EVT_POL )) begin
      `sla_error(get_name(), ("could not add register SEQENG_EVT_POL"));
    end
   SEQENG_EVT_POL.TRIG_EVT_POL.set_paths({"SEQENG_EVT_POL.TRIG_EVT_POL"});
   SEQENG_EVT_POL.RSVD_31_16.set_paths({"SEQENG_EVT_POL.RSVD_31_16"});

    `ifdef SLA_RAL_COVERAGE
    sla_ral_reg::regname = "FUSE_PULL_STS";
    `endif
    FUSE_PULL_STS = pmu_mmr_FUSE_PULL_STS_reg::type_id::create("FUSE_PULL_STS", this);
    FUSE_PULL_STS.set_cfg(16'h0, 16'h0, 16'h0, 12'h610, 32, 32'b00000000000000000000000000000000);
    FUSE_PULL_STS.set_space_addr("MSG", 12'h610);
    FUSE_PULL_STS.set_space_addr("CR-SB",12'h610); 
    FUSE_PULL_STS.set_space("MSG");
    FUSE_PULL_STS.set_msg_opcode("CR-SB");
    FUSE_PULL_STS.set_security_policy("security_Policy_group", "PMU_SAI_POLICY0");
    if ( $test$plusargs("FUSE_PULL_STS:dont_test") ) FUSE_PULL_STS.set_test_reg(1'b0);
    if (!add_reg( FUSE_PULL_STS )) begin
      `sla_error(get_name(), ("could not add register FUSE_PULL_STS"));
    end
   FUSE_PULL_STS.FP_FUSE_ERR.set_paths({"FUSE_PULL_STS.FP_FUSE_ERR"});
   FUSE_PULL_STS.RSVD_7_1.set_paths({"FUSE_PULL_STS.RSVD_7_1"});
   FUSE_PULL_STS.FP_FUSE_ERR_TYP.set_paths({"FUSE_PULL_STS.FP_FUSE_ERR_TYP"});
   FUSE_PULL_STS.FP_STRP_ERR.set_paths({"FUSE_PULL_STS.FP_STRP_ERR"});
   FUSE_PULL_STS.RSVD_23_17.set_paths({"FUSE_PULL_STS.RSVD_23_17"});
   FUSE_PULL_STS.FP_STRP_ERR_TYP.set_paths({"FUSE_PULL_STS.FP_STRP_ERR_TYP"});

    `ifdef SLA_RAL_COVERAGE
    sla_ral_reg::regname = "FW_CPL_STS";
    `endif
    FW_CPL_STS = pmu_mmr_FW_CPL_STS_reg::type_id::create("FW_CPL_STS", this);
    FW_CPL_STS.set_cfg(16'h0, 16'h0, 16'h0, 12'h614, 32, 32'b00000000000000000000000000000000);
    FW_CPL_STS.set_space_addr("MSG", 12'h614);
    FW_CPL_STS.set_space_addr("CR-SB",12'h614); 
    FW_CPL_STS.set_space("MSG");
    FW_CPL_STS.set_msg_opcode("CR-SB");
    FW_CPL_STS.set_security_policy("security_Policy_group", "PMU_SAI_POLICY0");
    if ( $test$plusargs("FW_CPL_STS:dont_test") ) FW_CPL_STS.set_test_reg(1'b0);
    if (!add_reg( FW_CPL_STS )) begin
      `sla_error(get_name(), ("could not add register FW_CPL_STS"));
    end
   FW_CPL_STS.FW_CPL_ERR.set_paths({"FW_CPL_STS.FW_CPL_ERR"});
   FW_CPL_STS.CPL_SEQ_ID.set_paths({"FW_CPL_STS.CPL_SEQ_ID"});
   FW_CPL_STS.CPL_SEQ_LN.set_paths({"FW_CPL_STS.CPL_SEQ_LN"});
   FW_CPL_STS.RSVD_31_18.set_paths({"FW_CPL_STS.RSVD_31_18"});

    `ifdef SLA_RAL_COVERAGE
    sla_ral_reg::regname = "PTCH_CPL_STS";
    `endif
    PTCH_CPL_STS = pmu_mmr_PTCH_CPL_STS_reg::type_id::create("PTCH_CPL_STS", this);
    PTCH_CPL_STS.set_cfg(16'h0, 16'h0, 16'h0, 12'h618, 32, 32'b00000000000000000000000000000000);
    PTCH_CPL_STS.set_space_addr("MSG", 12'h618);
    PTCH_CPL_STS.set_space_addr("CR-SB",12'h618); 
    PTCH_CPL_STS.set_space("MSG");
    PTCH_CPL_STS.set_msg_opcode("CR-SB");
    PTCH_CPL_STS.set_security_policy("security_Policy_group", "PMU_SAI_POLICY0");
    if ( $test$plusargs("PTCH_CPL_STS:dont_test") ) PTCH_CPL_STS.set_test_reg(1'b0);
    if (!add_reg( PTCH_CPL_STS )) begin
      `sla_error(get_name(), ("could not add register PTCH_CPL_STS"));
    end
   PTCH_CPL_STS.PTCH_FTCH_ERR.set_paths({"PTCH_CPL_STS.PTCH_FTCH_ERR"});
   PTCH_CPL_STS.PTCH_FTCH_ADDR.set_paths({"PTCH_CPL_STS.PTCH_FTCH_ADDR"});
   PTCH_CPL_STS.PTCH_FTCH_SEQ_ID.set_paths({"PTCH_CPL_STS.PTCH_FTCH_SEQ_ID"});
   PTCH_CPL_STS.PTCH_FTCH_SEQ_LN.set_paths({"PTCH_CPL_STS.PTCH_FTCH_SEQ_LN"});

    `ifdef SLA_RAL_COVERAGE
    sla_ral_reg::regname = "CH_CPL_STS";
    `endif
    CH_CPL_STS = pmu_mmr_CH_CPL_STS_reg::type_id::create("CH_CPL_STS", this);
    CH_CPL_STS.set_cfg(16'h0, 16'h0, 16'h0, 12'h61C, 32, 32'b00000000000000000000000000000000);
    CH_CPL_STS.set_space_addr("MSG", 12'h61C);
    CH_CPL_STS.set_space_addr("CR-SB",12'h61C); 
    CH_CPL_STS.set_space("MSG");
    CH_CPL_STS.set_msg_opcode("CR-SB");
    CH_CPL_STS.set_security_policy("security_Policy_group", "PMU_SAI_POLICY0");
    if ( $test$plusargs("CH_CPL_STS:dont_test") ) CH_CPL_STS.set_test_reg(1'b0);
    if (!add_reg( CH_CPL_STS )) begin
      `sla_error(get_name(), ("could not add register CH_CPL_STS"));
    end
   CH_CPL_STS.FIRST_ERR.set_paths({"CH_CPL_STS.FIRST_ERR"});
   CH_CPL_STS.FIRST_ERR_SRC.set_paths({"CH_CPL_STS.FIRST_ERR_SRC"});
   CH_CPL_STS.LAST_ERR.set_paths({"CH_CPL_STS.LAST_ERR"});
   CH_CPL_STS.LAST_ERR_SRC.set_paths({"CH_CPL_STS.LAST_ERR_SRC"});

    `ifdef SLA_RAL_COVERAGE
    sla_ral_reg::regname = "IPRDY_STS";
    `endif
    IPRDY_STS = pmu_mmr_IPRDY_STS_reg::type_id::create("IPRDY_STS", this);
    IPRDY_STS.set_cfg(16'h0, 16'h0, 16'h0, 12'h620, 32, 32'b00000000000000000000000000000000);
    IPRDY_STS.set_space_addr("MSG", 12'h620);
    IPRDY_STS.set_space_addr("CR-SB",12'h620); 
    IPRDY_STS.set_space("MSG");
    IPRDY_STS.set_msg_opcode("CR-SB");
    IPRDY_STS.set_security_policy("security_Policy_group", "PMU_SAI_POLICY0");
    if ( $test$plusargs("IPRDY_STS:dont_test") ) IPRDY_STS.set_test_reg(1'b0);
    if (!add_reg( IPRDY_STS )) begin
      `sla_error(get_name(), ("could not add register IPRDY_STS"));
    end
   IPRDY_STS.FIRST_ERR.set_paths({"IPRDY_STS.FIRST_ERR"});
   IPRDY_STS.FIRST_ERR_SRC.set_paths({"IPRDY_STS.FIRST_ERR_SRC"});
   IPRDY_STS.LAST_ERR.set_paths({"IPRDY_STS.LAST_ERR"});
   IPRDY_STS.LAST_ERR_SRC.set_paths({"IPRDY_STS.LAST_ERR_SRC"});

    `ifdef SLA_RAL_COVERAGE
    sla_ral_reg::regname = "BPRP_ACK_STS";
    `endif
    BPRP_ACK_STS = pmu_mmr_BPRP_ACK_STS_reg::type_id::create("BPRP_ACK_STS", this);
    BPRP_ACK_STS.set_cfg(16'h0, 16'h0, 16'h0, 12'h624, 32, 32'b00000000000000000000000000000000);
    BPRP_ACK_STS.set_space_addr("MSG", 12'h624);
    BPRP_ACK_STS.set_space_addr("CR-SB",12'h624); 
    BPRP_ACK_STS.set_space("MSG");
    BPRP_ACK_STS.set_msg_opcode("CR-SB");
    BPRP_ACK_STS.set_security_policy("security_Policy_group", "PMU_SAI_POLICY0");
    if ( $test$plusargs("BPRP_ACK_STS:dont_test") ) BPRP_ACK_STS.set_test_reg(1'b0);
    if (!add_reg( BPRP_ACK_STS )) begin
      `sla_error(get_name(), ("could not add register BPRP_ACK_STS"));
    end
   BPRP_ACK_STS.FIRST_ERR.set_paths({"BPRP_ACK_STS.FIRST_ERR"});
   BPRP_ACK_STS.FIRST_ERR_SRC.set_paths({"BPRP_ACK_STS.FIRST_ERR_SRC"});
   BPRP_ACK_STS.LAST_ERR.set_paths({"BPRP_ACK_STS.LAST_ERR"});
   BPRP_ACK_STS.LAST_ERR_SRC.set_paths({"BPRP_ACK_STS.LAST_ERR_SRC"});

    `ifdef SLA_RAL_COVERAGE
    sla_ral_reg::regname = "PMETO_STS";
    `endif
    PMETO_STS = pmu_mmr_PMETO_STS_reg::type_id::create("PMETO_STS", this);
    PMETO_STS.set_cfg(16'h0, 16'h0, 16'h0, 12'h628, 32, 32'b00000000000000000000000000000000);
    PMETO_STS.set_space_addr("MSG", 12'h628);
    PMETO_STS.set_space_addr("CR-SB",12'h628); 
    PMETO_STS.set_space("MSG");
    PMETO_STS.set_msg_opcode("CR-SB");
    PMETO_STS.set_security_policy("security_Policy_group", "PMU_SAI_POLICY0");
    if ( $test$plusargs("PMETO_STS:dont_test") ) PMETO_STS.set_test_reg(1'b0);
    if (!add_reg( PMETO_STS )) begin
      `sla_error(get_name(), ("could not add register PMETO_STS"));
    end
   PMETO_STS.FIRST_ERR.set_paths({"PMETO_STS.FIRST_ERR"});
   PMETO_STS.FIRST_ERR_SRC.set_paths({"PMETO_STS.FIRST_ERR_SRC"});
   PMETO_STS.LAST_ERR.set_paths({"PMETO_STS.LAST_ERR"});
   PMETO_STS.LAST_ERR_SRC.set_paths({"PMETO_STS.LAST_ERR_SRC"});

    `ifdef SLA_RAL_COVERAGE
    sla_ral_reg::regname = "TELEM_STS";
    `endif
    TELEM_STS = pmu_mmr_TELEM_STS_reg::type_id::create("TELEM_STS", this);
    TELEM_STS.set_cfg(16'h0, 16'h0, 16'h0, 12'h62C, 32, 32'b00000000000000000000000000000000);
    TELEM_STS.set_space_addr("MSG", 12'h62C);
    TELEM_STS.set_space_addr("CR-SB",12'h62C); 
    TELEM_STS.set_space("MSG");
    TELEM_STS.set_msg_opcode("CR-SB");
    TELEM_STS.set_security_policy("security_Policy_group", "PMU_SAI_POLICY0");
    if ( $test$plusargs("TELEM_STS:dont_test") ) TELEM_STS.set_test_reg(1'b0);
    if (!add_reg( TELEM_STS )) begin
      `sla_error(get_name(), ("could not add register TELEM_STS"));
    end
   TELEM_STS.FIRST_ERR.set_paths({"TELEM_STS.FIRST_ERR"});
   TELEM_STS.FIRST_ERR_SRC.set_paths({"TELEM_STS.FIRST_ERR_SRC"});
   TELEM_STS.LAST_ERR.set_paths({"TELEM_STS.LAST_ERR"});
   TELEM_STS.LAST_ERR_SRC.set_paths({"TELEM_STS.LAST_ERR_SRC"});

    `ifdef SLA_RAL_COVERAGE
    sla_ral_reg::regname = "SB_EP_DBG_CTL";
    `endif
    SB_EP_DBG_CTL = pmu_mmr_SB_EP_DBG_CTL_reg::type_id::create("SB_EP_DBG_CTL", this);
    SB_EP_DBG_CTL.set_cfg(16'h0, 16'h0, 16'h0, 12'h630, 32, 32'b00000000000000000000000000100000);
    SB_EP_DBG_CTL.set_space_addr("MSG", 12'h630);
    SB_EP_DBG_CTL.set_space_addr("CR-SB",12'h630); 
    SB_EP_DBG_CTL.set_space("MSG");
    SB_EP_DBG_CTL.set_msg_opcode("CR-SB");
    SB_EP_DBG_CTL.set_security_policy("security_Policy_group", "PMU_SAI_POLICY0");
    if ( $test$plusargs("SB_EP_DBG_CTL:dont_test") ) SB_EP_DBG_CTL.set_test_reg(1'b0);
    if (!add_reg( SB_EP_DBG_CTL )) begin
      `sla_error(get_name(), ("could not add register SB_EP_DBG_CTL"));
    end
   SB_EP_DBG_CTL.ISM_FRC_NOT_IDL.set_paths({"SB_EP_DBG_CTL.ISM_FRC_NOT_IDL"});
   SB_EP_DBG_CTL.ISM_FRC_IDL.set_paths({"SB_EP_DBG_CTL.ISM_FRC_IDL"});
   SB_EP_DBG_CTL.RSVD_2.set_paths({"SB_EP_DBG_CTL.RSVD_2"});
   SB_EP_DBG_CTL.SC_CG_OVR.set_paths({"SB_EP_DBG_CTL.SC_CG_OVR"});
   SB_EP_DBG_CTL.FRC_CREDIT_REQ.set_paths({"SB_EP_DBG_CTL.FRC_CREDIT_REQ"});
   SB_EP_DBG_CTL.SBCG_EN.set_paths({"SB_EP_DBG_CTL.SBCG_EN"});
   SB_EP_DBG_CTL.RSVD_7_6.set_paths({"SB_EP_DBG_CTL.RSVD_7_6"});
   SB_EP_DBG_CTL.PMSB_ISM_FRC_NOT_IDL.set_paths({"SB_EP_DBG_CTL.PMSB_ISM_FRC_NOT_IDL"});
   SB_EP_DBG_CTL.PMSB_ISM_FRC_IDL.set_paths({"SB_EP_DBG_CTL.PMSB_ISM_FRC_IDL"});
   SB_EP_DBG_CTL.RSVD_10.set_paths({"SB_EP_DBG_CTL.RSVD_10"});
   SB_EP_DBG_CTL.PMSB_SC_CG_OVR.set_paths({"SB_EP_DBG_CTL.PMSB_SC_CG_OVR"});
   SB_EP_DBG_CTL.PMSB_FRC_CREDIT_REQ.set_paths({"SB_EP_DBG_CTL.PMSB_FRC_CREDIT_REQ"});
   SB_EP_DBG_CTL.RSVD_15_13.set_paths({"SB_EP_DBG_CTL.RSVD_15_13"});

    `ifdef SLA_RAL_COVERAGE
    sla_ral_reg::regname = "SURVIVABILITY_CTL_0";
    `endif
    SURVIVABILITY_CTL_0 = pmu_mmr_SURVIVABILITY_CTL_0_reg::type_id::create("SURVIVABILITY_CTL_0", this);
    SURVIVABILITY_CTL_0.set_cfg(16'h0, 16'h0, 16'h0, 12'h680, 32, 32'b00100111000100000000000000000000);
    SURVIVABILITY_CTL_0.set_space_addr("MSG", 12'h680);
    SURVIVABILITY_CTL_0.set_space_addr("CR-SB",12'h680); 
    SURVIVABILITY_CTL_0.set_space("MSG");
    SURVIVABILITY_CTL_0.set_msg_opcode("CR-SB");
    SURVIVABILITY_CTL_0.set_security_policy("security_Policy_group", "PMU_SAI_POLICY0");
    if ( $test$plusargs("SURVIVABILITY_CTL_0:dont_test") ) SURVIVABILITY_CTL_0.set_test_reg(1'b0);
    if (!add_reg( SURVIVABILITY_CTL_0 )) begin
      `sla_error(get_name(), ("could not add register SURVIVABILITY_CTL_0"));
    end
   SURVIVABILITY_CTL_0.CHK_TMR_EN.set_paths({"SURVIVABILITY_CTL_0.CHK_TMR_EN"});
   SURVIVABILITY_CTL_0.CHK_FORCE.set_paths({"SURVIVABILITY_CTL_0.CHK_FORCE"});
   SURVIVABILITY_CTL_0.HLT_RESUME.set_paths({"SURVIVABILITY_CTL_0.HLT_RESUME"});
   SURVIVABILITY_CTL_0.SURV_CTL.set_paths({"SURVIVABILITY_CTL_0.SURV_CTL"});
   SURVIVABILITY_CTL_0.CHK_EXP_CNT.set_paths({"SURVIVABILITY_CTL_0.CHK_EXP_CNT"});

    `ifdef SLA_RAL_COVERAGE
    sla_ral_reg::regname = "SURVIVABILITY_CTL_1";
    `endif
    SURVIVABILITY_CTL_1 = pmu_mmr_SURVIVABILITY_CTL_1_reg::type_id::create("SURVIVABILITY_CTL_1", this);
    SURVIVABILITY_CTL_1.set_cfg(16'h0, 16'h0, 16'h0, 12'h684, 32, 32'b00000000000000000000000000000000);
    SURVIVABILITY_CTL_1.set_space_addr("MSG", 12'h684);
    SURVIVABILITY_CTL_1.set_space_addr("CR-SB",12'h684); 
    SURVIVABILITY_CTL_1.set_space("MSG");
    SURVIVABILITY_CTL_1.set_msg_opcode("CR-SB");
    SURVIVABILITY_CTL_1.set_security_policy("security_Policy_group", "PMU_SAI_POLICY0");
    if ( $test$plusargs("SURVIVABILITY_CTL_1:dont_test") ) SURVIVABILITY_CTL_1.set_test_reg(1'b0);
    if (!add_reg( SURVIVABILITY_CTL_1 )) begin
      `sla_error(get_name(), ("could not add register SURVIVABILITY_CTL_1"));
    end
   SURVIVABILITY_CTL_1.SURV_CTL.set_paths({"SURVIVABILITY_CTL_1.SURV_CTL"});

    `ifdef SLA_RAL_COVERAGE
    sla_ral_reg::regname = "SURVIVABILITY_STS_0";
    `endif
    SURVIVABILITY_STS_0 = pmu_mmr_SURVIVABILITY_STS_0_reg::type_id::create("SURVIVABILITY_STS_0", this);
    SURVIVABILITY_STS_0.set_cfg(16'h0, 16'h0, 16'h0, 12'h688, 32, 32'b00000000000000000000000000000000);
    SURVIVABILITY_STS_0.set_space_addr("MSG", 12'h688);
    SURVIVABILITY_STS_0.set_space_addr("CR-SB",12'h688); 
    SURVIVABILITY_STS_0.set_space("MSG");
    SURVIVABILITY_STS_0.set_msg_opcode("CR-SB");
    SURVIVABILITY_STS_0.set_security_policy("security_Policy_group", "PMU_SAI_POLICY0");
    if ( $test$plusargs("SURVIVABILITY_STS_0:dont_test") ) SURVIVABILITY_STS_0.set_test_reg(1'b0);
    if (!add_reg( SURVIVABILITY_STS_0 )) begin
      `sla_error(get_name(), ("could not add register SURVIVABILITY_STS_0"));
    end
   SURVIVABILITY_STS_0.CHK_TIMER_FIRED.set_paths({"SURVIVABILITY_STS_0.CHK_TIMER_FIRED"});
   SURVIVABILITY_STS_0.SURV_STS.set_paths({"SURVIVABILITY_STS_0.SURV_STS"});

    `ifdef SLA_RAL_COVERAGE
    sla_ral_reg::regname = "SURVIVABILITY_STS_1";
    `endif
    SURVIVABILITY_STS_1 = pmu_mmr_SURVIVABILITY_STS_1_reg::type_id::create("SURVIVABILITY_STS_1", this);
    SURVIVABILITY_STS_1.set_cfg(16'h0, 16'h0, 16'h0, 12'h68C, 32, 32'b00000000000000000000000000000000);
    SURVIVABILITY_STS_1.set_space_addr("MSG", 12'h68C);
    SURVIVABILITY_STS_1.set_space_addr("CR-SB",12'h68C); 
    SURVIVABILITY_STS_1.set_space("MSG");
    SURVIVABILITY_STS_1.set_msg_opcode("CR-SB");
    SURVIVABILITY_STS_1.set_security_policy("security_Policy_group", "PMU_SAI_POLICY0");
    if ( $test$plusargs("SURVIVABILITY_STS_1:dont_test") ) SURVIVABILITY_STS_1.set_test_reg(1'b0);
    if (!add_reg( SURVIVABILITY_STS_1 )) begin
      `sla_error(get_name(), ("could not add register SURVIVABILITY_STS_1"));
    end
   SURVIVABILITY_STS_1.SURV_STS.set_paths({"SURVIVABILITY_STS_1.SURV_STS"});

    `ifdef SLA_RAL_COVERAGE
    sla_ral_reg::regname = "IP_FORCE_FET_EN_OVR_EN_0";
    `endif
    IP_FORCE_FET_EN_OVR_EN_0 = pmu_mmr_IP_FORCE_FET_EN_OVR_EN_0_reg::type_id::create("IP_FORCE_FET_EN_OVR_EN_0", this);
    IP_FORCE_FET_EN_OVR_EN_0.set_cfg(16'h0, 16'h0, 16'h0, 12'h690, 32, 32'b00000000000000000000000000000000);
    IP_FORCE_FET_EN_OVR_EN_0.set_space_addr("MSG", 12'h690);
    IP_FORCE_FET_EN_OVR_EN_0.set_space_addr("CR-SB",12'h690); 
    IP_FORCE_FET_EN_OVR_EN_0.set_space("MSG");
    IP_FORCE_FET_EN_OVR_EN_0.set_msg_opcode("CR-SB");
    IP_FORCE_FET_EN_OVR_EN_0.set_security_policy("security_Policy_group", "PMU_SAI_POLICY0");
    if ( $test$plusargs("IP_FORCE_FET_EN_OVR_EN_0:dont_test") ) IP_FORCE_FET_EN_OVR_EN_0.set_test_reg(1'b0);
    if (!add_reg( IP_FORCE_FET_EN_OVR_EN_0 )) begin
      `sla_error(get_name(), ("could not add register IP_FORCE_FET_EN_OVR_EN_0"));
    end
   IP_FORCE_FET_EN_OVR_EN_0.FET_EN_B_OVR_EN.set_paths({"IP_FORCE_FET_EN_OVR_EN_0.FET_EN_B_OVR_EN"});

    `ifdef SLA_RAL_COVERAGE
    sla_ral_reg::regname = "IP_FORCE_FET_EN_OVR_EN_1";
    `endif
    IP_FORCE_FET_EN_OVR_EN_1 = pmu_mmr_IP_FORCE_FET_EN_OVR_EN_1_reg::type_id::create("IP_FORCE_FET_EN_OVR_EN_1", this);
    IP_FORCE_FET_EN_OVR_EN_1.set_cfg(16'h0, 16'h0, 16'h0, 12'h694, 32, 32'b00000000000000000000000000000000);
    IP_FORCE_FET_EN_OVR_EN_1.set_space_addr("MSG", 12'h694);
    IP_FORCE_FET_EN_OVR_EN_1.set_space_addr("CR-SB",12'h694); 
    IP_FORCE_FET_EN_OVR_EN_1.set_space("MSG");
    IP_FORCE_FET_EN_OVR_EN_1.set_msg_opcode("CR-SB");
    IP_FORCE_FET_EN_OVR_EN_1.set_security_policy("security_Policy_group", "PMU_SAI_POLICY0");
    if ( $test$plusargs("IP_FORCE_FET_EN_OVR_EN_1:dont_test") ) IP_FORCE_FET_EN_OVR_EN_1.set_test_reg(1'b0);
    if (!add_reg( IP_FORCE_FET_EN_OVR_EN_1 )) begin
      `sla_error(get_name(), ("could not add register IP_FORCE_FET_EN_OVR_EN_1"));
    end
   IP_FORCE_FET_EN_OVR_EN_1.FET_EN_B_OVR_EN.set_paths({"IP_FORCE_FET_EN_OVR_EN_1.FET_EN_B_OVR_EN"});

    `ifdef SLA_RAL_COVERAGE
    sla_ral_reg::regname = "IP_FORCE_FET_EN_OVR_VAL_0";
    `endif
    IP_FORCE_FET_EN_OVR_VAL_0 = pmu_mmr_IP_FORCE_FET_EN_OVR_VAL_0_reg::type_id::create("IP_FORCE_FET_EN_OVR_VAL_0", this);
    IP_FORCE_FET_EN_OVR_VAL_0.set_cfg(16'h0, 16'h0, 16'h0, 12'h698, 32, 32'b00000000000000000000000000000000);
    IP_FORCE_FET_EN_OVR_VAL_0.set_space_addr("MSG", 12'h698);
    IP_FORCE_FET_EN_OVR_VAL_0.set_space_addr("CR-SB",12'h698); 
    IP_FORCE_FET_EN_OVR_VAL_0.set_space("MSG");
    IP_FORCE_FET_EN_OVR_VAL_0.set_msg_opcode("CR-SB");
    IP_FORCE_FET_EN_OVR_VAL_0.set_security_policy("security_Policy_group", "PMU_SAI_POLICY0");
    if ( $test$plusargs("IP_FORCE_FET_EN_OVR_VAL_0:dont_test") ) IP_FORCE_FET_EN_OVR_VAL_0.set_test_reg(1'b0);
    if (!add_reg( IP_FORCE_FET_EN_OVR_VAL_0 )) begin
      `sla_error(get_name(), ("could not add register IP_FORCE_FET_EN_OVR_VAL_0"));
    end
   IP_FORCE_FET_EN_OVR_VAL_0.FET_EN_B_OVR_VAL.set_paths({"IP_FORCE_FET_EN_OVR_VAL_0.FET_EN_B_OVR_VAL"});

    `ifdef SLA_RAL_COVERAGE
    sla_ral_reg::regname = "IP_FORCE_FET_EN_OVR_VAL_1";
    `endif
    IP_FORCE_FET_EN_OVR_VAL_1 = pmu_mmr_IP_FORCE_FET_EN_OVR_VAL_1_reg::type_id::create("IP_FORCE_FET_EN_OVR_VAL_1", this);
    IP_FORCE_FET_EN_OVR_VAL_1.set_cfg(16'h0, 16'h0, 16'h0, 12'h69C, 32, 32'b00000000000000000000000000000000);
    IP_FORCE_FET_EN_OVR_VAL_1.set_space_addr("MSG", 12'h69C);
    IP_FORCE_FET_EN_OVR_VAL_1.set_space_addr("CR-SB",12'h69C); 
    IP_FORCE_FET_EN_OVR_VAL_1.set_space("MSG");
    IP_FORCE_FET_EN_OVR_VAL_1.set_msg_opcode("CR-SB");
    IP_FORCE_FET_EN_OVR_VAL_1.set_security_policy("security_Policy_group", "PMU_SAI_POLICY0");
    if ( $test$plusargs("IP_FORCE_FET_EN_OVR_VAL_1:dont_test") ) IP_FORCE_FET_EN_OVR_VAL_1.set_test_reg(1'b0);
    if (!add_reg( IP_FORCE_FET_EN_OVR_VAL_1 )) begin
      `sla_error(get_name(), ("could not add register IP_FORCE_FET_EN_OVR_VAL_1"));
    end
   IP_FORCE_FET_EN_OVR_VAL_1.FET_EN_B_OVR_VAL.set_paths({"IP_FORCE_FET_EN_OVR_VAL_1.FET_EN_B_OVR_VAL"});

    `ifdef SLA_RAL_COVERAGE
    sla_ral_reg::regname = "GEN_CTL_0";
    `endif
    GEN_CTL_0 = pmu_mmr_GEN_CTL_0_reg::type_id::create("GEN_CTL_0", this);
    GEN_CTL_0.set_cfg(16'h0, 16'h0, 16'h0, 12'h6A0, 32, 32'b00000000000000000000000000000000);
    GEN_CTL_0.set_space_addr("MSG", 12'h6A0);
    GEN_CTL_0.set_space_addr("CR-SB",12'h6A0); 
    GEN_CTL_0.set_space("MSG");
    GEN_CTL_0.set_msg_opcode("CR-SB");
    GEN_CTL_0.set_security_policy("security_Policy_group", "PMU_SAI_POLICY0");
    if ( $test$plusargs("GEN_CTL_0:dont_test") ) GEN_CTL_0.set_test_reg(1'b0);
    if (!add_reg( GEN_CTL_0 )) begin
      `sla_error(get_name(), ("could not add register GEN_CTL_0"));
    end
   GEN_CTL_0.GEN_CTL_0.set_paths({"GEN_CTL_0.GEN_CTL_0"});
   GEN_CTL_0.GEN_CTL_1.set_paths({"GEN_CTL_0.GEN_CTL_1"});
   GEN_CTL_0.GEN_CTL_2.set_paths({"GEN_CTL_0.GEN_CTL_2"});
   GEN_CTL_0.GEN_CTL_3.set_paths({"GEN_CTL_0.GEN_CTL_3"});
   GEN_CTL_0.GEN_CTL_4.set_paths({"GEN_CTL_0.GEN_CTL_4"});
   GEN_CTL_0.GEN_CTL_5.set_paths({"GEN_CTL_0.GEN_CTL_5"});
   GEN_CTL_0.GEN_CTL_6.set_paths({"GEN_CTL_0.GEN_CTL_6"});
   GEN_CTL_0.GEN_CTL_7.set_paths({"GEN_CTL_0.GEN_CTL_7"});
   GEN_CTL_0.GEN_CTL_8.set_paths({"GEN_CTL_0.GEN_CTL_8"});
   GEN_CTL_0.GEN_CTL_9.set_paths({"GEN_CTL_0.GEN_CTL_9"});
   GEN_CTL_0.GEN_CTL_10.set_paths({"GEN_CTL_0.GEN_CTL_10"});
   GEN_CTL_0.GEN_CTL_11.set_paths({"GEN_CTL_0.GEN_CTL_11"});
   GEN_CTL_0.GEN_CTL_12.set_paths({"GEN_CTL_0.GEN_CTL_12"});
   GEN_CTL_0.GEN_CTL_13.set_paths({"GEN_CTL_0.GEN_CTL_13"});
   GEN_CTL_0.GEN_CTL_14.set_paths({"GEN_CTL_0.GEN_CTL_14"});
   GEN_CTL_0.GEN_CTL_15.set_paths({"GEN_CTL_0.GEN_CTL_15"});
   GEN_CTL_0.GEN_CTL_16.set_paths({"GEN_CTL_0.GEN_CTL_16"});
   GEN_CTL_0.GEN_CTL_17.set_paths({"GEN_CTL_0.GEN_CTL_17"});
   GEN_CTL_0.GEN_CTL_18.set_paths({"GEN_CTL_0.GEN_CTL_18"});
   GEN_CTL_0.GEN_CTL_19.set_paths({"GEN_CTL_0.GEN_CTL_19"});
   GEN_CTL_0.GEN_CTL_20.set_paths({"GEN_CTL_0.GEN_CTL_20"});
   GEN_CTL_0.GEN_CTL_21.set_paths({"GEN_CTL_0.GEN_CTL_21"});
   GEN_CTL_0.GEN_CTL_22.set_paths({"GEN_CTL_0.GEN_CTL_22"});
   GEN_CTL_0.GEN_CTL_23.set_paths({"GEN_CTL_0.GEN_CTL_23"});
   GEN_CTL_0.GEN_CTL_24.set_paths({"GEN_CTL_0.GEN_CTL_24"});
   GEN_CTL_0.GEN_CTL_25.set_paths({"GEN_CTL_0.GEN_CTL_25"});
   GEN_CTL_0.GEN_CTL_26.set_paths({"GEN_CTL_0.GEN_CTL_26"});
   GEN_CTL_0.GEN_CTL_27.set_paths({"GEN_CTL_0.GEN_CTL_27"});
   GEN_CTL_0.GEN_CTL_28.set_paths({"GEN_CTL_0.GEN_CTL_28"});
   GEN_CTL_0.GEN_CTL_29.set_paths({"GEN_CTL_0.GEN_CTL_29"});
   GEN_CTL_0.GEN_CTL_30.set_paths({"GEN_CTL_0.GEN_CTL_30"});
   GEN_CTL_0.GEN_CTL_31.set_paths({"GEN_CTL_0.GEN_CTL_31"});

    `ifdef SLA_RAL_COVERAGE
    sla_ral_reg::regname = "GEN_CTL_1";
    `endif
    GEN_CTL_1 = pmu_mmr_GEN_CTL_1_reg::type_id::create("GEN_CTL_1", this);
    GEN_CTL_1.set_cfg(16'h0, 16'h0, 16'h0, 12'h6A4, 32, 32'b00000000000000000000000000000000);
    GEN_CTL_1.set_space_addr("MSG", 12'h6A4);
    GEN_CTL_1.set_space_addr("CR-SB",12'h6A4); 
    GEN_CTL_1.set_space("MSG");
    GEN_CTL_1.set_msg_opcode("CR-SB");
    GEN_CTL_1.set_security_policy("security_Policy_group", "PMU_SAI_POLICY0");
    if ( $test$plusargs("GEN_CTL_1:dont_test") ) GEN_CTL_1.set_test_reg(1'b0);
    if (!add_reg( GEN_CTL_1 )) begin
      `sla_error(get_name(), ("could not add register GEN_CTL_1"));
    end
   GEN_CTL_1.GEN_CTL_0.set_paths({"GEN_CTL_1.GEN_CTL_0"});
   GEN_CTL_1.GEN_CTL_1.set_paths({"GEN_CTL_1.GEN_CTL_1"});
   GEN_CTL_1.GEN_CTL_2.set_paths({"GEN_CTL_1.GEN_CTL_2"});
   GEN_CTL_1.GEN_CTL_3.set_paths({"GEN_CTL_1.GEN_CTL_3"});
   GEN_CTL_1.GEN_CTL_4.set_paths({"GEN_CTL_1.GEN_CTL_4"});
   GEN_CTL_1.GEN_CTL_5.set_paths({"GEN_CTL_1.GEN_CTL_5"});
   GEN_CTL_1.GEN_CTL_6.set_paths({"GEN_CTL_1.GEN_CTL_6"});
   GEN_CTL_1.GEN_CTL_7.set_paths({"GEN_CTL_1.GEN_CTL_7"});
   GEN_CTL_1.GEN_CTL_8.set_paths({"GEN_CTL_1.GEN_CTL_8"});
   GEN_CTL_1.GEN_CTL_9.set_paths({"GEN_CTL_1.GEN_CTL_9"});
   GEN_CTL_1.GEN_CTL_10.set_paths({"GEN_CTL_1.GEN_CTL_10"});
   GEN_CTL_1.GEN_CTL_11.set_paths({"GEN_CTL_1.GEN_CTL_11"});
   GEN_CTL_1.GEN_CTL_12.set_paths({"GEN_CTL_1.GEN_CTL_12"});
   GEN_CTL_1.GEN_CTL_13.set_paths({"GEN_CTL_1.GEN_CTL_13"});
   GEN_CTL_1.GEN_CTL_14.set_paths({"GEN_CTL_1.GEN_CTL_14"});
   GEN_CTL_1.GEN_CTL_15.set_paths({"GEN_CTL_1.GEN_CTL_15"});
   GEN_CTL_1.GEN_CTL_16.set_paths({"GEN_CTL_1.GEN_CTL_16"});
   GEN_CTL_1.GEN_CTL_17.set_paths({"GEN_CTL_1.GEN_CTL_17"});
   GEN_CTL_1.GEN_CTL_18.set_paths({"GEN_CTL_1.GEN_CTL_18"});
   GEN_CTL_1.GEN_CTL_19.set_paths({"GEN_CTL_1.GEN_CTL_19"});
   GEN_CTL_1.GEN_CTL_20.set_paths({"GEN_CTL_1.GEN_CTL_20"});
   GEN_CTL_1.GEN_CTL_21.set_paths({"GEN_CTL_1.GEN_CTL_21"});
   GEN_CTL_1.GEN_CTL_22.set_paths({"GEN_CTL_1.GEN_CTL_22"});
   GEN_CTL_1.GEN_CTL_23.set_paths({"GEN_CTL_1.GEN_CTL_23"});
   GEN_CTL_1.GEN_CTL_24.set_paths({"GEN_CTL_1.GEN_CTL_24"});
   GEN_CTL_1.GEN_CTL_25.set_paths({"GEN_CTL_1.GEN_CTL_25"});
   GEN_CTL_1.GEN_CTL_26.set_paths({"GEN_CTL_1.GEN_CTL_26"});
   GEN_CTL_1.GEN_CTL_27.set_paths({"GEN_CTL_1.GEN_CTL_27"});
   GEN_CTL_1.GEN_CTL_28.set_paths({"GEN_CTL_1.GEN_CTL_28"});
   GEN_CTL_1.GEN_CTL_29.set_paths({"GEN_CTL_1.GEN_CTL_29"});
   GEN_CTL_1.GEN_CTL_30.set_paths({"GEN_CTL_1.GEN_CTL_30"});
   GEN_CTL_1.GEN_CTL_31.set_paths({"GEN_CTL_1.GEN_CTL_31"});

    `ifdef SLA_RAL_COVERAGE
    sla_ral_reg::regname = "GEN_STS_0";
    `endif
    GEN_STS_0 = pmu_mmr_GEN_STS_0_reg::type_id::create("GEN_STS_0", this);
    GEN_STS_0.set_cfg(16'h0, 16'h0, 16'h0, 12'h6A8, 32, 32'b00000000000000000000000000000000);
    GEN_STS_0.set_space_addr("MSG", 12'h6A8);
    GEN_STS_0.set_space_addr("CR-SB",12'h6A8); 
    GEN_STS_0.set_space("MSG");
    GEN_STS_0.set_msg_opcode("CR-SB");
    GEN_STS_0.set_security_policy("security_Policy_group", "PMU_SAI_POLICY0");
    if ( $test$plusargs("GEN_STS_0:dont_test") ) GEN_STS_0.set_test_reg(1'b0);
    if (!add_reg( GEN_STS_0 )) begin
      `sla_error(get_name(), ("could not add register GEN_STS_0"));
    end
   GEN_STS_0.PHY_PMU_GLBL_STABLE.set_paths({"GEN_STS_0.PHY_PMU_GLBL_STABLE"});
   GEN_STS_0.GEN_STS.set_paths({"GEN_STS_0.GEN_STS"});

    `ifdef SLA_RAL_COVERAGE
    sla_ral_reg::regname = "GEN_STS_1";
    `endif
    GEN_STS_1 = pmu_mmr_GEN_STS_1_reg::type_id::create("GEN_STS_1", this);
    GEN_STS_1.set_cfg(16'h0, 16'h0, 16'h0, 12'h6AC, 32, 32'b00000000000000000000000000000000);
    GEN_STS_1.set_space_addr("MSG", 12'h6AC);
    GEN_STS_1.set_space_addr("CR-SB",12'h6AC); 
    GEN_STS_1.set_space("MSG");
    GEN_STS_1.set_msg_opcode("CR-SB");
    GEN_STS_1.set_security_policy("security_Policy_group", "PMU_SAI_POLICY0");
    if ( $test$plusargs("GEN_STS_1:dont_test") ) GEN_STS_1.set_test_reg(1'b0);
    if (!add_reg( GEN_STS_1 )) begin
      `sla_error(get_name(), ("could not add register GEN_STS_1"));
    end
   GEN_STS_1.GEN_STS.set_paths({"GEN_STS_1.GEN_STS"});

    `ifdef SLA_RAL_COVERAGE
    sla_ral_reg::regname = "PMU_FUSE_0";
    `endif
    PMU_FUSE_0 = pmu_mmr_PMU_FUSE_0_reg::type_id::create("PMU_FUSE_0", this);
    PMU_FUSE_0.set_cfg(16'h0, 16'h0, 16'h0, 16'h2000, 32, 32'b00000000000000000000000000000000);
    PMU_FUSE_0.set_space_addr("MSG", 16'h2000);
    PMU_FUSE_0.set_space_addr("CR-SB",16'h2000); 
    PMU_FUSE_0.set_space("MSG");
    PMU_FUSE_0.set_msg_opcode("CR-SB");
    PMU_FUSE_0.set_security_policy("security_Policy_group", "PMU_SAI_POLICY0");
    if ( $test$plusargs("PMU_FUSE_0:dont_test") ) PMU_FUSE_0.set_test_reg(1'b0);
    if (!add_reg( PMU_FUSE_0 )) begin
      `sla_error(get_name(), ("could not add register PMU_FUSE_0"));
    end
   PMU_FUSE_0.FUSE_DW0.set_paths({"PMU_FUSE_0.FUSE_DW0"});

    `ifdef SLA_RAL_COVERAGE
    sla_ral_reg::regname = "PMU_FUSE_1";
    `endif
    PMU_FUSE_1 = pmu_mmr_PMU_FUSE_1_reg::type_id::create("PMU_FUSE_1", this);
    PMU_FUSE_1.set_cfg(16'h0, 16'h0, 16'h0, 16'h2004, 32, 32'b00000000000000000000000000000000);
    PMU_FUSE_1.set_space_addr("MSG", 16'h2004);
    PMU_FUSE_1.set_space_addr("CR-SB",16'h2004); 
    PMU_FUSE_1.set_space("MSG");
    PMU_FUSE_1.set_msg_opcode("CR-SB");
    PMU_FUSE_1.set_security_policy("security_Policy_group", "PMU_SAI_POLICY0");
    if ( $test$plusargs("PMU_FUSE_1:dont_test") ) PMU_FUSE_1.set_test_reg(1'b0);
    if (!add_reg( PMU_FUSE_1 )) begin
      `sla_error(get_name(), ("could not add register PMU_FUSE_1"));
    end
   PMU_FUSE_1.FUSE_DW1.set_paths({"PMU_FUSE_1.FUSE_DW1"});

    `ifdef SLA_RAL_COVERAGE
    sla_ral_reg::regname = "PMU_FUSE_2";
    `endif
    PMU_FUSE_2 = pmu_mmr_PMU_FUSE_2_reg::type_id::create("PMU_FUSE_2", this);
    PMU_FUSE_2.set_cfg(16'h0, 16'h0, 16'h0, 16'h2008, 32, 32'b00000000000000000000000000000000);
    PMU_FUSE_2.set_space_addr("MSG", 16'h2008);
    PMU_FUSE_2.set_space_addr("CR-SB",16'h2008); 
    PMU_FUSE_2.set_space("MSG");
    PMU_FUSE_2.set_msg_opcode("CR-SB");
    PMU_FUSE_2.set_security_policy("security_Policy_group", "PMU_SAI_POLICY0");
    if ( $test$plusargs("PMU_FUSE_2:dont_test") ) PMU_FUSE_2.set_test_reg(1'b0);
    if (!add_reg( PMU_FUSE_2 )) begin
      `sla_error(get_name(), ("could not add register PMU_FUSE_2"));
    end
   PMU_FUSE_2.FUSE_DW2.set_paths({"PMU_FUSE_2.FUSE_DW2"});

    `ifdef SLA_RAL_COVERAGE
    sla_ral_reg::regname = "PMU_FUSE_3";
    `endif
    PMU_FUSE_3 = pmu_mmr_PMU_FUSE_3_reg::type_id::create("PMU_FUSE_3", this);
    PMU_FUSE_3.set_cfg(16'h0, 16'h0, 16'h0, 16'h200C, 32, 32'b00000000000000000000000000000000);
    PMU_FUSE_3.set_space_addr("MSG", 16'h200C);
    PMU_FUSE_3.set_space_addr("CR-SB",16'h200C); 
    PMU_FUSE_3.set_space("MSG");
    PMU_FUSE_3.set_msg_opcode("CR-SB");
    PMU_FUSE_3.set_security_policy("security_Policy_group", "PMU_SAI_POLICY0");
    if ( $test$plusargs("PMU_FUSE_3:dont_test") ) PMU_FUSE_3.set_test_reg(1'b0);
    if (!add_reg( PMU_FUSE_3 )) begin
      `sla_error(get_name(), ("could not add register PMU_FUSE_3"));
    end
   PMU_FUSE_3.FUSE_DW3.set_paths({"PMU_FUSE_3.FUSE_DW3"});

    `ifdef SLA_RAL_COVERAGE
    sla_ral_reg::regname = "PMU_FUSE_4";
    `endif
    PMU_FUSE_4 = pmu_mmr_PMU_FUSE_4_reg::type_id::create("PMU_FUSE_4", this);
    PMU_FUSE_4.set_cfg(16'h0, 16'h0, 16'h0, 16'h2010, 32, 32'b00000000000000000000000000000000);
    PMU_FUSE_4.set_space_addr("MSG", 16'h2010);
    PMU_FUSE_4.set_space_addr("CR-SB",16'h2010); 
    PMU_FUSE_4.set_space("MSG");
    PMU_FUSE_4.set_msg_opcode("CR-SB");
    PMU_FUSE_4.set_security_policy("security_Policy_group", "PMU_SAI_POLICY0");
    if ( $test$plusargs("PMU_FUSE_4:dont_test") ) PMU_FUSE_4.set_test_reg(1'b0);
    if (!add_reg( PMU_FUSE_4 )) begin
      `sla_error(get_name(), ("could not add register PMU_FUSE_4"));
    end
   PMU_FUSE_4.FUSE_DW4.set_paths({"PMU_FUSE_4.FUSE_DW4"});

    `ifdef SLA_RAL_COVERAGE
    sla_ral_reg::regname = "PMU_FUSE_5";
    `endif
    PMU_FUSE_5 = pmu_mmr_PMU_FUSE_5_reg::type_id::create("PMU_FUSE_5", this);
    PMU_FUSE_5.set_cfg(16'h0, 16'h0, 16'h0, 16'h2014, 32, 32'b00000000000000000000000000000000);
    PMU_FUSE_5.set_space_addr("MSG", 16'h2014);
    PMU_FUSE_5.set_space_addr("CR-SB",16'h2014); 
    PMU_FUSE_5.set_space("MSG");
    PMU_FUSE_5.set_msg_opcode("CR-SB");
    PMU_FUSE_5.set_security_policy("security_Policy_group", "PMU_SAI_POLICY0");
    if ( $test$plusargs("PMU_FUSE_5:dont_test") ) PMU_FUSE_5.set_test_reg(1'b0);
    if (!add_reg( PMU_FUSE_5 )) begin
      `sla_error(get_name(), ("could not add register PMU_FUSE_5"));
    end
   PMU_FUSE_5.FUSE_DW5.set_paths({"PMU_FUSE_5.FUSE_DW5"});

    `ifdef SLA_RAL_COVERAGE
    sla_ral_reg::regname = "PMU_FUSE_6";
    `endif
    PMU_FUSE_6 = pmu_mmr_PMU_FUSE_6_reg::type_id::create("PMU_FUSE_6", this);
    PMU_FUSE_6.set_cfg(16'h0, 16'h0, 16'h0, 16'h2018, 32, 32'b00000000000000000000000000000000);
    PMU_FUSE_6.set_space_addr("MSG", 16'h2018);
    PMU_FUSE_6.set_space_addr("CR-SB",16'h2018); 
    PMU_FUSE_6.set_space("MSG");
    PMU_FUSE_6.set_msg_opcode("CR-SB");
    PMU_FUSE_6.set_security_policy("security_Policy_group", "PMU_SAI_POLICY0");
    if ( $test$plusargs("PMU_FUSE_6:dont_test") ) PMU_FUSE_6.set_test_reg(1'b0);
    if (!add_reg( PMU_FUSE_6 )) begin
      `sla_error(get_name(), ("could not add register PMU_FUSE_6"));
    end
   PMU_FUSE_6.FUSE_DW6.set_paths({"PMU_FUSE_6.FUSE_DW6"});

    `ifdef SLA_RAL_COVERAGE
    sla_ral_reg::regname = "PMU_FUSE_7";
    `endif
    PMU_FUSE_7 = pmu_mmr_PMU_FUSE_7_reg::type_id::create("PMU_FUSE_7", this);
    PMU_FUSE_7.set_cfg(16'h0, 16'h0, 16'h0, 16'h201C, 32, 32'b00000000000000000000000000000000);
    PMU_FUSE_7.set_space_addr("MSG", 16'h201C);
    PMU_FUSE_7.set_space_addr("CR-SB",16'h201C); 
    PMU_FUSE_7.set_space("MSG");
    PMU_FUSE_7.set_msg_opcode("CR-SB");
    PMU_FUSE_7.set_security_policy("security_Policy_group", "PMU_SAI_POLICY0");
    if ( $test$plusargs("PMU_FUSE_7:dont_test") ) PMU_FUSE_7.set_test_reg(1'b0);
    if (!add_reg( PMU_FUSE_7 )) begin
      `sla_error(get_name(), ("could not add register PMU_FUSE_7"));
    end
   PMU_FUSE_7.FUSE_DW7.set_paths({"PMU_FUSE_7.FUSE_DW7"});

    `ifdef SLA_RAL_COVERAGE
    sla_ral_reg::regname = "PMU_FUSE_8";
    `endif
    PMU_FUSE_8 = pmu_mmr_PMU_FUSE_8_reg::type_id::create("PMU_FUSE_8", this);
    PMU_FUSE_8.set_cfg(16'h0, 16'h0, 16'h0, 16'h2020, 32, 32'b00000000000000000000000000000000);
    PMU_FUSE_8.set_space_addr("MSG", 16'h2020);
    PMU_FUSE_8.set_space_addr("CR-SB",16'h2020); 
    PMU_FUSE_8.set_space("MSG");
    PMU_FUSE_8.set_msg_opcode("CR-SB");
    PMU_FUSE_8.set_security_policy("security_Policy_group", "PMU_SAI_POLICY0");
    if ( $test$plusargs("PMU_FUSE_8:dont_test") ) PMU_FUSE_8.set_test_reg(1'b0);
    if (!add_reg( PMU_FUSE_8 )) begin
      `sla_error(get_name(), ("could not add register PMU_FUSE_8"));
    end
   PMU_FUSE_8.FUSE_DW8.set_paths({"PMU_FUSE_8.FUSE_DW8"});

    `ifdef SLA_RAL_COVERAGE
    sla_ral_reg::regname = "PMU_FUSE_9";
    `endif
    PMU_FUSE_9 = pmu_mmr_PMU_FUSE_9_reg::type_id::create("PMU_FUSE_9", this);
    PMU_FUSE_9.set_cfg(16'h0, 16'h0, 16'h0, 16'h2024, 32, 32'b00000000000000000000000000000000);
    PMU_FUSE_9.set_space_addr("MSG", 16'h2024);
    PMU_FUSE_9.set_space_addr("CR-SB",16'h2024); 
    PMU_FUSE_9.set_space("MSG");
    PMU_FUSE_9.set_msg_opcode("CR-SB");
    PMU_FUSE_9.set_security_policy("security_Policy_group", "PMU_SAI_POLICY0");
    if ( $test$plusargs("PMU_FUSE_9:dont_test") ) PMU_FUSE_9.set_test_reg(1'b0);
    if (!add_reg( PMU_FUSE_9 )) begin
      `sla_error(get_name(), ("could not add register PMU_FUSE_9"));
    end
   PMU_FUSE_9.FUSE_DW9.set_paths({"PMU_FUSE_9.FUSE_DW9"});

    `ifdef SLA_RAL_COVERAGE
    sla_ral_reg::regname = "PMU_FUSE_10";
    `endif
    PMU_FUSE_10 = pmu_mmr_PMU_FUSE_10_reg::type_id::create("PMU_FUSE_10", this);
    PMU_FUSE_10.set_cfg(16'h0, 16'h0, 16'h0, 16'h2028, 32, 32'b00000000000000000000000000000000);
    PMU_FUSE_10.set_space_addr("MSG", 16'h2028);
    PMU_FUSE_10.set_space_addr("CR-SB",16'h2028); 
    PMU_FUSE_10.set_space("MSG");
    PMU_FUSE_10.set_msg_opcode("CR-SB");
    PMU_FUSE_10.set_security_policy("security_Policy_group", "PMU_SAI_POLICY0");
    if ( $test$plusargs("PMU_FUSE_10:dont_test") ) PMU_FUSE_10.set_test_reg(1'b0);
    if (!add_reg( PMU_FUSE_10 )) begin
      `sla_error(get_name(), ("could not add register PMU_FUSE_10"));
    end
   PMU_FUSE_10.FUSE_DW10.set_paths({"PMU_FUSE_10.FUSE_DW10"});

    `ifdef SLA_RAL_COVERAGE
    sla_ral_reg::regname = "PMU_FUSE_11";
    `endif
    PMU_FUSE_11 = pmu_mmr_PMU_FUSE_11_reg::type_id::create("PMU_FUSE_11", this);
    PMU_FUSE_11.set_cfg(16'h0, 16'h0, 16'h0, 16'h202C, 32, 32'b00000000000000000000000000000000);
    PMU_FUSE_11.set_space_addr("MSG", 16'h202C);
    PMU_FUSE_11.set_space_addr("CR-SB",16'h202C); 
    PMU_FUSE_11.set_space("MSG");
    PMU_FUSE_11.set_msg_opcode("CR-SB");
    PMU_FUSE_11.set_security_policy("security_Policy_group", "PMU_SAI_POLICY0");
    if ( $test$plusargs("PMU_FUSE_11:dont_test") ) PMU_FUSE_11.set_test_reg(1'b0);
    if (!add_reg( PMU_FUSE_11 )) begin
      `sla_error(get_name(), ("could not add register PMU_FUSE_11"));
    end
   PMU_FUSE_11.FUSE_DW11.set_paths({"PMU_FUSE_11.FUSE_DW11"});

    `ifdef SLA_RAL_COVERAGE
    sla_ral_reg::regname = "PMU_FUSE_12";
    `endif
    PMU_FUSE_12 = pmu_mmr_PMU_FUSE_12_reg::type_id::create("PMU_FUSE_12", this);
    PMU_FUSE_12.set_cfg(16'h0, 16'h0, 16'h0, 16'h2030, 32, 32'b00000000000000000000000000000000);
    PMU_FUSE_12.set_space_addr("MSG", 16'h2030);
    PMU_FUSE_12.set_space_addr("CR-SB",16'h2030); 
    PMU_FUSE_12.set_space("MSG");
    PMU_FUSE_12.set_msg_opcode("CR-SB");
    PMU_FUSE_12.set_security_policy("security_Policy_group", "PMU_SAI_POLICY0");
    if ( $test$plusargs("PMU_FUSE_12:dont_test") ) PMU_FUSE_12.set_test_reg(1'b0);
    if (!add_reg( PMU_FUSE_12 )) begin
      `sla_error(get_name(), ("could not add register PMU_FUSE_12"));
    end
   PMU_FUSE_12.FUSE_DW12.set_paths({"PMU_FUSE_12.FUSE_DW12"});

    `ifdef SLA_RAL_COVERAGE
    sla_ral_reg::regname = "PMU_FUSE_13";
    `endif
    PMU_FUSE_13 = pmu_mmr_PMU_FUSE_13_reg::type_id::create("PMU_FUSE_13", this);
    PMU_FUSE_13.set_cfg(16'h0, 16'h0, 16'h0, 16'h2034, 32, 32'b00000000000000000000000000000000);
    PMU_FUSE_13.set_space_addr("MSG", 16'h2034);
    PMU_FUSE_13.set_space_addr("CR-SB",16'h2034); 
    PMU_FUSE_13.set_space("MSG");
    PMU_FUSE_13.set_msg_opcode("CR-SB");
    PMU_FUSE_13.set_security_policy("security_Policy_group", "PMU_SAI_POLICY0");
    if ( $test$plusargs("PMU_FUSE_13:dont_test") ) PMU_FUSE_13.set_test_reg(1'b0);
    if (!add_reg( PMU_FUSE_13 )) begin
      `sla_error(get_name(), ("could not add register PMU_FUSE_13"));
    end
   PMU_FUSE_13.FUSE_DW13.set_paths({"PMU_FUSE_13.FUSE_DW13"});

    `ifdef SLA_RAL_COVERAGE
    sla_ral_reg::regname = "PMU_FUSE_14";
    `endif
    PMU_FUSE_14 = pmu_mmr_PMU_FUSE_14_reg::type_id::create("PMU_FUSE_14", this);
    PMU_FUSE_14.set_cfg(16'h0, 16'h0, 16'h0, 16'h2038, 32, 32'b00000000000000000000000000000000);
    PMU_FUSE_14.set_space_addr("MSG", 16'h2038);
    PMU_FUSE_14.set_space_addr("CR-SB",16'h2038); 
    PMU_FUSE_14.set_space("MSG");
    PMU_FUSE_14.set_msg_opcode("CR-SB");
    PMU_FUSE_14.set_security_policy("security_Policy_group", "PMU_SAI_POLICY0");
    if ( $test$plusargs("PMU_FUSE_14:dont_test") ) PMU_FUSE_14.set_test_reg(1'b0);
    if (!add_reg( PMU_FUSE_14 )) begin
      `sla_error(get_name(), ("could not add register PMU_FUSE_14"));
    end
   PMU_FUSE_14.FUSE_DW14.set_paths({"PMU_FUSE_14.FUSE_DW14"});

    `ifdef SLA_RAL_COVERAGE
    sla_ral_reg::regname = "PMU_FUSE_15";
    `endif
    PMU_FUSE_15 = pmu_mmr_PMU_FUSE_15_reg::type_id::create("PMU_FUSE_15", this);
    PMU_FUSE_15.set_cfg(16'h0, 16'h0, 16'h0, 16'h203C, 32, 32'b00000000000000000000000000000000);
    PMU_FUSE_15.set_space_addr("MSG", 16'h203C);
    PMU_FUSE_15.set_space_addr("CR-SB",16'h203C); 
    PMU_FUSE_15.set_space("MSG");
    PMU_FUSE_15.set_msg_opcode("CR-SB");
    PMU_FUSE_15.set_security_policy("security_Policy_group", "PMU_SAI_POLICY0");
    if ( $test$plusargs("PMU_FUSE_15:dont_test") ) PMU_FUSE_15.set_test_reg(1'b0);
    if (!add_reg( PMU_FUSE_15 )) begin
      `sla_error(get_name(), ("could not add register PMU_FUSE_15"));
    end
   PMU_FUSE_15.FUSE_DW15.set_paths({"PMU_FUSE_15.FUSE_DW15"});

    `ifdef SLA_RAL_COVERAGE
    sla_ral_reg::regname = "PMU_STRAP_0";
    `endif
    PMU_STRAP_0 = pmu_mmr_PMU_STRAP_0_reg::type_id::create("PMU_STRAP_0", this);
    PMU_STRAP_0.set_cfg(16'h0, 16'h0, 16'h0, 16'h3000, 32, 32'b00000000000000000000000000000000);
    PMU_STRAP_0.set_space_addr("MSG", 16'h3000);
    PMU_STRAP_0.set_space_addr("CR-SB",16'h3000); 
    PMU_STRAP_0.set_space("MSG");
    PMU_STRAP_0.set_msg_opcode("CR-SB");
    PMU_STRAP_0.set_security_policy("security_Policy_group", "PMU_SAI_POLICY0");
    if ( $test$plusargs("PMU_STRAP_0:dont_test") ) PMU_STRAP_0.set_test_reg(1'b0);
    if (!add_reg( PMU_STRAP_0 )) begin
      `sla_error(get_name(), ("could not add register PMU_STRAP_0"));
    end
   PMU_STRAP_0.SOFT_STRAP_DW0.set_paths({"PMU_STRAP_0.SOFT_STRAP_DW0"});

    `ifdef SLA_RAL_COVERAGE
    sla_ral_reg::regname = "PMU_STRAP_1";
    `endif
    PMU_STRAP_1 = pmu_mmr_PMU_STRAP_1_reg::type_id::create("PMU_STRAP_1", this);
    PMU_STRAP_1.set_cfg(16'h0, 16'h0, 16'h0, 16'h3004, 32, 32'b00000000000000000000000000000000);
    PMU_STRAP_1.set_space_addr("MSG", 16'h3004);
    PMU_STRAP_1.set_space_addr("CR-SB",16'h3004); 
    PMU_STRAP_1.set_space("MSG");
    PMU_STRAP_1.set_msg_opcode("CR-SB");
    PMU_STRAP_1.set_security_policy("security_Policy_group", "PMU_SAI_POLICY0");
    if ( $test$plusargs("PMU_STRAP_1:dont_test") ) PMU_STRAP_1.set_test_reg(1'b0);
    if (!add_reg( PMU_STRAP_1 )) begin
      `sla_error(get_name(), ("could not add register PMU_STRAP_1"));
    end
   PMU_STRAP_1.SOFT_STRAP_DW1.set_paths({"PMU_STRAP_1.SOFT_STRAP_DW1"});

    `ifdef SLA_RAL_COVERAGE
    sla_ral_reg::regname = "PMU_STRAP_2";
    `endif
    PMU_STRAP_2 = pmu_mmr_PMU_STRAP_2_reg::type_id::create("PMU_STRAP_2", this);
    PMU_STRAP_2.set_cfg(16'h0, 16'h0, 16'h0, 16'h3008, 32, 32'b00000000000000000000000000000000);
    PMU_STRAP_2.set_space_addr("MSG", 16'h3008);
    PMU_STRAP_2.set_space_addr("CR-SB",16'h3008); 
    PMU_STRAP_2.set_space("MSG");
    PMU_STRAP_2.set_msg_opcode("CR-SB");
    PMU_STRAP_2.set_security_policy("security_Policy_group", "PMU_SAI_POLICY0");
    if ( $test$plusargs("PMU_STRAP_2:dont_test") ) PMU_STRAP_2.set_test_reg(1'b0);
    if (!add_reg( PMU_STRAP_2 )) begin
      `sla_error(get_name(), ("could not add register PMU_STRAP_2"));
    end
   PMU_STRAP_2.SOFT_STRAP_DW2.set_paths({"PMU_STRAP_2.SOFT_STRAP_DW2"});

    `ifdef SLA_RAL_COVERAGE
    sla_ral_reg::regname = "PMU_STRAP_3";
    `endif
    PMU_STRAP_3 = pmu_mmr_PMU_STRAP_3_reg::type_id::create("PMU_STRAP_3", this);
    PMU_STRAP_3.set_cfg(16'h0, 16'h0, 16'h0, 16'h300C, 32, 32'b00000000000000000000000000000000);
    PMU_STRAP_3.set_space_addr("MSG", 16'h300C);
    PMU_STRAP_3.set_space_addr("CR-SB",16'h300C); 
    PMU_STRAP_3.set_space("MSG");
    PMU_STRAP_3.set_msg_opcode("CR-SB");
    PMU_STRAP_3.set_security_policy("security_Policy_group", "PMU_SAI_POLICY0");
    if ( $test$plusargs("PMU_STRAP_3:dont_test") ) PMU_STRAP_3.set_test_reg(1'b0);
    if (!add_reg( PMU_STRAP_3 )) begin
      `sla_error(get_name(), ("could not add register PMU_STRAP_3"));
    end
   PMU_STRAP_3.SOFT_STRAP_DW3.set_paths({"PMU_STRAP_3.SOFT_STRAP_DW3"});

    `ifdef SLA_RAL_COVERAGE
    sla_ral_reg::regname = "PMU_SAI_READ_POLICY0_HI";
    `endif
    PMU_SAI_READ_POLICY0_HI = pmu_mmr_PMU_SAI_READ_POLICY0_HI_reg::type_id::create("PMU_SAI_READ_POLICY0_HI", this);
    PMU_SAI_READ_POLICY0_HI.set_powerwell("primary");
    PMU_SAI_READ_POLICY0_HI.set_cfg(16'h0, 16'h0, 16'h0, 20'h0F000, 32, 32'b00000000000000000000000000000000);
    PMU_SAI_READ_POLICY0_HI.set_space_addr("MSG", 20'h0F000);
    PMU_SAI_READ_POLICY0_HI.set_space_addr("CR-SB",28'h000F000); 
    PMU_SAI_READ_POLICY0_HI.set_space("MSG");
    PMU_SAI_READ_POLICY0_HI.set_msg_opcode("CR-SB");
    PMU_SAI_READ_POLICY0_HI.set_security_policy("security_Policy_group", "PMU_SAI_POLICY0");
    if ( $test$plusargs("PMU_SAI_READ_POLICY0_HI:dont_test") ) PMU_SAI_READ_POLICY0_HI.set_test_reg(1'b0);
    if (!add_reg( PMU_SAI_READ_POLICY0_HI )) begin
      `sla_error(get_name(), ("could not add register PMU_SAI_READ_POLICY0_HI"));
    end
   PMU_SAI_READ_POLICY0_HI.read_policy.set_paths({"PMU_SAI_READ_POLICY0_HI.read_policy"});

    `ifdef SLA_RAL_COVERAGE
    sla_ral_reg::regname = "PMU_SAI_READ_POLICY0_LO";
    `endif
    PMU_SAI_READ_POLICY0_LO = pmu_mmr_PMU_SAI_READ_POLICY0_LO_reg::type_id::create("PMU_SAI_READ_POLICY0_LO", this);
    PMU_SAI_READ_POLICY0_LO.set_powerwell("primary");
    PMU_SAI_READ_POLICY0_LO.set_cfg(16'h0, 16'h0, 16'h0, 20'h0F004, 32, 32'b00000000000000000000000000000000);
    PMU_SAI_READ_POLICY0_LO.set_space_addr("MSG", 20'h0F004);
    PMU_SAI_READ_POLICY0_LO.set_space_addr("CR-SB",28'h000F004); 
    PMU_SAI_READ_POLICY0_LO.set_space("MSG");
    PMU_SAI_READ_POLICY0_LO.set_msg_opcode("CR-SB");
    PMU_SAI_READ_POLICY0_LO.set_security_policy("security_Policy_group", "PMU_SAI_POLICY0");
    if ( $test$plusargs("PMU_SAI_READ_POLICY0_LO:dont_test") ) PMU_SAI_READ_POLICY0_LO.set_test_reg(1'b0);
    if (!add_reg( PMU_SAI_READ_POLICY0_LO )) begin
      `sla_error(get_name(), ("could not add register PMU_SAI_READ_POLICY0_LO"));
    end
   PMU_SAI_READ_POLICY0_LO.read_policy.set_paths({"PMU_SAI_READ_POLICY0_LO.read_policy"});

    `ifdef SLA_RAL_COVERAGE
    sla_ral_reg::regname = "PMU_SAI_READ_POLICY1_HI";
    `endif
    PMU_SAI_READ_POLICY1_HI = pmu_mmr_PMU_SAI_READ_POLICY1_HI_reg::type_id::create("PMU_SAI_READ_POLICY1_HI", this);
    PMU_SAI_READ_POLICY1_HI.set_powerwell("primary");
    PMU_SAI_READ_POLICY1_HI.set_cfg(16'h0, 16'h0, 16'h0, 20'h0F008, 32, 32'b00000000000000000000000000000000);
    PMU_SAI_READ_POLICY1_HI.set_space_addr("MSG", 20'h0F008);
    PMU_SAI_READ_POLICY1_HI.set_space_addr("CR-SB",28'h000F008); 
    PMU_SAI_READ_POLICY1_HI.set_space("MSG");
    PMU_SAI_READ_POLICY1_HI.set_msg_opcode("CR-SB");
    PMU_SAI_READ_POLICY1_HI.set_security_policy("security_Policy_group", "PMU_SAI_POLICY1");
    if ( $test$plusargs("PMU_SAI_READ_POLICY1_HI:dont_test") ) PMU_SAI_READ_POLICY1_HI.set_test_reg(1'b0);
    if (!add_reg( PMU_SAI_READ_POLICY1_HI )) begin
      `sla_error(get_name(), ("could not add register PMU_SAI_READ_POLICY1_HI"));
    end
   PMU_SAI_READ_POLICY1_HI.read_policy.set_paths({"PMU_SAI_READ_POLICY1_HI.read_policy"});

    `ifdef SLA_RAL_COVERAGE
    sla_ral_reg::regname = "PMU_SAI_READ_POLICY1_LO";
    `endif
    PMU_SAI_READ_POLICY1_LO = pmu_mmr_PMU_SAI_READ_POLICY1_LO_reg::type_id::create("PMU_SAI_READ_POLICY1_LO", this);
    PMU_SAI_READ_POLICY1_LO.set_powerwell("primary");
    PMU_SAI_READ_POLICY1_LO.set_cfg(16'h0, 16'h0, 16'h0, 20'h0F00C, 32, 32'b00000000000000000000000000000000);
    PMU_SAI_READ_POLICY1_LO.set_space_addr("MSG", 20'h0F00C);
    PMU_SAI_READ_POLICY1_LO.set_space_addr("CR-SB",28'h000F00C); 
    PMU_SAI_READ_POLICY1_LO.set_space("MSG");
    PMU_SAI_READ_POLICY1_LO.set_msg_opcode("CR-SB");
    PMU_SAI_READ_POLICY1_LO.set_security_policy("security_Policy_group", "PMU_SAI_POLICY1");
    if ( $test$plusargs("PMU_SAI_READ_POLICY1_LO:dont_test") ) PMU_SAI_READ_POLICY1_LO.set_test_reg(1'b0);
    if (!add_reg( PMU_SAI_READ_POLICY1_LO )) begin
      `sla_error(get_name(), ("could not add register PMU_SAI_READ_POLICY1_LO"));
    end
   PMU_SAI_READ_POLICY1_LO.read_policy.set_paths({"PMU_SAI_READ_POLICY1_LO.read_policy"});

    `ifdef SLA_RAL_COVERAGE
    sla_ral_reg::regname = "PMU_SAI_READ_POLICY2_HI";
    `endif
    PMU_SAI_READ_POLICY2_HI = pmu_mmr_PMU_SAI_READ_POLICY2_HI_reg::type_id::create("PMU_SAI_READ_POLICY2_HI", this);
    PMU_SAI_READ_POLICY2_HI.set_powerwell("primary");
    PMU_SAI_READ_POLICY2_HI.set_cfg(16'h0, 16'h0, 16'h0, 20'h0F010, 32, 32'b00000000000000000000000000000000);
    PMU_SAI_READ_POLICY2_HI.set_space_addr("MSG", 20'h0F010);
    PMU_SAI_READ_POLICY2_HI.set_space_addr("CR-SB",28'h000F010); 
    PMU_SAI_READ_POLICY2_HI.set_space("MSG");
    PMU_SAI_READ_POLICY2_HI.set_msg_opcode("CR-SB");
    PMU_SAI_READ_POLICY2_HI.set_security_policy("security_Policy_group", "PMU_SAI_POLICY2");
    if ( $test$plusargs("PMU_SAI_READ_POLICY2_HI:dont_test") ) PMU_SAI_READ_POLICY2_HI.set_test_reg(1'b0);
    if (!add_reg( PMU_SAI_READ_POLICY2_HI )) begin
      `sla_error(get_name(), ("could not add register PMU_SAI_READ_POLICY2_HI"));
    end
   PMU_SAI_READ_POLICY2_HI.read_policy.set_paths({"PMU_SAI_READ_POLICY2_HI.read_policy"});

    `ifdef SLA_RAL_COVERAGE
    sla_ral_reg::regname = "PMU_SAI_READ_POLICY2_LO";
    `endif
    PMU_SAI_READ_POLICY2_LO = pmu_mmr_PMU_SAI_READ_POLICY2_LO_reg::type_id::create("PMU_SAI_READ_POLICY2_LO", this);
    PMU_SAI_READ_POLICY2_LO.set_powerwell("primary");
    PMU_SAI_READ_POLICY2_LO.set_cfg(16'h0, 16'h0, 16'h0, 20'h0F014, 32, 32'b00000000000000000000000000000000);
    PMU_SAI_READ_POLICY2_LO.set_space_addr("MSG", 20'h0F014);
    PMU_SAI_READ_POLICY2_LO.set_space_addr("CR-SB",28'h000F014); 
    PMU_SAI_READ_POLICY2_LO.set_space("MSG");
    PMU_SAI_READ_POLICY2_LO.set_msg_opcode("CR-SB");
    PMU_SAI_READ_POLICY2_LO.set_security_policy("security_Policy_group", "PMU_SAI_POLICY2");
    if ( $test$plusargs("PMU_SAI_READ_POLICY2_LO:dont_test") ) PMU_SAI_READ_POLICY2_LO.set_test_reg(1'b0);
    if (!add_reg( PMU_SAI_READ_POLICY2_LO )) begin
      `sla_error(get_name(), ("could not add register PMU_SAI_READ_POLICY2_LO"));
    end
   PMU_SAI_READ_POLICY2_LO.read_policy.set_paths({"PMU_SAI_READ_POLICY2_LO.read_policy"});

    `ifdef SLA_RAL_COVERAGE
    sla_ral_reg::regname = "PMU_SAI_WRITE_POLICY0_HI";
    `endif
    PMU_SAI_WRITE_POLICY0_HI = pmu_mmr_PMU_SAI_WRITE_POLICY0_HI_reg::type_id::create("PMU_SAI_WRITE_POLICY0_HI", this);
    PMU_SAI_WRITE_POLICY0_HI.set_powerwell("primary");
    PMU_SAI_WRITE_POLICY0_HI.set_cfg(16'h0, 16'h0, 16'h0, 20'h0F018, 32, 32'b00000000000000000000000000000000);
    PMU_SAI_WRITE_POLICY0_HI.set_space_addr("MSG", 20'h0F018);
    PMU_SAI_WRITE_POLICY0_HI.set_space_addr("CR-SB",28'h000F018); 
    PMU_SAI_WRITE_POLICY0_HI.set_space("MSG");
    PMU_SAI_WRITE_POLICY0_HI.set_msg_opcode("CR-SB");
    PMU_SAI_WRITE_POLICY0_HI.set_security_policy("security_Policy_group", "PMU_SAI_POLICY0");
    if ( $test$plusargs("PMU_SAI_WRITE_POLICY0_HI:dont_test") ) PMU_SAI_WRITE_POLICY0_HI.set_test_reg(1'b0);
    if (!add_reg( PMU_SAI_WRITE_POLICY0_HI )) begin
      `sla_error(get_name(), ("could not add register PMU_SAI_WRITE_POLICY0_HI"));
    end
   PMU_SAI_WRITE_POLICY0_HI.write_policy.set_paths({"PMU_SAI_WRITE_POLICY0_HI.write_policy"});

    `ifdef SLA_RAL_COVERAGE
    sla_ral_reg::regname = "PMU_SAI_WRITE_POLICY0_LO";
    `endif
    PMU_SAI_WRITE_POLICY0_LO = pmu_mmr_PMU_SAI_WRITE_POLICY0_LO_reg::type_id::create("PMU_SAI_WRITE_POLICY0_LO", this);
    PMU_SAI_WRITE_POLICY0_LO.set_powerwell("primary");
    PMU_SAI_WRITE_POLICY0_LO.set_cfg(16'h0, 16'h0, 16'h0, 20'h0F01C, 32, 32'b00000000000000000000000000000000);
    PMU_SAI_WRITE_POLICY0_LO.set_space_addr("MSG", 20'h0F01C);
    PMU_SAI_WRITE_POLICY0_LO.set_space_addr("CR-SB",28'h000F01C); 
    PMU_SAI_WRITE_POLICY0_LO.set_space("MSG");
    PMU_SAI_WRITE_POLICY0_LO.set_msg_opcode("CR-SB");
    PMU_SAI_WRITE_POLICY0_LO.set_security_policy("security_Policy_group", "PMU_SAI_POLICY0");
    if ( $test$plusargs("PMU_SAI_WRITE_POLICY0_LO:dont_test") ) PMU_SAI_WRITE_POLICY0_LO.set_test_reg(1'b0);
    if (!add_reg( PMU_SAI_WRITE_POLICY0_LO )) begin
      `sla_error(get_name(), ("could not add register PMU_SAI_WRITE_POLICY0_LO"));
    end
   PMU_SAI_WRITE_POLICY0_LO.write_policy.set_paths({"PMU_SAI_WRITE_POLICY0_LO.write_policy"});

    `ifdef SLA_RAL_COVERAGE
    sla_ral_reg::regname = "PMU_SAI_WRITE_POLICY1_HI";
    `endif
    PMU_SAI_WRITE_POLICY1_HI = pmu_mmr_PMU_SAI_WRITE_POLICY1_HI_reg::type_id::create("PMU_SAI_WRITE_POLICY1_HI", this);
    PMU_SAI_WRITE_POLICY1_HI.set_powerwell("primary");
    PMU_SAI_WRITE_POLICY1_HI.set_cfg(16'h0, 16'h0, 16'h0, 20'h0F020, 32, 32'b00000000000000000000000000000000);
    PMU_SAI_WRITE_POLICY1_HI.set_space_addr("MSG", 20'h0F020);
    PMU_SAI_WRITE_POLICY1_HI.set_space_addr("CR-SB",28'h000F020); 
    PMU_SAI_WRITE_POLICY1_HI.set_space("MSG");
    PMU_SAI_WRITE_POLICY1_HI.set_msg_opcode("CR-SB");
    PMU_SAI_WRITE_POLICY1_HI.set_security_policy("security_Policy_group", "PMU_SAI_POLICY1");
    if ( $test$plusargs("PMU_SAI_WRITE_POLICY1_HI:dont_test") ) PMU_SAI_WRITE_POLICY1_HI.set_test_reg(1'b0);
    if (!add_reg( PMU_SAI_WRITE_POLICY1_HI )) begin
      `sla_error(get_name(), ("could not add register PMU_SAI_WRITE_POLICY1_HI"));
    end
   PMU_SAI_WRITE_POLICY1_HI.write_policy.set_paths({"PMU_SAI_WRITE_POLICY1_HI.write_policy"});

    `ifdef SLA_RAL_COVERAGE
    sla_ral_reg::regname = "PMU_SAI_WRITE_POLICY1_LO";
    `endif
    PMU_SAI_WRITE_POLICY1_LO = pmu_mmr_PMU_SAI_WRITE_POLICY1_LO_reg::type_id::create("PMU_SAI_WRITE_POLICY1_LO", this);
    PMU_SAI_WRITE_POLICY1_LO.set_powerwell("primary");
    PMU_SAI_WRITE_POLICY1_LO.set_cfg(16'h0, 16'h0, 16'h0, 20'h0F024, 32, 32'b00000000000000000000000000000000);
    PMU_SAI_WRITE_POLICY1_LO.set_space_addr("MSG", 20'h0F024);
    PMU_SAI_WRITE_POLICY1_LO.set_space_addr("CR-SB",28'h000F024); 
    PMU_SAI_WRITE_POLICY1_LO.set_space("MSG");
    PMU_SAI_WRITE_POLICY1_LO.set_msg_opcode("CR-SB");
    PMU_SAI_WRITE_POLICY1_LO.set_security_policy("security_Policy_group", "PMU_SAI_POLICY1");
    if ( $test$plusargs("PMU_SAI_WRITE_POLICY1_LO:dont_test") ) PMU_SAI_WRITE_POLICY1_LO.set_test_reg(1'b0);
    if (!add_reg( PMU_SAI_WRITE_POLICY1_LO )) begin
      `sla_error(get_name(), ("could not add register PMU_SAI_WRITE_POLICY1_LO"));
    end
   PMU_SAI_WRITE_POLICY1_LO.write_policy.set_paths({"PMU_SAI_WRITE_POLICY1_LO.write_policy"});

    `ifdef SLA_RAL_COVERAGE
    sla_ral_reg::regname = "PMU_SAI_WRITE_POLICY2_HI";
    `endif
    PMU_SAI_WRITE_POLICY2_HI = pmu_mmr_PMU_SAI_WRITE_POLICY2_HI_reg::type_id::create("PMU_SAI_WRITE_POLICY2_HI", this);
    PMU_SAI_WRITE_POLICY2_HI.set_powerwell("primary");
    PMU_SAI_WRITE_POLICY2_HI.set_cfg(16'h0, 16'h0, 16'h0, 20'h0F028, 32, 32'b00000000000000000000000000000000);
    PMU_SAI_WRITE_POLICY2_HI.set_space_addr("MSG", 20'h0F028);
    PMU_SAI_WRITE_POLICY2_HI.set_space_addr("CR-SB",28'h000F028); 
    PMU_SAI_WRITE_POLICY2_HI.set_space("MSG");
    PMU_SAI_WRITE_POLICY2_HI.set_msg_opcode("CR-SB");
    PMU_SAI_WRITE_POLICY2_HI.set_security_policy("security_Policy_group", "PMU_SAI_POLICY2");
    if ( $test$plusargs("PMU_SAI_WRITE_POLICY2_HI:dont_test") ) PMU_SAI_WRITE_POLICY2_HI.set_test_reg(1'b0);
    if (!add_reg( PMU_SAI_WRITE_POLICY2_HI )) begin
      `sla_error(get_name(), ("could not add register PMU_SAI_WRITE_POLICY2_HI"));
    end
   PMU_SAI_WRITE_POLICY2_HI.write_policy.set_paths({"PMU_SAI_WRITE_POLICY2_HI.write_policy"});

    `ifdef SLA_RAL_COVERAGE
    sla_ral_reg::regname = "PMU_SAI_WRITE_POLICY2_LO";
    `endif
    PMU_SAI_WRITE_POLICY2_LO = pmu_mmr_PMU_SAI_WRITE_POLICY2_LO_reg::type_id::create("PMU_SAI_WRITE_POLICY2_LO", this);
    PMU_SAI_WRITE_POLICY2_LO.set_powerwell("primary");
    PMU_SAI_WRITE_POLICY2_LO.set_cfg(16'h0, 16'h0, 16'h0, 20'h0F02C, 32, 32'b00000000000000000000000000000000);
    PMU_SAI_WRITE_POLICY2_LO.set_space_addr("MSG", 20'h0F02C);
    PMU_SAI_WRITE_POLICY2_LO.set_space_addr("CR-SB",28'h000F02C); 
    PMU_SAI_WRITE_POLICY2_LO.set_space("MSG");
    PMU_SAI_WRITE_POLICY2_LO.set_msg_opcode("CR-SB");
    PMU_SAI_WRITE_POLICY2_LO.set_security_policy("security_Policy_group", "PMU_SAI_POLICY2");
    if ( $test$plusargs("PMU_SAI_WRITE_POLICY2_LO:dont_test") ) PMU_SAI_WRITE_POLICY2_LO.set_test_reg(1'b0);
    if (!add_reg( PMU_SAI_WRITE_POLICY2_LO )) begin
      `sla_error(get_name(), ("could not add register PMU_SAI_WRITE_POLICY2_LO"));
    end
   PMU_SAI_WRITE_POLICY2_LO.write_policy.set_paths({"PMU_SAI_WRITE_POLICY2_LO.write_policy"});

    `ifdef SLA_RAL_COVERAGE
    sla_ral_reg::regname = "PMU_SAI_CONTROL_POLICY0_HI";
    `endif
    PMU_SAI_CONTROL_POLICY0_HI = pmu_mmr_PMU_SAI_CONTROL_POLICY0_HI_reg::type_id::create("PMU_SAI_CONTROL_POLICY0_HI", this);
    PMU_SAI_CONTROL_POLICY0_HI.set_cfg(16'h0, 16'h0, 16'h0, 20'h0F030, 32, 32'b00000000000000000000000000000000);
    PMU_SAI_CONTROL_POLICY0_HI.set_space_addr("MSG", 20'h0F030);
    PMU_SAI_CONTROL_POLICY0_HI.set_space_addr("CR-SB",28'h000F030); 
    PMU_SAI_CONTROL_POLICY0_HI.set_space("MSG");
    PMU_SAI_CONTROL_POLICY0_HI.set_msg_opcode("CR-SB");
    PMU_SAI_CONTROL_POLICY0_HI.set_security_policy("security_Policy_group", "PMU_SAI_POLICY0");
    if ( $test$plusargs("PMU_SAI_CONTROL_POLICY0_HI:dont_test") ) PMU_SAI_CONTROL_POLICY0_HI.set_test_reg(1'b0);
    if (!add_reg( PMU_SAI_CONTROL_POLICY0_HI )) begin
      `sla_error(get_name(), ("could not add register PMU_SAI_CONTROL_POLICY0_HI"));
    end
   PMU_SAI_CONTROL_POLICY0_HI.cp_policy.set_paths({"PMU_SAI_CONTROL_POLICY0_HI.cp_policy"});

    `ifdef SLA_RAL_COVERAGE
    sla_ral_reg::regname = "PMU_SAI_CONTROL_POLICY0_LO";
    `endif
    PMU_SAI_CONTROL_POLICY0_LO = pmu_mmr_PMU_SAI_CONTROL_POLICY0_LO_reg::type_id::create("PMU_SAI_CONTROL_POLICY0_LO", this);
    PMU_SAI_CONTROL_POLICY0_LO.set_cfg(16'h0, 16'h0, 16'h0, 20'h0F034, 32, 32'b00000000000000000000000000000000);
    PMU_SAI_CONTROL_POLICY0_LO.set_space_addr("MSG", 20'h0F034);
    PMU_SAI_CONTROL_POLICY0_LO.set_space_addr("CR-SB",28'h000F034); 
    PMU_SAI_CONTROL_POLICY0_LO.set_space("MSG");
    PMU_SAI_CONTROL_POLICY0_LO.set_msg_opcode("CR-SB");
    PMU_SAI_CONTROL_POLICY0_LO.set_security_policy("security_Policy_group", "PMU_SAI_POLICY0");
    if ( $test$plusargs("PMU_SAI_CONTROL_POLICY0_LO:dont_test") ) PMU_SAI_CONTROL_POLICY0_LO.set_test_reg(1'b0);
    if (!add_reg( PMU_SAI_CONTROL_POLICY0_LO )) begin
      `sla_error(get_name(), ("could not add register PMU_SAI_CONTROL_POLICY0_LO"));
    end
   PMU_SAI_CONTROL_POLICY0_LO.cp_policy.set_paths({"PMU_SAI_CONTROL_POLICY0_LO.cp_policy"});

    `ifdef SLA_RAL_COVERAGE
    sla_ral_reg::regname = "PMU_SAI_CONTROL_POLICY1_HI";
    `endif
    PMU_SAI_CONTROL_POLICY1_HI = pmu_mmr_PMU_SAI_CONTROL_POLICY1_HI_reg::type_id::create("PMU_SAI_CONTROL_POLICY1_HI", this);
    PMU_SAI_CONTROL_POLICY1_HI.set_cfg(16'h0, 16'h0, 16'h0, 20'h0F038, 32, 32'b00000000000000000000000000000000);
    PMU_SAI_CONTROL_POLICY1_HI.set_space_addr("MSG", 20'h0F038);
    PMU_SAI_CONTROL_POLICY1_HI.set_space_addr("CR-SB",28'h000F038); 
    PMU_SAI_CONTROL_POLICY1_HI.set_space("MSG");
    PMU_SAI_CONTROL_POLICY1_HI.set_msg_opcode("CR-SB");
    PMU_SAI_CONTROL_POLICY1_HI.set_security_policy("security_Policy_group", "PMU_SAI_POLICY1");
    if ( $test$plusargs("PMU_SAI_CONTROL_POLICY1_HI:dont_test") ) PMU_SAI_CONTROL_POLICY1_HI.set_test_reg(1'b0);
    if (!add_reg( PMU_SAI_CONTROL_POLICY1_HI )) begin
      `sla_error(get_name(), ("could not add register PMU_SAI_CONTROL_POLICY1_HI"));
    end
   PMU_SAI_CONTROL_POLICY1_HI.cp_policy.set_paths({"PMU_SAI_CONTROL_POLICY1_HI.cp_policy"});

    `ifdef SLA_RAL_COVERAGE
    sla_ral_reg::regname = "PMU_SAI_CONTROL_POLICY1_LO";
    `endif
    PMU_SAI_CONTROL_POLICY1_LO = pmu_mmr_PMU_SAI_CONTROL_POLICY1_LO_reg::type_id::create("PMU_SAI_CONTROL_POLICY1_LO", this);
    PMU_SAI_CONTROL_POLICY1_LO.set_cfg(16'h0, 16'h0, 16'h0, 20'h0F03C, 32, 32'b00000000000000000000000000000000);
    PMU_SAI_CONTROL_POLICY1_LO.set_space_addr("MSG", 20'h0F03C);
    PMU_SAI_CONTROL_POLICY1_LO.set_space_addr("CR-SB",28'h000F03C); 
    PMU_SAI_CONTROL_POLICY1_LO.set_space("MSG");
    PMU_SAI_CONTROL_POLICY1_LO.set_msg_opcode("CR-SB");
    PMU_SAI_CONTROL_POLICY1_LO.set_security_policy("security_Policy_group", "PMU_SAI_POLICY1");
    if ( $test$plusargs("PMU_SAI_CONTROL_POLICY1_LO:dont_test") ) PMU_SAI_CONTROL_POLICY1_LO.set_test_reg(1'b0);
    if (!add_reg( PMU_SAI_CONTROL_POLICY1_LO )) begin
      `sla_error(get_name(), ("could not add register PMU_SAI_CONTROL_POLICY1_LO"));
    end
   PMU_SAI_CONTROL_POLICY1_LO.cp_policy.set_paths({"PMU_SAI_CONTROL_POLICY1_LO.cp_policy"});

    `ifdef SLA_RAL_COVERAGE
    sla_ral_reg::regname = "PMU_SAI_CONTROL_POLICY2_HI";
    `endif
    PMU_SAI_CONTROL_POLICY2_HI = pmu_mmr_PMU_SAI_CONTROL_POLICY2_HI_reg::type_id::create("PMU_SAI_CONTROL_POLICY2_HI", this);
    PMU_SAI_CONTROL_POLICY2_HI.set_cfg(16'h0, 16'h0, 16'h0, 20'h0F040, 32, 32'b00000000000000000000000000000000);
    PMU_SAI_CONTROL_POLICY2_HI.set_space_addr("MSG", 20'h0F040);
    PMU_SAI_CONTROL_POLICY2_HI.set_space_addr("CR-SB",28'h000F040); 
    PMU_SAI_CONTROL_POLICY2_HI.set_space("MSG");
    PMU_SAI_CONTROL_POLICY2_HI.set_msg_opcode("CR-SB");
    PMU_SAI_CONTROL_POLICY2_HI.set_security_policy("security_Policy_group", "PMU_SAI_POLICY2");
    if ( $test$plusargs("PMU_SAI_CONTROL_POLICY2_HI:dont_test") ) PMU_SAI_CONTROL_POLICY2_HI.set_test_reg(1'b0);
    if (!add_reg( PMU_SAI_CONTROL_POLICY2_HI )) begin
      `sla_error(get_name(), ("could not add register PMU_SAI_CONTROL_POLICY2_HI"));
    end
   PMU_SAI_CONTROL_POLICY2_HI.cp_policy.set_paths({"PMU_SAI_CONTROL_POLICY2_HI.cp_policy"});

    `ifdef SLA_RAL_COVERAGE
    sla_ral_reg::regname = "PMU_SAI_CONTROL_POLICY2_LO";
    `endif
    PMU_SAI_CONTROL_POLICY2_LO = pmu_mmr_PMU_SAI_CONTROL_POLICY2_LO_reg::type_id::create("PMU_SAI_CONTROL_POLICY2_LO", this);
    PMU_SAI_CONTROL_POLICY2_LO.set_cfg(16'h0, 16'h0, 16'h0, 20'h0F044, 32, 32'b00000000000000000000000000000000);
    PMU_SAI_CONTROL_POLICY2_LO.set_space_addr("MSG", 20'h0F044);
    PMU_SAI_CONTROL_POLICY2_LO.set_space_addr("CR-SB",28'h000F044); 
    PMU_SAI_CONTROL_POLICY2_LO.set_space("MSG");
    PMU_SAI_CONTROL_POLICY2_LO.set_msg_opcode("CR-SB");
    PMU_SAI_CONTROL_POLICY2_LO.set_security_policy("security_Policy_group", "PMU_SAI_POLICY2");
    if ( $test$plusargs("PMU_SAI_CONTROL_POLICY2_LO:dont_test") ) PMU_SAI_CONTROL_POLICY2_LO.set_test_reg(1'b0);
    if (!add_reg( PMU_SAI_CONTROL_POLICY2_LO )) begin
      `sla_error(get_name(), ("could not add register PMU_SAI_CONTROL_POLICY2_LO"));
    end
   PMU_SAI_CONTROL_POLICY2_LO.cp_policy.set_paths({"PMU_SAI_CONTROL_POLICY2_LO.cp_policy"});

      _ral_env.set_policy_mapping("PMU_SAI_POLICY2", "CP",  PMU_SAI_CONTROL_POLICY2_LO , PMU_SAI_CONTROL_POLICY2_HI );  
      _ral_env.set_policy_mapping("PMU_SAI_POLICY2", "WAC",  PMU_SAI_WRITE_POLICY2_LO , PMU_SAI_WRITE_POLICY2_HI );  
      _ral_env.set_policy_mapping("PMU_SAI_POLICY2", "RAC",  PMU_SAI_READ_POLICY2_LO , PMU_SAI_READ_POLICY2_HI );  
      _ral_env.set_policy_mapping("PMU_SAI_POLICY0", "CP",  PMU_SAI_CONTROL_POLICY0_LO , PMU_SAI_CONTROL_POLICY0_HI );  
      _ral_env.set_policy_mapping("PMU_SAI_POLICY0", "WAC",  PMU_SAI_WRITE_POLICY0_LO , PMU_SAI_WRITE_POLICY0_HI );  
      _ral_env.set_policy_mapping("PMU_SAI_POLICY0", "RAC",  PMU_SAI_READ_POLICY0_LO , PMU_SAI_READ_POLICY0_HI );  
      _ral_env.set_policy_mapping("PMU_SAI_POLICY1", "CP",  PMU_SAI_CONTROL_POLICY1_LO , PMU_SAI_CONTROL_POLICY1_HI );  
      _ral_env.set_policy_mapping("PMU_SAI_POLICY1", "WAC",  PMU_SAI_WRITE_POLICY1_LO , PMU_SAI_WRITE_POLICY1_HI );  
      _ral_env.set_policy_mapping("PMU_SAI_POLICY1", "RAC",  PMU_SAI_READ_POLICY1_LO , PMU_SAI_READ_POLICY1_HI );  
  endfunction

 virtual function void print_sv_ral_file();  
    $display("RAL file type [%s], RAL file instance [%s], SV file ---> %s", get_type_name(), get_name(), `__FILE__); 
 endfunction 
endclass : pmu_mmr_file

// ================================================


`endif
