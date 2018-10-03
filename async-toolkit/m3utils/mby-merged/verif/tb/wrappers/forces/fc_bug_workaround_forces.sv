// -----------------------------------------------------------------------------
// Copyright(C) 2011 Intel Corporation, Confidential Information
// -----------------------------------------------------------------------------
//
// Filename:    fc_bug_workaround_forces.sv
// Revision:    0
// Revised By:  mmdhanif
// Revised On:  Date: 8/12/2016
// Created By:  mmdhanif
// Created On:  8/12/2016
// Description: This file contains the bug workaround forces 
//              Forces in this file must have a bug/bugtool id associated with it
//              Once the bug is closed/released, the forces must be removed 
//------------------------------------------------------------------------------

    // -------------------------------------------
    // Setting up DUT BUG WORKAROUNDS
    // -------------------------------------------
    // Owner: mmdhanif
    // Date : 8/12/2016
    // HSD  : bXXXXXX
    // Info : Dummy force - template/reference for force enabling
    // -------------------------------------------
    `INITIAL begin
        if (apply_forces.patch_bXXXXXX) begin
            $display("Apply forces for bXXXXXX Simulation Workaround");

        end      
    end      

    // -------------------------------------------
    // Owner: dteo
    // Date : 8/10/2017
    // HSD  : bXXXXXX
    // Info : Placeholder for LAN_POWERGOOD_RST_B# GPIO workaround
    // -------------------------------------------
    `INITIAL begin
        $display("Apply forces for LAN_POWERGOOD_RST_B# Workaround");

        force `PMU_WRAPPER1.prescc_powergood_rst_b = `HVL_TOP.fc_sig_if.lan_powergood_rst_b;
        force `CGU.int_pwell_pok_early_sus = `HVL_TOP.fc_sig_if.lan_powergood_rst_b;
        force `CCU_TOP.ccu_SUSCLK_AON1_main_powergood_rst_b = `CGU.powergood_rst_b_postdfx;
        force `CCU_TOP.ccu_SUSCLK_AON1_trunk_powergood_rst_b = `CGU.powergood_rst_b_postdfx;
     
    end      

    // -------------------------------------------
    // Owner: dteo
    // Date : 8/22/2017
    // HSD  : bXXXXXX
    // Info : Placeholder for Early Boot workarounds
    // -------------------------------------------
    `INITIAL begin
        $display("Apply forces for Early Boot Workarounds");
        // DFX
        //force `DFXAGG_TOP.dfxagg_side_clkack = `DFXAGG_TOP.dfxagg_side_clkreq;
        force `DFXAGG_TOP.dfxagg_side_ism_lock_b = 1;
        force `DFXAGG_TOP.dfxagg_boot_halt_b[0] = 1;
        force `DFXAGG_TOP.dfxagg_boot_halt_b[1] = 1;
        
        // CGU
//        force `CGU.thermtrip_global_n = 1; 
        force `CGU.sideband.cgu_pll_enable = `PMU_WRAPPER1.pmu_cgu_pll_en; 
        force `PMU_WRAPPER1.cgu_pmu_pll_valid = `CGU.aips.cgupll_plllock;

//Jaya Added - Start
force `sbr_gp0.sbr_clk_clkack = `sbr_gp0.sbr_clk_clkreq;
force `sbr_gp0.async0_clkack = `sbr_gp0.async0_clkreq;
force `sbr_gp0.async1_clkack = `sbr_gp0.async1_clkreq;
force `sbr_gp1.sbr_clk_clkack = `sbr_gp1.sbr_clk_clkreq;
force `sbr_gp2.sbr_clk_clkack = `sbr_gp2.sbr_clk_clkreq;
// Jaya - End

/*  Jaya  OLD
        // SBR
        force `SBR0.sbr_clk_clkack = `SBR0.sbr_clk_clkreq;
        //force `SBR0.sbr0_inst.fuse_ctrl_sbr0_pok = `FUSE_TOP.fuse_side_pok;
        //force `SBR0.sbr0_inst.fuse_drng_sbr0_pok = `FUSE_TOP.drng_side_pok;
        
        force `SBR1.sbr_clk_clkack = `SBR1.sbr_clk_clkreq;
        force `SBR1.async0_clkack = `SBR1.async0_clkreq;
        //force `SBR1.sbr1_inst.espispi_sbr1_pok = `SPI_TOP.side_pok;

        force `SBR2.sbr_clk_clkack = `SBR2.sbr_clk_clkreq;
        force `SBR2.async0_clkack = `SBR2.async0_clkreq;
        force `SBR2.async0_clk = `CGU.rosc120clk;
        //force `SBR2.sbr2_inst.dfx_aggr_sbr2_pok = `DFXAGG_TOP.dfxagg_side_pok;

        force `SBR3.sbr_clk_clkack = `SBR3.sbr_clk_clkreq;
        force `SBR3.async0_clkack = `SBR3.async0_clkreq;
        force `SBR3.async0_clk = `CGU.rosc120clk;

        force `SBR4.sbr_clk_clkack = `SBR4.sbr_clk_clkreq;
        force `SBR4.async0_clkack = `SBR4.async0_clkreq;
        force `SBR4.async0_clk = `CGU.rosc120clk;

        force `SBR5.sbr_clk_clkack = `SBR5.sbr_clk_clkreq;
        force `SBR5.async0_clkack = `SBR5.async0_clkreq;
        force `SBR5.async0_clk = `CGU.rosc120clk;
        
        force `SBR6.sbr_clk_clkack = `SBR6.sbr_clk_clkreq;
        force `SBR6.async0_clkack = `SBR6.async0_clkreq;
        force `SBR6.async0_clk = `CGU.rosc120clk;
        
        force `SBRPROXY.sbr_clk_clkack = `SBRPROXY.sbr_clk_clkreq;
        force `SBRPROXY.async0_clkack = `SBRPROXY.async0_clkreq;
        force `SBRPROXY.async0_clk = `CGU.rosc120clk;
*/ //Jaya
        // Fuse
        force `FUSE_TOP.fuse_side_ism_locked = 0;
        force `FUSE_TOP.fuse_func_clk_clkack = `FUSE_TOP.fuse_func_clk_clkreq;
        force `FUSE_TOP.fuse_side_clkack = `FUSE_TOP.fuse_side_clkreq;
        force `FUSE_TOP.drng_side_clkack = `FUSE_TOP.drng_side_clkreq;

        // SPI
        force `PMU_WRAPPER1.spi_early_boot_done = 1;
		
		//IO Widget (now Early Boot device)
	    //force `IOW_TOP.iow_side_clkack = `IOW_TOP.iow_side_clkreq;
	    //force `IOW_TOP.pmc_iow_wake = 1;
		//force `IOW_TOP.strap_iow_sb_pid[7:0] = 'hFC;

        // PMU
        force `PMU_WRAPPER1.cds5_early_boot_done = 1;
        force `PMU_WRAPPER1.sbr_pmu_side_clkack = `PMU_WRAPPER1.pmu_sbr_side_clkreq;
        force `PMU_WRAPPER1.i_pmu.i_pmummr.pmummrgen1.INT_STS_0.IP_PWR_STS_ALL1_STS = 1; //ssn temp to work around dashboard and FW issue 
`ifdef HSUART_ENV_ENABLE
		force `PMU_WRAPPER1.i_pmu.i_pmummr.pmummrgen1.ST_DIS_MASK_0 = 32'b1111_1111_1111_1111_1110_1100_0111_1111;
`else
		force `PMU_WRAPPER1.i_pmu.i_pmummr.pmummrgen1.ST_DIS_MASK_0 = 32'b1111_1111_1111_1111_1111_1101_1111_1111;
`endif
		force `PMU_WRAPPER1.i_pmu.i_pmummr.pmummrgen1.ST_DIS_MASK_1 = 32'hffffffff;
		force `PMU_WRAPPER1.emp_fuse_pull_complete = 1;
        force `PMU_WRAPPER1.gpio0_prim_pok = 1; // GPIO does not support prim_pok
		force `PMU_WRAPPER1.psf0_prim_pok = 1; // PSF0 does not support prim_pok 
		force `PMU_WRAPPER1.psf1_prim_pok = 1; // PSF1 does not support prim_pok;
		force `PMU_WRAPPER1.i_pmu.i_pmummr.cr_ip_ready_sts[7] = 1; // PSF0 does not send IP_READY
		force `PMU_WRAPPER1.i_pmu.i_pmummr.cr_ip_ready_sts[8] = 1; // PSF1 does not send IP_READY
		force `PMU_WRAPPER1.i_pmu.i_pmummr.cr_ip_ready_sts[12] = 1; // HSUART does not send IP_READY
		force `PMU_WRAPPER1.emp_prim_rst_b = 0; // Disable EMP for DLC
		force `PMU_WRAPPER1.cgu_sb_portid = 'h12; // update CGU sideband Port ID

        // HSUART
        //force `HSU_UARTIOSF_TOP.side_rst_b = `PMU_WRAPPER1.emp_prim_rst_b;
        //force `HSU_UARTIOSF_TOP.pmc_hsu_wake = 1;
        //force `HSU_UARTIOSF_TOP.prim_clkack =  `HSU_UARTIOSF_TOP.prim_clkreq;
        //force `HSU_UARTIOSF_TOP.side_clkack =  `HSU_UARTIOSF_TOP.side_clkreq;
        //force `HSU_UARTIOSF_TOP.pgcb_clkack =  `HSU_UARTIOSF_TOP.pgcb_clkreq;
        //force `HSU_UARTIOSF_TOP.pmc_hsu_pg_ack_b = `HSU_UARTIOSF_TOP.hsu_pmc_pg_req_b;
        //force `HSU_UARTIOSF_TOP.pgcb_rst_b = `PMU_WRAPPER1.emp_prim_rst_b;



        // GPIO
    //akau    force `GPCOMMSTUNIT_WRAPPER.strapenable_rsmrstb = `CGU.aips.cgupll_plllock;
    //akau    force `GPCOMMSTUNIT_WRAPPER.sbr_gpsbmst_side_clkack = `GPCOMMSTUNIT_WRAPPER.gpsbmst_sbr_side_clkreq;
    //akau    force `GPCOMMSTUNIT_WRAPPER.pcg_gpcom_gpio_clkack = `GPCOMMSTUNIT_WRAPPER.gpcom_pcg_gpio_clkreq;
    //akau    force `GPCOMMSTUNIT_WRAPPER.pcg_gpcom_pgcb_clkack = `GPCOMMSTUNIT_WRAPPER.gpcom_pcg_pgcb_clkreq;
    //akau    force `GPCOMMSTUNIT_WRAPPER.pmc_gpcom_vnnack = `GPCOMMSTUNIT_WRAPPER.gpcom_pmc_vnnreq;
    end      

