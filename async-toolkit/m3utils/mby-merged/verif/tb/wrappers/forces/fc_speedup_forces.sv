// -----------------------------------------------------------------------------
// Copyright(C) 2011 Intel Corporation, Confidential Information
// -----------------------------------------------------------------------------
//
// Filename:    fc_speedup_forces.sv
// Revision:    0
// Revised By:  mmdhanif
// Revised On:  Date: 8/12/2016
// Created By:  mmdhanif
// Created On:  8/12/2016
// Description: This file contains the forces associated only with 
//              simulation speedup workaround 
//------------------------------------------------------------------------------

    // -------------------------------------------
    // Setting up SIMULATION SPEEDUP WORKAROUNDS
    // -------------------------------------------
    // Owner: mmdhanif
    // Date : 8/12/20116
    // Info : Dummy force - template/reference for force enabling
    // -------------------------------------------
    `INITIAL begin
        if (apply_forces.speedup_forces) begin
            $display("Apply forces for speedup_XXXXXX Simulation Workaround");

        end      

    // -------------------------------------------
    // Owner: dteo
    // Date : 8/23/2017
    // Info : Speedup PMU
    // -------------------------------------------
        if (apply_forces.speedup_pmu) begin
            $display("Apply forces to speedup PMU");
            force `PMU_WRAPPER1.pmu_fastrst_sus = 1;
        end      

    // -------------------------------------------
    // Owner: dteo
    // Date : 8/23/2017
    // Info : Bypass Fuse Sense
    // -------------------------------------------
        if (apply_forces.bypass_fuse_sense) begin
            $display("Apply forces to bypass Fuse Sense");
            force `FUSE_TOP.i_chassis_fuse_controller_top.i_fuse_array_cntrl.i_fuse_sense_fsm.bypass_pwron_sense = 1;
        end      

    // -------------------------------------------
    // Owner: mmdhanif
    // Date : 8/23/2017
    // Info : Bypass SPI Descriptor & SS load from Flash
    // -------------------------------------------
        if (apply_forces.bypass_spi_desc_ss_ld) begin
            $display("Apply forces to bypass Descriptor and SS load from Flash");
            //force `SPI_TOP.spi_pgd_top1.iosf_top1.spi_iosfsb_i.iosfsb_creg_i.iosfsb_cfg_reg_i.creg_dfx_ovrd.bsdl = 1;
            //force `SPI_TOP.spi_pgd_top1.iosf_top1.spi_iosfsb_i.iosfsb_regstrap_i.xphsbidec1.SSVALID = 1;
            force `FUSE_TOP.i_fuse_clock_power_cntrl.ss_ld_done = 1;
        end      


    end      
