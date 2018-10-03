// -----------------------------------------------------------------------------
// Copyright(C) 2011 Intel Corporation, Confidential Information
// -----------------------------------------------------------------------------
//
// Filename:    RCSfile: force_if.sv
// Revision:    Revision: 0 
// Revised By:  Author: mmdhanif 
// Revised On:  Date: 8/12/2016 
// Created By:  mmdhanif 
// Created On:  8/12/2016
// Description: Interface object that is used for force generation at FC  
//
// -----------------------------------------------------------------------------

interface force_if;

    //-------------------------------
    //   TB Force  
    //-------------------------------

    bit forces_set_by_testbench = 0;
  
    //-------------------------------
    //    SPEEDUP FORCES
    //-------------------------------
    bit func_mphy_forces = 0;
    bit speedup_forces = 1;
    bit voltage_detect_forces = 0;
    bit bypass_spi_desc_ss_ld = 1;
    bit bypass_fuse_sense = 1;
    bit speedup_pmu = 1;

    //-------------------------------
    //    MODEL PATCH FORCES
    //-------------------------------
    bit upf_replacement_forces = 1;

    //-------------------------------
    //    BUG WORKAROUND FORCES
    //-------------------------------
    bit patch_bXXXXXX = 0;
    //-------------------------------
    initial begin

        // DO NOT set SPEEDUP forces here: SPEEDUP forces should be set in test/testbench code
        // All forces are enabled by default - unless specifically disable in cmdline
        // -------------------------------------------
        // Setting up DUT WORKAROUNDS
        // ------------------------------------------- 
        // Owner: mmdhanif
        // Date : 8/12/20116 
        // HSD  : bXXXXXX 
        // Info : Dummy force - template/reference for force enabling 

        if(!$test$plusargs("bXXXXXX_FIXED")) begin
            patch_bXXXXXX = 1;
        end      

    end      
endinterface
