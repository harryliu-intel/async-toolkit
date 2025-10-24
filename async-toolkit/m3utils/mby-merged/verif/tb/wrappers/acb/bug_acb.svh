// -----------------------------------------------------------------------------
// Copyright(C) 2011 Intel Corporation, Confidential Information
// -----------------------------------------------------------------------------
//
// Filename:    bug_acb.sv
// Revision:    0
// Revised By:  mmdhanif
// Revised On:  Date: 8/12/2016
// Created By:  mmdhanif
// Created On:  8/12/2016
// Description: This file contains the all the assertion control for any RTL Bug related issue
//              All assertion here must have a ticket to it  

    // -------------------------------------------------------------------------
    // Owner: mmdhanif
    // Date : 8/12/20116
    // HSD  : bt_pre0_rtc
    // Info : Assertion to control bugXXXXXX  
    // -------------------------------------------------------------------------

    // -------------------------------------------------------------------------
    // Owner: dteo
    // Date : 8/23/2017
    // HSD  : 
    // Info : ACBs for 0p0 boot bringup
    // -------------------------------------------------------------------------
    `ACB_TURN_OFF_BEGIN(0p0_boot_bringup)
//        $assertoff(0, `HVL_TOP);
//        $assertoff(0, `HDL_TOP);
        $assertoff(0, `FUSE_TOP);
        $assertoff(0, `CGU.aips.sscpll.ip74xplllcicl_anatop0.ibuf_ro);
        $assertoff(0, `CGU.aips.sscpll.ip74xplllcicl_anatop0.iosc.iscbias);
        //$assertoff(0, `HVL_TOP.soc_sbr6_sbr4_intf.genblk.sbc_compliance.agent_ism_compliance);
        //$assertoff(0, `HVL_TOP.soc_sbr6_sbr4_intf.genblk.sbc_compliance.fabric_ism_compliance);
        //$assertoff(0, `HVL_TOP.soc_sbr5_sbr4_intf.genblk.sbc_compliance.agent_ism_compliance);
        //$assertoff(0, `HVL_TOP.soc_sbr5_sbr4_intf.genblk.sbc_compliance.fabric_ism_compliance);
        //$assertoff(0, `HVL_TOP.soc_sbr4_sbr3_intf.genblk.sbc_compliance.agent_ism_compliance);
        //$assertoff(0, `HVL_TOP.soc_sbr4_sbr3_intf.genblk.sbc_compliance.fabric_ism_compliance);
        //$assertoff(0, `HVL_TOP.soc_sbrProxy_sbr2_intf.genblk.sbc_compliance.agent_ism_compliance);
        //$assertoff(0, `HVL_TOP.soc_sbrProxy_sbr2_intf.genblk.sbc_compliance.fabric_ism_compliance);
        //$assertoff(0, `HVL_TOP.soc_sbr3_sbr2_intf.genblk.sbc_compliance.agent_ism_compliance);
        //$assertoff(0, `HVL_TOP.soc_sbr3_sbr2_intf.genblk.sbc_compliance.fabric_ism_compliance);
        //$assertoff(0, `HVL_TOP.soc_sbr2_sbr1_intf.genblk.sbc_compliance.agent_ism_compliance);
        //$assertoff(0, `HVL_TOP.soc_sbr2_sbr1_intf.genblk.sbc_compliance.fabric_ism_compliance);
        //$assertoff(0, `HVL_TOP.soc_sbr1_sbr0_intf.genblk.sbc_compliance.agent_ism_compliance);
        //$assertoff(0, `HVL_TOP.soc_sbr1_sbr0_intf.genblk.sbc_compliance.fabric_ism_compliance);
    `ACB_TURN_OFF_END


    // -------------------------------------------------------------------------
initial begin
    turnAssertionsOff_0p0_boot_bringup();
end      

