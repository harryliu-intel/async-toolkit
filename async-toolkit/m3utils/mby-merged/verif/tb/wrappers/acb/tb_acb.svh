// -----------------------------------------------------------------------------
// INTEL CONFIDENTIAL
// Copyright(c) 2018, Intel Corporation. All Rights Reserved
// -----------------------------------------------------------------------------
//
// Created By   :  Mmdhanif
// Created On   :  08/12/2016
// Description: This file contains the all the assertion control for any TB Bug related issue (assertion bug)
//              All assertion here must have a bugtool ticket to it  


    // -------------------------------------------------------------------------
    // Setting up Assertion Control
    // -------------------------------------------------------------------------
    // Owner: Jonathan Novack - jtnovack
    // Date : 10/18/20116
    // HSD  : 2201039834
    // Info : Assertion to suppress upmas assertion check on time 0ns
    // -------------------------------------------------------------------------
    `ifdef UPMAS_RTL_ENABLE
    `ACB_TURN_OFF_BEGIN(upmas_time0_off)
        $assertoff(0, `pmsrvr_upmas);
    `ACB_TURN_OFF_END

    `ACB_TURN_ON_BEGIN(upmas_time0_on)
        $asserton(0, `pmsrvr_upmas);
    `ACB_TURN_ON_END
    
    initial begin
        turnAssertionsOff_upmas_time0_off();
        wait(`pmsrvr_upmas.punit_pma_prim_rst_b === 1'b1 || `pmsrvr_upmas.punit_pma_prim_rst_b === 1'b0);
        turnAssertionsOn_upmas_time0_on();
    end      
    `endif

