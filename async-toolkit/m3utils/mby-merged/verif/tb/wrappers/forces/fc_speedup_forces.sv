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

    end      
