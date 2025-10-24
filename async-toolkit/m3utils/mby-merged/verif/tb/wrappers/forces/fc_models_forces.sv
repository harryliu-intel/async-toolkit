// -----------------------------------------------------------------------------
// Copyright(C) 2011 Intel Corporation, Confidential Information
// -----------------------------------------------------------------------------
//
// Filename:    fc_models_forces.sv
// Revision:    0
// Revised By:  mmdhanif
// Revised On:  Date: 8/12/2016
// Created By:  mmdhanif
// Created On:  8/12/2016
// Description: This file contains the forces that are only model specific
//------------------------------------------------------------------------------

    // -------------------------------------------
    // Setting up MODEL SPECIFIC WORKAROUNDS
    // -------------------------------------------
    // Owner: mmdhanif
    // Date : 8/12/20116
    // HSD  : bXXXXXX
    // Info : Dummy force - template/reference for force enabling
    // -------------------------------------------
    `INITIAL begin
        if (apply_forces.upf_replacement_forces) begin
            $display("Apply forces for model_XXXXXX Simulation Workaround");
        end      
    end      

    // -------------------------------------------
