// -----------------------------------------------------------------------------
// Copyright(C) 2011 Intel Corporation, Confidential Information
// -----------------------------------------------------------------------------
//
// Filename:    force_if.sv
// Revision:    0
// Revised By:  mmdhanif
// Revised On:  Date: 8/12/2016
// Created By:  mmdhanif
// Created On:  8/12/2016
// Description: Force Main File 
//
// -----------------------------------------------------------------------------

`define INITIAL always @(posedge apply_forces.forces_set_by_testbench)

`include "fc_bug_workaround_forces.sv"
`include "fc_speedup_forces.sv"
`include "fc_models_forces.sv"

