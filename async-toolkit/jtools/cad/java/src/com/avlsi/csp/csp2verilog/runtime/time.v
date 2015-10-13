// vim:et:sw=4:ts=4:tw=80:

// INTEL TOP SECRET
// Copyright 2004 - 2013 Intel Corporation
// All Rights Reserved.

`ifndef PRS2VERILOG_TIMEUNIT
`define PRS2VERILOG_TIMEUNIT 1ns
`endif
`define PRS2VERILOG_DELAY(prsDelay, delayBias, extraDelay)                     \
    (((prsDelay)*(delayBias)+(extraDelay))*`PRS2VERILOG_TAU*`PRS2VERILOG_TIMEUNIT)

/******************************************************************************/
/** CAST2VERILOG_TIME
 *
 * \desc            Retrieves the current simulation time in DSim time units.
 *
 * \return          the current simulation time in DSim time units.
 *
 ******************************************************************************/
`define CAST2VERILOG_TIME                                                      \
    ((100.0/(`PRS2VERILOG_TAU*`PRS2VERILOG_TIMEUNIT))*$realtime)

`define CAST2VERILOG_WAIT(waitTime)                                            \
    #((waitTime)*(`PRS2VERILOG_TAU*`PRS2VERILOG_TIMEUNIT)/100.0)

`define PRS2VERILOG_TIMESCALE                                                  \
    timeunit `PRS2VERILOG_TIMEUNIT; timeprecision 1fs;
