// vim:et:sw=4:ts=4:tw=80:

// INTEL TOP SECRET
// Copyright 2004 - 2013 Intel Corporation
// All Rights Reserved.

`define PRS2VERILOG_DELAY(prsDelay, delayBias, extraDelay)                     \
    (((prsDelay)*(delayBias)+(extraDelay))*`PRS2VERILOG_TAU*1ns)

/******************************************************************************/
/** CAST2VERILOG_TIME
 *
 * \desc            Retrieves the current simulation time in DSim time units.
 *
 * \return          the current simulation time in DSim time units.
 *
 ******************************************************************************/
`define CAST2VERILOG_TIME                                                      \
    ((100.0/(`PRS2VERILOG_TAU*1ns))*$realtime)

`define CAST2VERILOG_WAIT(waitTime)                                            \
    #((waitTime)*(`PRS2VERILOG_TAU*1ns)/100.0)

`define PRS2VERILOG_TIMESCALE                                                  \
    timeunit 1ns; timeprecision 1fs;
