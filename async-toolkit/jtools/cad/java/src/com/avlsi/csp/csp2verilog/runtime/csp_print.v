// vim:et:sw=4:ts=4:tw=79:

// INTEL TOP SECRET
// Copyright (C) 2010 - 2013 Intel Corporation
// All Rights Reserved.

// Note: The $write output will redirected to the Verilog simulator logfile,
// while the $display output will be redirected to the Java test bench
// logfile.

`ifdef ENABLE_CSP_PRINT

`define CAST2VERILOG_CSP_PRINT(msg)                                           \
    repeat (1)                                                                \
    begin                                                                     \
        var string __msg;                                                     \
        __msg = __csp_to_sv_string(msg);                                      \
        $write("%t: %s: %s\n", $time, CAST2VERILOG_INSTANCE, __msg);          \
    end

`define CAST2VERILOG_CSP_PRINT2(tag, msg)                                     \
    repeat (1)                                                                \
    begin                                                                     \
        var string __msg, __tag;                                              \
        __msg = __csp_to_sv_string(msg);                                      \
        __tag = __csp_to_sv_string(tag);                                      \
        $write("%t: %s: %s (tag: %s)\n", $time, CAST2VERILOG_INSTANCE, __msg, __tag); \
    end

`else

`define CAST2VERILOG_CSP_PRINT(msg)
`define CAST2VERILOG_CSP_PRINT2(tag, msg)

`endif
