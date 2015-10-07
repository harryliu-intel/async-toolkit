// vim:et:sw=4:ts=4:tw=79:

// INTEL TOP SECRET
// Copyright (C) 2006 - 2013 Intel Corporation
// All Rights Reserved.

// Note: The $write output will redirected to the Verilog simulator logfile,
// while the $error, $warning and $display output will be redirected to
// the Java test bench logfile.

`define CAST2VERILOG_MULTIPLE_GUARDS_TRUE_ERROR(info)                         \
    assert (0)                                                                \
    else                                                                      \
    begin                                                                     \
        $write({"%t: %m: ERROR: More than 1 guard in deterministic",          \
                " selection true at %s\n"},                                   \
               $time,                                                         \
               info);                                                         \
        $error("More than 1 guard in deterministic selection true at %s\n",   \
               info);                                                         \
    end

`define CAST2VERILOG_MULTIPLE_GUARDS_TRUE_ERROR2(info, num_true, true_guards) \
    assert (0)                                                                \
    else                                                                      \
    begin                                                                     \
        $write({"%t: %m: ERROR: More than 1 guard in deterministic",          \
                " selection true at %s: guards"},                             \
               $time,                                                         \
               info);                                                         \
        for (int i = num_true; i > 0; i--)                                    \
            $write(" #%0d", true_guards[num_true - 1]);                       \
        $write("\n");                                                         \
        $error("More than 1 guard in deterministic selection true at %s\n",   \
               info);                                                         \
    end

`define CAST2VERILOG_CSP_ERROR(info)                                          \
    assert (0)                                                                \
    else                                                                      \
    begin                                                                     \
        $write("%t: %m: ERROR: Reached CSP 'error' statement at %s\n",        \
               $time,                                                         \
               info);                                                         \
        $error("Reached CSP 'error' statement at %s\n", info);                \
    end

`define CAST2VERILOG_CSP_ASSERT(info,expr)                                    \
    assert (expr)                                                             \
    else                                                                      \
    begin                                                                     \
        $write("%t: %m: ERROR: Assertion failed at %s\n", $time, info);       \
        $error("Assertion failed at %s\n", info);                             \
    end

`define CAST2VERILOG_CSP_ASSERT2(info,expr,msg)                               \
    assert (expr)                                                             \
    else                                                                      \
    repeat (1)                                                                \
    begin                                                                     \
        var string __msg;                                                     \
        __msg = __csp_to_sv_string(msg);                                      \
        $write("%t: %m: ERROR: Assertion failed at %s: %s\n",                 \
               $time,                                                         \
               info,                                                          \
               __msg);                                                        \
        $error("Assertion failed at %s: %s\n", info, __msg);                  \
    end

`define CAST2VERILOG_CSP_WARNING(info,msg)                                    \
    assert (0)                                                                \
    else                                                                      \
    repeat (1)                                                                \
    begin                                                                     \
        var string __msg;                                                     \
        __msg = __csp_to_sv_string(msg);                                      \
        $write("%t: %m: Warning at %s: %s\n", $time, info, __msg);            \
        $warning("Warning at %s: %s\n", info, __msg);                         \
    end
