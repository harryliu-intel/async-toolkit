// Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
// SPDX-License-Identifier: Apache-2.0

`ifndef INC206
`define INC206

// ===================================================
// Flop macros -- from Nebulon 2.01 - Bhuvan Meka - 071912  

`ifndef RTLGEN_FF
`define RTLGEN_FF(rtl_clk, rst_n, rst_val, d, q) \
    always_ff @(posedge rtl_clk, negedge rst_n) \
        if (!rst_n) q <= rst_val; \
        else        q <= d;
`endif // RTLGEN_FF

`ifndef RTLGEN_EN_FF
`define RTLGEN_EN_FF(rtl_clk, rst_n, rst_val, en, d, q) \
    always_ff @(posedge rtl_clk, negedge rst_n) \
        if (!rst_n) q <= rst_val; \
        else \
            if (en) q <= d;
`endif // RTLGEN_EN_FF

`endif
