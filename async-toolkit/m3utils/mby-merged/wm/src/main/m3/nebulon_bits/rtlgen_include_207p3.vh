`ifndef INC207
`define INC207

// HEY!  Whenever we change this file, we must also change the name of the
// package and the `ifndefs too!
// There is the $rtlgen_include_version in specman.pm which must also be changed.

// HEY!  Whenever we change this file, we must also make the following changes
// in the nebulon_qa directory.  (If we are doing nebulon_qa testing.)
// 1.	Add symlinks to ./nebulon_qa/tests/RTL_tests/src/
// 2.	Add symlinks to ./nebulon_qa/tests/RTL_tests/common/


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


// ===================================================
// Latch macros -- from Nebulon 2.07p3 - Bhuvan Meka   

`ifndef RTLGEN_EN_LATCH
`define RTLGEN_EN_LATCH(rst_n, rst_val, en, d, q) \
    always_latch \
        if (!rst_n) q <= rst_val; \
        else \
            if (en) q <= d;
`endif // RTLGEN_EN_LATCH

`endif



