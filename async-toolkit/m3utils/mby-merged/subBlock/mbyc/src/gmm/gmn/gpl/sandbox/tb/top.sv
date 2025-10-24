// Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
// SPDX-License-Identifier: Apache-2.0

module top;
import shared_pkg::*;
import mby_gmm_pkg::*;

// DUT interfaces
logic                  cclk;
logic                  reset_n;
mby_tag_ring_t         tag_ring_in[MBY_MAX_NUM_MGP-1:0][1:0];
mby_gpol_state_bcast_t gpol_update_bcast_out_left;
mby_gpol_state_bcast_t gpol_update_bcast_out_right;

// DUT
mby_gpl_top dut(.*);

// Clocks
clkgen #(.FREQ_MHZ(1200.0)) clkgen_cclk(cclk);

// Utility tasks/functions
`include "util.sv"

// Test sequence
initial begin : test_seq
    string test;

    $vcdplusmemon;

    begin : reset_seq
        reset_n = 0;
        delay_cclk(10);
        reset_n = 1;
        delay_cclk(1);
    end

    $value$plusargs("test=%s", test);
    case(test)
        "doa": `include "doa.svh"
        default: $fatal("Invalid test: %s", test);
    endcase

    delay_cclk(10);
    $finish;
end

// Test timeout and other checks
initial begin : test_timeout
    int timeout;

    if(!$value$plusargs("timeout=%d", timeout))
        timeout = 1000000;

    if(timeout != 0) begin
        delay_cclk(timeout);
        $fatal("Test timeout after %0d cclks", timeout);
    end
end
endmodule
