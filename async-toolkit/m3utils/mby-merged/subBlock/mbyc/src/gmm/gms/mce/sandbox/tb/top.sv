module top;
import shared_pkg::*;
import mby_gmm_pkg::*;
import mby_gms_pkg::*;

// DUT interfaces
logic                                                cclk;
logic                                                reset_n;
logic [MBY_MAX_NUM_MGP-1:0]                          i_mc_deep_q_wr = 0;
logic [MBY_MAX_NUM_MGP-1:0][MBY_PORTID_IN_MGP_MSB:0] i_mc_deep_q_rx_port_id = 0;
logic [MBY_MAX_NUM_MGP-1:0]                          i_mirror_deep_q_wr = 0;
logic [MBY_MAX_NUM_MGP-1:0][MBY_PORTID_IN_MGP_MSB:0] i_mirror_deep_q_rx_port_id = 0;
mby_pod_ptr_ring_t                                   i_mc_mirror_pod_ring_left = 0;
mby_pod_ptr_ring_t                                   i_mc_mirror_pod_ring_right = 0;
logic                                                o_stall_mc_mirror_pod_ring_left;
logic                                                o_stall_mc_mirror_pod_ring_right;
mc_mirror_deque_from_mce_t                           o_mce_mc_mirror_dequeue[15:0];
mby_mc_tag_ring_t                                    o_mc_tag_ring_out_left[3:0];
mby_mc_tag_ring_t                                    o_mc_tag_ring_out_right[3:0];

// DUT
mby_mce_top dut(.*);

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
