module top;

// DUT interfaces
logic cclk;
logic reset_n;

// DUT
<DUT> dut(.*);

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
