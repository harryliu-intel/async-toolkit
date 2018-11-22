task delay_cclk(int n);
    repeat(n) @(posedge cclk);
endtask
