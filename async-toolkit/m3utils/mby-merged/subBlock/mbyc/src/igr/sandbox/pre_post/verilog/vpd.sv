// Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
// SPDX-License-Identifier: Apache-2.0

module vpd;

initial begin
    if($test$plusargs("tracestart")) begin
        integer tracestart = 0;
        $value$plusargs("tracestart=%d", tracestart);
        $display("%10.3f %m INFO  Scheduling dumping at time=%0d", $realtime, tracestart);

        #tracestart;

        $vcdplusautoflushon;

        if($test$plusargs("tracedeltacycleon")) begin
            $vcdplusdeltacycleon;
        end

        $vcdpluson();

        if($test$plusargs("tracememon")) begin
            $vcdplusmemon();
        end

    end else begin
        $display("%10.3f %m INFO  To enable dumping, use TRACESTART=%%d", $realtime);
    end
end

endmodule
