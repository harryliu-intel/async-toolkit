#!/bin/sh -x
spicebuilder -prog rw -pm io -extractpath tcam.sp -assertholdfrac 0.2 -clk 1.0e9 -risefallfrac 0.1 -holdfrac 0.5 -step 10e-12 -f xa -design andrew 
