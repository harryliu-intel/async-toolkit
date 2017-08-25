#!/bin/sh -x

spicebuilder -prog var -pm io -extractpath tcam.sp -step 10e-12 -f xa -assertholdfrac 0.2 -clk 1.0e9 -risefallfrac 0.1 -holdfrac 0.4 -design sdg64 -var 0 0 0 0 0 6 0 0 0 0 0 0 0 0 -measure bitlinebump

