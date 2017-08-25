#!/bin/sh -x
rm -rf a1

runspice.sh a1 b xa -srcdir /p/hlp/mnystroe/tcam/tcam64_42_extracted -p1274 -pfx s742rf040b064e1r1w1tbehsaa4pc2 -prog singlehit -pm io -extractpath tcam.sp -step 10e-12 -assertholdfrac 0.2 -temp 110 -clk 1e9 -m ffff -risefallfrac 0.1 -holdfrac 0.4 -design sdg64 -measure hitlinedroop
