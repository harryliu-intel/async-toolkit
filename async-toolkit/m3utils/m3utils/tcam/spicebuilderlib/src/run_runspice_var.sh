#!/bin/sh -x
rm -rf var[0-7]
rm -rf var[0-7]_0


#runspice.sh var0 b xa -srcdir /p/hlp/mnystroe/tcam/tcam64_42_extracted -p1274 -pfx s742rf040b064e1r1w1tbehsaa4pc2_var -prog var -pm io -extractpath tcam.sp -step 10e-12 -assertholdfrac 0.2 -clk 1.0e9 -risefallfrac 0.1 -holdfrac 0.4 -design sdg64 -var 6 0 0 0 0 0 0 0 0 0 0 0 0 0  &

#runspice.sh var1 b xa -srcdir /p/hlp/mnystroe/tcam/tcam64_42_extracted -p1274 -pfx s742rf040b064e1r1w1tbehsaa4pc2_var -prog var -pm io -extractpath tcam.sp -step 10e-12 -assertholdfrac 0.2 -clk 1.0e9 -risefallfrac 0.1 -holdfrac 0.4 -design sdg64 -var 0 0 6 0 0 0 0 0 0 0 0 0 0 0  &

#runspice.sh var2 b xa -srcdir /p/hlp/mnystroe/tcam/tcam64_42_extracted -p1274 -pfx s742rf040b064e1r1w1tbehsaa4pc2_var -prog var -pm io -extractpath tcam.sp -step 10e-12 -assertholdfrac 0.2 -clk 1.0e9 -risefallfrac 0.1 -holdfrac 0.4 -design sdg64 -var 0 6 0 0 0 0 0 0 0 0 0 0 0 0  &

#runspice.sh var3 b xa -srcdir /p/hlp/mnystroe/tcam/tcam64_42_extracted -p1274 -pfx s742rf040b064e1r1w1tbehsaa4pc2_var -prog var -pm io -extractpath tcam.sp -step 10e-12 -assertholdfrac 0.2 -clk 1.0e9 -risefallfrac 0.1 -holdfrac 0.4 -design sdg64 -var 0 0 0 6 0 0 0 0 0 0 0 0 0 0  &

runspice.sh var4 b xa -srcdir /p/hlp/mnystroe/tcam/tcam64_42_extracted -p1274 -pfx s742rf040b064e1r1w1tbehsaa4pc2_var -prog var -pm io -extractpath tcam.sp -step 10e-12 -assertholdfrac 0.2 -clk 1.0e9 -risefallfrac 0.1 -holdfrac 0.4 -design sdg64 -var 0 0 0 0 0 6 0 0 0 0 0 0 0 0  -measure bitlinebump &

#runspice.sh var5 b xa -srcdir /p/hlp/mnystroe/tcam/tcam64_42_extracted -p1274 -pfx s742rf040b064e1r1w1tbehsaa4pc2_var -prog var -pm io -extractpath tcam.sp -step 10e-12 -assertholdfrac 0.2 -clk 1.0e9 -risefallfrac 0.1 -holdfrac 0.4 -design sdg64 -var 0 0 0 0 0 0 0 6 0 0 0 0 0 0  &

#runspice.sh var6 b xa -srcdir /p/hlp/mnystroe/tcam/tcam64_42_extracted -p1274 -pfx s742rf040b064e1r1w1tbehsaa4pc2_var -prog var -pm io -extractpath tcam.sp -step 10e-12 -assertholdfrac 0.2 -clk 1.0e9 -risefallfrac 0.1 -holdfrac 0.4 -design sdg64 -var 0 0 0 0 0 0 0 0 0 6 0 0 0 0  &

#runspice.sh var7 b xa -srcdir /p/hlp/mnystroe/tcam/tcam64_42_extracted -p1274 -pfx s742rf040b064e1r1w1tbehsaa4pc2_var -prog var -pm io -extractpath tcam.sp -step 10e-12 -assertholdfrac 0.2 -clk 1.0e9 -risefallfrac 0.1 -holdfrac 0.4 -design sdg64 -var 0 0 0 0 0 0 0 0 0 0 0 6 0 0  &

runspice.sh var4_0 b xa -srcdir /p/hlp/mnystroe/tcam/tcam64_42_extracted -p1274 -pfx s742rf040b064e1r1w1tbehsaa4pc2_var -prog var -pm io -extractpath tcam.sp -step 10e-12 -assertholdfrac 0.2 -clk 1.0e9 -cfg 0 -risefallfrac 0.1 -holdfrac 0.4 -design sdg64 -var 0 0 0 0 0 6 0 0 0 0 0 0 0 0  -measure bitlinebump &

wait
