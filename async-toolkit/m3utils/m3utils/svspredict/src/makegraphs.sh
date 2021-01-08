#!/bin/sh -x

# 319W comes from...
# asking for 4% margin for power supply
# 3W guardband for serdes
#
# ---- all taken off the TDP target of 350W

CUSTMAXPWR=350

../AMD64_LINUX/svspredict -d JBay -H 30 -samples 100000 -a jbay_svs.dat -custmaxpower ${CUSTMAXPWR} | 2>&1 tee makegraphs.log

NEWTARGETPWR=`grep DIECUTOFF makegraphs.log | awk '{print $2}'`


DATE=`date +"%Y-%m-%d %H:%M"`
PAUSE=0
RES="2000,1600 font \"arial,30\""

PFX=jbay_svs
gnuplot << __GNUPLOT__

# first the SVS curve

set title "JBay B0 SVS Curve ${DATE}"
set xlabel "V/[V]"
set ylabel "P/[W]"

set term png size ${RES}
set output "${PFX}.png"

plot "${PFX}.dat" using 2:5

pause ${PAUSE}

__GNUPLOT__

PFX=hist_hist
gnuplot << __GNUPLOT__

# then the power histogram

set title "JBay B0 Process Power Histogram ${DATE} (samples=100K)"
set ylabel "count/[1]"
set xlabel "P/[W]"

set term png size ${RES}
set output "${PFX}.png"

set arrow from ${NEWTARGETPWR},graph(0,0) to ${NEWTARGETPWR},graph(1,1) nohead lw 5

plot "${PFX}.dat" with lines

pause ${PAUSE}

__GNUPLOT__

PFX=hist_loss
gnuplot << __GNUPLOT__

# then the yield loss

set title "JBay B0 Process Power Yield Loss ${DATE}"
set ylabel "yield loss/[1]"
set xlabel "P/[W]"

set term png size ${RES}
set output "${PFX}.png"

plot [:][0.01:1] "${PFX}.dat" with lines

pause ${PAUSE}

__GNUPLOT__

PFX=hist_deltav_loss
gnuplot << __GNUPLOT__

# then the yield loss

set title "JBay B0 Process Voltage Margin Yield Loss ${DATE} Ptarget=${TARGETPWR}W"
set ylabel "yield loss/[1]"
set xlabel "voltage margin/[V]"

set term png size ${RES}
set output "${PFX}.png"

plot [:][0.01:1] "${PFX}.dat" with lines

pause ${PAUSE}

__GNUPLOT__

PFX=hist_cutoff_hist
gnuplot << __GNUPLOT__

# then the power histogram

set title "JBay B0 Process Power Histogram ${DATE} (samples=100K)"
set ylabel "count/[1]"
set xlabel "P/[W]"

set term png size ${RES}
set output "${PFX}.png"

plot "${PFX}.dat" with lines

pause ${PAUSE}

__GNUPLOT__

