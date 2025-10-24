#!/bin/sh -x

DATE=`date +"%Y-%m-%d %H:%M"`
PAUSE=0
RES="1500,1000 font \"arial,30\""
OFF=31

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

plot "${PFX}.dat" using (\$1+${OFF}):2 with lines

pause ${PAUSE}

__GNUPLOT__

PFX=hist_loss
gnuplot << __GNUPLOT__

# then the yield loss

set title "JBay B0 Process Power Yield Loss ${DATE} (samples=100K)"
set logscale y
set ylabel "yield loss/[1]"
set xlabel "P/[W]"

set term png size ${RES}
set output "${PFX}.png"

plot [:][0.01:1] "${PFX}.dat" using (\$1+${OFF}):2 with lines

pause ${PAUSE}

__GNUPLOT__
