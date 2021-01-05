#!/bin/sh -x

DATE=`date +"%Y-%m-%d %H:%M"`

gnuplot << __GNUPLOT__

# first the SVS curve

set title "JBay B0 SVS Curve ${DATE}"
set xlabel "V/[V]"
set ylabel "P/[W]"
plot "jbay.svs" using 2:5

pause 1

__GNUPLOT__

gnuplot << __GNUPLOT__

# then the power histogram

set title "JBay B0 Process Power Histogram ${DATE} (samples=100K)"
set ylabel "count/[1]"
set xlabel "P/[W]"
plot "hist_hist.dat" with lines

pause 1

__GNUPLOT__

gnuplot << __GNUPLOT__

# then the yield loss

set title "JBay B0 Process Power Yield Loss ${DATE} (samples=100K)"
set logscale y
set ylabel "yield loss/[1]"
set xlabel "P/[W]"
plot [:][0.01:1]"hist_loss.dat" with lines

pause 1

__GNUPLOT__
