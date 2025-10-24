#!/bin/sh
# make-report <scriptdir> <outdir> <runname>

scriptdir=$1
outdir=$2
report=$3
meta=${scriptdir}/${report}.meta


results=`find ${outdir}/${report} -name properties -print`

if [ "X$results" = "X" ]; then
		delay="NO-RESULT"
else
		delay=`grep delay $results | sed 's/^.*delay = \(.*\),.*/\1/'`
fi
#echo $res

#start_probe=`grep 'chip.alta.scheduler.rx_sched.start.START_PROBE' $meta`
start_probe=`grep 'lib.metastable.probe.PROBE_WAIT' $meta`
slow_probe=`grep 'BUFFERED_PROBE_WAIT(1)' $meta`
fast_probe=`grep 'BUFFERED_PROBE_WAIT(3)' $meta`
dual_arbit=`grep 'DUAL_ARB' $meta`
instance=`awk 'p { print $0; p=0; } /relative-instance-name/ { p=1; } ' $meta`

sp="***UNKNOWN***"

if [ "X$slow_probe" != "X" ]; then
		sp="SLOW_PROBE"
fi
if [ "X$fast_probe" != "X" ]; then
		sp="FAST_PROBE"
fi
if [ "X$dual_arbit" != "X" ]; then
		sp="DUAL_ARB"
fi
if [ "X$start_probe" != "X" ]; then
		sp="FAST_PROBE"
fi
		
#echo  X$results $3 $instance $delay $sp
if [ "X$results" = "X" ]; then
		/usr/bin/printf "%-80s %-50s %10s   %s\n" $3 $instance $delay $sp
else
		/usr/bin/printf "%-80s %-50s %10.2f   %s\n" $3 $instance $delay $sp
fi
