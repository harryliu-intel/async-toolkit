#!/bin/sh -x

EXEC=/nfs/site/disks/wdisk.133/mnystroe/1588/sim/AMD64_LINUX/sim1588

samples=10000
#cycles=50000
base=1e9

for prec in "0e-6" "1e-6" "4.6e-6" "50e-9" "100e-6" "50e-6" "200e-6" "400e-6"; do
    for spd in "1 8" "330 640" "825 1280" "24 1000" "1 1"; do
        for ref in "257 256" "17 16" "8192 8191" "256 255" "128 127" "8 7" "9 8" "7 6" "32 31" "33 32"; do
            for jitter in "0e-12" "1e-12" "3e-12" "10e-12" "30e-12" "100e-12"; do
                step=`echo $ref | sed 's/ .*$//'`
                cycles=$(((step/100+1)*5000)) 
                nbjob run ${EXEC} ${base} $spd $ref $prec $step $jitter $samples $cycles &
            done
        done
    done
done


