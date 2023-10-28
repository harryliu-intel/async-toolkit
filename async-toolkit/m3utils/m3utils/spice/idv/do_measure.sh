#!/bin/sh

sim=$1
shift

if [ "$sim" == "xa" ]; then

echo -n $* 
echo `grep "^tcyc"  xa.meas | awk '{print $3}'`,\
`grep "^i_dyn" xa.meas | awk '{print $3}'`,\
`grep "^i_lea" xa.meas | awk '{print $3}'`,\
`pwd`,\
"default"

elif [ "$sim" == "hspice" ]; then

echo -n $* 
echo `grep "^tcyc"  circuit.mt0 | awk '{print $3}'`,\
`grep "^i_dyn" circuit.mt0 | awk '{print $3}'`,\
`grep "^i_lea" circuit.mt0 | awk '{print $3}'`,\
`pwd`,\
"default"
    
else
    echo "Unknown sim $sim"
    exit 1
fi
