#!/bin/sh

echo -n $* 
echo `grep "^tcyc"  xa.meas | awk '{print $3}'`,\
`grep "^i_dyn" xa.meas | awk '{print $3}'`,\
`grep "^i_lea" xa.meas | awk '{print $3}'`,\
`pwd`,\
"default"
