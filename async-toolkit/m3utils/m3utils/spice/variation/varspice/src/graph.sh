#!/bin/sh

# graph results of sigma variation optimizations
# run summary.sh FIRST

anal=${M3UTILS}/spice/variation/varspice/src/anal.awk

for f in result_*p?.csv; do
    $anal $f | tail -1 | awk '{print $2 " " $1}'
done
