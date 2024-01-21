#!/bin/sh

for dir in *.run; do
    pushd $dir
    ${M3UTILS}/spice/fwr_clk_tc2/AMD64_LINUX/measure
    popd
done
