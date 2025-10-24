#!/bin/sh -x
./run_sigma.sh i0s 3  lvt &
./run_sigma.sh i0m 2 ulvt &
./run_sigma.sh i0m 2  lvt &
./run_sigma.sh i0m 3 ulvt &
./run_sigma.sh i0m 3  lvt &
wait
