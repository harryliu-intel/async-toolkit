#!/bin/sh -x

sed -e s/NON_ATOMIC/PG0/ -e s/ATOMIC_128/PG1/ -e s/ATOMIC_256/PG2/ atomic.csv > atomichack.csv

time ../AMD64_LINUX/genpg -skipholes -bits 28 -sv atomichack.sv atomichack.csv

