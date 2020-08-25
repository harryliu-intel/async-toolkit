#!/bin/sh -x

PROG=../AMD64_LINUX/mismatch

${PROG} -K 1 -N 2 -H 15 -trunc 3 -o hist_k1_t3.dat

${PROG} -K 1 -N 2 -H 15 -trunc 2 -o hist_k1_t2.dat

${PROG} -K 2 -N 2 -H 15 -trunc 3 -o hist_k2_t3.dat

${PROG} -K 2 -N 2 -H 15 -trunc 2 -o hist_k2_t2.dat

${PROG} -K 3 -N 2 -H 15 -trunc 3 -o hist_k3_t3.dat

${PROG} -K 3 -N 2 -H 15 -trunc 2 -o hist_k3_t2.dat

${PROG} -K 5 -N 2 -H 15 -trunc 3 -o hist_k3_t3.dat

${PROG} -K 5 -N 2 -H 15 -trunc 2 -o hist_k5_t2.dat

${PROG} -K 5 -N 2 -H 15 -trunc 10 -o hist_k5_t10.dat



