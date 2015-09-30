#!/bin/sh

DBITS=$1

MAXFANIN=4 
#OPTS=-syntactic
OPTS=-structural

AMD64_LINUX/buildecc -w -m write_ecc_${DBITS} -d ${DBITS} -maxfanin ${MAXFANIN} ${OPTS} -defs ecc_defs.v
AMD64_LINUX/buildecc -r -m read_ecc_${DBITS}  -d ${DBITS} -maxfanin ${MAXFANIN} ${OPTS}  

