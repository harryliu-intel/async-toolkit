#!/bin/sh

DBITS=$1

OPTS=-syntactic
#OPTS=-structural
MAXFANIN=4

rm -rf output
mkdir output

AMD64_LINUX/buildecc -maxfanin ${MAXFANIN} ${OPTS} -range 1 299 output

