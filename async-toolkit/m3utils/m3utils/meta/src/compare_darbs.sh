#!/bin/sh

a=$1
b=$2

echo REFERENCE $a
echo FAILING   $b
echo
grep RESULT ${a}.scm.out
grep RESULT ${b}.scm.out 
echo
grep EARLY-SLEW ${a}.scm.out
grep EARLY-SLEW ${b}.scm.out 
echo

grep EARLY-PREDECESSORS ${a}.scm.out | sed s/EARLY-PREDECESSORS// > /tmp/short.a$$
grep EARLY-PREDECESSORS ${b}.scm.out | sed s/EARLY-PREDECESSORS// > /tmp/short.b$$

grep LATE-PREDECESSORS ${a}.scm.out | sed s/LATE-PREDECESSORS// > /tmp/long.a$$
grep LATE-PREDECESSORS ${b}.scm.out | sed s/LATE-PREDECESSORS// > /tmp/long.b$$

echo "SHORT PATH COMPARISON:"

~/meta/meta/src/compare-lists.sh /tmp/short.a$$ /tmp/short.b$$


echo
echo 
echo 

echo "LONG PATH COMPARISON:"

~/meta/meta/src/compare-lists.sh /tmp/long.a$$  /tmp/long.b$$
