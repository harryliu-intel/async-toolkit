#!/bin/sh

# munge through nb.out and look for glitchy nodes

files=`grep -i glitches nb.out/* | \
       grep -v No | \
       awk -F: '{printf "%s:%s:%s ",$1,$2,$3}' |\
       sort | uniq`

#echo $files

for f in ${files}; do
    echo -n $f " "
    grep "START GLITCH SEARCH" $f | awk '{print $4}'
done
