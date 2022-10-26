#!/bin/sh

ls *.run/measure.dat | awk -F. '{print $1}' > m &
ls *.sh | awk -F. '{print $1}' > s &
wait
cat s s m | sort | uniq -c | sort -gr | grep " 2 " | awk '{print $2}' | sort -g > r
