#!/bin/sh
chmod +x *.script

. `dirname $0`/settings.sh

echo JAVAMEM=$JAVAMEM
echo SUBMITMEM=$SUBMITMEM

for script in *.script; do
	qsub -p -400 -l a=lx24-amd64,mem=${SUBMITMEM} -now no $script
	#sleep 30
done
