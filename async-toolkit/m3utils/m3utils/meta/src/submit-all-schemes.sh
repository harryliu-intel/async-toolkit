#!/bin/sh
chmod +x *.script

for script in *.script; do
	qsub -p -400 -l a=lx24-amd64,mem=4G,centos=5 -now no $script
	#sleep 30
done
