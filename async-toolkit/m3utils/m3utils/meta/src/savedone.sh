#!/bin/sh

for f in *.out; do
	root=`echo $f | sed s/.scm.out//`
	echo ============ $root ============ 
	find . ../scripts -name ${root}\* -exec mv {} ../done \;

done
