#!/bin/sh 
#

# summarize optimization sim data into results files
# run BEFORE graph.sh

for dir in *p?; do
	echo summarizing $dir
	cat $dir/00*/result.csv > result_${dir}.csv &
done	

echo waiting
wait
