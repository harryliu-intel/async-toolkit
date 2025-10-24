#!/bin/sh 

rdls=*.rdl

for f in ${rdls}; do
#	echo checking ${f}
	grep ${f} *.rdl > /dev/null
	st=$?
#	echo $st
	if [ "$st" -ne "0" ]; then
		echo NOT FOUND: ${f} 
	fi
done
