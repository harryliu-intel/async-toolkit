#!/bin/sh

if   [ "X$CM3"       != "X" ]; then
	echo 1
elif [ "X$GC_USECM3" != "X" ]; then 
	echo 1
elif [ -f ".USECM3" ]; then
	echo 1
fi
