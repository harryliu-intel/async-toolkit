#!/bin/sh
# savefiles.sh <scriptdir> <unique-name>
scriptdir=$1
uniquename=$2
mv ${scriptdir}/${uniquename}.* ${scriptdir}/../done
