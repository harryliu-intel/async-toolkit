#!/bin/sh
# make-report <scriptdir> <outdir> <runname>

scriptdir=$1
outdir=$2
report=$3

spec=${scriptdir}/${report}.spec
meta=${scriptdir}/${report}.meta

echo     "UNIQUE-INSTANCE " $report ">>>>>"
echo
echo     "INSTANCE-INFORMATION    ===="
cat $meta
echo
echo

results=`find ${outdir}/${report} -name properties -print`

#echo $results

if [ "X$results" = "X" ]; then
		echo '****************************** NO RESULTS FOUND ******************************'
		echo '****************************** NO RESULTS FOUND ******************************'
		echo '****************************** NO RESULTS FOUND ******************************'
else
		echo "RESULTS    ================="
		cat $results
fi
echo "INSTANCE-SPEC   ============"
cat $spec
echo "<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<"
