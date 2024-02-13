#!/usr/intel/bin/zsh

# should source appropriate sourceme file first

source rechar_setup_env.zsh

files=(${RECHAR_SIS_WORKDIR}/*_*/*vt/siliconsmart.log)

rm -f lastlog$$

for file in ${files}; do

    last=`tail -1 $file`

    echo "$file $last" >> lastlog$$
    
done

filecount=`wc -l < lastlog$$`
donecount=`grep -c '^Maximum virtual' lastlog$$`
progcount=`grep -c '%.*Failed.*Elapsed' lastlog$$`

echo "done       $donecount"
echo "inprogress $progcount"
echo "total      `expr $donecount + $progcount`"
echo "files      $filecount"

busyworkers=`grep Failed lastlog$$ | sed 's/.*Active Workers: \([0-9]*\),.*/\1/' | awk '{x += $1} END { print x }'`

echo "busy workers   $busyworkers"
echo "target workers $workers"

grep Failed lastlog$$ | sed 's/\([^ ]*\)\/siliconsmart\.log .*(\([0-9\.]*\)%,.*Active Workers: \([0-9]*\),.*/\1 \2 \3/' | awk '{cost = $2 * $3; rem = 100 - $2; remcost = rem * cost; printf("%s %f %f %f %f %f\n", $1, $2, $3, cost, rem, remcost)}' > cost$$

sumcost=`awk '{sum += $NF} END {print sum}' cost$$`
echo "Cost sum is $sumcost"

rm -f rebalance.sh
echo "#!/bin/sh -x" > rebalance.sh
awk -v sumcost=${sumcost} -v workers=${workers} '{printf("echo \"change_parameter run_list_maxsize %d\" > %s/config/change.tcl\n", $6 / sumcost * (workers - 1) + 1, $1)}' cost$$ >> rebalance.sh
chmod +x rebalance.sh

cat rebalance.sh

echo "Run ./rebalance.sh , if you dare!"

rm -f cost$$
rm -f lastlog$$



