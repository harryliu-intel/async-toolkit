#!/bin/sh -x
# Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
# SPDX-License-Identifier: Apache-2.0


#
# launcher script for netbatch jobs
# here we assume that the jobs themselves exist as shell scripts in ${rundir}
# and they are numbered as ${tasknum}.sh
# and that rundir is ${tasknum}.run
# we further assume that the scripts create (in one way or another) a file
# called measure.dat
#
# we check for the existence of measure.dat before starting the job.
# if the file exists, we do not need to re-run everything.
# This makes the script idempotent.
#
# In order to use this script, your netbatch job should look like so:
#
# nbjob run --log-file <log-path> /path/to/this/launcher.sh <rundir> <start-id> <count>
#
# e.g.,
#
# nbjob run --log-file /nfs/site/disks/zsc3_fon_fe_0001/mnystroe/m3utils/spice/techc/src/nb.run-2022-10-19T15:27:35-07:00/2490.log /nfs/site/disks/zsc3_fon_fe_0001/mnystroe/m3utils/spice/techc/src/launcher.sh /nfs/site/disks/zsc3_fon_fe_0001/mnystroe/m3utils/spice/techc/src/nb.run-2022-10-19T15:27:35-07:00 2490 10
#
# mika.nystroem@intel.com
# October 20, 2022
#

rundir=$1
start=$2
n=$3

tasknum=${start}
limtask=`expr ${start} + ${n}`

date

while [ "${tasknum}" -lt "${limtask}" ]; do
    shfile=${rundir}/${tasknum}.sh
    msfile=${rundir}/${tasknum}.run/measure.dat
    if [ -x "${shfile}" ] && [ ! -f "${msfile}" ]; then
        ${shfile} > ${shfile}.launcher.log 2>&1 &
    fi
    tasknum=`expr ${tasknum} + 1`
    sleep 1
done

wait
