#!/bin/sh -x

rundir=$1
start=$2
n=$3

tasknum=${start}
limtask=`expr ${start} + ${n}`

while [ "${tasknum}" -lt "${limtask}" ]; do
    shfile=${rundir}/${tasknum}.sh
    msfile=${rundir}/${tasknum}.run/measure.dat
    if [ -x "${shfile}" ] && [ ! -f "${msfile}" ]; then
        ${shfile} &
    fi
    tasknum=`expr ${tasknum} + 1`
    sleep 1
done

wait
