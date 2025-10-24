#!/bin/sh -x
rd=$1
md=$2
cd $1/$2
shift
shift
export DEBUGLEVEL=10

step=10e-12

# example problem: make y=x^2 curve!
if [ "test" = "NOtest" ]; then
    echo ${2} ${8} ${10}

    echo ${8} ${10} ${2} | awk '{a=$1; b=$2; t=$3;\
    x=(a-.75)/.10;\
    y=(b-1e9)/0.01e9;\
    print (y>x*x+t/100) ? "PASS" : "FAIL"; }'  > result
else
    echo spicebuilder $* 
    spicebuilder -step ${step} $*  > spicebuilder.log 2>&1 
    pwd
    SIM=`cat out.simulator`
    if   [ "${SIM}" = "xa" ]; then
        xa out.spice
        convert_trace --scale-time 1e-6 --time-step ${step} --fsdb xa.fsdb --translate out
    elif [ "${SIM}" = "hspice" ]; then
        hspice -64 out.spice
        convert_trace --csdf out.tr0 --translate out
    else
        echo "unknown simulator ${SIM}"
        exit 1
    fi
        
    asserter out out.ass > asserter.out 2>&1
    tail -1 asserter.out > result
fi

touch ../done/$md
