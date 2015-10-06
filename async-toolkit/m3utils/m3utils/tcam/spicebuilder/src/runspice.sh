#!/bin/sh -x

echo "PATH=${PATH}"      >  $1/$2/runspice.log
echo "HOST=`hostname`"   >> $1/$2/runspice.log

rd=$1
md=$2
sim=$3

cd $1/$2

echo $0 $* 

shift
shift
shift
export DEBUGLEVEL=10

step=10e-12

SRCDIR=/p/hlp/thkoo1/lion/tcam/inway7_extraction/ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tcam_512_40_4000_rcx/
PFX=ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tcam_512_40_4000

# example problem: make y=x^2 curve!
if [ "test" = "NOtest" ]; then
    echo ${2} ${8} ${10}

    echo ${8} ${10} ${2} | awk '{a=$1; b=$2; t=$3;\
    x=(a-.75)/.10;\
    y=(b-1e9)/0.01e9;\
    print (y>x*x+t/100) ? "PASS" : "FAIL"; }'  > result
else
    grep -v '^\.PARAM[^A-Za-z0-9]*$' ${SRCDIR}/${PFX}.sp > ${PFX}.sp

    pwd

    if   [ "${sim}" = "xa" ]; then
        cp ${PFX}.sp tcam.sp
        echo spicebuilder -extractpath tcam.sp -step ${step} $* 
        spicebuilder -extractpath tcam.sp -step ${step} $*  > spicebuilder.log 2>&1 
        xa out.spice
        convert_trace --scale-time 1e-6 --time-step ${step} --fsdb xa.fsdb --translate out
    elif [ "${sim}" = "hspice" ]; then
        spice2spice --rename=1 ${PFX}.sp tcam.sp
        echo spicebuilder -rename -runlvl 1 -extractpath tcam.sp -step ${step} $*
        spicebuilder -rename -runlvl 1 -extractpath tcam.sp -step ${step} $*  > spicebuilder.log 2>&1 
        hspice -64 out.spice > hspice.log 2>&1
        ct -rename x1 out.tr0 out > ct.log 2>&1
    else
        echo "unknown simulator ${SIM}"
        exit 1
    fi
        
    asserter out out.ass > asserter.out 2>&1
    tail -1 asserter.out > result
fi

touch ../done/$md
