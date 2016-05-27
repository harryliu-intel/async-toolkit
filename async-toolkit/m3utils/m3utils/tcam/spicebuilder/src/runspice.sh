#!/bin/sh -x

rd=$1
md=$2

NBDIR=${__NB_JOBID}.nb

rundir=$rd/$md/${NBDIR}
mkdir -p $rundir

echo "PATH=${PATH}"      >  ${rundir}/runspice.log
echo "HOST=`hostname`"   >> ${rundir}/runspice.log


cd ${rundir}

echo $0 $* 

shift
shift
export DEBUGLEVEL=10

step=10e-12


PROG=rw

SRCDIR=/p/hlp/thkoo1/lion/tcam/inway7_extraction/ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tcam_512_40_4000_rcx/
PFX=ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tcam_512_40_4000

if [ "$1" = "-memoize" ]; then
	shift
	pdir=../../$1
	shift
  	memoize=1
else
	memoize=0
fi

sim=$1
shift

if [ "$1" = "-deck" ]; then
    shift
    deck=$1
    shift
    SRCDIR=/nfs/sc/disks/hlp_0015/mnystroe/tcam
    PFX=tcam_${deck}
fi


SPFILE=${PFX}.sp

# example problem: make y=x^2 curve!
if [ "test" = "NOtest" ]; then
    echo ${2} ${8} ${10}

    echo ${8} ${10} ${2} | awk '{a=$1; b=$2; t=$3;\
    x=(a-.75)/.10;\
    y=(b-1e9)/0.01e9;\
    print (y>x*x+t/100) ? "PASS" : "FAIL"; }'  > result
    touch ../../done/$md

else
    grep -v '^\.PARAM[^A-Za-z0-9]*$' ${SRCDIR}/${PFX}.sp > ${PFX}.sp

    pwd

    if   [ "${sim}" = "xa" ]; then
        cp ${SPFILE} tcam.sp
        
        ln -sf ${SRCDIR}/${PFX}.spf ./tcam.spf # wont work for hspice

        echo spicebuilder -prog ${PROG} -pm io -extractpath tcam.sp -step ${step} $* 
        spicebuilder -prog ${PROG} -pm io -extractpath tcam.sp -step ${step} $*  > spicebuilder.log 2>&1 
	if [ "${memoize}" = "0" ]; then
	        xa out.spice
       	 	convert_trace --scale-time 1e-6 --time-step ${step} --fsdb xa.fsdb --translate out
        	rm xa.fsdb
        fi
    elif [ "${sim}" = "hspice" ]; then
        spice2spice --rename=1 ${SPFILE} tcam.sp
        echo spicebuilder -prog ${PROG} -rename -runlvl 1 -extractpath tcam.sp -step ${step} $*
        spicebuilder -prog ${PROG} -rename -runlvl 1 -extractpath tcam.sp -step ${step} $*  > spicebuilder.log 2>&1 
	if [ "${memoize}" = "0" ]; then
	        hspice -64 out.spice > hspice.log 2>&1
        	ct -rename x1 out.tr0 out > ct.log 2>&1
	        rm out.tr0
	fi
    else
        echo "unknown simulator ${SIM}"
        exit 1
    fi

    if [ "${memoize}" = "1" ]; then
	echo "doing memoize"
	while [ ! -f ${pdir}/*/DONE ]; do
		sleep 10
	done
	bzcat ${pdir}/*/out.trace.bz2 > out.trace
	cp ${pdir}/*/out.names .
	echo ${pdir} > PARENT
    fi
        
    asserter out out.ass > asserter.out 2>&1
    tail -1 asserter.out > result
    ln -sf ${NBDIR}/result ../result
    touch ../../done/$md
    bzip2 *.log *.out
    if [ "${memoize}" = "1" ]; then
   	/bin/rm -f out.trace *.sp *.names 
    else
    	bzip2 out.trace
    fi
    touch DONE
fi

