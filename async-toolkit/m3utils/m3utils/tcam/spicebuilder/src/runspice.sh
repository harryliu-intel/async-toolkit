#!/bin/sh -x

# runspice.sh
#  <run directory>
#  <instance subdirectory>
#  [-memoize <memoize-dir>]
#  <simulator>
#  [-deck <deck-name>]
# <spicebuilder-opts...>

# important note: "-f <simulator>" must be part of <spicebuilder-opts>, else
# sim definitely wont work!

failexit()
{
    status=$1
    source=$2
    echo $source "failed"
    touch DONE
    exit $status
}

setproc()
{
    proc=$1

    if   [ "$proc" == "1273" ]; then
        hspice_lib_models=/p/hdk/pu_tu/prd/kits_p1273/15.3.1.2/models/cmi/synopsys_lynx/cmi_lib/suse_64
        export hspice_lib_models
        hspice_model_root=/p/hdk/pu_tu/prd/kits_p1273/15.3.1.2/models/hspice
        export hspice_model_root
        hspice_model=p1273_1x1r3.hsp
        export hspice_model
	SCALE_TIME="--scale-time 1e-6"
    elif [ "$proc" == "1274" ]; then
#        hspice_lib_models=/p/hdk/cad/process/p1274.2_sim/p1274.2_15ww17.1p1/cmi/14.4.0/synopsys/suse64
#        hspice_model_root=/p/hdk/cad/process/p1274.2_sim/p1274.2_15ww17.1p1/hsp/
#        hspice_model=p1274_0x1r1.hsp
	hspice_lib_models=/p/hdk/cad/process/p1274.3_sim/p1274.3x2r1_16ww40.4/cmi/16.1.P2/synopsys/suse64/
	hspice_model_root=/p/hdk/cad/process/p1274.3_sim/p1274.3x2r1_16ww40.4/hsp/
	hspice_model=p1274_3x2r1.hsp
	SCALE_TIME="" # from the L version of xa

        export hspice_lib_models
        export hspice_model_root
        export hspice_model
    else
        failexit "unknown process $1"
    fi
}

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

memoize=0
case "$1" in
    -memoize)
	shift
	pdir=../../$1
	shift
  	memoize=1
        ;;
    *)
        ;;
esac

sim=$1
shift
understood=1

echo "args" $*

PM="-pm io"
XP="-extractpath tcam.sp"
DESIGN=""

while [[ $understood -eq 1 ]]; do
case "$1" in
    -deck)
        shift
        deck=$1
        shift
        SRCDIR=/nfs/sc/disks/hlp_0015/mnystroe/tcam
        PFX=tcam_${deck}
        ;;

    -srcdir)
        shift
        SRCDIR=$1
        shift
        ;;

    -pfx)
        shift
        PFX=$1
        shift
        ;;

    -prog)
        shift
        PROG=$1
        shift
        ;;

    -design)
        shift
        DESIGN="-design $1"
        shift
        ;;

    -f)  # ignore second sim spec
        shift
        shift
        ;;

    -pm)
        shift
        PM="-pm $1"
        shift
        ;;

    -extractpath)
        shift
        XP="-extractpath $1"
        shift
        ;;

    -step)
        shift
        step=$1
        shift
        ;;

    -p1274)
        shift
        setproc 1274
        ;;

    -p1273)
        shift
        setproc 1273
        ;;

    -proc)
        shift
        setproc $1
        shift
        ;;
    
    *)
        understood=0
esac
done


SPFILE=${PFX}.sp

echo memoize ${memoize}

if [ "test" = "NOtest" ]; then
    # example problem: make y=x^2 curve!
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

	which spicebuilder
        echo spicebuilder -prog ${PROG} ${PM} ${XP} ${DESIGN} -step ${step} -f xa $* 
             spicebuilder -prog ${PROG} ${PM} ${XP} ${DESIGN} -step ${step} -f xa $*  > spicebuilder.log 2>&1 || failexit $? "spicebuilder"
        
	if [ "${memoize}" = "0" ]; then
	        xa out.spice || failexit $? "xa"
       	 	convert_trace ${SCALE_TIME} --time-step ${step} --fsdb xa.fsdb --translate out || failexit $? "convert_trace"
        	rm xa.fsdb
        fi
    elif [ "${sim}" = "hspice" ]; then
        spice2spice --rename=1 ${SPFILE} tcam.sp
        echo spicebuilder -prog ${PROG} ${PM} -rename -runlvl 1 ${XP} -step ${step} -f hspice $*
        spicebuilder -prog ${PROG} ${PM} -rename -runlvl 1 ${XP} -step ${step} -f hspice $*  > spicebuilder.log 2>&1 
	if [ "${memoize}" = "0" ]; then
	        hspice -64 out.spice > hspice.log 2>&1 || failexit $? "hspice"
        	ct -rename x1 out.tr0 out > ct.log 2>&1 || failexit $? "ct"
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
        
    asserter out out.ass > asserter.out 2>&1 # no failexit here, asserter exits with status 1 on mismatch
    tail -1 asserter.out > result
    ln -sf ${NBDIR}/result ../result
    ln -sf ${NBDIR}/asserter.measure ../asserter.measure
    touch ../../done/$md
    bzip2 *.log *.out
    if [ "${memoize}" = "1" ]; then
   	/bin/rm -f out.trace *.sp *.names 
    else
    	bzip2 out.trace
    fi
    touch DONE
fi

