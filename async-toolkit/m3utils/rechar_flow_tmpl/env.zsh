#
# env.zsh
#
# common environment settings for Siliconsmart characterization runs
# source into master script
#
# Author: mika.nystroem@intel.com
# 2023
#

#
# configure environment
#

#
# the technology:
#
export techlib=lib783               # P1278.3
export wordy=78dot3                 # used by numerous Intel libs

######################################################################
#
# the specific std cell library we are recharacterizing:
#
#export stdlibname=i0s_160h_50pp    # i0s is the short library
export stdlibname=i0m_180h_50pp     # i0m the tall library

#
# the specific PDK version:
#
# PDK 0p5 release:
#export pdk_version=pdk050_r3v2p0_efv
#
# PDK 0p8 1st release:
#export pdk_version=pdk080_r4v0p0_efv
# 
# PDK 0p8 2nd release:
export pdk_version=pdk080_r4v1p0_efv


# 
# set up prototype corner, from which we rechar:
#
export protocorner=tttt_0p550v_100c_tttt_cmax_ccslnt

#
# configure parallelism in run:
#
workers=2000 # workers to launch via SIS
pllcmds=40   # parallel commands to run through sislaunch (mainly for extract.zsh)

############################################################################
##############   SHOULDN'T HAVE TO CHANGE ANYTHING BELOW HERE ##############   
############################################################################

# 
# basic startup checks...
#

[[ -f $cell_list ]] || { echo "Please ensure cell_list exists"; exit 1 }

echo "cell_list : " $cell_list

[[ -n $CTH_SESSION_ID ]] || { echo "Please run from inside Cheetah"; exit 1 }
[[ -n $SNPSLMD_LICENSE_FILE || -n $LM_PROJECT ]] || { echo "Licenses not configured"; exit 1 }
[[ -n $NBQSLOT ]] || { echo "NetBatch not configured"; exit 1 }
[[ -n $NBPOOL ]] || { echo "NetBatch not configured"; exit 1 }

#
# varous variable settings
#

export fulllib=${techlib}_${stdlibname}

export stdcell_dir=/p/hdk/cad/stdcells/${fulllib}/${pdk_version}

# override siliconsmart location for transistor types
export SILICONSMART_ROOT_DIR=`pwd`/siliconsmart_overrides-22.06.06_rc

bundles=($(awk '{print $1}' $cell_list | sort -u))
echo "bundles are $bundles"

# job control follows:
# control how jobs are launched.
# internal means this script does it (else, sislaunch does it)
# serial means do the bundles one at a time
# to set option, set it to anything but ""
# to clear option, set it to ""
internal=""
serial=1
sislaunchfile="jobs.dat"
sislaunch=${M3UTILS}/spice/sislaunch/AMD64_LINUX/sislaunch
sispath=`which siliconsmart`

if [[ -z $internal ]]; then
    if [[ -z $M3UTILS ]]; then
        "Must set M3UTILS to use sislaunch program"
        exit 1
    fi
    if [[ ! -x $sislaunch ]]; then
        echo "sislaunch program $sislaunch does not exist"
        exit 1
    fi
fi

export internal serial sislaunchfile sislaunch workers pllcmds sispath
