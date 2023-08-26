# common environment settings for Siliconsmart characterization runs
# source into master script

[[ -f $cell_list ]] || { echo "Please ensure cell_list exists"; exit 1 }

echo "cell_list : " $cell_list

[[ -n $CTH_SESSION_ID ]] || { echo "Please run from inside Cheetah"; exit 1 }
[[ -n $SNPSLMD_LICENSE_FILE || -n $LM_PROJECT ]] || { echo "Licenses not configured"; exit 1 }
[[ -n $NBQSLOT ]] || { echo "NetBatch not configured"; exit 1 }
[[ -n $NBPOOL ]] || { echo "NetBatch not configured"; exit 1 }

# PDK 0p5 release:
#export stdcell_dir=/p/hdk/cad/stdcells/lib783_i0s_160h_50pp/pdk050_r3v2p0_efv
#
# PDK 0p8 1st release:
#export stdcell_dir=/p/hdk/cad/stdcells/lib783_i0s_160h_50pp/pdk080_r4v0p0_efv
# 
# PDK 0p8 2nd release:
export stdcell_dir=/p/hdk/cad/stdcells/lib783_i0s_160h_50pp/pdk080_r4v1p0_efv

# override siliconsmart location for transistor types
export SILICONSMART_ROOT_DIR=`pwd`/siliconsmart_overrides-22.06.06_rc

export techlib=lib783
export stdlibname=i0s_160h_50pp
export fulllib=${techlib}_${stdlibname}
export wordy=78dot3

bundles=($(awk '{print $1}' $cell_list | sort -u))
echo "bundles are " $bundles

