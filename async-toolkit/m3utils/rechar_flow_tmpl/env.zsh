# common environment settings for Siliconsmart characterization runs
# source into master script

[[ -f $cell_list ]] || { echo "Please ensure cell_list exists"; exit 1 }

echo "cell_list : " $cell_list

[[ -n $CTH_SESSION_ID ]] || { echo "Please run from inside Cheetah"; exit 1 }
[[ -n $SNPSLMD_LICENSE_FILE || -n $LM_PROJECT ]] || { echo "Licenses not configured"; exit 1 }
[[ -n $NBQSLOT ]] || { echo "NetBatch not configured"; exit 1 }
[[ -n $NBPOOL ]] || { echo "NetBatch not configured"; exit 1 }

#export stdcell_dir=/p/hdk/cad/stdcells/lib783_i0s_160h_50pp/pdk050_r3v2p0_efv
export stdcell_dir=/p/hdk/cad/stdcells/lib783_i0s_160h_50pp/pdk080_r4v0p0_efv
export stdcell_dir=/p/hdk/cad/stdcells/lib783_i0s_160h_50pp/pdk080_r4v1p0_efv

# override siliconsmart location for transistor types
#export SILICONSMART_ROOT_DIR=/nfs/site/disks/zsc3_fin_data_share/rliu68/siliconsmart_overrides-22.06.06_rc
#export SILICONSMART_ROOT_DIR=/nfs/site/disks/zsc3_fon_fe_0001/mnystroe/rechar_flow.1278/siliconsmart_overrides-22.06.06_rc
export SILICONSMART_ROOT_DIR=/nfs/site/disks/zsc9_fwr_sd_001/mnystroe/rechar_flow.1278_lv/siliconsmart_overrides-22.06.06_rc

export techlib=lib783
export stdlibname=i0s_160h_50pp
export fulllib=${techlib}_${stdlibname}
export wordy=78dot3

bundles=($(awk '{print $1}' $cell_list | sort -u))
echo "bundles are " $bundles

