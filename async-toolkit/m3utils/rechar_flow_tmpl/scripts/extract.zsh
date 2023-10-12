#!/usr/intel/bin/zsh -x

cell_list=$1
shift
if [[ $# -eq 0 ]]; then
    bundles=($(awk '{print $1}' $cell_list |sort -u))
else
    bundles=("$@")
fi

rm -f $sislaunchfile
echo "STDCELLDIR:"${stdcell_dir} > $sislaunchfile
echo "internal=$internal"

for bundle in $bundles; do
#	siliconsmart ${0:h}/extract_cell.tcl $stdcell_dir/$bundle/lib/lib764_g1i_210h_50pp_${bundle}_tttt_0p550v_100c_tttt_cmax_ccslnt.lib.gz lib764_g1i_210h_50pp_${bundle}_tttt_0p550v_100c_tttt_cmax_ccslnt.lib =(grep "^$bundle" $cell_list | awk '{print $2}')

    dir=$stdcell_dir/$bundle/lib

    #    siliconsmart ${0:h}/extract_cell.tcl $dir/lib764_g1i_210h_50pp_${bundle}_tttt_0p550v_100c_tttt_cmax_ccslnt.lib.gz lib764_g1i_210h_50pp_${bundle}_tttt_0p550v_100c_tttt_cmax_ccslnt.lib =(grep "^$bundle" $cell_list | awk '{print $2}')
    grep "^${bundle}" $cell_list | awk '{print $2}' > ${bundle}.cells

    cmd="siliconsmart ${0:h}/extract_cell.tcl $dir/${fulllib}_${bundle}_tttt_0p550v_100c_tttt_cmax_ccslnt.lib.gz ${fulllib}_${bundle}_tttt_0p550v_100c_tttt_cmax_ccslnt.lib ${bundle}.cells"

    if    [[ -z $internal ]]; then
        echo "CMD:$cmd" >> $sislaunchfile
    else
        eval $cmd
    fi
done

if [[ -z $internal ]]; then
    date
    pwd
    $sislaunch -pllcmds $pllcmds $sislaunchfile || exit 1
    echo "status $status"
fi
