#!/usr/intel/bin/zsh
cell_list=$1
shift
if [[ $# -eq 0 ]]; then
    bundles=($(awk '{print $1}' $cell_list |sort -u))
else
    bundles=("$@")
fi

for bundle in $bundles; do
#	siliconsmart ${0:h}/extract_cell.tcl $stdcell_dir/$bundle/lib/lib764_g1i_210h_50pp_${bundle}_tttt_0p550v_100c_tttt_cmax_ccslnt.lib.gz lib764_g1i_210h_50pp_${bundle}_tttt_0p550v_100c_tttt_cmax_ccslnt.lib =(grep "^$bundle" $cell_list | awk '{print $2}')

    dir=$stdcell_dir/$bundle/lib

    
#    siliconsmart ${0:h}/extract_cell.tcl $dir/lib764_g1i_210h_50pp_${bundle}_tttt_0p550v_100c_tttt_cmax_ccslnt.lib.gz lib764_g1i_210h_50pp_${bundle}_tttt_0p550v_100c_tttt_cmax_ccslnt.lib =(grep "^$bundle" $cell_list | awk '{print $2}')
    siliconsmart ${0:h}/extract_cell.tcl $dir/${fulllib}_${bundle}_tttt_0p550v_100c_tttt_cmax_ccslnt.lib.gz ${fulllib}_${bundle}_tttt_0p550v_100c_tttt_cmax_ccslnt.lib =(grep "^$bundle" $cell_list | awk '{print $2}')
    
done
