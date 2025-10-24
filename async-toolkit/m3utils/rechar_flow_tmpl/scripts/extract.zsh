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
    dir=$stdcell_dir/$bundle/lib

    grep "^${bundle}" $cell_list | awk '{print $2}' > ${bundle}.cells

    cmd="siliconsmart ${0:h}/extract_cell.tcl $dir/${fulllib}_${bundle}_${protocorner}.lib.gz ${fulllib}_${bundle}_${protocorner}.lib ${bundle}.cells"

    if    [[ -z $internal ]]; then
        echo "CMD:`pwd`:$cmd" >> $sislaunchfile
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
