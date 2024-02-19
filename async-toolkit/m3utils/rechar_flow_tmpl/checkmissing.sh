#!/usr/intel/bin/zsh

source rechar_setup_env.zsh

# catalog missing cells in a run

pvts=`ls -l ${RECHAR_SIS_WORKDIR}/bundles/*/lib/*.lib.gz | grep -v ^l | awk '{print $9}' | sed 's,.*vt_\([^/_]*_[^/_]*_[^/_]*_[^/_]*_[^/_]*_[^/_]*\)\.lib\.gz,\1,' | sort | uniq`

echo "PVTS expected are: `echo $corners_list | wc -w`"
echo $corners_list

echo "PVTS observed are: `echo $pvts | wc -l`"
echo $pvts

for p in $=pvts; do

    echo "PVT $p :"
    
ls -l ${RECHAR_SIS_WORKDIR}/bundles/*/lib/*${p}.lib.gz | grep -v ^l | awk '{print $9}' | xargs -n1 zgrep 'cell(i0s' | sed 's/^.*cell(\(i0s[a-z0-9]*\)).*$/\1/' | sort | uniq > generated.cells
awk '{print $2}' cell_list | sort | uniq > required.cells

cat required.cells required.cells generated.cells | sort | uniq -c | grep -v " 3 "
done

