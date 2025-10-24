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
    
ls -l ${RECHAR_SIS_WORKDIR}/bundles/*/lib/*${p}.lib.gz | grep -v ^l | awk '{print $9}' | xargs -n1 zgrep 'cell(i0s' | sed 's/^.*cell(\(i0s[a-z0-9]*\)).*$/\1/' | sort | uniq > ${p}_generated.cells
awk '{print $2}' ${RECHAR_CELL_LIST_FN} | sort | uniq > required.cells

cat required.cells required.cells ${p}_generated.cells | sort | uniq -c | grep " 1 " > ${p}_gen_but_not_req.cells
cat required.cells required.cells ${p}_generated.cells | sort | uniq -c | grep " 2 " > ${p}_req_but_not_gen.cells

gen_cnt=`wc -l ${p}_generated.cells`
req_cnt=`wc -l required.cells`
gnr_cnt=`wc -l ${p}_gen_but_not_req.cells`
rng_cnt=`wc -l ${p}_req_but_not_gen.cells`

echo "required cells             ${req_cnt}"
echo "generated cells            ${gen_cnt}"
echo "generated but not required ${gnr_cnt}"
echo "required but not generated ${rng_cnt}"
done

