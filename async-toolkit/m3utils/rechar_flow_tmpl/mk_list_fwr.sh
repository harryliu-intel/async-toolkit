#!/bin/sh -x

# take all the cells in ${target}.cells and construct a cell_list

target=fwr
src_list="base ldrdsicore dsicore ldrseq edrseq seq supseq dsiseq ldrclk clk spcl supclk supbase ldrbase edrbase ldrsupbase ldrsupseq dsiobsolete"
thresh_list="ulvt lvt"
pfx=i0s
rev=pdk080_r4v0p0_efv

rm cell_list.${target}

for src in $src_list; do
for th in $thresh_list; do
    echo doing $th
    bundle=${src}_${th}
    
    d=/p/hdk/cad/stdcells/lib783_i0s_160h_50pp/${rev}/${bundle}/spf/lib783_i0s_160h_50pp_${bundle}_100c_tttt_cmax/

    ls $d | sed 's/\.spf//' > ${bundle}.cells
    cat ${bundle}.cells ${target}.cells | sort | uniq -c | sort -g | grep '^ *2 *' | awk '{print $2}'> ${target}__${bundle}.cells

    for cell in `cat ${target}__${bundle}.cells`; do
        echo ${bundle} ${cell} >> cell_list.${target}
    done
done
done

echo "=====  MISSING CELLS  ====="

awk '{print $2}' cell_list.${target} > c2.${target}
cat c2.${target} c2.${target} ${target}.cells | sort | uniq -c | sort -g | grep -v " 3 "

cat >> cell_list.${target} << EOF
spcl_lvt i0sddtih0ab1d02x5
spcl_lvt i0sddtil0ab1d02x5
EOF
