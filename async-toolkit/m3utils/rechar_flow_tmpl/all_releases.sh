#!/usr/bin/zsh
#
#

releases=(007.fwr_pdk0p80_r4v1p0_efv_300mV_cmax_cmin.003 012.fwr_supplemental 014.fwr_2023_09_30 015.i0slsn210aa1d03x5 016.fwr_2023_10_06 017.i0sfvz803)

rm -f all_cells.txt

for r in $releases; do
	zgrep 'cell(*i0s' /nfs/site/disks/zsc9_fwr_lib_char_001/mnystroe/release/${r}/bundles/*/lib/*_tttt_0p300v_100c_tttt_cmax_ccslnt.lib.gz | sed 's/^.*(\(i0s[0-9a-z]*\)).*$/\1/' >> all_cells.txt
done

find_cells.sh all_cells.txt

copy_thresholds.sh resultcells.dat
