#!/bin/sh -x
# Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
# SPDX-License-Identifier: Apache-2.0


target=cell_list.latches
src=seq
thresh_list="ulvt lvt"
pfx=i0sl

rm ${target}

for th in $thresh_list; do
    echo doing $th
    bundle=${src}_${th}
    
    pushd /p/hdk/cad/stdcells/lib783_i0s_160h_50pp/pdk050_r3v2p0_efv/${bundle}/spf/lib783_i0s_160h_50pp_${bundle}_100c_tttt_ctyp/

    cells=`echo ${pfx}*.spf | sed 's/\.spf//g'`

    popd
    
    echo cells : $cells

    for c in $cells; do
        echo ${bundle} ${c} >> ${target}
    done

done
