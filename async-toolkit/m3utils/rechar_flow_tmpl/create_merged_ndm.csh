# Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
# SPDX-License-Identifier: Apache-2.0

create_workspace -technology /p/hdk/cad/pdk/pdk783_r0.8_23ww24.2/apr/synopsys/tech/m14_2x_1xa_1xb_6ya_2yb_2yc__bm5_1ye_1yf_2ga_mim3x_1gb__bumpp/i0s_160h_50pp_tp1/p1278_icc2.tf -scale_factor 4000 myreflib
read_ndm -views {frame design} /nfs/site/disks/zsc9_fwr_lib_char_001/mnystroe/release/007.fwr_pdk0p80_r4v1p0_efv_300mV_cmax_cmin.003/bundles/dsiobsolete_lvt/ndm/lib783_i0s_160h_50pp_dsiobsolete_lvt.ndm
read_ndm -views {frame design} /p/hdk/cad/stdcells/lib783_i0s_160h_50pp/pdk080_r4v1p0_efv/dsiobsolete_lvt/ndm/lib783_i0s_160h_50pp_dsiobsolete_lvt.ndm

read_db -process_label lib783_tttt_tttt_cmax [glob "/p/hdk/cad/stdcells/lib783_i0s_160h_50pp/pdk080_r4v1p0_efv/dsiobsolete_lvt/lib/*tttt_cmax*_ccslnt.ldb"]
read_db -process_label lib783_tttt_tttt_cmax [glob "/nfs/site/disks/zsc9_fwr_lib_char_001/mnystroe/release/007.fwr_pdk0p80_r4v1p0_efv_300mV_cmax_cmin.003/bundles/dsiobsolete_lvt/lib/lib783_i0s_160h_50pp_dsiobsolete_lvt_tttt_*_tttt_cmax_ccslnt.ldb"]
read_db -process_label lib783_tttt_tttt_cmax [glob "/nfs/site/disks/zsc9_fwr_lib_char_001/mnystroe/release/012.fwr_supplemental/bundles/dsiobsolete_lvt/lib/lib783_i0s_160h_50pp_dsiobsolete_lvt_tttt_*_tttt_cmax_ccslnt.ldb"]

read_db -process_label lib783_tttt_tttt_cmin [glob "/p/hdk/cad/stdcells/lib783_i0s_160h_50pp/pdk080_r4v1p0_efv/dsiobsolete_lvt/lib/*tttt_cmin*_ccslnt.ldb"]
read_db -process_label lib783_tttt_tttt_cmin [glob "/nfs/site/disks/zsc9_fwr_lib_char_001/mnystroe/release/007.fwr_pdk0p80_r4v1p0_efv_300mV_cmax_cmin.003/bundles/dsiobsolete_lvt/lib/lib783_i0s_160h_50pp_dsiobsolete_lvt_tttt_*_tttt_cmin_ccslnt.ldb"]
read_db -process_label lib783_tttt_tttt_cmin [glob "/nfs/site/disks/zsc9_fwr_lib_char_001/mnystroe/release/012.fwr_supplemental/bundles/dsiobsolete_lvt/lib/lib783_i0s_160h_50pp_dsiobsolete_lvt_tttt_*_tttt_cmin_ccslnt.ldb"]

read_db -process_label lib783_rcff_pcff_cmin [glob "/p/hdk/cad/stdcells/lib783_i0s_160h_50pp/pdk080_r4v1p0_efv/dsiobsolete_lvt/lib/*rcff*pcff*_ccslnt.ldb"]
read_db -process_label lib783_rcff_pcff_cmin [glob "/nfs/site/disks/zsc9_fwr_lib_char_001/mnystroe/release/007.fwr_pdk0p80_r4v1p0_efv_300mV_cmax_cmin.003/bundles/dsiobsolete_lvt/lib/lib783_i0s_160h_50pp_dsiobsolete_lvt*rcff*pcff*ccslnt.ldb"]
read_db -process_label lib783_rcff_pcff_cmin [glob "/nfs/site/disks/zsc9_fwr_lib_char_001/mnystroe/release/012.fwr_supplemental/bundles/dsiobsolete_lvt/lib/lib783_i0s_160h_50pp_dsiobsolete_lvt*rcff*pcff*ccslnt.ldb"]

read_db -process_label lib783_rcss_pcss_cmax [glob "/p/hdk/cad/stdcells/lib783_i0s_160h_50pp/pdk080_r4v1p0_efv/dsiobsolete_lvt/lib/*rcss*pcss*_ccslnt.ldb"]
read_db -process_label lib783_rcss_pcss_cmax [glob "/nfs/site/disks/zsc9_fwr_lib_char_001/mnystroe/release/007.fwr_pdk0p80_r4v1p0_efv_300mV_cmax_cmin.003/bundles/dsiobsolete_lvt/lib/lib783_i0s_160h_50pp_dsiobsolete_lvt*rcss*pcss*ccslnt.ldb"]
read_db -process_label lib783_rcss_pcss_cmax [glob "/nfs/site/disks/zsc9_fwr_lib_char_001/mnystroe/release/012.fwr_supplemental/bundles/dsiobsolete_lvt/lib/lib783_i0s_160h_50pp_dsiobsolete_lvt*rcss*pcss*ccslnt.ldb"]

#read_db -process_label lib783_rfff_tttt_cmin [glob "/p/hdk/cad/stdcells/lib783_i0s_160h_50pp/pdk080_r4v1p0_efv/dsiobsolete_lvt/lib/*rfff*tttt*_ccslnt.ldb"]
read_db -process_label lib783_rfff_tttt_cmin [glob "/nfs/site/disks/zsc9_fwr_lib_char_001/mnystroe/release/007.fwr_pdk0p80_r4v1p0_efv_300mV_cmax_cmin.003/bundles/dsiobsolete_lvt/lib/lib783_i0s_160h_50pp_dsiobsolete_lvt*rfff*tttt*ccslnt.ldb"]
read_db -process_label lib783_rfff_tttt_cmin [glob "/nfs/site/disks/zsc9_fwr_lib_char_001/mnystroe/release/012.fwr_supplemental/bundles/dsiobsolete_lvt/lib/lib783_i0s_160h_50pp_dsiobsolete_lvt*rfff*tttt*ccslnt.ldb"]

read_db -process_label lib783_tttt_tttt_ctyp [glob "/p/hdk/cad/stdcells/lib783_i0s_160h_50pp/pdk080_r4v1p0_efv/dsiobsolete_lvt/lib/*tttt_ctyp*_ccslnt.ldb"]
read_db -process_label lib783_tttt_tttt_ctyp [glob "/nfs/site/disks/zsc9_fwr_lib_char_001/mnystroe/release/007.fwr_pdk0p80_r4v1p0_efv_300mV_cmax_cmin.003/bundles/dsiobsolete_lvt/lib/lib783_i0s_160h_50pp_dsiobsolete_lvt*tttt*ctyp*_ccslnt.ldb"]
read_db -process_label lib783_tttt_tttt_ctyp [glob "/nfs/site/disks/zsc9_fwr_lib_char_001/mnystroe/release/012.fwr_supplemental/bundles/dsiobsolete_lvt/lib/lib783_i0s_160h_50pp*tttt*ctyp*ccslnt.ldb"]


check_workspace -allow_missing
commit_workspace -output /nfs/site/disks/zsc9_fwr_sd_001/mostafab/PESG_char_test2/siliconsmart/lib783_i0s_160h_50pp_dsiobsolete_lvt.ndm


#create_workspace -technology /p/hdk/cad/pdk/pdk783_r0.8_23ww24.2/apr/synopsys/tech/m14_2x_1xa_1xb_6ya_2yb_2yc__bm5_1ye_1yf_2ga_mim3x_1gb__bumpp/i0s_160h_50pp_tp1/p1278_icc2.tf -scale_factor 4000 myreflib
#read_ndm -views {design} /nfs/site/disks/zsc9_fwr_lib_char_001/mnystroe/release/007.fwr_pdk0p80_r4v1p0_efv_300mV_cmax_cmin.003/bundles/dsiobsolete_lvt/ndm/lib783_i0s_160h_50pp_dsiobsolete_lvt.ndm
#read_db -process_label lib783_tttt_tttt_cmax [glob "/nfs/site/disks/zsc9_fwr_lib_char_001/mnystroe/release/012.fwr_supplemental/bundles/dsiobsolete_lvt/lib/lib783_i0s_160h_50pp_dsiobsolete_lvt_tttt_0p300v_100c_tttt_cmax_ccslnt.ldb"]
#check_workspace -allow_missing
#commit_workspace -output /nfs/site/disks/zsc9_fwr_sd_001/mostafab/PESG_char_test2/siliconsmart/lib783_i0s_160h_50pp_dsiobsolete_lvt_rechar3.ndm
