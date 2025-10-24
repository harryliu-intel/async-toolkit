# 1276.4 spice models with PDX installation paths
setenv hsp_dir /p/hdk/cad/pdk/pdk764_r0.4HP3_22ww20.1
setenv hspice_lib_models $hsp_dir/cmi/hspice/cmi/lnx86/64bit
setenv PDMI_LIB $hsp_dir/cmi/hspice/pdmi/lnx86/64bit/pdmi.so
setenv hspice_model_root $hsp_dir/models/core/hspice/m17_6x_2ya_2yb_2yc_2yd_1ye_1ga_mim3x_1gb__bumpp
setenv hspice_model p1276_4.hsp
$*
