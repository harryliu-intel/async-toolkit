# Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
# SPDX-License-Identifier: Apache-2.0


# source this file

set dir=/nfs/site/disks/zsc9_fwr_sd_001/mnystroe/1278_lowvoltage/2023ww29d1/models_core_hspice/m14_2x_1xa_1xb_6ya_2yb_2yc__bm5_1ye_1yf_2ga_mim3x_1gb__bumpp/

set dir=/nfs/site/disks/zsc9_fwr_sd_001/mnystroe/1278_lowvoltage/2023ww29d2/models_core_hspice/1/m14_2x_1xa_1xb_6ya_2yb_2yc__bm5_1ye_1yf_2ga_mim3x_1gb__bumpp/

set corner=tttt

set hspfile=${dir}/p1278_3.hsp
set cornerfile=${hspfile}:${corner}

setenv PROJECT_HSP_FILE     ${hspfile}
setenv DP_HSPICE_MODEL      ${hspfile}
setenv COMMANDER_SIM_SP_INC ${cornerfile}
setenv PROJECT_SIM_SP_INC   ${cornerfile}

setenv hspice_lib_models /p/hdk/cad/pdk/pdk783_r0.9_23ww26.5_alpha/cmi/hspice/cmi/lnx86/64bit

