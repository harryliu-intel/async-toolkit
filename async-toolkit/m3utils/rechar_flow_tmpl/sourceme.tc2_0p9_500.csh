# Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
# SPDX-License-Identifier: Apache-2.0

# /p/hdk/bin/cth_psetup -p ipde/2024.07.01 -cfg 78p3_opt12_r09hp1.cth -nowash -tool ipde_all -x '$SETUP_IPDE'

setenv RECHAR_CELL_LIST_FN cell_list.tc2_pdk0p9_002 
setenv RECHAR_SIS_WORKDIR  siliconsmart.tc2_0p9_500
setenv RECHAR_ENV_ZSH      env.zsh.0p9u2_bigger
setenv RECHAR_PVTS_ZSH     pvts.zsh.500_100_100_cmincmax
setenv RECHAR_LVF_MODE     ml

./finish_setup.sh

