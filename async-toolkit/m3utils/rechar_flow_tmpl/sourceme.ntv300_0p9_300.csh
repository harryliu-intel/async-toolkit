# Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
# SPDX-License-Identifier: Apache-2.0

# /p/hdk/bin/cth_psetup -p ipde/2024.07.01 -cfg 78p3_opt12_r09hp1.cth -nowash -tool ipde_all -x '$SETUP_IPDE'

setenv RECHAR_CELL_LIST_FN cell_list.ntv300 
setenv RECHAR_SIS_WORKDIR  siliconsmart.ntv300_0p9_300
setenv RECHAR_ENV_ZSH      env.zsh.0p9u2_big
setenv RECHAR_PVTS_ZSH     pvts.zsh.300_85_100_cmincmax
setenv RECHAR_LVF_MODE     sba

./finish_setup.sh

