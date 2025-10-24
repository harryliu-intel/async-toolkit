# Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
# SPDX-License-Identifier: Apache-2.0

setenv RECHAR_CELL_LIST_FN cell_list.i0slsn000aa1d02x5
setenv RECHAR_SIS_WORKDIR  siliconsmart.i0slsn000aa1d02x5_0p9_cmax
setenv RECHAR_ENV_ZSH      env.zsh.0p9u2_big
setenv RECHAR_PVTS_ZSH     pvts.zsh.300_85_125_cmax
setenv RECHAR_LVF_MODE     sba

# /p/hdk/bin/cth_psetup -p ipde/2023.46.04 -cfg 78p3_opt12_r08hp2.cth -nowash -tool ipde_all -x '$SETUP_IPDE'

./finish_setup.sh
