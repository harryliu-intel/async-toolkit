# /p/hdk/bin/cth_psetup -p ipde/2024.07.01 -cfg 78p3_opt12_r09hp1.cth -nowash -tool ipde_all -x '$SETUP_IPDE'

setenv RECHAR_CELL_LIST_FN cell_list.i0snand02aa1n02x5 
setenv RECHAR_SIS_WORKDIR  siliconsmart.i0snand02aa1n02x5
setenv RECHAR_ENV_ZSH      env.zsh.0p9u2_big
setenv RECHAR_PVTS_ZSH     pvts.zsh.300_85_cmax
#setenv RECHAR_LVF_MODE     sba
setenv RECHAR_LVF_MODE     ml
setenv RECHAR_STOP         characterize

./finish_setup.sh

