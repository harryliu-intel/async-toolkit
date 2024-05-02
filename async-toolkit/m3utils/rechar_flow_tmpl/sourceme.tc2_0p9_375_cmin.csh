# /p/hdk/bin/cth_psetup -p ipde/2024.07.01 -cfg 78p3_opt12_r09hp1.cth -nowash -tool ipde_all -x '$SETUP_IPDE'

setenv RECHAR_CELL_LIST_FN cell_list.tc2_pdk0p9_002 
setenv RECHAR_SIS_WORKDIR  siliconsmart.tc2_0p9_375_cmin
setenv RECHAR_ENV_ZSH      env.zsh.0p9u2_bigger
setenv RECHAR_PVTS_ZSH     pvts.zsh.375_125_125_cmin
setenv RECHAR_LVF_MODE     ml

#setenv RECHAR_START        compile_ldbs
#setenv RECHAR_STOP         compile_ldbs
setenv RECHAR_START        create_ndms
unsetenv RECHAR_STOP

./finish_setup.sh

