#avoid post-processor errs
#echo SIMSTAT: ============== SOC: Start of Test ==================
#save the new environment variables, since they'll be overwritten by the restore
exec env > env.txt
#restore the simulation
#restore [getenv ACE_PROJECT_HOME]/results/tests/[getenv SAVE_RSTORE_TESTNAME]/saved_vcs
restore [getenv ACE_UTILS_ROOT]/results/save_restore/saved_vcs
#indicate that it's been restored
force top.fctop_sig_if.restore 1 -deposit
#dump -add { top }  -depth 8 -scope "."
#run the simulation
run
#dump -add { top }  -depth 0 -scope "."
#run -absolute 700us
#dump -close
exit
