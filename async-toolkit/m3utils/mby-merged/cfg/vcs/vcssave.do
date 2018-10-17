#run until the testbench says it's time to save
stop -change fc_hvl_top.fc_sig_if.save -condition {fc_hvl_top.fc_sig_if.save==1}
#indicate that the test is running with the SAVE enabled. 
#force fc_hvl_top.fc_sig_if.enable_save 1 -deposit
#dump -add { top }  -depth 0 -scope "."
run
#delay until a new tick
run 1
#dump -close
#save the image
#save [getenv ACE_PROJECT_HOME]/results/save_restore/saved_vcs 
save [getenv ACE_UTILS_ROOT]/results/save_restore/saved_vcs 
#run until completion
#run
#avoid post-processor errs
#SIMSTAT:============== SOC: End of Test ==================
#$finish at simulation time
#all done
#quit
