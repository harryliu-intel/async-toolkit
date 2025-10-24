#
# Job Scheduler Channels
#
GridCmd -queue {Zebu} -submit {/usr/intel/bin/nbjob run --target 'pdx_critical' --qslot '/SDG/sdg74/fe/build/fxr' --class 'SLES11SP4&&16G' --mode interactive --rlimits "stacksize=unlimited" --post-exec "sleep 3"} -delete {} -njobs {0}
GridCmd -queue {ZebuLight} -submit {/usr/intel/bin/nbjob run --target 'pdx_critical' --qslot '/SDG/sdg74/fe/build/fxr' --class 'SLES11SP4&&8G' --mode interactive --rlimits "stacksize=unlimited" --post-exec "sleep 3"} -delete {} -njobs {0}
GridCmd -queue {ZebuSuperHeavy} -submit {/usr/intel/bin/nbjob run --target 'pdx_critical' --qslot '/SDG/sdg74/fe/build/fxr' --class 'SLES11SP4&&64G&&4C' --mode interactive --rlimits "stacksize=unlimited" --post-exec "sleep 3"} -delete {} -njobs {0}
GridCmd -queue {ZebuIse} -submit {/usr/intel/bin/nbjob run --target 'pdx_critical' --qslot '/SDG/sdg74/fe/build/fxr' --class 'SLES11SP4&&64G&&4C' --mode interactive  --post-exec "sleep 3"} -delete {} -njobs {0}
GridCmd -queue {ZebuSynthesis} -submit {/usr/intel/bin/nbjob run --target 'pdx_critical' --qslot '/SDG/sdg74/fe/build/fxr' --class 'SLES11SP4&&64G' --mode interactive  --post-exec "sleep 3"} -delete {} -njobs {0}
GridCmd -queue {ZebuVcs} -submit {/usr/intel/bin/nbjob run --target 'pdx_critical' --qslot '/SDG/sdg74/fe/build/fxr' --class 'SLES11SP4&&32G' --mode interactive  --post-exec "sleep 3"} -delete {} -njobs {0}
GridCmd -queue {ZebuHeavy} -submit {/usr/intel/bin/nbjob run --target 'pdx_critical'  --qslot '/SDG/sdg74/fe/build/fxr' --class 'SLES11SP4&&64G&&4C' --mode interactive --rlimits "stacksize=unlimited" --post-exec "sleep 3"} -delete {} -njobs {0}
GridCmd -queue {zGraphGenerator} -submit {/usr/intel/bin/nbjob run --target 'pdx_critical' --qslot '/SDG/sdg74/fe/build/fxr' --class 'SLES11SP4&&64G' --mode interactive --rlimits "stacksize=unlimited" --post-exec "sleep 3"} -delete {} -njobs {0}
GridCmd -queue {Target_Config} -submit {/usr/intel/bin/nbjob run --target 'pdx_critical' --qslot '/SDG/sdg74/fe/build/fxr' --class 'SLES11SP4&&8G' --mode interactive --rlimits "stacksize=unlimited" --post-exec "sleep 3"} -delete {} -njobs {0}


#
# VCS Task Association
#
# GridTaskAssociation -task VCS_Task_Analyzer -queue {ZebuVcs}
# GridTaskAssociation -task VCS_Task_Builder -queue {ZebuVcs}
# GridTaskAssociation -task zGraphGenerator -queue {zGraphGenerator}
# GridTaskAssociation -task zTopBuild -queue {zGraphGenerator}
# GridTaskAssociation -task Target_Config -queue {Target_Config}

#
# NFS latency guard (should reduce spurious failure)
#
MaxNfsRetries -access_level {1}
