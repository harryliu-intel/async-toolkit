# To set proofgrid:

set task "jaspergold_proofgrid_hash"
set_proofgrid_mode shell
set nbpool sc_express
set nbq srd/hlp
set_proofgrid_per_engine_max_jobs 1
set_proofgrid_shell "/usr/intel/bin/nbjob run --mode blocking --target $nbpool --task $task"

