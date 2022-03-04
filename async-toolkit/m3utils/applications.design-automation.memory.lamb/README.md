# LAtch-based Memory Blocks (LAMBs)
###   Intel XFG 2021-2022
Implementation and various tools associated with size, power, and timing estimation.

To produce a single LAMB requires Cheetah environment. Execute, for example:

```
/p/hdk/bin/cth_psetup -p tfc -cfg tfc_n3.cth -tool librarycompiler -ward BUILD/cdp_lamb_n3bhd_1r1w1c_4d_10b \
   -cmd "icc2_lm_shell -x 'source $GTR_HOME/tcl/gtr_main.tcl ; gtr_lamb_gen_views -data_width 10 -data_depth 4'; exit 0"
```

To produce a single LAMB in a WARD.

The HIP can then be released with the 'SHIP' tool. This will perform IP quality check, and potentially fail. For example:
```
/p/hdk/bin/cth_psetup -p tfc -cfg tfc_n3.cth
$R2GSETUP
ship.pl -block cdp_lamb_n3bhd_1r1w1c_4d_10b -tag testtag -ip_type hip -source ../applications.design-automation.memory.lamb/BUILD/cdp_lamb_n3bhd_1r1w1c_4d_10b
```

###   Idea and architecture: Pat Bosshart, Barefoot/Intel

###   N3 and N5 implementation: Paul Donehue, Intel 

###   Characterization and prediction tools: Mika Nystrom, Intel
