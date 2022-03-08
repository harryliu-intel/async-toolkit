# LAtch-based Memory Blocks (LAMBs)
###   Intel XFG 2021-2022
Implementation and various tools associated with size, power, and timing estimation.

To produce a single LAMB requires Cheetah environment. Execute, for example, in the top-level directory of the repo:

```
setenv GTR_HOME $PWD/gtr
/p/cth/bin/cth_psetup -p tfc -cfg tfc_n5.cth -tool librarycompiler,fusioncompiler,cth_LR -ward BUILD/cdp_lamb_n3bhd_1r1w1c_4d_10b \
   -cmd "icc2_lm_shell -batch -x 'source $GTR_HOME/tcl/gtr_main.tcl ; gtr_lamb_gen_views -data_width 10 -data_depth 4'"
```

To produce a single LAMB in a WARD.

The HIP can then be checked for IP quality and released with the 'SHIP' tool. This will perform IP quality check, and potentially fail.

For example:
```
/p/hdk/bin/cth_psetup -p tfc -tool ship -cfg tfc_n5.cth -ward BUILD/ship
ship.pl -block cdp_lamb_n3bhd_1r1w1c_4d_10b -tag testtag -ip_type hip -skip_stages archive \
  -source ../cdp_lamb_n3bhd_1r1w1c_4d_10b
```

Omit `-skip_stages archive` to actually perform the release to the $PROJ_ARCHIVE area. The recommended
form is to use an arc tag based off of the Git state, for example `git describe`.

You can also batch up creation of LAMBs with the $GTR_HOME/tcl/batchLambs.tcl

### Preliminary flow-through support

```
setenv GTR_HOME $PWD/gtr
/p/cth/bin/cth_psetup -p tfc -cfg tfc_n5.cth -tool librarycompiler,fusioncompiler,cth_LR -ward BUILD/cdp_lamb_n3bhd_1ftr1w1c_4d_10b \
   -cmd "icc2_lm_shell -batch -x 'source $GTR_HOME/tcl/gtr_main.tcl ; gtr_lamb_gen_views -data_width 10 -data_depth 4 -flow_through'"
```

###   Idea and architecture: Pat Bosshart, Barefoot/Intel

###   N3 and N5 implementation: Paul Donehue, Intel 

###   Characterization and prediction tools: Mika Nystrom, Intel

###   ERRATA

- tfc_n5.cth above is not a typo!  We don't know why tfc_n3.cth doesn't work at the moment.
