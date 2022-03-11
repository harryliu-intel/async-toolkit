# LAtch-based Memory Blocks (LAMBs)
_Intel NEX/CG/XFG_
This is implementation and various tools associated with size, power, and timing estimation of LAMBs. The intent of these
circuits is to provide an area-efficient alternative for cases when the overhead of an SRAM would otherwise
dictate flip-flop based storage.

### Single LAMB product
To produce a single LAMB requires Cheetah environment. Execute, for example:

```
setenv GTR_HOME $PWD/gtr
/p/cth/bin/cth_psetup -p tfc -cfg tfc_n5.cth -tool librarycompiler,fusioncompiler,cth_LR -ward BUILD/cdp_lamb_n3bhd_1r1w1c_4d_10b \
   -cmd "icc2_lm_shell -batch -x 'source $GTR_HOME/tcl/gtr_main.tcl ; gtr_lamb_gen_views -data_width 10 -data_depth 4'"
```

To produce a single LAMB in a WARD.

The HIP can then be checked for IP quality and released with the 'SHIP' tool. This will perform IP quality check, and potentially fail.

For example:
```
/p/hdk/bin/cth_psetup -p tfc -tool ship -cfg tfc_n3.cth -ward BUILD/ship
ship.pl -block cdp_lamb_n3bhd_1r1w1c_4d_10b -tag testtag -ip_type hip -skip_stages archive \
  -source ../cdp_lamb_n3bhd_1r1w1c_4d_10b
```

Omit `-skip_stages archive` to actually perform the release to the $PROJ_ARCHIVE area. The recommended
form is to use an arc tag based off of the Git state, for example `git describe`.

### Batch Production And 'SHIP'ing of LAMBs
You can also batch up creation of LAMBs with the script provided in `$GTR_HOME/tcl/batchLambs.tcl`.

The typical usage case is to start with a space separated file described the desired configurations.

For example:

Consider `lambs.txt` as:
```
1r1w1c 4 10
1r1w1c 4 12
1r1w1c 4 14
```
This describes a LAMB requirements of width 4, for depths 10, 12, and 14.

To build up a task file for them, execute:

```
setenv GTR_HOME $PWD/gtr
$GTR_HOME/tcl/batchLambs.tcl -lf lambs.txt -archive
```

This will build up an task file for a Netbatch feeder that will build each LAMB, then run SHIP on each LAMB, and archive the
results to the projects `$PROJ_ARCHIVE` area with a tag based on the current version of the generator. Omit the `-archive` to 
skip the archive step (but still perform the SHIP quality checks).

### References
- [Cheetah Design System](https://goto/cheetah)
- [SHIP Tool](http://goto/ship)

###   Idea and architecture: Pat Bosshart, Barefoot/Intel

###   N3 and N5 implementation: Paul Donehue, Intel 

###   Characterization and prediction tools: Mika Nystrom, Intel

### Copyright
Copyright 2021 - 2022 Intel Corporation All Rights Reserved.

The source code contained or described herein and all documents related
to the source code ("Material") are owned by Intel Corporation or its
suppliers or licensors. Title to the Material remains with Intel
Corporation or its suppliers and licensors. The Material contains trade
secrets and proprietary and confidential information of Intel or its
suppliers and licensors. The Material is protected by worldwide copyright
and trade secret laws and treaty provisions. No part of the Material may
be used, copied, reproduced, modified, published, uploaded, posted,
transmitted, distributed, or disclosed in any way without Intel's prior
express written permission.

No license under any patent, copyright, trade secret or other intellectual
property right is granted to or conferred upon you by disclosure or
delivery of the Materials, either expressly, by implication, inducement,
estoppel or otherwise. Any license under such intellectual property rights
must be express and approved by Intel in writing.