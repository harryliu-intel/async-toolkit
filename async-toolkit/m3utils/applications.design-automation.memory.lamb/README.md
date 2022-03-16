# LAtch-based Memory Blocks (LAMBs)
_Intel NEX/CG/XFG_

### Purpose and Intent
This is implementation and various tools associated with size, power, and timing estimation of LAMBs. The intent of these
circuits is to provide an area-efficient alternative for cases when the overhead of an SRAM would otherwise
dictate flip-flop based storage.

### Variants
- 1r1w1c: Standard 1r1w, 1 clock implementation
- 1ftr1w1c: 1r1w memory, 1 clock, with flow-through (i.e. unflopped output data). Under development.

### Process Technology
This work is being developed primarily for TSMC N3E process, but the principle is generic.

### Supported Views
The present state of the code is to generate 'placeholder' content to enable logic design and
pipecleaning  physical design flows. This includes:

- SV behavioral verilog
- Liberty Files @ SSGNP, 0C
- Synopsys DB files based on Liberty Files
- LEF abstracts of rough pin positions
- Synopsys NDM Abstracts Based on the LEF and Liberty Files

### Views Under Development
- Additional process corner, voltages and temperature combinations
- OASIS/GDSII Layout

### Requirements
- SLES12 machine within HPC environment
- Up-to-date Cheetah Environment, with access to relevant tool licenses

### Release Methodology
Releases to broad silicon teams should always be referenceable back to a specific git point.
Generally, only do releases against an annotated git tag, to ensure reproducibility if additional
HIPs are added to the released set. For example, to checkout the annotated tag lamb_0_0_2, you can do:

```
git clone https://github.com/intel-innersource/applications.design-automation.memory.lamb.git
git checkout lamb_0_0_2
```

GitHub maintains a list of the [existing annotated tags](https://github.com/intel-innersource/applications.design-automation.memory.lamb/tags) for this project.

### Single LAMB product
To produce a single LAMB requires Cheetah environment. Execute, for example:

```
setenv GTR_HOME $PWD/gtr
/p/cth/bin/cth_psetup -p tfc -cfg tfc_ipde_n3.cth -tool librarycompiler,fusioncompiler/R-2020.09-SP5-T-20211019,cth_LR -ward BUILD/cdp_lamb_n3bhd_1r1w1c_4d_10b \
   -cmd "icc2_lm_shell -batch -x 'source $GTR_HOME/tcl/gtr_main.tcl ; gtr_lamb_gen_views -data_width 10 -data_depth 4'"
```

To produce a single LAMB in a WARD.

The HIP can then be checked for IP quality and released with the `SHIP` tool. This will perform an IP quality check, and potentially fail.

For example:
```
/p/hdk/bin/cth_psetup -p tfc -tool ship -cfg tfc_ipde_n3.cth -ward BUILD/ship
ship.pl -tag testtag -ip_type hip -test -skip_stages archive \
  -block cdp_lamb_n3bhd_1r1w1c_4d_10b -source ../cdp_lamb_n3bhd_1r1w1c_4d_10b 
```

Omit `-skip_stages archive` and `-test` to actually perform the release to the `$PROJ_ARCHIVE` area. Use an arc tag based off
of the Git state, for example `git describe` will provide a reference that would be a correct argument to
`git checkout` in the future.

### Batch Production And 'SHIP'ing of LAMBs
You can also batch up creation of LAMBs with the script provided in `$GTR_HOME/tcl/batchLambs.tcl`.

The typical usage case is to start with a space separated file described the desired configurations, or the names of the needed instances.

For example:

Consider `lambs.txt` as:
```
1r1w1c 4 10
1r1w1c 4 12
1r1w1c 4 14
cdp_lamb_n3bhd_1r1w1c_64d_66b
```
This describes a LAMB requirements of width 4, for depths 10, 12, and 14 and for an instance of depth 64, width 66.

To build up a task file for them, execute:

```
git checkout <the git tag of the generator version you want>
setenv GTR_HOME $PWD/gtr
$GTR_HOME/tcl/batchLambs.tcl -lf lambs.txt -archive
```

This will build up an task file for a Netbatch feeder that will build each LAMB, then run SHIP on each LAMB, and archive the
results to the projects `$PROJ_ARCHIVE` area with a tag based on the current version of the generator. Omit the `-archive` to 
skip the archive step (but still perform the SHIP quality checks).

You will need to use nbfeeder to execute the task file.

### References
- [Cheetah Design System](https://goto/cheetah)
- [SHIP Tool](http://goto/ship)

###  Credits
- Idea and architecture: Pat Bosshart
- N3 and N5 implementation: Paul Donehue @pdonehue
- Characterization and prediction tools: Mika Nystrom @mikanystrom-intel

### Known Errata and Caveats
- The tool is very sensitive to the installation of Fusion Compiler used. This seems to be related to its use of the 'read_lib' operation
  within icc2_lm_shell (which calls out to another executable). Setting the environment variable SYNOPSYS_LC_ROOT to
  a current Library Compiler release can resolve this. [Reference](https://solvnetplus.synopsys.com/s/article/IC-Compiler-II-Library-Manager-read-lib-Command-Setup-and-Usage-1576092603765)

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
