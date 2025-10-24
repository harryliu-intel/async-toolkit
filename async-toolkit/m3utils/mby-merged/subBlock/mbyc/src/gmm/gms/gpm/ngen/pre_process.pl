#!/usr/bin/env perl 
# Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
# SPDX-License-Identifier: Apache-2.0

open(SF, "$ENV{'MODEL_ROOT'}/target/mby/mgm_run/gpm/src/gpm_shells_wrapper.v") || die "can't open input file
";
open(TF, ">./gpm_shells_wrapper_inc.v") || die "can't open output file
";

print TF "`include        \"gpm_mem.def\"
";
while (<SF>) {
   printf TF ("%s",$_);
}
close (TF);
close (SF);
