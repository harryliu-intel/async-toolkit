#!/usr/bin/env perl 
# Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
# SPDX-License-Identifier: Apache-2.0


# -------------------------------------------------------------------
# --                      Intel Proprietary
# --              Copyright (C) 2013 Intel Corporation
# --                    All Rights Reserved
# -------------------------------------------------------------------
# Function: Pre process the Ngen output file for this block.
# Copied from: Jon Bagge jon.bagge@intel.com
#--------------------------------------------------------------------            
open(SF, "$ENV{'MODEL_ROOT'}/target/mby/mgm_run/mce/src/mce_shells_wrapper.v") || die "can't open input file, please run MGM flow\n";
open(TF, ">./mce_shells_wrapper_inc.v") || die "can't open output file\n";

print TF "`include        \"mce_mem.def\"\n";
while (<SF>) {
   printf TF ("%s",$_);
}
close (TF);
close (SF);
