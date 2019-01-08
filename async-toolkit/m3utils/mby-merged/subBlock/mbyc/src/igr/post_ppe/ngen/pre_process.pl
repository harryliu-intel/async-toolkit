#!/usr/bin/env perl 

# -------------------------------------------------------------------
# --                      Intel Proprietary
# --              Copyright (C) 2013 Intel Corporation
# --                    All Rights Reserved
# -------------------------------------------------------------------
# Function: Pre process the Ngen output file for this block.
# Copied from: Jon Bagge jon.bagge@intel.com
#--------------------------------------------------------------------            
open(SF, "../../../../../../target/mby/mgm_run/post_ppe/src/post_ppe_shells_wrapper.v") || die "can't open input file\n";
open(TF, ">./post_ppe_shells_wrapper_inc.v") || die "can't open output file\n";

print TF "`include        \"post_ppe_mem.def\"\n";
while (<SF>) {
   printf TF ("%s",$_);
}
close (TF);
close (SF);
