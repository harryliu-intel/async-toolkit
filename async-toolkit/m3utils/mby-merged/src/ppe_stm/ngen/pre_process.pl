#!/usr/bin/env perl 

# -------------------------------------------------------------------
# --                      Intel Proprietary
# --              Copyright (C) 2013 Intel Corporation
# --                    All Rights Reserved
# -------------------------------------------------------------------
# Function: Pre process the Ngen output file for this block.
# Authored by: Jon Bagge jon.bagge@intel.com
#--------------------------------------------------------------------            
open(SF, "../../../target/mby/mgm_run/ppe_stm/src/ppe_stm_shells_wrapper.v") || die "can't open input file\n";
open(TF, ">./ppe_stm_shells_wrapper_inc.v") || die "can't open output file\n";

print TF "`include        \"ppe_stm_mem.def\"\n";
while (<SF>) {
   printf TF ("%s",$_);
}
close (TF);
close (SF);
