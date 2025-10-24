#!/usr/bin/perl
# Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
# SPDX-License-Identifier: Apache-2.0


# -------------------------------------------------------------------
# --                      Intel Proprietary
# --              Copyright (C) 2013 Intel Corporation
# --                    All Rights Reserved
# -------------------------------------------------------------------
# Function: Post process the Ngen output file for this block.
# Copied from: Jon Bagge jon.bagge@intel.com
#--------------------------------------------------------------------            
open(SF, "./ngen/mby_msh_gen_mem.sv") || die "can't open input file\n";
open(TF, ">./mby_msh_gen_mem.sv") || die "can't open output file\n";

while (<SF>) {
   $str = $_;
   if ($str =~ /^logic.*msh_mem_bank._if;/) {
   } else {
     printf TF ("%s",$_);
   }
}
close (TF);
