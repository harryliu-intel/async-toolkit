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
open(SF, "./ngen/mby_igr_pbb_gen_mem.sv") || die "can't open input file\n";
open(TF, ">./mby_igr_pbb_gen_mem.sv") || die "can't open output file\n";

while (<SF>) {
   $str = $_;
#   if ($str =~ /^\s*module\s*mby_igr_pbb_gen_mem \(/) {
#     printf TF "module mby_igr_pbb_gen_mem\n";
#     printf TF "import mby_igr_pkg::*;\n";
#     printf TF "(\n";
#   } else {
   if ($str =~ /^\s*logic\s*igr_pbb.*_if;\s*$/) {

   } else {
     printf TF ("%s",$_);
   }
#  }
}
close (TF);
