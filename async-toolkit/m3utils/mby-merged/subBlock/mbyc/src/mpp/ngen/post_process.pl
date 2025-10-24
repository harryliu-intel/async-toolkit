#!/usr/intel/bin/perl
# Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
# SPDX-License-Identifier: Apache-2.0




open ($infile_fh,  '<', "./ngen/mby_mpp.sv")       or die "Could not open mby_mpp.sv\n";
open ($outfile_fh, '>', "./ngen/mby_mpp_fixed.sv") or die "Could not open mby_mpp_fixed.sv\n";

while (<$infile_fh>) {
   if ($_ =~ m/module mby_mpp \(/) {
      print $outfile_fh "module mby_mpp\n";
      print $outfile_fh "import mby_egr_pkg::*;\n";
      print $outfile_fh "import mby_msh_pkg::*;\n";
      print $outfile_fh "import shared_pkg::*;\n";
      print $outfile_fh "(\n";
   }
   else {
      print $outfile_fh $_;
   }
}
close $outfile_fh;
