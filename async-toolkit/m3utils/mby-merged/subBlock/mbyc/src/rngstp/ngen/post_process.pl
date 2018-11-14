#!/usr/intel/bin/perl



open ($infile_fh,  '<', "./ngen/mby_mgp.sv")       or die "Could not open mby_mgp.sv\n";
open ($outfile_fh, '>', "./ngen/mby_mgp_fixed.sv") or die "Could not open mby_mgp_fixed.sv\n";

while (<$infile_fh>) {
   if ($_ =~ m/module mby_mgp \(/) {
      print $outfile_fh "module mby_mgp\n";
      print $outfile_fh "import mby_igr_pkg::*;\n";
      print $outfile_fh "(\n";
   }
   else {
      print $outfile_fh $_;
   }
}
close $outfile_fh;
