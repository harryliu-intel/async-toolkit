#!/usr/intel/bin/perl



open ($infile_fh,  '<', "./ngen/mby_gmn_par.sv")       or die "Could not open mby_gmn_par.sv\n";
open ($outfile_fh, '>', "./ngen/mby_gmn_par_fixed.sv") or die "Could not open mby_gmn_par_fixed.sv\n";

while (<$infile_fh>) {
   if ($_ =~ m/module mby_gmn_par \(/) {
      print $outfile_fh "module mby_gmn_par\n";
      print $outfile_fh "import mby_igr_pkg::*;\n";
      print $outfile_fh "import mby_egr_pkg::*;\n";
      print $outfile_fh "import mby_gmm_pkg::*;\n";
      print $outfile_fh "(\n";
   }
   else {
      print $outfile_fh $_;
   }
}
close $outfile_fh;
