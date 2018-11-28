#!/usr/intel/bin/perl



open ($infile_fh,  '<', "./ngen/mby_gms_par.sv")       or die "Could not open mby_gms_par.sv\n";
open ($outfile_fh, '>', "./ngen/mby_gms_par_fixed.sv") or die "Could not open mby_gms_par_fixed.sv\n";


while (<$infile_fh>) {
   if ($_ =~ m/module mby_gms_par \(/) {
      print $outfile_fh "module mby_gms_par\n";
      print $outfile_fh "import mby_igr_pkg::*;\n";
      print $outfile_fh "import mby_egr_pkg::*;\n";
      print $outfile_fh "import mby_gmm_pkg::*;\n";
      print $outfile_fh "(\n";
   }
   #elsif ($_ =~ m/^logic\w*[68:0][1:0] ;/) {
   elsif ($_ =~ m/^logic/) {
      # Don't print this line   
   }
   else {
      print $outfile_fh $_;
   }
}
close $outfile_fh;
