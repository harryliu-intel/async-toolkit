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
      print $outfile_fh "// Tag ring interface (ingress -to- egress/GMM)\n";
      print $outfile_fh "mby_tag_ring_t           tag_ring_in [MBY_MAX_NUM_MGP_MSB:0][1:0] ;\n";
      print $outfile_fh "mby_tag_ring_t           tag_ring_out[MBY_MAX_NUM_MGP_MSB:0][1:0] ;\n";
      print $outfile_fh "\n";
      print $outfile_fh "// MultiCast tag ring interafce (MCE-to-egress)\n";
      print $outfile_fh "mby_mc_tag_ring_t        mc_tag_ring_out_left  [3:0] ;\n";
      print $outfile_fh "mby_mc_tag_ring_t        mc_tag_ring_out_right [3:0] ;\n";
      print $outfile_fh "\n";
      print $outfile_fh "// Dequeue (Egress -to- GMM)\n";
      print $outfile_fh "mby_unicast_deque_t      mby_deque_from_egr [MBY_MAX_NUM_MGP_MSB:0][1:0];\n";
      print $outfile_fh "\n";
   }
   else {
      print $outfile_fh $_;
   }
}
close $outfile_fh;
