#!/usr/bin/env perl 

# -------------------------------------------------------------------
# --                      Intel Proprietary
# --              Copyright (C) 2013 Intel Corporation
# --                    All Rights Reserved
# -------------------------------------------------------------------
# Function: Post process the Ngen output file for this block.
# Authored by: Jon Bagge jon.bagge@intel.com
#--------------------------------------------------------------------            
open(SF, "ngen/ppe_stm_tx_top.sv") || die "can't open input file\n";
open(TF, ">./ppe_stm_tx_top.sv") || die "can't open output file\n";

while (<SF>) {
   ($m1) = $_ =~ /.*i_mod_tbl_rdata.*mod_tbl_rdata\[9:0\]\[7:0\]\[71:0\].*/;
   if($m1) {
      $_ =~ s/mod_tbl_rdata\[9:0\]\[7:0\]\[71:0\]/mod_tbl_rdata/;
   }
   printf TF ("%s",$_);
}
close (TF);
close (SF);
