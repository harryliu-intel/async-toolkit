#!/usr/bin/env perl 
# Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
# SPDX-License-Identifier: Apache-2.0


# -------------------------------------------------------------------
# --                      Intel Proprietary
# --              Copyright (C) 2013 Intel Corporation
# --                    All Rights Reserved
# -------------------------------------------------------------------
# Function: Post process the Ngen output file for this block.
# Authored by: Jon Bagge jon.bagge@intel.com
#--------------------------------------------------------------------            
open(SF, "ngen/ppe_stm_rx_top.sv") || die "can't open input file\n";
open(TF, ">./ppe_stm_rx_top.sv") || die "can't open output file\n";

while (<SF>) {
   ($m1) = $_ =~ /.*i_fwd_tbl0_rdata.*fwd_tbl0_rdata\[47:0\]\[3:0\]\[71:0\].*/;
   ($m2) = $_ =~ /.*i_fwd_tbl1_rdata.*fwd_tbl1_rdata\[15:0\]\[3:0\]\[71:0\].*/;
   if($m1) {
      $_ =~ s/fwd_tbl0_rdata\[47:0\]\[3:0\]\[71:0\]/fwd_tbl0_rdata/;
   }
   if($m2) {
      $_ =~ s/fwd_tbl1_rdata\[15:0\]\[3:0\]\[71:0\]/fwd_tbl1_rdata/;
   }
   printf TF ("%s",$_);
}
close (TF);
close (SF);
