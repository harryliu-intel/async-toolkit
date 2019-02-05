#!/usr/bin/env perl 

# -------------------------------------------------------------------
# --                      Intel Proprietary
# --              Copyright (C) 2013 Intel Corporation
# --                    All Rights Reserved
# -------------------------------------------------------------------
# Function: Post process the Ngen output file for this block.
# Authored by: Jon Bagge jon.bagge@intel.com
#--------------------------------------------------------------------            
open(SF, "ngen/parser_top.sv") || die "can't open input file\n";
open(TF, ">./parser_top.sv") || die "can't open output file\n";

while (<SF>) {
   ($m1) = $_ =~ /^logic.*\(IGR_PPE_DATA_WIDTH\+IGR_PPE_ECC_WIDTH\)-1.*/;
   ($m2) = $_ =~ /^logic.*\(IGR_PPE_MD_WIDTH\+IGR_PPE_MD_ECC_WIDTH\)-1.*/;
   ($m3) = $_ =~ /.*i_input_data_fifo_rd_data.*input_data_fifo_rd_data\[1:0\]\[\(IGR_PPE_DATA_WIDTH\+IGR_PPE_ECC_WIDTH\)-1:0\].*/;
   ($m4) = $_ =~ /.*i_input_md_fifo_rd_data.*input_md_fifo_rd_data\[1:0\]\[\(IGR_PPE_MD_WIDTH\+IGR_PPE_MD_ECC_WIDTH\)-1:0\].*/;
   ($m5) = $_ =~ /.*o_q_input_data_fifo_wr_data.*input_data_fifo_wr_data\[1:0\]\[\(IGR_PPE_DATA_WIDTH\+IGR_PPE_ECC_WIDTH\)-1:0\].*/;
   ($m6) = $_ =~ /.*o_q_input_md_fifo_wr_data.*input_md_fifo_wr_data\[1:0\]\[\(IGR_PPE_MD_WIDTH\+IGR_PPE_MD_ECC_WIDTH\)-1:0\].*/;
   if($m1) {
      $_ =~ s/\(IGR_PPE_DATA_WIDTH\+IGR_PPE_ECC_WIDTH\)-1/1095/;
   }
   if($m2) {
      $_ =~ s/\(IGR_PPE_MD_WIDTH\+IGR_PPE_MD_ECC_WIDTH\)-1/123/;
   }
   if($m3) {
      $_ =~ s/input_data_fifo_rd_data\[1:0\]\[\(IGR_PPE_DATA_WIDTH\+IGR_PPE_ECC_WIDTH\)-1:0\]/input_data_fifo_rd_data/;
   }
   if($m4) {
      $_ =~ s/input_md_fifo_rd_data\[1:0\]\[\(IGR_PPE_MD_WIDTH\+IGR_PPE_MD_ECC_WIDTH\)-1:0\]/input_md_fifo_rd_data/;
   }
   if($m5) {
      $_ =~ s/input_data_fifo_wr_data\[1:0\]\[\(IGR_PPE_DATA_WIDTH\+IGR_PPE_ECC_WIDTH\)-1:0\]/input_data_fifo_wr_data/;
   }
   if($m6) {
      $_ =~ s/input_md_fifo_wr_data\[1:0\]\[\(IGR_PPE_MD_WIDTH\+IGR_PPE_MD_ECC_WIDTH\)-1:0\]/input_md_fifo_wr_data/;
   }
   printf TF ("%s",$_);
}
close (TF);
close (SF);
