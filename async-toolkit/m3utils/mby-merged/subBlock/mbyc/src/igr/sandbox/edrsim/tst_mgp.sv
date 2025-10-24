// Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
// SPDX-License-Identifier: Apache-2.0

  rst <= 1'b0;
repeat(10)
  @(posedge cclk);
  
  rst <= 1'b1;
  grp_a0_rx_data_valid <= '0;  
  grp_a0_rx_port_num[1:0] <= '0;
  grp_a0_rx_time_stamp    <= 40'h123456789a;
  grp_a0_rx_metadata      <= '0;

   repeat(10)
  @(posedge cclk);
  rst <= 1'b0;
 repeat(100)  // wait for behave mem init done
  @(posedge cclk);
  grp_a0_rx_data_valid[7:0] <= '1;
  grp_a0_rx0_data <= {8{8'hff}};
  grp_a0_rx1_data <= {8{8'h11}};
  grp_a0_rx2_data <= {8{8'h22}};
  grp_a0_rx3_data <= {8{8'h33}};
  grp_a0_rx4_data <= {8{8'h44}};
  grp_a0_rx5_data <= {8{8'h55}};
  grp_a0_rx6_data <= {8{8'h66}};
  grp_a0_rx7_data <= {2{32'hdead_beef}};

  grp_a0_rx_metadata.eop <= 1'b1;
  grp_a0_rx_metadata.sop <= 1'b1;

  grp_a0_rx_metadata.eop_pos <= 3'd7;
  grp_a0_rx_metadata.sop_pos <= 3'd0;
  repeat(1)
    @(posedge cclk);
  grp_a0_rx_metadata <= '0;
  grp_a0_rx_metadata.eop <= 1'b0;
  grp_a0_rx_metadata.eop_pos <= 3'd0;
  grp_a0_rx_metadata.sop <= 1'b1;
  grp_a0_rx_metadata.sop_pos <= 3'd0;
  grp_a0_rx_data_valid <= 8'b1111_1111;
  grp_a0_rx0_data <= {8{8'hff}};
  grp_a0_rx1_data <= {8{8'h11}};
  grp_a0_rx2_data <= {8{8'h22}};
  grp_a0_rx3_data <= {8{8'h33}};
  grp_a0_rx4_data <= {8{8'h44}};
  grp_a0_rx5_data <= {8{8'h55}};
  grp_a0_rx6_data <= {8{8'h66}};
  grp_a0_rx7_data <= {8{8'h77}};
     repeat(1)
  @(posedge cclk);
  grp_a0_rx_metadata.sop <= 1'b0;
    grp_a0_rx_metadata.eop <= 1'b1;
  grp_a0_rx_metadata.eop_pos <= 3'd0;
  grp_a0_rx_data_valid <= 8'b0000_0001; 
  grp_a0_rx0_data <= {2{32'hdead_beef}};
  grp_a0_rx1_data <= {8{8'h11}};
  grp_a0_rx2_data <= {8{8'h22}};
  grp_a0_rx3_data <= {8{8'h33}};
  grp_a0_rx4_data <= {8{8'h44}};
  grp_a0_rx5_data <= {8{8'h55}};
  grp_a0_rx6_data <= {8{8'h66}};
  grp_a0_rx7_data <= {8{8'h77}};
     repeat(1)
  @(posedge cclk);
  grp_a0_rx_metadata <= '0;
  grp_a0_rx_data_valid <= 8'b0000_0000; 
     repeat(1000)
  @(posedge cclk);
