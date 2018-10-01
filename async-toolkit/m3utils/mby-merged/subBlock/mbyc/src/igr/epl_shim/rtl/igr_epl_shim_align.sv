//
//  Copyright 2006 - 2017 Intel Corporation All Rights Reserved.
//
//  The source code contained or described herein and all documents related
//  to the source code ("Material") are owned by Intel Corporation or its
//  suppliers or licensors. Title to the Material remains with Intel
//  Corporation or its suppliers and licensors. The Material contains trade
//  secrets and proprietary and confidential information of Intel or its
//  suppliers and licensors. The Material is protected by worldwide copyright
//  and trade secret laws and treaty provisions. No part of the Material may
//  be used, copied, reproduced, modified, published, uploaded, posted,
//  transmitted, distributed, or disclosed in any way without Intel's prior
//  express written permission.
//
//  No license under any patent, copyright, trade secret or other intellectual
//  property right is granted to or conferred upon you by disclosure or
//  delivery of the Materials, either expressly, by implication, inducement,
//  estoppel or otherwise. Any license under such intellectual property rights
//  must be express and approved by Intel in writing.
//
//------------------------------------------------------------------------------
// -- Author       : Edward C. Ross
// -- Project Name : Madison Bay
// -- Description  : This block removes any flit gaps in the 8 flit EPL data segment.
//
//------------------------------------------------------------------------------



module igr_epl_shim_align 
  import mby_igr_pkg::*;
(
  input logic [7:0]                  rx_data_v,
  input epl_md_t                         rx_md,
  input epl_ts_t                         rx_ts,  
  input epl_data_t [0:7]               rx_data,
  
  output epl_md_t                  rx_md_align,
  output epl_data_t [0:7]        rx_data_align


);

  logic [7:0] maskf0;
  logic [7:0] maskf1;
  logic [7:0] maskf2;
  logic [7:0] maskf3;
  logic [7:0] maskf4;
  logic [7:0] maskf5;
  logic [7:0] maskf6;
  logic [7:0] maskf7;
  
  //desigmware count_ones
  parameter width = 8;  //used by function
  logic [width-1:0] cnt_ones;
  `include "DW_dp_count_ones_function.inc"
  assign cnt_ones = DWF_dp_count_ones(rx_data_v);
  
  
  always_comb begin

    unique case(rx_data_v) inside  //mask to place first valid data flit in align_data flit0
      8'b????_???1: maskf0 = 8'b0000_0001; //0:0
      8'b????_??10: maskf0 = 8'b0000_0010; //1:0
      8'b????_?100: maskf0 = 8'b0000_0100; //2:0
      8'b????_1000: maskf0 = 8'b0000_1000; //3:0
      8'b???1_0000: maskf0 = 8'b0001_0000; //4:0
      8'b??10_0000: maskf0 = 8'b0010_0000; //5:0
      8'b?100_0000: maskf0 = 8'b0100_0000; //6:0
      8'b1000_0000: maskf0 = 8'b1000_0000; //7:0
      default:      maskf0 = '0;
    endcase
    
    unique case(rx_data_v) inside  //mask to place 2nd valid data flit in align_data flit1
      8'b????_??11:                                           maskf1 = 8'b0000_0010; //1:1
      8'b????_?101, 8'b????_?110:                             maskf1 = 8'b0000_0100; //2:1, 1zero
      8'b????_1001, 8'b????_1010, 8'b????_1100:               maskf1 = 8'b0000_1000; //3:1, 2zero
      8'b???1_0001, 8'b???1_0010, 8'b???1_0100, 8'b???1_1000: maskf1 = 8'b0001_0000; //4:1, 3zero
      8'b??10_0001, 8'b??10_0010, 8'b??10_0100, 8'b??10_1000,
      8'b??11_0000:                                           maskf1 = 8'b0010_0000; //5:1, 4zero
      8'b?100_0001, 8'b?100_0010, 8'b?100_0100, 8'b?100_1000,
      8'b?101_0000, 8'b?110_0000:                             maskf1 = 8'b0100_0000; //6:1, 5zero
      8'b1000_0001, 8'b1000_0010, 8'b1000_0100, 8'b1000_1000,
      8'b1001_0000, 8'b1010_0000, 8'b1100_0000:               maskf1 = 8'b0100_0000; //7:1, 6zero
      default:                                                maskf1 = '0;
    endcase

    unique case(rx_data_v) inside  //mask to place 3rd valid data flit in align_data flit2
      8'b????_?111: maskf2 = 8'b0000_0100; //2:2
      8'b????_1011, 8'b????_1101, 8'b????_1110:               maskf2 = 8'b0000_1000; //3:2
      8'b???1_0011, 8'b???1_0101, 8'b???1_0110, 8'b???1_1001,
      8'b???1_1010, 8'b???1_1100:                             maskf2 = 8'b0001_0000; //4:2
      8'b??10_0011, 8'b??10_0101, 8'b??10_0110, 8'b??10_1001,
      8'b??10_1010, 8'b??10_1100, 8'b??11_0001, 8'b??11_0100,
      8'b??11_1000:                                           maskf2 = 8'b0010_0000; //5:2
      8'b?100_0011, 8'b?100_0101, 8'b?100_0110, 8'b?100_1001,
      8'b?100_1010, 8'b?100_1100, 8'b?101_0001, 8'b?101_0010,
      8'b?101_0100, 8'b?101_1000, 8'b?110_0001, 8'b?110_0010,
      8'b?110_0100, 8'b?110_1000, 8'b?111_0000:               maskf2 = 8'b0100_0000; //6:2
      8'b1000_0011, 8'b1000_0101, 8'b1000_0110, 8'b1000_1001,
      8'b1000_1010, 8'b1000_1100, 8'b1001_0001, 8'b1001_0010,
      8'b1001_0100, 8'b1001_1000, 8'b1010_0001, 8'b1010_0010,
      8'b1010_0100, 8'b1010_1000, 8'b1011_0000, 8'b1100_0001,
      8'b1100_0010, 8'b1100_0100, 8'b1100_1000, 8'b1101_0000,
      8'b1110_0000:                                           maskf2 = 8'b1000_0000; //7:2
      default:      maskf2 = '0;
    endcase

    unique case(rx_data_v) inside  //mask to place 4th valid data flit in align_data flit3
      8'b????_1111:                                           maskf3 = 8'b0000_1000; //3:3
      8'b???1_0111, 8'b???1_1011, 8'b???1_1101, 8'b???1_1100: maskf3 = 8'b0001_0000; //4:3
      8'b??10_0111, 8'b??10_1011, 8'b??10_1101, 8'b??10_1110,
      8'b??11_0011, 8'b??11_0101, 8'b??11_0110, 8'b??11_1010,
      8'b??11_1100:                                           maskf3 = 8'b0010_0000; //5:3
      8'b?100_0111, 8'b?100_1011, 8'b?100_1101, 8'b?100_1110,
      8'b?101_0011, 8'b?101_0101, 8'b?101_0110, 8'b?101_1010,
      8'b?101_1100, 8'b?110_0011, 8'b?110_0101, 8'b?110_0110,
      8'b?110_1010, 8'b?110_1100, 8'b?111_0001, 8'b?111_0010,
      8'b?111_0100, 8'b?111_1000:                             maskf3 = 8'b0100_0000; //6:3
      8'b1000_0111, 8'b1000_1011, 8'b1000_1101, 8'b1000_1110,
      8'b1001_0011, 8'b1001_0101, 8'b1001_0110, 8'b1001_1001,
      8'b1001_1010, 8'b1001_1100, 8'b1010_0011, 8'b1010_0101,      
      8'b1010_0110, 8'b1010_1001, 8'b1010_1010, 8'b1010_1110,      
      8'b1011_0001, 8'b1011_0010, 8'b1011_0100, 8'b1011_1000,
      8'b1100_0011, 8'b1100_0101, 8'b1100_0110, 8'b1100_1001,
      8'b1100_1010, 8'b1100_1100, 8'b1101_0001, 8'b1101_0010,
      8'b1101_0100, 8'b1101_1000, 8'b1110_0001, 8'b1110_0010,
      8'b1110_0100, 8'b1110_1000, 8'b1111_0000:               maskf3 = 8'b1000_0000; //7:3
      default:      maskf3 = '0;
    endcase
    
    unique case(rx_data_v) inside  //mask to place 5th valid data flit in align_data flit4
      8'b???1_1111: maskf4 = 8'b0001_0000; //4:4
      8'b??10_1111, 8'b??11_0111, 8'b??11_1011, 8'b??11_1101,
      8'b??11_1110:                                           maskf4 = 8'b0010_0000; //5:4
      8'b?100_1111, 8'b?101_0111, 8'b?101_1011, 8'b?101_1110,
      8'b?101_1110, 8'b?110_0111, 8'b?110_1011, 8'b?110_1101,
      8'b?110_1110, 8'b?111_0011, 8'b?111_0101, 8'b?111_0110,
      8'b?111_1001, 8'b?111_1010, 8'b?111_1100:               maskf4 = 8'b0100_0000; //6:4
      8'b1000_1111, 8'b1001_0111, 8'b1001_1101, 8'b1001_1110,
      8'b1010_0111, 8'b1010_1101, 8'b1010_1110, 8'b1011_0011,
      8'b1011_0101, 8'b1011_0110, 8'b1011_1001, 8'b1011_1010,
      8'b1011_1100, 8'b1100_0111, 8'b1100_1011, 8'b1100_1101,
      8'b1100_1110, 8'b1101_0011, 8'b1101_0101, 8'b1101_0110,
      8'b1101_1001, 8'b1101_1010, 8'b1101_1100, 8'b1110_0011,
      8'b1110_0101, 8'b1110_0110, 8'b1110_1001, 8'b1110_1010,
      8'b1110_1100, 8'b1111_0001, 8'b1111_0010, 8'b1111_0100,
      8'b1111_1000:                                           maskf4 = 8'b1000_0000; //7:4
      default:                                                maskf4 = '0;
    endcase

    unique case(rx_data_v) inside  //mask to place 6th valid data flit in align_data flit5
      8'b??11_1111: maskf5 = 8'b0010_0000; //5:5
      8'b?101_1111, 8'b?110_1111, 8'b?111_0111, 8'b?111_1011,
      8'b?111_1101, 8'b?111_1110:                             maskf5 = 8'b0100_0000; //6:5
      8'b1001_1111, 8'b1010_1111, 8'b1011_0111, 8'b1011_1011,
      8'b1011_1101, 8'b1011_1110, 8'b1100_1111, 8'b1101_0111,
      8'b1101_1101, 8'b1101_1101, 8'b1101_1110, 8'b1110_0111,
      8'b1110_1011, 8'b1110_1101, 8'b1110_1110, 8'b1111_0011,
      8'b1111_0101, 8'b1111_0110, 8'b1111_1001, 8'b1111_1010,
      8'b1111_1100:                                           maskf5 = 8'b1000_0000; //7:5
      default:                                                maskf5 = '0;
    endcase

    unique case(rx_data_v) inside  //mask to place 7th valid data flit in align_data flit4
      8'b?111_1111: maskf6 = 8'b0100_0000; //6:6
      8'b1011_1111, 8'b1101_1111, 8'b1110_1111, 8'b1111_0111,
      8'b1111_1011, 8'b1111_1101, 8'b1111_1110:               maskf6 = 8'b1000_0000; //7:6
      default:                                                maskf6 = '0;
    endcase

    unique case(rx_data_v) inside  //mask to place 8th valid data flit in align_data flit4
      8'b1111_1111:                                           maskf7 = 8'b1000_0000; //7:7
      default:                                                maskf7 = '0;
    endcase
    
//build packed data segment
    rx_data_align[0] = ({72{maskf0[0]}} & rx_data[0]) | ({72{maskf0[1]}} & rx_data[1]) |
                       ({72{maskf0[2]}} & rx_data[2]) | ({72{maskf0[3]}} & rx_data[3]) |
                       ({72{maskf0[4]}} & rx_data[4]) | ({72{maskf0[5]}} & rx_data[5]) |
                       ({72{maskf0[6]}} & rx_data[6]) | ({72{maskf0[7]}} & rx_data[7]);
                       
    rx_data_align[1] = ({72{maskf1[0]}} & rx_data[0]) | ({72{maskf1[1]}} & rx_data[1]) |
                       ({72{maskf1[2]}} & rx_data[2]) | ({72{maskf1[3]}} & rx_data[3]) |
                       ({72{maskf1[4]}} & rx_data[4]) | ({72{maskf1[5]}} & rx_data[5]) |
                       ({72{maskf1[6]}} & rx_data[6]) | ({72{maskf1[7]}} & rx_data[7]);
                       
    rx_data_align[2] = ({72{maskf2[0]}} & rx_data[0]) | ({72{maskf2[1]}} & rx_data[1]) |
                       ({72{maskf2[2]}} & rx_data[2]) | ({72{maskf2[3]}} & rx_data[3]) |
                       ({72{maskf2[4]}} & rx_data[4]) | ({72{maskf2[5]}} & rx_data[5]) |
                       ({72{maskf2[6]}} & rx_data[6]) | ({72{maskf2[7]}} & rx_data[7]);
                       
    rx_data_align[3] = ({72{maskf3[0]}} & rx_data[0]) | ({72{maskf3[1]}} & rx_data[1]) |
                       ({72{maskf3[2]}} & rx_data[2]) | ({72{maskf3[3]}} & rx_data[3]) |
                       ({72{maskf3[4]}} & rx_data[4]) | ({72{maskf3[5]}} & rx_data[5]) |
                       ({72{maskf3[6]}} & rx_data[6]) | ({72{maskf3[7]}} & rx_data[7]);
                       
    rx_data_align[4] = ({72{maskf4[0]}} & rx_data[0]) | ({72{maskf4[1]}} & rx_data[1]) |
                       ({72{maskf4[2]}} & rx_data[2]) | ({72{maskf4[3]}} & rx_data[3]) |
                       ({72{maskf4[4]}} & rx_data[4]) | ({72{maskf4[5]}} & rx_data[5]) |
                       ({72{maskf4[6]}} & rx_data[6]) | ({72{maskf4[7]}} & rx_data[7]);
                       
    rx_data_align[5] = ({72{maskf5[0]}} & rx_data[0]) | ({72{maskf5[1]}} & rx_data[1]) |
                       ({72{maskf5[2]}} & rx_data[2]) | ({72{maskf5[3]}} & rx_data[3]) |
                       ({72{maskf5[4]}} & rx_data[4]) | ({72{maskf5[5]}} & rx_data[5]) |
                       ({72{maskf5[6]}} & rx_data[6]) | ({72{maskf5[7]}} & rx_data[7]);
                       
    rx_data_align[6] = ({72{maskf6[0]}} & rx_data[0]) | ({72{maskf6[1]}} & rx_data[1]) |
                       ({72{maskf6[2]}} & rx_data[2]) | ({72{maskf6[3]}} & rx_data[3]) |
                       ({72{maskf6[4]}} & rx_data[4]) | ({72{maskf6[5]}} & rx_data[5]) |
                       ({72{maskf6[6]}} & rx_data[6]) | ({72{maskf6[7]}} & rx_data[7]);
                       
    rx_data_align[7] = ({72{maskf7[0]}} & rx_data[0]) | ({72{maskf7[1]}} & rx_data[1]) |
                       ({72{maskf7[2]}} & rx_data[2]) | ({72{maskf7[3]}} & rx_data[3]) |
                       ({72{maskf7[4]}} & rx_data[4]) | ({72{maskf7[5]}} & rx_data[5]) |
                       ({72{maskf7[6]}} & rx_data[6]) | ({72{maskf7[7]}} & rx_data[7]);
  end //always_comb
  

endmodule
