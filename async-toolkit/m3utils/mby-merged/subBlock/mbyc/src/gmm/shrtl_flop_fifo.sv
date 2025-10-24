
///
///  INTEL CONFIDENTIAL
///
///  Copyright 2015 Intel Corporation All Rights Reserved.
///
///  The source code contained or described herein and all documents related
///  to the source code ("Material") are owned by Intel Corporation or its
///  suppliers or licensors. Title to the Material remains with Intel
///  Corporation or its suppliers and licensors. The Material contains trade
///  secrets and proprietary and confidential information of Intel or its
///  suppliers and licensors. The Material is protected by worldwide copyright
///  and trade secret laws and treaty provisions. No part of the Material may
///  be used, copied, reproduced, modified, published, uploaded, posted,
///  transmitted, distributed, or disclosed in any way without Intel's prior
///  express written permission.
///
///  No license under any patent, copyright, trade secret or other intellectual
///  property right is granted to or conferred upon you by disclosure or
///  delivery of the Materials, either expressly, by implication, inducement,
///  estoppel or otherwise. Any license under such intellectual property rights
///  must be express and approved by Intel in writing.
///
// -------------------------------------------------------------------
// --                      Intel Proprietary
// --              Copyright (C) 2013 Intel Corporation
// --                    All Rights Reserved
// -------------------------------------------------------------------

//
//    File:           shrtl_flop_fifo.v
//    Function:       DEPTHxWIDTH  flop fifo 
//    Developed by:  bruce.pirie@intel.com
//
//    Description:    DEPTHxWIDTH  flop fifo with registered o_empty, o_full, o_cnt, o_rd_data 
//                    pushes of full  fifos without simultaneous pop generate o_overflow but are otherwise ignored
//                    pops   of empty fifos generate o_underflow but are otherwise ignored
//                    DEPTH does not need to be power of 2
//                    DEPTH must be >= 1
//                    In addition, there are multiple parameters that can help timing.
//Features:  
//  - input i_upstream_empty, o_pull
//    These allow  placing a series of 1 or 2 deep fifos to form a pipeline that 'pulls' data along allowing the pipeline control to be at the end of the pipeline.
//  - built-in coverage assertions and assert_fifo assertion inside included file, shrtl_flop_fifo.sva
//  - can induce data faults with same fault injection style as in fake_ram.v, fake_reg.v, cray_flop_ram.v.
//    useful for testing fifos with parity or ecc.
//
//  - discussion of ALLOW_PUSHING_WHEN_FULL_AND_POPPING parameter
//     ALLOW_PUSHING_WHEN_FULL_AND_POPPING  q_r_full i_push   i_pop  o_overflow        result
//     -----------------------------------  -------- ------   -----  ---------        ------------------------------------
//                                      0       0      1       x       0              normal, input data is pushed
//                                      0       1      1       x       1              input data is dropped whether or not we are also popping
//                                      1       0      1       x       0              normal, input data is pushed
//                                      1       1      1       0       1              input data is dropped because we are not also popping
//                                      1       1      1       1       0              input data is pushed  because we are     also popping
//     There are timing penalties when ALLOW_PUSHING_WHEN_FULL_AND_POPPING is 1 because the internal push is also a function of pop. So if
//     pop is slow, that spills over into the push.
//  - all outputs except o_overflow, o_underflow, o_pull registered directly from flops
//  - o_overflow output that is true when full fifo is being pushed and not popped
//  - o_underflow output that is true when empty fifo is popped
//  - main fifo rdindex, wrindex rollover:
//    for non-power-of-2 main fifos, rd_ptr, wr_ptr do an 'and' reduction of the 1 bits of the max index 
//    instead of examining every bit. This takes advantage of the fact that the first (and only) time all of the 1 bits
//    in the max index are 1 in the rd/wr index is when it is time to roll over.
//    for power-of-2 main fifos, the index naturally rolls over to 0 when incremented.
//    The logic for the 2 flavors are a function of parameter POWER_OF_2, so no gates for the other flavor will be synthesized.
//  -  REPLICATE_FULL_INTERNALLY parameter that will replicate the full flop for each of DEPTH-1 words of the main fifo and each
//     of NCOPIES_RD_DATA words of the one deep fifo 
//  - overflow and underflow conditions leave the fifo unchanged.
//  - optional multiple registered copies of o_full for timing with NBITS_FULL parameter
//  - optional multiple registered copies of o_empty for timing with NBITS_EMPTY parameter
//  - optional multiple registered copies of o_rd_data for timing with NCOPIES_RD_DATA parameter
//
//  - DEPTH doesn't have to be a power of 2
//  - only (DEPTH-1)*WIDTH  + WIDTH*NCOPIES_RD_DATA data flops are used for storage
//  - all flops are clk gated
//if DEPTH > 2
//  - consists of a DEPTH-1 main fifo feeding a 1 deep register fifo for a total of a DEPTH fifo.
//  - main_fifo is clk-gated per/word write, one-deep fifo is clk gated per one-deep write
//    which makes the clocking rate of the main fifo ram 
//    = write_rate/(DEPTH-1) vs. 100% for an ungated flop ram.
//  - if the fifo is empty or holds 1 and is being popped, the DEPTH-1 main fifo is bypassed 
//    and the pushed data goes directly into the 1-deep fifo. This gives 1 clk latency through the fifo.
//if DEPTH == 2
//  - consists of a 1 deep main fifo feeding a 1 deep register fifo for a total of a 2 deep fifo.
//  - main_fifo is clk-gated per/word write, one-deep fifo is clk gated per one-deep write
//  - if the fifo is empty or holds 1 and is being popped, the 1 deep main fifo is bypassed 
//    and the pushed data goes directly into the 1-deep fifo. This gives 1 clk latency through the fifo.
//  - no read addr or write addr regs for 1 deep main fifo
//if DEPTH == 1
//  -  no main fifo and associated regs are generated
//  - only WIDTH * NCOPIES_RD_DATA flops are generated for o_rd_data
//    plus 1 flop to indicate for  o_cnt, plus NBITS_EMPTY flops for o_empty, plus NBITS_FULL flops for o_full

//  sample instantiation:
// shrtl_flop_fifo  
// #(.DEPTH(16),.WIDTH(80))
// XX_fifo (
// /* input  logic                                   */ .shclk            (),
// /* input  logic                                   */ .i_rst            (),
//                                                                        
// /* input  logic [WIDTH-1:0]                       */ .i_wr_data        (),
// /* input  logic                                   */ .i_push           (XX_fifo_push),
//                                                                        
// /* input                                          */ .i_pop            (XX_fifo_pop),
// /* output logic [NCOPIES_RD_DATA-1:0][WIDTH-1:0]  */ .o_rd_data        (),
//                                                             
// /* output logic [NBITS_FULL-1:0]                  */ .o_full           (XX_fifo_full),
// /* output logic [shrtl_nbits(DEPTH)-1:0]           */ .o_cnt            (XX_fifo_cnt),      // useful for debugging if goes to RO mmr
// /* output logic                                   */ .o_overflow        (XX_fifo_overflow ), // true only during push of full fifo without simultaneous pop
// /* output logic                                   */ .o_underflow       (XX_fifo_underflow ), // true only during pop  of empty fifo
// /* output logic [NBITS_EMPTY-1:0]                 */ .o_empty          (XX_fifo_empty),
                                                     
// /* input  logic                                   */ .i_upstream_empty (), // if self-moving pipeline stage, connect to o_empty of upstream stage, else tie to 1 or 0.
// /* output logic                                   */ .o_pull           ()  // if self-moving pipeline stage, connect to i_pop of upstream stage and i_push of this stage.
// );
  
//
//
// ===============================================
// USING a sequence of 1 deep shrtl_flop_fifos to form a pipeline that downstream 1-deep fifos pull data from their non-empty upstream neighbor when not full or popping.
// 
// This scheme allows the entire pipeline to automatically advance 1 stage based on control logic at the end of the pipeline. The example below shows the control at the output
// of a block with the ouput fed by a pipeline of processing. Having the control at the output of a block allows the round-trip credit requirements of the downstream blocks input
// fifo to be minimal. The downstream input fifo only needs to be 3-5 deep to cover the round-trip credit latency.
// If you look at o_pull output,
// always_comb o_pull = !i_upstream_empty
//                    &( DEPTH == 1 ? (valid_pop | !o_full[0])  // valid_pop snakes through all the contiguous 1-deep fifo stages through o_pull outputs which go to i_pop inputs  until 
//                                                              // it gets to start of pipeline or hits a non-1-deep fifo.
//                                  :              !o_full[0]); // DEPTH > 1 breaks the valid_pop path which snakes through all contiguous 1-deep pipeline stages

// NOTE 1. You see that for a contiguous set of 1-deep pipeline stages, the last stage i_pop forms o_pull which goes to upstream stage i_pop which forms that stages o_pull, etc.
//         all the way up to the head stage of the contiguous set of 1-deep pipeline stages. So the last i_pop snakes through all the other stages.
//         If this is a timing problem for the upper stages, you can break this chain by making an intermediate stage a 2-deep fifo. See the logic above.
//         You should consider fifo widths as well as position when deciding what stage or stages you make 2-deep for timing, trying to minimize flops also.
// NOTE 2. For sub-block to sub-block connections, you can dispense with an input fifo and credit scheme. 
//         upstream to downstream signal:
//           o_empty (flop output) to i_upstream_empty. 
//           o_rd_data (flop output) to i_wr_data
//         downstream to upstream signal
//           o_pull to i_pop
//            o_pull is either snaked i_pop if first stage of downstream sub-block is 1-deep
//            or, if 2+-deep,  this simple combinatorial of two flops (!i_upstream_empty (upstream flop) & !o_full (local flop))



// Here is a sample hookup of a 16-deep fifo feeding 3 1-deep pipeline stage fifos that feed a registered block output stage.
// Notice in the example how the widths of each stage are different reflecting different data widths at each pipeline stage.
//  shrtl_flop_fifo
//  #(.DEPTH(16),.WIDTH(100))
//  main_fifo (
//  /* input  logic [WIDTH-1:0]                      */ .i_wr_data        (wr_data),
//  /* input  logic                                  */ .i_push           (main_fifo_push),
//  /* input                                         */ .i_pop            (pull[1]),
//  /* output logic [NCOPIES_RD_DATA-1:0][WIDTH-1:0] */ .o_rd_data        (data_0),
//  /* output logic [NBITS_EMPTY-1:0]                */ .o_empty          (empty[0]),
//  /* input  logic                                  */ .i_upstream_empty (1'b0), // used for pipeline implementations, tie off to 1 or 0 if not pipeline implementation.
//  /* output logic                                  */ .o_pull           ()// used for pipeline implementations
//  );
//  
//  data_1_in = stage processed data_0; // pipeline processing stage 1

//  // pipe stage 1, begin pipe stage
//  shrtl_flop_fifo
//  #(.DEPTH(1),.WIDTH(125),.ALLOW_PUSHING_WHEN_FULL_AND_POPPING(1))
//  fifo_p1 (
//  /* input  logic [WIDTH-1:0]                      */ .i_wr_data        (data_1_in),
//  /* input  logic                                  */ .i_push           (pull[1]),
//  /* input                                         */ .i_pop            (pull[2]),
//  /* output logic [NCOPIES_RD_DATA-1:0][WIDTH-1:0] */ .o_rd_data        (data_1_out),
//  /* output logic [NBITS_EMPTY-1:0]                */ .o_empty          (empty[1]),
//  /* input  logic                                  */ .i_upstream_empty (empty[0]), // used for pipeline implementations, tie off to 1 or 0 if not pipeline implementation.
//  /* output logic                                  */ .o_pull           (pull[1])// used for pipeline implementations
//  );
//  
//  data_2_in = stage processed data_1_out; // pipeline processing stage 2

//  // pipe stage 2, begin pipe stage
//  shrtl_flop_fifo
//  #(.DEPTH(1),.WIDTH(146),.ALLOW_PUSHING_WHEN_FULL_AND_POPPING(1))
//  fifo_p2 (
//  /* input  logic [WIDTH-1:0]                      */ .i_wr_data        (data_2_in),
//  /* input  logic                                  */ .i_push           (pull[2]),
//  /* input                                         */ .i_pop            (pull[3]),
//  /* output logic [NCOPIES_RD_DATA-1:0][WIDTH-1:0] */ .o_rd_data        (data_2_out),
//  /* output logic [NBITS_EMPTY-1:0]                */ .o_empty          (empty[2]),
//  /* input  logic                                  */ .i_upstream_empty (empty[1]), // used for pipeline implementations, tie off to 1 or 0 if not pipeline implementation.
//  /* output logic                                  */ .o_pull           (pull[2])// used for pipeline implementations
//  );
//  
//  data_3_in = stage processed data_2_out; // pipeline processing stage 3

//  // pipe stage 3, begin pipe stage
//  shrtl_flop_fifo
//  #(.DEPTH(1),.WIDTH(172),.ALLOW_PUSHING_WHEN_FULL_AND_POPPING(1))
//  fifo_p3 (
//  /* input  logic [WIDTH-1:0]                      */ .i_wr_data        (data_3_in),
//  /* input  logic                                  */ .i_push           (pull[3]),
//  /* input                                         */ .i_pop            (pull[4]),
//  /* output logic [NCOPIES_RD_DATA-1:0][WIDTH-1:0] */ .o_rd_data        (data_3_out),
//  /* output logic [NBITS_EMPTY-1:0]                */ .o_empty          (empty[3]),
//  /* input  logic                                  */ .i_upstream_empty (empty[2]), // used for pipeline implementations, tie off to 1 or 0 if not pipeline implementation.
//  /* output logic                                  */ .o_pull           (pull[3])// used for pipeline implementations
//  );
//  
//  output_data = stage processed data_3_out; // pipeline processing stage 4

//  // output stage
//  always_comb  pull[4] = !empty[3]
//                       && (  ( q_downstream_credits_used <  max_credits)
//                          || ((q_downstream_credits_used == max_credits) && q_credit_ack_in) // this saves a shclk cycle for roundtrip credit path
//                          );                                                               // reducing downstream fifo minimum depth by 1 for 100% thruput.
//  end
//  always_ff @(posedge shclk) begin
//    o_valid                     <= pull[4];
//    if (pull[4]) o_data         <= output_data;
//    if (i_rst) begin
//      q_credit_ack_in           <= '0;
//      q_downstream_credits_used <= '0;
//    end else begin
//      q_credit_ack_in           <= i_credit_ack_in;
//      q_downstream_credits_used <= q_downstream_credits_used
//                                 + pull[4]
//                                 - q_credit_ack_in;
//    end
//  end


// 
// ===============================================
// lintra push -52523, -0241, -0305

`include "shrtl_nbits_pkg.vh"

module  shrtl_flop_fifo
import shrtl_nbits_pkg::*;
#(parameter DEPTH=16,
            WIDTH=80,
            REPLICATE_FULL_INTERNALLY=0,            // if you see a timing problem from q_internal_full, set this to 1 which will replicate the full flop
                                                    // internally, 1 copy for each word of the main fifo plus 1 copy for each copy of o_rd_data plus
                                                    // 1 copy for o_cnt
            ALLOW_PUSHING_WHEN_FULL_AND_POPPING=0,  // allows pushing full fifo when popping. see discussion above
            NCOPIES_RD_DATA=1,                      // multiple registered copies of rd_data. see discussion above
            NBITS_FULL=1,                           // allows more than 1 o_full register for timing
            NBITS_EMPTY=1,                          // allows more than 1 o_empty register for timing
            RESET_DATA=0,                           // set to 1 if you want the data path reset
            VALUE_CHK  = 1                          // 0 allows turning off value chk in the assert_fifo assertion
                                                    // sometimes x's are intentionally written

)
(
input  logic                               shclk,
input  logic                               i_rst,

input  logic [WIDTH-1:0]                   i_wr_data,
input  logic                               i_push,
                                           
input  logic                               i_pop,
output logic  [NCOPIES_RD_DATA-1:0][WIDTH-1:0] o_rd_data,
            
output logic [NBITS_FULL-1:0]              o_full,
output logic [shrtl_nbits(DEPTH)-1:0]      o_cnt,      // useful for debugging if goes to RO mmr
output logic                               o_overflow,  // true during push of full fifo without simultaneous pop
output logic                               o_underflow, // true during pop of empty fifo
output logic [NBITS_EMPTY-1:0]             o_empty,
  
input  logic                               i_upstream_empty, // if self-moving pipeline stage, connect to o_empty of upstream stage, else tie to 1 or 0.
output logic                               o_pull            // if self-moving pipeline stage, connect to i_pop of upstream stage and i_push of this stage.
);
`include "shrtl_pwr2.svh"

localparam CNT_WIDTH = shrtl_nbits(DEPTH);
localparam CNT_MSB  = shrtl_nbits(DEPTH)-1;   // cnt  max value is DEPTH
localparam ADDR_MSB = shrtl_nbits(DEPTH-2)-1; // main fifo address max value is DEPTH - 2
localparam MAIN_FIFO_DEPTH = DEPTH > 0 ? DEPTH-1         // main fifo depth, could be 0 if DEPTH == 1
                                       : 0;
localparam MAIN_FIFO_MAX_INDEX = DEPTH-2;
localparam POWER_OF_2 = shrtl_pwr2(MAIN_FIFO_DEPTH);
localparam MAX_CNT_MINUS_1 = DEPTH-1;
localparam TWO = 2;
localparam NUMBER_OF_FULL_USES =  1                // one misc. for cnt, underflow, overflow, etc.
                               +  NCOPIES_RD_DATA  // one for each copy of o_rd_data for clk gating
                               +  MAIN_FIFO_DEPTH; // one for each word of main fifo for clk gating
localparam FULL_MISC = NUMBER_OF_FULL_USES-1;      // this copy is used for overflow, count etc.
localparam INTERNAL_COPIES_FULL = REPLICATE_FULL_INTERNALLY == 0 ? 1   // only one copy of q_internal_full
                                                                 : NUMBER_OF_FULL_USES; // a copy for each use
// for NUMBER_OF_FULL_USES-1:0 range
// most significant, FULL_MISC, services misc. cnt, overflow, underflow
// FULL_MISC-1:MAIN_FIFO_DEPTH, services NCOPIES_RD_DATA  copies of o_rd_data for the 1-depp
// MAIN_FIFO_DEPTH-1:0          services each word of main fifo (assuming DEPTH > 1)
logic [NUMBER_OF_FULL_USES-1:0] internal_full;
logic [NUMBER_OF_FULL_USES-1:0] valid_push;
logic [INTERNAL_COPIES_FULL-1:0] q_internal_full;
// arm_translate_off
generate
  if   (INTERNAL_COPIES_FULL == 1) begin: ONE_INTERNAL_FULL
    // only one internal full flop needed
    always_comb internal_full = {NUMBER_OF_FULL_USES{q_internal_full}};
  end else begin: MULTIPLE_INTERNAL_FULL
    // 1 internal flop per use of full
    always_comb internal_full = q_internal_full;
  end
endgenerate
// arm_translate_on
                             
logic    full_nxt,empty_nxt;
logic    [CNT_MSB:0] q_cnt;
logic     [NCOPIES_RD_DATA-1:0][WIDTH-1:0] q_one_deep_fifo_data;
logic    [WIDTH-1:0] one_deep_fifo_data_nxt;
logic    [NCOPIES_RD_DATA-1:0] push_one_deep_fifo;
logic    valid_pop;
always_comb valid_pop     = i_pop  &  ~o_empty[0];

//  full clocking
logic q_reset;
always_ff @ (posedge shclk) begin
  if (q_reset | i_rst) q_reset <= i_rst; // clk gate
  if (q_reset) begin
    q_internal_full <= {INTERNAL_COPIES_FULL{1'b0}};
    o_full          <= {NBITS_FULL{1'b0}};
    o_empty         <= {NBITS_EMPTY{1'b1}};
  end else begin
    if (valid_push[FULL_MISC] != valid_pop) begin
      q_internal_full <= {INTERNAL_COPIES_FULL{full_nxt}};
      o_full          <= {NBITS_FULL{full_nxt}};
      o_empty         <= {NBITS_EMPTY{empty_nxt}};
    end
  end
end

always_comb o_pull = !i_upstream_empty
                   &( DEPTH == 1 ? (valid_pop | !o_full[0])  // valid_pop snakes through all the contiguous 1-deep fifo pipeline stages through o_pull outputs which go to i_pop inputs  until 
                                                             // it gets to start of contiguous 1-deep pipeline  stages or hits a non-1-deep fifo. 
                                                             // When DEPTH == 1, need to or in valid_pop to get 100% thruput.
                                 :              !o_full[0]); // DEPTH > 1 breaks the valid_pop path which snakes through all contiguous 1-deep
                                                             // pipeline stages
                     
always_comb o_underflow   = i_pop  &  o_empty[0];
always_comb  begin
  if (ALLOW_PUSHING_WHEN_FULL_AND_POPPING != 0) begin
    // this slows  things down but  allows us to push full fifos if also popping
    valid_push    = {NUMBER_OF_FULL_USES{i_push}} 
                  & (~internal_full 
                    | {NUMBER_OF_FULL_USES{i_pop}}
                    );
    o_overflow     = i_push & internal_full[FULL_MISC] & ~i_pop;
  end else begin
    // this speeds things up but doesn't allow us to push full fifos if also popping
    valid_push    = {NUMBER_OF_FULL_USES{i_push}} 
                  &  ~internal_full;
    o_overflow     = i_push &  internal_full[FULL_MISC];
  end
end


// generate 3 flavors of fifo which use increasingly more stuff
// 1 deep, 2 deep, >2 deep
// no use having the 1 deep and 2 deep carry extra baggage for coverage, atrenta, verplex, magma
// arm_translate_off

generate 
  if (DEPTH > 2)  begin: THREE_PLUS_DEEP
    // **************************************************************
    // **************************************************************
    // >2 deep fifo, need main flop fifo, q_main_fifo_read_addr, q_main_fifo_write_addr
    // this function only used if DEPTH > 2
    localparam ADDR_WIDTH = ADDR_MSB + 1;
    function automatic logic [ADDR_MSB:0] next_addr;
    input logic [ADDR_MSB:0] current_addr;
    begin
      if (POWER_OF_2) begin
        // incrementing naturally rolls around from max value to 0 without needing comparison
        next_addr   = current_addr 
                    + ADDR_WIDTH   ' (1'b1);
      end else begin
        // need to compare to max value
        // this method of comparing is an and-reduce of only the bits in q_main_fifo_read_addr whose value is 1 in MAIN_FIFO_MAX_INDEX
        // which is a lot more efficient than looking at both the 1 value bits AND the 0 value bits
        if ((current_addr[ADDR_MSB:0] & MAIN_FIFO_MAX_INDEX[ADDR_MSB:0]) == MAIN_FIFO_MAX_INDEX[ADDR_MSB:0]) begin
          // max addr, roll to 0
          next_addr   = '0;
        end else begin
          next_addr   = current_addr
                      + ADDR_WIDTH   ' (1'b1);
        end
      end
    end
    endfunction

    logic                 q_cnt_eq_0, q_cnt_eq_1, q_cnt_gt_1;
    logic    [ADDR_MSB:0] q_main_fifo_read_addr;
    logic    [ADDR_MSB:0] q_main_fifo_write_addr;
    
    logic [MAIN_FIFO_DEPTH-1:0]   write_addr_decode,main_fifo_wr_en;
    
    // integer expressions on rhs
    logic    [CNT_MSB:0] cnt_plus1,cnt_minus1;
    
    logic    push_main_fifo;
    logic    pop_main_fifo;
    logic [MAIN_FIFO_DEPTH-1:0][WIDTH-1:0] q_flop_ram ;
    logic    [WIDTH-1:0] main_fifo_read_data;
    always_comb  main_fifo_read_data = q_flop_ram[q_main_fifo_read_addr];
    always_comb  empty_nxt = q_cnt_eq_1 & !valid_push[FULL_MISC] & valid_pop;
    
    always_ff @(posedge shclk) begin
      // this gets us a clk gate per word
      for (int i = 0; i < MAIN_FIFO_DEPTH; i++) begin
        if (main_fifo_wr_en[i]) begin
          q_flop_ram[i] <= i_wr_data;
        end
      end
      
      if (q_reset) begin
        q_main_fifo_read_addr  <= '0;
        q_main_fifo_write_addr <= '0;
        q_cnt                  <= '0;
        q_cnt_eq_0             <= 1'b1; // these are one-hot
        q_cnt_eq_1             <= 1'b0;
        q_cnt_gt_1             <= 1'b0;
        if (RESET_DATA) q_one_deep_fifo_data <= '0;
      end else begin       
        // only need q_main_fifo_read_addr, q_main_fifo_write_addr if DEPTH > 2
        // update q_main_fifo_read_addr
        if (pop_main_fifo) begin
          q_main_fifo_read_addr <= next_addr(q_main_fifo_read_addr);
        end
        // update q_main_fifo_write_addr
        if (push_main_fifo) begin
          q_main_fifo_write_addr <= next_addr(q_main_fifo_write_addr);
        end
        if (valid_push[FULL_MISC] !=  valid_pop) begin
          
          q_cnt      <= valid_push[FULL_MISC] ? cnt_plus1 : cnt_minus1;
          q_cnt_eq_0 <= (q_cnt_eq_1 & i_pop); // going true
          q_cnt_eq_1 <= (q_cnt_eq_0 &  i_push)           // going true
                      | ((q_cnt == TWO[CNT_MSB:0]) & i_pop); // going true

          q_cnt_gt_1 <= (q_cnt_eq_1 &  i_push)           // going true
                      | (q_cnt_gt_1 & ~((q_cnt == TWO[CNT_MSB:0])  & i_pop)); // staying true
        end
        for (int push_one_deep_i = 0; push_one_deep_i < NCOPIES_RD_DATA; push_one_deep_i++) begin
          if (push_one_deep_fifo[push_one_deep_i]) begin
            // write each copy of q_rd_data
            q_one_deep_fifo_data[push_one_deep_i] <= one_deep_fifo_data_nxt;
          end
        end
      end
    end
       
//       q_cnt valid_pop valid_push  pop_main_fifo push_main_fifo push_one_deep_fifo  empty_nxt  one_deep_fifo_data_nxt
//       0        0      0              0              0             0                    1      q_one_deep_fifo_data
//       0        0      1              0              0             1                    0      i_wr_data
//       0        1      x              illegal, should not be popping empty fifo 
//                                     
//       1        0      0              0              0             0                    0      q_one_deep_fifo_data
//       1        0      1              0              1             0                    0      q_one_deep_fifo_data
//       1        1      0              0              0             0                    1      q_one_deep_fifo_data
//       1        1      1              0              0             1                    0      i_wr_data
//                                     
//       >1       0      0              0              0             0                    0      q_one_deep_fifo_data
//       >1       0      1              0              1             0                    0      q_one_deep_fifo_data
//       >1       1      0              1              0             1                    0      main_fifo_read_data
//       >1       1      1              1              1             1                    0      main_fifo_read_data
       
    always_comb  begin
      write_addr_decode                         = {MAIN_FIFO_DEPTH{1'b0}};
      write_addr_decode[q_main_fifo_write_addr] = 1'b1;
      main_fifo_wr_en = write_addr_decode
                      & valid_push[DEPTH-2:0]; // ok to write q_flop_ram[q_main_fifo_write_addr] even if i_wr_data is bypassing main fifo directly into 1 deep
    end                                        // it isn't necessary, but the logic is simpler and faster (at the cost of an extra word unnecessarily clocking).
    always_comb  push_one_deep_fifo = (valid_push[NCOPIES_RD_DATA-1+MAIN_FIFO_DEPTH:MAIN_FIFO_DEPTH] & {NCOPIES_RD_DATA{q_cnt_eq_0}}) // valid_push & q_cnt_eq_0
                                    | {NCOPIES_RD_DATA{valid_pop & !empty_nxt}} // or any pop that doesn't empty
                                    ;
    always_comb  begin
      // default values, possibly overridden below
      one_deep_fifo_data_nxt = main_fifo_read_data; // let clk gating do the hold action
      push_main_fifo         = 1'b0;
      pop_main_fifo          = 1'b0;

      case ({q_cnt_gt_1,q_cnt_eq_1,q_cnt_eq_0})
        3'b001: begin
                  // fifo o_cnt == 0
                  one_deep_fifo_data_nxt = i_wr_data;
                end
        3'b010: begin
                  // fifo o_cnt == 1
                  // we can used i_pop rather than valid_pop here since we are not empty
                  // we can used i_push rather than valid_push here since we are not full
                  //  (the fifo is >2 deep and the cnt is 1)
                  case ({i_pop,i_push})
                    2'b00: begin  // lintra s-68092
                             // defaults are good
                           end
                    2'b01: begin
                             push_main_fifo = 1'b1;
                           end
                    2'b10: begin  // lintra s-68092
                             // defaults are good
                           end
                    2'b11: begin
                             one_deep_fifo_data_nxt = i_wr_data;
                           end
                  endcase
                end
      
     // 3'b100: begin  // this is 1-hot case
       default: begin
                  // fifo o_cnt > 1
                  if (valid_push[FULL_MISC]) begin
                    push_main_fifo = 1'b1;
                  end
                  // we can used i_pop rather than valid_pop here since we are not empty
                  if (i_pop) begin
                    // i_pop from main into one_deep 
                    pop_main_fifo          = 1'b1;
                  end
                end
      endcase
    end
    
    always_comb o_cnt = q_cnt;
    always_comb begin
      // all of these will only be used if valid_push != valid_pop
      cnt_plus1  = q_cnt + 'b1;
      cnt_minus1 = q_cnt - 'b1;
      full_nxt   = 1'b0; // to prevent latches
      // the following statement will be true or false when valid_push != valid_pop reflecting what full should be.
      // when valid_push == valid_pop, full_nxt isn't used
      if (   (q_cnt  == MAX_CNT_MINUS_1[CNT_MSB:0]) &  i_push // going full
         ) begin
        full_nxt = 1'b1;
      end
    end


  end else if (DEPTH > 1 ) begin: TWO_DEEP
    // **************************************************************
    // 2 deep fifo
    // 1 deep fifo and  main flop fifo is 1 deep so doesn't need q_main_fifo_read_addr, q_main_fifo_write_addr

    logic    q_cnt_eq_0, q_cnt_eq_1, q_cnt_gt_1;
    logic    [CNT_MSB:0] cnt_nxt;
    
    logic    push_main_fifo;
    logic    [WIDTH-1:0] main_fifo_read_data;
    always_comb  empty_nxt = q_cnt_eq_1 & !valid_push[FULL_MISC] & valid_pop;
    
    always_ff @(posedge shclk) begin
     
      if (q_reset) begin
        q_cnt       <= 2'b0;

        // these 3 are one-hot
        q_cnt_eq_0    <= 1'b1;
        q_cnt_eq_1    <= 1'b0;
        q_cnt_gt_1    <= 1'b0;
        if (RESET_DATA) q_one_deep_fifo_data <= '0;
      end else begin       
        if (valid_push[FULL_MISC] !=  valid_pop) begin
          q_cnt      <= cnt_nxt;
          q_cnt_eq_0 <= cnt_nxt == 2'd0;
          q_cnt_eq_1 <= cnt_nxt == 2'd1;
          q_cnt_gt_1 <= cnt_nxt >  2'd1;
        end
        for (int push_one_deep_i = 0; push_one_deep_i < NCOPIES_RD_DATA; push_one_deep_i++) begin
          if (push_one_deep_fifo[push_one_deep_i]) begin
            // write each copy of q_rd_data
            q_one_deep_fifo_data[push_one_deep_i] <= one_deep_fifo_data_nxt;
          end
        end
      end
      if (push_main_fifo) begin
        main_fifo_read_data <= i_wr_data;
      end
    end
     
//    q_cnt valid_pop valid_push  pop_main_fifo push_main_fifo push_one_deep_fifo  empty_nxt  one_deep_fifo_data_nxt
//    0        0      0              0              0             0                    1      q_one_deep_fifo_data
//    0        0      1              0              0             1                    0      i_wr_data
//    0        1      x              illegal, valid_pop wont be 1 if empty. should not be popping empty fifo 
//                                  
//    1        0      0              0              0             0                    0      q_one_deep_fifo_data
//    1        0      1              0              1             0                    0      q_one_deep_fifo_data
//    1        1      0              0              0             0                    1      q_one_deep_fifo_data
//    1        1      1              0              0             1                    0      i_wr_data
//                                  
//    >1       0      0              0              0             0                    0      q_one_deep_fifo_data
//    >1       0      1              0              1             0                    0      q_one_deep_fifo_data
//    >1       1      0              1              0             1                    0      main_fifo_read_data
//    >1       1      1              1              1             1                    0      main_fifo_read_data
    
    always_comb  begin
      // defaults, may be overridden below
      one_deep_fifo_data_nxt = main_fifo_read_data; // let clk gating do the hold action
      push_one_deep_fifo     = (valid_push[NCOPIES_RD_DATA-1+MAIN_FIFO_DEPTH:MAIN_FIFO_DEPTH] & {NCOPIES_RD_DATA{q_cnt_eq_0}}) // valid_push & q_cnt_eq_0
                             | {NCOPIES_RD_DATA{valid_pop & !empty_nxt}} // or any pop that doesn't empty
                             ;
      push_main_fifo         = 1'b0;
      case ({q_cnt_gt_1,q_cnt_eq_1,q_cnt_eq_0})
        3'b001: begin
                  // fifo o_cnt == 0
                  one_deep_fifo_data_nxt = i_wr_data;
                end
        3'b010: begin
                  // fifo o_cnt == 1
                  // we can use i_pop rather than valid_pop here since we are not empty
                  // we can use i_push rather than valid_push here since we are not full
                  //  (the fifo is 2 deep and the cnt is 1)
                  case ({i_pop,i_push})
                    2'b00: begin  // lintra s-68092
                             // defaults are good
                           end
                    2'b01: begin
                             push_main_fifo = 1'b1;
                           end
                    2'b10: begin  // lintra s-68092
                             // defaults are good
                           end
                    2'b11: begin
                             one_deep_fifo_data_nxt = i_wr_data;
                           end
                  endcase
                end
    //  3'b100: begin this is 1-hot case
       default: begin
                  // fifo o_cnt > 1
                  if (valid_push[0]) begin
                    push_main_fifo = 1'b1;
                  end
                end
      endcase
    end

    always_comb o_cnt                   = q_cnt;
    always_comb begin
      // all of these will only be used if valid_push != valid_pop
      cnt_nxt  = q_cnt
               + 2 ' (valid_push[FULL_MISC])
               - 2 ' (valid_pop);
      full_nxt = 1'b0; // to prevent latches
      // the following statement will be true or false when valid_push != valid_pop reflecting what full should be.
      // when valid_push == valid_pop, full_nxt isn't used
      if (  (q_cnt  == MAX_CNT_MINUS_1[CNT_MSB:0]) &  i_push  // going full
         ) begin
        full_nxt = 1'b1;
      end
    end
     // end  2 deep fifo generate
  end else if (DEPTH > 0) begin: ONE_DEEP
     // **************************************************************
     // 1 deep fifo only, no main flop fifo
     logic                cnt_nxt;
     always_comb empty_nxt = ~cnt_nxt;
     always_ff @(posedge shclk) begin
     
       if (q_reset) begin
         q_cnt       <= 1'b0;
         if (RESET_DATA) q_one_deep_fifo_data <= '0;
       end else begin       
         if (valid_push[FULL_MISC] !=  valid_pop) begin
           q_cnt     <= cnt_nxt;
         end
         for (int push_one_deep_i = 0; push_one_deep_i < NCOPIES_RD_DATA; push_one_deep_i++) begin
           if (push_one_deep_fifo[push_one_deep_i]) begin
             q_one_deep_fifo_data[push_one_deep_i] <= one_deep_fifo_data_nxt;
           end
         end
       end
     end
     always_comb  begin
       one_deep_fifo_data_nxt = i_wr_data;
       push_one_deep_fifo = valid_push[NCOPIES_RD_DATA-1:0];
       //ALLOW_PUSHING_WHEN_FULL_AND_POPPING full valid_push valid_pop   cnt_nxt
       // these are the only possible combinations when ALLOW_PUSHING_WHEN_FULL_AND_POPPING = 0
       //                0                    0       0        0           0 no change
       //                0                    0       1        0           1 go to 1 (full)
       //                0                    1       0        0           1 no change
       //                0                    1       0        1           0 go to 0 (empty), note: cant have valid push if full
       // these are the only possible combinations when ALLOW_PUSHING_WHEN_FULL_AND_POPPING = 1
       // there is only one more valid combination.
       //                1                    0       0        0           0 no change
       //                1                    0       1        0           1 go to 1 (full)
       //                1                    1       0        0           1 no change
       //                1                    1       0        1           0 go to 0 (empty), note: cant have valid push if full
       //                1                    1       1        1           1 no change
       // doesn't matter what cnt_nxt is if valid_push ==  valid_pop
       // if they are !=, cnt_nxt does matter, then they are 0,1 or 1,0 so valid_push value works for cnt_nxt
       cnt_nxt  = valid_push[FULL_MISC];
       full_nxt = valid_push[FULL_MISC];
     end
     always_comb o_cnt   = q_cnt;
  end // 1 deep fifo generate
endgenerate
// arm_translate_on
  

/*--------------------------------------------------------------------------*/
`ifdef VCSSIM
`ifdef FAKE_MEM_FAULT_TEST
/*
Using the fault features of this ram.
You need to define FAKE_MEM_FAULT_TEST.
The fault capability is only enabled with this define to speed up simulation times
when not testing faults.

There are is one fault vector that can be used to induce faults on the read data.
When enabled the data in the fault vector is used to alter the read data from the mem array.
The type of fault is selectable. You can choose to flip, set or clear bits.

There are support subroutines:

set_fault_type(type); // sets what type of fault
0= no fault (initial default)
1= xor (flip bits)
2= or (set bits)
3= and (clear bits)


pattern_fault_mem (pattern, data); // patterns fault vector
(Note: The data field needs to be supplied for all patterns. Use 0 for patterns 0-2.)
0= all 0's
1= all 1's
2= user data
3= user data

*/

//VCS coverage off

logic [WIDTH-1:0] fault_rd;
logic [WIDTH-1:0] mem_fault_array ;
logic [1:0] fault_type;
initial   fault_type = 2'h0;

always_comb begin // Force a read error
  case (fault_type)
    2'h0:       fault_rd = q_one_deep_fifo_data[0];                   // no error
    2'h1:       fault_rd = q_one_deep_fifo_data[0] ^ mem_fault_array; // xor
    2'h2:       fault_rd = q_one_deep_fifo_data[0] | mem_fault_array; // or
    2'h3:       fault_rd = q_one_deep_fifo_data[0] & mem_fault_array; // and
    default:    fault_rd = q_one_deep_fifo_data[0];
  endcase
  for (int fault_i = 0; fault_i < NCOPIES_RD_DATA; fault_i++) begin
    o_rd_data[fault_i] = fault_rd;
  end
end

//Subroutines
task set_fault_type;
input [1:0] f_type;
begin
  fault_type = f_type;
end
endtask

task write_fault_entry;
input [WIDTH-1:0] data;
begin
  mem_fault_array = data;
end
endtask

function [WIDTH-1:0] read_fault_entry;
begin
  read_fault_entry = mem_fault_array;
end
endfunction

task pattern_fault_mem;
// 0 - all 0's
// 1 - all 1's
// 2 - user data
// 3 - user data
input [1:0] pattern;
input [WIDTH-1:0] data;
begin
  case (pattern)
    2'h0:     mem_fault_array = {WIDTH{1'b0}};
    2'h1:     mem_fault_array = {WIDTH{1'b1}};
    2'h2:     mem_fault_array = data;
    2'h3:     mem_fault_array = data;
    default:  mem_fault_array = {WIDTH{1'b0}};
  endcase
end
endtask

//VCS coverage on

`else // `ifdef FAKE_MEM_FAULT_TEST
always_comb o_rd_data = q_one_deep_fifo_data;
`endif

/*--------------------------------------------------------------------------*/
`else // `ifdef VCSSIM
always_comb o_rd_data = q_one_deep_fifo_data;
`endif

`ifdef VCSSIM
//`include "shrtl_flop_fifo.svh"
`ifndef SVA_OFF
assert_fifo #(.depth (DEPTH),
              .elem_sz(WIDTH),
              .msg("ASSERT ERROR. fifo"),
              .uflow_chk(1),
              .oflow_chk(1),
              .coverage_level_2 (1),
              .coverage_level_3(27),
              .value_chk   (VALUE_CHK)
             )
a__fifo      (.clk      (shclk),
              .reset_n  (q_reset === 1'b0),
              .enq      (i_push),
              .enq_data (i_wr_data),
              .deq      (i_pop),
              .deq_data (o_rd_data[0])
             );

`endif
`endif

endmodule
// lintra pop
