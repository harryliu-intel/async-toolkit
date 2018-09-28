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
// -- Author : Hess Hodge <hess.hodge@intel.com>
// -- Project Name : Apple River
// --
// -- Function: Locking Parallel Round Robin Arbiter (LPRRA)
// --
// -- Description:
// --  Description:  Implementation of the PRRA described in:
// --  "Algorithm-Hardware Codesign of Fast Parallel Round-Robin Arbiters"
// --  IEEE Transactions of Parallel and Distributed Systems
// --  Vol. 18, No. 1, January 2007
// --  Si Qing Zheng and Mei Yang.
// --
// -- Truth Table:
// --
// -- q_locked request granted update |  o_gnt next   next
// --                  tail           |        locked head
// -- --------------------------------+-----------------
// --     0     no       *       *    |    0     0    keep      // no request and not locked, keep state
// --                                 |
// --     0     yes      0       0    |  gnt     1    o_gnt     // some request and not locked, grant and become locked
// --     0     yes      0       1    |  from    1    o_gnt     // some request and not locked, grant and become locked
// --     0     yes      1       0    |  encod-  1    o_gnt     // some request and not locked, grant and become locked
// --     0     yes      1       1    |  er      0    o_gnt+1   // some request and not locked, but tail and updated, stay unlocked and increment head
// --                                 |
// --     1     no req   *       *    |    0     1    keep      // locked and no request at head, stay locked and keep state
// --           at head               |
// --                                 |
// --     1     some     0       0    |  req     1    keep      // locked and some request at head, grant head and stay locked
// --     1     req      0       1    |  ANDed   1    keep      // locked and some request at head, grant head and stay locked
// --     1     at       1       0    |  with    1    keep      // locked and some request at head, grant head and stay locked
// --     1     head     1       1    |  q_head  0    o_gnt+1   // locked and some request at head, and tail updated, unlock and increment head
// --
// --
// -------------------------------------------------------------------

module mby_arb_lprr #(
    parameter N    = 32,
    parameter LOGN = 5
) (
    input             clk,
    input             i_rst,      // reset
    input             i_upd,      // update (flit accepted downstream)
    input     [N-1:0] i_tail,     // flit tail flags
    input     [N-1:0] i_req,      // request in
    output            o_q_locked, // arbiter is locked
    output [LOGN-1:0] o_idx,      // encoded winner
    output            o_req_vld , // request is valid
    output            o_gnt_tail, // the granted flit was a tail
    output    [N-1:0] o_gnt       // grant out
);

// keep track if arbiter is to be locked on the current grant
logic q_locked;

// registered reset
logic q_rst;

// head pointer
logic [N-1:0] q_head;

// wires for output of priority encoder
wire    [N-1:0] gnt;
wire [LOGN-1:0] idx;
wire            some_req;

// request vector ANDed with the head vector
wire [N-1:0] req_and_head = i_req & q_head;

// there is a request at the head pointer
wire a_req_at_head = |req_and_head;

// the granted flit was a tail
wire granted_tail = |{o_gnt & i_tail};

// when to rotate the head pointer
wire rotate_head = i_upd & granted_tail; // flit accepted downstream and it was a tail

// when to keep the head pointer unchanged
wire keep_head =  q_locked & ~a_req_at_head                // locked and no request at head
               |  q_locked &  a_req_at_head & ~rotate_head // locked and some request, but head pointer not rotated
               | ~q_locked & ~some_req;                    // not locked and no request

// when to lock the head pointer
wire lock_head = ~q_locked & some_req & ~rotate_head; // some request that does not rotate the head pointer

// next value of locked
wire next_locked = ~q_rst &  q_locked & keep_head  // stay locked while keeping head
                 | ~q_rst & ~q_locked & lock_head; // OR go locked when locking head

// next value of head (will be zeros on reset)
wire [N-1:0] next_head = {N{~q_rst & rotate_head}} & {o_gnt[N-2:0],o_gnt[N-1]} // rotated grant
                       | {N{~q_rst & lock_head  }} & o_gnt                     // current grant
                       | {N{~q_rst & keep_head  }} & q_head;                   // current head

// when to update head register
wire head_cg = q_rst | some_req;

// parallel priority encoder
shrtl_var_prio_enc #(
    .N       (N)
) shrtl_var_prio_enc (
    .i_req   (i_req),
    .i_hd    (q_head),
    .o_gnt   (gnt),
    .o_idx   (idx),
    .o_valid (some_req)
);

// update locked and head registers
always_ff @(posedge clk)
begin
    q_rst    <= i_rst;
    q_locked <= next_locked;

    if (head_cg) // coarse clock gating
        q_head <= next_head;
end

// assign outputs
assign o_gnt      = q_locked ? req_and_head  : gnt;      // grant request at head pointer if locked
assign o_req_vld  = q_locked ? a_req_at_head : some_req; // req valid at head pointer if locked
assign o_idx      = idx;                                 // index of granted request
assign o_q_locked = q_locked;                            // if arbiter is locked or not
assign o_gnt_tail = granted_tail;                        // granting a tail flit

endmodule : mby_arb_lprr
