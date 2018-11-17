///
///  INTEL CONFIDENTIAL
///
///  Copyright 2018 Intel Corporation All Rights Reserved.
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
//-----------------------------------------------------------------------------
// -- Intel Proprietary
// -- Copyright (C) 2018 Intel Corporation
// -- All Rights Reserved
//-----------------------------------------------------------------------------
// -- Author:  John M. Greth (john.greth@intel.com)
// -- Project Name: Madison Bay
// -- FUB:  MBY_EGR_SCH
// -- Module: hfi_rx_psc_rsp_checker
// -- Description: Selects a winner from 16 TCs and 16 MGPs to one lane:
// --
// -- Outputs are all flops with the exception of arb_pop which has one cell.
// -- Inputs are expected to be flopped outside the block, as they do have some noticeable load.
// -- Latency from one arb to win is one clock. There is no internal bypass path (yet).
// --
//-----------------------------------------------------------------------------

module egr_top_tb_runtime (
//TODO change width of interfaces to parameters
  output logic                                  clk,
  output logic                               arst_n);

int timeout;

initial begin
    clk = 1'b0;
    #5;
    forever #5 clk = ~clk;
end

initial begin
    arst_n <= 0;
    for (int i = 0; i < 100; i++) @(posedge clk);
    arst_n <= 1;
end

logic quiesce;
always_comb quiesce = arst_n && app_bfm.quiesce && tagring_bfm.quiesce;

initial begin
    @(posedge clk);
    @(posedge clk);
    @(posedge clk);
    // Quiesce should assert and then still be asserted 5 clocks later.
    // This protects against glitches as a single item is passed from
    // one component to another.
    while (!quiesce) begin
        while (!quiesce) @(posedge clk);
        @(posedge clk);
        @(posedge clk);
        @(posedge clk);
        @(posedge clk);
        @(posedge clk);
    end
    $display("EGR sandbox completed gracefully");
    @(posedge clk);
    @(posedge clk);
    @(posedge clk);
    $finish();
end

logic [15:0] timestamp;

initial begin
    while (1) begin
        for (int i = 0; i < 100; i++) begin
            @(posedge clk);
            timestamp = $time() / 10;
        end
        $display("Timestamp: @%d", timestamp); // TODO: add some interesting metrics here.
    end
end

initial begin
    @(posedge clk);
    @(posedge clk);
    @(posedge clk);
    for (int i = 0; i < timeout - 3; i++) @(posedge clk);
    $fatal("Error: timeout @%d", $time());
end

initial begin
    if (!$value$plusargs("TIMEOUT=%d", timeout))
        timeout = 1000;
    if (timeout == 0)
        timeout = 1000;
end

endmodule : egr_top_tb_runtime
