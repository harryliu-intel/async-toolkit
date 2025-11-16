// Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
// SPDX-License-Identifier: Apache-2.0

module CELEMENT #(parameter width = 1)
                 (input [width - 1:0] in, output reg out);
always @*
if (&in) out = 1'b1;
else if (~|in) out = 1'b0;
endmodule
