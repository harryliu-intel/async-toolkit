// Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
// SPDX-License-Identifier: Apache-2.0

`ifndef _TSU_PKG_VH
`define _TSU_PKG_VH
package tsu_pkg;

  localparam FCLK_DIV_BITS = 3;
  localparam PHASE_B_WIDTH = 32;

  typedef struct packed {
    logic                          marker_v;
    logic [PHASE_B_WIDTH -1:0]     phase_b;
  } info;


endpackage

`endif
