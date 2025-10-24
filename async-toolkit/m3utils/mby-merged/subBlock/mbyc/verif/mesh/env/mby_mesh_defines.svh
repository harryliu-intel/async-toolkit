// Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
// SPDX-License-Identifier: Apache-2.0

parameter NUM_GMM_COLS   = 1;
parameter NUM_REQ = 3;
parameter NUM_MSH_ROWS   = 16;
parameter NUM_MSH_COLS   = 8;

parameter WREQ_WIDTH = (4 + MSH_SEG_PTR_WIDTH + MSH_WD_SEL_WIDTH + MSH_PCLASS_WIDTH);
parameter RREQ_WIDTH = WREQ_WIDTH + MSH_ID_WIDTH;
parameter DATA_WIDTH = MSH_DATA_WIDTH + MSH_ECC_WIDTH;

