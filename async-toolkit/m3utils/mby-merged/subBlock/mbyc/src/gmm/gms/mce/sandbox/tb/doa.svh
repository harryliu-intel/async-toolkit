// Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
// SPDX-License-Identifier: Apache-2.0

begin
i_mc_deep_q_wr = 1;
i_mc_deep_q_rx_port_id[7] = 11;
delay_cclk(1);
i_mc_deep_q_wr = 0;
end
