# Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
# SPDX-License-Identifier: Apache-2.0

change_selection -replace [get_flat_cells *q_reqs_in_fin*data*]
change_selection -add [get_flat_cells *q_reqs_in_data_mux*]
change_selection -add [get_flat_cells *reqs_out_tpo*data*]

