/* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. */
/* SPDX-License-Identifier: Apache-2.0 */

#include <mby_common.h>
#include <mby_top_map.h>

void basic_fwd_init
(
    mby_ppe_rx_top_map * const rx_top_map,
    mby_shm_map        * const shm_map,
    fm_uint32                  fwd_port,
    fm_macaddr                 dmac
);
