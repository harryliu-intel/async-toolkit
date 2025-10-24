/* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. */
/* SPDX-License-Identifier: Apache-2.0 */

#include <stdio.h> // printf()
#include <mby_top_map.h>
#include <mby_top_map_main.h>
#include <model_c_write.h> // write_field()
#include "mby_pipeline.h"
#include "varchar.h"

// we implement the interface required of us by the model_server

void
mby_top_map_Setup     (mby_top_map       const * r,
                       mby_top_map__addr const * w)
{
    printf("Hello from the mby_top_map_Setup!\n");

    printf("field is %d\n", r->mpp[0].mgp[0].rx_ppe.parser.PARSER_PORT_CFG[7].INITIAL_PTR);

    write_field(w->mpp[0].mgp[0].rx_ppe.parser.PARSER_PORT_CFG[7].INITIAL_PTR, 12);

    printf("field is %d\n", r->mpp[0].mgp[0].rx_ppe.parser.PARSER_PORT_CFG[7].INITIAL_PTR);
}

void
mby_top_map_SendPacket(mby_top_map       const * r,
                       mby_top_map__addr const * w,
                       int                       port,
                       unsigned char           * packet,
                       unsigned int              length)
{
    // Register/Memory Maps:
    mby_ppe_rx_top_map       const * const rx_top_map   = &(r->mpp[0].mgp[0].rx_ppe);
    mby_ppe_rx_top_map__addr const * const rx_top_map_w = &(w->mpp[0].mgp[0].rx_ppe);
    mby_shm_map              const * const shm_map      = &(r->mpp[0].shm);

    // Input struct:
    mbyRxMacToParser mac2par;

    // Output struct:
    mbyRxStatsToRxOut rxs2rxo;

    // RX_DATA:
    varchar_t rx_data;

    rx_data.data   = packet;
    rx_data.length = length;

    // Populate input:
    mac2par.RX_PORT   = (fm_uint32) port;

    // Call RX pipeline:
    RxPipeline(rx_top_map, rx_top_map_w, shm_map, &mac2par, &rxs2rxo);
}
