// -*- mode:c -*-

// Copyright (C) 2018 Intel Corporation

#include <mby_top_map.h>

#include <string.h>
#include <stdio.h>
#include <malloc.h>

#include <mby_top_map_main.h>
#include <model_c_write.h> // write_field()

#include "varchar.h"

#include "mby_model.h"
#include "mby_pipeline.h"
#include "mby_errors.h"
#include "mby_init.h"


// This is the persistent state of the model registers:
static mby_top_map top_map;

fm_status mbyResetModel
(
    mby_top_map__addr const * const w
)
{
    fm_status sts = FM_OK;

    // mbyModelLoadDefaults(w); <-- FIXME!!!

    return sts;
}

fm_status mbyInitRegs
(
    mby_top_map__addr const * const w
)
{
    fm_status sts = FM_OK;

    // mby_init_common_regs(&(w->mpp[0].mgp[0].rx_ppe), &(w->mpp[0].mgp[0].tx_ppe)); // <--- REVISIT!!!

    return sts;
}

fm_status mbyTopMapSetup
(
    mby_top_map       const * const r,
    mby_top_map__addr const * const w
)
{
    fm_status sts = FM_OK;

    printf("Hello from mbyTopMapSetup!\n");

    for (fm_uint i = 0; i < 2; i++)
    {
        printf("  PARSER_PORT_CFG[7].INITIAL_PTR = 0x%08x\n",
               r->mpp[0].mgp[0].rx_ppe.parser.PARSER_PORT_CFG[7].INITIAL_PTR);

        if (i == 0)
            write_field(w->mpp[0].mgp[0].rx_ppe.parser.PARSER_PORT_CFG[7].INITIAL_PTR, 12);
    }

    return sts;
}

// Persistent store for RX output and TX input:
// THESE NEED TO GO AWAY!
static mbyRxStatsToRxOut rxs2rxo;
static mbyTxInToModifier txi2mod;

fm_status mbySendPacket
(
    mby_top_map       const * const r,
    mby_top_map__addr const * const w,
    fm_uint32                 const port,
    fm_byte           const * const packet,
    fm_uint32                 const length
)
{
    fm_status sts = FM_OK;

    // Top CSR map for tile 0 receive pipeline:
    // TODO use the pipeline associated to the specific ingress port
    mby_ppe_rx_top_map       const * const rx_top_map   = &(r->mpp[0].mgp[0].rx_ppe);
    mby_ppe_rx_top_map__addr const * const rx_top_map_w = &(w->mpp[0].mgp[0].rx_ppe);
    mby_shm_map              const * const shm_map      = &(r->mpp[0].shm);

    // Input struct:
    mbyRxMacToParser mac2par;

    // RX_DATA:
    varchar_t rx_data;

    rx_data.data   = packet;
    rx_data.length = length;

    // Populate input:
    mac2par.RX_PORT   = port;
    mac2par.RX_LENGTH = length;

    for (fm_uint i = 0; i < MBY_PA_MAX_SEG_LEN; i++)
        mac2par.SEG_DATA[i] = (i < length) ? packet[i] : 0;

    // Call RX pipeline:
    RxPipeline(rx_top_map, rx_top_map_w, shm_map, &mac2par, &rxs2rxo);

    return sts;
}

fm_status mbyReceivePacket
(
    mby_top_map       const * const r,
    mby_top_map__addr const * const w,
    varchar_t         const * const rx_data,
    fm_uint32                 const max_pkt_size,
    fm_uint32               * const port,
    varchar_t               * const tx_data
)
{
    fm_status sts = FM_OK;

    // Top CSR map for tile 0 transmit pipeline:
    // TODO use the pipeline associated to the specific egress port
    mby_ppe_tx_top_map       const * const tx_top_map   =
      &(r->mpp[0].mgp[0].tx_ppe);
    mby_ppe_tx_top_map__addr const * const tx_top_map_w =
      &(w->mpp[0].mgp[0].tx_ppe);
    mby_shm_map              const * const shm_map      =
      &(r->mpp[0].shm);

    // Input struct:
    txi2mod.CONTENT_ADDR  = rxs2rxo.CONTENT_ADDR;
    txi2mod.DROP_TTL      = rxs2rxo.DROP_TTL;
    txi2mod.ECN           = rxs2rxo.ECN;
    txi2mod.EDGLORT       = rxs2rxo.EDGLORT;
    txi2mod.FNMASK        = rxs2rxo.FNMASK;
    txi2mod.IS_TIMEOUT    = rxs2rxo.IS_TIMEOUT;
    txi2mod.L2_DMAC       = rxs2rxo.L2_DMAC;
    txi2mod.L2_EVID1      = rxs2rxo.L2_EVID1;
    txi2mod.MARK_ROUTED   = rxs2rxo.MARK_ROUTED;
    txi2mod.MIRTYP        = rxs2rxo.MIRTYP;
    txi2mod.MOD_IDX       = rxs2rxo.MOD_IDX;
    txi2mod.MOD_PROF_IDX  = rxs2rxo.MOD_PROF_IDX;
    txi2mod.NO_MODIFY     = rxs2rxo.NO_MODIFY;
    txi2mod.OOM           = rxs2rxo.OOM;
    txi2mod.PARSER_INFO   = rxs2rxo.PARSER_INFO;
    txi2mod.PA_HDR_PTRS   = rxs2rxo.PA_HDR_PTRS;
    txi2mod.PM_ERR        = rxs2rxo.PM_ERR;
    txi2mod.PM_ERR_NONSOP = rxs2rxo.PM_ERR_NONSOP;
    txi2mod.QOS_L3_DSCP   = rxs2rxo.QOS_L3_DSCP;
    txi2mod.SAF_ERROR     = rxs2rxo.SAF_ERROR;
    txi2mod.TAIL_CSUM_LEN = rxs2rxo.TAIL_CSUM_LEN;
    txi2mod.TX_DROP       = rxs2rxo.TX_DROP;
    txi2mod.TX_TAG        = rxs2rxo.TX_TAG;
    txi2mod.XCAST         = rxs2rxo.XCAST;

    // Output struct:
    mbyTxStatsToTxMac txs2mac;

    // Call RX pipeline:
    varchar_builder_t txd_builder;
    varchar_builder_init(&txd_builder, tx_data, malloc, free);

    TxPipeline(tx_top_map,
               tx_top_map_w,
               shm_map,
               rx_data,
               &txi2mod,
               &txs2mac,
               &txd_builder);

    // Populate output:
    *port   = txs2mac.TX_PORT;

    return sts;
}
