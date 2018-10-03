
#include "mby_model.h"
#include "mby_pipeline.h"
#include "mby_reg_ctrl.h"
#include "mby_errors.h"
#include <mby_init.h>

#include <string.h>
#include <stdio.h>

// This is the persistent state of the model registers:
#ifdef USE_NEW_CSRS
static mby_top_map top_map;
#else
static fm_uint32   regs[MBY_REGISTER_ARRAY_SIZE];
#endif

fm_status mbyResetModel(const fm_uint32 sw)
{
    fm_status sts = FM_OK;

    if (sw != 0)
        sts = FM_ERR_UNSUPPORTED;
    else
    {
#ifdef USE_NEW_CSRS
        // mbyModelLoadDefaults(top_map); <-- FIXME!!!
#else
        mbyModelLoadDefaults(regs);
#endif
    }
    return sts;
}

fm_status mby_init_regs(const fm_uint32 sw)
{
    fm_status sts = FM_OK;

    if (sw != 0)
        sts = FM_ERR_UNSUPPORTED;
    else
    {
#ifdef USE_NEW_CSRS
        // mby_init_common_regs(top_map); <-- FIXME!!!
#else
        mby_init_common_regs(regs);
#endif
    }
    return sts;
}

fm_status mbyReadReg
(
    const fm_uint32   sw,
    const fm_uint32   addr,
    fm_uint64 * const val
)
{
    fm_status sts = FM_OK;

    if (sw != 0)
        sts = FM_ERR_UNSUPPORTED;
    else
    {
        // FM_LOG_PRINT("Read64 register addr=0x%x\n", addr);
#ifdef USE_NEW_CSRS
        // FIXME!!!
#else
        sts = mbyModelReadCSR64(regs, addr, val);
#endif
    }
    return sts;
}

fm_status mbyWriteRegMult
(
    const fm_uint32 sw,
    const fm_uint32 addr,
    const fm_int len,
    const fm_uint32 * const val
)
{
    fm_status sts = FM_OK;

    if (sw != 0)
        sts = FM_ERR_UNSUPPORTED;
    else
    {
#ifdef USE_NEW_CSRS
        // FIXME!!!
#else
        sts = mbyModelWriteCSRMult(regs, addr, len, val);
#endif
    }
    return sts;
}

fm_status mbyWriteReg
(
    const fm_uint32 sw,
    const fm_uint32 addr,
    const fm_uint64 val
)
{
    fm_status sts = FM_OK;

    if (sw != 0)
        sts = FM_ERR_UNSUPPORTED;
    else
    {
        // FM_LOG_PRINT("Write64 register addr=0x%x val=0x%llx\n", addr, val);
#ifdef USE_NEW_CSRS
        // FIXME!!!
#else
        sts = mbyModelWriteCSR64(regs, addr, val);
#endif
    }
    return sts;
}

// Persistent store for RX output and TX input:
static mbyRxStatsToRxOut rxs2rxo;
static mbyTxInToModifier txi2mod;

fm_status mbySendPacket
(
    const fm_uint32         sw,
    const fm_uint32         port,
    const fm_byte   * const packet,
    const fm_uint32         length
)
{
    fm_status sts = FM_OK;

    if (sw != 0)
        sts = FM_ERR_UNSUPPORTED;
    else
    {
    // Top CSR map for tile 0 receive pipeline:
#ifdef USE_NEW_CSRS
        mby_ppe_rx_top_map * const rx_top_map = &(top_map.mpt[0].rx_ppe);
        mby_shm_map        * const shm_map    = &(top_map.shm);
#endif
        // Input struct:
        mbyRxMacToParser mac2par;

        // Populate input:
        mac2par.RX_DATA   = (fm_byte *) packet;
        mac2par.RX_LENGTH = (fm_uint32) length;
        mac2par.RX_PORT   = (fm_uint32) port;

        // Call RX pipeline:
#ifdef USE_NEW_CSRS
        RxPipeline(rx_top_map,
                   shm_map,    &mac2par, &rxs2rxo);
#else
        RxPipeline(regs,       &mac2par, &rxs2rxo);
#endif
    }
    return sts;
}

fm_status mbyReceivePacket
(
    const fm_uint32         sw,
    fm_uint32       * const port,
    fm_byte         * const packet,
    fm_uint32       * const length,
    const fm_uint32         max_pkt_size
)
{
    fm_status sts = FM_OK;

    if (sw != 0)
        sts = FM_ERR_UNSUPPORTED;
    else
    {
        // Top CSR map for tile 0 transmit pipeline:
#ifdef USE_NEW_CSRS
        mby_ppe_tx_top_map * const tx_top_map = &(top_map.mpt[0].tx_ppe);
#endif
        // Input struct:
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
        txi2mod.NO_MODIFY     = rxs2rxo.NO_MODIFY;
        txi2mod.OOM           = rxs2rxo.OOM;
        txi2mod.PARSER_INFO   = rxs2rxo.PARSER_INFO;
        txi2mod.PM_ERR        = rxs2rxo.PM_ERR;
        txi2mod.PM_ERR_NONSOP = rxs2rxo.PM_ERR_NONSOP;
        txi2mod.QOS_L3_DSCP   = rxs2rxo.QOS_L3_DSCP;
        txi2mod.RX_DATA       = rxs2rxo.RX_DATA;
        txi2mod.RX_LENGTH     = rxs2rxo.RX_LENGTH;
        txi2mod.SAF_ERROR     = rxs2rxo.SAF_ERROR;
        txi2mod.TAIL_CSUM_LEN = rxs2rxo.TAIL_CSUM_LEN;
        txi2mod.TX_DATA       = packet; // points at provided buffer
        txi2mod.TX_DROP       = rxs2rxo.TX_DROP;
        txi2mod.TX_TAG        = rxs2rxo.TX_TAG;
        txi2mod.XCAST         = rxs2rxo.XCAST;

        // Output struct:
        mbyTxStatsToTxMac txs2mac;

        // Call RX pipeline:
#ifdef USE_NEW_CSRS
        TxPipeline(tx_top_map, &txi2mod, &txs2mac);
#else
        TxPipeline(regs,       &txi2mod, &txs2mac);
#endif

        // Populate output:
        *port   = txs2mac.TX_PORT;
        *length = txs2mac.TX_LENGTH;

        // assert (length <= max_pkt_size) <-- REVISIT!!!
    }
    return sts;
}
