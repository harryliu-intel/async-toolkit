
#include "mby_model.h"
#include "mby_pipeline.h"
#include "mby_reg_ctrl.h"

// FIXME this is not allowed - errno should be part of the model
#include "model_server/mby_srv_errno.h"

#include <string.h>
#include <stdio.h>

// This is the persistent state of the model
static fm_uint32 regs[MBY_REGISTER_ARRAY_SIZE];

fm_status mbyResetModel(const fm_uint32 sw)
{
    if (sw != 0)
        return FM_ERR_UNSUPPORTED;

    for (int i = 0;i < MBY_REGISTER_ARRAY_SIZE;++i)
        regs[i] = mbyModelGetRegisterDefault(i*4);

    return FM_OK;
}

fm_status mbyReadReg
(
    const fm_uint32   sw,
    const fm_uint32   addr,
    fm_uint64 * const val
)
{
    if (sw != 0)
        return FM_ERR_UNSUPPORTED;
    // FM_LOG_PRINT("Read64 register addr=0x%x\n", addr);
    return mbyModelReadCSR64(regs, addr, val);
}

fm_status mbyWriteReg
(
    const fm_uint32 sw,
    const fm_uint32 addr,
    const fm_uint64 val
)
{
    if (sw != 0)
        return FM_ERR_UNSUPPORTED;
    // FM_LOG_PRINT("Write64 register addr=0x%x val=0x%llx\n", addr, val);
    return mbyModelWriteCSR64(regs, addr, val);
}

// Persistent store for RX output:
static mbyRxStatsToRxOut rxs2rxo;

fm_status mbySendPacket
(
    const fm_uint32         sw,
    const fm_uint32         port,
    const fm_byte   * const packet,
    const fm_uint32         length
)
{
    if (sw != 0)
        return FM_ERR_UNSUPPORTED;

    // Input struct:
    mbyRxMacToParser mac2par;

    // Populate input:
    mac2par.RX_DATA   = (fm_byte *) packet;
    mac2par.RX_LENGTH = (fm_uint32) length;
    mac2par.RX_PORT   = (fm_uint32) port;

    // Call RX pipeline:
    RxPipeline(regs, &mac2par, &rxs2rxo);

    return FM_OK;
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
    if (sw != 0)
        return FM_ERR_UNSUPPORTED;

    // Output struct:
    mbyTxStatsToTxMac txs2mac;

    // Call RX pipeline:
    TxPipeline(regs, &rxs2rxo, &txs2mac);

    // Populate output:
    *port   = txs2mac.TX_PORT;
    *length = txs2mac.TX_LENGTH;

    // Assert (length <= max_pkt_size) <-- REVISIT!!!

    for (fm_uint i = 0; i < max_pkt_size; i++)
        packet[i] = (i < (*length)) ? txs2mac.TX_DATA[i] : 0;

    return FM_ERR_NO_MORE; // temporary <-- REVISIT!!!
}

