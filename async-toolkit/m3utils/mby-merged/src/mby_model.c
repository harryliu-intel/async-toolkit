
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
    // TODO this function must set the default register values
    FM_NOT_USED(sw);

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
    FM_NOT_USED(sw);
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
    FM_NOT_USED(sw);
    // FM_LOG_PRINT("Write64 register addr=0x%x val=0x%llx\n", addr, val);
    return mbyModelWriteCSR64(regs, addr, val);
}

fm_status mbySendPacket
(
    const fm_uint32         sw,
    const fm_uint32         port,
    const fm_byte   * const packet,
    const fm_uint32         length
)
{
    FM_NOT_USED(sw);

    mbyRxMacToParser  mac2par;
    mbyTxStatsToTxMac txs2mac;

    for (fm_uint32 i = 0; i < MBY_MAX_PACKET_SIZE; i++)
        mac2par.RX_DATA[i] = (i < length) ? packet[i] : 0;

    mac2par.RX_LENGTH = length;
    mac2par.RX_PORT   = port;

    Pipeline(regs, &mac2par, &txs2mac);

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
    FM_NOT_USED(sw);

    printf("%s unimplemented - returning NO_MORE data\n", __func__);

    *port   = 0;
    *length = 0;

    return FM_ERR_NO_MORE;
}

