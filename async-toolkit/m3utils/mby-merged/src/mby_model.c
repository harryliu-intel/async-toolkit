
#include "mby_model.h"
#include "mby_pipeline.h"
#include "mby_reg_ctrl.h"

// FIXME this is not allowed - errno should be part of the model
#include "model_server/mby_srv_errno.h"

#include <string.h>
#include <stdio.h>

// This is the persistent state of the model
static fm_uint32 regs[MBY_REGISTER_ARRAY_SIZE];

fm_status mbyResetModel(fm_int sw)
{
    // TODO this function must set the default register values
    FM_NOT_USED(sw);
    for (int i = 0;i < MBY_REGISTER_ARRAY_SIZE;++i)
        regs[i] = mbyModelGetRegisterDefault(i*4);
    return FM_OK;
}

fm_status mbyReadReg(fm_int sw, fm_uint addr, fm_uint64 *val)
{
    FM_NOT_USED(sw);
    // FM_LOG_PRINT("Read64 register addr=0x%x\n", addr);
    return mbyModelReadCSR64(regs, addr, val);
}

fm_status mbyWriteReg(fm_int sw, fm_uint addr, fm_uint64 val)
{
    FM_NOT_USED(sw);
    // FM_LOG_PRINT("Write64 register addr=0x%x val=0x%llx\n", addr, val);
    return mbyModelWriteCSR64(regs, addr, val);
}

fm_status mbySendPacket
(
    const fm_int          sw,
    const fm_int          port,
    const fm_byte * const packet,
    const fm_int          length
)
{
    FM_NOT_USED(sw);

    mbyMacToParser    mac2par;
    mbyTxStatsToTxOut txs2txo;

    for (fm_uint32 i = 0; i < MBY_MAX_PACKET_SIZE; i++)
        mac2par.RX_DATA[i] = (length) ? packet[i] : 0;

    mac2par.RX_LENGTH = length;
    mac2par.RX_PORT   = port;

    Pipeline(regs, &mac2par, &txs2txo);

    return FM_OK;
}

fm_status mbyReceivePacket
(
    const fm_int          sw,
    fm_int        * const port,
    fm_byte       * const packet,
    fm_int        * const length,
    const fm_int          maxPktSize
)
{
    FM_NOT_USED(sw);

    printf("%s unimplemented - returning NO_MORE data\n", __func__);

    *port   = 0;
    *length = 0;

    return FM_ERR_NO_MORE;
}

