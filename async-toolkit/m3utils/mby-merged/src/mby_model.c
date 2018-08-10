

#include "mby_model.h"
#include "mby_pipeline.h"

#include <string.h>

// This is the persistent state of the model
static fm_uint32 regs[MBY_REGISTER_ARRAY_SIZE];

fm_status mbyResetModel(fm_int sw)
{
	// TODO this function must set the default register values
	FM_NOT_USED(sw);
	memset(regs, 0, sizeof(regs));
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

fm_status mbySendPacket(fm_int sw, fm_int port, fm_byte *packet,
		fm_int length) // , fm_modelSidebandData *sbData)
{
	mbyMacToParser mac2par;

	FM_NOT_USED(sw);

	mac2par.RX_DATA = packet;
	mac2par.RX_LENGTH = length;
	mac2par.RX_PORT = port;
	// TODO do we need packet metadata in MBY?
	//mac2par.PKT_META = sbData->pktMeta;
	Pipeline(regs, &mac2par);
	return FM_OK;
}

