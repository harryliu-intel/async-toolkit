
// -*- mode:c -*-

// Copyright (C) 2018 Intel Corporation

#ifndef _MBY_MODEL_H_
#define _MBY_MODEL_H_

#include "mby_common.h"

/* Public interfaces exposed by the MBY functional model */
fm_status mbyResetModel(const fm_uint32 sw);

fm_status mbyReadReg
(
    const fm_uint32   sw,
    const fm_uint32   addr,
    fm_uint64 * const val
);

fm_status mbyWriteReg
(
    const fm_uint32 sw,
    const fm_uint32 addr,
    const fm_uint64 val
);

fm_status mbySendPacket
(
    const fm_uint32         sw,
    const fm_uint32         port,
    const fm_byte   * const packet,
    const fm_uint32         length
);

fm_status mbyReceivePacket
(
    const fm_uint32         sw,
    fm_uint32       * const port,
    fm_byte         * const packet,
    fm_uint32       * const length,
    const fm_uint32         max_pkt_size
);

#endif /* _MBY_MODEL_H_ */
