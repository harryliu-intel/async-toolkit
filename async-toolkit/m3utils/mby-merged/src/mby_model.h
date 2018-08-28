
// -*- mode:c -*-

// Copyright (C) 2018 Intel Corporation

#ifndef _MBY_MODEL_H_
#define _MBY_MODEL_H_

#include "mby_common.h"

/* Public interfaces exposed by the MBY functional model */
fm_status mbyResetModel(fm_int sw);

fm_status mbyReadReg(fm_int sw,
                     fm_uint addr,
                     fm_uint64 *val);

fm_status mbyWriteReg(fm_int sw,
                      fm_uint addr,
                      fm_uint64 val);

fm_status mbySendPacket
(
    const fm_int          sw,
    const fm_int          port,
    const fm_byte * const packet,
    const fm_int          length
);

fm_status mbyReceivePacket
(
    const fm_int          sw,
    fm_int        * const port,
    fm_byte       * const packet,
    fm_int        * const length,
    const fm_int          maxPktSize
);

#endif /* _MBY_MODEL_H_ */
