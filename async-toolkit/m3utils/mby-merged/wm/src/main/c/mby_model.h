// -*- mode:c -*-

// Copyright (C) 2018 Intel Corporation

#ifndef _MBY_MODEL_H_
#define _MBY_MODEL_H_

#include "mby_common.h"
#include "varchar.h"

/* Public interfaces exposed by the MBY functional model */
fm_status mbyResetModel
(
    mby_top_map__addr const * const w
);

fm_status mbyInitRegs
(
    mby_top_map__addr const * const w
);

fm_status mbyTopMapSetup
(
    mby_top_map       const * const r,
    mby_top_map__addr const * const w
);

fm_status mbySendPacket
(
    mby_top_map       const * const r,
    mby_top_map__addr const * const w,
    fm_uint32                 const port,
    fm_byte           const * const packet,
    fm_uint32                 const length
);

fm_status mbyReceivePacket
(
    mby_top_map       const * const r,
    mby_top_map__addr const * const w,
    varchar_t         const *       rx_data,
    fm_uint32                 const max_pkt_size,
    fm_uint32               * const port,
    fm_byte                 * const packet,
    fm_uint32               * const length
);

#endif /* _MBY_MODEL_H_ */
