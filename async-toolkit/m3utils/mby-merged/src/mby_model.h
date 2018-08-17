
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

fm_status mbySendPacket(fm_int sw,
                        fm_int port,
                        fm_byte
                        *packet,
                        fm_int length); //, fm_modelSidebandData *sbData);

/* TODO Implement this function...how?
fm_status hlpModelReceivePacket(fm_int sw,
                                fm_int * port,
                                fm_byte * packet,
                                fm_int * length,
                                fm_int maxPktSize,
                                fm_modelSidebandData *sbData);
*/

#endif /* _MBY_MODEL_H_ */
