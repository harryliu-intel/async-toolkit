// -*- mode:c -*-

// Copyright (C) 2019 Intel Corporation

#ifndef MBY_MA_TABLE_H
#define MBY_MA_TABLE_H

#include "fm_types.h"

typedef enum mbyMaLookupEntryTypeEnum
{
    MBY_MA_LOOKUP_ENTRY_TYPE_NOTUSED      = 0,
    MBY_MA_LOOKUP_ENTRY_TYPE_PROVISIONAL  = 1,
    MBY_MA_LOOKUP_ENTRY_TYPE_DYNAMIC      = 2,
    MBY_MA_LOOKUP_ENTRY_TYPE_SECURE       = 3,
    MBY_MA_LOOKUP_ENTRY_TYPE_STATIC       = 4,
    MBY_MA_LOOKUP_ENTRY_TYPE_SECURESTATIC = 5

} mbyMaLookupEntryType;

typedef struct mbyMaTableStruct
{
    fm_byte                 _RSVD5_;
    fm_byte                 OLD_PORT;
    fm_byte                 NEW_PORT;
    mbyMaLookupEntryType    ENTRY_TYPE;
    fm_byte                 _RSVD3_;
    fm_byte                 TRIG_ID;
    fm_uint16               S_GLORT;
    fm_uint16               D_GLORT;
    fm_byte                 _RSVD2_;
    fm_bool                 _RSVD1_;
    fm_uint16               L2_DOMAIN;
    fm_uint16               VID;
    fm_uint64               MAC_ADDRESS;

} mbyMaTable;

#endif // MBY_MA_TABLE_H
