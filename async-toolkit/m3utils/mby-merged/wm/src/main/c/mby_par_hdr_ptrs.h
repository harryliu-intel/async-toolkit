// -*- mode:c -*-

// Copyright (C) 2019 Intel Corporation

#ifndef MBY_PAR_HDR_PTRS_H
#define MBY_PAR_HDR_PTRS_H

#include "fm_types.h"

#define MBY_N_PARSER_PTRS  8

typedef struct mbyParserHdrPtrsStruct
{
    fm_byte          OFFSET      [MBY_N_PARSER_PTRS]; // offsets to data of interest within packet
    fm_bool          OFFSET_VALID[MBY_N_PARSER_PTRS]; // parser offset valid flags
    fm_byte          PROT_ID     [MBY_N_PARSER_PTRS]; // parser protocol IDs

} mbyParserHdrPtrs;

#endif
