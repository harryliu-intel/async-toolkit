// -*- mode:cpp -*-

// Copyright (C) 2018 Intel Corporation

#include <cmock/cmock.h>

extern "C" {
#include "mby_lpm_regs.h"
}

DECLARE_FUNCTION_MOCK3(Mock_mbyLpmGetTcamEntry, mbyLpmGetTcamEntry,
                       void (fm_uint32 *, const fm_uint16, mbyLpmTcamEntry * const));

DECLARE_FUNCTION_MOCK3(Mock_mbyLpmGetTcamSubtrie, mbyLpmGetTcamSubtrie,
                       void (fm_uint32 *, const fm_uint16, mbyLpmSubtrie * const));

DECLARE_FUNCTION_MOCK3(Mock_mbyLpmGetSubtrie, mbyLpmGetSubtrie,
                       void (fm_uint32 *, const fm_uint16, mbyLpmSubtrie * const));

DECLARE_FUNCTION_MOCK3(Mock_mbyLpmGetSubtrieStore, mbyLpmGetSubtrieStore,
                       void (fm_uint32 *, const fm_uint16, mbyLpmSubtrieStore * const));

DECLARE_FUNCTION_MOCK3(Mock_mbyLpmGetKeyMasks, mbyLpmGetKeyMasks,
                       void (fm_uint32 *, const fm_byte, mbyLpmKeyMasks * const));
