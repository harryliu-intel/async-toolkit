// -*- mode:cpp -*-

// Copyright (C) 2018 Intel Corporation

#include "mock_lpm_regs.h"

IMPLEMENT_FUNCTION_MOCK3(Mock_mbyLpmGetTcamEntry, mbyLpmGetTcamEntry,
 						 void (fm_uint32 *, const fm_uint16, mbyLpmTcamEntry * const));

IMPLEMENT_FUNCTION_MOCK3(Mock_mbyLpmGetTcamSubtrie, mbyLpmGetTcamSubtrie,
						 void (fm_uint32 *, const fm_uint16, mbyLpmSubtrie * const));

IMPLEMENT_FUNCTION_MOCK3(Mock_mbyLpmGetSubtrie, mbyLpmGetSubtrie,
						 void (fm_uint32 *, const fm_uint16, mbyLpmSubtrie * const));

IMPLEMENT_FUNCTION_MOCK3(Mock_mbyLpmGetSubtrieStore, mbyLpmGetSubtrieStore,
						 void (fm_uint32 *, const fm_uint16, mbyLpmSubtrieStore * const));
