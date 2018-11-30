// -*- mode:cpp -*-

// Copyright (C) 2018 Intel Corporation

#include "mock_lpm_regs.h"

IMPLEMENT_FUNCTION_MOCK3(Mock_mbyLpmGetTcamEntry, mbyLpmGetTcamEntry,
 						 void (mby_ppe_cgrp_a_map * const, const fm_uint16,
							   mbyLpmTcamEntry * const));

IMPLEMENT_FUNCTION_MOCK3(Mock_mbyLpmGetTcamSubtrie, mbyLpmGetTcamSubtrie,
						 void (mby_ppe_cgrp_a_map * const, const fm_uint16,
							   mbyLpmSubtrie * const));

IMPLEMENT_FUNCTION_MOCK3(Mock_mbyLpmGetSubtrie, mbyLpmGetSubtrie,
						 void (mby_ppe_cgrp_a_map * const, const fm_uint16,
							   mbyLpmSubtrie * const));

IMPLEMENT_FUNCTION_MOCK3(Mock_mbyLpmGetSubtrieStore, mbyLpmGetSubtrieStore,
						 void (mby_ppe_cgrp_a_map * const, const fm_uint16,
							   mbyLpmSubtrieStore * const));

IMPLEMENT_FUNCTION_MOCK3(Mock_mbyLpmGetKeySels, mbyLpmGetKeySels,
						 void (mby_ppe_cgrp_a_map * const, const fm_byte,
							   mbyLpmKeySels * const));
