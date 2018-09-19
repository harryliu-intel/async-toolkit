// -*- mode:cpp -*-

// Copyright (C) 2018 Intel Corporation

// #define MBY_UNIT_TEST

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <time.h>

/* Need to specify extern "C" so that C type linkage. */
extern "C" {
#include "mby_pipeline.h"
#include "mby_common.c"
#include "mby_bitfield.c"
#include "mby_lpm_regs.c"
#include "mby_lpm.c"
}

#include <limits.h>
#include <gtest/gtest.h>

#include "mock_lpm_regs.h"

using namespace ::testing;

class LpmTests : public testing::Test {
	/* You should make the members protected s.t. they can be accessed from
	 * sub-classes.
	 */
	protected:
		NiceMock<Mock_mbyLpmGetTcamEntry> mock_mbyLpmGetTcamEntry;

		/* SetUp for all the Tests derived from LpmTests class */
		void SetUp() {
			/* This could have perf impact when we have a lot of tests */
		}

		/* TearDown for all the Tests derived from LpmTests class */
		void TearDown() {
			/* Do any other cleanup if necessary. (Semaphores, etc) */
		}
};

TEST_F(LpmTests, DummyTest) {

	/* Use assertions to check the outputs */
	EXPECT_EQ(1, 1);
}

