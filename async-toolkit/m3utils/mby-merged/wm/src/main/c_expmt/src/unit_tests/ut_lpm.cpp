// -*- mode:cpp -*-

// Copyright (C) 2018 Intel Corporation

// #define MBY_UNIT_TEST

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <time.h>
#include <limits.h>

/* Use C type linkage */
extern "C" {
#include "mby_pipeline.h"
#include "mby_lpm.h"
}

#include "mock_lpm_regs.h"
#include <gmock/gmock.h>
#include <gtest/gtest.h>

using namespace ::testing;

class LpmTests : public testing::Test {
	/* You should make the members protected s.t. they can be accessed from
	 * sub-classes.
	 */
	protected:
		StrictMock<Mock_mbyLpmGetTcamEntry> mock_getTcamEntry;
		StrictMock<Mock_mbyLpmGetTcamSubtrie> mock_getTcamSubtrie;
		StrictMock<Mock_mbyLpmGetSubtrie> mock_getSubtrie;
		StrictMock<Mock_mbyLpmGetSubtrieStore> mock_getSubtrieStore;

		struct mbyLpmStaticFuncs f;

		/* SetUp for all the Tests derived from LpmTests class */
		void SetUp() {
			/* This could have perf impact when we have a lot of tests */
			mbyGetLpmStaticFuncs(&f);
		}

		/* TearDown for all the Tests derived from LpmTests class */
		void TearDown() {
			/* Do any other cleanup if necessary. (Semaphores, etc) */
		}
};


/* =============== LPM TCAM tests ==================== */
#define TCAM_TEST_KEY 0x1234
static const mbyLpmTcamEntry entry_inv = {0xffffffff, 0xffffffff};
static const mbyLpmTcamEntry entry_key = {TCAM_TEST_KEY, ~(fm_uint32)TCAM_TEST_KEY};

TEST_F(LpmTests, TcamEmptyTest) {

	mbyLpmTcamLookup lookup   = {TCAM_TEST_KEY, FALSE, 0};

	EXPECT_FUNCTION_CALL(mock_getTcamEntry, (NULL, _, _))
		.Times(MBY_REG_SIZE(LPM_MATCH_TCAM))
		.WillRepeatedly(SetArgPointee<2>(entry_inv));

	f._lookUpLpmTcam(NULL, &lookup);

	EXPECT_EQ(lookup.hit_valid, FALSE);
}

TEST_F(LpmTests, TcamMatchTest) {

	mbyLpmTcamLookup lookup   = {TCAM_TEST_KEY, FALSE, 0};

	EXPECT_FUNCTION_CALL(mock_getTcamEntry, (NULL, _, _))
		.Times(2)
		.WillOnce(SetArgPointee<2>(entry_inv))
		.WillOnce(SetArgPointee<2>(entry_key));

	f._lookUpLpmTcam(NULL, &lookup);

	EXPECT_EQ(lookup.hit_valid, TRUE);
	EXPECT_EQ(lookup.hit_index, 1);
}

TEST_F(LpmTests, BitInArray) {

	fm_uint64 array[2] = {0x10, 0x4};

	EXPECT_EQ(f._getBitIn64BitsArray(array, 0), 0);
	EXPECT_EQ(f._getBitIn64BitsArray(array, 4), 1);

	EXPECT_EQ(f._getBitIn64BitsArray(array, 64 + 0), 0);
	EXPECT_EQ(f._getBitIn64BitsArray(array, 64 + 2), 1);
}

TEST_F(LpmTests, CountOnes) {

	fm_uint64 array[2] = {0x11, 0x4};

	EXPECT_EQ(f._countOneIn64BitsArray(array, 0), 0); // bit 1 is excluded!!
	EXPECT_EQ(f._countOneIn64BitsArray(array, 1), 1);
	EXPECT_EQ(f._countOneIn64BitsArray(array, 4), 1); // bit 4 is excluded!!
	EXPECT_EQ(f._countOneIn64BitsArray(array, 5), 2);

	EXPECT_EQ(f._countOneIn64BitsArray(array, 64 + 0), 2);
	EXPECT_EQ(f._countOneIn64BitsArray(array, 64 + 2), 2);
	EXPECT_EQ(f._countOneIn64BitsArray(array, 64 + 3), 3);
	EXPECT_EQ(f._countOneIn64BitsArray(array, 64 + 64), 3);
}

// =================== Subtrie exploration tests ==================
TEST_F(LpmTests, SubtrieNoHit) {

	const fm_uint16 root_ptr = 0x0;
	mbyLpmSubtrie subtrie = {root_ptr, 0x0, 0};

	const fm_byte key[1] = {0x8};
	const fm_uint16 key_len = 7;
	mbyLpmSubtrieLookup st_lookup = {key, key_len, FALSE, 0};

	// The trie is empty - no match possible
	mbyLpmSubtrieStore st_store = {{0x0, 0x0, 0x0, 0x0},
								   {0x0, 0x0, 0x0, 0x0},
								   0x0};
	EXPECT_FUNCTION_CALL(mock_getSubtrieStore, (NULL, root_ptr, _))
		.Times(1)
		.WillOnce(SetArgPointee<2>(st_store));

	f._exploreSubtrie(NULL, &subtrie, &st_lookup);

	EXPECT_EQ(st_lookup.hit_valid, FALSE);
}

TEST_F(LpmTests, SubtrieOneHit) {

	const fm_uint16 root_ptr = 0x0;
	mbyLpmSubtrie subtrie = {root_ptr, 0x0, 0};

	const fm_byte key[1] = {0x8};
	const fm_uint16 key_len = 7;
	mbyLpmSubtrieLookup st_lookup = {key, key_len, FALSE, 0};

	const fm_uint32 action_base_ptr = 0x1234;
	mbyLpmSubtrieStore st_store = {{0x2, 0x0, 0x0, 0x0},
								   {0x0, 0x0, 0x0, 0x0},
								   action_base_ptr};

	EXPECT_FUNCTION_CALL(mock_getSubtrieStore, (NULL, root_ptr, _))
		.Times(1)
		.WillOnce(SetArgPointee<2>(st_store));

	f._exploreSubtrie(NULL, &subtrie, &st_lookup);

	EXPECT_EQ(st_lookup.hit_valid, TRUE);
	EXPECT_EQ(st_lookup.hit_ptr, action_base_ptr + 0);
}
