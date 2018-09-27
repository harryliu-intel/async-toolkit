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

	EXPECT_FUNCTION_CALL(mock_getTcamEntry, (NULL, _, _))
		.Times(MBY_REG_SIZE(LPM_MATCH_TCAM))
		.WillRepeatedly(SetArgPointee<2>(entry_inv));

	mbyLpmTcamLookup lookup = {TCAM_TEST_KEY, FALSE, 0};
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

// Empty subtrie - no match possible
const mbyLpmSubtrieStore empty_store = {{0x0, 0x0, 0x0, 0x0}, {0x0, 0x0, 0x0, 0x0}, 0x0};

// Empty subtrie but has one child - recursion when key[0] = 0x0
const mbyLpmSubtrieStore parent_store = {{0x0, 0x0, 0x0, 0x0}, {0x1, 0x0, 0x0, 0x0}, 0x0};

// Subtrie with single match when key[0] = 0x8
const fm_uint32 action_base_ptr = 0x12;
const mbyLpmSubtrieStore match_store = {{0x3, 0x0, 0x0, 0x0}, {0x0, 0x0, 0x0, 0x0},
										action_base_ptr};

const fm_uint16 root_ptr = 0x0;
const fm_uint16 child_ptr = 0x1;

// Single subtrie with no children
const mbyLpmSubtrie single_subtrie = {root_ptr, 0x0, 0};

// A parent subtrie and its child
const mbyLpmSubtrie parent_subtrie = {root_ptr, child_ptr, 1};
const mbyLpmSubtrie child_subtrie = {child_ptr, 0x10, 0};


TEST_F(LpmTests, SubtrieNoHit) {

	// Test with different key lengths: 0b to 16b
	for (fm_byte key_len=0; key_len <= 16; ++key_len)
	{
		EXPECT_FUNCTION_CALL(mock_getSubtrieStore, (NULL, root_ptr, _))
			.Times(1)
			.WillOnce(SetArgPointee<2>(empty_store));

		// Basic test - No hit anywhere since the tree is empty
		const fm_byte key[2] = {0x8, 0x10};
		mbyLpmSubtrieLookup st_lookup = {key, key_len, FALSE, 0};
		f._exploreSubtrie(NULL, &single_subtrie, &st_lookup);

		EXPECT_EQ(st_lookup.hit_valid, FALSE);
	}
}

TEST_F(LpmTests, SubtrieNoHitRecursive) {

	// Test with different key lengths: 0b to 24b
	for (fm_byte key_len=0; key_len <= 24; ++key_len)
	{
		EXPECT_FUNCTION_CALL(mock_getSubtrieStore, (NULL, root_ptr, _))
			.Times(1)
			.WillOnce(SetArgPointee<2>(parent_store));

		if (key_len >= 8)
		{
			// Fetch the child subtrie and storage
			EXPECT_FUNCTION_CALL(mock_getSubtrie, (NULL, child_ptr, _))
				.Times(1)
				.WillOnce(SetArgPointee<2>(child_subtrie));

			EXPECT_FUNCTION_CALL(mock_getSubtrieStore, (NULL, child_ptr, _))
				.Times(1)
				.WillOnce(SetArgPointee<2>(empty_store));
		}

		// Test recursion - start from parent subtrie then goes into child
		const fm_byte key[3] = {0x0, 0x0, 0x0};
		mbyLpmSubtrieLookup st_lookup = {key, key_len, FALSE, 0};
		f._exploreSubtrie(NULL, &parent_subtrie, &st_lookup);

		EXPECT_EQ(st_lookup.hit_valid, FALSE);
	}
}

TEST_F(LpmTests, SubtrieMultiHit) {

	EXPECT_FUNCTION_CALL(mock_getSubtrieStore, (NULL, root_ptr, _))
		.Times(1)
		.WillOnce(SetArgPointee<2>(match_store));

	// This will hit on both idx 0 and 1, but only the longest is returned
	const fm_byte key[1] = {0x8};
	mbyLpmSubtrieLookup st_lookup = {key, 7, FALSE, 0};
	f._exploreSubtrie(NULL, &single_subtrie, &st_lookup);

	EXPECT_EQ(st_lookup.hit_valid, TRUE);
	EXPECT_EQ(st_lookup.hit_ptr, action_base_ptr + 1);
}

TEST_F(LpmTests, SubtrieMultiHitRecursive) {

	// Test with different key lengths: 0b to 24b
	for (fm_byte key_len=0; key_len <= 24; ++key_len)
	{
		EXPECT_FUNCTION_CALL(mock_getSubtrieStore, (NULL, root_ptr, _))
			.Times(1)
			.WillOnce(SetArgPointee<2>(parent_store));

		if (key_len >= 8)
		{
			// Fetch the child subtrie and storage
			EXPECT_FUNCTION_CALL(mock_getSubtrie, (NULL, child_ptr, _))
				.Times(1)
				.WillOnce(SetArgPointee<2>(child_subtrie));

			EXPECT_FUNCTION_CALL(mock_getSubtrieStore, (NULL, child_ptr, _))
				.Times(1)
				.WillOnce(SetArgPointee<2>(match_store));
		}

		// Depending on the key len, it will hit on idx 0 or 1
		// Only the longest prefix match is returned
		const fm_byte key[3] = {0x0, 0x8, 0x24};
		mbyLpmSubtrieLookup st_lookup = {key, key_len, FALSE, 0};
		f._exploreSubtrie(NULL, &parent_subtrie, &st_lookup);

		EXPECT_EQ(st_lookup.hit_valid, key_len >= 8);
		if (key_len == 8)
			EXPECT_EQ(st_lookup.hit_ptr, action_base_ptr + 0);
		else if (key_len > 8)
			EXPECT_EQ(st_lookup.hit_ptr, action_base_ptr + 1);
	}
}
