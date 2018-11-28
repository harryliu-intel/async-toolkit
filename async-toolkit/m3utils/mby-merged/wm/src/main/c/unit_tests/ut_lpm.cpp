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
		StrictMock<Mock_mbyLpmGetKeySels> mock_getKeySels;

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


/* =============== Key generation tests ==================== */

#define PROFILE_ID 0x10

#define MD_KEY8_IDX 12
#define MD_KEY8_VAL 0x3d
#define MD_KEY16_IDX 9
#define MD_KEY16_VAL 0x3435

#define ADDR_KEY8_IDX 25
#define ADDR_KEY8_VAL 0xfc
#define ADDR_KEY16_IDX 7
#define ADDR_KEY16_VAL 0xabcd
#define ADDR_KEY32_IDX 13
#define ADDR_KEY32_VAL 0x87654321

#define LPM_KEY_LEN 20

static void configureKeys(mbyLpmKeySels *key_sels,
						  mbyClassifierKeysStruct *keys)
{
	key_sels->md_key16_sel 	 = 0x1 << MD_KEY16_IDX;
	key_sels->addr_key8_sel  = 0x1 << ADDR_KEY8_IDX;
	key_sels->addr_key16_sel = 0x1 << ADDR_KEY16_IDX;
	key_sels->addr_key32_sel = 0x1 << ADDR_KEY32_IDX;
	for (int i = 0; i < MBY_LPM_KEY_MAX_BYTES_LEN; ++i)
		key_sels->key_mask[i] = 0xff;

	keys->key16[MD_KEY16_IDX]   = MD_KEY16_VAL;
	keys->key8[ADDR_KEY8_IDX]   = ADDR_KEY8_VAL;
	keys->key16[ADDR_KEY16_IDX] = ADDR_KEY16_VAL;
	keys->key32[ADDR_KEY32_IDX] = ADDR_KEY32_VAL;
}

TEST_F(LpmTests, KeyGenSimple) {

    mbyLpmKeySels key_sels  = {0};
	mbyClassifierKeysStruct keys = {0};
	configureKeys(&key_sels, &keys);

	EXPECT_FUNCTION_CALL(mock_getKeySels, (NULL, PROFILE_ID, _))
		.Times(1)
		.WillRepeatedly(SetArgPointee<2>(key_sels));

	mbyLpmKey lpmKey;
	f._lpmGenerateKey(NULL, &keys, PROFILE_ID, &lpmKey);

	// Packed meta-data - starting from key MSB
	EXPECT_EQ(lpmKey.key[LPM_KEY_LEN - 1], (MD_KEY16_VAL >> 8) & 0xff);
	EXPECT_EQ(lpmKey.key[LPM_KEY_LEN - 2], MD_KEY16_VAL & 0xff);
	EXPECT_EQ(lpmKey.key[LPM_KEY_LEN - 3], 0x0);
	EXPECT_EQ(lpmKey.key[LPM_KEY_LEN - 4], 0x0);
	// Packed address
	EXPECT_EQ(lpmKey.key[LPM_KEY_LEN - 5], (ADDR_KEY32_VAL >> 24) & 0xff);
	EXPECT_EQ(lpmKey.key[LPM_KEY_LEN - 6], (ADDR_KEY32_VAL >> 16) & 0xff);
	EXPECT_EQ(lpmKey.key[LPM_KEY_LEN - 7], (ADDR_KEY32_VAL >> 8) & 0xff);
	EXPECT_EQ(lpmKey.key[LPM_KEY_LEN - 8], ADDR_KEY32_VAL & 0xff);
	EXPECT_EQ(lpmKey.key[LPM_KEY_LEN - 9], (ADDR_KEY16_VAL >> 8) & 0xff);
	EXPECT_EQ(lpmKey.key[LPM_KEY_LEN -10], ADDR_KEY16_VAL & 0xff);
	EXPECT_EQ(lpmKey.key[LPM_KEY_LEN -11], ADDR_KEY8_VAL);
	EXPECT_EQ(lpmKey.key[LPM_KEY_LEN -12], 0x0);

	EXPECT_EQ(lpmKey.key_len, 16 + 16 + 8 + 16 + 32);
}

// This is exactly like the test above, but uses also the key mask
TEST_F(LpmTests, KeyGenMasked) {

    mbyLpmKeySels key_sels  = {0};
	mbyClassifierKeysStruct keys = {0};
	configureKeys(&key_sels, &keys);
	key_sels.key_mask[LPM_KEY_LEN - 8] = 0x0;

	EXPECT_FUNCTION_CALL(mock_getKeySels, (NULL, PROFILE_ID, _))
		.Times(1)
		.WillRepeatedly(SetArgPointee<2>(key_sels));

	mbyLpmKey lpmKey;
	f._lpmGenerateKey(NULL, &keys, PROFILE_ID, &lpmKey);

	// Packed meta-data - starting from key MSB
	EXPECT_EQ(lpmKey.key[LPM_KEY_LEN - 1], (MD_KEY16_VAL >> 8) & 0xff);
	EXPECT_EQ(lpmKey.key[LPM_KEY_LEN - 2], MD_KEY16_VAL & 0xff);
	EXPECT_EQ(lpmKey.key[LPM_KEY_LEN - 3], 0x0);
	EXPECT_EQ(lpmKey.key[LPM_KEY_LEN - 4], 0x0);
	// Packed address
	EXPECT_EQ(lpmKey.key[LPM_KEY_LEN - 5], (ADDR_KEY32_VAL >> 24) & 0xff);
	EXPECT_EQ(lpmKey.key[LPM_KEY_LEN - 6], (ADDR_KEY32_VAL >> 16) & 0xff);
	EXPECT_EQ(lpmKey.key[LPM_KEY_LEN - 7], (ADDR_KEY32_VAL >> 8) & 0xff);
	EXPECT_EQ(lpmKey.key[LPM_KEY_LEN - 8], 0x0);
	EXPECT_EQ(lpmKey.key[LPM_KEY_LEN - 9], (ADDR_KEY16_VAL >> 8) & 0xff);
	EXPECT_EQ(lpmKey.key[LPM_KEY_LEN -10], ADDR_KEY16_VAL & 0xff);
	EXPECT_EQ(lpmKey.key[LPM_KEY_LEN -11], ADDR_KEY8_VAL);
	EXPECT_EQ(lpmKey.key[LPM_KEY_LEN -12], 0x0);

	EXPECT_EQ(lpmKey.key_len, 16 + 16 + 8 + 16 + 32);
}


/* =============== LPM TCAM tests ==================== */
#define TCAM_TEST_KEY 0x1234
static const mbyLpmTcamEntry entry_inv = {0xffffffff, 0xffffffff};
static const mbyLpmTcamEntry entry_key = {TCAM_TEST_KEY, ~(fm_uint32)TCAM_TEST_KEY};

TEST_F(LpmTests, TcamEmptyTest) {

	EXPECT_FUNCTION_CALL(mock_getTcamEntry, (NULL, _, _))
		.Times(mby_ppe_cgrp_a_nested_map_LPM_MATCH_TCAM__n)
		.WillRepeatedly(SetArgPointee<2>(entry_inv));

	mbyLpmTcamLookup lookup = {TCAM_TEST_KEY, FALSE, 0};
	f._lookUpLpmTcam(NULL, &lookup);

	EXPECT_EQ(lookup.hit_valid, FALSE);
}

TEST_F(LpmTests, TcamSingleMatchTest) {

	EXPECT_FUNCTION_CALL(mock_getTcamEntry, (NULL, _, _))
		.Times(mby_ppe_cgrp_a_nested_map_LPM_MATCH_TCAM__n)
		.WillOnce(SetArgPointee<2>(entry_inv))
		.WillOnce(SetArgPointee<2>(entry_key))
		.WillRepeatedly(SetArgPointee<2>(entry_inv));

	mbyLpmTcamLookup lookup = {TCAM_TEST_KEY, FALSE, 0};
	f._lookUpLpmTcam(NULL, &lookup);

	EXPECT_EQ(lookup.hit_valid, TRUE);
	EXPECT_EQ(lookup.hit_index, 1);
}

TEST_F(LpmTests, TcamMultiMatchTest) {

	EXPECT_FUNCTION_CALL(mock_getTcamEntry, (NULL, _, _))
		.Times(mby_ppe_cgrp_a_nested_map_LPM_MATCH_TCAM__n)
		.WillOnce(SetArgPointee<2>(entry_inv))
		.WillOnce(SetArgPointee<2>(entry_key)) // match but lower priority
		.WillOnce(SetArgPointee<2>(entry_inv))
		.WillOnce(SetArgPointee<2>(entry_key)) // match with highest priority
		.WillRepeatedly(SetArgPointee<2>(entry_inv));

	mbyLpmTcamLookup lookup = {TCAM_TEST_KEY, FALSE, 0};
	f._lookUpLpmTcam(NULL, &lookup);

	EXPECT_EQ(lookup.hit_valid, TRUE);
	EXPECT_EQ(lookup.hit_index, 3);
}

/* =============== Bitmap registers utilities tests ==================== */
TEST_F(LpmTests, BitInArray) {

	fm_uint64 array[2] = {0x0800000000000000, 0x2000000000000000};

	EXPECT_EQ(f._getBitIn64BitsArray(array, 0), 0);
	EXPECT_EQ(f._getBitIn64BitsArray(array, 4), 1);

	EXPECT_EQ(f._getBitIn64BitsArray(array, 64 + 0), 0);
	EXPECT_EQ(f._getBitIn64BitsArray(array, 64 + 2), 1);
}

TEST_F(LpmTests, CountOnes) {

	fm_uint64 array[2] = {0x8800000000000000, 0x2000000000000000};

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
const mbyLpmSubtrieStore parent_store = {{0x0, 0x0, 0x0, 0x0},
										 {0x8000000000000000, 0x0, 0x0, 0x0}, 0x0};

// Subtrie with single match when key[0] = 0x8
const fm_uint32 action_base_ptr = 0x12;
const mbyLpmSubtrieStore match_store = {{0xc000000000000000, 0x0, 0x0, 0x0}, 
										{0x0, 0x0, 0x0, 0x0},
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
