// -*- mode:cpp -*-

// Copyright (C) 2018 Intel Corporation

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <time.h>

/* Need to specify extern "C" so that C type linkage. */
extern "C" {
#include "mby_pipeline.h"
#include "mby_common.c"
#include "mby_bitfield.c"
#include "fm_crc32.c"
#include "mby_classifier.c"
}

#include <limits.h>
#include "gtest/gtest.h"

class ClassifierTests : public testing::Test {
	/* You should make the members protected s.t. they can be accessed from
	 * sub-classes.
	 */
	protected:
		fm_uint32 regs[MBY_REGISTER_ARRAY_SIZE];

		/* SetUp for all the Tests derived from ClassifierTests class */
		void SetUp() {
			/* This could have perf impact when we have a lot of tests */
			memset(regs, 0, sizeof(regs));
		}

		/* TearDown for all the Tests derived from ClassifierTests class */
		void TearDown() {
			/* Do any other cleanup if necessary. (Semaphores, etc) */
		}
};

TEST_F(ClassifierTests, DummyTest) {
	mbyMapperToClassifier in;
	mbyClassifierToHash   out;

	/* Prepare the input variables */

	/* Call the function */
	Classifier(regs, &in, &out);

	/* Use assertions to check the outputs */
	EXPECT_EQ(1, 1);
}

TEST_F(ClassifierTests, getTcamCfg) {
	fm_byte group = 0;
	fm_byte slice = 1;
	fm_byte scenario = 2;
	mbyClassifierTcamCfg tcam_cfg;

	// Prepare the register space
	// TODO use a dedicated function to write the register?
	fm_uint32 tcam_cfg_addr = MBY_FFU_TCAM_CFG(group, slice, scenario, 0) / 4;
	regs[tcam_cfg_addr] = ~0x0;
	regs[tcam_cfg_addr + 1] = ~0x0;

	// Call the function and verify the output
	getTcamCfg(regs, group, slice, scenario, &tcam_cfg);
	EXPECT_EQ(tcam_cfg.CHUNK_MASK, 0xffff);
	EXPECT_EQ(tcam_cfg.START_COMPARE, 0x1);
	EXPECT_EQ(tcam_cfg.START_SET, 0x1);
	EXPECT_EQ(tcam_cfg.SELECT_TOP, 0x3f);
	EXPECT_EQ(tcam_cfg.SELECT0, 0x7f);
	EXPECT_EQ(tcam_cfg.SELECT1, 0x7f);
	EXPECT_EQ(tcam_cfg.SELECT2, 0x7f);
	EXPECT_EQ(tcam_cfg.SELECT3, 0x7f);

	// Register in not init so all the vars are expected to be 0
	scenario += 1;
	getTcamCfg(regs, group, slice, scenario, &tcam_cfg);
	EXPECT_EQ(tcam_cfg.CHUNK_MASK, 0x0);
	EXPECT_EQ(tcam_cfg.START_COMPARE, 0x0);
	EXPECT_EQ(tcam_cfg.START_SET, 0x0);
	EXPECT_EQ(tcam_cfg.SELECT_TOP, 0x0);
	EXPECT_EQ(tcam_cfg.SELECT0, 0x0);
	EXPECT_EQ(tcam_cfg.SELECT1, 0x0);
	EXPECT_EQ(tcam_cfg.SELECT2, 0x0);
	EXPECT_EQ(tcam_cfg.SELECT3, 0x0);
}

TEST_F(ClassifierTests, getTcamEntry) {
	fm_byte group = 0;
	fm_byte slice = 1;
    fm_uint16 index = 2;
    mbyClassifierTcamEntry tcam_entry;

	// Prepare the register space
	fm_uint32 tcam_addr = MBY_FFU_TCAM(group, slice, index, 0) / 4;
	regs[tcam_addr] = 0x12345678;
	regs[tcam_addr + 1] = 0xab;
	regs[tcam_addr + 2] = 0x87654321;
	regs[tcam_addr + 3] = 0xba;

	// Call the function and verify the output
	getTcamEntry(regs, group, slice, index, &tcam_entry);
	EXPECT_EQ(tcam_entry.Key, 0xab12345678);
	EXPECT_EQ(tcam_entry.KeyInvert, 0xba87654321);

	// Register is expected to be all 0
	index += 1;
	getTcamEntry(regs, group, slice, index, &tcam_entry);
	EXPECT_EQ(tcam_entry.Key, 0x0);
	EXPECT_EQ(tcam_entry.KeyInvert, 0x0);
}

#if 0
TEST_F(ClassifierTests, selectKeyMask) {
    mbyClassifierTcamCfg tcam_cfg;
    mbyClassifierKeys    keys;
    mbyLookupInfo        lookup_info;

}
#endif
