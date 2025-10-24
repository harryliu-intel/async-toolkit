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
#include "mby_mapper.c"
}

#include <limits.h>
#include "gtest/gtest.h"

class HelloMapper : public testing::Test {
	/* You should make the members protected s.t. they can be accessed from
	 * sub-classes.
	 */
	protected:

		/* SetUp for all the Tests derived from HelloMapper class */
		static void SetUpTestCase() {
		}

		/* TearDown for all the Tests derived from HelloMapper class */
		static void TearDownTestCase() {
			/* Do any other cleanup if necessary. (Semaphores, etc) */
		}
};

TEST_F(HelloMapper, DummyTest) {
	fm_uint32             regs[MBY_REGISTER_ARRAY_SIZE];
	mbyParserToMapper     in;
	mbyMapperToClassifier out;

	/* Prepare the input variables */

	/* Call the function */
	Mapper(regs, &in, &out);

	/* Use assertions to check the outputs */
	EXPECT_EQ(1, 1);
}

TEST_F(HelloMapper, StaticFuncTest) {
	fm_uint key;
	fm_uint rot;

	key = 0x123456;
	rot = 0;
	EXPECT_EQ(key, rotateKey(key, rot));
}

