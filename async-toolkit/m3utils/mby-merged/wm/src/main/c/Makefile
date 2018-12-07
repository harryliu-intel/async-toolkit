# -*- mode:Makefile -*-

# Copyright (C) 2018 Intel Corporation

# Tool versions:
ROOT_DIR := $(shell dirname $(realpath $(lastword $(MAKEFILE_LIST))))
include ${ROOT_DIR}/ToolVersions.mk

SRC_DIR1 = .
# mby_top_map.h
SRC_DIR2 = ../m3/genviews/src/build_c/mby_c/src
# model_c_write.h
SRC_DIR3 = ../m3/model_server/src

INCLUDES = -I$(SRC_DIR1) -I$(SRC_DIR2) -I$(SRC_DIR3)

SOURCES += mby_crc32.c
SOURCES += mby_bitfield.c
SOURCES += mby_common.c
SOURCES += mby_parser.c
SOURCES += mby_mapper.c
SOURCES += mby_lpm_regs.c
SOURCES += mby_lpm.c
SOURCES += mby_cgrp_regs.c
SOURCES += mby_wcm.c
SOURCES += mby_exactmatch.c
SOURCES += mby_classifier.c
SOURCES += mby_hash.c
SOURCES += mby_nexthop.c
SOURCES += mby_maskgen.c
SOURCES += mby_triggers_regs.c
SOURCES += mby_triggers.c
SOURCES += mby_congmgmt.c
SOURCES += mby_rxstats.c
SOURCES += mby_modifier.c
SOURCES += mby_txstats.c

SOURCES += mby_reg_ctrl.c
SOURCES += mby_pipeline.c
SOURCES += mby_errors.c
SOURCES += mby_init.c

SOURCES += mby_model.c       # used by C tests
SOURCES += mby_model_basic.c # used by M3 model server

# Build dir:
OS_TARG = AMD64_LINUX
BLD_DIR = $(OS_TARG)

OBJECTS = $(addprefix ${BLD_DIR}/,$(SOURCES:.c=.o))

# Libraries:
LIB_NAME    = mby_c_model
LIB_STATIC  = ${BLD_DIR}/lib${LIB_NAME}.a
LIB_SHARED  = ${BLD_DIR}/lib${LIB_NAME}.so
LIBRARIES   = $(LIB_STATIC) $(LIB_SHARED)

ifeq ($(VERBOSE),1)
ECHO :=
else
ECHO := @
endif

CFLAGS  = -std=gnu99
CFLAGS += -Wall
CFLAGS += -Wextra
CFLAGS += -Werror
CFLAGS += -Wno-long-long
CFLAGS += -Wno-missing-field-initializers
CFLAGS += -Wno-unused
CFLAGS += -pedantic-errors
CFLAGS += -fPIC
CFLAGS += -Wno-variadic-macros
CFLAGS += -Wno-override-init

ifeq ($(DEBUG),1)
CFLAGS += -O0 -g -ggdb3
else
CFLAGS += -O1
endif

DEFINES   = -D_GNU_SOURCE
DEFINES  += -D_FM_ARCH_x86_64
ifeq ($(DEBUG),1)
endif

DELFILES  = $(OBJECTS) $(BLD_DIR) $(LIBRARIES) *.o *.so *.so.5

.PHONY: all
all: $(BLD_DIR) $(OBJECTS) $(LIBRARIES)

${BLD_DIR}/mby_errors.o        : mby_errors.c     	   mby_errors.h
${BLD_DIR}/mby_crc32.o         : mby_crc32.c               mby_crc32.h
${BLD_DIR}/mby_bitfield.o      : mby_bitfield.c 	                                  mby_bitfield.h
${BLD_DIR}/mby_common.o        : mby_common.c                                mby_common.h mby_bitfield.h
${BLD_DIR}/mby_parser.o        : mby_parser.c     	   mby_parser.h      mby_common.h mby_bitfield.h
${BLD_DIR}/mby_mapper.o        : mby_mapper.c     	   mby_mapper.h      mby_common.h mby_bitfield.h
${BLD_DIR}/mby_cgrp_regs.o     : mby_cgrp_regs.c           mby_cgrp_regs.h   mby_common.h mby_bitfield.h
${BLD_DIR}/mby_clsfr_regs.o    : mby_cgrp_regs.c 	   mby_cgrp_regs.h   mby_common.h mby_bitfield.h
${BLD_DIR}/mby_lpm_regs.o      : mby_lpm_regs.c   	   mby_lpm_regs.h    mby_common.h mby_bitfield.h
${BLD_DIR}/mby_lpm.o           : mby_lpm.c        	   mby_lpm.h         mby_common.h mby_bitfield.h
${BLD_DIR}/mby_wcm.o           : mby_wcm.c        	   mby_wcm.h         mby_common.h mby_bitfield.h
${BLD_DIR}/mby_exactmatch.o    : mby_exactmatch.c 	   mby_exactmatch.h  mby_common.h mby_bitfield.h
${BLD_DIR}/mby_classifier.o    : mby_classifier.c 	   mby_classifier.h  mby_common.h mby_bitfield.h
${BLD_DIR}/mby_hash.o          : mby_hash.c       	   mby_hash.h        mby_common.h mby_bitfield.h
${BLD_DIR}/mby_nexthop.o       : mby_nexthop.c    	   mby_nexthop.h     mby_common.h mby_bitfield.h
${BLD_DIR}/mby_maskgen.o       : mby_maskgen.c    	   mby_maskgen.h     mby_common.h mby_bitfield.h
${BLD_DIR}/mby_triggers_regs.o : mby_triggers_regs.c       mby_triggers.h    mby_common.h mby_bitfield.h
${BLD_DIR}/mby_triggers.o      : mby_triggers.c   	   mby_triggers.h    mby_common.h mby_bitfield.h
${BLD_DIR}/mby_congmgmt.o      : mby_congmgmt.c   	   mby_congmgmt.h    mby_common.h mby_bitfield.h
${BLD_DIR}/mby_rxstats.o       : mby_rxstats.c    	   mby_rxstats.h     mby_common.h mby_bitfield.h
${BLD_DIR}/mby_modifier.o      : mby_modifier.c   	   mby_modifier.h    mby_common.h mby_bitfield.h
${BLD_DIR}/mby_txstats.o       : mby_txstats.c    	   mby_txstats.h     mby_common.h mby_bitfield.h
${BLD_DIR}/mby_pipeline.o      : mby_pipeline.c            mby_pipeline.h
${BLD_DIR}/mby_reg_ctrl.o      : mby_reg_ctrl.c            mby_reg_ctrl.h
${BLD_DIR}/mby_model.o         : mby_model.c               mby_model.h
${BLD_DIR}/mby_model_basic.o   : mby_model_basic.c         mby_model_basic.h
${BLD_DIR}/mby_errors.o        : mby_errors.c              mby_errors.h      mby_model.h
${BLD_DIR}/mby_init.o          : mby_init.c                mby_init.h

# write_field() used by M3 model_server:
${BLD_DIR}/model_c_write.o     : $(SRC_DIR3)/model_c_write.c $(SRC_DIR3)/model_c_write.h

# write_field() used by C tests:
${BLD_DIR}/mby_write_field.o   : mby_write_field.c mby_write_field.h

${BLD_DIR}/%.o: %.c
	@echo '  Compiling' $<
	$(ECHO) $(GCC) -o $@ $(DEFINES) $(CFLAGS) $(INCLUDES) -c $<

$(BLD_DIR):
	@echo '  Creating build directory' $@
	$(ECHO) $(MKDIR) $@

$(LIB_STATIC): $(OBJECTS)
	@echo '  Creating static library' $@
	$(ECHO) $(AR) $@ $^

$(LIB_SHARED): $(OBJECTS)
	@echo '  Creating shared library' $@
	$(ECHO) $(GCC) -shared -o $@ $^

libs: $(LIBRARIES)

.PHONY: clean
clean:
	@echo '  Cleaning'
	$(ECHO) $(RM) $(DELFILES)

# ========================== Unit testing ==================================

# Shared location with Google tests binaries and include files
GTEST_ROOT ?= /p/utils/srd/software/unit-test/GoogleTest-SLES11/googletest
GTEST_INCLUDES = -I$(GTEST_ROOT)/include
GTEST_OBJECTS = $(GTEST_ROOT)/make/gtest_main.a

GMOCK_ROOT ?= /p/utils/srd/software/unit-test/GoogleTest-SLES11/googlemock
GMOCK_INCLUDES = -I/p/utils/srd/software/unit-test/GoogleTest-SLES11/C-Mock-master/include
GMOCK_INCLUDES += -I$(GMOCK_ROOT)/include
GMOCK_OBJECTS = $(GMOCK_ROOT)/make/gmock_main.a


UT_CFLAGS += -O0 -g -ggdb3

UT_BINARIES += unit_tests/ut_lpm.exe

unit_tests/liblpm.so: mby_lpm.o mby_lpm_regs.o mby_bitfield.o mby_common.o
unit_tests/ut_lpm.exe: unit_tests/ut_lpm.o unit_tests/mock_lpm_regs.o unit_tests/liblpm.so

DELFILES += unit_tests/*.so unit_tests/*.o unit_tests/*.exe

unit_tests/%.so:
	@echo '  Linking UT shared lib' $@
	$(ECHO) $(GCC) -shared -o $@ $^

unit_tests/%.o: unit_tests/%.cpp
	@echo '  Compiling UT' $<
	$(ECHO) $(GPP) -o $@ $(DEFINES) $(UT_CFLAGS) $(GTEST_INCLUDES) $(GMOCK_INCLUDES) $(INCLUDES) -c $<

unit_tests/%.exe:
	@echo '  Linking UT exe' $@
	$(ECHO) $(GPP) -o $@ $(DEFINES) $(GMOCK_INCLUDES) $(GTEST_INCLUDES) $(INCLUDES) \
		-pthread -lpthread -ldl -rdynamic $(GEST_OBJECTS) $(GMOCK_OBJECTS) $^

.PHONY: unit_test
unit_test: $(UT_BINARIES)
	@for ut in $(UT_BINARIES) ; do \
		echo '  Executing UT' $$ut; \
		$$ut; \
	done
