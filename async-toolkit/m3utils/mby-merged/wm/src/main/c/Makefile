# -*- mode:Makefile -*-

# Copyright (C) 2018 Intel Corporation

ROOT_DIR := $(shell dirname $(realpath $(lastword $(MAKEFILE_LIST))))

C_DIR   = .
M3_DIR  = ../m3
MAP_DIR = ${M3_DIR}/genviews/src/build_c/mby_c/src
SRV_DIR = ${M3_DIR}/model_server/src

# set default value of DEBUG to 1
include ../../make/Make_c.defs

# Tool versions:
include ${C_DIR}/ToolVersions.mk

INCLUDES  = -I.
INCLUDES += -I${MAP_DIR}
INCLUDES += -I${SRV_DIR}

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
SOURCES += varchar.c

# used by C tests:
SOURCES += mby_model.c

# used by M3 model server:
SOURCES += mby_model_basic.c

# Build dir:
OS_TARG = AMD64_LINUX
BLD_DIR = $(OS_TARG)
BLD_DTS = ${BLD_DIR}/.time_stamp

OBJECTS = $(addprefix ${BLD_DIR}/,$(SOURCES:.c=.o))

# Libraries:
LIB_NAME   = mby_c_model
LIB_M3EXPT = ${BLD_DIR}/.M3EXPORTS
LIB_STATIC = ${BLD_DIR}/lib${LIB_NAME}.a
LIB_SHARED = ${BLD_DIR}/lib${LIB_NAME}.so
LIBRARIES  = $(LIB_M3EXPT) $(LIB_STATIC) $(LIB_SHARED)

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
#FLAGS += -gstabs+ # initially added to aid in M3 debugging, but interferes with -ggdb3, so removed
else
CFLAGS += -O1
endif

DEFINES   = -D_GNU_SOURCE
DEFINES  += -D_FM_ARCH_x86_64
ifeq ($(DEBUG),1)
endif

DELFILES = $(BLD_DIR) *.o *.so *.log

.PHONY: all
all: $(BLD_DTS) $(OBJECTS) $(LIBRARIES)

$(BLD_DTS):
	@echo '  Creating build directory'
	$(ECHO) $(MKDIR) $(BLD_DIR) && touch ${BLD_DTS}

${BLD_DIR}/%.o: %.c
	@echo '  Compiling' $<
	$(ECHO) $(GCC) -o $@ $(DEFINES) $(CFLAGS) $(INCLUDES) -c $<

# Added dependencies:
${BLD_DIR}/mby_errors.o        : $(BLD_DTS) Makefile mby_errors.c        mby_errors.h
${BLD_DIR}/mby_crc32.o         : $(BLD_DTS) Makefile mby_crc32.c         mby_crc32.h
${BLD_DIR}/mby_bitfield.o      : $(BLD_DTS) Makefile mby_bitfield.c                                      mby_bitfield.h
${BLD_DIR}/mby_common.o        : $(BLD_DTS) Makefile mby_common.c        mby_common.h                    mby_bitfield.h
${BLD_DIR}/mby_parser.o        : $(BLD_DTS) Makefile mby_parser.c        mby_parser.h      mby_common.h  mby_bitfield.h
${BLD_DIR}/mby_mapper.o        : $(BLD_DTS) Makefile mby_mapper.c        mby_mapper.h      mby_common.h  mby_bitfield.h
${BLD_DIR}/mby_cgrp_regs.o     : $(BLD_DTS) Makefile mby_cgrp_regs.c     mby_cgrp_regs.h   mby_common.h  mby_bitfield.h
${BLD_DIR}/mby_clsfr_regs.o    : $(BLD_DTS) Makefile mby_cgrp_regs.c     mby_cgrp_regs.h   mby_common.h  mby_bitfield.h
${BLD_DIR}/mby_lpm_regs.o      : $(BLD_DTS) Makefile mby_lpm_regs.c      mby_lpm_regs.h    mby_common.h  mby_bitfield.h
${BLD_DIR}/mby_lpm.o           : $(BLD_DTS) Makefile mby_lpm.c           mby_lpm.h         mby_common.h  mby_bitfield.h
${BLD_DIR}/mby_wcm.o           : $(BLD_DTS) Makefile mby_wcm.c           mby_wcm.h         mby_common.h  mby_bitfield.h
${BLD_DIR}/mby_exactmatch.o    : $(BLD_DTS) Makefile mby_exactmatch.c    mby_exactmatch.h  mby_common.h  mby_bitfield.h
${BLD_DIR}/mby_classifier.o    : $(BLD_DTS) Makefile mby_classifier.c    mby_classifier.h  mby_common.h  mby_bitfield.h
${BLD_DIR}/mby_hash.o          : $(BLD_DTS) Makefile mby_hash.c          mby_hash.h        mby_common.h  mby_bitfield.h
${BLD_DIR}/mby_nexthop.o       : $(BLD_DTS) Makefile mby_nexthop.c       mby_nexthop.h     mby_common.h  mby_bitfield.h
${BLD_DIR}/mby_maskgen.o       : $(BLD_DTS) Makefile mby_maskgen.c       mby_maskgen.h     mby_common.h  mby_bitfield.h
${BLD_DIR}/mby_triggers_regs.o : $(BLD_DTS) Makefile mby_triggers_regs.c mby_triggers.h    mby_common.h  mby_bitfield.h
${BLD_DIR}/mby_triggers.o      : $(BLD_DTS) Makefile mby_triggers.c      mby_triggers.h    mby_common.h  mby_bitfield.h
${BLD_DIR}/mby_congmgmt.o      : $(BLD_DTS) Makefile mby_congmgmt.c      mby_congmgmt.h    mby_common.h  mby_bitfield.h
${BLD_DIR}/mby_rxstats.o       : $(BLD_DTS) Makefile mby_rxstats.c       mby_rxstats.h     mby_common.h  mby_bitfield.h
${BLD_DIR}/mby_modifier.o      : $(BLD_DTS) Makefile mby_modifier.c      mby_modifier.h    mby_common.h  mby_bitfield.h
${BLD_DIR}/mby_txstats.o       : $(BLD_DTS) Makefile mby_txstats.c       mby_txstats.h     mby_common.h  mby_bitfield.h
${BLD_DIR}/mby_pipeline.o      : $(BLD_DTS) Makefile mby_pipeline.c      mby_pipeline.h
${BLD_DIR}/mby_reg_ctrl.o      : $(BLD_DTS) Makefile mby_reg_ctrl.c      mby_reg_ctrl.h
${BLD_DIR}/mby_model.o         : $(BLD_DTS) Makefile mby_model.c         mby_model.h
${BLD_DIR}/mby_model_basic.o   : $(BLD_DTS) Makefile mby_model_basic.c   mby_model_basic.h
${BLD_DIR}/mby_errors.o        : $(BLD_DTS) Makefile mby_errors.c        mby_errors.h      mby_model.h
${BLD_DIR}/mby_init.o          : $(BLD_DTS) Makefile mby_init.c          mby_init.h

$(LIB_M3EXPT): $(OBJECTS)
	@echo   '  Creating M3 library exports file'
	$(ECHO) echo '_define_lib("mby_c_model")'                       >  $@
	$(ECHO) echo '_import_m3lib("mby_c_model", "c", "AMD64_LINUX")' >> $@

$(LIB_STATIC): $(OBJECTS)
	@echo '  Creating static library'
	$(ECHO) $(AR) $@ $^
	$(ECHO) $(RANLIB) $@

$(LIB_SHARED): $(OBJECTS)
	@echo '  Creating shared library'
	$(ECHO) $(GCC) -shared -o $@ $^

libs: $(BLD_DTS) $(LIBRARIES)

.PHONY: clean
clean:
	@echo '  Cleaning'
	$(ECHO) $(RM) $(DELFILES)

.PHONY: show_sources
show_sources:
	@echo 'SOURCES :'
	@$(foreach i, $(SOURCES), echo $(i);)

.PHONY: show_includes
show_includes:
	@echo 'INCLUDES :'
	@$(foreach i, $(INCLUDES), echo $(i);)

.PHONY: show_objects
show_objects:
	@echo 'OBJECTS :'
	@$(foreach i, $(OBJECTS), echo $(i);)

.PHONY: show_libraries
show_libraries:
	@echo 'LIBRARIES :'
	@$(foreach i, $(LIBRARIES), file $(i);)

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

unit_tests/liblpm.so: ${BLD_DIR}/mby_lpm.o ${BLD_DIR}/mby_lpm_regs.o ${BLD_DIR}/mby_bitfield.o ${BLD_DIR}/mby_common.o
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
