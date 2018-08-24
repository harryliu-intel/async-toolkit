# -*- mode:Makefile -*-

# Copyright (C) 2018 Intel Corporation

INC_DIR = .

INCLUDES  = -I$(INC_DIR)

SOURCES += mby_crc32.c
SOURCES += mby_bitfield.c
SOURCES += mby_common.c
SOURCES += mby_parser.c
SOURCES += mby_mapper.c
SOURCES += mby_classifier.c
SOURCES += mby_hash.c
SOURCES += mby_nexthop.c
SOURCES += mby_maskgen.c
SOURCES += mby_triggers.c
SOURCES += mby_congmgmt.c
SOURCES += mby_rxstats.c
SOURCES += mby_rxtotx.c
SOURCES += mby_modifier.c
SOURCES += mby_txstats.c
SOURCES += mby_pipeline.c
SOURCES += mby_reg_ctrl.c

OBJECTS = $(SOURCES:.c=.o)

# C/C++ compiler:
CC = gcc
GPP = g++

# Linker:
LD  = ld


ifeq ($(VERBOSE),1)
ECHO :=
else
ECHO := @
endif

CFLAGS  = -std=gnu99
CFLAGS += -Wno-long-long
CFLAGS += -Wextra
CFLAGS += -Wall
CFLAGS += -Werror
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

DELFILES  = $(OBJECTS) *.o *.so

.PHONY: all
all: $(OBJECTS)

mby_common.o     : mby_common.c                      mby_common.h mby_bitfield.h
mby_parser.o     : mby_parser.c     mby_parser.h     mby_common.h mby_bitfield.h
mby_mapper.o     : mby_mapper.c     mby_mapper.h     mby_common.h mby_bitfield.h
mby_classifier.o : mby_classifier.c mby_classifier.h mby_common.h mby_bitfield.h
mby_hash.o       : mby_hash.c       mby_hash.h       mby_common.h mby_bitfield.h
mby_nexthop.o    : mby_nexthop.c    mby_nexthop.h    mby_common.h mby_bitfield.h
mby_maskgen.o    : mby_maskgen.c    mby_maskgen.h    mby_common.h mby_bitfield.h
mby_triggers.o   : mby_triggers.c   mby_triggers.h   mby_common.h mby_bitfield.h
mby_congmgmt.o   : mby_congmgmt.c   mby_congmgmt.h   mby_common.h mby_bitfield.h
mby_rxstats.o    : mby_rxstats.c    mby_rxstats.h    mby_common.h mby_bitfield.h
mby_rxtotx.o     : mby_rxtotx.c     mby_rxtotx.h     mby_common.h mby_bitfield.h
mby_modifier.o   : mby_modifier.c   mby_modifier.h   mby_common.h mby_bitfield.h
mby_txstats.o    : mby_txstats.c    mby_txstats.h    mby_common.h mby_bitfield.h

PIPELINE_STAGES += mby_parser.o
PIPELINE_STAGES += mby_mapper.o
PIPELINE_STAGES += mby_classifier.o
PIPELINE_STAGES += mby_hash.o
PIPELINE_STAGES += mby_nexthop.o
PIPELINE_STAGES += mby_maskgen.o
PIPELINE_STAGES += mby_rxstats.o
PIPELINE_STAGES += mby_rxtotx.o
PIPELINE_STAGES += mby_modifier.o
PIPELINE_STAGES += mby_txstats.o

mby_pipeline.o   : mby_pipeline.c   mby_pipeline.h   $(PIPELINE_STAGES)
mby_reg_ctrl.o   : mby_reg_ctrl.c   mby_reg_ctrl.h
mby_model.o      : mby_model.c      mby_model.h		 mby_pipeline.o

%.o: %.c
	@echo '  Compiling' $<
	$(ECHO) $(CC) -o $@ $(DEFINES) $(CFLAGS) $(INCLUDES) -c $<

libmbymodel.so: $(OBJECTS) mby_model.o
	@echo '  Linking' $@
	$(ECHO) $(CC) -shared -o $@ $^

.PHONY: clean
clean:
	@echo '  Cleaning'
	$(ECHO) rm -f $(DELFILES)

# ============================ Model server ===============================

DELFILES += model_server/*.o mby_server

model_server/%.o: model_server/%.c
	@echo '  Compiling' $<
	$(ECHO) $(CC) -o $@ $(DEFINES) $(CFLAGS) $(INCLUDES) -c $<

MS_OBJECTS += model_server/mby_srv_main.o
MS_OBJECTS += model_server/mby_srv_time.o
MS_OBJECTS += model_server/mby_srv_socket.o
MS_OBJECTS += model_server/mby_srv_errno.o
MS_OBJECTS += model_server/mby_srv_utils.o
MS_OBJECTS += model_server/mby_srv_message.o
MS_OBJECTS += model_server/mby_srv_handlers.o
MS_OBJECTS += model_server/mby_srv_nvmimg.o

# Search libmbymodel.so in the same folder and remember it in the binary (-R)
MS_FLAGS += -Wl,-R -Wl,. -L.

mby_server: $(MS_OBJECTS) libmbymodel.so
	@echo '  Linking mby_server'
	$(ECHO) $(CC) -o model_server/mby_server $(DEFINES) $(INCLUDES) \
	$^ -pthread -lpthread -ldl -lrt -rdynamic $(MS_FLAGS) -lmbymodel
	@mv model_server/mby_server .


# ========================== Unit testing ==================================

# Shared location with Google tests binaries and include files
GTEST_ROOT ?= /p/utils/srd/software/unit-test/GoogleTest-SLES11/googletest
GTEST_INCLUDES = -I$(GTEST_ROOT)/include
GTEST_OBJECTS = $(GTEST_ROOT)/make/gtest_main.a

UT_CFLAGS += -O0 -g -ggdb3

UT_BINARIES += unit_tests/ut_mapper.exe
UT_BINARIES += unit_tests/ut_classifier.exe

unit_tests/ut_mapper.exe: unit_tests/ut_mapper.o
unit_tests/ut_classifier.exe: unit_tests/ut_classifier.o

DELFILES += unit_tests/*.o unit_tests/*.exe model_server/msrv

unit_tests/%.o: unit_tests/%.cpp
	@echo '  Compiling' $<
	$(ECHO) $(GPP) -o $@ $(DEFINES) $(UT_CFLAGS) $(GTEST_INCLUDES) $(INCLUDES) -c $<

unit_tests/%.exe:
	@echo '  Linking' $<
	$(ECHO) $(GPP) -o $@ $(DEFINES) $(GTEST_INCLUDES) $(INCLUDES) \
		-pthread -lpthread -ldl -rdynamic $(GTEST_OBJECTS) $^

.PHONY: unit_test
unit_test: $(UT_BINARIES)
	@for ut in $(UT_BINARIES) ; do \
		echo '  Executing UT' $$ut; \
		$$ut; \
	done

