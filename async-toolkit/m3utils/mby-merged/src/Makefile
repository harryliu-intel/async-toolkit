# -*- mode:Makefile -*-

# Copyright (C) 2018 Intel Corporation

INC_DIR = .

INCLUDES  = -I$(INC_DIR)

SOURCES += fm_crc32.c
SOURCES += mby_bitfield.c
SOURCES += mby_common.c
SOURCES += mby_parser.c
SOURCES += mby_mapper.c
SOURCES += mby_classifier.c
SOURCES += mby_pipeline.c

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

DELFILES  = $(OBJECTS) *.o tests/*.o tests/*.exe

.PHONY: all
all: $(OBJECTS)

mby_common.o     : mby_common.c                      mby_common.h mby_bitfield.h
mby_parser.o     : mby_parser.c     mby_parser.h     mby_common.h mby_bitfield.h
mby_mapper.o     : mby_mapper.c     mby_mapper.h     mby_common.h mby_bitfield.h
mby_classifier.o : mby_classifier.c mby_classifier.h mby_common.h mby_bitfield.h

%.o: %.c
	@echo '  Compiling' $<
	$(ECHO) $(CC) -o $@ $(DEFINES) $(CFLAGS) $(INCLUDES) -c $<

.PHONY: clean
clean:
	@echo '  Cleaning'
	$(ECHO) rm -f $(DELFILES)

# ========================== Unit testing ==================================

# Shared location with Google tests binaries and include files
GTEST_ROOT ?= /p/utils/srd/software/unit-test/GoogleTest-SLES11/googletest
GTEST_INCLUDES = -I$(GTEST_ROOT)/include
GTEST_OBJECTS = $(GTEST_ROOT)/make/gtest_main.a

UT_BINARIES += tests/ut_mapper.exe
# UT_BINARIES += tests/ut_classifier.exe

tests/ut_mapper.exe: tests/ut_mapper.o
tests/ut_classifier.exe: tests/ut_classifier.o mby_classifier.o mby_common.o

tests/%.o: tests/%.cpp
	@echo '  Compiling' $<
	$(ECHO) $(GPP) -o $@ $(DEFINES) $(GTEST_INCLUDES) $(INCLUDES) -c $<

tests/%.exe:
	@echo '  Linking' $<
	$(ECHO) $(GPP) -o $@ $(DEFINES) $(GTEST_INCLUDES) $(INCLUDES) \
		-pthread -lpthread -ldl -rdynamic $(GTEST_OBJECTS) $^

.PHONY: test
test: $(UT_BINARIES)
	@for ut in $(UT_BINARIES) ; do \
		echo '  Executing UT' $$ut; \
		$$ut; \
	done
