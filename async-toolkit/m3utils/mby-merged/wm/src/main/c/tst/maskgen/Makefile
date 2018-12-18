# -*- mode:Makefile -*-

# Copyright (C) 2018 Intel Corporation

ROOT_DIR := $(shell dirname $(realpath $(lastword $(MAKEFILE_LIST))))

COM_DIR = ../common
C_DIR   = ../../../c
M3_DIR  = ../../../m3
MAP_DIR = ${M3_DIR}/genviews/src/build_c/mby_c/src

# Tool versions:
include ${C_DIR}/ToolVersions.mk

INCLUDES  = -I.
INCLUDES += -I$(COM_DIR)
INCLUDES += -I$(MAP_DIR)
INCLUDES += -I$(C_DIR)

SOURCES = mby_maskgen_test.c

# Build dir:
OS_TARG = AMD64_LINUX
BLD_DIR = $(OS_TARG)
BLD_DTS = ${BLD_DIR}/.time_stamp

OBJECTS = $(addprefix ${BLD_DIR}/,$(SOURCES:.c=.o))

# Functional Model (a.k.a. White Model) static library:
WM_LIB   = ${C_DIR}/${BLD_DIR}/libmby_c_model.a

# Common test static library (includes write_field() function):
COM_LIB  = ${COM_DIR}/${BLD_DIR}/libmodel_c_write.a

TARGET   = ${BLD_DIR}/mby_maskgen_test

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

LDFLAGS =

DELFILES = $(BLD_DIR) *.o *.so *.log

.PHONY: all
all: $(BLD_DTS) $(OBJECTS) $(TARGET)

$(BLD_DTS):
	@echo '  Creating build directory'
	$(ECHO) $(MKDIR) $(BLD_DIR) && touch ${BLD_DTS}

${BLD_DIR}/%.o: %.c
	@echo '  Compiling' $<
	$(ECHO) $(GCC) -o $@ $(DEFINES) $(CFLAGS) $(INCLUDES) -c $<

# Added dependencies:
${BLD_DIR}/mby_maskgen_test.o: \
        $(BLD_DTS) \
        Makefile \
        mby_maskgen_test.c \
        mby_maskgen_test.h \
        ${C_DIR}/mby_maskgen.h \
        ${C_DIR}/mby_pipeline.h \
        ${C_DIR}/mby_common.h \
        ${C_DIR}/mby_reg_ctrl.h

$(WM_LIB):
	make -C ${C_DIR}

$(COM_LIB):
	make -C ${COM_DIR}

$(TARGET): $(BLD_DTS) $(OBJECTS) $(WM_LIB) $(COM_LIB)
	@echo '  Linking' $(TARGET)
	$(ECHO) $(GCC) $(LDFLAGS) -o $(TARGET) $(OBJECTS) $(WM_LIB) $(COM_LIB)

.PHONY: run
run: $(TARGET)
	@echo '  Running $(TARGET)'
	@/usr/bin/time --format='Ran for %e seconds.' \
            ./$(TARGET) | tee run.log

.PHONY: valgrind
valgrind: $(TARGET)
	@echo '  Running $(TARGET) in Valgrind'
	$(VAL) \
            --track-origins=yes \
            --leak-check=full \
            --show-leak-kinds=all \
	$(TARGET) 2>&1 | tee val.log

.PHONY: callgrind
callgrind: $(TARGET)
	@echo '  Running $(TARGET) in Cachegrind'
	$(VAL) \
            --tool=callgrind \
            --verbose \
            --dump-every-bb=12500000000 \
	$(TARGET) 2>&1 | tee /cal.log

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
