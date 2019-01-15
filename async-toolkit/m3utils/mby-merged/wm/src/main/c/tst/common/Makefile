# -*- mode:Makefile -*-

# Copyright (C) 2018 Intel Corporation

ROOT_DIR := $(shell dirname $(realpath $(lastword $(MAKEFILE_LIST))))

C_DIR   = ../../../c
M3_DIR  = ../../../m3
MAP_DIR = ${M3_DIR}/genviews/src/build_c/mby_c/src
SRV_DIR = ${M3_DIR}/model_server/src

# Tool versions:
include ${C_DIR}/ToolVersions.mk

INCLUDES  = -I.
INCLUDES += -I$(C_DIR)
INCLUDES += -I$(MAP_DIR)
INCLUDES += -I$(SRV_DIR)

SOURCES  = ${C_DIR}/uint.c
SOURCES += ${C_DIR}/fm_alos_debughash.c
SOURCES += ${MAP_DIR}/mby_top_map.c
SOURCES += tst_model_c_write.c

# Build dir:
OS_TARG = AMD64_LINUX
BLD_DIR = $(OS_TARG)
BLD_DTS = ${BLD_DIR}/.time_stamp

OBJECTS = $(addprefix ${BLD_DIR}/,$(notdir $(SOURCES:.c=.o)))

# Libraries:
LIB_NAME   = model_c_write
LIB_STATIC = ${BLD_DIR}/lib${LIB_NAME}.a
LIBRARIES  = $(LIB_STATIC)

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

DELFILES = $(BLD_DIR) *.o *.so *.so.5

.PHONY: all
all: $(BLD_DTS) $(OBJECTS) $(LIBRARIES)

$(BLD_DTS):
	@echo '  Creating build directory'
	$(ECHO) $(MKDIR) $(BLD_DIR) && touch ${BLD_DTS}

${BLD_DIR}/%.o: ${C_DIR}/%.c
	@echo '  Compiling' $<
	$(ECHO) $(GCC) -o $@ $(DEFINES) $(CFLAGS) $(INCLUDES) -c $<

${BLD_DIR}/%.o: ${MAP_DIR}/%.c
	@echo '  Compiling' $<
	$(ECHO) $(GCC) -o $@ $(DEFINES) $(CFLAGS) $(INCLUDES) -c $<

${BLD_DIR}/%.o: %.c
	@echo '  Compiling' $<
	$(ECHO) $(GCC) -o $@ $(DEFINES) $(CFLAGS) $(INCLUDES) -c $<

# write_field() used by C tests:
${BLD_DIR}/uint.o:              $(BLD_DTS) Makefile ${C_DIR}/uint.c ${C_DIR}/uint.h
${BLD_DIR}/fm_alos_debughash.o: $(BLD_DTS) Makefile ${C_DIR}/fm_alos_debughash.c ${C_DIR}/fm_alos_debughash.h
${BLD_DIR}/mby_top_map.o:       $(BLD_DTS) Makefile ${MAP_DIR}/mby_top_map.c ${MAP_DIR}/mby_top_map.h
${BLD_DIR}/model_c_write.o:     $(BLD_DTS) Makefile model_c_write.c model_c_write.h ${C_DIR}/fm_types.h ${C_DIR}/fm_alos_debughash.h

$(LIB_STATIC): $(BLD_DTS) $(OBJECTS)
	@echo '  Creating static library'
	$(ECHO) $(AR) $@ $^
	$(ECHO) $(RANLIB) $@

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
