
# common defs for all the tests

.DEFAULT_GOAL := all

.SUFFIXES:

ROOT_DIR := $(shell dirname $(realpath $(lastword $(MAKEFILE_LIST))))

C_DIR   = ../../../c
M3_DIR  = ../../../m3

include ../../../../make/Make_c.defs

COM_DIR = ../common

INCLUDES += -I$(COM_DIR)

# Functional Model (a.k.a. White Model) static library:
WM_LIB   = $(C_DIR)/$(BLD_DIR)/libmby_c_model.a

# Common test static library (includes write_field() function):
COM_LIB  = $(COM_DIR)/$(BLD_DIR)/libmodel_c_write.a

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

ifeq ($(VERBOSE),1)
ECHO :=
else
ECHO := @
endif


LDFLAGS =

DELFILES = $(BLD_DIR) *.o *.so *.log

OBJECTS = $(addprefix $(BLD_DIR)/,$(notdir $(SOURCES:.c=.o)))

TARGET = $(BLD_DIR)/$(TARGETNM)

GLOBALDEPS = $(C_DIR)/mby_pipeline.h $(C_DIR)/mby_common.h	

