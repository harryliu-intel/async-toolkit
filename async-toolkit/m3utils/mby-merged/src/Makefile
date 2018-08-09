# -*- mode:Makefile -*-

# Copyright (C) 2018 Intel Corporation

INC_DIR = .

INCLUDES  = -I$(INC_DIR)

SOURCES += fm_crc32.c
SOURCES += mby_common.c
SOURCES += mby_parser.c
SOURCES += mby_mapper.c
SOURCES += mby_classifier.c
SOURCES += mby_hash.c
SOURCES += mby_pipeline.c

OBJECTS = $(SOURCES:.c=.o)

# C compiler:
CC = gcc

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

DELFILES  = $(OBJECTS) *.o

.PHONY: all
all: $(OBJECTS)

mby_common.o     : mby_common.c                      mby_common.h
mby_parser.o     : mby_parser.c     mby_parser.h     mby_common.h
mby_mapper.o     : mby_mapper.c     mby_mapper.h     mby_common.h
mby_classifier.o : mby_classifier.c mby_classifier.h mby_common.h

%.o: %.c
	@echo '  Compiling' $<
	$(ECHO) $(CC) -o $@ $(DEFINES) $(CFLAGS) $(INCLUDES) -c $< 

.PHONY: clean
clean:
	@echo '  Cleaning'
	$(ECHO) rm -f $(DELFILES)
