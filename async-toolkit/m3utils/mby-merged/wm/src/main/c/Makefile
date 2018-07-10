# -*- mode:Makefile -*-

# Copyright (C) 2018 Intel Corporation

INC_DIR = .

INCLUDES  = -I$(INC_DIR)

SOURCES += mby_parser.c

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

mby_parser.o : mby_parser.c mby_parser.h

%.o: %.c
	@echo '  Compiling' $<
	$(ECHO) $(CC) -o $@ $(DEFINES) $(CFLAGS) $(INCLUDES) -c $< 

.PHONY: clean
clean:
	@echo '  Cleaning'
	$(ECHO) rm -f $(DELFILES)
