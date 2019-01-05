# -*- mode:Makefile -*-

# Copyright (C) 2018 Intel Corporation

# Tool versions:
ROOT_DIR := $(shell dirname $(realpath $(lastword $(MAKEFILE_LIST))))
include ${ROOT_DIR}/../../../main/c/ToolVersions.mk

include ../../../make/Make_c.defs

LIB_DIR = ..
EXEC = client_regwrite
SRCS = client_regwrite.c fpps_reg_table.c
OBJS = $(SRCS:.c=.o)

CFLAGS = -fPIC -Wall -Wextra -O2 -g -Wno-unused-function
CPPFLAGS = -I. -I ${LIB_DIR}
LDFLAGS = -L $(LIB_DIR) -lmby_tcp_client

.PHONY: all
all: ${EXEC}

.PHONY: lib
lib:
	make -C ${LIB_DIR}

$(SRCS:.c=.d):%.d:%.c
	$(GCC) $(CPPFLAGS) $(CFLAGS) -MM $< >$@

ifneq ($(MAKECMDGOALS),clean)
include $(SRCS:.c=.d)
endif

.PHONY: regwrite
$(EXEC): ${LIB_DIR} lib client_regwrite.c
	-$(GCC) $(CPPFLAGS) ${CFLAGS} ${LDFLAGS} ${SRCS} -o ${EXEC}

.PHONY: clean
clean:
	-${RM} ${EXEC} ${OBJS} $(SRCS:.c=.d)
