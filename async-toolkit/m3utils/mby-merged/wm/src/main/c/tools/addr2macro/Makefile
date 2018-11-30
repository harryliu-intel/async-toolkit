# -*- mode:Makefile -*-

# Copyright (C) 2018 Intel Corporation

# Tool versions:
ROOT_DIR := $(shell dirname $(realpath $(lastword $(MAKEFILE_LIST))))
include ${ROOT_DIR}/../../ToolVersions.mk

TARGET = addr2macro
CXXFLAGS = -std=c++11

ifeq ($(DEBUG),1)
CXXFLAGS += -O0 -g
else
CXXFLAGS += -O3
endif

$(TARGET): regs.h main.cpp
	$(GPP) $(CXXFLAGS) main.cpp -o $(TARGET)

clean:
	$(RM) $(TARGET)

.PHONY: clean
