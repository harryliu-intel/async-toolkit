# -*- mode:Makefile -*-

# Copyright (C) 2018 Intel Corporation

# Tool versions:
ROOT_DIR := $(shell dirname $(realpath $(lastword $(MAKEFILE_LIST))))
include ${ROOT_DIR}/../ToolVersions.mk

CONFIG_FILE = doxygen.cfg
OUTPUT_DIR  = doxygen-output
HTML_DIR    = ${OUTPUT_DIR}/html
HTML_TOP    = $(HTML_DIR)/index.html

# Project number (from Git):
export PROJECT_NUMBER = $(shell git describe --always --dirty="-with-uncommitted-changes" --long --tags)

ifeq ($(VERBOSE),1)
ECHO :=
else
ECHO := @
endif

.PHONY: all
all: url

.PHONY: tag
tag:
	@echo "  Tagging as $(PROJECT_NUMBER)"

$(HTML_TOP): $(CONFIG_FILE) tag
	@echo '  Running doxygen'
	$(ECHO) $(DOX) $(CONFIG_FILE)

.PHONY: url
url: $(HTML_TOP)
	@echo ' Point your HTML browser at this URL:'
	@echo $(PWD)/$(HTML_TOP)

.PHONY: clean
clean:
	@echo '  Cleaning doxygen output dir'
	$(ECHO) $(RM) $(HTML_DIR)
