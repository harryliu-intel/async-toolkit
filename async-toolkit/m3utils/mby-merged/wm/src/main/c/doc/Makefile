# -*- mode:Makefile -*-

# Copyright (C) 2018 Intel Corporation

CONFIG_FILE = doxygen.cfg
OUTPUT_DIR  = doxygen-output
HTML_DIR    = ${OUTPUT_DIR}/html
HTML_TOP    = $(HTML_DIR)/index.html

# Doxygen compiler:
DOXYGEN_VERSION := 1.8.11
DOXYGEN_PATH := /usr/intel/pkgs/doxygen/$(DOXYGEN_VERSION)
DOXYGEN = $(DOXYGEN_PATH)/bin/doxygen

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
	$(ECHO) $(DOXYGEN) $(CONFIG_FILE)

.PHONY: url
url: $(HTML_TOP)
	@echo ' Point your HTML browser at this URL:'
	@echo $(PWD)/$(HTML_TOP)

.PHONY: clean
clean:
	@echo '  Cleaning doxygen output dir'
	$(ECHO) rm -rf $(HTML_DIR)
