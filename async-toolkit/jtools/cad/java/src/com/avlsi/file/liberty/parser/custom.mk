LIBERTY_PARSER_DIST := liberty_parse-2.5

LIBERTY_PARSER_DIR := $(CURR_PROJECT_DIR)/$(LIBERTY_PARSER_DIST)

CURR_NON_BUILD_DIRS := $(LIBERTY_PARSER_DIST) $(CURR_NON_BUILD_DIRS)

CURR_RESULT_FILES := $(CURR_RESULT_FILES) 

LIBERTY_PARSER_SOURCE := \
	PI.c \
	attr_lookup.c \
	group_lookup.c \
	liberty_front_lex.c \
	liberty_parser.c \
	libhash.c \
	libstrtab.c \
	mymalloc.c \
	syntax_decls.c \
	syntax_checks.c \
	token.c

$(CURR_TARGET_DIR)/liberty.i: $(CURR_PROJECT_DIR)/liberty.i
	cp '$<' '$@'

$(CURR_TARGET_DIR)/liberty_wrap.c $(CURR_TARGET_DIR)/liberty.java: $(CURR_TARGET_DIR)/liberty.i $(LIBERTY_PARSER_DIR)/si2dr_liberty.h
	cd '$(@D)' && swig '-I$(LIBERTY_PARSER_DIR)' -java -package com.avlsi.file.liberty.parser '$<'

$(CURR_TARGET_DIR)/liberty_wrap.o: $(CURR_TARGET_DIR)/liberty_wrap.c
	$(GCC) -I$(LIBERTY_PARSER_DIR) -c '$<' -fPIC -o '$@'

$(CURR_TARGET_DIR)/$(LIBERTY_PARSER_DIST)/Makefile: $(shell find $(LIBERTY_PARSER_DIR) -type f)
	rm -rf '$(@D)' && \
	cp -a $(LIBERTY_PARSER_DIR) '$(@D)' && \
	cd '$(@D)' && \
	./configure CFLAGS=-fPIC
