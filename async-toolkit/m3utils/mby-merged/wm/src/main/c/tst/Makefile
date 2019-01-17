# -*- mode:Makefile -*-

# Copyright (C) 2018 Intel Corporation

.SUFFIXES:

ROOT_DIR := $(shell dirname $(realpath $(lastword $(MAKEFILE_LIST))))

C_DIR   = ../../c
M3_DIR  = ../../m3

include ../../../make/Make_c.defs

COMMONLIB= common/$(BLD_DIR)/libmodel_c_write.a

.PHONY: all
all: test_parser test_mapper test_exactmatch test_wcm test_nexthop \
     test_maskgen test_triggers test_rxstats test_modifier \
     test_basic_fwd test_basic_flood

.PHONY: test_all
test_all: all

.PHONY: clean
clean:
	@echo "Cleaning ..."
	$(MAKE) -C common      clean
	$(MAKE) -C parser      clean
	$(MAKE) -C mapper      clean
	$(MAKE) -C classifier  clean
	$(MAKE) -C exactmatch  clean
	$(MAKE) -C wcm         clean
	$(MAKE) -C nexthop     clean
	$(MAKE) -C maskgen     clean
	$(MAKE) -C triggers    clean
	$(MAKE) -C rxstats     clean
	$(MAKE) -C modifier    clean
	$(MAKE) -C basic_fwd   clean
	$(MAKE) -C basic_flood clean



$(COMMONLIB):
	@echo "Building common library ..."
	$(MAKE) -C common all

.PHONY: test_parser
test_parser: $(COMMONLIB)
	@echo "Testing parser ..."
	$(MAKE) -C parser run

.PHONY: test_mapper
test_mapper: $(COMMONLIB)
	@echo "Testing mapper ..."
	$(MAKE) -C mapper run

.PHONY: test_classifier
test_classifier:
	@echo "Testing classifier ..."
	$(MAKE) -C classifier run

.PHONY: test_exactmatch
test_exactmatch: $(COMMONLIB)
	@echo "Testing exactmatch ..."
	$(MAKE) -C exactmatch run

.PHONY: test_wcm
test_wcm: $(COMMONLIB)
	@echo "Testing wcm ..."
	$(MAKE) -C wcm run

.PHONY: test_nexthop
test_nexthop: $(COMMONLIB)
	@echo "Testing nexthop ..."
	$(MAKE) -C nexthop run

.PHONY: test_maskgen
test_maskgen: $(COMMONLIB)
	@echo "Testing maskgen ..."
	$(MAKE) -C maskgen run

.PHONY: test_maskgen
test_triggers: $(COMMONLIB)
	@echo "Testing triggers ..."
	$(MAKE) -C triggers run

.PHONY: test_rxstats
test_rxstats: $(COMMONLIB)
	@echo "Testing rxstats ..."
	$(MAKE) -C rxstats run

.PHONY: test_modifier
test_modifier: $(COMMONLIB)
	@echo "Testing modifier ..."
	$(MAKE) -C modifier run

.PHONY: test_basic_fwd
test_basic_fwd: $(COMMONLIB)
	@echo "Testing basic forwarding ..."
	$(MAKE) -C basic_fwd run

.PHONY: test_basic_flood
test_basic_flood: $(COMMONLIB)
	@echo "Testing basic flooding ..."
	$(MAKE) -C basic_flood run
