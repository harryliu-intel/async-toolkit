# -*- mode:Makefile -*-

# Copyright (C) 2018 Intel Corporation

.PHONY: all
all: common_lib \
     test_parser test_mapper test_exactmatch test_wcm test_nexthop \
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

.PHONY: common_lib
common_lib:
	@echo "Building common library ..."
	$(MAKE) -C common all

.PHONY: test_parser
test_parser:
	@echo "Testing parser ..."
	$(MAKE) -C parser run

.PHONY: test_mapper
test_mapper:
	@echo "Testing mapper ..."
	$(MAKE) -C mapper run

.PHONY: test_classifier
test_classifier:
	@echo "Testing classifier ..."
	$(MAKE) -C classifier run

.PHONY: test_exactmatch
test_exactmatch:
	@echo "Testing exactmatch ..."
	$(MAKE) -C exactmatch run

.PHONY: test_wcm
test_wcm:
	@echo "Testing wcm ..."
	$(MAKE) -C wcm run

.PHONY: test_nexthop
test_nexthop:
	@echo "Testing nexthop ..."
	$(MAKE) -C nexthop run

.PHONY: test_maskgen
test_maskgen:
	@echo "Testing maskgen ..."
	$(MAKE) -C maskgen run

.PHONY: test_maskgen
test_triggers:
	@echo "Testing triggers ..."
	$(MAKE) -C triggers run

.PHONY: test_rxstats
test_rxstats:
	@echo "Testing rxstats ..."
	$(MAKE) -C rxstats run

.PHONY: test_modifier
test_modifier:
	@echo "Testing modifier ..."
	$(MAKE) -C modifier run

.PHONY: test_basic_fwd
test_basic_fwd:
	@echo "Testing basic forwarding ..."
	$(MAKE) -C basic_fwd run

.PHONY: test_basic_flood
test_basic_flood:
	@echo "Testing basic flooding ..."
	$(MAKE) -C basic_flood run
