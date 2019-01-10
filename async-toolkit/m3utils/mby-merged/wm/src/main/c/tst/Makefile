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
	make -C common      clean
	make -C parser      clean
	make -C mapper      clean
	make -C classifier  clean
	make -C exactmatch  clean
	make -C wcm         clean
	make -C nexthop     clean
	make -C maskgen     clean
	make -C triggers    clean
	make -C rxstats     clean
	make -C modifier    clean
	make -C basic_fwd   clean
	make -C basic_flood clean

.PHONY: common_lib
common_lib:
	@echo "Building common library ..."
	make -C common all

.PHONY: test_parser
test_parser:
	@echo "Testing parser ..."
	make -C parser run

.PHONY: test_mapper
test_mapper:
	@echo "Testing mapper ..."
	make -C mapper run

.PHONY: test_classifier
test_classifier:
	@echo "Testing classifier ..."
	make -C classifier run

.PHONY: test_exactmatch
test_exactmatch:
	@echo "Testing exactmatch ..."
	make -C exactmatch run

.PHONY: test_wcm
test_wcm:
	@echo "Testing wcm ..."
	make -C wcm run

.PHONY: test_nexthop
test_nexthop:
	@echo "Testing nexthop ..."
	make -C nexthop run

.PHONY: test_maskgen
test_maskgen:
	@echo "Testing maskgen ..."
	make -C maskgen run

.PHONY: test_maskgen
test_triggers:
	@echo "Testing triggers ..."
	make -C triggers run

.PHONY: test_rxstats
test_rxstats:
	@echo "Testing rxstats ..."
	make -C rxstats run

.PHONY: test_modifier
test_modifier:
	@echo "Testing modifier ..."
	make -C modifier run

.PHONY: test_basic_fwd
test_basic_fwd:
	@echo "Testing basic forwarding ..."
	make -C basic_fwd run

.PHONY: test_basic_flood
test_basic_flood:
	@echo "Testing basic flooding ..."
	make -C basic_flood run
