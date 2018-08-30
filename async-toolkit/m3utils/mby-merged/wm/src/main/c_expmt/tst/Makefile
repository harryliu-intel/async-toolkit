# -*- Makefile -*-

.PHONY: clean
clean:
	@echo "Cleaning ..."
	make -C parser clean
	make -C mapper clean
	make -C basic_fwd clean

.PHONY: all
all: test_parser test_mapper test_basic_fwd

.PHONY: test_all
test_all: all

.PHONY: test_parser
test_parser:
	@echo "Testing parser ..."
	make -C parser run

.PHONY: test_mapper
test_mapper:
	@echo "Testing mapper ..."
	make -C mapper run

.PHONY: test_basic_fwd
test_basic_fwd:
	@echo "Testing basic forwarding ..."
	make -C basic_fwd run
