# -*- Makefile -*-

.PHONY: clean
clean:
	@echo "Cleaning ..."
	make -C parser clean
	make -C mapper clean

.PHONY: all
all: test_parser test_mapper

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
