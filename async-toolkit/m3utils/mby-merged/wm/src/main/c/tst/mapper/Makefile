# -*- mode:Makefile -*-

# Copyright (C) 2018 Intel Corporation

include ../make/Make.defs
SOURCES = mby_mapper_test.c

OBJECTS = $(addprefix ${BLD_DIR}/,$(SOURCES:.c=.o))

.PHONY: all
all: $(BLD_DTS) $(OBJECTS) $(TARGET)

$(BLD_DTS):
	@echo '  Creating build directory'
	$(ECHO) $(MKDIR) $(BLD_DIR) && touch ${BLD_DTS}

${BLD_DIR}/%.o: %.c
	@echo '  Compiling' $<
	$(ECHO) $(GCC) -o $@ $(DEFINES) $(CFLAGS) $(INCLUDES) -c $<

# Added dependencies:
${BLD_DIR}/mby_mapper_test.o: \
        $(BLD_DTS) Makefile \
        mby_mapper_test.c $(C_DIR)/mby_mapper.h \
        $(C_DIR)/mby_common.h

.PHONY: $(WM_LIB)
$(WM_LIB):
	make -C ${C_DIR}

.PHONY: $(COM_LIB)
$(COM_LIB):
	make -C ${COM_DIR}

$(TARGET): $(BLD_DTS) $(OBJECTS) $(WM_LIB) $(COM_LIB)
	@echo '  Linking' $(TARGET)
	$(ECHO) $(GCC) $(LDFLAGS) -o $(TARGET) $(OBJECTS) $(WM_LIB) $(COM_LIB)

.PHONY: run
run: $(TARGET)
	@echo '  Running $(TARGET)'
	@/usr/bin/time --format='Ran for %e seconds.' \
            ./$(TARGET) | tee run.log

.PHONY: valgrind
valgrind: $(TARGET)
	@echo '  Running $(TARGET) in Valgrind'
	$(VAL) \
            --track-origins=yes \
            --leak-check=full \
            --show-leak-kinds=all \
	$(TARGET) 2>&1 | tee val.log

.PHONY: callgrind
callgrind: $(TARGET)
	@echo '  Running $(TARGET) in Cachegrind'
	$(VAL) \
            --tool=callgrind \
            --verbose \
            --dump-every-bb=12500000000 \
	$(TARGET) 2>&1 | tee /cal.log

.PHONY: clean
clean:
	@echo '  Cleaning'
	$(ECHO) $(RM) $(DELFILES)

.PHONY: show_sources
show_sources:
	@echo 'SOURCES :'
	@$(foreach i, $(SOURCES), echo $(i);)

.PHONY: show_includes
show_includes:
	@echo 'INCLUDES :'
	@$(foreach i, $(INCLUDES), echo $(i);)

.PHONY: show_objects
show_objects:
	@echo 'OBJECTS :'
	@$(foreach i, $(OBJECTS), echo $(i);)
