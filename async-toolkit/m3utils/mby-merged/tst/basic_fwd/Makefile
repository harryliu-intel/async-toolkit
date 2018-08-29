SRC_DIR = ../../src
TST_DIR = .

INCLUDES  = -I$(SRC_DIR)
INCLUDES += -I$(TST_DIR)

SOURCES  = $(SRC_DIR)/mby_reg_ctrl.c
SOURCES += $(SRC_DIR)/mby_maskgen.c
SOURCES += $(SRC_DIR)/mby_hash.c
SOURCES += $(SRC_DIR)/mby_nexthop.c
SOURCES += $(SRC_DIR)/mby_common.c
SOURCES += $(SRC_DIR)/mby_crc32.c
SOURCES += $(SRC_DIR)/mby_classifier.c
SOURCES += $(SRC_DIR)/mby_rxtotx.c
SOURCES += $(SRC_DIR)/mby_parser.c
SOURCES += $(SRC_DIR)/mby_rxstats.c
SOURCES += $(SRC_DIR)/mby_txstats.c
SOURCES += $(SRC_DIR)/mby_congmgmt.c
SOURCES += $(SRC_DIR)/mby_pipeline.c
SOURCES += $(SRC_DIR)/mby_modifier.c
SOURCES += $(SRC_DIR)/mby_triggers.c
SOURCES += $(SRC_DIR)/mby_model.c
SOURCES += $(SRC_DIR)/mby_bitfield.c
SOURCES += $(SRC_DIR)/mby_mapper.c

SOURCES += $(TST_DIR)/mby_basic_fwd_test.c
SOURCES += $(TST_DIR)/borrowed/mby_srv_utils.c
SOURCES += $(TST_DIR)/borrowed/mby_srv_nvmimg.c

OBJECTS = $(SOURCES:.c=.o)

TARGET = mby_basic_fwd_test

# C compiler:
CC = gcc

# Linker:
LD  = ld

ifeq ($(VERBOSE),1)
ECHO :=
else
ECHO := @
endif

CFLAGS  = -std=gnu99
CFLAGS += -Wno-long-long
CFLAGS += -Wextra
CFLAGS += -Wall
CFLAGS += -Werror
CFLAGS += -Wno-missing-field-initializers
CFLAGS += -Wno-unused
CFLAGS += -pedantic-errors
CFLAGS += -fPIC
CFLAGS += -Wno-variadic-macros
CFLAGS += -Wno-override-init

ifeq ($(DEBUG),1)
CFLAGS += -O0 -g -ggdb3
else
CFLAGS += -O1
endif

DEFINES   = -D_GNU_SOURCE
DEFINES  += -D_FM_ARCH_x86_64
ifeq ($(DEBUG),1)
endif

LDFLAGS =

DELFILES  =  *.o *.log $(OBJECTS) $(TARGET)

.PHONY: all
all: $(TARGET)

$(SRC_DIR)/mby_reg_ctrl.o  : $(SRC_DIR)/mby_reg_ctrl.c   $(SRC_DIR)/mby_reg_ctrl.h
$(SRC_DIR)/mby_maskgen.o   : $(SRC_DIR)/mby_maskgen.c    $(SRC_DIR)/mby_maskgen.h
$(SRC_DIR)/mby_hash.o      : $(SRC_DIR)/mby_hash.c       $(SRC_DIR)/mby_hash.h
$(SRC_DIR)/mby_nexthop.o   : $(SRC_DIR)/mby_nexthop.c    $(SRC_DIR)/mby_nexthop.h
$(SRC_DIR)/mby_common.o    : $(SRC_DIR)/mby_common.c     $(SRC_DIR)/mby_common.h
$(SRC_DIR)/mby_crc32.o     : $(SRC_DIR)/mby_crc32.c      $(SRC_DIR)/mby_crc32.h
$(SRC_DIR)/mby_classifier.o: $(SRC_DIR)/mby_classifier.c $(SRC_DIR)/mby_classifier.h
$(SRC_DIR)/mby_rxtotx.o    : $(SRC_DIR)/mby_rxtotx.c     $(SRC_DIR)/mby_rxtotx.h
$(SRC_DIR)/mby_parser.o    : $(SRC_DIR)/mby_parser.c     $(SRC_DIR)/mby_parser.h
$(SRC_DIR)/mby_rxstats.o   : $(SRC_DIR)/mby_rxstats.c    $(SRC_DIR)/mby_rxstats.h
$(SRC_DIR)/mby_txstats.o   : $(SRC_DIR)/mby_txstats.c    $(SRC_DIR)/mby_txstats.h
$(SRC_DIR)/mby_congmgmt.o  : $(SRC_DIR)/mby_congmgmt.c   $(SRC_DIR)/mby_congmgmt.h
$(SRC_DIR)/mby_pipeline.o  : $(SRC_DIR)/mby_pipeline.c   $(SRC_DIR)/mby_pipeline.h
$(SRC_DIR)/mby_modifier.o  : $(SRC_DIR)/mby_modifier.c   $(SRC_DIR)/mby_modifier.h
$(SRC_DIR)/mby_triggers.o  : $(SRC_DIR)/mby_triggers.c   $(SRC_DIR)/mby_triggers.h
$(SRC_DIR)/mby_model.o     : $(SRC_DIR)/mby_model.c      $(SRC_DIR)/mby_model.h
$(SRC_DIR)/mby_bitfield.o  : $(SRC_DIR)/mby_bitfield.c   $(SRC_DIR)/mby_bitfield.h
$(SRC_DIR)/mby_mapper.o    : $(SRC_DIR)/mby_mapper.c     $(SRC_DIR)/mby_mapper.h

$(TST_DIR)/borrowed/mby_srv_errno.o : $(TST_DIR)/borrowed/mby_srv_errno.c  $(TST_DIR)/borrowed/mby_srv_errno.h
$(TST_DIR)/borrowed/mby_srv_utils.o : $(TST_DIR)/borrowed/mby_srv_utils.c  $(TST_DIR)/borrowed/mby_srv_utils.h
$(TST_DIR)/borrowed/mby_srv_nvmimg.o: $(TST_DIR)/borrowed/mby_srv_nvmimg.c $(TST_DIR)/borrowed/mby_srv_nvmimg.h
$(TST_DIR)/mby_basic_fwd_test.o     : $(TST_DIR)/mby_basic_fwd_test.c

.PHONY: clean
clean:
	@echo '  Cleaning'
	$(ECHO) rm -f $(DELFILES)

%.o: %.c
	@echo '  Compiling' $<
	$(ECHO) $(CC) -o $@ $(DEFINES) $(CFLAGS) $(INCLUDES) -c $< 

$(TARGET): $(OBJECTS)
	@echo '  Linking' $(TARGET)
	$(ECHO) $(CC) $(LDFLAGS) -o $(TARGET) $(OBJECTS)

.PHONY: run
run: $(TARGET)
	@echo '  Running $(TARGET)'
	@/usr/bin/time --format='Ran for %e seconds.' \
            $(TARGET) | tee run.log

.PHONY: valgrind
valgrind: $(TARGET)
	@echo '  Running $(TARGET) in Valgrind'
	valgrind \
            --track-origins=yes \
            --leak-check=full \
            --show-leak-kinds=all \
	$(TARGET) 2>&1 | tee val.log

.PHONY: callgrind
callgrind: $(TARGET)
	@echo '  Running $(TARGET) in Cachegrind'
	valgrind \
            --tool=callgrind \
            --verbose \
            --dump-every-bb=12500000000 \
	$(TARGET) 2>&1 | tee /cal.log
