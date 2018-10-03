SRC_DIR  = ../../src
TST_DIR  = .

INC_DIR  = ../../../c                                  # uint.h
INC_DIR2 = ../../../m3/genviews/src/build_c/mby_c/src  # mby_top_map.h

INCLUDES  = -I$(SRC_DIR)
INCLUDES += -I$(TST_DIR)
INCLUDES += -I$(INC_DIR) -I$(INC_DIR2)

SOURCES  = $(SRC_DIR)/mby_common.c
SOURCES += $(SRC_DIR)/mby_crc32.c
SOURCES += $(SRC_DIR)/mby_bitfield.c
SOURCES += $(SRC_DIR)/mby_lpm.c
SOURCES += $(SRC_DIR)/mby_lpm_regs.c
SOURCES += $(SRC_DIR)/mby_wcm.c
SOURCES += $(SRC_DIR)/mby_exactmatch.c
SOURCES += $(SRC_DIR)/mby_clsfr_regs.c
SOURCES += $(SRC_DIR)/mby_classifier.c
SOURCES += $(SRC_DIR)/mby_reg_ctrl.c

SOURCES += $(TST_DIR)/mby_classifier_test.c

OBJECTS = $(SOURCES:.c=.o)

TARGET = mby_classifier_test

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

ifeq ($(USE_NEW_CSRS),1)
CFLAGS += -DUSE_NEW_CSRS
endif

LDFLAGS =

DELFILES  =  *.o *.log $(OBJECTS) $(TARGET)

.PHONY: all
all: $(TARGET)

$(SRC_DIR)/mby_common.o    : $(SRC_DIR)/mby_common.c     $(SRC_DIR)/mby_common.h
$(SRC_DIR)/mby_crc32.o     : $(SRC_DIR)/mby_crc32.c      $(SRC_DIR)/mby_crc32.h
$(SRC_DIR)/mby_bitfield.o  : $(SRC_DIR)/mby_bitfield.c   $(SRC_DIR)/mby_bitfield.h
$(SRC_DIR)/mby_lpm.o       : $(SRC_DIR)/mby_lpm.c        $(SRC_DIR)/mby_lpm.h
$(SRC_DIR)/mby_lpm_regs.o  : $(SRC_DIR)/mby_lpm_regs.c   $(SRC_DIR)/mby_lpm_regs.h
$(SRC_DIR)/mby_wcm.o       : $(SRC_DIR)/mby_wcm.c        $(SRC_DIR)/mby_wcm.h
$(SRC_DIR)/mby_exactmatch.o: $(SRC_DIR)/mby_exactmatch.c $(SRC_DIR)/mby_exactmatch.h
$(SRC_DIR)/mby_clsfr_regs.o: $(SRC_DIR)/mby_clsfr_regs.c $(SRC_DIR)/mby_clsfr_regs.h
$(SRC_DIR)/mby_classifier.o: $(SRC_DIR)/mby_classifier.c $(SRC_DIR)/mby_classifier.h
$(SRC_DIR)/mby_reg_ctrl.o  : $(SRC_DIR)/mby_reg_ctrl.c   $(SRC_DIR)/mby_reg_ctrl.h

$(TST_DIR)/mby_classifier_test.o : $(TST_DIR)/mby_classifier_test.c $(SRC_DIR)/mby_classifier.h $(SRC_DIR)/mby_common.h

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
