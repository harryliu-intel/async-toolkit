SRC_DIR = ../../src
TST_DIR = .

INCLUDES  = -I$(SRC_DIR)
INCLUDES += -I$(TST_DIR)

SOURCES  = $(SRC_DIR)/fm_crc32.c
SOURCES += $(SRC_DIR)/mby_bitfield.c
SOURCES += $(SRC_DIR)/mby_common.c
SOURCES += $(SRC_DIR)/mby_parser.c
SOURCES += $(TST_DIR)/mby_parser_test.c

OBJECTS = $(SOURCES:.c=.o)

TARGET = mby_parser_test

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

$(SRC_DIR)/mby_common.o :      $(SRC_DIR)/mby_common.c $(SRC_DIR)/mby_common.h
$(SRC_DIR)/fm_crc32.o :        $(SRC_DIR)/fm_crc32.c $(SRC_DIR)/fm_crc32.h $(SRC_DIR)/mby_common.h
$(SRC_DIR)/mby_bitfield.o :    $(SRC_DIR)/mby_bitfield.c $(SRC_DIR)/mby_bitfield.h $(SRC_DIR)/mby_common.h
$(SRC_DIR)/mby_parser.o :      $(SRC_DIR)/mby_parser.c $(SRC_DIR)/mby_parser.h $(SRC_DIR)/mby_common.h
$(TST_DIR)/mby_parser_test.o : $(TST_DIR)/mby_parser_test.c $(TST_DIR)/mby_parser_test.h $(SRC_DIR)/mby_parser.h $(SRC_DIR)/fm_crc32.h $(SRC_DIR)/mby_common.h

.PHONY: clean
clean:
	@echo '  Cleaning'
	$(ECHO) rm -f $(DELFILES)

%.o: %.c
	@echo '  Compiling' $<
	$(ECHO) $(CC) -o $@ $(DEFINES) $(CFLAGS) $(INCLUDES) -c $< 

$(TARGET): $(OBJECTS)
	@echo '  Linking' $<
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
