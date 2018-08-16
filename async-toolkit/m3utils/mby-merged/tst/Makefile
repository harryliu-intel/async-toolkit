SRC_DIR = ../src
TST_DIR = ../tst

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

DELFILES  =  *.o $(OBJECTS) $(TARGET)

.PHONY: all
all: $(OBJECTS) $(TARGET)

$(SRC_DIR)/mby_common.o :      $(SRC_DIR)/mby_common.c $(SRC_DIR)/mby_common.h
$(SRC_DIR)/fm_crc32.o :        $(SRC_DIR)/fm_crc32.c $(SRC_DIR)/fm_crc32.h $(SRC_DIR)/mby_common.h
$(SRC_DIR)/mby_bitfield.o :    $(SRC_DIR)/mby_bitfield.c $(SRC_DIR)/mby_bitfield.h $(SRC_DIR)/mby_common.h
$(SRC_DIR)/mby_parser.o :      $(SRC_DIR)/mby_parser.c $(SRC_DIR)/mby_parser.h $(SRC_DIR)/mby_common.h
$(TST_DIR)/mby_parser_test.o : $(TST_DIR)/mby_parser_test.c $(TST_DIR)/mby_parser_test.h $(SRC_DIR)/mby_parser.h $(SRC_DIR)/fm_crc32.h $(SRC_DIR)/mby_common.h

%.o: %.c
	@echo '  Compiling' $<
	$(ECHO) $(CC) -o $@ $(DEFINES) $(CFLAGS) $(INCLUDES) -c $< 

$(TARGET):
	@echo '  Linking' $<
	$(CC) $(LDFLAGS) -o $(TARGET) $(TST_DIR)/mby_parser_test.o $(SRC_DIR)/mby_parser.o $(SRC_DIR)/mby_common.o $(SRC_DIR)/fm_crc32.o $(SRC_DIR)/mby_bitfield.o

.PHONY: clean
clean:
	@echo '  Cleaning'
	$(ECHO) rm -f $(DELFILES)
