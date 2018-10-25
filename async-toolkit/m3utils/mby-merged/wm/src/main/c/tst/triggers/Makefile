SRC_DIR  = ../..
TST_DIR  = .

INC_DIR  = ../../../c                                  # uint.h
INC_DIR2 = ../../../m3/genviews/src/build_c/mby_c/src  # mby_top_map.h

INCLUDES  = -I$(SRC_DIR)
INCLUDES += -I$(TST_DIR)
INCLUDES += -I$(TST_DIR)/../common
INCLUDES += -I$(INC_DIR) -I$(INC_DIR2)

SOURCES  = $(SRC_DIR)/mby_common.c
SOURCES += $(SRC_DIR)/mby_crc32.c
SOURCES += $(SRC_DIR)/mby_bitfield.c
SOURCES += $(SRC_DIR)/mby_triggers.c
SOURCES += $(SRC_DIR)/mby_triggers_regs.c
SOURCES += $(SRC_DIR)/mby_reg_ctrl.c


SOURCES += $(TST_DIR)/mby_triggers_test.c

OBJECTS = $(SOURCES:.c=.o)

TARGET = mby_triggers_test

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

$(SRC_DIR)/mby_common.o    		: $(SRC_DIR)/mby_common.c     		$(SRC_DIR)/mby_common.h
$(SRC_DIR)/mby_crc32.o     		: $(SRC_DIR)/mby_crc32.c      		$(SRC_DIR)/mby_crc32.h
$(SRC_DIR)/mby_bitfield.o  		: $(SRC_DIR)/mby_bitfield.c   		$(SRC_DIR)/mby_bitfield.h
$(SRC_DIR)/mby_triggers.o  		: $(SRC_DIR)/mby_triggers.c   		$(SRC_DIR)/mby_triggers.h
$(SRC_DIR)/mby_triggers_regs.o  : $(SRC_DIR)/mby_triggers_regs.c   	$(SRC_DIR)/mby_triggers_regs.h
$(SRC_DIR)/mby_reg_ctrl.o  		: $(SRC_DIR)/mby_reg_ctrl.c   		$(SRC_DIR)/mby_reg_ctrl.h

$(TST_DIR)/mby_triggers_test.o : $(TST_DIR)/mby_triggers_test.c $(SRC_DIR)/mby_triggers.h $(SRC_DIR)/mby_common.h

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
            ./$(TARGET) | tee run.log

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
