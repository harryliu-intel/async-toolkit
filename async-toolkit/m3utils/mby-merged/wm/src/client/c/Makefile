# C/C++ compiler:
CC = gcc

# Linker:
LD  = ld

# Recursive forced remove:
RM = rm -rf

ROOT_DIR := $(shell dirname $(realpath $(lastword $(MAKEFILE_LIST))))

MBY_TCP_CLIENT = mby_tcp_client

TARGET_LIB_O = $(MBY_TCP_CLIENT)_library.o
TARGET_LIB   = lib$(MBY_TCP_CLIENT).so
TARGET_SAM_O = $(MBY_TCP_CLIENT)_sample.o
TARGET_SAM   = $(MBY_TCP_CLIENT)_sample

TARGET_ARGS  = -m ../../main/m3/model_server/AMD64_LINUX/models.packetServer

ifeq ($(VERBOSE),1)
ECHO :=
else
ECHO := @
endif

CFLAGS  = -std=gnu99
CFLAGS += -Wall
CFLAGS += -Wextra
CFLAGS += -Werror
CFLAGS += -Wno-error=unused-result  # downgrade to warning instead of error
CFLAGS += -Wno-long-long
CFLAGS += -Wno-missing-field-initializers
CFLAGS += -Wno-unused
CFLAGS += -pedantic-errors
CFLAGS += -fPIC
#FLAGS += -Wno-variadic-macros # mby_client.c uses variadic macros!
CFLAGS += -Wno-override-init

ifeq ($(DEBUG),1)
CFLAGS += -O0 -g -ggdb3
else
CFLAGS += -O1
endif

# by default NO_SV=1 to disable SV-specific code:
NO_SV ?= 1

# if NO_SV=1, disable SV-specific code:
ifeq ($(NO_SV),1)
	CFLAGS += -DNO_SV
else
	VCS_INCLUDES = $(shell ToolConfig.pl get_tool_path vcsmx)/include
	CFLAGS += -iquote $(VCS_INCLUDES)
endif

# optionally specify a read timeout (in milliseconds):
ifneq ($(READ_TIMEOUT),)
	CFLAGS += -DREAD_TIMEOUT=$(READ_TIMEOUT)
endif

INCLUDES = -I.

DEFINES =
ifeq ($(DEBUG),1)
endif

LDFLAGS  = -shared
LDFLAGS += -Wl,-rpath,.

.PHONY: all
all: $(TARGET_DIR) $(TARGET_LIB) $(TARGET_SAM)

$(TARGET_LIB_O): $(MBY_TCP_CLIENT)_library.c $(MBY_TCP_CLIENT)_types.h $(MBY_TCP_CLIENT)_library.h
	@echo '  Compiling library'
	$(ECHO) $(CC) -o $@ $(DEFINES) $(CFLAGS) $(INCLUDES) -c $<

$(TARGET_LIB): $(MBY_TCP_CLIENT)_library.o
	@echo '  Linking library'
	$(ECHO) $(CC) $(LDFLAGS) -o $@ $^

$(TARGET_SAM_O): $(MBY_TCP_CLIENT)_sample.c $(MBY_TCP_CLIENT)_types.h $(MBY_TCP_CLIENT)_library.h
	@echo '  Compiling client'
	$(ECHO) $(CC) -o $@ $(DEFINES) $(CFLAGS) $(INCLUDES) -c $<

$(TARGET_SAM): $(MBY_TCP_CLIENT)_sample.o $(TARGET_LIB)
	@echo '  Linking client'
	$(ECHO) $(CC) -o $(TARGET_SAM) $(MBY_TCP_CLIENT)_sample.o -Wl,-rpath,$(ROOT_DIR) -L. -l$(MBY_TCP_CLIENT)

.PHONY: run
run: $(TARGET_SAM)
	@echo '  Running $(TARGET_SAM)'
	./$(TARGET_SAM) $(TARGET_ARGS) 2>&1 | tee run.log

.PHONY: valgrind
valgrind: $(TARGET_SAM)
	@echo '  Running $(TARGET_SAM) in Valgrind'
	valgrind \
            --track-origins=yes \
            --leak-check=full \
            --show-leak-kinds=all \
	./$(TARGET_SAM) $(TARGET_ARGS) 2>&1 | tee val.log

.PHONY: clean
clean:
	@echo '  Cleaning'
	$(ECHO) $(RM) $(TARGET_LIB_O) $(TARGET_LIB) $(TARGET_SAM_O) $(TARGET_SAM) *.o *.log
