# -*- mode:Makefile -*-

# Copyright (C) 2018 Intel Corporation

#REFIX   = /home/tjnagler/cg/hlp/wm-dir/wm/nd_ies-hlp_wm/ies
PREFIX   = ../../../../nd_ies-hlp_wm/ies

SRC_DIR1 = $(PREFIX)/src/platforms/whiteModelLib
SRC_DIR2 = $(PREFIX)/src/platforms/common/model/hlp
SRC_DIR3 = $(PREFIX)/src/platforms/common/model/hlp/debug
SRC_DIR4 = $(PREFIX)/src/common
SRC_DIR5 = $(PREFIX)/examples/expmt/src/c

INC_DIR0  = $(PREFIX)/include
INC_DIR1  = $(PREFIX)/include/platforms
INC_DIR2  = $(PREFIX)/include/platforms/whiteModelLib
INC_DIR3  = $(PREFIX)/include/platforms/whiteModelLib/kernel
INC_DIR4  = $(PREFIX)/testing/include
INC_DIR5  = $(PREFIX)/include/alos
INC_DIR6  = $(PREFIX)/include/alos/linux
INC_DIR7  = $(PREFIX)/include/std/intel
INC_DIR8  = $(PREFIX)/applications/support/include
INC_DIR9  = $(PREFIX)/applications/protocols/include
INC_DIR10 = $(PREFIX)/examples/expmt/src/c

INCLUDES  = -I$(INC_DIR0) -I$(INC_DIR1) -I$(INC_DIR2) -I$(INC_DIR3) -I$(INC_DIR4) \
            -I$(INC_DIR5) -I$(INC_DIR6) -I$(INC_DIR7) -I$(INC_DIR8) -I$(INC_DIR9) -I$(INC_DIR10)

OBJ_DIR  = ./obj
DEP_DIR  = ./dep
BIN_DIR  = ./bin
LIB_DIR  = ./lib
DAT_DIR  = ./dat
LOG_DIR  = ./log

#INARY  = $(BIN_DIR)/msg_dump_inject_test
BINARY  = $(BIN_DIR)/model_server
LIBRARY = $(LIB_DIR)/libWm.so

#_NAME  = AclActionRam_hlp-model
#T_NAME  = AclApiFunctions_hlp-model
T_NAME   = VlanTagging_hlp-model_519606380
T_ARGS  = -d 0 -inj dat/$(T_NAME)

SOURCES  = $(SRC_DIR1)/fm_model_lib.c

SOURCES += $(SRC_DIR2)/hlp_model_learning.c
SOURCES += $(SRC_DIR2)/hlp_model_parser.c
SOURCES += $(SRC_DIR2)/hlp_model_triggers.c
SOURCES += $(SRC_DIR2)/hlp_model_gen_mask.c
SOURCES += $(SRC_DIR2)/hlp_model_stats.c
SOURCES += $(SRC_DIR2)/hlp_model_mgmt.c
SOURCES += $(SRC_DIR2)/hlp_model_mapper.c
SOURCES += $(SRC_DIR2)/hlp_model_reg_ctrl.c
SOURCES += $(SRC_DIR2)/hlp_model_modify.c
SOURCES += $(SRC_DIR2)/hlp_model_next_hop.c
SOURCES += $(SRC_DIR2)/hlp_model_xbar.c
SOURCES += $(SRC_DIR2)/hlp_model_policer.c
SOURCES += $(SRC_DIR2)/hlp_model_basic.c
SOURCES += $(SRC_DIR2)/hlp_model_glort.c
SOURCES += $(SRC_DIR2)/hlp_model_l2_lookup.c
SOURCES += $(SRC_DIR2)/hlp_model_platform.c
SOURCES += $(SRC_DIR2)/hlp_model_macsec.c
SOURCES += $(SRC_DIR2)/hlp_model_cm.c
SOURCES += $(SRC_DIR2)/hlp_model_fsched.c
SOURCES += $(SRC_DIR2)/hlp_model_ffu_final_actions.c
SOURCES += $(SRC_DIR2)/hlp_model_ffu_classifier.c
SOURCES += $(SRC_DIR2)/hlp_model_mac.c
SOURCES += $(SRC_DIR2)/hlp_model_macsec_aux.c
SOURCES += $(SRC_DIR2)/hlp_model_main.c
SOURCES += $(SRC_DIR2)/hlp_model_hash.c

SOURCES += $(SRC_DIR3)/hlp_model_debug.c

SOURCES += $(SRC_DIR4)/fm_bitarray.c
SOURCES += $(SRC_DIR4)/fm_bitfield.c
SOURCES += $(SRC_DIR4)/fm_c11_annex_k.c
SOURCES += $(SRC_DIR4)/fm_crc32.c
SOURCES += $(SRC_DIR4)/fm_string.c

SOURCES += $(SRC_DIR5)/msg_dump_inject.c
#OURCES += $(SRC_DIR5)/msg_dump_inject_test.c
SOURCES += $(SRC_DIR5)/model_server.c

OBJECTS = $(addprefix $(OBJ_DIR)/, $(notdir $(SOURCES:.c=.o)))

# C compiler:
CC = gcc

# Linker:
LD  = ld

ifeq ($(VERBOSE),1)
ECHO := 
else
ECHO := @
endif

DEPFLAGS = -MT $@ -MMD -MP -MF $(DEP_DIR)/$*.Td

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
CFLAGS += $(DEPFLAGS)
CFLAGS += -O0 -g -ggdb3
else
CFLAGS += -O1
endif

DEFINES   = -D_GNU_SOURCE
DEFINES  += -D_FM_ARCH_x86_64
DEFINES  += -DINSTRUMENT_LOG_LEVEL=0
DEFINES  += -DDMPINJ_MODS
ifeq ($(DEBUG),1)
DEFINES  += -DDEBUG_PRINT_RECEIVED_PACKET
endif

#LIB_DIR  = /home/tjnagler/cg/hlp/wm-dir/wm/nd_ies-hlp_wm/ies/build

LDFLAGS   = -std=gnu99
LDFLAGS  += -fPIC
#DFLAGS  += -lm 
#DFLAGS  += -lpthread
#DFLAGS  += -Wl,-rpath,$(LIB_DIR)
#DFLAGS  += -L$(LIB_DIR) -lWm

DELFILES  = $(OBJECTS) $(BINARY) $(LIBRARY) $(OBJ_DIR)/*.o $(LOG_DIR)/*.log

.PHONY: all
all: $(BINARY) $(LIBRARY)

$(OBJ_DIR)/%.o: $(SRC_DIR1)/%.c
	@echo '  Compiling' $<
	$(ECHO) $(CC) -o $@ $(DEFINES) $(CFLAGS) $(INCLUDES) -c $< 

$(OBJ_DIR)/%.o: $(SRC_DIR2)/%.c
	@echo '  Compiling' $<
	$(ECHO) $(CC) -o $@ $(DEFINES) $(CFLAGS) $(INCLUDES) -c $< 

$(OBJ_DIR)/%.o: $(SRC_DIR3)/%.c
	@echo '  Compiling' $<
	$(ECHO) $(CC) -o $@ $(DEFINES) $(CFLAGS) $(INCLUDES) -c $< 

$(OBJ_DIR)/%.o: $(SRC_DIR4)/%.c
	@echo '  Compiling' $<
	$(ECHO) $(CC) -o $@ $(DEFINES) $(CFLAGS) $(INCLUDES) -c $< 

$(OBJ_DIR)/%.o: $(SRC_DIR5)/%.c
	@echo '  Compiling' $<
	$(ECHO) $(CC) -o $@ $(DEFINES) $(CFLAGS) $(INCLUDES) -c $< 

$(OBJ_DIR)/libWm.lds: 	
	@echo '  Locating shared library at: 0x02000000'
	$(ECHO) $(CC) -shared -Wl,--verbose -o $(OBJ_DIR)/a.out 2>&1 \
        | sed -e '/^======/,/^======/!d' -e '/^======/d;s/0\(.*\)\(+ SIZEOF_HEADERS\)/0x02000000\1\2/' > $(OBJ_DIR)/libWm.lds

$(BINARY): $(OBJECTS)
	@echo '  Linking' $(BINARY)
	$(ECHO) $(CC) -o $(BINARY) $(OBJECTS) $(LDFLAGS)

$(LIBRARY): $(OBJECTS) $(OBJ_DIR)/libWm.lds
	@echo '  Linking' $(LIBRARY)
	$(ECHO) $(CC) -Wl,-T,$(OBJ_DIR)/libWm.lds $(LDFLAGS) -shared -o $(LIBRARY) $(OBJECTS) -lpthread -lrt

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

.PHONY: show_cflags
show_cflags:
	@echo 'CFLAGS :'
	@$(foreach i, $(CFLAGS), echo $(i);)

.PHONY: show_ldflags
show_ldflags:
	@echo 'LDFLAGS :'
	@$(foreach i, $(LDFLAGS), echo $(i);)

.PHONY: run
run: $(BINARY)
	@echo '  Running $(BINARY) $(T_ARGS)'
	@/usr/bin/time --format='Ran for %e seconds.' \
            $(BINARY) $(T_ARGS) | tee $(LOG_DIR)/c-run.log

.PHONY: valgrind
valgrind: $(BINARY)
	@echo '  Running $(BINARY) $(T_ARGS) in Valgrind'
	valgrind \
            --track-origins=yes \
            --leak-check=full \
            --show-leak-kinds=all \
	$(BINARY) $(T_ARGS) 2>&1 | tee $(LOG_DIR)/c-valgrind.log

.PHONY: callgrind
callgrind: $(BINARY)
	@echo '  Running $(BINARY) $(T_ARGS) in Callgrind'
	valgrind \
            --tool=callgrind \
            --verbose \
            --dump-every-bb=20000000000 \
	$(BINARY) $(T_ARGS) 2>&1 | tee $(LOG_DIR)/c-callgrind.log

.PHONY: clean
clean:
	@echo '  Cleaning'
	$(ECHO) rm -f $(DELFILES)
