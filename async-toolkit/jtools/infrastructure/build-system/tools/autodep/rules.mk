AUTODEP_TAR_FILE := $(BUILD_SYSTEM_ROOT)/tools/autodep/autodep-0.2.1-fulcrum.tar.gz

AUTODEP_SOURCE_ROOT := $(ROOT_TARGET_DIR)/tools/sources/autodep

AUTODEP_BUILD_ROOT := $(ROOT_TARGET_DIR)/tools/builds/autodep


AUTODEP_MAKE_FILES_DIR := $(BUILD_SYSTEM_ROOT)/tools/autodep

CURR_INTERMEDIATE_MAKE_FILES := $(AUTODEP_SOURCE_ROOT)/autodeptar.d $(CURR_INTERMEDIATE_MAKE_FILES)

CURR_PROJECT_DIR := $(call CONONICALIZE_PATH,$(AUTODEP_SOURCE_ROOT))

CURR_TARGET_DIR := $(call CONONICALIZE_PATH,$(AUTODEP_BUILD_ROOT))

CURR_PROJECT_NAME := autodep

autodep: $(AUTODEP_BIN)

TOOLS_BINARIES_TARGETS := $(AUTODEP_BUILD_ROOT)/autodep $(AUTODEP_BUILD_ROOT)/autodep.so $(TOOLS_BINARIES_TARGETS)

CURR_RESULT_FILES:= $(AUTODEP_BIN) $(AUTODEP_LIB) $(CURR_RESULT_FILES)

$(AUTODEP_BUILD_ROOT)/autodep $(AUTODEP_BUILD_ROOT)/autodep.so : $(AUTODEP_SOURCE_ROOT)/.sometimesmakesucks

$(AUTODEP_BIN): $(AUTODEP_BUILD_ROOT)/autodep $(AUTODEP_LIB) $(AUTODEP_BIN_DIR)/.sometimesmakesucks
	cp $< $@

$(AUTODEP_LIB): $(AUTODEP_BUILD_ROOT)/autodep.so $(AUTODEP_LIB_DIR)/.sometimesmakesucks
	cp $< $@

$(AUTODEP_SOURCE_ROOT)/.sometimesmakesucks: $(AUTODEP_TAR_FILE)
	$(BUILD_SYSTEM_ROOT)/safemkdir.zsh $(@D)
	tar -C $(@D) -xmzf $<
	touch $@

$(AUTODEP_BUILD_ROOT)/.sometimesmakesucks:
	$(BUILD_SYSTEM_ROOT)/safemkdir.zsh $(@D)
	touch $@

$(AUTODEP_LIB_DIR)/.sometimesmakesucks:
	$(BUILD_SYSTEM_ROOT)/safemkdir.zsh $(@D)
	touch $@

$(AUTODEP_BIN_DIR)/.sometimesmakesucks:
	$(BUILD_SYSTEM_ROOT)/safemkdir.zsh $(@D)
	touch $@

CURR_INTERMEDIATE_FILES := $(AUTODEP_SOURCE_ROOT)/.sometimesmakesucks \
                           $(AUTODEP_BUILD_ROOT)/.sometimesmakesucks  \
                           $(AUTODEP_LIB_DIR)/.sometimesmakesucks     \
                           $(AUTODEP_BIN_DIR)/.sometimesmakesucks     \
                           $(CURR_INTERMEDIATE_FILES)


#include $(AUTODEP_BUILD_ROOT)/defaults.mk




