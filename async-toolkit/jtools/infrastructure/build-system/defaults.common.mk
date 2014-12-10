#all the stuff to do upon entering a directory but before recursing to sub directories.

#Reset variables for this directory.
include $(BUILD)/var_reset.mk

include $(BUILD)/calc_sub_dirs_info.mk


#Get all per directory rules for each file type we support.

#List of dir.mk files to include for supported file types.
ifneq ("$(strip $(FILE_TYPES))","")
FILE_TYPE_DIR_MK_FILES := $(addsuffix /dir.mk, $(addprefix $(BUILD)/filetypes/, $(FILE_TYPES) ) )
else
FILE_TYPE_DIR_MK_FILES := $(wildcard $(BUILD)/filetypes/*/dir.mk )
endif

#Include the dir.mk files
include $(FILE_TYPE_DIR_MK_FILES)


#Include the custom.mk for the current directory if one exists.
ifeq ("$(strip $(NO_CUSTOM))","")
CUSTOM_MK := $(CURR_PROJECT_DIR)/custom.mk
-include $(CUSTOM_MK)
endif

CURR_NON_BUILD_DIRS := $(CURR_NON_BUILD_DIRS) CVS
include $(BUILD)/calc_sub_dirs_info.mk

#calculate variables for sub targets default.mk's.
include $(BUILD)/calc_sub_target_mk_include_info.mk

CURR_INTERMEDIATE_MAKE_FILES := $(CURR_SUB_TARGET_MK_INCLUDES) \
$(CURR_TARGET_DEPS) \
$(CURR_TARGET_DOC_DEPS) \
$(CURR_INTERMEDIATE_MAKE_FILES)

ALL_DEP_FILES := $(ALL_DEP_FILES) $(CURR_TARGET_DEPS) $(CURR_TARGET_DOC_DEPS)

include $(BUILD)/infrastructure.mk

include $(BUILD)/defaults.clean.mk
