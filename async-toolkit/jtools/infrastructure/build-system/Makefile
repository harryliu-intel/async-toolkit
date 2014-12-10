

#When we attempt to read a makefile that will add it to the list of dependencies for the main Makefile.
#Once all make file have been loaded make checks to make sure nothing the main make file depends on needs rebuilding.
#If there are things the main Makefile depends on that need rebuild then it will rebuild those things and then
#rerun make.  This variable is used to prevent the loading of genereated dependency file ( .d files ) until after
#all default.mk files have been generated and read in.  The default.mk files in turn read custom.mk, so by ensuring that
#.d files are not read until after all the default.mk files have been read then we can be assured that the .d files will not
#be read until after all the custom.mk files have been read.
LOAD_DEP_FILES :=1
FULCRUM_NO_UTIL_MAKEFILES := 1

ifeq ("$(strip $(BUILD_SYSTEM_ROOT))","")
$(error You must set the variable BUILD_SYSTEM_ROOT to be the path of the root of the build system directory hierarchy.)
endif

include $(BUILD_SYSTEM_ROOT)/systemtypes/default.mk

MAKEFILE_KERNEL_NAME=$(shell uname -s)
MAKEFILE_MACHINE_TYPE=$(shell uname -m)

MAKEFILE_ARCH_NAME=$(MAKEFILE_KERNEL_NAME)-$(MAKEFILE_MACHINE_TYPE)

-include $(BUILD_SYSTEM_ROOT)/systemtypes/$(MAKEFILE_ARCH_NAME).mk

MAKEFILE_HOST_NAME=$(shell hostname)

-include $(BUILD_SYSTEM_ROOT)/systems/$(MAKEFILE_HOST_NAME).mk


BUILD:=$(BUILD_SYSTEM_ROOT)

#Include a sub-make file which defins variables for the commands used in the make system.
include $(BUILD_SYSTEM_ROOT)/commands.mk

default:

.SUFFIXES:

#include a sub-make file to define usefule user-defined functions.
include $(BUILD_SYSTEM_ROOT)/functions.mk

ROOT_PROJECT_DIR := 

ROOT_TARGET_DIR := 

ifeq ("$(strip $(ROOT_PROJECT_DIR))","")
$(error You must set the variable ROOT_PROJECT_DIR to be the path of the root of the source tree you want to build.)
endif

ifeq ("$(strip $(ROOT_TARGET_DIR))","")
$(error You must set the variable ROOT_TARGET_DIR to be the path of the root of the result tree you want to build into.)
endif 

CURR_PROJECT_DIR := $(call CONONICALIZE_PATH,$(ROOT_PROJECT_DIR))

CURR_TARGET_DIR := $(call CONONICALIZE_PATH,$(ROOT_TARGET_DIR))

TOOLS_ROOT := $(ROOT_TARGET_DIR)/tools

TOOLS_SOURCES_ROOT := $(TOOLS_ROOT)/sources

TOOLS_BUILDS_ROOT := $(TOOLS_ROOT)/builds

TOOLS_BINARIES_ROOT := $(TOOLS_ROOT)/binaries

TOOL_VAR_FILES := $(wildcard $(BUILD_SYSTEM_ROOT)/tools/*/vars.mk )
include $(TOOL_VAR_FILES)

# make ues the FIRST rule it finds for a target
# the ONLY way to make defaults.mk, so put it first
%/defaults.mk: $(DEFAULTS_MK_GEN_SCRIPT) $(DEFAULTS_MK_TEMPL)
	@$(DEFAULTS_MK_GEN_CMD) "$(@D)"

# include top level custom.mk
ifeq ("$(strip $(NO_CUSTOM))","")
-include $(CURR_PROJECT_DIR)/custom.mk
endif

ifeq ($(MAKE_PWD_ONLY),1)
ifneq ("$(strip $(MAKE_PWD_ONLY_TARGETS))","")
MKPWDONLY_TARGETS := $(MAKE_PWD_ONLY_TARGETS)
else
MKPWDONLY_TARGETS := $(MAKECMDGOALS)
endif
endif


ifneq ("$(strip $(FILE_TYPES))","")
FILE_TYPE_ONCE_MK_FILES := $(wildcard $(BUILD_SYSTEM_ROOT)/filetypes/*/once.mk )
else
FILE_TYPE_ONCE_MK_FILES := $(wildcard $(BUILD)/filetypes/*/once.mk )
endif

include $(FILE_TYPE_ONCE_MK_FILES)


include $(BUILD_SYSTEM_ROOT)/defaults.common.mk

include $(BUILD_SYSTEM_ROOT)/defaults.subincludes.mk

include $(BUILD_SYSTEM_ROOT)/dir_exit.mk


allpdf: $(ALL_PDF_FILES)

allps: $(ALL_PS_FILES)

allpreviewps: $(ALL_PREVIEW_PS_FILES)

#$(error $(ALL_DEP_FILES) $(LOAD_DEP_FILES))

ifeq ($(LOAD_DEP_FILES),1)
-include $(ALL_DEP_FILES)
endif

#Rule to clean up temporary files and results.
clean: $(CURR_TARGET_DIR)/cleanresults $(CURR_TARGET_DIR)/cleanintermediates #$(CURR_TARGET_DIR)/cleantools

ifneq ("$(CURR_TARGET_DIR)",".")
ifneq ("$(CURR_TARGET_DIR)","./")
cleanall: $(CURR_TARGET_DIR)/cleanall #$(CURR_TARGET_DIR)/cleantools
endif
endif

CURR_INTERMEDIATE_FILES := 
CURR_RESULT_FILES :=
CURR_INTERMEDIATE_MAKE_FILES := 

#$(CURR_TARGET_DIR)/cleantools:
#	rm -rf $(TOOLS_ROOT)

#TOOL_RULES_FILES := $(wildcard $(BUILD_SYSTEM_ROOT)/tools/*/rules.mk )
#include $(TOOL_RULES_FILES)

#$(TOOLS_BINARIES_TARGETS):
#	make -f $(BUILD_SYSTEM_ROOT)/Makefile "ROOT_TARGET_DIR=$(TOOLS_BUILDS_ROOT)" "ROOT_PROJECT_DIR=$(TOOLS_SOURCES_ROOT)" $@

