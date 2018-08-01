##############################################################################
# File:          Makefile
# Creation Date: 05/12/2017
# Description:   Makefile to generate registers source code
#
#                -------------------------------------------------
#                INTERNAL USE ONLY - NOT FOR EXTERNAL DISTRIBUTION
#                -------------------------------------------------
#
# INTEL CONFIDENTIAL
# Copyright 2017 Intel Corporation. All Rights Reserved.
#
# The source code contained or described herein and all documents related
# to the source code ("Material") are owned by Intel Corporation or its
# suppliers or licensors. Title to the Material remains with Intel
# Corporation or its suppliers and licensors. The Material contains trade
# secrets and proprietary and confidential information of Intel or its
# suppliers and licensors. The Material is protected by worldwide copyright
# and trade secret laws and treaty provisions. No part of the Material may
# be used, copied, reproduced, modified, published, uploaded, posted,
# transmitted, distributed, or disclosed in any way without Intel's prior
# express written permission.
#
# No license under any patent, copyright, trade secret or other intellectual
# property right is granted to or conferred upon you by disclosure or
# delivery of the Materials, either expressly, by implication, inducement,
# estoppel or otherwise. Any license under such intellectual property rights
# must be express and approved by Intel in writing.
##############################################################################

#############################################################################
#                               CONFIGURABLES                               #
#############################################################################

# Specifies the default RTL version number.  This must match the directory
# name (no path) that contains the RDL files for this particular version.
RTL_VERSION_DEFAULT = wm

#############################################################################
#                                 BOOTSTRAP                                 #
#############################################################################

# Find a parent directory that contains a specified subdir or file.
# Can be used instead of ../.. paths to find parent directories.
define findParentDir
	$(shell 
		SPATH=`pwd`; 
		pathToFind="$(strip $(1))"; 
		while [ "$${SPATH}" != "" ]; do 
			if [ -e "$${SPATH}/$${pathToFind}" ]; then 
				break; 
			else 
				SPATH=`dirname $$SPATH`;
			fi; 
		done;
		echo $${SPATH};
	)
endef

#############################################################################
#                                  GLOBALS                                  #
#############################################################################

MAKEFILE_DIR 		= $(abspath $(dir $(lastword $(MAKEFILE_LIST))))

# Force tool versions
export USER_ITOOLS	=
export PROJECT_ITOOLS = $(MAKEFILE_DIR)/Makefile.itools

RTL_VERSION			?= $(RTL_VERSION_DEFAULT)
RTL_VERSION			:= $(strip $(RTL_VERSION))

CO_ROOT			   ?= $(realpath $(call findParentDir,core))
RDL_BASE_DIR		= $(abspath $(MAKEFILE_DIR)/rdl)
RDL_DIR			   ?= $(abspath $(RDL_BASE_DIR)/$(RTL_VERSION))
TEMPLATE_DIR		= $(abspath $(MAKEFILE_DIR)/templates)
GENERATED_BASE_DIR	= $(abspath $(MAKEFILE_DIR)/generated)
GENERATED_DIR		= $(abspath $(GENERATED_BASE_DIR)/$(RTL_VERSION))
TEMPLATES 			= $(subst $(TEMPLATE_DIR)/,,\
						$(shell find $(TEMPLATE_DIR) -type f -name '*.template'))
REG_GEN				= $(abspath $(MAKEFILE_DIR)/rdl.rb)
TOP_MAP_RDL			= "mby_top_map.rdl"

#############################################################################
#                                  TARGETS                                  #
#############################################################################

.DEFAULT_GOAL 		:= all

help:
	@echo
	@echo "Register Source Code Generator Help"
	@echo
	@echo "------------------------------------------------------------------"
	@echo "ENV VARIABLES       DESCRIPTION"
	@echo "------------------------------------------------------------------"
	@echo "RTL_VERSION         RTL version to generate and/or install."
	@echo "                    Default is $(RTL_VERSION_DEFAULT)"
	@echo 
	@echo "RDL_DIR             When RTL_VERSION=wm, this variable specifies"
	@echo "                    the path to the RDL files.  Normally, these"
	@echo "                    would be present in the WM repository."
	@echo
	@echo "RDL_SRC_DIR         Path to the RDL files for the update target."
	@echo 
	@echo "WM_ROOT             Path to the White Model repository, for the"
	@echo "                    install target to install generated WM content."
	@echo
	@echo "FPPS_REG_VERSION    The registers version number"
	@echo
	@echo "FPPS_REG_TAG        The registers tag number"
	@echo
	@echo "------------------------------------------------------------------"
	@echo "TARGET              DESCRIPTION"
	@echo "------------------------------------------------------------------"
	@echo "clean               Remove all generated content"
	@echo 
	@echo "gen                 Generate source code from registers for the RTL"
	@echo "                    version specified with RTL_VERSION=<ver>.  The"
	@echo "                    default version is $(RTL_VERSION_DEFAULT)."
	@echo 
	@echo "install             Install generated files into the API."
	@echo "                    if WM_ROOT=<path> is specified, the white model"
	@echo "                    files will also be installed."
	@echo
	@echo "list                List available RTL versions to generate from."
	@echo
	@echo "all                 Default: clean, gen, install"
	@echo 
	@echo "update              Updates register set from a release."
	@echo "                    This is only done to add support for a new"
	@echo "                    RTL version.  Requires the environment variables"
	@echo "                    RTL_VERSION=<version> and RDL_SRC_DIR=<path> to "
	@echo "                    be set to the new version id and the path to the"
	@echo "                    RDL files directory, respectively."
	@echo
	@echo "------------------------------------------------------------------"
	@echo "EXAMPLES"
	@echo "------------------------------------------------------------------"
	@echo "Generate code from registers source and install, including white "
	@echo "model files.  Using the default RTL version."
	@echo "FPPS_REG_VERSION=0x1006 FPPS_REG_TAG=0x11749 WM_ROOT=/path/wm \\"
	@echo "RDL_DIR=/path/to/wm/srdl make"
	@echo 
	@echo "Generate code from registers source and install, don't install"
	@echo "white model files.  Using the default RTL version."
	@echo "FPPS_REG_VERSION=0x1006 FPPS_REG_TAG=0x11749 \\"
	@echo "RDL_DIR=/path/to/wm/srdl make"
	@echo
	@echo "Generate code from registers source and install, no version info "
	@echo "or white model files.  Using the default RTL version."
	@echo "RDL_DIR=/path/to/wm/srdl make"
	@echo
	@echo "Generate code from register source and install, no version info "
	@echo "or white model files.  Using RTL version 0P8-16ww51e_p1."
	@echo "RTL_VERSION=0P8-16ww51e_p1 make"
	@echo
	@echo "Add or update a RDL set from the DV or white model repository."
	@echo "RTL_VERSION=0P8-16ww51e_p1 RDL_SRC_DIR=/path/to/srdl make update"

clean_all:
	rm -rf $(GENERATED_BASE_DIR)

clean:
	rm -rf $(GENERATED_DIR)

gen:
	@echo "Generating register source code for RTL version: $(RTL_VERSION)"
	@if [ "$(RDL_DIR)" == "" -o ! -d $(RDL_DIR) ]; then \
		echo "ERROR: RDL_DIR does not exist: $(RDL_DIR)"; \
		exit 1; \
	fi
	@mkdir -p $(GENERATED_DIR) && \
	cd $(RDL_DIR) && \
		for template in $(TEMPLATES:.template=); do \
			echo "$(notdir $(TEMPLATE_DIR))/$$template.template -> $(subst $(MAKEFILE_DIR)/,,$(GENERATED_DIR))/$$template"; \
			mkdir -p `dirname $(GENERATED_DIR)/$$template` && \
			$(REG_GEN) \
				$(notdir $(TEMPLATE_DIR))/$$template \
				$(TOP_MAP_RDL) \
				$(notdir $(GENERATED_BASE_DIR))/$(RTL_VERSION)/$$template; \
		done

install:
	$(eval API_DIR = $(GENERATED_DIR)/api)
	$(eval FILE_LIST = $(subst $(API_DIR)/,,$(shell find $(API_DIR) -type f)))
	$(eval WM_DIR = $(GENERATED_DIR)/wm)
	$(eval WM_FILE_LIST = $(subst $(WM_DIR)/,,$(shell find $(WM_DIR) -type f)))
	@echo ""; \
	echo "Installing API files"; \
	if [ "$(CO_ROOT)" == "" -o ! -d $(CO_ROOT) ]; then \
		echo "Checkout root not found: $(CO_ROOT)"; \
		exit 1; \
	fi; \
	for file in $(FILE_LIST); do \
		echo "$(subst $(MAKEFILE_DIR)/,,$(API_DIR))/$$file -> $(CO_ROOT)/$$file"; \
		install -D -m 0644 $(API_DIR)/$$file $(CO_ROOT)/$$file; \
	done; \
	if [ "$(WM_ROOT)" != "" -a -d "$(WM_ROOT)" ]; then \
		echo ""; \
		echo "Installing WM files: $(WM_ROOT)"; \
		for file in $(WM_FILE_LIST); do \
			echo "$(subst $(MAKEFILE_DIR)/,,$(WM_DIR))/$$file -> $(WM_ROOT)/$$file"; \
			install -D -m 0644 $(WM_DIR)/$$file $(WM_ROOT)/$$file; \
		done; \
	fi

update:
	@echo "Adding RTL version $(RTL_VERSION) from $(RDL_SRC_DIR)"
	@if [ "$(RDL_SRC_DIR)" == "" -o ! -d "$(RDL_SRC_DIR)" ]; then \
		echo "RDL_SRC_DIR is not set correctly: $(RDL_SRC_DIR)"; \
		exit 1; \
	fi;
	mkdir -p $(RDL_DIR) && \
	cp -rv $(RDL_SRC_DIR)/*.rdl $(RDL_DIR)

list:
	@ls -1 $(RDL_BASE_DIR)

all: clean gen install
