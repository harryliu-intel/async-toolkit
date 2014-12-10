# Copyright 2003 Fulcrum Microsystems.  All rights reserved.
# $Id$
# $DateTime$
# $Author$
package_files_mkpackage := $(BUILD_SYSTEM_ROOT)/filetypes/packagingfiles/mkpackage.pl

package_files_package_dep := $(package_files_mkpackage) -d

packageInst := $(BUILD_SYSTEM_ROOT)/filetypes/packagingfiles/package-inst.sh

.PHONEY: installallpackages allpackages

ifneq ("$(FULCRUM_RESULTS_DIR)","")
%-package: %.tar.gz
	mkdir -p $(FULCRUM_RESULTS_DIR)
	cp $< $(FULCRUM_RESULTS_DIR)/$(basename $(basename $(<F)))-$(FULCRUM_BUILD_ID).tar.gz
else
%-package: %.tar.gz
	@echo -n ""
endif


%-install: %.tar.gz
	mkdir -p $(ROOT_TARGET_DIR)/install
	$(packageInst) $< $(ROOT_TARGET_DIR)/install

FULCRUM_COMMON_BIN=/home/local/common/fulcrum/bin
UPDATEFMDB=$(FULCRUM_COMMON_BIN)/updatefmdb
BUILDFULCRUM=$(FULCRUM_COMMON_BIN)/buildfulcrum
ifeq ("$(BUILD_CHANGE_NUMBER)","")
BUILD_CHANGE_NUMBER=$(shell p4 changes -m 1 | awk '{print $$2}')
endif

%-finstall:
	echo $* | sed -e 's:.*/::'
	$(BUILDFULCRUM) -v --overwrite --toolhome $(ROOT_TARGET_DIR)/install --root-target-dir $(ROOT_TARGET_DIR) --root-project-dir $(ROOT_PROJECT_DIR) --keep --nop4sync --packages `echo $* | sed -e 's:.*/::'`
#	mkdir -p $(ROOT_TARGET_DIR)/install/tools
#	BULID_CHANGE_NUMBER=$(BUILD_CHANGE_NUMBER) $(packageInst) $< $(ROOT_TARGET_DIR)/install/tools
#	touch $(ROOT_TARGET_DIR)/install/tools/`echo '$*' | sed -e 's:.*/::'`/$(BUILD_CHANGE_NUMBER)/.installed
#	$(UPDATEFMDB) --toolhome $(ROOT_TARGET_DIR)/install
