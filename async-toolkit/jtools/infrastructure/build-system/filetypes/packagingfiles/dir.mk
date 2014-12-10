CURR_TARGET_PACKAGE_FILES := $(wildcard $(CURR_PROJECT_DIR)/*.package )



ifneq ("$(CURR_TARGET_PACKAGE_FILES)","")

PACKAGEFILES_PACKAGE_NAMES := $(patsubst $(CURR_PROJECT_DIR)/%.package, \
                                         %,                             \
                                         $(CURR_TARGET_PACKAGE_FILES) )

PACKAGEFILES_DEPS := $(foreach packageName,                   \
                               $(PACKAGEFILES_PACKAGE_NAMES), \
                               $(CURR_TARGET_DIR)/$(packageName).package.d)

PACKAGEFILES_TARGETS := $(foreach packageName,                   \
                                  $(PACKAGEFILES_PACKAGE_NAMES), \
                                  $(CURR_TARGET_DIR)/$(packageName).tar.gz)

PACKAGEFILES_PHONIES := $(foreach packageName,                   \
                                  $(PACKAGEFILES_PACKAGE_NAMES), \
                                  $(CURR_TARGET_DIR)/$(packageName)-package)

PACKAGEFILES_INSTALLS := $(foreach packageName,                   \
                                  $(PACKAGEFILES_PACKAGE_NAMES), \
                                  $(CURR_TARGET_DIR)/$(packageName)-install)

CURR_TARGET_DEPS := $(PACKAGEFILES_DEPS) $(CURR_TARGET_DEPS)

# note, dependency check now does not check return code (see bug 7929)
$(CURR_TARGET_DIR)/%.java $(CURR_TARGET_DIR)/%.package.d: $(CURR_PROJECT_DIR)/%.package $(package_files_mkpackage)
	-$(package_files_package_dep) $< $(@D) $(*F) $(<D) >$(@D)/$(*F).package.d

.PHONEY: $(PACKAGEFILES_PHONIES) $(PACKAGEFILES_INSTALLS)


allpackages: $(PACKAGEFILES_PHONIES)


CURR_RESULT_FILES := $(CURR_RESULT_FILES) $(PACKAGEFILES_TARGETS)

$(CURR_TARGET_DIR)/%.tar.gz: $(CURR_PROJECT_DIR)/%.package $(package_files_mkpackage)
	$(package_files_mkpackage) $(MAKELINKS) --root-project-dir '$(ROOT_PROJECT_DIR)' --build-system-root '$(BUILD_SYSTEM_ROOT)' $< $(@D) $(*F) $(<D) 


installallpackages: $(PACKAGEFILES_INSTALLS)

endif


.PHONEY: $(CURR_TARGET_DIR)/installallpackages
$(CURR_TARGET_DIR)/installallpackages: installallpackages
