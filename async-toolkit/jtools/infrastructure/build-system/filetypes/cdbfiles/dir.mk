#to make layout the following must hold

#1) there is a layout.cdb
#2) there is a corresponding file in the spec tree

ifeq ($(MAKE_LAYOUT),1)

#stack
CDBFILES_VARS_CHANGED_STACK :=$(CDBFILES_VARS_CHANGED) $(CDBFILES_VARS_CHANGED_STACK)
CDBFILES_VARS_CHANGED :=0
CDB_CUSTOM_MK := $(wildcard $(CURR_PROJECT_DIR)/cdb-custom.mk)

ifneq ("$(strip $(CDB_CUSTOM_MK))","")
LVS_FLAGS_TEMP := $(LVS_FLAGS)
DRC_FLAGS_TEMP := $(DRC_FLAGS)
-include $(CURR_PROJECT_DIR)/cdb-custom.mk 

ifneq ("$(strip $(LVS_FLAGS_TEMP))","$(strip $(LVS_FLAGS))")
CDBFILES_VARS_CHANGED :=1
else
ifneq ("$(strip $(DRC_FLAGS_TEMP))","$(strip $(DRC_FLAGS))")
CDBFILES_VARS_CHANGED :=1
endif
endif

ifeq ($(CDBFILES_VARS_CHANGED),1)
PUSH_SCOPED_VAR_VAR_NAME := LVS_FLAGS
include $(BUILD)/include-functions/pushscopedvar.mk
PUSH_SCOPED_VAR_VAR_NAME := DRC_FLAGS
include $(BUILD)/include-functions/pushscopedvar.mk
endif

endif #cdb_custom
#END stack

CURR_TARGET_CDB_FILES := $(wildcard $(CURR_PROJECT_DIR)/layout.cdb)

ifneq ("$(strip $(CURR_TARGET_CDB_FILES))","")

CURR_CELL := $(call GET_CELL_NAME_FOR_VIEW_DIR, $(CURR_TARGET_DIR) )

CAST_CELL := $(shell echo $(CURR_CELL) | $(VIRTUOSO_INTEGRATION_ARCH_BIN)/rename --type=cell --from=cadence --to=cast )

CAST_FILES := $(foreach dir,$(call STRIP_FIRST_WORD,$(subst :, ,$(CAST_PATH))),$(dir)/$(subst .,/,$(CAST_CELL)).cast )

CURR_IN_SPEC := $(shell for castfile in "$(CAST_FILES)" ; do if [ -e $$castfile ] ; then echo $$castfile; fi ; done )

ifneq ("$(strip $(CURR_IN_SPEC))","")

CURR_VIEW := $(call GET_VIEW_NAME_FOR_VIEW_DIR, $(CURR_TARGET_DIR) )
CURR_CELL_DIR := $(call GET_PARENT_DIR, $(CURR_TARGET_DIR) )

### LAYOUT DEPENDENCIES
ifneq ("$(filter $(CURR_VIEW),layout floorplan)", "")
CURR_TARGET_DEPS  := $(patsubst $(CURR_PROJECT_DIR)/%.cdb, $(CURR_TARGET_DIR)/$(CURR_CELL).gds2.d, $(CURR_TARGET_CDB_FILES) ) $(CURR_TARGET_DEPS)
$(CURR_TARGET_DIR)/$(CURR_CELL).gds2.d: $(CURR_PROJECT_DIR)/layout.cdb
	$(EXEC_LINUX) $(EXEC_CADENCE) eval $(BUILD)/filetypes/cdbfiles/cdbdep.sh $(VIRTUOSO_INTEGRATION_PACKAGE_ROOT) $(FULCRUM_PDK_PACKAGE_ROOT) $(DFII_DIR) $< '"$@ $(basename $@)"' $@
endif

### LAYOUT DERIVED ###
ifeq ($(CURR_VIEW),layout)

### gds2 ###
#CURR_RESULT_FILES := $(patsubst $(CURR_PROJECT_DIR)/%.cdb, $(CURR_TARGET_DIR)/$(CURR_CELL).gds2, $(CURR_TARGET_CDB_FILES) ) $(CURR_RESULT_FILES)
$(CURR_TARGET_DIR)/gds2: $(CURR_TARGET_DIR)/$(CURR_CELL).gds2
$(CURR_TARGET_DIR)/$(CURR_CELL).gds2 $(CURR_TARGET_DIR)/$(CURR_CELL).bindrul:
	-$(EXEC_LINUX) $(EXEC_CADENCE) $(VIRTUOSO_INTEGRATION_ARCH_BIN)/gdsIIWrite --cell="$(notdir $(basename $@) )" --fulcrum-pdk-root=$(FULCRUM_PDK_PACKAGE_ROOT) --working-dir=$(@D)  --cast-path=$(CAST_PATH) --view=$(call GET_VIEW_NAME_FOR_VIEW_DIR, $(@D) ) --dfII-dir=$(DFII_DIR) --output=$(basename $@).gds2 --bind-rul=$(basename $@).bindrul
.PRECIOUS: $(CURR_TARGET_DIR)/$(CURR_CELL).gds2 $(CURR_TARGET_DIR)/$(CURR_CELL).bindrul

### SPICE ###
$(CURR_TARGET_DIR)/spice: $(CURR_TARGET_DIR)/$(CURR_CELL).spice

$(CURR_TARGET_DIR)/$(CURR_CELL).spice: $(CURR_TARGET_DIR)/$(CURR_CELL).spice_gds2
	$(EXEC_LINUX) $(VIRTUOSO_INTEGRATION_ARCH_BIN)/cdl_renamer --source-cdl-file=$< --name-in=gds2 --name-out=cast --translated-cdl=$@ --translated-nmap=/dev/null

#dont make spice if gds2 is too big...it would take forever
$(CURR_TARGET_DIR)/$(CURR_CELL).spice_gds2: $(CURR_CELL_DIR)/$(CURR_CELL).cdl_gds2 $(CURR_TARGET_DIR)/$(CURR_CELL).gds2
	-if [[ ( -e $< ) && ( "$$((`du $< | awk '{print $$1}'` <= 1000 ))"=="1" ) ]] ; then $(EXEC_LINUX) $(EXEC_CADENCE) $(VIRTUOSO_INTEGRATION_ARCH_BIN)/extract --cell=$(notdir $(basename $@) ) --fulcrum-pdk-root=$(FULCRUM_PDK_PACKAGE_ROOT) --working-dir=$(@D) --root-cell-name=$(shell echo "$(notdir $(basename $@) )" | $(VIRTUOSO_INTEGRATION_ARCH_BIN)/rename --from=cadence --to=gds2 --type=cell ) --cdl-file=$(word 1, $^ ) --gdsII-file=$(word 2, $^ ) --output=$@; fi

.PRECIOUS: $(CURR_TARGET_DIR)/$(CURR_CELL).spice_gds2
.PRECIOUS: $(CURR_TARGET_DIR)/$(CURR_CELL).spice

### ASSURA ###OB

ifneq ("$(strip $(LVS_FLAGS))","")
LVS_FLAGS-$(CURR_CELL) := $(LVS_FLAGS)
endif

$(CURR_TARGET_DIR)/$(CURR_CELL).lvs.tar.bz2: $(CURR_CELL_DIR)/$(CURR_CELL).cdl $(CURR_TARGET_DIR)/$(CURR_CELL).gds2 $(CURR_TARGET_DIR)/$(CURR_CELL).bindrul
	mkdir -p /mnt/fulcrum/scratch3a/cadadmin
	-lvs_dir=`mktemp -d /mnt/fulcrum/scratch3a/cadadmin/lvs.XXXXXX`; \
	$(EXEC_SUN) $(EXEC_CADENCE) perl -I$(VIRTUOSO_INTEGRATION_PACKAGE_ROOT)/share/script/perl/ve/front-end -I$(VIRTUOSO_INTEGRATION_PACKAGE_ROOT)/share/script/perl/ve/back-end $(VIRTUOSO_INTEGRATION_PACKAGE_ROOT)/share/script/perl/ve/front-end/vfe VerificationType=AssuraLvs RunName=lvs  LayoutCell=$(shell echo "$(notdir $(basename $(basename $(basename $@) ) ) )" | $(VIRTUOSO_INTEGRATION_ARCH_BIN)/rename --from=cadence --to=gds2 --type=cell ) RuleFile=$(FULCRUM_PDK_PACKAGE_ROOT)/share/Fulcrum/assura/extract.rul CompareFile=$(FULCRUM_PDK_PACKAGE_ROOT)/share/Fulcrum/assura/compare.rul AssuraRsfInclude=$(FULCRUM_PDK_PACKAGE_ROOT)/share/Fulcrum/assura/LVSinclude.rsf WorkingDir=$$lvs_dir  SchematicFile=$(word 1, $^) GDS2File=$(word 2, $^ ) $(addprefix AssuraSet=, $(LVS_FLAGS-$(notdir $(basename $(basename $(basename $@)))))) BindingFile=$(word 3, $^) &>/dev/null ; \
	if [ -e "$$lvs_dir/lvs.cls" ] ; then tar -cjf $@ -C $$lvs_dir . ; fi; rm -rf $$lvs_dir

.PRECIOUS: $(CURR_TARGET_DIR)/$(CURR_CELL).lvs.tar.bz2

ifneq ("$(strip $(DRC_FLAGS))","")
DRC_FLAGS-$(CURR_CELL) := $(DRC_FLAGS)
endif

$(CURR_TARGET_DIR)/$(CURR_CELL).drc.tar.bz2: $(CURR_TARGET_DIR)/$(CURR_CELL).gds2
	mkdir -p /mnt/fulcrum/scratch3a/cadadmin
	-drc_dir=`mktemp -d /mnt/fulcrum/scratch3a/cadadmin/drc.XXXXXX`; \
	$(EXEC_SUN) $(EXEC_CADENCE) perl -I$(VIRTUOSO_INTEGRATION_PACKAGE_ROOT)/share/script/perl/ve/front-end -I$(VIRTUOSO_INTEGRATION_PACKAGE_ROOT)/share/script/perl/ve/back-end $(VIRTUOSO_INTEGRATION_PACKAGE_ROOT)/share/script/perl/ve/front-end/vfe VerificationType=AssuraDrc RunName=drc  WorkingDir=$$drc_dir GDS2File=$(word 1, $^ ) LayoutCell=$(shell echo "$(notdir $(basename $(basename $(basename $@) ) ) )" | $(VIRTUOSO_INTEGRATION_ARCH_BIN)/rename --from=cadence --to=gds2 --type=cell )  RuleFile=$(FULCRUM_PDK_PACKAGE_ROOT)/share/Fulcrum/assura/drc.rul &> /dev/null; \
	if [ -e "$$drc_dir/drc.err" ] ; then tar -cjf $@ -C $$drc_dir . ; fi ; rm -rf $$drc_dir

.PRECIOUS: $(CURR_TARGET_DIR)/$(CURR_CELL).drc.tar.bz2


$(CURR_TARGET_DIR)/%/run.measure: $(CURR_TARGET_DIR)/%/aspice.inc $(CURR_TARGET_DIR)/$(CURR_CELL).aspice
	node=$(call GET_NTH_FROM_LAST_DIR,$@,2); \
	cell=$(call GET_CELL_NAME_FOR_VIEW_DIR, ) \
	$(EXEC_LINUX) aspice run -path $(ASPICE_PATH) -up $$node -dn $$node --internal-wires 1> run.out 2> run.err 

ASPICE_TIME := 20e-9
$(CURR_TARGET_DIR)/%/env.asp:
	env=$(call GET_NTH_FROM_LAST_DIR,$@,2); \
	cell=$(call CADENCE_REVERSE_ESCAPE,$(call GET_NTH_FROM_LAST_DIR,$@,4)); \
	mkdir -p $(@D); \
	$(EXEC_LINUX) $(VIRTUOSO_INTEGRATION_ARCH_BIN)/jflat --cell=$$cell:$$env --cast-path=$(CAST_PATH) --tool=aspice > $@


$(CURR_TARGET_DIR)/%/run.asp: $(CURR_TARGET_DIR)/$(CURR_CELL).aspice
	env=$*
	true=$(call GET_NTH_FROM_LAST_DIR,$(@D),3); \
	temp=$(call GET_NTH_FROM_LAST_DIR,$(@D),4); \
	corner=$(call GET_NTH_FROM_LAST_DIR,$(@D),5); \
	node=$(call GET_NTH_FROM_LAST_DIR,$(@D),6); \
	echo ".temperature=$$temp" > $@; \
	echo ".true=$$true" >> $@; \
	echo ".corner=$$corner" >> $@; \
	echo ".timemax=$(ASPICE_TIME)" >> $@; \
	echo ".include $(word 1, $^)" >> $@; \
	echo ".include $(word 2, $^)" >> $@; \
	echo ".include $(ASPICE_INCLUDE)" >> $@;

.PRECIOUS: $(CURR_TARGET_DIR)/$(CURR_CELL).charge.tar.bz2

alllvs_result : $(CURR_TARGET_DIR)/lvs_result
alldrc_result : $(CURR_TARGET_DIR)/drc_result
alllvs : $(CURR_TARGET_DIR)/lvs
alldrc : $(CURR_TARGET_DIR)/drc

%/lvs_result: %/lvs.tar.bz2
	$(BUILD)/filetypes/cdbfiles/mysql_status lvs $< $(notdir $(basename $(basename $(basename $<) ) ) ) $(LAYOUT_DB) $(MYSQL_HOST) > $@
	cat $@

%/lvs: %/lvs.tar.bz2
	$(BUILD)/filetypes/cdbfiles/mysql_status lvs $< $(notdir $(basename $(basename $(basename $<) ) ) ) $(LAYOUT_DB) $(MYSQL_HOST)

%/drc_result: %/drc.tar.bz2
	$(BUILD)/filetypes/cdbfiles/mysql_status drc $< $(notdir $(basename $(basename $(basename $<) ) ) ) $(LAYOUT_DB) $(MYSQL_HOST) > $@
	cat $@

%/drc: %/drc.tar.bz2
	$(BUILD)/filetypes/cdbfiles/mysql_status drc $< $(notdir $(basename $(basename $(basename $<) ) ) ) $(LAYOUT_DB) $(MYSQL_HOST)

endif #layout

### FLOORPLAN DERIVED ###
ifeq ($(CURR_VIEW),floorplan)

endif #floorplan

allcharge_result : $(CURR_TARGET_DIR)/charge_result
alltransient_result : $(CURR_TARGET_DIR)/transient_result

### RESULT DATABASE ###

$(CURR_TARGET_DIR)/charge_result: $(CURR_TARGET_DIR)/$(CURR_CELL).charge.tar.bz2
	$(BUILD)/filetypes/cdbfiles/mysql_status charge $< $(notdir $(basename $(basename $(basename $<) ) ) ) $(LAYOUT_DB) $(MYSQL_HOST) > $@
	cat $@

$(CURR_TARGET_DIR)/charge: $(CURR_TARGET_DIR)/$(CURR_CELL).charge.tar.bz2
	$(BUILD)/filetypes/cdbfiles/mysql_status charge $< $(notdir $(basename $(basename $(basename $<) ) ) ) $(LAYOUT_DB) $(MYSQL_HOST)



#######################


endif #CURR_IN_SPEC

endif #CURR_TARGET_CDB_FILES

endif #MAKE_LAYOUT
