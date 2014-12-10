# SLURPABLE is used to prevent a make failure if the file does not exist

SLURPABLE := $(shell if [ -s '$(CURR_SLURP_DIR)/cell.gds2' ] ; then echo 1; fi)
ifeq ($(SLURPABLE),1)
$(CURR_CELL_DIR)/cell.gds2: $(CURR_SLURP_DIR)/cell.gds2
	#TASK=gds2_slurp CELL=$(call GET_CAST_FULL_NAME,$(@D)) VIEW=$(call GET_NTH_FROM_LAST_DIR,$(@D),1)
	ln -sf '$<' '$@'
endif

SLURPABLE := $(shell if [ -s '$(CURR_SLURP_DIR)/extracted/cell.aspice' ] ; then echo 1; fi)
ifeq ($(SLURPABLE),1)
$(CURR_CELL_DIR)/extracted/cell.spice_topcell: $(CURR_SLURP_DIR)/extracted/cell.spice_topcell
	#TASK=topcell_slurp MODE=extracted CELL=$(call GET_CAST_FULL_NAME,$(@D)) VIEW=$(call GET_NTH_FROM_LAST_DIR,$(@D),1)
	ln -sf '$<' '$@'

endif

SLURPABLE := $(shell if [ -s '$(CURR_SLURP_DIR)/extracted/cell.spice_gds2' ] ; then echo 1; fi)
ifeq ($(SLURPABLE),1)
$(CURR_CELL_DIR)/extracted/cell.spice_gds2: $(CURR_SLURP_DIR)/extracted/cell.spice_gds2
	#TASK=spice_gds2_slurp MODE=extracted CELL=$(call GET_CAST_FULL_NAME,$(@D)) VIEW=$(call GET_NTH_FROM_LAST_DIR,$(@D),1)
	ln -sf '$<' '$@'

endif

SLURPABLE := $(shell if [ -s '$(CURR_SLURP_DIR)/extracted/cell.spice' ] ; then echo 1; fi)
ifeq ($(SLURPABLE),1)
$(CURR_CELL_DIR)/extracted/cell.spice: $(CURR_SLURP_DIR)/extracted/cell.spice
	#TASK=spice_slurp MODE=extracted CELL=$(call GET_CAST_FULL_NAME,$(@D)) VIEW=$(call GET_NTH_FROM_LAST_DIR,$(@D),1)
	ln -sf '$<' '$@'

endif

SLURPABLE := $(shell if [ -s '$(CURR_SLURP_DIR)/extracted/cell.aspice' ] ; then echo 1; fi)
ifeq ($(SLURPABLE),1)
$(CURR_CELL_DIR)/extracted/cell.aspice: $(CURR_SLURP_DIR)/extracted/cell.aspice
	#TASK=aspice_slurp MODE=extracted CELL=$(call GET_CAST_FULL_NAME,$(@D)) VIEW=$(call GET_NTH_FROM_LAST_DIR,$(@D),1)
	ln -sf '$<' '$@'

endif

SLURPABLE := $(shell if [ -s '$(CURR_SLURP_DIR)/extracted/extract.err' ] ; then echo 1; fi)
ifeq ($(SLURPABLE),1)
$(CURR_CELL_DIR)/extracted/extract.err: $(CURR_SLURP_DIR)/extracted/extract.err
	#TASK=extracterr_slurp MODE=extracted CELL=$(call GET_CAST_FULL_NAME,$(@D)) VIEW=$(call GET_NTH_FROM_LAST_DIR,$(@D),1)
	ln -sf '$<' '$@'

endif

SLURPABLE := $(shell if [ -s '$(CURR_SLURP_DIR)/extracted/extract.result' ] ; then echo 1; fi)
ifeq ($(SLURPABLE),1)
$(CURR_CELL_DIR)/extracted/extract.result: $(CURR_SLURP_DIR)/extracted/extract.result
	#TASK=extractresult_slurp MODE=extracted CELL=$(call GET_CAST_FULL_NAME,$(@D)) VIEW=$(call GET_NTH_FROM_LAST_DIR,$(@D),1)
	ln -sf '$<' '$@'

endif

SLURPABLE := $(shell if [ -s '$(CURR_SLURP_DIR)/extracted/cell.spice_include' ] ; then echo 1; fi)
ifeq ($(SLURPABLE),1)
$(CURR_CELL_DIR)/extracted/cell.spice_include: $(CURR_SLURP_DIR)/extracted/cell.spice_include
	#TASK=includeslurp MODE=extracted CELL=$(call GET_CAST_FULL_NAME,$(@D)) VIEW=$(call GET_NTH_FROM_LAST_DIR,$(@D),1)
	ln -sf '$<' '$@'

endif

SLURPABLE := $(shell if [ -s '$(CURR_SLURP_DIR)/../cell.cdl' ] ; then echo 1; fi)
ifeq ($(SLURPABLE),1)
$(CURR_CELL_DIR)/../cell.cdl: $(CURR_SLURP_DIR)/../cell.cdl
	#TASK=cellcdl_slurp MODE=extracted CELL=$(call GET_CAST_FULL_NAME,$(@D)) VIEW=$(call GET_NTH_FROM_LAST_DIR,$(@D),1)
	ln -sf '$<' '$@'

endif

SLURPABLE := $(shell if [ -s '$(CURR_SLURP_DIR)/../../cell.cdl' ] ; then echo 1; fi)
ifeq ($(SLURPABLE),1)
$(CURR_CELL_DIR)/../../cell.cdl: $(CURR_SLURP_DIR)/../../cell.cdl
	#TASK=cellcdl_slurp2 MODE=extracted CELL=$(call GET_CAST_FULL_NAME,$(@D)) VIEW=$(call GET_NTH_FROM_LAST_DIR,$(@D),1)
	ln -sf '$<' '$@'

endif
