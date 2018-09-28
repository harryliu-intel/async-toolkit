RM = /bin/rm
MKDIR := /bin/mkdir
CP := /bin/cp

################################################################
# ENV variables
################################################################
export SNPSLMD_QUEUE := 1
#export COLLAGE_WORK := $(MODEL_ROOT)/collage_work
################################################################
# Defaults
################################################################
COPY_TO_DEST := 0
SOC := ""

################################################################
# Collage setup
################################################################
# tool variables/settings
CORE_BIN := $(CORE_TOOLS_DIR)/$(CORE_TOOLS_VER)/bin
COL_INIT := $(COLLAGE_ROOT)/core/common/tcl/collage_init.tcl
ifeq ($(COLLAGE_BATCH_MODE),1) 
  SHELLFLG := -shell
else 
  SHELLFLG :=
endif
CA = $(CORE_BIN)/coreAssembler  -timeout_count 10 $(SHELLFLG) -x "source $(COL_INIT)"
CB = $(CORE_BIN)/coreBuilder    -timeout_count 10 $(SHELLFLG) -x "source $(COL_INIT)"
CC = $(CORE_BIN)/coreConsultant -timeout_count 10 $(SHELLFLG) -x "source $(COL_INIT)"


################################################################
# directory paths
################################################################

WORK    =       	$(MODEL_ROOT)/tools
COLLAGE = 		$(MODEL_ROOT)/tools/collage
COLLAGE_IP_KITS = 	$(COLLAGE_WORK)/ip_kits
BUILDER_FILES = 	$(wildcard $(COLLAGE_WORK)/builder.*)

################################################################
# project working vars - conditionally set if not already in environment
################################################################

COLLAGE_PROJECT_BOOTSTRAP ?= $(COLLAGE)/assemble/soc_assemble.tcl
SBR_ASSEMBLER = $(CHASSIS_ROOT)/configs/common/gen_ip/iosf_sb_fabric_main/assemble/assembler.sbr_network.tcl
CMN_ASSEMBLER = $(CHASSIS_ROOT)/configs/common/soc/assemble/assembler.soc.tcl
SOC_ASSEMBLER = $(COLLAGE)/assemble/assembler.soc.tcl
TESTBENCH_GEN = $(COLLAGE)/assemble/open_chassis_ws_gen_tb.tcl
SUBSYS_SPECS_DIR = $(MODEL_ROOT)/tools/collage/subsystems
################################################################
# MAINLINE calls
################################################################
.PHONY: clean
.PHONY: assemble assemble_tb 

all: assemble assemble_tb

################################################################
# Create collage_work if not exist
################################################################
$(COLLAGE_WORK):
	/bin/mkdir -p $@
################################################################
# Collage IP kits generation
################################################################
build:  $(COLLAGE_WORK)
	rm -rf $(COLLAGE_WORK)
	$(foreach build_file, $(BUILDER_FILES), $(CB) -f $(build_file) || exit;) 
	$(CP) -r $(COLLAGE_WORK)/ip_kits $(COLLAGE)/ip_kits
	$(CP) -r $(COLLAGE_WORK)/log $(COLLAGE)/log
	$(CP) -r $(COLLAGE_WORK)/reports $(COLLAGE)/reports

build_fxp:  $(COLLAGE_WORK)
	rm -rf $(COLLAGE_WORK)
	mkdir $(COLLAGE_WORK)
	test -d $(COLLAGE)/ip_kits || mkdir $(COLLAGE)/ip_kits
	test -d $(COLLAGE)/reports || mkdir $(COLLAGE)/reports
	$(CB) -f $(MODEL_ROOT)/tools/collage/build/builder.fxp.tcl
	$(CP) -r $(COLLAGE_WORK)/ip_kits/fxp.coreKit $(COLLAGE)/ip_kits/fxp.coreKit

build_emp:  $(COLLAGE_WORK)
	rm -rf $(COLLAGE_WORK)
	mkdir $(COLLAGE_WORK)
	test -d $(COLLAGE)/ip_kits || mkdir $(COLLAGE)/ip_kits
	test -d $(COLLAGE)/reports || mkdir $(COLLAGE)/reports
	$(CB) -f $(MODEL_ROOT)/tools/collage/build/builder.emp.tcl
	$(CP) -r $(COLLAGE_WORK)/ip_kits/emp_top.coreKit $(COLLAGE)/ip_kits/emp_top.coreKit

build_cgu:  $(COLLAGE_WORK)
	rm -rf $(COLLAGE_WORK) $(CGU_ROOT)/tools/collage/ip_kits $(CGU_ROOT)/tools/collage/reports
	mkdir $(COLLAGE_WORK)
	test -d $(CGU_ROOT)/tools/collage/ip_kits || mkdir $(CGU_ROOT)/tools/collage/ip_kits
	test -d $(CGU_ROOT)/tools/collage/reports || mkdir $(CGU_ROOT)/tools/collage/reports
	$(CB) -f $(CGU_ROOT)/tools/collage/build/builder.cgu.tcl
	$(CP) -f $(COLLAGE_WORK)/ip_kits/cgu.coreKit $(CGU_ROOT)/tools/collage/ip_kits/cgu.coreKit
	$(CP) -f $(WORKAREA)/tools/collage/reports/cgu.* $(CGU_ROOT)/tools/collage/reports

################################################################
# Collage RTL assembly
################################################################
assemble: $(COLLAGE_WORK)
	cd $(COLLAGE_WORK); $(CA) -f $(WORK)/collage/assemble/assembler.soc.tcl

################################################################
# Collage TB assembly
################################################################
assemble_tb: $(COLLAGE_WORK)
	cd $(COLLAGE_WORK);  $(CA) -f $(WORK)/collage/assemble/open_chassis_ws_gen_tb.tcl

################################################################
# Collage Open workspace
################################################################
open_soc: $(COLLAGE_WORK)
	cd $(COLLAGE_WORK);  $(CA) -f $(WORK)/collage/assemble/open_chassis_ws.tcl

################################################################
# remove generated files
################################################################
clean:
	rm -rf log 
	rm -rf reports 
	rm -rf gen
	rm -f subsystems.txt
	rm -f rt_shell_command.log
	rm -f soc.undriven_net_conn.txt
	rm -rf $(COLLAGE_WORK)/*

clean_rtl: $(COLLAGE_WORK)
	-/bin/rm -rf $(COLLAGE_WORK)/units
	-/bin/rm -rf $(COLLAGE_WORK)/source
	-/bin/rm -rf $(COLLAGE_WORK)/soc
	-/bin/rm -rf $(COLLAGE_WORK)/installed_kits
	-/bin/rm -rf $(COLLAGE_WORK)/rt_shell_command.log
	-/bin/rm -rf $(COLLAGE_WORK)/dir
clean_log_rep: $(COLLAGE_WORK)
	-/bin/rm -rf $(COLLAGE_WORK)/log
	-/bin/rm -rf $(COLLAGE_WORK)/reports

clean_tb: $(COLLAGE_WORK)
	-/bin/rm -rf $(COLLAGE_WORK)/verif
	-/bin/rm -rf $(COLLAGE_WORK)/soc_tb
	-/bin/rm -rf $(COLLAGE_WORK)/tb_globs*


