################################################################################
# NOTES
################################################################################
# 	- Makefiles are particular about having no white space after the end of a line
# 	- Makefiles are particular about having true tabs (not expanded into spaces) 

################################################################################
# Directories
################################################################################
PHYSROOT     := $(shell pwd)
PHYSWARD     := $(PHYSROOT)/ward
LOGICROOT    ?= $(shell pwd | sed 's/\/sandbox\/phys//')
VALROOT      := $(LOGICROOT)/sandbox/val
RTLLISTDIR   := $(VALROOT)/bin
export PHYSROOT LOGICROOT VALROOT

################################################################################
# Shared variables
################################################################################
SYN_MODULE := tmpl_dut

################################################################################
# Help targets
################################################################################
.DEFAULT_GOAL := help
.PHONY : help
help:
	@# Prints this help documentation
	@echo Targets
	@echo -------
	@grep -hB1 "@#" $(MAKEFILE_LIST) | grep -ve -- | sed "s/^\(\S\+\):.*/\1:/" | sed "s/@# //"
	@echo
	@echo Variables
	@echo ---------
	@grep -h "#@" $(MAKEFILE_LIST) | grep -v HIDE | sed "s/\(\S\+\)\s*[?:]\?=.*#@\S*/\1/"

VARS ?= $(sort $(.VARIABLES))
.PHONY : vars
vars:
	@# Prints list of make variables and their values
	@# List of variables set in VARS, which by default is all make variables
	@$(foreach v,$(filter-out .VARIABLES VARS,$(VARS)),echo "$v = $($v)";)

DIRS ?= LOGICROOT PHYSROOT PHYSWARD VALROOT 
.PHONY : dirs
dirs:
	@# Prints list of interesting dirs and their values
	@$(foreach d,$(DIRS),echo "$d = $($d)";)


################################################################################
# Synthesis targets
################################################################################
syn: $(RTLLISTDIR)/rtl_list.tcl $(PHYSROOT)/cfg/unit_syn.clocks.tbl $(PHYSROOT)/cfg/unit_syn.io.tbl
	mkdir -p $(PHYSWARD)
	cd $(PHYSWARD) && /p/sl2/utils/bin/fhdk16_unit_syn apr $(SYN_MODULE) $(RTLLISTDIR) -cfg APRA0P10 -clk $(PHYSROOT)/cfg/unit_syn.clocks.tbl -io $(PHYSROOT)/cfg/unit_syn.io.tbl &	

physgui: $(PHYSWARD)/soc_apr-be-sl2-a0/$(SYN_MODULE)-be/syn/logs/dc.log
	cd $(PHYSWARD)/soc_apr-be-sl2-a0/$(SYN_MODULE)-be && /p/sl2/utils/bin/fhdk16setup -c APRA0P10 --run_cmd $(PHYSROOT)/scripts/load_syn_final_start_gui.pl


################################################################################
# Clean targets
################################################################################
.PHONY : clean
clean:
	@# Removes $(PHYSWARD)
	rm -rf $(PHYSWARD)

