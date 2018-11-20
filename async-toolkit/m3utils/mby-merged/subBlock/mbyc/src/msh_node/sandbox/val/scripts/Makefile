################################################################################
# NOTES
################################################################################
#   - try this:  make help
# 	- Makefiles are particular about having no white space after the end of a line
# 	- Makefiles are particular about having true tabs (not expanded into spaces) 

################################################################################
# Directories
################################################################################
TBROOT       := $(shell pwd)
LOGICROOT    ?= $(shell pwd | sed 's/\/msh_node\/sandbox\/val//')
#LOGICROOT    ?= $(shell pwd | sed 's/\/sandbox\/val//')
#SL2_LIB      ?= $(shell ToolConfig.pl get_tool_var ipconfig/sl2_library INC_PATH)
#SL2_LIB_IP16 ?= $(shell ToolConfig.pl get_tool_var ipconfig/sl2lib_ip16 INC_PATH)
#SVA_LIB      ?= $(shell ToolConfig.pl get_tool_var sva_lib INC_PATH)
SL2_LIB      ?= /p/hdk/rtl/ip_models/shdk74/sl2_library/sl2_library-srvr10nm-apr_a0-latest
SL2_LIB_IP16 ?= /p/hdk/rtl/ip_models/shdk74/sl2lib_ip16/sl2lib_ip16-srvr10nm-latest
SVA_LIB      ?= /p/hdk/rtl/cad/x86-64_linux26/dt/sva_lib/8.1/SVA_LIB
VCS_HOME     ?= /p/hdk/rtl/cad/x86-64_linux26/synopsys/vcsmx/L-2016.06-SP2-7
VCS_TARGET_ARCH ?= suse64
ASSERT_LIB   ?= /p/hdk/rtl/cad/x86-64_linux30/dt/sva_lib/15.4p5_shOpt64
export LOGICROOT TBROOT SL2_LIB SL2_LIB_IP16 SVA_LIB VCS_HOME VCS_TARGET_ARCH ASSERT_LIB

################################################################################
# Other project-related options
################################################################################
BUILD_OPTS += +lint=all,noMSIVD,noVCDE,noSV-PIU,noVNGS,noUI,noONGS,noNS,noSVA-NSVU,noSVA-NSEF,noSVA-CE,noSVA-DIU,noSVA-DAU
BUILD_OPTS += +warn=noUII-L # Get rid of Warning about unused interfaces in mem pkg

################################################################################
# DVE
################################################################################
DVE_OPTS += $(DVE_USER)
.PHONY : dve
dve:
	@# Invokes DVE
	@# DVE flags set in DVE_OPTS/DVE_USER
	$(VCS_HOME)/bin/dve $(DVE_OPTS) &

dve_%: $(TBROOT)/run/%/vcdplus.vpd
	@# Invokes DVE on specified test results 
	$(VCS_HOME)/bin/dve $(DVE_OPTS) -vpd run/$*/vcdplus.vpd &

################################################################################
# VERDI
################################################################################
VERDI_OPTS += -sv -ssy -f $(FILES_FILE)
VERDI_OPTS += +define+FAKE_MEM
VERDI_OPTS += -nologo
VERDI_OPTS += $(VERDI_USER)

################################################################################
# Shared variables
################################################################################
BINROOT   := bin#@              default root bin directory "bin/"
RUNROOT   := run#@              default root run directory "run/"
BINDIR    ?= $(BINROOT)/$*#@               Compile in ./BINDIR (default: ./bin/<test>)
RUNDIR    ?= $(RUNROOT)/$*#@               Run in ./RUNDIR (default: ./run/<test>)
URG_REPORTDIR := report#@        URG Report direcotry (default: report)
TIMESTAMP := $(shell date +'%Y%m%d%H%M%S')
DUT_TOP   := top.msh_node
DUT_FILE_LIST_NAME := 2_dut_file_list
PKG_TOP   := mby_msh_pkg 
PKG_FILE_LIST_NAME := 1_pkg_file_list

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

DIRS ?= LOGICROOT TBROOT SL2_LIB SL2_LIB_IP16 SVA_LIB VCS_HOME BINROOT RUNROOT URG_REPORTDIR
.PHONY : dirs
dirs:
	@# Prints list of interesting dirs and their values
	@$(foreach d,$(DIRS),echo "$d = $($d)";)

################################################################################
# all tests defined in the test directory
################################################################################
TESTS := $(basename $(notdir $(wildcard $(TBROOT)/tests/*.sv)))

################################################################################
# Build targets
################################################################################
MDIR ?= $(BINDIR)/csrc
BUILD_OPTS += -sverilog +systemverilogext+.v+.sv+.svh+.sva -M -Mupdate -PP \
              +vcs+lic+wait +notimingcheck -Mdir=$(MDIR) -o $(BINDIR)/simv \
              -l $(BINDIR)/compile.log $(BUILD_USER) -bom $(PKG_TOP) -bfl $(BINROOT)/$(PKG_FILE_LIST_NAME) -bom $(DUT_TOP) -bfl $(BINROOT)/$(DUT_FILE_LIST_NAME)
#BUILD_OPTS += -sverilog +systemverilogext+.v+.sv+.svh+.sva -M -Mupdate -PP \
#              +vcs+lic+wait +notimingcheck -Mdir=$(MDIR) -o $(BINDIR)/simv \
#              -l $(BINDIR)/compile.log $(BUILD_USER) -bfl $(BINROOT)/$(DUT_FILE_LIST_NAME)

build_%: mgm_mem $(TBROOT)/tests/%.sv
	@# Builds monolithic simulator (RTL + test) in bin/$(BINDIR)/test_<test>
	@# VCS version set in VCS_HOME
	@# Mdir set in MDIR
	@# Other compile flags set in BUILD_OPTS/BUILD_USER
	mkdir -p $(BINDIR)
	$(VCS_HOME)/bin/vcs $(strip $(BUILD_OPTS)) $(TBROOT)/tests/$*.sv
	cat $(BINROOT)/*.bfl | $(TBROOT)/scripts/create_rtl_list.pl > $(BINROOT)/rtl_list.tcl

mgm_mem:
	mgm -c msh -cnfg $(TBROOT)/../../mem/msh_node_sandbox_config_file -includes $(MODEL_ROOT)/tools/mgm/blocks_inclusion -apr_struct $(MODEL_ROOT)/tools/mgm/apr_structure -csv $(MODEL_ROOT)/tools/mgm/mby_physical_params.csv -unique mby -clk $(MODEL_ROOT)/tools/mgm/clk_file -rprt_dir $(TBROOT)/verilog/mgm_run/rtl -relative_path_from $(MODEL_ROOT)

.PHONY : clean realclean
clean:
	@# Removes $(BINROOT)
	rm -rf $(BINROOT)
	rm -rf $(TBROOT)/verilog/mgm_run
realclean : clean
	@# Removes $(RUNROOT) and $(URG_REPORTDIR)
	rm -rf $(RUNROOT)
	rm -rf $(URG_REPORTDIR)

################################################################################
# Run targets
################################################################################
BUILD ?= 1 #@                Compiles simulator before running (default: 1)
PRERUN ?=   #@               Optional commands to execute before running simulation
SEED ?=   #@                 Random seed
RUN_OPTS += -l run.log +ntb_random_seed$(if $(SEED),=$(SEED),_automatic) $(RUN_USER)

run_%: $(if $(filter 0,$(BUILD)),,build_%) $(TBROOT)/tests/%.sv
	@# Runs test
	@# Run flags set in RUN_OPTS/RUN_USER
	mkdir -p $(RUNDIR)
	$(PRERUN)
	cd $(RUNDIR) && ../../$(BINDIR)/simv $(strip $(RUN_OPTS))
	@echo "Random seed used: SEED=$(SEED)"
	@grep -q 'Simulation PASSED' $(RUNDIR)/run.log && \
	 !(grep -e 'Simulation \*FAILED\*' \
	        -e 'started at .* failed at' \
	        -qm1 $(RUNDIR)/run.log) && \
	 echo "** Successfully completed verification!" || echo "** Verification encountered an error!"


################################################################################
# Verdi targets
################################################################################
verdi_%: $(TBROOT)/tests/%.sv
	verdi $(VERDI_OPTS) $(TBROOT)/tests/$*.sv &

################################################################################
# Convenient build and run variables
################################################################################
ERROR ?= 1 #@                Stop simulation after ERROR errors (default: 1)
ifdef ERROR
    RUN_OPTS += +UVM_MAX_QUIT_COUNT=$(ERROR) -assert global_finish_maxfail=$(ERROR)
endif

DEBUG ?= 0 #@                Enables full GUI debugging (default: 0)
ifeq "$(strip $(DEBUG))" "1"
    BUILD_OPTS += -debug_all -cflags -g
    RUN_OPTS   += -gui
endif

VPD_SV   ?= $(TBROOT)/verilog/vpd.sv #@               VPD module (default: $(SHARED_DV)/verilog/vpd.sv)
VERDI_SV ?= $(TBROOT)/verilog/verdi.sv #@             VERDI module (default: $(SHARED_DV)/verilog/verdi.sv)
VERDI ?= 0 #@                Enables verdi (default: 0)
TRACE ?= 1 #@                Enables VPD dumping (default: 1)
TRACESTART ?=   #@           Start time for VPD dumping (default: dumping not started at runtime)
TRACEMEMON ?= 0 #@           Enables VPD dumping of MDAs and memories (default: 0)
ifeq "$(strip $(TRACE))" "1"
    ifdef TRACESTART
        RUN_OPTS   += +tracestart=$(TRACESTART)
    endif
    ifeq "$(strip $(VERDI))" "1"
        BUILD_OPTS += -debug_pp $(VERDI_SV)
        BUILD_OPTS += -P $$VERDI_HOME/share/PLI/VCS/LINUX64/novas.tab \
                         $$VERDI_HOME/share/PLI/VCS/LINUX64/pli.a
    else # vcs
        BUILD_OPTS += -debug_pp $(VPD_SV)
        ifeq "$(strip $(TRACEMEMON))" "1"
            RUN_OPTS   += +tracememon
        endif
    endif
endif

CCOV ?= 0 #@                 Enables code coverage (default: 0)
ifeq "$(strip $(CCOV))" "1"
    BUILD_OPTS += -cm line+cond+fsm+tgl+branch+assert -cm_cond basic -cm_libs yv \
                  -cm_line contassign -cm_noconst -cm_tgl mda
    RUN_OPTS   += -cm line+cond+fsm+tgl+branch+assert
endif

FCOV ?= 0 #@                 Enables functional coverage (default: 0)
ifeq "$(strip $(FCOV))" "0"
    BUILD_OPTS += -assert disable_cover -covg_disable_cg
else
    BUILD_OPTS += +define+COVER_ON
endif

ASSERT ?= 0 #@               Enables assertions (default: 0)
ifeq "$(strip $(ASSERT))" "0"
    BUILD_OPTS += -assert disable_assert
else
    RUN_OPTS   += -assert nopostproc
    BUILD_OPTS += +define+ASSERT_ON
    BUILD_OPTS += -assert svaext
endif

ifeq "$(findstring 1,$(CCOV)$(FCOV))" "1"
    RUN_OPTS   += -cm_name $*.$(TIMESTAMP)
endif

HEARTBEAT ?= 0 #@            Enables heartbeat debug (default: 0)
ifeq "$(strip $(HEARTBEAT))" "0"
else
    BUILD_OPTS += +define+HEARTBEAT_ON
    RUN_OPTS   += +heartbeat=$(HEARTBEAT)
endif

################################################################################
# URG
################################################################################
URG_OPTS += $(addprefix -dir ,$(wildcard $(BINROOT)/*/simv.vdb)) -report $(URG_REPORTDIR)/ \
            $(addprefix -elfile ,$(wildcard $(TBROOT)/scripts/coverage.el)) $(URG_USER)
.PHONY : urg
urg:
	@# Generates URG report in $(URG_REPORTDIR)/ from all simv.vdb directories under $(BINROOT)
	@# Automatically detects $(TBROOT)/scripts/coverage.el
	@# URG flags set in URG_OPTS/URG_USER
	$(VCS_HOME)/bin/urg $(URG_OPTS)

################################################################################
# Repetitions
################################################################################
REPS ?= 0
rep_%:
	@# Build once, then
	@# Runs test REPS times (default: 0 = infinity)
	@# Stops after first failure
	$(MAKE) $(addprefix build_,$*)
	@for ((rep=1; rep<=$(REPS) || $(REPS)<1; rep++)); do \
	    echo "Repetition $$rep"; \
	    $(MAKE) BUILD=0 TRACE=0 $(addprefix run_,$*); \
	    grep -q 'Simulation PASSED' $(RUNDIR)/run.log \
	        && !(grep -e 'Simulation \*FAILED\*' -e 'started at .* failed at' -qm1 $(RUNDIR)/run.log) \
	        || exit 1; \
	done
	@echo "Successfully completed $(REPS) repetitions of test:$*!"

################################################################################
# Build All
################################################################################
.PHONY : build_all
build_all:
	@# build all tests
	@$(foreach test, $(TESTS), \
	    echo "Building test: $(test)"; \
	    $(MAKE) $(addprefix build_,$(test)); \
	)

################################################################################
# Regression
# Run each test in the tests directory, then check all run logs.
################################################################################
REGRESS_TRACE := 0 # don't dump traces in regression by default
.PHONY : regress
regress: realclean build_all
	@# run all tests
	@$(foreach test, $(TESTS), \
	    echo "Running test: $(test)"; \
	    $(MAKE) BUILD=0 TRACE=$(REGRESS_TRACE) $(addprefix run_,$(test)); \
	)
	@# check all tests
	@$(foreach test, $(TESTS), \
	    echo -n "Checking test: $(test) "; \
	    grep -q 'Simulation PASSED' $(RUNROOT)/$(test)/run.log \
	        && !(grep -e 'Simulation \*FAILED\*' -e 'started at .* failed at' -qm1 $(RUNROOT)/$(test)/run.log) \
	        && echo "** Passed!" || echo "** Failed!"; \
	)
