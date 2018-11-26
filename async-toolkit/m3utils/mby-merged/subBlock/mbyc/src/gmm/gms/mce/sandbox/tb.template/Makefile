SHELL = /usr/bin/csh -f

-include Makefile.overrides

SETUP   ?= $(if $(VCS_HOME),,source setup;)
NBJOB   ?= $(if $(findstring tool,$(__NB_CLASS)),,nbjob run $(NBFLAGS))
NBFLAGS ?= --target sc_normal --class SLES11SP4\&\&1G --qslot /DCG/MadisonBay/RTL/normal --mode interactive
VCS     ?= vcs
FILES   ?= -f files
TEST    ?= doa

################################################################################
# Base build and run flags
BASEVCSFLAGS += -sverilog +libext+.sv+.v+.vs+.vp -l vcs.log -timescale=1ns/1ps -debug_pp +vcs+vcdpluson \
                +vcs+lic+wait -xlrm uniq_prior_final -xprop=xprop.cfg +notimingchecks +define+VCSSIM \
                +warn=noUII-L
BASERUNFLAGS += -l $(TEST).log +vpdfile+$(TEST).vpd +ntb_random_seed$(if $(SEED),=$(SEED),_automatic) \
                -assert nopostproc +test=$(TEST)

################################################################################
# Exported vars for "files" files, mainly used for base directory paths
NEBULON ?= $(shell ToolConfig.pl get_tool_path nebulon)
MBY_SRC ?= $(MODEL_ROOT)/subBlock/mbyc/src
.EXPORT_ALL_VARIABLES:

################################################################################
# User targets
default: build run

build simv:
	$(SETUP) $(NBJOB) $(VCS) $(BASEVCSFLAGS) $(VCSFLAGS) $(FILES)

run: simv
	$(NBJOB) simv $(BASERUNFLAGS) $(RUNFLAGS)
	@echo
	@echo Checking $(TEST).log... | tee -a $(TEST).log
	@(grep -q -i -P '(error|fatal|fail)' $(TEST).log && (echo FAIL) || (echo PASS)) | tee -a $(TEST).log

clean:
	rm -rf $(wildcard csrc simv* *.log ucli.key *.vpd ##* unifiedInference.log xprop.log)

################################################################################
# Targets for Makefile debug
VARS ?= $(sort $(.VARIABLES))
vars:
	@$(foreach v,$(filter-out .VARIABLES VARS,$(VARS)),echo '$v = $($v)';)
vars_noex:
	@$(foreach v,$(filter-out .VARIABLES VARS,$(VARS)),echo '$v = $(value $v)';)
