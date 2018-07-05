#
# Makefile to build
# - registers.html,
# - hlp_register_constants_pkg.vh,
# - hlp_register_struct.vh, and
# - various RTL models and data
#
# Usage: make all |   - default target - see below
#             has |   - ../../results/doc/registers.mif/.xml
#             jdc |   - .reg to .rdl conversion
#             cpk     - cpk_fgrp register doc and data
#

# Input files
DOT_REG         := $(wildcard *.reg)
DOT_RDL         := $(DOT_REG:.reg=.rdl)
ALL_RDL         := $(wildcard *.rdl) $(DOT_RDL)
DOT_LOGICAL     := $(shell find $(MODEL_ROOT)/src/mem_gen \
                     -regex ".*\.\(custom_\)?logical")

# Output files
HTML            := ../../results/doc/registers.html
PARAM           := ../rtl/hlp/common/hlp_register_constants_pkg.vh
STRUCT          := ../rtl/hlp/common/hlp_register_struct.vh
PG              := policy_group.csv
BASEQ           := baseq.csv
SECURITY_PG     := ../../src/rtl/hlp/lsm/imn_root/hlp_sai_security_addr_pg.sv

# Output paths
RESULTSXMLDIR   := ../../results/doc/output/xml/
RESULTSQCDIR    := ../../results/doc/output/qualitychecker
RESULTSDOCDIR   := ../../results/doc/

# Generators
GENRDL          := ../../scripts/reg2rdl.sh
MTOOLS          := /p/hlp/utils/mnystroe/2016WW49/bin.x86_64
GENPG           := ${MTOOLS}/genpg
CHECKOVERLAPS   := ${MTOOLS}/checkoverlaps
GEN             := ../../scripts/srdl/rdl.rb
UPPERCASE       := ../../scripts/srdl/uppercase_field.pl
ATOMICGEN       := ../../scripts/srdl/atomic_pkg_gen.pl
IMN_BACKDOOR    := ../../scripts/srdl/imn_backdoor_path

# Targets
all: $(DOT_RDL) $(SECURITY_PG) hlp_mem_gen.rdl rtl atomic wm_regs
has: has_mif
jdc: $(DOT_RDL)
cpk: cpk_fgrp

# Rules
include Makefile.cpk # make cpk
include Makefile.has # make has

$(DOT_RDL): $(DOT_REG) $(GENRDL)
	$(UPPERCASE) ./
	cd ../../tools/cplusplus && make
	cd ../../tools/reg2rdl && make
	$(GENRDL) $(DOT_REG)

$(RESULTSDOCDIR):
	mkdir -p $(RESULTSDOCDIR)

$(PG): $(ALL_RDL) $(RESULTSDOCDIR) hlp_mem_gen.rdl
	$(GEN) sv_pg hlp_top_map.rdl $(PG)

$(BASEQ): $(ALL_RDL) hlp_mem_gen.rdl
	$(GEN) sv_baseq hlp_top_map.rdl $(BASEQ)
	
# Note for hlp_mgmt_handler_mgmt soft_ctrl is skipped because type is RW/V, it is implemented in custom code
rtl: $(ALL_RDL) $(RESULTSDOCDIR)
	$(GEN) html      hlp_top_map.rdl $(HTML)
	$(GEN) sv_param  hlp_top_map.rdl $(PARAM)
	$(GEN) sv_struct hlp_top_map.rdl $(STRUCT)
	cp -f $(HTML) registers.html
	$(GEN) sv_rtl hlp_mapper.rdl:0,7  ../rtl/hlp/ipp/ffu/fcmn/mapper/hlp_mapper_mgmt1.sv
	$(GEN) sv_rtl hlp_mapper.rdl:9,10 ../rtl/hlp/ipp/ffu/fcmn/mapper/hlp_mapper_mgmt2.sv
	$(GEN) sv_rtl hlp_mapper.rdl:19,10 ../rtl/hlp/ipp/ffu/fcmn/mapper/hlp_mapper_mgmt3.sv
	$(GEN) sv_rtl hlp_mapper.rdl:29,1 ../rtl/hlp/ipp/ffu/fcmn/mapper/hlp_mapper_mgmt4.sv
	$(GEN) sv_rtl hlp_entropy.rdl     ../rtl/hlp/ipp/ffu/fcmn/entropy/hlp_entropy_mgmt.sv
	$(GEN) sv_rtl hlp_mpls_mux.rdl    ../rtl/hlp/ipp/ffu/fcmn/mpls_mux/hlp_mpls_mux_mgmt.sv
	$(GEN) sv_rtl hlp_imn.rdl:2,1     ../rtl/hlp/lsm/chassis/hlp_imn_mgmt.sv
	$(GEN) sv_rtl hlp_imn.rdl:42,2    ../rtl/hlp/lsm/hlp_cdc_mgmt.sv
	$(GEN) sv_rtl hlp_imn.rdl:9,1     ../rtl/hlp/lsm/chassis/hlp_fuse_ctrlr_mgmt.sv
	$(GEN) sv_rtl hlp_imn.rdl:10,2    ../rtl/hlp/lsm/imn_root/hlp_fuse_handler_mgmt.sv
	$(GEN) sv_rtl hlp_imn.rdl:44,6    ../rtl/hlp/lsm/imn_root/hlp_mgmt_handler_mgmt.sv
	$(GEN) sv_rtl hlp_imn.rdl:63,6    ../rtl/hlp/lsm/imn_root/hlp_bsm_mgmt.sv
	$(GEN) sv_rtl hlp_ffu_sweeper.rdl ../rtl/hlp/ipp/ffu/fcmn/ffu_mgmt/hlp_ffu_sweeper_mgmt.sv
	$(GEN) sv_shc hlp_mem_gen.rdl:hlp_FCMN_SHELL_CTL_map ../rtl/hlp/ipp/ffu/fcmn/hlp_fcmn_shell_ctl_mgmt.sv
	$(GEN) sv_shc hlp_mem_gen.rdl:hlp_TAIL_SHELL_CTL_map ../rtl/hlp/ipp/tail/hlp_tail_shell_ctl_mgmt.sv
	$(GEN) sv_shc hlp_mem_gen.rdl:hlp_FWD_SHELL_CTL_map ../rtl/hlp/ipp/fwd/hlp_fwd_shell_ctl_mgmt.sv
	$(GEN) sv_shc hlp_mem_gen.rdl:hlp_FGHASH_SHELL_CTL_map ../rtl/hlp/ipp/ffu/fghash/hlp_fghash_shell_ctl_mgmt.sv
	$(GEN) sv_shc hlp_mem_gen.rdl:hlp_FGRP_SHELL_CTL_map ../rtl/hlp/ipp/ffu/fgrp/hlp_fgrp_shell_ctl_mgmt.sv
	$(GEN) sv_shc hlp_mem_gen.rdl:CPK_FGRP_SHELL_CTL_map ../rtl/hlp/ipp/ffu/cpk_fgrp/hlp_cpk_fgrp_shell_ctl_mgmt.sv
	$(GEN) sv_shc hlp_mem_gen.rdl:hlp_PARSER_SHELL_CTL_map ../rtl/hlp/ipp/parser/hlp_pa_shell_ctl_mgmt.sv
	$(GEN) sv_shc hlp_mem_gen.rdl:hlp_MOD_SHELL_CTL_map ../rtl/hlp/mod/hlp_mod_shell_ctl_mgmt.sv
	$(GEN) sv_shc hlp_mem_gen.rdl:hlp_IMN_SHELL_CTL_map ../rtl/hlp/lsm/hlp_lsm_shell_ctl_mgmt.sv
	$(GEN) sv_shc hlp_mem_gen.rdl:hlp_SIA_SHELL_CTL_map ../rtl/hlp/ports/port4/sia/hlp_sia_shell_ctl_mgmt.sv
	$(GEN) sv_shc hlp_mem_gen.rdl:hlp_SCHED_SHELL_CTL_map ../rtl/hlp/sched/hlp_sched_shell_ctl_mgmt.sv
	$(GEN) sv_shc hlp_mem_gen.rdl:hlp_MSEC_SHELL_CTL_map ../rtl/hlp/ports/msec/hlp_msec_shell_ctl_mgmt.sv

hlp_mem_gen.rdl: $(DOT_LOGICAL)
	$(GEN) rdl_shell_ctl hlp_mem_gen.rdl

qc: $(ALL_RDL)
	mkdir -p $(RESULTSDOCDIR)
	nebulon -timeout 60000 -input hlp_top_map.rdl -out_dir $(RESULTSDOCDIR) -qualitychecker

wm_regs: $(ALL_RDL)
	cd $(MODEL_ROOT)/verif/wm && make -f Makefile_cpk.registers build
	cd $(MODEL_ROOT)/verif/wm && make -f Makefile.registers build

atomic: $(PARAM) checkoverlaps 
	perl $(ATOMICGEN)

$(SECURITY_PG): $(PG) $(GENPG)
	$(GENPG) --policygroups 8 HLP_PG0 HLP_PG1 HLP_PG2 HLP_PG3 HLP_PG4 HLP_PG5 HLP_PG6 HLP_PG7 -copyrightpath ${MODEL_ROOT}/scripts/intelcopyright.txt -skipholes -bits 28 -sv $(SECURITY_PG) $(PG)

backdoor: $(ALL_RDL) $(RESULTSDOCDIR) hlp_mem_gen.rdl
	$(GEN) sv_path hlp_top_map.rdl hlp_backdoor.path
	$(IMN_BACKDOOR)

checkoverlaps: $(BASEQ) 
	$(CHECKOVERLAPS) $(BASEQ)

.PHONY: clean
clean:
	cd ../../tools/cplusplus && make clean
	cd ../../tools/reg2rdl && make clean
	rm -rf $(RESULTSDOCDIR)
	rm -f $(HTML) registers.html
	rm -f $(DOT_RDL)
	rm -f hlp_mem_gen.rdl
	rm -f $(PG)
	rm -f $(SECURITY_PG)
	rm -f $(BASEQ)
