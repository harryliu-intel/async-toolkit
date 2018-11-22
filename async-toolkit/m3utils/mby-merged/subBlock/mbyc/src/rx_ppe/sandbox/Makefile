#/////////////////////////////////////////////////////////////////////////////
#/
#/ Copyright (c) Intel Corp. All Rights Reserved.
#/ Unpublished Proprietary Information.
#/
#/ Function:       Makefile for compiling, running shared verilog testbenches
#/ Authored by:    Jon Bagge
#/
#/////////////////////////////////////////////////////////////////////////////

RTL_SRC = ../..
TARGET_SRC = ../../../../../target
DV_SRC = ../sandbox/dv/verilog
ASSERT_LIB = /p/hdk/rtl/cad/x86-64_linux30/dt/sva_lib/15.4p5_shOpt64
SHARED_SRC = ../../shared
SIM_DIR = dv/run

INCS = $(RTL_SRC)/rx_ppe/rtl
INCS += $(RTL_SRC)/parser/rtl
INCS += $(RTL_SRC)/class_gpa/rtl
INCS += $(RTL_SRC)/class_gpb/rtl
INCS += $(RTL_SRC)/action/rtl
INCS += $(SHARED_SRC)/rtl
INCS += $(SHARED_SRC)/interfaces
INCS += $(TARGET_SRC)/mby/mgm_run/parser/src
INCS += $(TARGET_SRC)/mby/mgm_run/parser/src/mem_wrap
INCS += $(TARGET_SRC)/mby/mgm_run/class_gpa/src
INCS += $(TARGET_SRC)/mby/mgm_run/class_gpa/src/mem_wrap
INCS += $(TARGET_SRC)/mby/mgm_run/class_gpb/src
INCS += $(TARGET_SRC)/mby/mgm_run/class_gpb/src/mem_wrap
INCS += $(TARGET_SRC)/mby/mgm_run/action/src
INCS += $(TARGET_SRC)/mby/mgm_run/action/src/mem_wrap
INCS += $(TARGET_SRC)/mby/mgm_run/rtl
INCS += $(ASSERT_LIB)/SVA_LIB
INCS += /p/hdk/cad/ctech/v15ww50e/source/v

VCS_INC = $(addprefix +incdir+,$(INCS))

VCS_LIB = $(addprefix -y ,$(INCS))
VCS_LIB += $(addprefix -y ,$(DV_SRC))

VCS_SRC  =	$(SHARED_SRC)/rtl/shared_pkg.sv
VCS_SRC  +=	$(SHARED_SRC)/rtl/pkt_pkg.sv
VCS_SRC  +=	$(SHARED_SRC)/rtl/pkt_meta_pkg.sv
VCS_SRC  +=	$(SHARED_SRC)/rtl/rx_ppe_pkg.sv
VCS_SRC  +=	$(ASSERT_LIB)/SVA_LIB/intel_checkers.vs
VCS_SRC  += $(ASSERT_LIB)/SVA_LIB/intel_checkers_pkg.vs
VCS_SRC  += $(SHARED_SRC)/interfaces/ahb_rx_ppe_if.sv
VCS_SRC  += $(SHARED_SRC)/interfaces/glb_rx_ppe_if.sv
VCS_SRC  +=	$(SHARED_SRC)/interfaces/igr_rx_ppe_if.sv
VCS_SRC  +=	$(SHARED_SRC)/interfaces/rx_ppe_igr_if.sv
VCS_SRC  +=	$(SHARED_SRC)/interfaces/rx_ppe_ppe_stm0_if.sv
VCS_SRC  +=	$(SHARED_SRC)/interfaces/rx_ppe_ppe_stm1_if.sv
VCS_SRC  +=	$(SHARED_SRC)/interfaces/par_class_if.sv
VCS_SRC  += $(TARGET_SRC)/mby/mgm_run/parser/src/parser_mem.def
VCS_SRC  += $(TARGET_SRC)/mby/mgm_run/class_gpa/src/class_gpa_mem.def
VCS_SRC  += $(TARGET_SRC)/mby/mgm_run/class_gpb/src/class_gpb_mem.def
VCS_SRC  += $(TARGET_SRC)/mby/mgm_run/action/src/action_mem.def

VCS_FLAGS = -full64 +v2k -sverilog -notice +vcs+lic+wait +warn=noUII-L \
    -timescale=10ps/10ps -debug_pp +libext+.v+.sv+.vh+.svh+.vs+.vm+.sva \
    -assert svaext +define+VCSSIM+MBY_PARSER_BEHAVE_MEMS+MBY_CLASSIFIER_BEHAVE_MEMS \
    -Mdir=$(SIM_DIR)/csrc -o $(SIM_DIR)/$@

default: ppe_stm

.PHONY: rx_ppe ppe_stm clean

rx_ppe: $(SIM_DIR)
	@vcs $(VCS_FLAGS) $(VCS_LIB) $(VCS_INC) $(VCS_SRC) $(DV_SRC)/rx_ppe_tb.sv | tee $(SIM_DIR)/rx_ppe.log

ppe_stm: $(SIM_DIR)
	@vcs $(VCS_FLAGS) $(VCS_LIB) $(VCS_INC) $(SIM_DIR)/../verilog/ppe_stm_tb.sv | tee $(SIM_DIR)/ppe_stm.log

clean:
	rm -rf $(SIM_DIR)/csrc $(SIM_DIR)/*.daidir $(SIM_DIR)/*.vdb $(SIM_DIR)/ppe_stm $(SIM_DIR)/*.log $(SIM_DIR)/*.vpd $(SIM_DIR)/DVEfiles $(SIM_DIR)/vc_hdrs.h $(SIM_DIR)/ucli.key $(SIM_DIR)/rx_ppe

