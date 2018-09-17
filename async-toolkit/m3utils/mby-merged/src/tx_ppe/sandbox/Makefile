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
TARGET_SRC = ../../../target
DV_SRC = ../sandbox/dv/verilog
ASSERT_LIB = /p/hdk/rtl/cad/x86-64_linux30/dt/sva_lib/15.4p5_shOpt64
SHARED_SRC = ../../shared
SIM_DIR = dv/run

INCS = $(RTL_SRC)/tx_ppe/rtl
INCS += $(SHARED_SRC)/rtl
INCS += $(SHARED_SRC)/interfaces
INCS += $(ASSERT_LIB)/SVA_LIB

VCS_INC = $(addprefix +incdir+,$(INCS))

VCS_LIB = $(addprefix -y ,$(INCS))
VCS_LIB += $(addprefix -y ,$(DV_SRC))

VCS_SRC  =	$(SHARED_SRC)/rtl/shared_pkg.sv
VCS_SRC  +=	$(ASSERT_LIB)/SVA_LIB/intel_checkers_pkg.vs
VCS_SRC  += $(SHARED_SRC)/interfaces/ahb_rx_ppe_if.sv
VCS_SRC  += $(SHARED_SRC)/interfaces/glb_rx_ppe_if.sv
VCS_SRC  +=	$(SHARED_SRC)/interfaces/egr_tx_ppe_if.sv

VCS_FLAGS = -full64 +v2k -sverilog -notice +vcs+lic+wait +warn=noUII-L \
    -timescale=10ps/10ps -debug_pp +libext+.v+.sv+.vh+.svh+.vs+.vm+.sva \
    -assert svaext +define+VCSSIM+INTEL_SVA_OFF+MBY_PARSER_BEHAVE_MEMS \
    -Mdir=$(SIM_DIR)/csrc -o $(SIM_DIR)/$@

default: tx_ppe

.PHONY: tx_ppe clean

tx_ppe: $(SIM_DIR)
	@vcs $(VCS_FLAGS) $(VCS_LIB) $(VCS_INC) $(VCS_SRC) $(DV_SRC)/tx_ppe_tb.sv | tee $(SIM_DIR)/tx_ppe.log

clean:
	rm -rf $(SIM_DIR)/csrc $(SIM_DIR)/*.daidir $(SIM_DIR)/*.vdb $(SIM_DIR)/ppe_stm $(SIM_DIR)/*.log $(SIM_DIR)/*.vpd $(SIM_DIR)/DVEfiles $(SIM_DIR)/vc_hdrs.h $(SIM_DIR)/ucli.key $(SIM_DIR)/tx_ppe

