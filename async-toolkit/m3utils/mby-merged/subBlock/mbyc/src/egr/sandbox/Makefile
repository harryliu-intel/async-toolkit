# SRC is the relative path from the including Makefile to Make.fxr
# Set to the current directory for this Makefile
# Set it to a different relative path for including Make.fxr
# from Makefiles in other directories

# tb top
VCS_SRC  = rtl/egr_top_tb.sv
ifdef dut
    ifneq ($(dut),"egr_top")
        VCS_SRC  = ../$(dut)/sandbox/rtl/$(dut)_tb.sv
    endif
endif

SRC = ../..

include $(SRC)/shared/sandbox/Make.mby

VCS_DEFS += +define+VCSSIM +define+INTEL_SVA_OFF

VCS_FLAGS += $(VCS_DEFS)

INCS += $(SRC)/egr/rtl
INCS += ../interfaces
INCS += rtl
INCS += $(SRC)/egr/cpb/rtl
INCS += $(SRC)/egr/dpb/rtl
INCS += $(SRC)/egr/lcm/rtl
INCS += $(SRC)/egr/mri/rtl
INCS += $(SRC)/egr/pfs/rtl
INCS += $(SRC)/egr/prc/rtl
INCS += $(SRC)/egr/tcu/rtl
INCS += $(SRC)/egr/tmu/rtl
INCS += $(SRC)/egr/tqu/rtl
INCS += $(SRC)/mems/rtl/

VCS_INC = $(addprefix +incdir+,$(INCS))
BUILD_DEP = $(addsuffix /*,$(INCS))
VCS_LIB = $(addprefix -y ,$(INCS))

## tb top
#VCS_SRC  = rtl/egr_top_tb.sv

.PHONY: debug gate clean

simv: $(VCS_SRC) $(BUILD_DEP)
	vcs $(VCS_FLAGS) $(VCS_LIB) $(VCS_INC) $(VCS_SRC) -l build.log

debug: simv
	./simv $(TRACE_FLAGS) $(SIMV_FLAGS) +TIMESTAMP_FREQUENCY=100 +TIMEOUT=1000

gate: simv
	./simv $(SIMV_FLAGS) +TIMESTAMP_FREQUENCY=100 +TIMEOUT=1000
	rm -rf DVEfiles csrc/ simv simv.daidir/ simv.vdb/ ucli.key vc_hdrs.h *.txt *.txt.disablelog .synopsys_dve_rebuild.tcl *.vpd

clean:
	rm -rf DVEfiles csrc/ simv simv.daidir/ simv.vdb/ ucli.key vc_hdrs.h *.txt *.txt.disablelog .synopsys_dve_rebuild.tcl *.vpd *.log

