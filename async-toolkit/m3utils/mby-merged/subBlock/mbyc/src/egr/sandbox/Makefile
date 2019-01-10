
# Prevent Make from setting implicit rules for files that look like c/cpp.


# tb top
dut ?= egr_top
ifneq ($(dut),egr_top)
    VCS_SRC  = ../$(dut)/sandbox/rtl/$(dut)_tb.sv
else
    VCS_SRC  = rtl/egr_top_tb.sv
endif

ifndef SHRTL
$(warning SHRTL is not set. For better performance, use $ setenv SHRTL `ToolConfig.pl get_tool_path ipconfig/shrtl`)
SHRTL ?= $(shell ToolConfig.pl get_tool_path ipconfig/shrtl)
endif

ifndef DESIGNCOMPILER
$(warning DESIGNCOMPILER is not set. For better performance, use $ setenv DESIGNCOMPILER `ToolConfig.pl get_tool_path designcompiler`)
DESIGNCOMPILER ?= $(shell ToolConfig.pl get_tool_path designcompiler)
endif

# SRC is the relative path from the including Makefile
SRC = ../..

include $(SRC)/shared/sandbox/Make.mby

VCS_DEFS += +define+VCSSIM +define+INTEL_SVA_OFF

use_gui ?= 0
ifeq ($(use_gui),1)
    VCS_FLAGS += -gui
endif

VCS_FLAGS += $(VCS_DEFS)

INCS += $(SRC)/egr/rtl
INCS += $(SRC)/egr/interfaces
INCS += rtl
INCS += $(SRC)/egr/cpb/rtl
INCS += $(SRC)/egr/dpb/rtl
INCS += $(SRC)/egr/etag/rtl
INCS += $(SRC)/egr/lcm/rtl
INCS += $(SRC)/egr/mri/rtl
INCS += $(SRC)/egr/pfs/rtl
INCS += $(SRC)/egr/prc/rtl
INCS += $(SRC)/egr/tcu/rtl
INCS += $(SRC)/egr/tdb/rtl
INCS += $(SRC)/egr/tmu/rtl
INCS += $(SRC)/egr/tqu/rtl
INCS += $(SRC)/egr/cpb/interfaces
INCS += $(SRC)/egr/dpb/interfaces
INCS += $(SRC)/egr/etag/interfaces
INCS += $(SRC)/egr/lcm/interfaces
INCS += $(SRC)/egr/mri/interfaces
INCS += $(SRC)/egr/pfs/interfaces
INCS += $(SRC)/egr/prc/interfaces
INCS += $(SRC)/egr/tcu/interfaces
INCS += $(SRC)/egr/tdb/interfaces
INCS += $(SRC)/egr/tmu/interfaces
INCS += $(SRC)/egr/tqu/interfaces


INCS += $(SRC)/egr/rtl/mem

INCS += $(SRC)/egr/rtl/mem/cpb/src
INCS += $(SRC)/egr/rtl/mem/dpb/src
INCS += $(SRC)/egr/rtl/mem/etag/src
INCS += $(SRC)/egr/rtl/mem/lcm/src
INCS += $(SRC)/egr/rtl/mem/mri/src
INCS += $(SRC)/egr/rtl/mem/pfs/src
INCS += $(SRC)/egr/rtl/mem/prc/src
INCS += $(SRC)/egr/rtl/mem/tcu/src
INCS += $(SRC)/egr/rtl/mem/tdb/src
INCS += $(SRC)/egr/rtl/mem/tmu/src
INCS += $(SRC)/egr/rtl/mem/tqu/src

INCS += $(SRC)/egr/rtl/mem/cpb/src/mem_wrap
INCS += $(SRC)/egr/rtl/mem/dpb/src/mem_wrap
INCS += $(SRC)/egr/rtl/mem/etag/src/mem_wrap
INCS += $(SRC)/egr/rtl/mem/lcm/src/mem_wrap
INCS += $(SRC)/egr/rtl/mem/mri/src/mem_wrap
INCS += $(SRC)/egr/rtl/mem/pfs/src/mem_wrap
INCS += $(SRC)/egr/rtl/mem/prc/src/mem_wrap
INCS += $(SRC)/egr/rtl/mem/tcu/src/mem_wrap
INCS += $(SRC)/egr/rtl/mem/tdb/src/mem_wrap
INCS += $(SRC)/egr/rtl/mem/tmu/src/mem_wrap
INCS += $(SRC)/egr/rtl/mem/tqu/src/mem_wrap

INCS += $(SRC)/mems/rtl
INCS += $(SHRTL)/src/common/rtl
INCS += $(DESIGNCOMPILER)/dw/sim_ver

BUILD_DEP = $(addsuffix /*,$(INCS))
VCS_LIB = $(addprefix -y ,$(INCS))
VCS_INC = $(addprefix +incdir+,$(INCS))

## tb top
#VCS_SRC  = rtl/egr_top_tb.sv

.PHONY: debug gate clean

simv: $(VCS_SRC) $(BUILD_DEP)
	vcs $(VCS_FLAGS) $(VCS_LIB) $(VCS_INC) $(VCS_SRC) -l build.log

debug: simv
	./simv $(TRACE_FLAGS) $(SIMV_FLAGS) +TIMESTAMP_FREQUENCY=100 +TIMEOUT=1000

first_packet_ms1p0 first_packet_ms2p0 first_packet_ms2p1 first_packet_ms2p5: simv
	./simv $(TRACE_FLAGS) $(SIMV_FLAGS) +TIMESTAMP_FREQUENCY=100 +TIMEOUT=1000 +TESTNAME=$@

gate: simv
	./simv $(SIMV_FLAGS) +TIMESTAMP_FREQUENCY=100 +TIMEOUT=1000
	rm -rf DVEfiles csrc/ simv simv.daidir/ simv.vdb/ ucli.key vc_hdrs.h *.txt *.txt.disablelog .synopsys_dve_rebuild.tcl *.vpd

clean:
	rm -rf DVEfiles csrc/ simv simv.daidir/ simv.vdb/ ucli.key vc_hdrs.h *.txt *.txt.disablelog .synopsys_dve_rebuild.tcl *.vpd *.log

%:
	;

