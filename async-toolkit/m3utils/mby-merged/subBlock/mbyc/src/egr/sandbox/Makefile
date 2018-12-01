# SRC is the relative path from the including Makefile to Make.fxr
# Set to the current directory for this Makefile
# Set it to a different relative path for including Make.fxr
# from Makefiles in other directories

SRC = ../..

include $(SRC)/shared/sandbox/Make.mby

# VCS_DEFS += +define+VCSSIM

VCS_FLAGS += $(VCS_DEFS)

INCS += ../rtl
INCS += ../interfaces
INCS += rtl
INCS += ../partition1/dpb/rtl
INCS += ../partition1/epb/rtl
INCS += ../partition1/lcm/rtl
INCS += ../partition1/mri/rtl
INCS += ../partition1/mtm/rtl
INCS += ../partition1/pes/rtl
INCS += ../partition1/pfs/rtl
INCS += ../partition1/prc/rtl
INCS += ../partition1/txc/rtl
INCS += ../partition1/utm/rtl

VCS_INC = $(addprefix +incdir+,$(INCS))
VCS_LIB = $(addprefix -y ,$(INCS))

# tb top
VCS_SRC  = rtl/egr_top_tb.sv

.PHONY: debug gate clean

simv: $(VCS_SRC) ../rtl/* rtl/*
	vcs $(VCS_FLAGS) $(VCS_LIB) $(VCS_INC) $(VCS_SRC) -l build.log

debug: simv
	./simv $(TRACE_FLAGS) $(SIMV_FLAGS) +TIMESTAMP_FREQUENCY=100 +TIMEOUT=1000

gate: simv
	./simv $(SIMV_FLAGS) +TIMESTAMP_FREQUENCY=100 +TIMEOUT=1000
	rm -rf DVEfiles csrc/ simv simv.daidir/ simv.vdb/ ucli.key vc_hdrs.h *.txt *.txt.disablelog *.vpd

clean:
	rm -rf DVEfiles csrc/ simv simv.daidir/ simv.vdb/ ucli.key vc_hdrs.h *.txt *.txt.disablelog *.vpd *.log

