# SRC is the relative path from the including Makefile to Make.fxr
# Set to the current directory for this Makefile
# Set it to a different relative path for including Make.fxr
# from Makefiles in other directories

SRC = ../..

VCS_DEFS  = +define+VCSSIM
VCS_DEFS += +define+NO_SLA

VCS_FLAGS = -full64 +v2k -sverilog -notice +vcs+lic+wait +warn=noUII-L \
    -timescale=10ps/10ps -debug_pp +libext+.v+.sv+.vh+.svh+.vs+.vm+.sva \
    $(VCS_DEFS) -debug_all +vcs+vcdpluson

VCS_FLAGS += +notimingchecks -ignore driver_checks +warn=noTFIPC
VCS_FLAGS += -assert svaext+dve+disable_cover+enable_diag+filter_past

#upgrade warning to error on no module instance, FEBE errors out if NMIN
VCS_FLAGS += -error=NMIN:1

VCS_FLAGS += -error=ENUMASSIGN          # Illegal assignment to enum variable
VCS_FLAGS += -error=ICTA-SI             # Incompatible complex type
VCS_FLAGS += -error=SV-SELS             # Smaller enum label size
VCS_FLAGS += -error=IPC                 # Illegal high conn for port connect
VCS_FLAGS += -error=DPIMI               # Duplicate port in module instantiation
VCS_FLAGS += -error=REO                 # Return statement in non-void function must specify an expression.
VCS_FLAGS += -error=IPDW                # Identifier previously declared
VCS_FLAGS += -error=INAV                # Index into non-array variable
VCS_FLAGS += -error=FVNU                # Field value not used in struct expression
VCS_FLAGS += -error=ZONMCM              # Zero or negative multiconcat multiplier
VCS_FLAGS += -error=IPDASP              # Identifier previously declared in ANSI port declaration
VCS_FLAGS += -error=AOUP                # Attempt to override undefined parameter
VCS_FLAGS += -error=FRCREF              # Force on ref port not supported
VCS_FLAGS += -error=ISALS               # Incompatible structure/array literal
VCS_FLAGS += -error=SV-ANDNMD           # Argument names do not match
VCS_FLAGS += -error=SV-SVPIA            # String value passed to integral arg
VCS_FLAGS += -error=OSVF-NPVIUFPI       # Non-parameterized virtual interface is being used for parameterized interface

TRACE_FLAGS  = +notimingchecks +warn=noTFIPC +vcs+vcdpluson +error+999 +vpdfile+$@.vpd
SIMV_FLAGS   = -assert report=$@.txt+verbose+nopostproc -l $@.log

INCS  = $(SRC)/shared/interfaces
INCS += ../interfaces
INCS += ../rtl
INCS += ../sandbox

VCS_INC = $(addprefix +incdir+,$(INCS))

VCS_LIB = $(addprefix -y ,$(INCS))

#Start packages
VCS_SRC  = $(SRC)/shared/rtl/shared_pkg.sv
VCS_SRC += $(SRC)/shared/rtl/mby_gmm_pkg.sv
VCS_SRC += $(SRC)/shared/rtl/mby_egr_pkg.sv
VCS_SRC += $(SRC)/shared/rtl/mby_egr_pkg.sv

# tb top
VCS_SRC += egr_top_tb.sv

.PHONY: dve debug gate clean

dve:
	$(VCS_HOME)/bin/dve -full64

simv: $(VCS_SRC) ../interfaces/* ../rtl/* ../sandbox/*.sv
	vcs $(VCS_FLAGS) $(VCS_LIB) $(VCS_INC) $(VCS_SRC) -l build.log

debug: simv
	./simv $(TRACE_FLAGS) $(SIMV_FLAGS)

gate: simv
	./simv $(SIMV_FLAGS)
	rm -rf DVEfiles csrc/ simv simv.daidir/ simv.vdb/ ucli.key vc_hdrs.h *.txt *.txt.disablelog *.vpd

clean:
	rm -rf build.log DVEfiles csrc/ simv simv.daidir/ simv.vdb/ ucli.key vc_hdrs.h *.log *.txt *.txt.disablelog *.vpd

