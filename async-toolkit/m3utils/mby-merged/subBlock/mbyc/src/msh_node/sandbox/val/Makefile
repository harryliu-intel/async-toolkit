-include Makefile.overrides
include ./scripts/Makefile

FILES_FILE := ./scripts/files

BUILD_OPTS += -file $(FILES_FILE)
#BUILD_OPTS += -assert enable_diag
#BUILD_OPTS += +nospecify
BUILD_OPTS += -timescale=1ps/1ps
#BUILD_OPTS += +define+delay_value=0.001

#BUILD_OPTS += +define+VCSSIM
#BUILD_OPTS += +define+SHRTL_LIB_MODEL

# Use Fake Mem for speedy simulation
BUILD_OPTS += +define+FAKE_MEM
BUILD_OPTS += +define+MBY_MSH_BEHAVE_MEMS
BUILD_OPTS += +define+MBY_BEHAVE_MEMS

# FPGA builds use smaller memories
#BUILD_OPTS += +define+FPGA

# Testbed Switches
#BUILD_USER += +define+INP_DRIVER_DEBUG
BUILD_USER += +define+REQ_RECORD_DEBUG
BUILD_USER += +define+ARB_DEBUG

#BUILD_OPTS += -debug_all -gui

#DVE_USER += 

