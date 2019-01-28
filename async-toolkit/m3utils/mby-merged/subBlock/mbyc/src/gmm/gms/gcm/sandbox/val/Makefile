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
#BUILD_OPTS += +incdir+/nfs/sc/disks/sc_mby_00033/souravba/mby/work_root/TR/mem_gen/ww02p4_2/MBY_TEST_2/compout/views/sacrls0g4l1p128x68m2b1w0c0p0d0r1rm3sdrw00/
#BUILD_OPTS += +incdir+/nfs/sc/disks/sc_mby_00033/souravba/mby/work_root/TR/mem_gen/ww02p4_2/MBY_TEST_2/compout/views/sacrls0g4l1p32x47m4b1w0c0p0d0r1rm3sdrw00/
#BUILD_OPTS += +incdir+/nfs/sc/disks/sc_mby_00033/souravba/mby/work_root/TR/mem_gen/ww02p4_2/MBY_TEST_2/compout/views/sacrls0g4l1p64x47m4b1w0c0p0d0r1rm3sdrw00/
#BUILD_OPTS += +incdir+/nfs/sc/disks/sc_mby_00033/souravba/mby/work_root/TR/mem_gen/ww02p4_2/MBY_TEST_2/compout/views/sacrls0g4l1p64x68m2b1w0c0p0d0r1rm3sdrw00/
#BUILD_OPTS += +incdir+/nfs/sc/disks/sc_mby_00033/souravba/mby/work_root/TR/mem_gen/ww02p4_2/MBY_TEST_2/compout/views/sacrls0g4l1p72x48m2b1w0c0p0d0r1rm3sdrw00/
#BUILD_OPTS += +incdir+/nfs/sc/disks/sc_mby_00033/souravba/mby/work_root/TR/mem_gen/ww02p4_2/MBY_TEST_2/compout/views/sadrls0g4l2p32x26m2b1w0c0p0d0rm3sdrw00/
# Use Fake Mem for speedy simulation
BUILD_OPTS += +define+FAKE_MEM
#BUILD_OPTS += +define+INTEL_DC
#BUILD_OPTS += +define+MBY_GCM_BEHAVE_MEMS
#BUILD_OPTS += +define+MBY_MGM_ASIC_BEHAVE
BUILD_OPTS += +define+INTEL_SIMONLY
BUILD_OPTS += +define+INTEL_SVA_OFF

# FPGA builds use smaller memories
#BUILD_OPTS += +define+FPGA

# Testbed Switches
#BUILD_USER += +define+INP_DRIVER_DEBUG
BUILD_USER += +define+REQ_RECORD_DEBUG
BUILD_USER += +define+ARB_DEBUG

#BUILD_OPTS += -debug_all -gui

#DVE_USER += 

