# FocalPoint SDK Top-Level Makefile

# Do not use make's built-in rules 
# Allow user to specify more jobs
ifeq ($(JOBS),)
 MAKEFLAGS += -r 
else
 MAKEFLAGS += -r -j$(JOBS)
endif

# Default platform is the FocalPoint 2000 series EP
ifeq ($(PLATFORM),)
 PLATFORM:=fm85xxep
endif

# Default chips supported is everything but SWAG
ifeq ($(SUPPORTED_CHIP_FAMILIES),)
    SUPPORTED_CHIP_FAMILIES := FM2000 FM4000 HLP CPK
endif

# On Intel development environment?
DEV_ENV_INTEL=$(shell cat /etc/motd | grep "SUSE Linux")

# Include the makefile fragment that builds the core SDK
include ../build/Makefile.core

# Include other makefiles
include ../build/Makefile.perl
include ../build/Makefile.sdk_tests
include ../build/Makefile.applications
include ../build/Makefile.install

