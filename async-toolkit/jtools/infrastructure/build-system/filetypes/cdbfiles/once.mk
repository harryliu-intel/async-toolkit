ifeq ($(MAKE_LAYOUT),1)

ifeq ("$(strip $(VIRTUOSO_INTEGRATION_PACKAGE_ROOT))","")
$(error You must set the variable VIRTUOSO_INTEGRATION_PACKAGE_ROOT to make layout. )
endif

ifeq ("$(strip $(FULCRUM_PDK_PACKAGE_ROOT))","")
$(error You must set the variable FULCRUM_PDK_PACKAGE_ROOT to make layout. )
endif

ifeq ("$(strip $(CAST_PATH))","")
$(error You must set the variable CAST_PATH to make layout. )
endif

QRSH := $(shell which qrsh)
ifeq ("$(strip $(QRSH))","")
$(error "Couldn't find qrsh...needed to MAKE_LAYOUT" )
endif


OS_TYPE := $(shell uname -s)
ARCH_TYPE := $(shell uname -m)
VIRTUOSO_INTEGRATION_ARCH_BIN := $(VIRTUOSO_INTEGRATION_PACKAGE_ROOT)/$(OS_TYPE)-$(ARCH_TYPE)/bin

%.aspice: %.spice
	$(VIRTUOSO_INTEGRATION_PACKAGE_ROOT)/share/script/perl/spice/rc_spice2aspice $^ $@


endif
