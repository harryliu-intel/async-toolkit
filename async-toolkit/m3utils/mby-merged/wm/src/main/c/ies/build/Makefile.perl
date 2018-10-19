# vim:et:sw=4:syntax=make:ts=4:tw=79:
# Makefile for generating Perl wrappers

# $(warning "Platform = $(PLATFORM)")

ifeq ($(FM_TESTPOINT_ALT_LIBRARY),)
    USE_FM_TESTPOINT_ALT_LIBRARY = NO
else
    USE_FM_TESTPOINT_ALT_LIBRARY = YES
endif

INSTALL_TARGETS         += install-perl

PERL_INSTALL_DIRECTORY  = $(INSTALL_DIRECTORY)/perl-lib

#Building 32-bit executables on 64-bit machine
ifeq ($(PERL_FORCE_32),TRUE)
READKEY_CPPFLAGS           += -m32
READKEY_SHAREDFLAGS        += -m32
PERL_EXTRA_LIBS            += -m32
PERL_CPPFLAGS              += -m32
PERL_CFLAGS                += -m32
endif

PERL_CFLAGS             += -x c

PERL_WRAPPER_CPPFLAGS   +=                                                \
    -I../include                                                          \
    -I../include/alos                                                     \
    -I../include/alos/$(PLATFORM_OS)                                      \
    -I../include/platforms                                                \
    -I../include/platforms/$(PLATFORM)                                    \
    -I../include/platforms/$(PLATFORM)/kernel                             \
    -I../applications/support/include                                     \
    -I../applications/protocols/include                                   \
    -DSWIG_TYPE_TABLE=fm_sdk

# Add defines to CFLAGs to support chip familes
define add_perl_cflags_family
  PERL_WRAPPER_CPPFLAGS += -DFM_SUPPORT_$(1) 
endef

# Make sure the CFLAGS get updated to define the right support tags
$(foreach family,$(SUPPORTED_CHIP_FAMILIES),$(eval $(call add_perl_cflags_family,$(family))))

# The shared library MUST be dynamically linked with the C library to allow the
# fstat(2) library symbol to be resolved.
PERL_WRAPPER_LDFLAGS    +=                           \
    -L../build -lsupport -lprotocols -lFocalpointSDK \
    $(PERL_EXTRA_LIBS)
ifeq ($(PLATFORM_OS),linux)
PERL_WRAPPER_LDFLAGS    += -lrt -lc
endif

SWIG_FLAGS              = -perl5 -shadow -includeall -exportall           \
                          -ignoremissing -proxy

SWIG_IFACE_FILES        = ../perl-wrapper/sdk.i                           \
                          ../perl-wrapper/sdk_fragments.i                 \
                          ../perl-wrapper/sdk_library.i                   \
                          ../perl-wrapper/sdk_packet_handling.i           \
                          ../perl-wrapper/sdk_typemaps.i

ifdef SWIG_LCI_DIRECT_ACCESS
    PERL_WRAPPER_CPPFLAGS   += -DSWIG_LCI_DIRECT_ACCESS

    SWIG_IFACE_FILES        += ../perl-wrapper/sdk_lci.i
endif # SWIG_LCI_DIRECT_ACCESS

# Version was changed from '2.30.01' to '2.30' to avoid validation problems in
# some platforms. This was caused by Dynaloader.
READKEY_CPPFLAGS           +=   -c   -fno-strict-aliasing -pipe -Wdeclaration-after-statement \
                                -D_LARGEFILE_SOURCE -D_FILE_OFFSET_BITS=64 -O1  \
                                -DVERSION=\"2.30\" -DXS_VERSION=\"2.30\" -fpic

READKEY_SHAREDFLAGS        +=  -shared

to-lowercase=$(shell echo "$1" | tr '[A-Z]' '[a-z]')

# Prevent the generated wrapper file(s) and object file(s) from being deleted.
.PRECIOUS: ../perl-wrapper/%.g ../perl-wrapper/%.o

.PHONY: perl install-perl perl-clean

PERL_WRAPPER_OBJS       = ../perl-wrapper/SDK.$(SO_EXTENSION) ../perl-wrapper/SDK_INT.$(SO_EXTENSION)
PERL_WRAPPER_MODULES    = ../perl-wrapper/SDK.pm ../perl-wrapper/SDK_INT.pm

PERL_READKEY_PATH       = ../applications/TestPoint/Libraries/TermReadKey
PERL_READKEY_SRC        = $(PERL_READKEY_PATH)/ReadKey.c $(PERL_READKEY_PATH)/cchars.h \
                          $(PERL_READKEY_PATH)/ppport.h

ifeq (FM4000,$(findstring FM4000,$(SUPPORTED_CHIP_FAMILIES)))
    PERL_WRAPPER_OBJS    += ../perl-wrapper/FM4000.$(SO_EXTENSION)
    PERL_WRAPPER_MODULES += ../perl-wrapper/FM4000.pm
endif

ifeq (FM6000,$(findstring FM6000,$(SUPPORTED_CHIP_FAMILIES)))
    PERL_WRAPPER_OBJS    += ../perl-wrapper/FM6000.$(SO_EXTENSION) ../perl-wrapper/FM6000_REGS.$(SO_EXTENSION)
    PERL_WRAPPER_MODULES += ../perl-wrapper/FM6000.pm ../perl-wrapper/FM6000_REGS.pm
endif

ifeq (FM10000,$(findstring FM10000,$(SUPPORTED_CHIP_FAMILIES)))
    PERL_WRAPPER_OBJS    += ../perl-wrapper/FM10000.$(SO_EXTENSION) ../perl-wrapper/FM10000_REGS.$(SO_EXTENSION)
    PERL_WRAPPER_MODULES += ../perl-wrapper/FM10000.pm ../perl-wrapper/FM10000_REGS.pm
endif

ifeq (HLP,$(findstring HLP,$(SUPPORTED_CHIP_FAMILIES)))
    PERL_WRAPPER_OBJS    += ../perl-wrapper/HLP.$(SO_EXTENSION) ../perl-wrapper/HLP_REGS.$(SO_EXTENSION)
    PERL_WRAPPER_MODULES += ../perl-wrapper/HLP.pm ../perl-wrapper/HLP_REGS.pm
endif

ifeq (CPK,$(findstring CPK,$(SUPPORTED_CHIP_FAMILIES)))
    PERL_WRAPPER_OBJS    += ../perl-wrapper/CPK.$(SO_EXTENSION) ../perl-wrapper/CPK_REGS.$(SO_EXTENSION)
    PERL_WRAPPER_MODULES += ../perl-wrapper/CPK.pm ../perl-wrapper/CPK_REGS.pm
endif

###############################################################################
# Targets
###############################################################################

# ReadKey library:

perl : $(PERL_WRAPPER_OBJS) $(PERL_WRAPPER_MODULES)         

ifeq ($(USE_FM_TESTPOINT_ALT_LIBRARY),YES)

install-perl: perl 
	rm -fr $(PERL_INSTALL_DIRECTORY)
	@echo Creating Perl install directory $(PERL_INSTALL_DIRECTORY)
	mkdir -p $(PERL_INSTALL_DIRECTORY)
	mkdir -p $(PERL_INSTALL_DIRECTORY)/SDK
	@echo Copying Perl wrapper to $(PERL_INSTALL_DIRECTORY)
	cp ../perl-wrapper/SDK.$(SO_EXTENSION) $(PERL_INSTALL_DIRECTORY)
	cp $(filter-out %/SDK.$(SO_EXTENSION),$(PERL_WRAPPER_OBJS)) $(PERL_INSTALL_DIRECTORY)/SDK
	cat ../perl-wrapper/SDK.pm                          \
	    $(filter-out %/SDK.pm,$(PERL_WRAPPER_MODULES))  \
	    > $(PERL_INSTALL_DIRECTORY)/SDK.pm
	../build/gen_sdk_scalars
	cp ../perl-wrapper/SDKScalars.pm $(PERL_INSTALL_DIRECTORY)
	sed -i -e "/require SDK::.*/d" $(PERL_INSTALL_DIRECTORY)/SDK.pm
	find $(PERL_INSTALL_DIRECTORY) -name "*.$(SO_EXTENSION)" -exec chmod a+rx {} \;
	find $(PERL_INSTALL_DIRECTORY) -name "*.pm" -exec chmod a+r {} \;

else

$(PERL_READKEY_PATH)/ReadKey.o : $(PERL_READKEY_SRC)
	$(CC) $(READKEY_CPPFLAGS) $(PERL_CPPFLAGS) -o $(PERL_READKEY_PATH)/ReadKey.o  -c $(PERL_READKEY_PATH)/ReadKey.c

ReadKey.$(SO_EXTENSION) : $(PERL_READKEY_PATH)/ReadKey.o
	$(CC) $(READKEY_SHAREDFLAGS) -o $(PERL_READKEY_PATH)/$@ $^ $(READKEY_LDFLAGS)

install-perl: perl ReadKey.$(SO_EXTENSION)
	rm -fr $(PERL_INSTALL_DIRECTORY)
	@echo Creating Perl install directory $(PERL_INSTALL_DIRECTORY)
	mkdir -p $(PERL_INSTALL_DIRECTORY)
	@echo Copying ReadLine and ReadKey libraries
	mkdir -p $(PERL_INSTALL_DIRECTORY)/Term
	cp -r ../applications/TestPoint/Libraries/Term/* $(PERL_INSTALL_DIRECTORY)/Term
	cp -r ../applications/TestPoint/Libraries/TermReadKey/ReadKey.pm $(PERL_INSTALL_DIRECTORY)/Term
	cp ../applications/TestPoint/Libraries/TermReadKey/ReadKey.$(SO_EXTENSION) $(PERL_INSTALL_DIRECTORY)
	mkdir -p $(PERL_INSTALL_DIRECTORY)/SDK
	@echo Copying Perl wrapper to $(PERL_INSTALL_DIRECTORY)
	cp ../perl-wrapper/SDK.$(SO_EXTENSION) $(PERL_INSTALL_DIRECTORY)
	cp $(filter-out %/SDK.$(SO_EXTENSION),$(PERL_WRAPPER_OBJS)) $(PERL_INSTALL_DIRECTORY)/SDK
	cat ../perl-wrapper/SDK.pm                          \
	    $(filter-out %/SDK.pm,$(PERL_WRAPPER_MODULES))  \
	    > $(PERL_INSTALL_DIRECTORY)/SDK.pm
	../build/gen_sdk_scalars
	cp ../perl-wrapper/SDKScalars.pm $(PERL_INSTALL_DIRECTORY)
	sed -i -e "/require SDK::.*/d" $(PERL_INSTALL_DIRECTORY)/SDK.pm
	find $(PERL_INSTALL_DIRECTORY) -name "*.$(SO_EXTENSION)" -exec chmod a+rx {} \;
	find $(PERL_INSTALL_DIRECTORY) -name "*.pm" -exec chmod a+r {} \;

endif

perl-clean:
	rm -f $(wildcard ../perl-wrapper/*.g)       \
	      $(wildcard ../perl-wrapper/*.o)       \
	      $(wildcard ../perl-wrapper/*.pm)      \
	      $(wildcard ../perl-wrapper/*.$(SO_EXTENSION))      \
	      $(wildcard $(PERL_READKEY_PATH)/*.o)  \
	      $(wildcard $(PERL_READKEY_PATH)/*.$(SO_EXTENSION))  

###############################################################################
# Transformation Rules
###############################################################################

# Enable secondary expansion of static pattern rules.
.SECONDEXPANSION:

../perl-wrapper/%.g ../perl-wrapper/%.pm : ../perl-wrapper/$$(call to-lowercase,$$*).i $(SWIG_IFACE_FILES) $(ALL_HDRS)
	$(SWIG) $(SWIG_FLAGS) $(PERL_WRAPPER_CPPFLAGS) -o $(@D)/$*.g $<

../perl-wrapper/%.o : ../perl-wrapper/%.g
	$(CC) $(PERL_CPPFLAGS) $(PERL_WRAPPER_CPPFLAGS) $(PERL_CFLAGS) -o $@ -c $<

../perl-wrapper/%.$(SO_EXTENSION) : ../perl-wrapper/%.o sdk support protocols
	$(CC) $(LIBFLAGS) -o $@ $< $(PERL_WRAPPER_LDFLAGS)
#../perl-wrapper/checkForUndefSymbols $@

