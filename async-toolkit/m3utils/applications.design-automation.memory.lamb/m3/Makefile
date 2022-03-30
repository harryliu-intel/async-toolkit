.SUFFIXES:
.NOTPARALLEL:
.DELETE_ON_ERROR:

#
# Top-level Makefile for LAMB m3 directory
# Based on Makefile for Madison Bay's register generation
#

######################################################################
#
# config stuff -- update here
# MBY used ToolConfig.pl
# here we hard-code for now, because we don't have that infrastructure (yet?)
#

CM3=$(RTA_CM3_HOME)/bin/cm3
M3UTILSROOT=../m3utils
LAMBROOT=../m3
M3UTILSVERSION=$(shell ./git_get_hash.sh $(M3UTILSROOT))
APLOTSRC=/nfs/sc/disks/bfn_pd_cb_02/mnystroe/m3utils/lamb/lambbuilder/src/aplot/

######################################################################

OS_TARG=AMD64_LINUX

LAMBBUILDERLIB_ROOT=lambbuilderlib
LAMBBUILDERLIB_TARGDIR=$(LAMBBUILDERLIB_ROOT)/$(OS_TARG)
LAMBBUILDERLIB=$(LAMBBUILDERLIB_TARGDIR)/.done

LAMBBUILDER_ROOT=lambbuilder
LAMBBUILDER_TARGDIR=$(LAMBBUILDER_ROOT)/$(OS_TARG)
LAMBBUILDER=$(LAMBBUILDER_TARGDIR)/lambbuilder

LAMBCHARACTERIZE_ROOT=lambcharacterize
LAMBCHARACTERIZE_TARGDIR=$(LAMBCHARACTERIZE_ROOT)/$(OS_TARG)
LAMBCHARACTERIZE=$(LAMBCHARACTERIZE_TARGDIR)/lambcharacterize

APLOTDIR=$(LAMBBUILDER_ROOT)/src/aplot

M3UTILSVERSIONPREFIX=m3utils.version

TOP=.top
TOP_TEMPLATE=top_template

.PHONY: all aplot
# aplot is a binary from Andrew...
#all: mains 
#all: mains lambbuilder/src/aplot/aplot

.PHONY: mains
mains: $(LAMBBUILDER) $(LAMBCHARACTERIZE)

$(LAMBBUILDER): $(LAMBBUILDERLIB)
	pushd $(LAMBBUILDER_ROOT)/src && $(CM3) -x

$(LAMBCHARACTERIZE): $(LAMBBUILDERLIB)
	pushd $(LAMBCHARACTERIZE_ROOT)/src && $(CM3) -x

$(LAMBBUILDERLIB): $(TOP) 
	pushd $(LAMBBUILDERLIB_ROOT)/src && $(CM3) -x
	touch $@

# provide a mechanism to force rebuild on m3utils update
M3UTILS_VERSION_F=$(M3UTILSVERSIONPREFIX).$(M3UTILSVERSION)
$(M3UTILS_VERSION_F):
	touch $@

top: $(TOP)
$(TOP): $(TOP_TEMPLATE) $(M3UTILS_VERSION_F)
	cp $(TOP_TEMPLATE) $@
	sed -i "s#M3UTILS_ROOT#$(M3UTILSROOT)#" $@
	sed -i "s#LAMB_ROOT#$(LAMBROOT)#" $@
	echo LAMBTOP=\"$(PWD)\" >> $@

.PHONY: aplot
aplot: $(APLOTDIR)/aplot

$(APLOTDIR)/aplot: $(APLOTSRC)/aplot
	mkdir $(APLOTDIR) || true
	cp $(APLOTSRC)/aplot $(APLOTDIR)/aplot
	cp $(APLOTSRC)/libreadline.so.5 $(APLOTDIR)/libreadline.so.5

clean:
	rm -rf $(LAMBCHARACTERIZE_TARGDIR)
	rm -rf $(LAMBBUILDER_TARGDIR)
	rm -rf $(LAMBBUILDERLIB_TARGDIR)
	rm -f $(TOP)
	find . -name $(OS_TARG) | xargs rm -rf
	rm -rf $(APLOTDIR)
	rm -f $(M3UTILSVERSIONPREFIX).*
