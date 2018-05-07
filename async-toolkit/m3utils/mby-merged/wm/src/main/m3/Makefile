# Makefile for CM3 white model build
OS_TARG=AMD64_LINUX
CM3=$(shell ToolConfig.pl get_tool_exec cm3)

METAROOT=$(shell ToolConfig.pl get_tool_path meta)
SVPP=${METAROOT}/rdl/svpp/$(OS_TARG)/svpp 
PERLFE=${METAROOT}/perlfe/$(OS_TARG)/perlfe

NEBULON=$(shell ToolConfig.pl get_tool_path nebulon)

WM_SUPPORT_ROOT=wm_support
WM_SUPPORT_TARGDIR=$(WM_SUPPORT_ROOT)/$(OS_TARG)
WM_SUPPORT=$(WM_SUPPORT_ROOT)/$(OS_TARG)/.done

GENVIEWS_ROOT=genviews
GENVIEWS_TARGDIR=$(GENVIEWS_ROOT)/$(OS_TARG)
GENVIEWS=$(GENVIEWS_TARGDIR)/genviews

STRUCTGEN_ROOT=structgen
STRUCTGEN_TARGDIR=$(STRUCTGEN_ROOT)/$(OS_TARG)
STRUCTGEN=$(STRUCTGEN_TARGDIR)/structgen
M3_STRUCTS=$(STRUCTGEN_TARGDIR)/derived.m3m

MBY_TOP_RDL=../../../../tools/srdl/mby/mby_top_map.rdl
MBY_BUILD_ROOT=$(GENVIEWS_ROOT)/src/build/mby
MBYLIB=$(MBY_BUILD_ROOT)/$(OS_TARG)/libmby.a

HLP_TOP_RDL=/p/hlp/hlp_release/drops/hlp-DROP_1P2-17ww50d/src/srdl/hlp_top_map.rdl
HLP_BUILD_ROOT=$(GENVIEWS_ROOT)/src/build/hlp
HLPLIB=$(HLP_BUILD_ROOT)/$(OS_TARG)/libhlp.a

TOP=.top
TOP_TEMPLATE=top_template

# TEST_TARG=$(GENVIEWS_ROOT)/src/build/AMD64_LINUX/testme

# test: $(TEST_TARG)
# 	$(TEST_TARG)
mains: mby_mains hlp_mains

top: $(TOP)

$(TOP): $(TOP_TEMPLATE)
	sed "s#MODEL_ROOT#$(MODEL_ROOT)#" $< > $@
	sed -i "s#META_HOME#$(METAROOT)#" $@

mby_mains: $(GENVIEWS) $(MBYLIB)
	$(MAKE) -C $(GENVIEWS_ROOT)/src/mby_mains

hlp_mains: $(HLPLIB)

$(WM_SUPPORT): $(wildcard $(WM_SUPPORT_ROOT)/src/*) $(TOP)
	pushd $(WM_SUPPORT_ROOT)/src && $(CM3) -x
	touch $@

$(GENVIEWS): $(wildcard $(GENVIEWS_ROOT)/src/*) $(WM_SUPPORT) $(TOP)
	pushd $(GENVIEWS_ROOT)/src && $(CM3) -x

$(STRUCTGEN): $(wildcard $(STRUCTGEN_ROOT)/src/*) $(WM_SUPPORT) $(TOP)
	pushd $(STRUCTGEN_ROOT)/src && $(CM3) -x

$(M3_STRUCTS): $(STRUCTGEN)
	pushd $(STRUCTGEN_ROOT)/src && $(realpath $<) do.scm exit

rpc_structs: $(M3_STRUCTS)

$(MBYLIB): $(MBY_TOP_RDL) $(GENVIEWS)
	mkdir -p work
	${SVPP} --path $(NEBULON)/include:$(dir $<) < $< > work/i1.rdl
	${PERLFE} < work/i1.rdl > work/i2.rdl
	export PERL5LIB=$(dir $<).. && cat work/i2.rdl | perl > work/i3.rdl
	mkdir -p $(MBY_BUILD_ROOT)/src
	$(GENVIEWS_TARGDIR)/genviews -top mby_top_map -o $(MBY_BUILD_ROOT)/src < work/i3.rdl
	cp $(GENVIEWS_ROOT)/src/boilerplate/m3overrides $(MBY_BUILD_ROOT)/src
	sed 's/THELIB/mby/' $(GENVIEWS_ROOT)/src/boilerplate/m3makefile > $(MBY_BUILD_ROOT)/src/m3makefile
	cd $(MBY_BUILD_ROOT)/src && $(CM3) -x

$(HLPLIB): $(HLP_TOP_RDL) $(GENVIEWS)
	mkdir -p work
	${SVPP} --path $(NEBULON)/include:$(dir $<) < $< > work/i1.rdl
	${PERLFE} < work/i1.rdl > work/i2.rdl
	export PERL5LIB=$(dir $<) && cat work/i2.rdl | perl > work/i3.rdl
	mkdir -p $(HLP_BUILD_ROOT)/src
	$(GENVIEWS_TARGDIR)/genviews -top hlp_top_map -o $(HLP_BUILD_ROOT)/src < work/i3.rdl
	cp $(GENVIEWS_ROOT)/src/boilerplate/m3overrides $(HLP_BUILD_ROOT)/src
	sed 's/THELIB/hlp/' $(GENVIEWS_ROOT)/src/boilerplate/m3makefile > $(HLP_BUILD_ROOT)/src/m3makefile
	cd $(HLP_BUILD_ROOT)/src && $(CM3) -x


$(TEST_TARG): $(GENVIEWS)
	pushd $(GENVIEWS_ROOT)/src && ./test.sh

clean:
	rm -rf $(WM_SUPPORT_TARGDIR)
	rm -rf $(GENVIEWS_TARGDIR)
	rm -rf $(GENVIEWS_ROOT)/src/build
	rm -rf $(GENVIEWS_ROOT)/src/work
	find . -name $(OS_TARG) | xargs rm -rf 
	rm $(TOP)
