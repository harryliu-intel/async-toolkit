# Makefile for CM3 white model build
CM3=$(shell ToolConfig.pl get_tool_exec cm3)

METAROOT=$(shell ToolConfig.pl get_tool_path meta)
SVPP=${METAROOT}/rdl/svpp/AMD64_LINUX/svpp 
PERLFE=${METAROOT}/perlfe/AMD64_LINUX/perlfe

NEBULON=$(shell ToolConfig.pl get_tool_path nebulon)

OS_TARG=AMD64_LINUX
WM_SUPPORT_ROOT=wm_support
WM_SUPPORT_TARGDIR=$(WM_SUPPORT_ROOT)/$(OS_TARG)
WM_SUPPORT=$(WM_SUPPORT_ROOT)/$(OS_TARG)/.done

GENVIEWS_ROOT=genviews
GENVIEWS_TARGDIR=$(GENVIEWS_ROOT)/$(OS_TARG)
GENVIEWS=$(GENVIEWS_TARGDIR)/.done

MBY_TOP_RDL=../../../../tools/srdl/mby/mby_top_map.rdl
MBY_BUILD_ROOT=$(GENVIEWS_ROOT)/src/build/mby
MBYLIB=$(MBY_BUILD_ROOT)/AMD64_LINUX/libmbylib.a

# TEST_TARG=$(GENVIEWS_ROOT)/src/build/AMD64_LINUX/testme

# test: $(TEST_TARG)
# 	$(TEST_TARG)
mby_mains: $(GENVIEWS) $(MBYLIB)
	$(MAKE) -C $(GENVIEWS_ROOT)/src/mains

$(WM_SUPPORT): $(wildcard $(WM_SUPPORT_ROOT)/src/*)
	pushd $(WM_SUPPORT_ROOT)/src && $(CM3) -x
	touch $@

$(GENVIEWS): $(wildcard $(GENVIEWS_ROOT)/src/*) $(WM_SUPPORT)
	pushd $(GENVIEWS_ROOT)/src && $(CM3) -x
	touch $@

$(MBYLIB): $(MBY_TOP_RDL) $(GENVIEWS)
	${SVPP} --path $(NEBULON)/include:$(dir $<) < $< > work_i1.rdl
	${PERLFE} < work_i1.rdl > work_i2.rdl
	export PERL5LIB=$(dir $<).. && cat work_i2.rdl | perl > work_i3.rdl
	mkdir -p $(MBY_BUILD_ROOT)/src
	$(GENVIEWS_TARGDIR)/genviews -top mby_top_map -o $(MBY_BUILD_ROOT)/src < work_i3.rdl
	cp $(GENVIEWS_ROOT)/src/boilerplate/m3overrides $(MBY_BUILD_ROOT)/src
	sed 's/THELIB/mby/' $(GENVIEWS_ROOT)/src/boilerplate/m3makefile > $(MBY_BUILD_ROOT)/src/m3makefile
	cd $(MBY_BUILD_ROOT)/src && $(CM3) -x



$(TEST_TARG): $(GENVIEWS)
	pushd $(GENVIEWS_ROOT)/src && ./test.sh

clean:
	rm -rf $(WM_SUPPORT_TARGDIR)
	rm -rf $(GENVIEWS_TARGDIR)
	rm -rf $(GENVIEWS_ROOT)/src/build
	rm -rf $(GENVIEWS_ROOT)/src/work
	find . -name AMD64_LINUX | xargs rm -rf 

