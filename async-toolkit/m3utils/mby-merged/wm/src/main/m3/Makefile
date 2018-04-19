# Makefile for CM3 white model build
CM3=$(shell ToolConfig.pl get_tool_exec cm3)
OS_TARG=AMD64_LINUX
WM_SUPPORT_ROOT=wm_support
WM_SUPPORT_TARGDIR=$(WM_SUPPORT_ROOT)/$(OS_TARG)
WM_SUPPORT=$(WM_SUPPORT_ROOT)/$(OS_TARG)/.done

GENVIEWS_ROOT=genviews
GENVIEWS_TARGDIR=$(GENVIEWS_ROOT)/$(OS_TARG)
GENVIEWS=$(GENVIEWS_TARGDIR)/.done

TEST_TARG=$(GENVIEWS_ROOT)/src/build/AMD64_LINUX/testme

test: $(TEST_TARG)
	$(TEST_TARG)

$(WM_SUPPORT): $(wildcard $(WM_SUPPORT_ROOT)/src/*)
	pushd $(WM_SUPPORT_ROOT)/src && $(CM3) -x
	touch $@

$(GENVIEWS): $(wildcard $(GENVIEWS_ROOT)/src/*) $(WM_SUPPORT)
	pushd $(GENVIEWS_ROOT)/src && $(CM3) -x
	touch $@

$(TEST_TARG): $(GENVIEWS)
	pushd $(GENVIEWS_ROOT)/src && ./test.sh

clean:
	rm -rf $(WM_SUPPORT_TARGDIR)
	rm -rf $(GENVIEWS_TARGDIR)
	rm -rf $(GENVIEWS_ROOT)/src/build
	rm -rf $(GENVIEWS_ROOT)/src/work
	find . -name AMD64_LINUX | xargs rm -rf 
