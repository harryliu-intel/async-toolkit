# This file is included by cell.mk in each lve cell directory
# Remember that $(CURR_CELL_DIR) is evaluated when -reading- in the rules, not when -executing- the rules.
# Thus, it cannot be used inside rules!
# Instead, use $@, $<, $(@D), $(<D). $^, etc
ifeq ("$(CASTFILES)","1")


# these variables are defined by lve script 
# in the generated Makefile (Actually in targets.mk)
CURR_CELL := $(call GET_CAST_BASE_NAME,$(CURR_CELL_DIR))
CURR_CELL_DFII_DIR := $(DFII-DIR-$(CURR_CELL))

ifeq ("$(USE_ROUTED)","1")
JFLAT_ROUTED := --routed
ROUTED_SUFFIX := .routed
endif # "$(USE_ROUTED)" eq "1"
ifeq ("$(ACCURATE_MODE)","1")
ACCURATE_SUFFIX := .accurate
endif # "$(ACCURATE_MODE)" eq "1"

ifeq ("$(strip $(CDL_NAME_MAP))","")
JFLAT_NAME_MAP :=
else # "$(strip $(CDL_NAME_MAP))" eq ""
JFLAT_NAME_MAP := --cdl-name-map=$(CDL_NAME_MAP)
endif # "$(strip $(CDL_NAME_MAP))" eq ""

# Don't delete intermediates!
.PRECIOUS: $(CURR_CELL_DIR)/cell.localprops$(ROUTED_SUFFIX)$(ACCURATE_SUFFIX)
.PRECIOUS: $(CURR_CELL_DIR)/cell.nodeprops$(ROUTED_SUFFIX)$(ACCURATE_SUFFIX)
.PRECIOUS: $(CURR_CELL_DIR)/cell.portprops
.PRECIOUS: $(CURR_CELL_DIR)/cell.leakynodes$(ROUTED_SUFFIX)
.PRECIOUS: $(CURR_CELL_DIR)/cell.scenarios$(ROUTED_SUFFIX)
.PRECIOUS: $(CURR_CELL_DIR)/jflat$(ROUTED_SUFFIX)/env-ntpc/%.nodes
.PRECIOUS: $(CURR_CELL_DIR)/jflat$(ROUTED_SUFFIX)/env-ntpc/%.nodes.latest
.PRECIOUS: $(CURR_CELL_DIR)/%/cell.cdl
.PRECIOUS: $(CURR_CELL_DIR)/%/../cell.cdl
.PRECIOUS: $(CURR_CELL_DIR)/%/cast.changed
.PRECIOUS: $(CURR_CELL_DIR)/cell.cdl
.PRECIOUS: $(CURR_CELL_DIR)/cast.changed
.PRECIOUS: $(CURR_CELL_DIR)/jflat$(ROUTED_SUFFIX)/%
.PRECIOUS: $(CURR_CELL_DIR)/jflat$(ROUTED_SUFFIX)/%.latest

# stuff from jflat, all signatured
$(CURR_CELL_DIR)/jflat$(ROUTED_SUFFIX)/% : $(CURR_CELL_DIR)/jflat$(ROUTED_SUFFIX)/%.latest
	$(CASTFILES_UPDATE_SIGNATURE)

$(CURR_CELL_DIR)/%/cast.changed: $(CURR_CELL_DIR)/cell.cdl
	touch '$@'

$(CURR_CELL_DIR)/%/../cell.cdl: $(CURR_CELL_DIR)/cell.cdl
	sync

$(CURR_CELL_DIR)/%/cell.cdl: $(CURR_CELL_DIR)/cell.cdl
	sync
#	ln -sf ../cell.cdl '$@'

$(CURR_CELL_DIR)/%/cell.stats: $(CURR_CELL_DIR)/cell.stats
	sync
	ln -sf '$<' '$@'

$(CURR_CELL_DIR)/jflat$(ROUTED_SUFFIX)/%.latest: $(CURR_CELL_DIR)/cast.d

$(CURR_CELL_DIR)/cell.cdl: $(CURR_CELL_DIR)/jflat$(ROUTED_SUFFIX)/cdl/default.latest
	$(CASTFILES_UPDATE_SIGNATURE)

$(CURR_CELL_DIR)/cell.stats: $(CURR_CELL_DIR)/jflat$(ROUTED_SUFFIX)/query/default.latest
	$(CASTFILES_UPDATE_SIGNATURE)

# override with --cell-localprops=file
LOCALPROPS_SOURCE := $(CURR_CELL_DIR)/jflat$(ROUTED_SUFFIX)/local-nodes/default.latest
LOCALPROPS_TARGET := $(CURR_CELL_DIR)/cell.localprops$(ROUTED_SUFFIX)$(ACCURATE_SUFFIX)
PORTPROPS_SOURCE  := $(CURR_CELL_DIR)/jflat$(ROUTED_SUFFIX)/query/default.latest
PORTPROPS_TARGET  := $(CURR_CELL_DIR)/cell.portprops
NODEPROPS_SOURCE := $(CURR_CELL_DIR)/jflat$(ROUTED_SUFFIX)/node-props/default.latest
NODEPROPS_TARGET := $(CURR_CELL_DIR)/cell.nodeprops$(ROUTED_SUFFIX)$(ACCURATE_SUFFIX)
NODEPROPS_SOURCE_PATTERN := $(CURR_CELL_DIR)/%/jflat$(ROUTED_SUFFIX)/node-props/default.latest
NODEPROPS_TARGET_PATTERN := $(CURR_CELL_DIR)/%/cell.nodeprops$(ROUTED_SUFFIX)$(ACCURATE_SUFFIX)

ifneq ("$(CELL_LOCALPROPS)","")
    ifeq ($(shell test -s "$(CELL_LOCALPROPS)" ; echo $$?), 0)
        LOCALPROPS_SOURCE := $(CELL_LOCALPROPS)
endif # $(shell test -s "$(CELL_LOCALPROPS)" ; echo $$?) eq 0
endif # "$(CELL_LOCALPROPS)" ne ""
ifneq ($(shell cmp "$(LOCALPROPS_TARGET)" "$(LOCALPROPS_SOURCE)" 2>/dev/null | wc -l), 0)
    $(shell /bin/cp "$(LOCALPROPS_SOURCE)" "$(LOCALPROPS_TARGET)" &>/dev/null )
endif # $(shell cmp "$(LOCALPROPS_TARGET)" "$(LOCALPROPS_SOURCE)" 2>/dev/null | wc -l) ne 0
#ifneq ($(shell cmp "$(PORTPROPS_TARGET)" "$(PORTPROPS_SOURCE2)" 2>/dev/null | wc -l), 0)
#    $(shell awk 'NF==1 {print $$1}' "$(PORTPROPS_SOURCE)" > "$(PORTPROPS_TARGET)" 2>/dev/null )
#endif # $(shell cmp "$(PORTPROPS_TARGET)" "$(PORTPROPS_SOURCE)" 2>/dev/null | wc -l) ne 0
# this is needed to complete the dependencies on initial runs.
$(LOCALPROPS_TARGET) : $(LOCALPROPS_SOURCE)
	$(CASTFILES_UPDATE_SIGNATURE)

ifneq ("$(CELL_NODEPROPS)","")
    ifeq ($(shell test -s "$(CELL_NODEPROPS)" ; echo $$?), 0)
        NODEPROPS_SOURCE := $(CELL_NODEPROPS)
        NODEPROPS_SOURCE_PATTERN := $(CELL_NODEPROPS)
endif # $(shell test -s "$(CELL_NODEPROPS)" ; echo $$?) eq 0
endif # "$(CELL_NODEPROPS)" ne ""
ifneq ($(shell cmp "$(NODEPROPS_TARGET)" "$(NODEPROPS_SOURCE)" 2>/dev/null | wc -l), 0)
    $(shell /bin/cp "$(NODEPROPS_SOURCE)" "$(NODEPROPS_TARGET)" &>/dev/null )
endif # $(shell cmp "$(NODEPROPS_TARGET)" "$(NODEPROPS_SOURCE)" 2>/dev/null | wc -l) ne 0
# this is needed to complete the dependencies on initial runs.
$(NODEPROPS_TARGET) : $(NODEPROPS_SOURCE)
	$(CASTFILES_UPDATE_SIGNATURE)
# pattern rules for /../ upward references, unsafe as described in bug 28447
$(NODEPROPS_TARGET_PATTERN) : $(NODEPROPS_SOURCE_PATTERN)
	$(CASTFILES_UPDATE_SIGNATURE)

$(PORTPROPS_TARGET) : $(PORTPROPS_SOURCE)
	$(CASTFILES_ENQUEUE_TASK) && \
        awk 'NF == 1 {print $$0}' '$<' > '$@.tmp'
	if [[ ! ( -e "$@" ) ]] || ! cmp -s '$@' '$@.tmp' ; then \
	     mv -f '$@.tmp' '$@' && echo '$@ changed'; \
	   fi; /bin/rm -f '$@.tmp' ; \
	$(CASTFILES_DEQUEUE_TASK)


$(CURR_CELL_DIR)/cell.leakynodes$(ROUTED_SUFFIX): $(CURR_CELL_DIR)/jflat$(ROUTED_SUFFIX)/leaky-nodes/default.latest
	$(CASTFILES_UPDATE_SIGNATURE)

$(CURR_CELL_DIR)/cell.scenarios$(ROUTED_SUFFIX): $(CURR_CELL_DIR)/jflat$(ROUTED_SUFFIX)/alint-scenarios/default.latest
	$(CASTFILES_UPDATE_SIGNATURE)

$(CURR_CELL_DIR)/jflat$(ROUTED_SUFFIX)/env-ntpc/%.nodes : $(CURR_CELL_DIR)/jflat$(ROUTED_SUFFIX)/env-ntpc/%.nodes.latest
	$(CASTFILES_UPDATE_SIGNATURE)

$(CURR_CELL_DIR)/jflat$(ROUTED_SUFFIX)/env-ntpc/%.nodes.latest : $(CURR_CELL_DIR)/jflat$(ROUTED_SUFFIX)/env-ntpc/%.latest
	$(CASTFILES_ENQUEUE_TASK) && \
	cat '$<' | $(GNUGAWK) '{print $$1 " " $$4 " " $$5 " " $$6}' > '$@'; \
	$(CASTFILES_DEQUEUE_TASK)

$(CURR_CELL_DIR)/.nodes.latest$(ROUTED_SUFFIX):
	touch '$@'

$(CURR_CELL_DIR)/.nodes$(ROUTED_SUFFIX): $(CURR_CELL_DIR)/.nodes.latest$(ROUTED_SUFFIX)
	$(CASTFILES_UPDATE_SIGNATURE)
# cast dependencies and environments
$(CURR_CELL_DIR)/cast.d : $(CURR_CELL_DIR)/.nodes$(ROUTED_SUFFIX)
	#TASK=cast_deps CELL=$(call GET_CAST_FULL_NAME,$(@D))
	dir='$(@D)'; \
	task=cast_deps && $(CASTFILES_ENQUEUE_TASK) && \
	if [[ ( -n "$(call LVE_SKIP,deps)" ) && ( -e '$@' ) ]] ; then exit; fi; \
	until [[ ( -e "$$dir/.cellname" ) || ( "$$dir" == "/" ) ]]; do \
		dir="$$(dirname "$$dir")"; done; \
	target="$$dir/cast.d"; \
	if [[ ( -n "$(call LVE_SKIP,deps)" ) && ( -e "$$target" ) ]] ; then exit; fi; \
	QB_DIAG_FILE="$$target.diag" QB_RUN_NAME='lve_jflat' \
	  $(EXEC_PACKAGE) $(JFLAT) \
	  $(JFLAT_ROUTED) \
	  "--cell=$(call GET_CAST_FULL_NAME,$(@D)):*" \
	  "--cast-path=$(CAST_PATH)" \
	  "--tool=$(JFLAT_TOOLS)" \
	  "--nodes=$$(cat '$<')" \
	  "--internalWires=$(INTERNAL_WIRES)" \
	  "--internalRules=$(INTERNAL_RULES)" \
	  '--cdl-mos-parameters=m' \
	  '$(JFLAT_NAME_MAP)' \
	  "--cdl-translate=cadence" "--hsim-translate=gds2" "--query-translate=none" \
	  --hsim-rand-seed=0 --hsim-rand-length=4 \
	  "--query-tasks=prs,transistors=gate.STATICIZER.0:gate.STATICIZER.1,routing,density,tau,external_nodes=im:di:re" \
	  "--query-separator=," \
	  --query-no-header \
	  --ignore-feedback \
	  "--output-suffix=.latest" \
	  "--output-dir=$$dir/jflat$(ROUTED_SUFFIX)" \
	  "--cast-deps=$$target.tmp" \
	  "--cast-deps-target=$$target" && \
	if [ -f "$$target.tmp" ]; then mv "$$target.tmp" "$$target"; fi && \
	if [ -f "$$dir/jflat$(ROUTED_SUFFIX)/node-props/default.latest" ]; then \
		mkdir -p "$$dir/jflat$(ROUTED_SUFFIX)/local-nodes" && \
		$(GNUGAWK) '{ if ($$1 == "SIGNOFF") c = 18; else c = 17; \
		              if ($$c == "INTERNAL") { $$c = ""; print $$0 } }' < \
			"$$dir/jflat$(ROUTED_SUFFIX)/node-props/default.latest" | \
		$(GNUSED) -e 's/ \+/ /g' -e 's/ $$//' > \
			"$$dir/jflat$(ROUTED_SUFFIX)/local-nodes/default.latest"; \
	fi && \
	if [ "$(SIXTYFIVEMODE)" = "1" ]; then \
	 cdlsize265 $$dir/jflat$(ROUTED_SUFFIX)/cdl/default.latest; \
	fi; \
	$(CASTFILES_DEQUEUE_TASK)


$(CURR_CELL_DIR)/jflat$(ROUTED_SUFFIX)/%.latest: $(CURR_CELL_DIR)/.nodes$(ROUTED_SUFFIX)
	#TASK=cast_deps CELL=$(call GET_CAST_FULL_NAME,$(@D))
	dir='$(@D)'; \
	task=cast_deps && $(CASTFILES_ENQUEUE_TASK) && \
	if [[ ( -n "$(call LVE_SKIP,deps)" ) && ( -e '$@' ) ]] ; then exit; fi; \
	until [[ ( -e "$$dir/.cellname" ) || ( "$$dir" == "/" ) ]]; do \
		dir="$$(dirname "$$dir")"; done; \
	target="$$dir/cast.d"; \
	if [[ ( -n "$(call LVE_SKIP,deps)" ) && ( -e "$$target" ) ]] ; then exit; fi; \
	QB_DIAG_FILE="$$target.diag" QB_RUN_NAME='lve_jflat' \
	  $(EXEC_PACKAGE) $(JFLAT) \
	  $(JFLAT_ROUTED) \
	  "--cell=$(call GET_CAST_FULL_NAME,$(@D)):*" \
	  "--cast-path=$(CAST_PATH)" \
	  "--tool=$(JFLAT_TOOLS)" \
	  "--nodes=$$(cat '$<')" \
	  "--internalWires=$(INTERNAL_WIRES)" \
	  "--internalRules=$(INTERNAL_RULES)" \
	  '--cdl-mos-parameters=m' \
	  '$(JFLAT_NAME_MAP)' \
	  "--cdl-translate=cadence" "--hsim-translate=gds2" "--query-translate=none" \
	  --hsim-rand-seed=0 --hsim-rand-length=4 \
	  "--query-tasks=prs,transistors=gate.STATICIZER.0:gate.STATICIZER.1,routing,density,tau,external_nodes=im:di:re" \
	  "--query-separator=," \
	  --query-no-header \
	  --ignore-feedback \
	  "--output-suffix=.latest" \
	  "--output-dir=$$dir/jflat$(ROUTED_SUFFIX)" \
	  "--cast-deps=$$target.tmp" \
	  "--cast-deps-target=$$target" && \
	if [ -f "$$target.tmp" ]; then mv "$$target.tmp" "$$target"; fi && \
	if [ -f "$$dir/jflat$(ROUTED_SUFFIX)/node-props/default.latest" ]; then \
		mkdir -p "$$dir/jflat$(ROUTED_SUFFIX)/local-nodes" && \
		$(GNUGAWK) '{ if ($$1 == "SIGNOFF") c = 18; else c = 17; \
		              if ($$c == "INTERNAL") { $$c = ""; print $$0 } }' < \
			"$$dir/jflat$(ROUTED_SUFFIX)/node-props/default.latest" | \
		$(GNUSED) -e 's/ \+/ /g' -e 's/ $$//' > \
			"$$dir/jflat$(ROUTED_SUFFIX)/local-nodes/default.latest"; \
	fi && \
	if [ "$(SIXTYFIVEMODE)" = "1" ]; then \
	 cdlsize265 $$dir/jflat$(ROUTED_SUFFIX)/cdl/default.latest; \
	fi; \
	$(CASTFILES_DEQUEUE_TASK)



$(CURR_CELL_DIR)/%/slurp.mk : $(CURR_SLURP_DIR)/%/df2.d \
                              $(CURR_CELL_DIR)/%/df2.d \
			      $(CURR_SLURP_DIR)/%/../cell.cdl \
			      $(CURR_CELL_DIR)/cast.d
	rm -f '$@'
	touch '$@'
	if ! ( cmp -s '$(word 3,$^)' '$(dir $(word 4,$^))/jflat$(ROUTED_SUFFIX)/cdl/default.latest' ) ; then exit; fi; \
	layoutdiff=$$($(BUILD)/filetypes/cdbfiles/cdbdiff.pl \
		      '$(word 1,$^)' '$(word 2,$^)' ); \
	echo "\# $$layoutdiff"; \
	if [[ ( -n "$$layoutdiff" ) ]] ; then exit; fi; \
	echo 'CURR_CELL_DIR := $(@D)' > '$@'; \
	echo 'CURR_SLURP_DIR := $(<D)' >> '$@'; \
	echo 'include $$(BUILD)/filetypes/castfiles/slurp_cell_view.mk' >> '$@'	

$(CURR_CELL_DIR)/%/df2.d: $(CURR_CELL_DFII_DIR)/%/layout.oa
	#TASK=layout_deps CELL=$(call GET_CAST_FULL_NAME,$(@D)) VIEW=$(call GET_NTH_FROM_LAST_DIR,$(@D),1)
	mkdir -p '$(@D)'; \
	task=layout_deps && $(CASTFILES_ENQUEUE_TASK) && \
	if [[ ( -n "$(call LVE_SKIP,deps)" ) && ( -e '$@' ) ]] ; then exit; fi; \
	QB_DIAG_FILE='$@.diag' QB_RUN_NAME='lve_df2d' \
	  $(EXEC_PACKAGE) df2d \
	    --dfII-dir '$(DFII_DIR)' \
	    --cell '$(call GET_CAST_FULL_NAME,$(@D))' \
	    --view $(call GET_NTH_FROM_LAST_DIR,$(@D),1) \
	    --fulcrum-pdk-root=$(FULCRUM_PDK_PACKAGE_ROOT) \
	    --output-file '$@' && \
	$(CASTFILES_DEQUEUE_TASK)
	: < '$@'

$(CURR_CELL_DIR)/%/nanotime$(EXTRACT_DIR)/analog.mk: $(CURR_CELL_DIR)/cast.d
	mkdir -p '$(@D)' && \
	(echo 'SPICE_DIR := $(subst $(ROOT_TARGET_DIR),$$(ROOT_TARGET_DIR),$(@D))' && \
	 echo 'GDS_DIR := $(subst $(ROOT_TARGET_DIR),$$(ROOT_TARGET_DIR),$(call CANONICALIZE_PATH,$(@D)/..))' && \
	 echo 'CELL_DIR := $(subst $(ROOT_TARGET_DIR),$$(ROOT_TARGET_DIR),$(call CANONICALIZE_PATH,$(@D)/../..))' && \
	 echo 'include $$(BUILD)/filetypes/castfiles/nanotime.mk') > '$@'

$(CURR_CELL_DIR)/%/analog.mk: $(CURR_CELL_DIR)/cast.d
	mkdir -p '$(@D)'
	echo 'SPICE_DIR := $(subst $(ROOT_TARGET_DIR),$$(ROOT_TARGET_DIR),$(@D))' > '$@'
	echo 'CELL_DIR := $(subst $(ROOT_TARGET_DIR),$$(ROOT_TARGET_DIR),$(call CONONICALIZE_PATH,$(@D)/../..))' >> '$@'
	for env in $$(find '$(<D)/jflat$(ROUTED_SUFFIX)/aspice' -noleaf -mindepth 1 -type d -printf "%P\n"); do \
	  echo "ENV := $$env" >> '$@'; \
	  echo 'include $$(BUILD)/filetypes/castfiles/env.mk' >> '$@'; \
	done

# layout signoffs - will be read in after df2.d is made
ifneq ("$(strip $(wildcard $(CURR_CELL_DIR)/layout/df2.d))","")
$(CURR_CELL_DIR)/layout/drc.err: $(call GET_DRC_SIGNOFFS_FROM_CDBDEP,$(CURR_CELL_DIR)/layout/df2.d)
$(CURR_CELL_DIR)/layout/drc.result: $(call GET_DRC_CELL_SIGNOFF_FROM_CDBDEP,$(CURR_CELL_DIR)/layout/df2.d)
$(CURR_CELL_DIR)/layout/frc.err: $(call GET_FRC_SIGNOFFS_FROM_CDBDEP,$(CURR_CELL_DIR)/layout/df2.d)
$(CURR_CELL_DIR)/layout/frc.result: $(call GET_FRC_CELL_SIGNOFF_FROM_CDBDEP,$(CURR_CELL_DIR)/layout/df2.d)
$(CURR_CELL_DIR)/layout/hdrc.result: $(call GET_HDRC_SIGNOFFS_FROM_CDBDEP,$(CURR_CELL_DIR)/layout/df2.d)
$(CURR_CELL_DIR)/layout/hlvs.result: $(call GET_HLVS_SIGNOFF_FROM_CDBDEP,$(CURR_CELL_DIR)/layout/df2.d)
$(CURR_CELL_DIR)/layout/lvs.result: $(call GET_LVS_SIGNOFF_FROM_CDBDEP,$(CURR_CELL_DIR)/layout/df2.d)
#$(CURR_CELL_DIR)/layout/extract.result: $(call GET_LVS_SIGNOFF_FROM_CDBDEP,$(CURR_CELL_DIR)/layout/df2.d)
endif # "$(strip $(wildcard $(CURR_CELL_DIR)/layout/df2.d))" ne ""

CURR_TARGET_DEPS := $(CURR_TARGET_DEPS) $(CURR_LAYOUT_DEPS) $(CURR_SPICE_DEPS) $(CURR_CELL_DIR)/cast.d

# lve --justPrint=1 mode just does make -n, 
# but we must be sure that all signature files are up to date!
ifeq ($(JUST_PRINT_MODE),1)
CURR_TARGET_DEPS := $(CURR_TARGET_DEPS) $(CURR_CELL_DIR)/signature.mk $(CURR_CELL_DIR)/signature.includes
$(CURR_CELL_DIR)/signature.mk: $(CURR_CELL_DIR)/cast.d
	echo "$(@D)/signature.includes: \\" >> '$@'
	for env in $$(find '$(<D)/jflat$(ROUTED_SUFFIX)/aspice' -noleaf -mindepth 1 -type d -printf "%P\n"|grep -v default); do \
	  echo "$(@D)/jflat$(ROUTED_SUFFIX)/env-ntpc/$$env \\" >> '$@' ;\
	  echo "$(@D)/jflat$(ROUTED_SUFFIX)/env-ntpc/$$env.nodes \\" >> '$@' ;\
	  echo "$(@D)/jflat$(ROUTED_SUFFIX)/aspice/$$env/env.asp \\" >> '$@' ;\
	done
	echo "$(@D)/jflat$(ROUTED_SUFFIX)/aspice/default/prs.asp \\" >> '$@'
	echo "$(@D)/jflat$(ROUTED_SUFFIX)/aspice/default/noprs.asp \\" >> '$@'
	echo "$(@D)/cell.stats \\" >> '$@'
	echo "$(@D)/cell.cdl \\" >> '$@'
	echo "$(@D)/cell.localprops$(ROUTED_SUFFIX)$(ACCURATE_SUFFIX) \\" >> '$@'
	echo "$(@D)/cell.portprops \\" >> '$@'
	echo "$(@D)/cell.leakynodes$(ROUTED_SUFFIX) \\" >> '$@'
	echo "$(@D)/cell.scenarios$(ROUTED_SUFFIX)" >> '$@'

$(CURR_CELL_DIR)/signature.includes: $(CURR_CELL_DIR)/signature.mk
	touch '$@'
endif # $(JUST_PRINT_MODE) eq 1
endif # "$(CASTFILES)" eq "1" 249 lines back
