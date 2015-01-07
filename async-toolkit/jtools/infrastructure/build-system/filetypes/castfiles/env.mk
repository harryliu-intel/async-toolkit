# XXX - remember that $(SPICE_DIR) and $(CELL_DIR) are evaluated when -reading- in the rules, not when -executing- the rules.
# Thus, they cannot be used inside rules!
# Instead, use $@, $<, $(@D), $(<D). $^, etc

ifeq ("$(USE_ROUTED)","1")
ROUTED_SUFFIX := .routed
BUDGET_ROUTED := --routed
endif # "$(USE_ROUTED)" eq "1"
ifeq ("$(ACCURATE_MODE)","1")
ACCURATE_SUFFIX := .accurate
endif # "$(ACCURATE_MODE)" eq "1"
ifeq ("$(CLEAN_TRACE)", "")
CLEAN_TRACE := 0
endif # "$(CLEAN_TRACE)" eq ""

DELAY_CAP_INTS := $(foreach cap, $(DELAY_CAP), $(shell perl -e 'printf "%g", $(cap) / 1e-15'))

BUDGET_TAU := 43:20e-12 45:20.93e-12

.PRECIOUS: $(foreach cap, $(DELAY_CAP_INTS), $(SPICE_DIR)/%/directives.$(cap)f)
.PRECIOUS: $(SPICE_DIR)/alint/%/lib.raw
.PRECIOUS: $(SPICE_DIR)/alint/%/cell.lib
.PRECIOUS: $(SPICE_DIR)/alint/%/cell.fakelib
.PRECIOUS: $(SPICE_DIR)/alint/%/cell.timing
.PRECIOUS: $(SPICE_DIR)/alint/%/fullcell.timing
.PRECIOUS: $(SPICE_DIR)/alint/%/cell.slack
.PRECIOUS: $(foreach tau,$(BUDGET_TAU),$(SPICE_DIR)/alint/%/cell.paths_top.$(word 1,$(subst :, ,$(tau))))
.PRECIOUS: $(SPICE_DIR)/../../floorplan/estimated/instances/.instances
.PRECIOUS: $(CELL_DIR)/floorplan/estimated/instances/.instances
.PRECIOUS: $(SPICE_DIR)/alint/%/../../../../../../floorplan/estimated/instances/.instances
.PRECIOUS: $(SPICE_DIR)/../../layout/extracted/instances/.instances
.PRECIOUS: $(CELL_DIR)/layout/extracted/instances/.instances
.PRECIOUS: $(SPICE_DIR)/alint/%/../../../../../../layout/extracted/instances/.instances

$(SPICE_DIR)/alint/%/cell.slack : \
    $(SPICE_DIR)/alint/%/cell.timing \
    $(foreach tau,$(BUDGET_TAU),$(SPICE_DIR)/alint/%/cell.paths_top.$(word 1,$(subst :, ,$(tau))))
	(for tau in $(foreach tau,$(BUDGET_TAU),$(word 1,$(subst :, ,$(tau)))); do \
	  getslack '$(@D)/cell.timing' "${@D}/cell.paths_top.$$tau" $$tau; \
	done) > '${@D}/cell.slack'

# PATHS_TOP(tau,jautoTau,instances,layout)
define PATHS_TOP
$$(SPICE_DIR)/alint/%/cell.paths_top.$(1) : $(3)
	#TASK=budget$(1) VIEW=$$(call GET_NTH_FROM_LAST_DIR,$$(SPICE_DIR),2) CELL=$$(call GET_CAST_FULL_NAME,$$(@D))
	working_dir=`mktemp -d "$$(WORKING_DIR)/budget.XXXXXX"`; \
	chmod 2775 "$$$$working_dir" ; \
	subtype=$$$$(echo '$$(call GET_CAST_FULL_NAME,$$(@D))' | $(GNUSED) -e 's,^.*\.,,'); \
	cellname=$$$$(echo '$$(call GET_CAST_FULL_NAME,$$(@D))' | $(GNUSED) -e 's,\.[^\.]*$$$$,,'); \
	mkdir -p "$$$$working_dir/out"; \
	chmod 2775 "$$$$working_dir/out" ; \
	mkdir -p "$$(@D)"; \
	sync ; \
	QB_DIAG_FILE='$$@.diag' QB_RUN_NAME='lve_budget$(1)' \
	$(EXEC_PACKAGE) $(JAUTO) \
	  --config='$(FULCRUM_PDK_PACKAGE_ROOT)/share/Fulcrum/jauto/jauto.config' \
	  --config='$(FULCRUM_PDK_PACKAGE_ROOT)/share/Fulcrum/jauto/process.config' \
	  --solver=useJCGSolver \
	  --cdlRoot="$$$$working_dir/out" \
	  --outRoot="$$$$working_dir/out" \
	  --castInRoot='$(CAST_PATH)' \
	  --cellName="$$$$cellname" \
	  --subtype="$$$$subtype" \
	  --tau=$(2) \
	  --mode=size \
	  --define='lib.6T.6T.attr.exclude_sram_bits:true' \
	  --trial \
	  $(BUDGET_ROUTED) \
	  '$(4)' \
	  --completeCatPath \
	  --completeSizingPath \
	  --keepLancelotRuns \
	  --debug-level=10 \
	  --noDebug \
	  --proteusBudget \
	  --allFixedSize \
	  --quiet \
	  --violationReportLimit=0 ; \
	cp -p "$$$$working_dir/out/paths_top.debug" "$$@"; \
	cd "$$(WORKING_DIR)"; /bin/rm -rf "$$$$working_dir"
endef

ifeq ($(call GET_NTH_FROM_LAST_DIR,$(SPICE_DIR),1),nogeometry)
$(foreach tau,$(BUDGET_TAU),$(eval $(call PATHS_TOP,$(word 1,$(subst :, ,$(tau))),$(word 2,$(subst :, ,$(tau))),,--noFloorplan)))
else # $(call GET_NTH_FROM_LAST_DIR eq $(SPICE_DIR),1),nogeometry
INSTANCE_DIR := $(CELL_DIR)/$(call GET_NTH_FROM_LAST_DIR,$(SPICE_DIR),2)/$(patsubst extracted-%,extracted,$(call GET_NTH_FROM_LAST_DIR,$(SPICE_DIR),1))/instances
$(foreach tau,$(BUDGET_TAU),$(eval $(call PATHS_TOP,$(word 1,$(subst :, ,$(tau))),$(word 2,$(subst :, ,$(tau))),$(INSTANCE_DIR)/.instances,--layoutRoot=$(INSTANCE_DIR))))
endif # $(call GET_NTH_FROM_LAST_DIR eq $(SPICE_DIR),1),nogeometry

$(SPICE_DIR)/alint/%/cell.fakelib : 
	echo '#TASK=fakelib CELL=$(call GET_CAST_FULL_NAME,$(@D))' 1>&2 ; \
	  task=fakelib && $(CASTFILES_ENQUEUE_TASK) && \
	  QB_DIAG_FILE=/dev/null QB_RUN_NAME='lve_fake_lib' \
	  $(EXEC_PACKAGE) mkdefaultlib \
	  --java-flags="$(GLOBAL_JAVA_FLAGS)" \
	  $(GLOBAL_JRE_FLAGS) \
	  --cell="$(call GET_CAST_FULL_NAME,$(@D))" \
	  --size=$(FAKESIZE) \
	  --cast-path='$(CAST_PATH)' ; \
	  mkdir -p "$(@D)" ; \
	  /bin/cp -p "$(call GET_CAST_FULL_NAME,$(@D)).lib" "$(@D)/cell.lib" ; \
	  /bin/mv -f "$(call GET_CAST_FULL_NAME,$(@D)).lib" "$@" ; \
	  /bin/rm -f "$(call GET_CAST_FULL_NAME,$(@D)).timing" ; \
	  task=fakelib && $(CASTFILES_DEQUEUE_TASK)

$(SPICE_DIR)/alint/%/fullcell.timing : \
    $(SPICE_DIR)/alint/%/cell.timing \
    $(SPICE_DIR)/alint/%/cell.slack
	-cat '$(@D)/cell.timing' '$(@D)/cell.slack' > '$@'

$(SPICE_DIR)/alint/%/cell.lib : $(SPICE_DIR)/alint/%/fullcell.timing
	echo '#TASK=celllib CELL=$(call GET_CAST_FULL_NAME,$(@D))' 1>&2 ; \
	  task=celllib && $(CASTFILES_ENQUEUE_TASK) && \
	  QB_DIAG_FILE=/dev/null QB_RUN_NAME='lve_celllib' \
	  $(EXEC_PACKAGE) $(GENERATELIB) \
	  --cell="$(call GET_CAST_FULL_NAME,$(@D))" \
          --cast-path='$(CAST_PATH)' \
          --translate='$(LIBTRANSLATE)' \
          --timing-file='$<' > '$@.tmp'; \
          /bin/mv -f '$@.tmp' '$@'; \
	  task=celllib && $(CASTFILES_DEQUEUE_TASK)

.PRECIOUS: $(foreach a_num, $(ALINT_BIN_ENUM), $(SPICE_DIR)/%/lib_parallel/lib.in.$(a_num))
define LIB_IN_TEMP
$(SPICE_DIR)/%/lib_parallel/lib.in.$(1) : $(CELL_DIR)/cell.nodeprops$(ROUTED_SUFFIX)$(ACCURATE_SUFFIX)
	mkdir -p '$$(@D)'
	$$(CASTFILES_ENQUEUE_TASK) && \
	$$(MAKE_ALINT_IN) \
	  --dynamic-only=1 \
	  --bumpCC=0 --delayCC=0 \
	  --bumpTau=0 --delayTau="$(DELAY_TAU)" \
	  --delayCap="$(DELAY_CAP)" \
	  --delayFast="0" \
	  --leakage="0" \
	  --inverter-leakage="0" \
	  --maxNodePerAlintBin="$(MAX_NODE_PER_ALINT_BIN)" \
	  --maxAlintBinNum="$(MAX_ALINT_BIN_NUM)" \
	  --alintBin=$(1) \
	  --libChar=1 \
	  --static-only="$(LIB_STATIC_ONLY)" \
	  '$$<' '$$@.tmp' ; \
	mv -f '$$@.tmp' '$$@'; \
	$$(CASTFILES_DEQUEUE_TASK)
endef

$(foreach a_num, $(ALINT_BIN_ENUM), $(eval $(call LIB_IN_TEMP,$(a_num))))

.PRECIOUS: $(foreach a_num, $(ALINT_BIN_ENUM), $(SPICE_DIR)/%/alint_PO_parallel/alint_PO.in.$(a_num))
define ALINT_PO_IN_TEMP
$(SPICE_DIR)/%/alint_PO_parallel/alint_PO.in.$(1) : $(CELL_DIR)/cell.nodeprops$(ROUTED_SUFFIX)$(ACCURATE_SUFFIX)
	mkdir -p '$$(@D)'
	$$(CASTFILES_ENQUEUE_TASK) && \
	$$(MAKE_ALINT_IN) \
	  --dynamic-only=1 \
	  --bumpCC=0 --delayCC=0 \
	  --bumpTau=0 --delayTau="$(DELAY_TAU)" \
	  --delayCap="$(DELAY_CAP)" \
	  --delayFast="0" \
	  --leakage="0" \
	  --inverter-leakage="0" \
	  --threshCC="$(THRESH_CC)" --threshTau="$(THRESH_TAU)" --threshPercent="$(THRESH_PERCENT)" \
	  --maxNodePerAlintBin="$(MAX_NODE_PER_ALINT_BIN)" \
	  --maxAlintBinNum="$(MAX_ALINT_BIN_NUM)" \
	  --alint-incr="$(ALINT_INCR)" \
	  --alint-in-org='$$@.org' \
	  --alintBin=$(1) \
	  --alintPOChar=1 \
	  --static-only="$(LIB_STATIC_ONLY)" \
	  '$$<' '$$@.tmp' ; \
	mv -f '$$@.tmp' '$$@'; \
	$$(CASTFILES_DEQUEUE_TASK)
endef

$(foreach a_num, $(ALINT_BIN_ENUM), $(eval $(call ALINT_PO_IN_TEMP,$(a_num))))


define COMMON_ASP
	(echo "/* auto-generated by lve */"; \
	echo ".true=$$true;"; \
	if [[ $(call GET_SPICE_TYPE,$(@D)) == accurate ]] ; then \
	  echo ".accurate=1;"; \
	else \
	  echo ".accurate=0;"; \
	fi; \
	echo ".temperature=$$temp;"; \
	echo ".corner \"$$corner\";"; \
	if [[ "$$corner" == "ta" ]] ; then \
	  echo ".damosn=60e-9*120e-9*0.19;"; \
	  echo ".damosp=60e-9*120e-9*0.127;"; \
	else \
	  echo ".damosn=0;"; \
	  echo ".damosp=0;"; \
	fi;) > '$@.tmp';
endef

.PRECIOUS: $(SPICE_DIR)/alint/%/alint_PO.asp
$(SPICE_DIR)/alint/%/alint_PO.asp:
	mkdir -p '$(@D)' 
	$(CASTFILES_ENQUEUE_TASK) && \
	temp=$(subst C,,$(call GET_NTH_RUN_PARAM,$(@D),$(DEFAULT_ALINT_PARAMS),4)); \
	true=$(subst V,,$(call GET_NTH_RUN_PARAM,$(@D),$(DEFAULT_ALINT_PARAMS),3)); \
	corner=$(call GET_CORNER,$(call GET_NTH_RUN_PARAM,$(@D),$(DEFAULT_ALINT_PARAMS),2)); \
	$(COMMON_ASP) \
	echo ".include \"$(ALINT_INCLUDE)\";" >> '$@.tmp'; \
	echo ".max_bump_fanin_aggressors=$(MAX_BUMP_FANIN);" >> '$@.tmp'; \
	echo ".max_delay_fanin_aggressors=$(MAX_DELAY_FANIN);" >> '$@.tmp'; \
	if [[ -s "$(ALINT_EXTRA_ASP)" ]] ; then \
	  cat "$(ALINT_EXTRA_ASP)" >> '$@.tmp'; \
	fi; \
	echo "wire(Vdd,VddA,VddX);" >> '$@.tmp'; \
	echo ".include \"noprs.asp\";" >> '$@.tmp'; \
	echo ".include \"cell.aspice\";" >> '$@.tmp'; \
	mv -f '$@.tmp' '$@'; \
	$(CASTFILES_DEQUEUE_TASK)

.PRECIOUS: $(SPICE_DIR)/alint/%/lib.asp
$(SPICE_DIR)/alint/%/lib.asp:
	mkdir -p '$(@D)' 
	$(CASTFILES_ENQUEUE_TASK) && \
	temp=$(subst C,,$(call GET_NTH_RUN_PARAM,$(@D),$(DEFAULT_ALINT_PARAMS),4)); \
	true=$(subst V,,$(call GET_NTH_RUN_PARAM,$(@D),$(DEFAULT_ALINT_PARAMS),3)); \
	corner=$(call GET_CORNER,$(call GET_NTH_RUN_PARAM,$(@D),$(DEFAULT_ALINT_PARAMS),2)); \
	$(COMMON_ASP) \
	echo ".include \"$(ALINT_INCLUDE)\";" >> '$@.tmp'; \
	echo ".max_bump_fanin_aggressors=$(MAX_BUMP_FANIN);" >> '$@.tmp'; \
	echo ".max_delay_fanin_aggressors=$(MAX_DELAY_FANIN);" >> '$@.tmp'; \
	if [[ -s "$(ALINT_EXTRA_ASP)" ]] ; then \
	  cat "$(ALINT_EXTRA_ASP)" >> '$@.tmp'; \
	fi; \
	echo "wire(Vdd,VddA,VddX);" >> '$@.tmp'; \
	echo ".include \"noprs.asp\";" >> '$@.tmp'; \
	echo ".include \"cell.aspice\";" >> '$@.tmp'; \
	mv -f '$@.tmp' '$@'; \
	$(CASTFILES_DEQUEUE_TASK)

.PRECIOUS: $(SPICE_DIR)/%/lib.done
$(SPICE_DIR)/%/lib.done: $(foreach a_num, $(ALINT_BIN_ENUM), $(SPICE_DIR)/%/lib_parallel/lib.done.$(a_num))
	    $(MERGE_ALINT_OUT) --num-of-bins=$(MAX_ALINT_BIN_NUM) --alint-dir='$(@D)' --mode=lib
	    cat '$(@D)/lib_parallel/lib.err'.* >  '$(@D)/lib.err'
	    touch '$@'

.PRECIOUS: $(foreach a_num, $(ALINT_BIN_ENUM), $(SPICE_DIR)/%/lib_parallel/lib.done.$(a_num))
define LIB_DONE_TEMP
.PRECIOUS: $(SPICE_DIR)/%/lib_parallel/lib.raw.$(1)
ifeq ("$(strip $(ENV))","default")
ifneq ("$(NOEXTRACTDEPS)", "1")
$(SPICE_DIR)/%/lib_parallel/lib.done.$(1) $(SPICE_DIR)/%/lib_parallel/lib.raw.$(1): \
	$(CELL_DIR)/cell.portprops \
	$(SPICE_DIR)/extract.result \
	$(CELL_DIR)/cell.nodeprops$(ROUTED_SUFFIX)$(ACCURATE_SUFFIX) \
	$(SPICE_DIR)/cell.aspice \
	$(SPICE_DIR)/%/lib_parallel/lib.in.$(1) \
	$(SPICE_DIR)/%/lib.asp \
	$(CELL_DIR)/jflat$(ROUTED_SUFFIX)/aspice/$(ENV)/noprs.asp
	task=lib && $$(CASTFILES_ENQUEUE_TASK) && \
	rm -rf '$$(@D)/*.png' '$$(@D)/*.trace' '$$(@D)/*.names' '$$(@D)/*.out' '$$(@D)/*.err'
	if [[ ( -n "$$(call LVE_SKIP,lib)" ) && ( -e '$$(@D)/lib.done.$(1)' ) && ( -e '$$(@D)/lib.raw.$(1)' ) ]] ; then exit; fi; \
	if ! ( grep -q PASS '$$(word 2, $$(^))'); then cp '$$(word 2, $$(^))' '$$(@D)/lib.done.$(1)'; exit; fi; \
	export ASPICE_CORNER=$$(call GET_CORNER,$$(call GET_NTH_RUN_PARAM,$$(@D),$$(DEFAULT_ALINT_PARAMS),2)) && \
	cd '$$(dir $$(@D))' && \
	  if [[ -s '$$(@D)/lib.in.$(1)' ]]; then \
	    echo '#TASK=lib CELL=$$(call GET_CAST_FULL_NAME,$$(@D)) BIN=$(1)' 1>&2 ; \
	    /bin/rm -f '$$(@D)/lib.out.$(1)' '$$(@D)/lib.err.$(1)' '$$(@D)/lib.done.$(1)'; \
	    /bin/rm -rf "$$(@D)/../lib.bin.$(1)"; \
	    mkdir -p "$$(@D)/../lib.bin.$(1)"; \
	    cd "$$(@D)/../lib.bin.$(1)"; \
	    QB_DIAG_FILE='$$@.diag' QB_RUN_NAME='lve_lib' \
	    $$(EXEC_PACKAGE) $$(ASPICE) $$(ASPICE_OPTIONS) \
	    -path '$$(call MERGE_WORDS,$$(ASPICE_PATH) $$(^D) $$(dir $$(@D)) $(ROOT_TARGET_DIR) $(SUB_LVE_ROOT_DIR),:)' \
	    -top '$$(call GET_CAST_CDL_NAME,$$(@D))' \
	    -seed $$(call GET_SEED,$$(call GET_NTH_RUN_PARAM,$$(@D),$$(DEFAULT_ALINT_PARAMS),2)) \
	    --clean-trace=$$(CLEAN_TRACE) \
	    --task=lib \
	    --local-props='$$<' \
      --node-props='$$(word 3, $$(^))' \
	    '$$(dir $$(@D))/lib' -include '$$(@D)/lib.in.$(1)' \
	    1>'$$(@D)/lib.out.$(1)' 2>'$$(@D)/lib.err.$(1)' ; \
	    touch '$$(@D)/lib.raw.$(1)'; \
	    if [[ -d "$$(@D)/../lib.bin.$(1)" ]]; then \
	       touch '$$(@D)/lib.done.$(1)'; \
	       /bin/rm -rf "$$(@D)/../lib.bin.$(1)/lib_parallel" ; \
	    else \
	       echo 'lib CELL=$$(call GET_CAST_FULL_NAME,$$(@D)) BIN=$(1) FAILED' 1>&2 ; \
	    fi; \
	  else \
	    /bin/rm -f '$$(@D)/lib.out.$(1)' '$$(@D)/lib.err.$(1)'; \
	    /bin/rm -rf "$$(@D)/../lib.bin.$(1)"; \
	    touch '$$(@D)/lib.out.$(1)'; \
	    touch '$$(@D)/lib.err.$(1)'; \
	    touch '$$(@D)/lib.done.$(1)'; \
	    touch '$$(@D)/lib.raw.$(1)'; \
	  fi; \
	task=lib && $$(CASTFILES_DEQUEUE_TASK)

else # "$(NOEXTRACTDEPS)" ne "1" 48 lines back
$(SPICE_DIR)/%/lib_parallel/lib.done.$(1) $(SPICE_DIR)/%/lib_parallel/lib.raw.$(1): \
	$(CELL_DIR)/cell.portprops \
	$(CELL_DIR)/cell.nodeprops$(ROUTED_SUFFIX)$(ACCURATE_SUFFIX) \
	$(SPICE_DIR)/cell.aspice \
	$(SPICE_DIR)/%/lib_parallel/lib.in.$(1) \
	$(SPICE_DIR)/%/lib.asp \
	$(CELL_DIR)/jflat$(ROUTED_SUFFIX)/aspice/$(ENV)/noprs.asp
	task=lib && $$(CASTFILES_ENQUEUE_TASK) && \
	rm -rf '$$(@D)/*.png' '$$(@D)/*.trace' '$$(@D)/*.names' '$$(@D)/*.out' '$$(@D)/*.err'
	if [[ ( -n "$$(call LVE_SKIP,lib)" ) && ( -e '$$(@D)/lib.done.$(1)' ) && ( -e '$$(@D)/lib.raw.$(1)' ) ]] ; then exit; fi; \
	export ASPICE_CORNER=$$(call GET_CORNER,$$(call GET_NTH_RUN_PARAM,$$(@D),$$(DEFAULT_ALINT_PARAMS),2)) && \
	cd '$$(dir $$(@D))' && \
	  if [[ -s '$$(@D)/lib.in.$(1)' ]]; then \
	    echo '#TASK=lib CELL=$$(call GET_CAST_FULL_NAME,$$(@D)) BIN=$(1)' 1>&2 ; \
	    /bin/rm -f '$$(@D)/lib.out.$(1)' '$$(@D)/lib.err.$(1)' '$$(@D)/lib.done.$(1)'; \
	    /bin/rm -rf "$$(@D)/../lib.bin.$(1)"; \
	    mkdir -p "$$(@D)/../lib.bin.$(1)"; \
	    cd "$$(@D)/../lib.bin.$(1)"; \
	    QB_DIAG_FILE='$$@.diag' QB_RUN_NAME='lve_lib' \
	    $$(EXEC_PACKAGE) $$(ASPICE) $$(ASPICE_OPTIONS) \
	    -path '$$(call MERGE_WORDS,$$(ASPICE_PATH) $$(^D) $$(dir $$(@D)) $(ROOT_TARGET_DIR) $(SUB_LVE_ROOT_DIR),:)' \
	    -top '$$(call GET_CAST_CDL_NAME,$$(@D))' \
	    -seed $$(call GET_SEED,$$(call GET_NTH_RUN_PARAM,$$(@D),$$(DEFAULT_ALINT_PARAMS),2)) \
	    --clean-trace=$$(CLEAN_TRACE) \
	    --task=lib \
	    --local-props='$$<' \
      --node-props='$$(word 2, $$(^))' \
	    '$$(dir $$(@D))/lib' -include '$$(@D)/lib.in.$(1)' \
	    1>'$$(@D)/lib.out.$(1)' 2>'$$(@D)/lib.err.$(1)' ; \
	    touch '$$(@D)/lib.raw.$(1)'; \
	    if [[ -d "$$(@D)/../lib.bin.$(1)" ]]; then \
	       touch '$$(@D)/lib.done.$(1)'; \
	       /bin/rm -rf "$$(@D)/../lib.bin.$(1)/lib_parallel" ; \
	    else \
	       echo 'lib CELL=$$(call GET_CAST_FULL_NAME,$$(@D)) BIN=$(1) FAILED' 1>&2 ; \
	    fi; \
	  else \
	    /bin/rm -f '$$(@D)/lib.out.$(1)' '$$(@D)/lib.err.$(1)'; \
	    /bin/rm -rf "$$(@D)/../lib.bin.$(1)"; \
	    touch '$$(@D)/lib.out.$(1)'; \
	    touch '$$(@D)/lib.err.$(1)'; \
	    touch '$$(@D)/lib.done.$(1)'; \
	    touch '$$(@D)/lib.raw.$(1)'; \
	  fi; \
	task=lib && $$(CASTFILES_DEQUEUE_TASK)

endif # "$(NOEXTRACTDEPS)" ne "1" 95 lines back
endif # "$(strip $(ENV))" eq "default" 97 lines back
endef

$(foreach a_num, $(ALINT_BIN_ENUM), $(eval $(call LIB_DONE_TEMP,$(a_num))))

# summary for lib aspice
.PRECIOUS: $(SPICE_DIR)/alint/%/lib.raw

define LIB_RAW_TEMP
.PRECIOUS: $(SPICE_DIR)/alint/%/lib_parallel/lib.raw.$(1)
ifeq ("$(strip $(ENV))","default")
# 'cat' at end of the 'if' needed for more prompt lib.raw results
$(SPICE_DIR)/alint/%/lib_parallel/lib.raw.$(1) : \
			$(CELL_DIR)/cell.portprops \
			$(SPICE_DIR)/alint/%/lib_parallel/lib.done.$(1) \
			$(SPICE_DIR)/alint/%/lib_parallel/lib.in.$(1) 
	if [ "$(1)" = "0" -a -f '$$(@D)/lib.in.$(1)' -a ! -s '$$(@D)/lib.in.$(1)' ]; then \
	    echo '#TASK=lib_raw CELL=$$(call GET_CAST_FULL_NAME,$$(@D)) BIN=$(1)' 1>&2 ; \
	    task=lib_raw$(1) && $$(CASTFILES_ENQUEUE_TASK) && \
	    task=lib_raw && $$(CASTFILES_DEQUEUE_TASK); \
	    echo "NA lib $$(call GET_CAST_FULL_NAME,$$(@D)) `echo '$$(@D)' | sed -e 's:/lib_parallel::' -e 's:$$(ROOT_TARGET_DIR)/::'`" > '$$(@D)/lib.raw.$(1)' && \
	    cat '$$(@D)/'lib.raw.* > '$$(@D)'/../lib.raw.tmp.$(1) ; \
	    mv -f '$$(@D)'/../lib.raw.tmp.$(1) '$$(@D)'/../lib.raw ; \
	elif [ -s '$$(@D)/lib.in.$(1)' ]; then \
	    echo '#TASK=lib_raw CELL=$$(call GET_CAST_FULL_NAME,$$(@D)) BIN=$(1)' 1>&2 ; \
	    task=lib_raw$(1) && $$(CASTFILES_ENQUEUE_TASK) && \
	    /bin/rm -f '$$(@D)/lib.raw.$(1)' '$$(@D)/cell.timing.$(1)'; \
	    QB_DIAG_FILE=/dev/null QB_RUN_NAME='lve_lib_raw$(1)' \
	    $$(EXEC_LOW_PACKAGE) $$(RAWIFY) --use-db=$$(USEDB) --task=lib \
	    --root='$$(ROOT_TARGET_DIR)' --dir='$$(@D)' \
	    --cell="$$(call GET_CAST_FULL_NAME,$$(@D))" \
	    --mode="$$(call GET_SPICE_TYPE,$$(@D))" \
	    --local-props='$$<' \
	    --alint-in='$$(@D)/lib.in.$(1)'  $$(LVE_RAW_CONFIG) ; \
	    task=lib_raw && $$(CASTFILES_DEQUEUE_TASK); \
	    cat '$$(@D)/'lib.raw.* > '$$(@D)'/../lib.raw.tmp.$(1) ; \
	    mv -f '$$(@D)'/../lib.raw.tmp.$(1) '$$(@D)'/../lib.raw ; \
	else \
	    /bin/rm -f '$$(@D)/lib.raw.$(1)' '$$(@D)/cell.timing.$(1)'; \
	    touch '$$(@D)/cell.timing.$(1)'; \
	    touch '$$(@D)/lib.raw.$(1)'; \
	    touch '$$(@D)/../lib.raw'; \
	fi

endif # "$(strip $(ENV))" eq "default" 34 lines back
endef

# NOTE: wire(Vdd,VddA,VddX) is a work around for BUG 6802
ifeq ("$(strip $(ENV))","default")
.PRECIOUS: $(SPICE_DIR)/alint/%/alint.asp
$(SPICE_DIR)/alint/%/alint.asp:
	mkdir -p '$(@D)' 
	if [[ -s "$(ALINT_ASP)" ]] ; then \
	$(CASTFILES_ENQUEUE_TASK) && \
	cp -f "$(ALINT_ASP)" '$@'; \
	$(CASTFILES_DEQUEUE_TASK) \
	else \
	$(CASTFILES_ENQUEUE_TASK) && \
	temp=$(subst C,,$(call GET_NTH_RUN_PARAM,$(@D),$(DEFAULT_ALINT_PARAMS),4)); \
	true=$(subst V,,$(call GET_NTH_RUN_PARAM,$(@D),$(DEFAULT_ALINT_PARAMS),3)); \
	corner=$(call GET_CORNER,$(call GET_NTH_RUN_PARAM,$(@D),$(DEFAULT_ALINT_PARAMS),2)); \
	$(COMMON_ASP) \
	echo ".include \"$(ALINT_INCLUDE)\";" >> '$@.tmp'; \
	echo ".max_bump_fanin_aggressors=$(MAX_BUMP_FANIN);" >> '$@.tmp'; \
	echo ".max_delay_fanin_aggressors=$(MAX_DELAY_FANIN);" >> '$@.tmp'; \
	if [[ -s "$(ALINT_EXTRA_ASP)" ]] ; then \
	  cat "$(ALINT_EXTRA_ASP)" >> '$@.tmp'; \
	fi; \
	echo "wire(Vdd,VddA,VddX);" >> '$@.tmp'; \
	echo ".include \"noprs.asp\";" >> '$@.tmp'; \
	echo ".include \"cell.aspice\";" >> '$@.tmp'; \
	mv -f '$@.tmp' '$@'; \
	$(CASTFILES_DEQUEUE_TASK) fi


# the below should ALWAYS be six levels up, but calculated just in case (AAG)
# originally, this was an absolute link, but that can cause problems when
# changing canonical paths as Intel is wont to do.
.PRECIOUS: $(SPICE_DIR)/alint/%/leaky.nodes
$(SPICE_DIR)/alint/%/leaky.nodes: $(CELL_DIR)/cell.leakynodes$(ROUTED_SUFFIX)
	mkdir -p '$(@D)'
	d1=`echo '$<D' | awk -F/ '{print NF}'`; \
	d2=`echo '$@D' | awk -F/ '{print NF}'`; \
	up=""; n=$$d1; while [ $$n -lt $$d2 ]; do up="$$up../"; n=`expr $$n + 1`; done ; \
	ln -sf "$${up}cell.leakynodes$(ROUTED_SUFFIX)" '$@'

.PRECIOUS: $(foreach a_num, $(ALINT_BIN_ENUM), $(SPICE_DIR)/%/alint_parallel/alint.in.$(a_num))

define ALINT_IN_TEMP
$(SPICE_DIR)/%/alint_parallel/alint.in.$(1) : $(CELL_DIR)/cell.nodeprops$(ROUTED_SUFFIX)$(ACCURATE_SUFFIX) $(CELL_DIR)/cell.scenarios$(ROUTED_SUFFIX)
	mkdir -p '$$(@D)'
	if [[ -s "$(ALINT_IN)" ]] ; then \
	  $$(CASTFILES_ENQUEUE_TASK) && \
	  cp -f "$(ALINT_IN)" '$$@'; \
	  $$(CASTFILES_DEQUEUE_TASK) \
	else \
	  $$(CASTFILES_ENQUEUE_TASK) && \
	  $$(MAKE_ALINT_IN) \
	    --dynamic-only=$(ALINT_DYNAMIC_ONLY) \
	    --bumpCC="$(BUMP_CC)"   --delayCC="$(DELAY_CC)" \
	    --bumpTau="$(BUMP_TAU)" --delayTau="$(DELAY_TAU)" \
	    --delayCap="$(DELAY_CAP)" \
	    --delayFast="$(DELAY_FAST)" \
	    --threshCC="$(THRESH_CC)" --threshTau="$(THRESH_TAU)" --threshPercent="$(THRESH_PERCENT)" \
	    --defaultMaxBumpFanin="$(MAX_BUMP_FANIN)" \
	    --leakage="$(DO_LEAKAGE)" \
	    --inverter-leakage="$(DO_INVERTER_LEAKAGE)" \
	    --maxNodePerAlintBin="$(MAX_NODE_PER_ALINT_BIN)" \
	    --maxAlintBinNum="$(MAX_ALINT_BIN_NUM)" \
	    --alint-incr="$(ALINT_INCR)" \
	    --alint-in-org='$$@.org' \
	    --alint-scenarios='$$(<D)/cell.scenarios$$(ROUTED_SUFFIX)' \
	    --alintBin='$(1)' \
	    --redo-nodes='$(1)' \
	    '$$<' '$$@.tmp' ; \
	  mv -f '$$@.tmp' '$$@'; \
	  $$(CASTFILES_DEQUEUE_TASK) fi
endef

ifneq ("$(REDONODES)", "")
$(eval $(call ALINT_IN_TEMP,$(REDONODES)))
else # "$(REDONODES)" ne ""
$(foreach a_num, $(ALINT_BIN_ENUM), $(eval $(call ALINT_IN_TEMP,$(a_num))))
endif # "$(REDONODES)" ne ""

.PRECIOUS: $(SPICE_DIR)/%/alint.in
$(SPICE_DIR)/%/alint.in: $(CELL_DIR)/cell.nodeprops$(ROUTED_SUFFIX)$(ACCURATE_SUFFIX) $(CELL_DIR)/cell.scenarios$(ROUTED_SUFFIX)
	$(CASTFILES_ENQUEUE_TASK) && \
	$(MAKE_ALINT_IN) \
	  --dynamic-only=$(ALINT_DYNAMIC_ONLY) \
	  --bumpCC="$(BUMP_CC)"   --delayCC="$(DELAY_CC)" \
	  --bumpTau="$(BUMP_TAU)" --delayTau="$(DELAY_TAU)" \
	  --delayCap="$(DELAY_CAP)" \
	  --delayFast="$(DELAY_FAST)" \
	  --leakage="$(DO_LEAKAGE)" \
	  --threshCC="$(THRESH_CC)" --threshTau="$(THRESH_TAU)" --threshPercent="$(THRESH_PERCENT)" \
	  --inverter-leakage="$(DO_INVERTER_LEAKAGE)" \
	  --maxNodePerAlintBin="$(MAX_NODE_PER_ALINT_BIN)" \
	  --maxAlintBinNum="$(MAX_ALINT_BIN_NUM)" \
	  --alint-scenarios='$(<D)/cell.scenarios$(ROUTED_SUFFIX)' \
	  --alintBin=-1 \
	  '$<' '$@.tmp' ; \
	mv -f '$@.tmp' '$@'; \
	$(CASTFILES_DEQUEUE_TASK)

.PRECIOUS: $(SPICE_DIR)/%/alint.done
$(SPICE_DIR)/%/alint.done: $(foreach a_num, $(ALINT_BIN_ENUM), $(SPICE_DIR)/%/alint_parallel/alint.done.$(a_num))
	    $(MERGE_ALINT_OUT) --num-of-bins=$(MAX_ALINT_BIN_NUM) --alint-dir='$(@D)'
	    cat '$(@D)/alint_parallel/alint.err'.* >  '$(@D)/alint.err'
	    touch '$@'

.PRECIOUS: $(foreach a_num, $(ALINT_BIN_ENUM), $(SPICE_DIR)/%/alint_parallel/alint.done.$(a_num))

define ALINT_DONE_TEMP
.PRECIOUS: $(SPICE_DIR)/%/alint_parallel/alint.in.$(1)
.PRECIOUS: $(SPICE_DIR)/%/alint_parallel/alint.done.$(1)
.PRECIOUS: $(SPICE_DIR)/%/alint_parallel/alint.raw.$(1)
ifneq ("$(NOEXTRACTDEPS)", "1")
$(SPICE_DIR)/%/alint_parallel/alint.done.$(1) $(SPICE_DIR)/%/alint_parallel/alint.raw.$(1) : \
	$(CELL_DIR)/cell.localprops$(ROUTED_SUFFIX)$(ACCURATE_SUFFIX) \
	$(CELL_DIR)/jflat$(ROUTED_SUFFIX)/fanout/$(ENV).latest \
	$(SPICE_DIR)/extract.result \
	$(SPICE_DIR)/cell.aspice \
	$(SPICE_DIR)/%/alint_parallel/alint.in.$(1) \
	$(SPICE_DIR)/%/leaky.nodes \
	$(SPICE_DIR)/%/alint.asp \
	$(SPICE_DIR)/%/threshresp.result \
	$(SPICE_DIR)/%/threshresp_PO.result \
	$(CELL_DIR)/jflat$(ROUTED_SUFFIX)/fanin/$(ENV).latest \
	$(CELL_DIR)/jflat$(ROUTED_SUFFIX)/aspice/$(ENV)/noprs.asp \
	$(CELL_DIR)/cell.nodeprops$(ROUTED_SUFFIX)$(ACCURATE_SUFFIX)
	task=alint_raw && $$(CASTFILES_ENQUEUE_TASK) && \
	if [[ ( -n "$$(call LVE_SKIP,alint_raw)" ) && ( -e '$$(@D)/alint.done.$(1)' ) && ( -e '$$(@D)/alint.raw.$(1)' ) ]] ; then exit; fi; \
	if ! ( grep -q PASS '$$(word 3, $$(^))'); then cp '$$(word 3, $$(^))' '$$(@D)/alint.done.$(1)'; touch '$$(@D)/alint.raw.$(1)'; exit; fi; \
	if ! ( grep -q SUCCESS '$$(word 8, $$(^))'); then cp '$$(word 8, $$(^))' '$$(@D)/alint.done.$(1)'; touch '$$(@D)/alint.raw.$(1)'; exit; fi; \
	if  ( grep -q FAIL '$$(word 9, $$(^))'); then cp '$$(word 9, $$(^))' '$$(@D)/alint.done.$(1)'; touch '$$(@D)/alint.raw.$(1)'; exit; fi; \
	cd '$$(dir $$(@D))' && \
	  if [[ -s '$$(@D)/alint.in.$(1)' ]]; then \
	    echo '#TASK=alint_raw CELL=$$(call GET_CAST_FULL_NAME,$$(@D)) BIN=$(1)' 1>&2 ; \
	    cd "$$(@D)/../alint.bin.$(1)" ; \
	    cell=$$$$(echo '$$(call GET_GDS2_CDL_NAME,$$(@D))' ); \
	    working_dir=`mktemp -d "$$(WORKING_DIR)/$$$$cell.alint.bin.$(1).XXXXXX"`; \
	    QB_DIAG_FILE='$$(@D)/alint.done.$(1).diag' QB_RUN_NAME='lve_alint' \
	    $$(EXEC_PACKAGE) $$(RAWIFY) --use-db=$$(USEDB) --task=alint \
	    --root='$$(ROOT_TARGET_DIR)' --dir='$$(@D)' \
	    --sub-lve-root-dir='$$(SUB_LVE_ROOT_DIR)' \
	    --cell="$$(call GET_CAST_FULL_NAME,$$(@D))" \
	    --mode="$$(call GET_SPICE_TYPE,$$(@D))" \
	    --view-mode-order="$$(VIEW_MODE_ORDER)" \
			--bump-source='$$(BUMP_SOURCE_PVT)' \
      --power-sag=$$(POWER_SAG) \
      --bump-debug=$$(BUMP_DEBUG) \
      --bump-nearest-end=$$(BUMP_NEAREST_END) \
      --bump-work-dir="$$$$working_dir" \
      --additive-resp=$$(ADDITIVE_RESP) \
      --alint-PO=$$(ALINT_PO) \
	    --local-props='$$<' \
	    --node-props='$$(<D)/cell.nodeprops$$(ROUTED_SUFFIX)$$(ACCURATE_SUFFIX)' \
	    --leaky-nodes='$$(<D)/cell.leakynodes$$(ROUTED_SUFFIX)' \
	    --fanout-nodes='$$(<D)/jflat$(ROUTED_SUFFIX)/fanout/$(ENV).latest' \
	    --fanin-nodes='$$(<D)/jflat$(ROUTED_SUFFIX)/fanin/$(ENV).latest' \
	    --jflat-query='$$(<D)/jflat$(ROUTED_SUFFIX)/query/$(ENV).latest' \
	    --alint-dynamic-only=$$(ALINT_DYNAMIC_ONLY) \
	    --lib-static-only=$$(LIB_STATIC_ONLY) \
	    --max-heap-size=$$(MAX_HEAP_SIZE) \
	    --alint-in='$$(@D)/alint.in.$(1)' \
	    --threshCC="$$(THRESH_CC)" --threshTau="$$(THRESH_TAU)" --threshPercent="$$(THRESH_PERCENT)" \
      --cast-path=$$(CAST_PATH) $$(LVE_RAW_CONFIG) \
	    1>'$$(@D)/alint.out.$(1)' 2>'$$(@D)/alint.err.$(1)' ; \
	    touch '$$(@D)/alint.done.$(1)'; \
	    if [ "$${DELETE_BUMPCHECK_DIR}" = "1" ]; then /bin/rm -rf "$$$${working_dir}" ; fi; \
	  else \
	    /bin/rm -f '$$(@D)/alint.out.$(1)' '$$(@D)/alint.err.$(1)'; \
	    touch '$$(@D)/alint.out.$(1)'; \
	    touch '$$(@D)/alint.err.$(1)'; \
	    touch '$$(@D)/alint.done.$(1)'; \
	    touch '$$(@D)/alint.raw.$(1)'; \
	  fi; \
	task=alint_raw && $$(CASTFILES_DEQUEUE_TASK)
else # "$(NOEXTRACTDEPS)" ne "1" 54 lines back
$(SPICE_DIR)/%/alint_parallel/alint.done.$(1) $(SPICE_DIR)/%/alint_parallel/alint.raw.$(1) : \
	$(CELL_DIR)/cell.localprops$(ROUTED_SUFFIX)$(ACCURATE_SUFFIX) \
	$(CELL_DIR)/jflat$(ROUTED_SUFFIX)/fanout/$(ENV).latest \
	$(SPICE_DIR)/cell.aspice \
	$(SPICE_DIR)/%/alint_parallel/alint.in.$(1) \
	$(SPICE_DIR)/%/leaky.nodes \
	$(SPICE_DIR)/%/alint.asp \
	$(SPICE_DIR)/%/threshresp.result \
	$(SPICE_DIR)/%/threshresp_PO.result \
	$(CELL_DIR)/jflat$(ROUTED_SUFFIX)/fanin/$(ENV).latest \
	$(CELL_DIR)/jflat$(ROUTED_SUFFIX)/aspice/$(ENV)/noprs.asp \
	$(CELL_DIR)/cell.nodeprops$(ROUTED_SUFFIX)$(ACCURATE_SUFFIX)
	task=alint_raw && $$(CASTFILES_ENQUEUE_TASK) && \
	if [[ ( -n "$$(call LVE_SKIP,alint_raw)" ) && ( -e '$$(@D)/alint.done.$(1)' ) && ( -e '$$(@D)/alint.raw.$(1)' ) ]] ; then exit; fi; \
	if ! ( grep -q SUCCESS '$$(word 7, $$(^))'); then cp '$$(word 7, $$(^))' '$$(@D)/alint.done.$(1)'; touch '$$(@D)/alint.raw.$(1)';  exit; fi; \
	if  ( grep -q FAIL '$$(word 8, $$(^))'); then cp '$$(word 8, $$(^))' '$$(@D)/alint.done.$(1)'; touch '$$(@D)/alint.raw.$(1)';  exit; fi; \
	cd '$$(dir $$(@D))' && \
	  if [[ -s '$$(@D)/alint.in.$(1)' ]]; then \
	    echo '#TASK=alint_raw CELL=$$(call GET_CAST_FULL_NAME,$$(@D)) BIN=$(1)' 1>&2 ; \
	    cd "$$(@D)/../alint.bin.$(1)" ; \
	    cell=$$$$(echo '$$(call GET_GDS2_CDL_NAME,$$(@D))' ); \
	    working_dir=`mktemp -d "$$(WORKING_DIR)/$$$$cell.alint.bin.$(1).XXXXXX"`; \
	    QB_DIAG_FILE='$$(@D)/alint.done.$(1).diag' QB_RUN_NAME='lve_alint' \
	    $$(EXEC_PACKAGE) $$(RAWIFY) --use-db=$$(USEDB) --task=alint \
	    --root='$$(ROOT_TARGET_DIR)' --dir='$$(@D)' \
	    --sub-lve-root-dir='$$(SUB_LVE_ROOT_DIR)' \
	    --cell="$$(call GET_CAST_FULL_NAME,$$(@D))" \
	    --mode="$$(call GET_SPICE_TYPE,$$(@D))" \
	    --view-mode-order="$$(VIEW_MODE_ORDER)" \
			--bump-source='$$(BUMP_SOURCE_PVT)' \
      --power-sag=$$(POWER_SAG) \
      --bump-debug=$$(BUMP_DEBUG) \
      --bump-nearest-end=$$(BUMP_NEAREST_END) \
      --bump-work-dir="$$$$working_dir" \
      --additive-resp=$$(ADDITIVE_RESP) \
      --alint-PO=$$(ALINT_PO) \
	    --local-props='$$<' \
	    --node-props='$$(<D)/cell.nodeprops$$(ROUTED_SUFFIX)$$(ACCURATE_SUFFIX)' \
	    --leaky-nodes='$$(<D)/cell.leakynodes$$(ROUTED_SUFFIX)' \
	    --fanout-nodes='$$(<D)/jflat$(ROUTED_SUFFIX)/fanout/$(ENV).latest' \
	    --fanin-nodes='$$(<D)/jflat$(ROUTED_SUFFIX)/fanin/$(ENV).latest' \
	    --jflat-query='$$(<D)/jflat$(ROUTED_SUFFIX)/query/$(ENV).latest' \
	    --alint-dynamic-only=$$(ALINT_DYNAMIC_ONLY) \
	    --lib-static-only=$$(LIB_STATIC_ONLY) \
	    --max-heap-size=$$(MAX_HEAP_SIZE) \
	    --alint-in='$$(@D)/alint.in.$(1)' \
	    --threshCC="$$(THRESH_CC)" --threshTau="$$(THRESH_TAU)" --threshPercent="$$(THRESH_PERCENT)" \
      --cast-path=$$(CAST_PATH) $$(LVE_RAW_CONFIG) \
	    1>'$$(@D)/alint.out.$(1)' 2>'$$(@D)/alint.err.$(1)' ; \
	    touch '$$(@D)/alint.done.$(1)'; \
	    if [ "$${DELETE_BUMPCHECK_DIR}" = "1" ]; then /bin/rm -rf "$$$${working_dir}" ; fi; \
	  else \
	    /bin/rm -f '$$(@D)/alint.out.$(1)' '$$(@D)/alint.err.$(1)'; \
	    touch '$$(@D)/alint.out.$(1)'; \
	    touch '$$(@D)/alint.err.$(1)'; \
	    touch '$$(@D)/alint.done.$(1)'; \
	    touch '$$(@D)/alint.raw.$(1)'; \
	  fi; \
	task=alint_raw && $$(CASTFILES_DEQUEUE_TASK)
endif # "$(NOEXTRACTDEPS)" ne "1" 106 lines back
endef

ifneq ("$(REDONODES)", "")
.PRECIOUS: $(SPICE_DIR)/%/alint_parallel/alint.in.$(REDONODES)
.PRECIOUS: $(SPICE_DIR)/%/alint_parallel/alint.done.$(REDONODES)
.PRECIOUS: $(SPICE_DIR)/%/alint_parallel/alint.raw.$(REDONODES)
$(SPICE_DIR)/%/alint_parallel/alint.done.$(REDONODES) $(SPICE_DIR)/%/alint_parallel/alint.raw.$(REDONODES) : \
	$(CELL_DIR)/cell.localprops$(ROUTED_SUFFIX)$(ACCURATE_SUFFIX) \
	$(CELL_DIR)/jflat$(ROUTED_SUFFIX)/fanout/$(ENV).latest \
	$(SPICE_DIR)/extract.result \
	$(SPICE_DIR)/cell.aspice \
	$(SPICE_DIR)/%/alint_parallel/alint.in.$(REDONODES) \
	$(SPICE_DIR)/%/leaky.nodes \
	$(SPICE_DIR)/%/alint.asp \
	$(SPICE_DIR)/%/alint_parallel/threshresp.result.$(REDONODES) \
	$(SPICE_DIR)/%/threshresp_PO.result \
	$(CELL_DIR)/jflat$(ROUTED_SUFFIX)/fanin/$(ENV).latest \
	$(CELL_DIR)/jflat$(ROUTED_SUFFIX)/aspice/$(ENV)/noprs.asp \
	$(CELL_DIR)/cell.nodeprops$(ROUTED_SUFFIX)$(ACCURATE_SUFFIX)
	task=alint_raw && $(CASTFILES_ENQUEUE_TASK) && \
	if [[ ( -n "$(call LVE_SKIP,alint_raw)" ) && ( -e '$(@D)/alint.done.$(REDONODES)' ) && ( -e '$(@D)/alint.raw.$(REDONODES)' ) ]] ; then exit; fi; \
	if ! ( grep -q PASS '$(word 3, $(^))'); then cp '$(word 3, $(^))' '$(@D)/alint.done.$(REDONODES)'; touch '$(@D)/alint.raw.$(REDONODES)'; exit; fi; \
	if ! ( grep -q SUCCESS '$(word 8, $(^))'); then cp '$(word 8, $(^))' '$(@D)/alint.done.$(REDONODES)'; touch '$(@D)/alint.raw.$(REDONODES)'; exit; fi; \
	if  ( grep -q FAIL '$(word 9, $(^))'); then cp '$(word 9, $(^))' '$(@D)/alint.done.$(REDONODES)'; touch '$(@D)/alint.raw.$(REDONODES)'; exit; fi; \
	cd '$(dir $(@D))' && \
	  if [[ -s '$(@D)/alint.in.$(REDONODES)' ]]; then \
	    echo '#TASK=alint_raw CELL=$(call GET_CAST_FULL_NAME,$(@D)) BIN=$(REDONODES)' 1>&2 ; \
	    cd "$(@D)/../alint.bin.$(REDONODES)" ; \
	    cell=$$(echo '$(call GET_GDS2_CDL_NAME,$(@D))' ); \
	    working_dir=`mktemp -d "$(WORKING_DIR)/$$cell.alint.bin.$(REDONODES).XXXXXX"`; \
	    QB_DIAG_FILE='$(@D)/alint.done.$(REDONODES).diag' QB_RUN_NAME='lve_alint' \
	    $(EXEC_PACKAGE) $(RAWIFY) --use-db=$(USEDB) --task=alint \
	    --root='$(ROOT_TARGET_DIR)' --dir='$(@D)' \
	    --sub-lve-root-dir='$(SUB_LVE_ROOT_DIR)' \
	    --cell="$(call GET_CAST_FULL_NAME,$(@D))" \
	    --mode="$(call GET_SPICE_TYPE,$(@D))" \
	    --view-mode-order="$(VIEW_MODE_ORDER)" \
			--bump-source='$(BUMP_SOURCE_PVT)' \
      --power-sag=$(POWER_SAG) \
      --bump-debug=$(BUMP_DEBUG) \
      --bump-nearest-end=$(BUMP_NEAREST_END) \
      --bump-work-dir="$$working_dir" \
      --additive-resp=$(ADDITIVE_RESP) \
      --alint-PO=$(ALINT_PO) \
	    --local-props='$<' \
	    --node-props='$(<D)/cell.nodeprops$(ROUTED_SUFFIX)$(ACCURATE_SUFFIX)' \
	    --leaky-nodes='$(<D)/cell.leakynodes$(ROUTED_SUFFIX)' \
			--fanout-nodes='$(<D)/jflat$(ROUTED_SUFFIX)/fanout/$(ENV).latest' \
			--fanin-nodes='$(<D)/jflat$(ROUTED_SUFFIX)/fanin/$(ENV).latest' \
	    --jflat-query='$(<D)/jflat$(ROUTED_SUFFIX)/query/$(ENV).latest' \
	    --alint-dynamic-only=$(ALINT_DYNAMIC_ONLY) \
	    --lib-static-only=$(LIB_STATIC_ONLY) \
	    --max-heap-size=$(MAX_HEAP_SIZE) \
	    --alint-in='$(@D)/alint.in.$(REDONODES)' \
	    --threshCC="$(THRESH_CC)" --threshTau="$(THRESH_TAU)" --threshPercent="$(THRESH_PERCENT)" \
      --cast-path=$(CAST_PATH) $(LVE_RAW_CONFIG) \
	    1>'$(@D)/alint.out.$(REDONODES)' 2>'$(@D)/alint.err.$(REDONODES)' ; \
	    touch '$(@D)/alint.done.$($REDONODES)'; \
	    if [ "${DELETE_BUMPCHECK_DIR}" = "1" ]; then /bin/rm -rf "$${working_dir}" ; fi; \
	  else \
	    /bin/rm -f '$(@D)/alint.out.$(REDONODES)' '$(@D)/alint.err.$(REDONODES)'; \
	    touch '$(@D)/alint.out.$(REDONODES)'; \
	    touch '$(@D)/alint.err.$(REDONODES)'; \
	    touch '$(@D)/alint.done.$(REDONODES)'; \
	    touch '$(@D)/alint.raw.$(REDONODES)'; \
	  fi; \
	task=alint_raw && $(CASTFILES_DEQUEUE_TASK)
else # "$(REDONODES)" ne "" 57 lines back
$(foreach a_num, $(ALINT_BIN_ENUM), $(eval $(call ALINT_DONE_TEMP,$(a_num))))
endif # "$(REDONODES)" ne "" 59 lines back

# summary for alint aspice
.PRECIOUS: $(SPICE_DIR)/alint/%/alint.raw $(SPICE_DIR)/alint/%/directives.0
.PRECIOUS: $(SPICE_DIR)/alint/%/directives.cap

define ALINT_RAW_TEMP
# PRECIOUS not needed
.PRECIOUS: $(SPICE_DIR)/alint/%/alint_parallel/alint.raw.$(1)
# 'cat' at end of the 'if' needed for more prompt alint.raw results
$(SPICE_DIR)/alint/%/alint_parallel/alint.raw.$(1) : \
			$(CELL_DIR)/cell.localprops$(ROUTED_SUFFIX)$(ACCURATE_SUFFIX) \
			$(CELL_DIR)/cell.leakynodes$(ROUTED_SUFFIX) \
			$(SPICE_DIR)/alint/%/alint_parallel/alint.done.$(1) \
			$(SPICE_DIR)/alint/%/alint_parallel/alint.in.$(1) 
	if [ "$(1)" = "0" -a -f '$$(@D)/alint.in.$(1)' -a ! -s '$$(@D)/alint.in.$(1)' \
                -a ! -s '$$(<D)/cell.leakynodes$$(ROUTED_SUFFIX)' \
                -a ! -s '$$(<D)/cell.localprops$$(ROUTED_SUFFIX)' ]; then \
	    echo '#TASK=alint_raw CELL=$$(call GET_CAST_FULL_NAME,$$(@D)) BIN=$(1)' 1>&2 ; \
	    task=alint_raw$(1) && $$(CASTFILES_ENQUEUE_TASK) && \
	    task=alint_raw && $$(CASTFILES_DEQUEUE_TASK); \
	    echo "NA alint $$(call GET_CAST_FULL_NAME,$$(@D)) `echo '$$(@D)' | sed -e 's:/alint_parallel::' -e 's:$$(ROOT_TARGET_DIR)/::'`" > '$$(@D)/alint.raw.$(1)' && \
	    cat '$$(@D)/'alint.raw.* > '$$(@D)'/../alint.raw.tmp.$(1) ; \
	    mv -f '$$(@D)'/../alint.raw.tmp.$(1) '$$(@D)'/../alint.raw ; \
	elif [ -s '$$(@D)/alint.in.$(1)' ]; then \
	    echo '#TASK=alint_raw CELL=$$(call GET_CAST_FULL_NAME,$$(@D)) BIN=$(1)' 1>&2 ; \
	    task=alint_raw$(1) && $$(CASTFILES_ENQUEUE_TASK) && \
	    QB_DIAG_FILE=/dev/null QB_RUN_NAME='lve_alint_raw$(1)' \
	    $$(EXEC_LOW_PACKAGE) $$(RAWIFY) --use-db=$$(USEDB) --task=alint \
	    --root='$$(ROOT_TARGET_DIR)' --dir='$$(@D)' \
	    --sub-lve-root-dir='$$(SUB_LVE_ROOT_DIR)' \
	    --cell="$$(call GET_CAST_FULL_NAME,$$(@D))" \
	    --mode="$$(call GET_SPICE_TYPE,$$(@D))" \
	    --view-mode-order="$$(VIEW_MODE_ORDER)" \
      --additive-resp=$$(ADDITIVE_RESP) \
      --alint-PO=$$(ALINT_PO) \
	    --local-props='$$<' \
	    --leaky-nodes='$$(<D)/cell.leakynodes$$(ROUTED_SUFFIX)' \
	    --alint-dynamic-only=$$(ALINT_DYNAMIC_ONLY) \
	    --lib-static-only=$$(LIB_STATIC_ONLY) \
	    --max-heap-size=$$(MAX_HEAP_SIZE) \
	    --alint-in='$$(@D)/alint.in.$(1)' ; \
	    --threshCC="$$(THRESH_CC)" --threshTau="$$(THRESH_TAU)" --threshPercent="$$(THRESH_PERCENT)" \
      --cast-path=$$(CAST_PATH) $$(LVE_RAW_CONFIG) \
	    task=alint_raw && $$(CASTFILES_DEQUEUE_TASK); \
	    cat '$$(@D)/'alint.raw.* > '$$(@D)'/../alint.raw.tmp.$(1) ; \
	    mv -f '$$(@D)'/../alint.raw.tmp.$(1) '$$(@D)'/../alint.raw ; \
	else \
	    /bin/rm -f '$$(@D)/alint.raw.$(1)'; \
	    touch '$$(@D)/alint.raw.$(1)'; \
	fi

endef

.PRECIOUS: $(SPICE_DIR)/alint/%/plt.raw

$(SPICE_DIR)/alint/%/plt.raw : $(SPICE_DIR)/alint/%/directives.0 $(SPICE_DIR)/alint/%/directives.cap
	#TASK=pltgen1 CELL=$(call GET_CAST_FULL_NAME,$(@D))
	QB_DIAG_FILE='$@.diag' QB_RUN_NAME='lve_pltgen1' \
	temp=$(call GET_NTH_RUN_PARAM,$(@D),$(DEFAULT_ASPICE_PARAMS),4); \
	true=$(call GET_NTH_RUN_PARAM,$(@D),$(DEFAULT_ASPICE_PARAMS),3); \
	corner=$(call GET_NTH_RUN_PARAM,$(@D),$(DEFAULT_ASPICE_PARAMS),2); \
	view=$(call GET_NTH_FROM_LAST_DIR,$(@D),6); \
	mode=$(call GET_NTH_FROM_LAST_DIR,$(@D),5); \
	cvt="$$corner/$$true/$$temp"; \
	mkdir -p "$(ROOT_TARGET_DIR)/plt/$$cvt"; \
	viewmodes=($$view/$$mode $(VIEW_MODE_ORDER)); \
	for viewmode in $${viewmodes[@]}; do \
	    delays=($${delays[@]} $$viewmode/alint/$$cvt/directives.0); \
	    caps=($${caps[@]} $$viewmode/alint/$$cvt/directives.cap); \
	done; \
	ifs="$$IFS"; IFS=:; dirfile="$${delays[*]}|$${caps[*]}"; IFS="$$ifs"; \
	QB_DIAG_FILE='$@.diag' QB_RUN_NAME='lve_pltgen1' \
	$(EXEC_PACKAGE) $(GENERATEPLTSUBTYPES) \
           --cast-path='$(CAST_PATH)' \
           --prefix=plt \
           --dir-file="$$dirfile" \
           --output-dir="$(ROOT_TARGET_DIR)/plt/$$cvt" \
           --measure-dir="$(ROOT_TARGET_DIR):$(SUB_LVE_ROOT_DIR)" \
           --cell='$(call GET_CAST_FULL_NAME,$(@D))' \
           --top-only=1 2>&1 | grep -v '^Warning:' ; sync
	touch '$@'

# this is place holder for having the plt cast as a target
#.PRECIOUS: $(PLTDIR)/$(PLTPREFIX)/$(CELLDIR).cast
#$(PLTDIR)/$(PLTPREFIX)/$(CELLDIR).cast : $(SPICE_DIR)/alint/%/directives.0
#	#TASK=pltgen CELL=$(call GET_CAST_FULL_NAME,$(@D))
#	dirpath=gendirpath($<); \
#	$(GENERATEPLTSUBTYPES) \
#           --cast-path='$(CASTPATH)' \
#           --prefix=$(PLTPREFIX) \
#           --output-dir='$(PLTDIR)' \
#           --measure-dir=$(ROOTDIR) \
#           --dir-file='$$dirpath'  \
#           --cell='$(call GET_CAST_FULL_NAME,$(@D)' \
#           --top-only=1

ifneq ("$(REDONODES)", "")
$(SPICE_DIR)/alint/%/alint.raw $(SPICE_DIR)/alint/%/directives.0 $(SPICE_DIR)/alint/%/directives.cap:\
	         $(CELL_DIR)/jflat$(ROUTED_SUFFIX)/fanout/$(ENV).latest \
           $(CELL_DIR)/cell.portprops \
	         $(CELL_DIR)/jflat$(ROUTED_SUFFIX)/fanin/$(ENV).latest \
           $(SPICE_DIR)/alint/%/alint_parallel/alint.raw.$(REDONODES)
	dir="$(@D)" ; \
	for node in `echo "$(REDONODES)" | sed -e 's/:/ /g'`; do \
	  fgrep -v " node=$$node " "$$dir/alint.raw" > "$$dir/alint.raw.tmp"; \
	  fgrep " node=$$node " "$$dir/alint_parallel/alint.raw.$(REDONODES)" > "$$dir/alint_parallel/alint.raw.$(REDONODES).tmp"; \
	  sort "$$dir/alint.raw.tmp" "$$dir/alint_parallel/alint.raw.$(REDONODES).tmp" -o "$$dir/alint.raw"; \
	  /bin/rm -f "$$dir/alint_parallel/alint.raw.$(REDONODES).tmp"; \
	  /bin/rm -f "$$dir/alint.raw.tmp"; \
	  /bin/rm -f "$$dir/directives.0"; \
	  for bin in  $(ALINT_BIN_ENUM); do \
	    if [ -d "$$dir/alint.bin.$$bin/$$node" ]; then \
	      /bin/rm -rf "$$dir/alint.bin.$$bin/$$node"; \
	      rsync -a "$$dir/alint.bin.$(REDONODES)/$$node/" "$$dir/alint.bin.$$bin/$$node"; \
	    fi; \
	    touch "$$dir/directives.0"; \
	    if [ -d "$$dir/alint.bin.$$bin" ]; then gzip -cd "$$dir/alint.bin.$$bin/"*/directives.0.gz >> "$$dir/directives.0"; fi; \
	  done; \
	done; \
	/bin/rm -rf "$$dir/alint.bin.$(REDONODES)"; \
	/bin/touch -r "$$dir/alint_parallel/alint.raw.$(REDONODES)" "$$dir/alint_parallel/alint.raw.$(REDONODES).tmp"; \
	/bin/mv -f "$$dir/alint_parallel/alint.raw.$(REDONODES).tmp" "$$dir/alint_parallel/alint.raw.$(REDONODES)"; \
	mode='$(call GET_SPICE_TYPE,$(@D))'; \
	dir=measured_cap; \
	[[ "$$mode" == nogeometry || "$$mode" == estimated ]] && dir=estimated_cap; \
	sed -n 's/^PASS.* node=\([^ ]\+\).* cap=\([^ ]\+\).*/      '"$$dir"'(\1) = \2;/p' '$(@D)/alint.raw' > '$(@D)/directives.cap'; \
	clean=$(CLEAN_TRACE); \
	if [[ "$$clean" == 1 ]]; then \
	  if ! ( grep -q FAIL '$(@D)/alint.raw' ) ; then \
	    cellname=$$(echo '$(call GET_CAST_FULL_NAME,$(@D))'); \
	$(CASTFILES_ENQUEUE_TASK) && \
	deletetracefile \
        --cellName="$$cellname" \
        --fanoutfile='$<' \
        --portfile='$(word 2, $^)' \
        --alint-path='$(@D)' > '$(@D)/del.log';\
	$(CASTFILES_DEQUEUE_TASK); \
	  fi; \
	fi;

else # "$(REDONODES)" ne "" 44 lines back
# directives.0 is a side effect of lve_raw --task=alint
$(SPICE_DIR)/alint/%/alint.raw $(SPICE_DIR)/alint/%/directives.0 $(SPICE_DIR)/alint/%/directives.cap:\
	          $(CELL_DIR)/jflat$(ROUTED_SUFFIX)/fanout/$(ENV).latest \
            $(CELL_DIR)/cell.portprops \
	          $(CELL_DIR)/jflat$(ROUTED_SUFFIX)/fanin/$(ENV).latest \
            $(foreach a_num, $(ALINT_BIN_ENUM) , $(SPICE_DIR)/alint/%/alint_parallel/alint.raw.$(a_num))
	cat '$(@D)/alint_parallel/alint.raw'.* > '$(@D)/alint.raw';
	if [[ ! -s '$(@D)/alint.raw' ]]; then \
      echo 'PASS alint $(call GET_CAST_FULL_NAME,$(@D)) $(subst $(ROOT_TARGET_DIR)/,,$(@D))' > '$(@D)/alint.raw'; \
	fi; \
	$(foreach a_num, $(ALINT_BIN_ENUM) , /bin/touch -r '$(@D)/alint_parallel/alint.raw.$(a_num)' '$(@D)/alint_parallel/alint.raw.$(a_num).tmp' ; ) \
	$(foreach a_num, $(ALINT_BIN_ENUM) , /bin/mv -f '$(@D)/alint_parallel/alint.raw.$(a_num).tmp' '$(@D)/alint_parallel/alint.raw.$(a_num)' ; ) \
	if [[ "$(USEDB)" == 1 ]]; then raw2db --root='$(ROOT_TARGET_DIR)' '$(@D)/alint.raw'; fi
	( for bin in $(ALINT_BIN_ENUM); do \
	   infile="$(@D)/alint_parallel/alint.in.$$bin"; \
	   ncnt=0; fcnt=0; \
	   for node in `awk '/^alint / {print $$2}' "$$infile"`; do \
	      let ncnt=$$ncnt+1; \
	      if [[ -r "$(@D)/alint.bin.$$bin/$$node/directives.0.gz" ]]; then \
	         gzip -cd "$(@D)/alint.bin.$$bin/$$node/directives.0.gz";\
	         let fcnt=$$fcnt+1; \
	      fi;\
	     done;\
	     if [[ "$$fcnt" -lt "$$ncnt" ]]; then \
	       echo "Only $$fcnt of $$ncnt nodes from $$infile." 1>&2; \
	     fi; \
	    done; ) > '${@D}/directives.0'; \
	if [[ ! -s '$(@D)/directives.0' ]]; then \
	  for node in `awk '{print $$1}' "$(@D)/../../../../../../cell.localprops$(ROUTED_SUFFIX)$(ACCURATE_SUFFIX)"`; do \
	      if [[ -r "$(@D)/$$node/directives.0.gz" ]]; then gzip -cd "$(@D)/$$node/directives.0.gz"; fi; done > '${@D}/directives.0'; \
	fi; \
	mode='$(call GET_SPICE_TYPE,$(@D))'; \
	dir=measured_cap; \
	[[ "$$mode" == nogeometry || "$$mode" == estimated ]] && dir=estimated_cap; \
	sed -n 's/^PASS.* node=\([^ ]\+\).* cap=\([^ ]\+\).*/      '"$$dir"'(\1) = \2;/p' '$(@D)/alint.raw' > '$(@D)/directives.cap'; \
	cat '$(@D)/alint_parallel/alint.err'.* >  '$(@D)/alint.err'
	/bin/rm -f '$(@D)/alint_parallel/alint.err'.*
	clean=$(CLEAN_TRACE); \
	if [[ "$$clean" == 1 ]]; then \
	  if ! ( grep -q FAIL '$(@D)/alint.raw' ) ; then \
	    cellname=$$(echo '$(call GET_CAST_FULL_NAME,$(@D))'); \
	$(CASTFILES_ENQUEUE_TASK) && \
	deletetracefile \
        --cellName="$$cellname" \
        --fanoutfile='$<' \
        --portfile='$(word 2, $^)' \
        --alint-path='$(@D)' > '$(@D)/del.log';\
	$(CASTFILES_DEQUEUE_TASK); \
	  fi; \
	fi; \
	touch '$(@D)/'alint.done
endif # "$(REDONODES)" ne "" 93 lines back

#########################
# alint aspice
#########################
define ALINT_PO_ASPICE_DONE_TEMP
.PRECIOUS: $(SPICE_DIR)/%/alint_PO_parallel/threshresp.result.$(1)
ifeq ("$(strip $(ENV))","default")
ifneq ("$(NOEXTRACTDEPS)", "1")
$(SPICE_DIR)/%/alint_PO_parallel/alint_PO.done.$(1) $(SPICE_DIR)/%/alint_PO_parallel/threshresp.result.$(1): \
	$(CELL_DIR)/cell.portprops \
	$(SPICE_DIR)/extract.result \
	$(CELL_DIR)/cell.nodeprops$(ROUTED_SUFFIX)$(ACCURATE_SUFFIX) \
	$(SPICE_DIR)/cell.aspice \
	$(SPICE_DIR)/%/alint_PO_parallel/alint_PO.in.$(1) \
	$(SPICE_DIR)/%/alint_PO.asp \
	$(CELL_DIR)/jflat$(ROUTED_SUFFIX)/aspice/$(ENV)/noprs.asp
	task=alint_PO && $$(CASTFILES_ENQUEUE_TASK) && \
	rm -rf '$$(@D)/*.png' '$$(@D)/*.trace' '$$(@D)/*.names' '$$(@D)/*.out' '$$(@D)/*.err'
	if [[ ( -n "$$(call LVE_SKIP,alint_PO)" ) && ( -e '$$(@D)/alint_PO.done.$(1)' ) ]] ; then exit; fi; \
	if ! ( grep -q PASS '$$(word 2, $$(^))'); then cp '$$(word 2, $$(^))' '$$(@D)/alint_PO.done.$(1)'; cp '$$(word 2, $$(^))' '$$(@D)/threshresp.result.$(1)'; \
  exit; fi; \
	export ASPICE_CORNER=$$(call GET_CORNER,$$(call GET_NTH_RUN_PARAM,$$(@D),$$(DEFAULT_ALINT_PARAMS),2)) && \
	cd '$$(dir $$(@D))' && \
	  if [[ -s '$$(@D)/alint_PO.in.$(1)' ]]; then \
	    echo '#TASK=alint_PO CELL=$$(call GET_CAST_FULL_NAME,$$(@D)) BIN=$(1)' 1>&2 ; \
	    /bin/rm -f '$$(@D)/alint_PO.out.$(1)' '$$(@D)/alint_PO.err.$(1)' '$$(@D)/alint_PO.done.$(1)'; \
	    if [[ "$(ALINT_INCR)" == 0 ]]; then \
	    /bin/rm -rf "$$(@D)/../alint_PO.bin.$(1)"; \
      fi; \
	    mkdir -p "$$(@D)/../alint_PO.bin.$(1)"; \
	    cd "$$(@D)/../alint_PO.bin.$(1)"; \
	    QB_DIAG_FILE='$$@.diag' QB_RUN_NAME='lve_alint_PO' \
	    $$(EXEC_PACKAGE) $$(ASPICE) $$(ASPICE_OPTIONS) \
	    -path '$$(call MERGE_WORDS,$$(ASPICE_PATH) $$(^D) $$(dir $$(@D)) $(ROOT_TARGET_DIR) $(SUB_LVE_ROOT_DIR),:)' \
	    -top '$$(call GET_CAST_CDL_NAME,$$(@D))' \
	    -seed $$(call GET_SEED,$$(call GET_NTH_RUN_PARAM,$$(@D),$$(DEFAULT_ALINT_PARAMS),2)) \
	    --clean-trace=$$(CLEAN_TRACE) \
	    --alint-incr=$$(ALINT_INCR) \
	    --task=alint_PO \
	    --local-props='$$<' \
      --node-props='$$(word 3, $$(^))' \
	    '$$(dir $$(@D))/alint_PO' -include '$$(@D)/alint_PO.in.$(1)' \
	    1>'$$(@D)/alint_PO.out.$(1)' 2>'$$(@D)/alint_PO.err.$(1)' ; \
	    if [[ -d "$$(@D)/../alint_PO.bin.$(1)" ]]; then \
	       touch '$$(@D)/alint_PO.done.$(1)'; \
	       /bin/rm -rf "$$(@D)/../alint_PO.bin.$(1)/alint_PO_parallel" ; \
	    else \
	       echo 'alint_PO CELL=$$(call GET_CAST_FULL_NAME,$$(@D)) BIN=$(1) FAILED' 1>&2 ; \
	    fi; \
	  else \
	    /bin/rm -f '$$(@D)/alint_PO.out.$(1)' '$$(@D)/alint_PO.err.$(1)' '$$(@D)/alint_PO.in.$(1).org' '$$(@D)/threshresp.result.$(1)'; \
	    /bin/rm -rf "$$(@D)/../alint_PO.bin.$(1)"; \
	    touch '$$(@D)/threshresp.result.$(1)'; \
	    touch '$$(@D)/alint_PO.out.$(1)'; \
	    touch '$$(@D)/alint_PO.err.$(1)'; \
	    touch '$$(@D)/alint_PO.done.$(1)'; \
	  fi; \
	task=alint_PO && $$(CASTFILES_DEQUEUE_TASK)

else # "$(NOEXTRACTDEPS)" ne "1" 48 lines back
.PRECIOUS: $(SPICE_DIR)/%/alint_PO_parallel/threshresp.result.$(1)
$(SPICE_DIR)/%/alint_PO_parallel/alint_PO.done.$(1) $(SPICE_DIR)/%/alint_PO_parallel/threshresp.result.$(1): \
	$(CELL_DIR)/cell.portprops \
	$(CELL_DIR)/cell.nodeprops$(ROUTED_SUFFIX)$(ACCURATE_SUFFIX) \
	$(SPICE_DIR)/cell.aspice \
	$(SPICE_DIR)/%/alint_PO_parallel/alint_PO.in.$(1) \
	$(SPICE_DIR)/%/alint_PO.asp \
	$(CELL_DIR)/jflat$(ROUTED_SUFFIX)/aspice/$(ENV)/noprs.asp
	task=alint_PO && $$(CASTFILES_ENQUEUE_TASK) && \
	rm -rf '$$(@D)/*.png' '$$(@D)/*.trace' '$$(@D)/*.names' '$$(@D)/*.out' '$$(@D)/*.err'
	if [[ ( -n "$$(call LVE_SKIP,alint_PO)" ) && ( -e '$$(@D)/alint_PO.done.$(1)' ) ]] ; then exit; fi; \
	export ASPICE_CORNER=$$(call GET_CORNER,$$(call GET_NTH_RUN_PARAM,$$(@D),$$(DEFAULT_ALINT_PARAMS),2)) && \
	cd '$$(dir $$(@D))' && \
	  if [[ -s '$$(@D)/alint_PO.in.$(1)' ]]; then \
	    echo '#TASK=alint_PO CELL=$$(call GET_CAST_FULL_NAME,$$(@D)) BIN=$(1)' 1>&2 ; \
	    /bin/rm -f '$$(@D)/alint_PO.out.$(1)' '$$(@D)/alint_PO.err.$(1)' '$$(@D)/alint_PO.done.$(1)'; \
	    if [[ "$(ALINT_INCR)" == 0 ]]; then \
	    /bin/rm -rf "$$(@D)/../alint_PO.bin.$(1)"; \
      fi; \
	    mkdir -p "$$(@D)/../alint_PO.bin.$(1)"; \
	    cd "$$(@D)/../alint_PO.bin.$(1)"; \
	    QB_DIAG_FILE='$$@.diag' QB_RUN_NAME='lve_alint_PO' \
	    $$(EXEC_PACKAGE) $$(ASPICE) $$(ASPICE_OPTIONS) \
	    -path '$$(call MERGE_WORDS,$$(ASPICE_PATH) $$(^D) $$(dir $$(@D)) $(ROOT_TARGET_DIR) $(SUB_LVE_ROOT_DIR),:)' \
	    -top '$$(call GET_CAST_CDL_NAME,$$(@D))' \
	    -seed $$(call GET_SEED,$$(call GET_NTH_RUN_PARAM,$$(@D),$$(DEFAULT_ALINT_PARAMS),2)) \
	    --clean-trace=$$(CLEAN_TRACE) \
	    --alint-incr=$$(ALINT_INCR) \
	    --task=alint_PO \
	    --local-props='$$<' \
      --node-props='$$(word 2, $$(^))' \
	    '$$(dir $$(@D))/alint_PO' -include '$$(@D)/alint_PO.in.$(1)' \
	    1>'$$(@D)/alint_PO.out.$(1)' 2>'$$(@D)/alint_PO.err.$(1)' ; \
	    if [[ -d "$$(@D)/../alint_PO.bin.$(1)" ]]; then \
	       touch '$$(@D)/alint_PO.done.$(1)'; \
	       /bin/rm -rf "$$(@D)/../alint_PO.bin.$(1)/alint_PO_parallel" ; \
	    else \
	       echo 'alint_PO CELL=$$(call GET_CAST_FULL_NAME,$$(@D)) BIN=$(1) FAILED' 1>&2 ; \
	    fi; \
	  else \
	    /bin/rm -f '$$(@D)/alint_PO.out.$(1)' '$$(@D)/alint_PO.err.$(1)' '$$(@D)/alint_PO.in.$(1).org' '$$(@D)/threshresp.result.$(1)'; \
	    /bin/rm -rf "$$(@D)/../alint_PO.bin.$(1)"; \
	    touch '$$(@D)/threshresp.result.$(1)'; \
	    touch '$$(@D)/alint_PO.out.$(1)'; \
	    touch '$$(@D)/alint_PO.err.$(1)'; \
	    touch '$$(@D)/alint_PO.done.$(1)'; \
	  fi; \
	task=alint_PO && $$(CASTFILES_DEQUEUE_TASK)

endif # "$(NOEXTRACTDEPS)" ne "1" 94 lines back
endif # "$(strip $(ENV))" eq "default" 96 lines back
endef

$(foreach a_num, $(ALINT_BIN_ENUM), $(eval $(call ALINT_PO_ASPICE_DONE_TEMP,$(a_num))))

.PRECIOUS: $(SPICE_DIR)/%/threshresp_PO.result
ifneq ("$(ALINT_PO)", "0")
$(SPICE_DIR)/%/threshresp_PO.result: \
   $(foreach a_num, $(ALINT_BIN_ENUM) , $(SPICE_DIR)/%/alint_PO_parallel/threshresp.result.$(a_num))
	echo 'SUCCESS' > '$(@D)/threshresp_PO.result'; \
	dir="$(@D)" ; \
	for bin in $(ALINT_BIN_ENUM); do \
	  if ( grep -q 'FAIL' "$$dir/alint_PO_parallel/threshresp.result.$$bin"); \
      then echo 'FAIL' > "$$dir/threshresp_PO.result"; \
      break; \
    fi; \
	done
else # "$(ALINT_PO)" ne "0"
$(SPICE_DIR)/%/threshresp_PO.result:
	mkdir -p '$(@D)';
	echo 'IGNORE' > '$(@D)/threshresp_PO.result'
endif # "$(ALINT_PO)" ne "0"


define ALINT_ASPICE_DONE_TEMP
.PRECIOUS: $(SPICE_DIR)/%/alint_parallel/alint_aspice.done.$(1)
.PRECIOUS: $(SPICE_DIR)/%/alint_parallel/threshresp.result.$(1)
ifneq ("$(NOEXTRACTDEPS)", "1")
$(SPICE_DIR)/%/alint_parallel/alint_aspice.done.$(1) $(SPICE_DIR)/%/alint_parallel/threshresp.result.$(1) : \
	$(CELL_DIR)/cell.localprops$(ROUTED_SUFFIX)$(ACCURATE_SUFFIX) \
	$(SPICE_DIR)/extract.result \
	$(CELL_DIR)/cell.nodeprops$(ROUTED_SUFFIX)$(ACCURATE_SUFFIX) \
	$(SPICE_DIR)/cell.aspice \
	$(SPICE_DIR)/%/alint_parallel/alint.in.$(1) \
	$(SPICE_DIR)/%/leaky.nodes \
	$(SPICE_DIR)/%/alint.asp \
	$(CELL_DIR)/jflat$(ROUTED_SUFFIX)/aspice/$(ENV)/noprs.asp
	task=alint_aspice && $$(CASTFILES_ENQUEUE_TASK) && \
	rm -rf '$$(@D)/*.png' '$$(@D)/*.trace' '$$(@D)/*.names' '$$(@D)/*.out' '$$(@D)/*.err'
	if [[ ( -n "$$(call LVE_SKIP,alint_aspice)" ) && ( -e '$$(@D)/alint_aspice.done.$(1)' ) && ( -e '$$(@D)/threshresp.result.$(1)' ) ]] ; then exit; fi; \
	if ! ( grep -q PASS '$$(word 2, $$(^))'); then cp '$$(word 2, $$(^))' '$$(@D)/alint_aspice.done.$(1)'; cp '$$(word 2, $$(^))' '$$(@D)/threshresp.result.$(1)'; \
  exit; fi; \
	export ASPICE_CORNER=$$(call GET_CORNER,$$(call GET_NTH_RUN_PARAM,$$(@D),$$(DEFAULT_ALINT_PARAMS),2)) && \
	cd '$$(dir $$(@D))' && \
	  if [[ -s '$$(@D)/alint.in.$(1)' ]]; then \
	    echo '#TASK=alint_aspice CELL=$$(call GET_CAST_FULL_NAME,$$(@D)) BIN=$(1)' 1>&2 ; \
	    if [[ "$(ALINT_INCR)" == 0 ]]; then \
	    /bin/rm -rf "$$(@D)/../alint.bin.$(1)" ; \
      fi; \
	    mkdir -p "$$(@D)/../alint.bin.$(1)" ; \
	    cd "$$(@D)/../alint.bin.$(1)" ; \
	    QB_DIAG_FILE='$$(@D)/alint_aspice.done.$(1).diag' QB_RUN_NAME='lve_alint_aspice' \
	    $$(EXEC_PACKAGE) $$(ASPICE) $$(ASPICE_OPTIONS) \
	    -path '$$(call MERGE_WORDS,$$(ASPICE_PATH) $$(^D) $$(dir $$(@D)) $(ROOT_TARGET_DIR) $(SUB_LVE_ROOT_DIR),:)' \
	    -top '$$(call GET_CAST_CDL_NAME,$$(@D))' \
	    -seed $$(call GET_SEED,$$(call GET_NTH_RUN_PARAM,$$(@D),$$(DEFAULT_ALINT_PARAMS),2)) \
	    --clean-trace=$$(CLEAN_TRACE) \
	    --task=alint \
	    --alint-dynamic-only=$(ALINT_DYNAMIC_ONLY) \
	    --alint-incr=$(ALINT_INCR) \
	    --local-props='$$<' \
      --node-props='$$(word 3, $$(^))' \
	    --leaky-nodes='$$(<D)/cell.leakynodes$$(ROUTED_SUFFIX)' \
	    '$$(dir $$(@D))/alint' -include '$$(@D)/alint.in.$(1)' \
	    1>'$$(@D)/alint_aspice.out.$(1)' 2>'$$(@D)/alint_aspice.err.$(1)' ; \
	    touch '$$(@D)/threshresp.result.$(1)'; \
	    if [[ -d "$$(@D)/../alint.bin.$(1)" ]]; then \
	       touch '$$(@D)/alint_aspice.done.$(1)'; \
	       /bin/rm -rf "$$(@D)/../alint.bin.$(1)/alint_parallel" ; \
	    else \
	       echo 'alint_aspice CELL=$$(call GET_CAST_FULL_NAME,$$(@D)) BIN=$(1) FAILED' 1>&2 ; \
	    fi; \
	  else \
	    /bin/rm -f '$$(@D)/alint_aspice.out.$(1)' '$$(@D)/alint_aspice.err.$(1)' '$$(@D)/alint.in.$(1).org'; \
	    /bin/rm -rf "$$(@D)/../alint.bin.$(1)"; \
	    touch '$$(@D)/alint_aspice.out.$(1)'; \
	    touch '$$(@D)/alint_aspice.err.$(1)'; \
	    touch '$$(@D)/alint_aspice.done.$(1)'; \
	    touch '$$(@D)/threshresp.result.$(1)'; \
	  fi; \
	task=alint_aspice && $$(CASTFILES_DEQUEUE_TASK)
else # "$(NOEXTRACTDEPS)" ne "1" 50 lines back
$(SPICE_DIR)/%/alint_parallel/alint_aspice.done.$(1) $(SPICE_DIR)/%/alint_parallel/threshresp.result.$(1) : \
	$(CELL_DIR)/cell.localprops$(ROUTED_SUFFIX)$(ACCURATE_SUFFIX) \
	$(CELL_DIR)/cell.nodeprops$(ROUTED_SUFFIX)$(ACCURATE_SUFFIX) \
	$(SPICE_DIR)/cell.aspice \
	$(SPICE_DIR)/%/alint_parallel/alint.in.$(1) \
	$(SPICE_DIR)/%/leaky.nodes \
	$(SPICE_DIR)/%/alint.asp \
	$(CELL_DIR)/jflat$(ROUTED_SUFFIX)/aspice/$(ENV)/noprs.asp
	task=alint_aspice && $$(CASTFILES_ENQUEUE_TASK) && \
	rm -rf '$$(@D)/*.png' '$$(@D)/*.trace' '$$(@D)/*.names' '$$(@D)/*.out' '$$(@D)/*.err'
	if [[ ( -n "$$(call LVE_SKIP,alint_aspice)" ) && ( -e '$$(@D)/alint_aspice.done.$(1)' ) && ( -e '$$(@D)/threshresp.result.$(1)' ) ]] ; then exit; fi; \
	export ASPICE_CORNER=$$(call GET_CORNER,$$(call GET_NTH_RUN_PARAM,$$(@D),$$(DEFAULT_ALINT_PARAMS),2)) && \
	cd '$$(dir $$(@D))' && \
	  if [[ -s '$$(@D)/alint.in.$(1)' ]]; then \
	    echo '#TASK=alint_aspice CELL=$$(call GET_CAST_FULL_NAME,$$(@D)) BIN=$(1)' 1>&2 ; \
	    if [[ "$(ALINT_INCR)" == 0 ]]; then \
	    /bin/rm -rf "$$(@D)/../alint.bin.$(1)" ; \
      fi; \
	    mkdir -p "$$(@D)/../alint.bin.$(1)" ; \
	    cd "$$(@D)/../alint.bin.$(1)" ; \
	    QB_DIAG_FILE='$$(@D)/alint_aspice.done.$(1).diag' QB_RUN_NAME='lve_alint_aspice' \
	    $$(EXEC_PACKAGE) $$(ASPICE) $$(ASPICE_OPTIONS) \
	    -path '$$(call MERGE_WORDS,$$(ASPICE_PATH) $$(^D) $$(dir $$(@D)) $(ROOT_TARGET_DIR) $(SUB_LVE_ROOT_DIR),:)' \
	    -top '$$(call GET_CAST_CDL_NAME,$$(@D))' \
	    -seed $$(call GET_SEED,$$(call GET_NTH_RUN_PARAM,$$(@D),$$(DEFAULT_ALINT_PARAMS),2)) \
	    --clean-trace=$$(CLEAN_TRACE) \
	    --task=alint \
	    --alint-dynamic-only=$(ALINT_DYNAMIC_ONLY) \
	    --alint-incr=$(ALINT_INCR) \
	    --local-props='$$<' \
      --node-props='$$(word 2, $$(^))' \
	    --leaky-nodes='$$(<D)/cell.leakynodes$$(ROUTED_SUFFIX)' \
	    '$$(dir $$(@D))/alint' -include '$$(@D)/alint.in.$(1)' \
	    1>'$$(@D)/alint_aspice.out.$(1)' 2>'$$(@D)/alint_aspice.err.$(1)' ; \
	    touch '$$(@D)/threshresp.result.$(1)'; \
	    if [[ -d "$$(@D)/../alint.bin.$(1)" ]]; then \
	       touch '$$(@D)/alint_aspice.done.$(1)'; \
	       /bin/rm -rf "$$(@D)/../alint.bin.$(1)/alint_parallel" ; \
	    else \
	       echo 'alint_aspice CELL=$$(call GET_CAST_FULL_NAME,$$(@D)) BIN=$(1) FAILED' 1>&2 ; \
	    fi; \
	  else \
	    /bin/rm -f '$$(@D)/alint_aspice.out.$(1)' '$$(@D)/alint_aspice.err.$(1)' '$$(@D)/alint.in.$(1).org'; \
	    /bin/rm -rf "$$(@D)/../alint.bin.$(1)"; \
	    touch '$$(@D)/alint_aspice.out.$(1)'; \
	    touch '$$(@D)/alint_aspice.err.$(1)'; \
	    touch '$$(@D)/alint_aspice.done.$(1)'; \
	    touch '$$(@D)/threshresp.result.$(1)'; \
	  fi; \
	task=alint_aspice && $$(CASTFILES_DEQUEUE_TASK)
endif # "$(NOEXTRACTDEPS)" ne "1" 97 lines back
endef

ifneq ("$(REDONODES)", "")
.PRECIOUS: $(SPICE_DIR)/%/alint_parallel/alint.in.$(REDONODES)
.PRECIOUS: $(SPICE_DIR)/%/alint_parallel/alint_aspice.done.$(REDONODES)
.PRECIOUS: $(SPICE_DIR)/%/alint_parallel/threshresp.result.$(REDONODES)
$(SPICE_DIR)/%/alint_parallel/alint_aspice.done.$(REDONODES) $(SPICE_DIR)/%/alint_parallel/threshresp.result.$(REDONODES) : \
	$(CELL_DIR)/cell.localprops$(ROUTED_SUFFIX)$(ACCURATE_SUFFIX) \
	$(SPICE_DIR)/extract.result \
	$(CELL_DIR)/cell.nodeprops$(ROUTED_SUFFIX)$(ACCURATE_SUFFIX) \
	$(SPICE_DIR)/cell.aspice \
	$(SPICE_DIR)/%/alint_parallel/alint.in.$(REDONODES) \
	$(SPICE_DIR)/%/leaky.nodes \
	$(SPICE_DIR)/%/alint.asp \
	$(SPICE_DIR)/%/threshresp_PO.result \
	$(CELL_DIR)/jflat$(ROUTED_SUFFIX)/aspice/$(ENV)/noprs.asp
	task=alint_aspice && $(CASTFILES_ENQUEUE_TASK) && \
	rm -rf '$(@D)/*.png' '$(@D)/*.trace' '$(@D)/*.names' '$(@D)/*.out' '$(@D)/*.err'
	if [[ ( -n "$(call LVE_SKIP,alint_aspice)" ) && ( -e '$(@D)/alint_aspice.done.$(REDONODES)' ) && ( -e '$(@D)/threshresp.result.$(REDONODES)' ) ]] ; then exit; fi; \
	if ! ( grep -q PASS '$(word 2, $(^))'); then cp '$(word 2, $(^))' '$(@D)/alint_aspice.done.$(REDONODES)'; cp '$(word 2, $(^))' '$(@D)/threshresp.result.$(REDONODES)'; \
  exit; fi; \
	export ASPICE_CORNER=$(call GET_CORNER,$(call GET_NTH_RUN_PARAM,$(@D),$(DEFAULT_ALINT_PARAMS),2)) && \
	cd '$(dir $(@D))' && \
	  if [[ -s '$(@D)/alint.in.$(REDONODES)' ]]; then \
	    echo '#TASK=alint_aspice CELL=$(call GET_CAST_FULL_NAME,$(@D)) BIN=$(REDONODES)' 1>&2 ; \
	    /bin/rm -rf "$(@D)/../alint.bin.$(REDONODES)" ; \
	    mkdir -p "$(@D)/../alint.bin.$(REDONODES)" ; \
	    cd "$(@D)/../alint.bin.$(REDONODES)" ; \
	    QB_DIAG_FILE='$(@D)/alint_aspice.done.$(REDONODES).diag' QB_RUN_NAME='lve_alint_aspice' \
	    $(EXEC_PACKAGE) $(ASPICE) $(ASPICE_OPTIONS) \
	    -path '$(call MERGE_WORDS,$(ASPICE_PATH) $(^D) $(dir $(@D)) $(ROOT_TARGET_DIR) $(SUB_LVE_ROOT_DIR),:)' \
	    -top '$(call GET_CAST_CDL_NAME,$(@D))' \
	    -seed $(call GET_SEED,$(call GET_NTH_RUN_PARAM,$(@D),$(DEFAULT_ALINT_PARAMS),2)) \
	    --clean-trace=$(CLEAN_TRACE) \
	    --task=alint \
	    --alint-dynamic-only=$(ALINT_DYNAMIC_ONLY) \
	    --local-props='$<' \
      --node-props='$(word 3, $^)' \
	    --leaky-nodes='$(<D)/cell.leakynodes$(ROUTED_SUFFIX)' \
	    '$(dir $(@D))/alint' -include '$(@D)/alint.in.$(REDONODES)' \
	    1>'$(@D)/alint_aspice.out.$(REDONODES)' 2>'$(@D)/alint_aspice.err.$(REDONODES)' ; \
	    touch '$(@D)/treshresp.result.$(REDONODES)'; \
	    if [[ -d "$(@D)/../alint.bin.$(REDONODES)" ]]; then \
	       touch '$(@D)/alint_aspice.done.$(REDONODES)'; \
	       /bin/rm -rf "$(@D)/../alint.bin.$(REDONODES)/alint_parallel" ; \
	    else \
	       echo 'alint_aspice CELL=$(call GET_CAST_FULL_NAME,$(@D)) BIN=$(REDONODES) FAILED' 1>&2 ; \
	    fi; \
	  else \
	    /bin/rm -f '$(@D)/alint_aspice.out.$(REDONODES)' '$(@D)/alint_aspice.err.$(REDONODES)'; \
	    touch '$(@D)/alint_aspice.out.$(REDONODES)'; \
	    touch '$(@D)/alint_aspice.err.$(REDONODES)'; \
	    touch '$(@D)/alint_aspice.done.$(REDONODES)'; \
	    touch '$(@D)/treshresp.result.$(REDONODES)'; \
	  fi; \
	task=alint_aspice && $(CASTFILES_DEQUEUE_TASK)
else # "$(REDONODES)" ne "" 53 lines back
$(foreach a_num, $(ALINT_BIN_ENUM), $(eval $(call ALINT_ASPICE_DONE_TEMP,$(a_num))))
endif # "$(REDONODES)" ne "" 55 lines back


.PRECIOUS: $(SPICE_DIR)/alint/%/threshresp.result
ifneq ("$(REDONODES)", "")
$(SPICE_DIR)/alint/%/threshresp.result:\
           $(SPICE_DIR)/alint/%/alint_parallel/threshresp.result.$(REDONODES)
	dir="$(@D)"; \
	echo 'SUCCESS' > '$(@D)/threshresp.result'; \
	if ( grep -q 'FAIL' "$$dir/alint_parallel/threshresp.result.$(REDONODES)"); \
    then echo 'FAIL' > "$$dir/threshresp.result"; \
  fi;
else # "$(REDONODES)" ne ""
$(SPICE_DIR)/alint/%/threshresp.result:\
            $(foreach a_num, $(ALINT_BIN_ENUM) , $(SPICE_DIR)/alint/%/alint_parallel/threshresp.result.$(a_num))
	echo 'SUCCESS' > '$(@D)/threshresp.result'; \
	dir="$(@D)" ; isFail=0; \
	for bin in $(ALINT_BIN_ENUM); do \
	  if ( grep -q 'FAIL' "$$dir/alint_parallel/threshresp.result.$$bin"); \
      then echo 'FAIL' > "$$dir/threshresp.result"; \
      isFail=1; \
      break; \
    fi; \
	done; \
	if [[ $$isFail = '0' ]]; then \
	$(MERGE_ALINT_OUT) --postfix='_aspice' --num-of-bins=$(MAX_ALINT_BIN_NUM) --alint-dir='$(@D)'; \
	  cat '$(@D)/alint_parallel/alint_aspice.err'.* >  '$(@D)/alint_aspice.err'; \
	/bin/rm -f '$(@D)/alint_parallel/alint_aspice.err'.*; \
  fi;
	touch '$(@D)/'alint_aspice.done
endif # "$(REDONODES)" ne "" 26 lines back

#####################################
$(SPICE_DIR)/alint/%/lib.raw:\
            $(foreach a_num, $(ALINT_BIN_ENUM) , $(SPICE_DIR)/alint/%/lib_parallel/lib.raw.$(a_num))
	GLOBIGNORE='*.diag' && cat '$(@D)/lib_parallel/lib.raw'.* > '$(@D)/lib.raw'
	$(foreach a_num, $(ALINT_BIN_ENUM) , /bin/touch -r '$(@D)/lib_parallel/lib.raw.$(a_num)' '$(@D)/lib_parallel/lib.raw.$(a_num).tmp' ; ) \
	$(foreach a_num, $(ALINT_BIN_ENUM) , /bin/mv -f '$(@D)/lib_parallel/lib.raw.$(a_num).tmp' '$(@D)/lib_parallel/lib.raw.$(a_num)' ; ) \
	if [[ "$(USEDB)" == 1 ]]; then raw2db --root='$(ROOT_TARGET_DIR)' '$(@D)/lib.raw'; fi
	$(MERGE_ALINT_OUT) --num-of-bins=$(MAX_ALINT_BIN_NUM) --alint-dir='$(@D)' --mode=lib
	cat '$(@D)/lib_parallel/lib.err'.* >  '$(@D)/lib.err'
	/bin/rm -f '$(@D)/lib_parallel/lib.err'.*
	touch '$(@D)/'lib.done

$(SPICE_DIR)/alint/%/cell.timing : $(foreach a_num, $(ALINT_BIN_ENUM), $(SPICE_DIR)/alint/%/lib_parallel/lib.raw.$(a_num))
	sort -u "$(@D)"/lib_parallel/cell.timing.* > "$(@D)"/cell.timing

define DIRECTIVES_CAP
.PRECIOUS: $(SPICE_DIR)/alint/%/directives.$(1)f

$(SPICE_DIR)/alint/%/directives.$(1)f : $(SPICE_DIR)/alint/%/lib.raw
	( for infile in '$$(@D)/lib_parallel/lib.in'.*; do \
	   bin=`echo "$$$$infile" | sed -e 's/.*lib.in.//'`; \
	   for node in `awk '/^alint / {print $$$$2}' "$$$$infile"`; do \
	      if [[ -r "$$(@D)/lib.bin.$$$$bin/$$$$node/directives.$(1)f" ]]; then \
	         cat "$$(@D)/lib.bin.$$$$bin/$$$$node/directives.$(1)f";\
	      fi;\
	        done;\
	    done; ) > '$$(@D)/directives.$(1)f'; \
	if [[ ! -s '$$(@D)/directives.$(1)f' ]]; then \
	  for node in `awk '{print $$$$1}' "$$(@D)/../../../../../../cell.portprops" | sed -e 's/^[-+]//'`; do \
	      if [[ -r "$$(@D)/$$$$node/directives.$(1)f" ]]; then cat "$$(@D)/$$$$node/directives.$(1)f"; fi; done > '$${@D}/directives.$(1)f'; \
	fi; \

.PRECIOUS: $(SPICE_DIR)/alint/%/plt$(1)f.raw
$(SPICE_DIR)/alint/%/plt$(1)f.raw : $(SPICE_DIR)/alint/%/directives.$(1)f $(SPICE_DIR)/alint/%/plt.raw
	#TASK=pltgen$(1)f CELL=$$(call GET_CAST_FULL_NAME,$$(@D))
	temp=$$(call GET_NTH_RUN_PARAM,$$(@D),$$(DEFAULT_ASPICE_PARAMS),4); \
	true=$$(call GET_NTH_RUN_PARAM,$$(@D),$$(DEFAULT_ASPICE_PARAMS),3); \
	corner=$$(call GET_NTH_RUN_PARAM,$$(@D),$$(DEFAULT_ASPICE_PARAMS),2); \
	mkdir -p "$(ROOT_TARGET_DIR)/plt/$$$$corner/$$$$true/$$$$temp"; \
	QB_DIAG_FILE='$$@.diag' QB_RUN_NAME='lve_pltgen$(1)f' \
	$$(EXEC_PACKAGE) $$(GENERATEPLTSUBTYPES) \
           --cast-path="$$(CAST_PATH):$$(ROOT_TARGET_DIR)/plt/$$$$corner/$$$$true/$$$$temp" \
           --prefix=plt$(1)f \
           --dir-file='$$<' \
           --output-dir="$(ROOT_TARGET_DIR)/plt/$$$$corner/$$$$true/$$$$temp" \
           --measure-dir="$(ROOT_TARGET_DIR)" \
           --cell='$$(call GET_CAST_FULL_NAME,$$(@D))' \
           --parent='plt.$$(call GET_CAST_FULL_NAME,$$(@D))' \
           --top-only=1 2>&1 | grep -v '^Warning:' ; sync
	touch '$$@'

endef

$(foreach cap, $(DELAY_CAP_INTS), $(eval $(call DIRECTIVES_CAP,$(cap))))

$(SPICE_DIR)/alint/%/directives.done : $(foreach cap,$(DELAY_CAP_INTS),$(SPICE_DIR)/alint/%/directives.$(cap)f)
	touch '$@'

.PRECIOUS: $(SPICE_DIR)/alint/%/asta.raw

define ASTA_FOR_TAU

.PRECIOUS: $(SPICE_DIR)/alint/%/asta/$(1)/in
.PRECIOUS: $(SPICE_DIR)/alint/%/asta/$(1)/flat.violations

$(SPICE_DIR)/alint/%/asta/$(1)/in : $(SPICE_DIR)/alint/%/plt1f.raw $(SPICE_DIR)/alint/%/plt.raw
	mkdir -p '$$(@D)' && \
	$$(CASTFILES_ENQUEUE_TASK) && \
	$$(MAKE_ASTA_IN) --input-slew='$$(ASTA_INPUT_SLEW)' \
	                 --output-cap='$$(ASTA_OUTPUT_CAP)' \
	                 --walltime='$$(ASTA_WALLTIME)' \
	                 --tau='$(1)' \
	                 --output-dir='$$(@D)' \
	                 --cell='plt1f.$$(call GET_CAST_FULL_NAME,$$(@D))' && \
	$$(CASTFILES_DEQUEUE_TASK)

$(SPICE_DIR)/alint/%/asta/$(1)/flat.violations : $(SPICE_DIR)/alint/%/asta/$(1)/in
	#TASK=asta CELL=$$(call GET_CAST_FULL_NAME,$$(@D))
	temp=$$(call GET_NTH_RUN_PARAM,$$(@D),$$(DEFAULT_ASPICE_PARAMS),4); \
	true=$$(call GET_NTH_RUN_PARAM,$$(@D),$$(DEFAULT_ASPICE_PARAMS),3); \
	corner=$$(call GET_NTH_RUN_PARAM,$$(@D),$$(DEFAULT_ASPICE_PARAMS),2); \
	task=asta && $$(CASTFILES_ENQUEUE_TASK) && \
	QB_DIAG_FILE='$$@.diag' QB_RUN_NAME='lve_asta' \
	$$(EXEC_PACKAGE_ASTA) $$(JDSIM) --version \
           --cast-path="$$(CAST_PATH):$$(ROOT_TARGET_DIR)/plt/$$$$corner/$$$$true/$$$$temp" \
           --asta-threads=$$(ASTA_THREADS) \
           --no-readline --source='$$<' >'$$(@D)/out' 2>'$$(@D)/err' && \
	$$(CASTFILES_DEQUEUE_TASK)
endef

$(foreach tau, $(ASTA_TAU), $(eval $(call ASTA_FOR_TAU,$(tau))))

$(SPICE_DIR)/alint/%/asta.raw : $(foreach tau,$(ASTA_TAU),$(SPICE_DIR)/alint/%/asta/$(tau)/flat.violations)
	#TASK=asta_raw CELL=$(call GET_CAST_FULL_NAME,$(@D))
	task=asta_raw && $(CASTFILES_ENQUEUE_TASK) && \
	QB_DIAG_FILE=/dev/null QB_RUN_NAME='lve_asta_raw' \
	$(EXEC_LOW_PACKAGE) $(RAWIFY) --use-db=$(USEDB) --task=asta \
	    --root='$(ROOT_TARGET_DIR)' --dir='$(@D)' \
	    --cell="$(call GET_CAST_FULL_NAME,$(@D))" \
	    --mode="$(call GET_SPICE_TYPE,$(@D))" \
	    --asta-tau-bound=$(ASTA_TAU_BOUND) \
	    --asta-tau='$(ASTA_TAU)'  $(LVE_RAW_CONFIG) && \
	$(CASTFILES_DEQUEUE_TASK) && \
	touch '$@'

define SLINT
.PRECIOUS: $(SPICE_DIR)/alint/%/slint/in
.PRECIOUS: $(SPICE_DIR)/alint/%/slint/violations.all

$(SPICE_DIR)/alint/%/slint/in : $(SPICE_DIR)/alint/%/plt.raw
	mkdir -p '$$(@D)' && \
	$$(CASTFILES_ENQUEUE_TASK) && \
	$$(MAKE_SLINT_IN) --input-slew='$$(SLINT_INPUT_SLEW)' \
	                  --ocv-margin='$$(SLINT_OCV_MARGIN)' \
	                  --abs-margin='$$(SLINT_ABS_MARGIN)' \
	                  --output-dir='$$(@D)' \
	                  --cell='plt.$$(call GET_CAST_FULL_NAME,$$(@D))' && \
	$$(CASTFILES_DEQUEUE_TASK)

$(SPICE_DIR)/alint/%/slint/violations.all : $(SPICE_DIR)/alint/%/slint/in
	#TASK=slint CELL=$$(call GET_CAST_FULL_NAME,$$(@D))
	temp=$$(call GET_NTH_RUN_PARAM,$$(@D),$$(DEFAULT_ASPICE_PARAMS),4); \
	true=$$(call GET_NTH_RUN_PARAM,$$(@D),$$(DEFAULT_ASPICE_PARAMS),3); \
	corner=$$(call GET_NTH_RUN_PARAM,$$(@D),$$(DEFAULT_ASPICE_PARAMS),2); \
	task=slint && $$(CASTFILES_ENQUEUE_TASK) && \
	QB_DIAG_FILE='$$@.diag' QB_RUN_NAME='lve_slint' \
	$$(EXEC_PACKAGE) $$(JDSIM) --version \
           --cast-path="$$(CAST_PATH):$$(ROOT_TARGET_DIR)/plt/$$$$corner/$$$$true/$$$$temp" \
           --source='$$<' >'$$(@D)/out' 2>'$$(@D)/err' && \
	$$(CASTFILES_DEQUEUE_TASK)
endef
$(eval $(call SLINT))

$(SPICE_DIR)/alint/%/slint.raw : $(SPICE_DIR)/alint/%/slint/violations.all
	#TASK=slint_raw CELL=$(call GET_CAST_FULL_NAME,$(@D))
	task=slint_raw && $(CASTFILES_ENQUEUE_TASK) && \
	QB_DIAG_FILE=/dev/null QB_RUN_NAME='lve_slint_raw' \
	$(EXEC_LOW_PACKAGE) $(RAWIFY) --use-db=$(USEDB) --task=slint \
	    --root='$(ROOT_TARGET_DIR)' --dir='$(@D)' \
	    --cell="$(call GET_CAST_FULL_NAME,$(@D))" \
	    --mode="$(call GET_SPICE_TYPE,$(@D))"  $(LVE_RAW_CONFIG) && \
	$(CASTFILES_DEQUEUE_TASK) && \
	touch '$@'

endif # "$(strip $(ENV))" eq "default" 946 lines back

# ASPICE
.PRECIOUS: $(SPICE_DIR)/aspice/$(ENV)/%/aspice.asp
$(SPICE_DIR)/aspice/$(ENV)/%/aspice.asp: $(CELL_DIR)/cell.portprops
	mkdir -p '$(@D)' 
	if [[ -s "$(ASPICE_ASP)" ]] ; then \
	$(CASTFILES_ENQUEUE_TASK) && \
	cp -f "$(ASPICE_ASP)" '$@'; \
	$(CASTFILES_DEQUEUE_TASK) \
	else \
	$(CASTFILES_ENQUEUE_TASK) && \
	time=$(subst ns,e-9,$(call GET_NTH_RUN_PARAM,$(@D),$(DEFAULT_ASPICE_PARAMS),6)); \
	temp=$(subst C,,$(call GET_NTH_RUN_PARAM,$(@D),$(DEFAULT_ASPICE_PARAMS),5)); \
	true=$(subst V,,$(call GET_NTH_RUN_PARAM,$(@D),$(DEFAULT_ASPICE_PARAMS),4)); \
	corner=$(call GET_CORNER,$(call GET_NTH_RUN_PARAM,$(@D),$(DEFAULT_ASPICE_PARAMS),3)); \
	env="$(call GET_NTH_RUN_PARAM,$(@D),$(DEFAULT_ASPICE_PARAMS),2)"; \
	$(COMMON_ASP) \
        if [[ -n "$(ENV_CAP_LOAD)" ]] ; then \
           for node in `awk '/^+/ {print $$1}' '$<' | sed -e 's/+//'`; do \
               echo "cap (\"$$node\")($(ENV_CAP_LOAD));" >> '$@.tmp'; \
           done; \
        fi; \
	echo ".include \"$(ASPICE_INCLUDE)\";" >> '$@.tmp'; \
	echo ".prstau=$(PRS_TAU);" >> '$@.tmp'; \
	echo ".timemax=$$time;" >> '$@.tmp'; \
	if [[ -s "$(ASPICE_EXTRA_ASP)" ]] ; then \
	  cat "$(ASPICE_EXTRA_ASP)" >> '$@.tmp'; \
	fi; \
	echo ".include \"prs.asp\";" >> '$@.tmp'; \
	echo ".include \"cell.aspice\";" >> '$@.tmp'; \
	echo ".include \"env.asp\";" >> '$@.tmp'; \
	mv -f '$@.tmp' '$@'; \
	$(CASTFILES_DEQUEUE_TASK) fi

.PRECIOUS: $(SPICE_DIR)/aspice/$(ENV)/%/aspice.in
$(SPICE_DIR)/aspice/$(ENV)/%/aspice.in: $(CELL_DIR)/jflat$(ROUTED_SUFFIX)/env-ntpc/$(ENV).nodes	
	mkdir -p '$(@D)' 
	if [[ -s "$(ASPICE_IN)" ]] ; then \
	$(CASTFILES_ENQUEUE_TASK) && \
	cp -f "$(ASPICE_IN)" '$@'; \
	$(CASTFILES_DEQUEUE_TASK) \
	else \
	$(CASTFILES_ENQUEUE_TASK) && \
	min=$(word 1,$(subst _, ,$(call GET_NTH_RUN_PARAM,$(@D),$(DEFAULT_ASPICE_PARAMS),7))); \
	max=$(word 2,$(subst _, ,$(call GET_NTH_RUN_PARAM,$(@D),$(DEFAULT_ASPICE_PARAMS),7))); \
	time=$(subst ns,e-9,$(call GET_NTH_RUN_PARAM,$(@D),$(DEFAULT_ASPICE_PARAMS),6)); \
	$(MAKE_ASPICE_IN) "$$time" '$<' '$@' \
	  $(DIGITAL_TIME_UNIT) $(ASPICE_DIGITAL_TIME) "$$min" "$$max"; \
	$(CASTFILES_DEQUEUE_TASK) fi

.PRECIOUS: $(SPICE_DIR)/aspice/$(ENV)/%/aspice.done
$(SPICE_DIR)/aspice/$(ENV)/%/aspice.done: \
				   $(SPICE_DIR)/extract.result \
				   $(SPICE_DIR)/cell.aspice \
	                           $(SPICE_DIR)/aspice/$(ENV)/%/aspice.in \
			           $(SPICE_DIR)/aspice/$(ENV)/%/aspice.asp \
				   $(CELL_DIR)/jflat$(ROUTED_SUFFIX)/aspice/$(ENV)/env.asp \
                                   $(CELL_DIR)/jflat$(ROUTED_SUFFIX)/aspice/default/prs.asp
	#TASK=aspice ENV=$(call GET_NTH_RUN_PARAM,$(@D),$(DEFAULT_ASPICE_PARAMS),2) CELL=$(call GET_CAST_FULL_NAME,$(@D))
	task=aspice && $(CASTFILES_ENQUEUE_TASK) && \
	rm -rf '$(@D)/*.png' '$(@D)/*.trace' '$(@D)/*.names' '$(@D)/*.out' '$(@D)/*.err'
	if [[ ( -n "$(call LVE_SKIP,aspice)" ) && ( -e '$@' ) ]] ; then exit; fi; \
	if ! ( grep -q PASS '$<'); then cp '$<' '$@'; exit; fi; \
	export ASPICE_CORNER=$(call GET_CORNER,$(call GET_NTH_RUN_PARAM,$(@D),$(DEFAULT_ASPICE_PARAMS),3)) && \
	cd '$(@D)' && \
	  QB_DIAG_FILE='$@.diag' QB_RUN_NAME='lve_aspice' \
	  $(EXEC_PACKAGE) $(ASPICE) -minCC $(MINCC) $(ASPICE_OPTIONS) \
	  -path '$(call MERGE_WORDS,$(ASPICE_PATH) $(^D) $(ROOT_TARGET_DIR) $(SUB_LVE_ROOT_DIR),:)' \
	  -top '$(call GET_CAST_CDL_NAME,$(@D))' \
	  -seed $(call GET_SEED,$(call GET_NTH_RUN_PARAM,$(@D),$(DEFAULT_ASPICE_PARAMS),3)) \
	  aspice -include '$(@D)/aspice.in' \
	  1>'$(@D)/aspice.out' 2>'$(@D)/aspice.err'; \
	task=aspice && $(CASTFILES_DEQUEUE_TASK)


# summary for transient aspice
.PRECIOUS: $(SPICE_DIR)/aspice/$(ENV)/%/aspice.raw
$(SPICE_DIR)/aspice/$(ENV)/%/aspice.raw: $(CELL_DIR)/jflat$(ROUTED_SUFFIX)/env-ntpc/$(ENV) \
					 $(SPICE_DIR)/aspice/$(ENV)/%/aspice.done
	#TASK=aspice_raw ENV=$(call GET_NTH_RUN_PARAM,$(@D),$(DEFAULT_ASPICE_PARAMS),2) CELL=$(call GET_CAST_FULL_NAME,$(@D))
	task=aspice_raw && $(CASTFILES_ENQUEUE_TASK) && \
	QB_DIAG_FILE='$@.diag' QB_RUN_NAME='lve_raw_aspice' \
	$(EXEC_LOW_PACKAGE) $(RAWIFY) --use-db=$(USEDB) --task=aspice \
	--root='$(ROOT_TARGET_DIR)' --dir='$(@D)' \
	--cell="$(call GET_CAST_FULL_NAME,$(@D))" \
	--mode="$(call GET_SPICE_TYPE,$(@D))" \
	--env-ntpc='$<' $(LVE_RAW_CONFIG); \
	task=aspice_raw && $(CASTFILES_DEQUEUE_TASK)


# HSIM
ifeq ("$(HSIM_PRS_CAP)", "")
PRS_CAP :=
else # "$(HSIM_PRS_CAP)" eq ""
PRS_CAP := --prs-cap=$(HSIM_PRS_CAP)
endif # "$(HSIM_PRS_CAP)" eq ""
ifeq ("$(HSIM_PRS_DELAY)", "")
PRS_DELAY :=
else # "$(HSIM_PRS_DELAY)" eq ""
PRS_DELAY := --prs-delay=$(HSIM_PRS_DELAY)
endif # "$(HSIM_PRS_DELAY)" eq ""
ifeq ("$(ENV_CAP_LOAD)", "")
CAP_LOAD :=
else # "$(ENV_CAP_LOAD)" eq ""
CAP_LOAD := --cap-load=$(ENV_CAP_LOAD)
endif # "$(ENV_CAP_LOAD)" eq ""
ifeq ("$(HSIM_PRS_MIN_RES)", "")
PRS_MIN_RES :=
else # "$(HSIM_PRS_MIN_RES)" eq ""
PRS_MIN_RES := --prs-min-res=$(HSIM_PRS_MIN_RES)
endif # "$(HSIM_PRS_MIN_RES)" eq ""
ifeq ("$(HSIM_PRS_MAX_RES)", "")
PRS_MAX_RES :=
else # "$(HSIM_PRS_MAX_RES)" eq ""
PRS_MAX_RES := --prs-max-res=$(HSIM_PRS_MAX_RES)
endif # "$(HSIM_PRS_MAX_RES)" eq ""

.PRECIOUS: $(SPICE_DIR)/hsim/$(ENV)/%/hsim.out
.PRECIOUS: $(SPICE_DIR)/hsim/$(ENV)/%/hsim.fsdb
.PRECIOUS: $(SPICE_DIR)/hsim/$(ENV)/%/hsim.names
.PRECIOUS: $(SPICE_DIR)/hsim/$(ENV)/%/hsim.trace

$(SPICE_DIR)/hsim/$(ENV)/%/hsim.out: $(SPICE_DIR)/cell.spice_hsim \
			             $(CELL_DIR)/jflat$(ROUTED_SUFFIX)/hsim/$(ENV) \
			             $(CELL_DIR)/jflat$(ROUTED_SUFFIX)/env-ntpc/$(ENV)
	#TASK=hsim ENV=$(call GET_NTH_RUN_PARAM,$(@D),$(DEFAULT_SPICE_PARAMS),2) CELL=$(call GET_CAST_FULL_NAME,$(@D))
	mkdir -p '$(@D)'
	$(CASTFILES_ENQUEUE_TASK) && \
	working_dir=`mktemp -d "$(WORKING_DIR)/hsim.XXXXXX"`; \
	chmod 2775 "$$working_dir"; \
	time=$(subst ns,e-9,$(call GET_NTH_RUN_PARAM,$(@D),$(DEFAULT_SPICE_PARAMS),6)); \
	temp=$(subst C,,$(call GET_NTH_RUN_PARAM,$(@D),$(DEFAULT_SPICE_PARAMS),5)); \
	true=$(subst V,,$(call GET_NTH_RUN_PARAM,$(@D),$(DEFAULT_SPICE_PARAMS),4)); \
	corner=$(call GET_NTH_RUN_PARAM,$(@D),$(DEFAULT_SPICE_PARAMS),3); \
	env="$(call GET_NTH_RUN_PARAM,$(@D),$(DEFAULT_SPICE_PARAMS),2)"; \
	type=$(call GET_SPICE_TYPE,$(@D)); \
	reduce=$$([[ $$type == extracted ]] && echo 1 || echo 0); \
	nodes=$$($(GNUGAWK) '{print $$7}' '$(word 3,$^)' | sort -u | \
		$(RENAME) --type=node --from=cast --to=gds2 | \
		$(GNUGAWK) '{print "Xenv.Xtest." $$1}' | \
	        tr '\n' ',' | $(GNUSED) 's/,$$//' ); \
	QRSH_FLAGS="$(PACKAGE_FLAGS) -l hsim=1" \
	QB_DIAG_FILE='$@.diag' \
	QB_RUN_NAME='lve_hsim' \
	$(QB) $(HSIM) \
	  --run-directory="$$working_dir" \
	  --fulcrum-pdk-root=$(FULCRUM_PDK_PACKAGE_EXTRACT_ROOT) \
	  --cell-spice-file='$(word 1, $^)' \
	  --env-spice-file='$(word 2, $^)' \
	  --accurate=$(HSIM_ACCURATE) \
	  --hsim-vdd=$(HSIM_VDD) \
	  $(PRS_CAP) \
	  $(PRS_DELAY) \
	  $(PRS_MIN_RES) \
	  $(PRS_MAX_RES) \
	  --rc-reduction=$$reduce \
	  --minC=$(MINC) --minR=$(MINR) \
	  --run-time=$$time --process-corner=$$corner --vdd=$$true \
	  --measure-nodes="$$nodes" \
	  --delete=0 \
	  --lve-root-dir='$(ROOT_TARGET_DIR)' \
	  --sub-lve-root-dir='$(SUB_LVE_ROOT_DIR)' \
	  --output-dir='$(@D)' \
          --sigma-factor=$(SIGMA_FACTOR) \
	  --temp="$$temp" \
	"$(call GET_GDS2_CDL_NAME,$(@D))" "$(call GET_GDS2_CDL_NAME,$(@D))_U_env" &> '$(@D)/hsim.err' \
	&& if [ "${HSIM_DELETE}" = "1" ]; then /bin/rm -rf "$${working_dir}" ; fi;
	$(CASTFILES_DEQUEUE_TASK)

$(SPICE_DIR)/hsim/$(ENV)/%/hsim.names $(SPICE_DIR)/hsim/$(ENV)/%/hsim.trace : $(SPICE_DIR)/hsim/$(ENV)/%/hsim.out
	$(CASTFILES_ENQUEUE_TASK)
	(cd '$(@D)'; \
	 QB_DIAG_FILE='$@.diag' QB_RUN_NAME='lve_convert_trace' \
	 $(EXEC_LOW_PACKAGE) convert_trace --translate --fsdb hsim.fsdb hsim ;\
	)
	$(CASTFILES_DEQUEUE_TASK)

# 10485760
# summary for transient hsim
.PRECIOUS: $(SPICE_DIR)/hsim/%/hsim.raw
$(SPICE_DIR)/hsim/$(ENV)/%/hsim.raw: $(CELL_DIR)/jflat$(ROUTED_SUFFIX)/env-ntpc/$(ENV) \
				     $(SPICE_DIR)/hsim/$(ENV)/%/hsim.names
	#TASK=hsim_raw ENV=$(call GET_NTH_RUN_PARAM,$(@D),$(DEFAULT_SPICE_PARAMS),2) CELL=$(call GET_CAST_FULL_NAME,$(@D))
	$(CASTFILES_ENQUEUE_TASK) && \
	QB_DIAG_FILE='$@.diag' QB_RUN_NAME='lve_raw_hsim' \
	$(EXEC_LOW_PACKAGE) $(RAWIFY) --use-db=$(USEDB) --task=hsim \
	--root='$(ROOT_TARGET_DIR)' \
	--dir='$(@D)' \
	--cell="$(call GET_CAST_FULL_NAME,$(@D))" \
	--mode="$(call GET_SPICE_TYPE,$(@D))" \
	--env-ntpc='$<' $(LVE_RAW_CONFIG); \
	$(CASTFILES_DEQUEUE_TASK)

# XA (sadly, almost identical to HSIM, but is there no way to share the code?)
.PRECIOUS: $(SPICE_DIR)/xa/$(ENV)/%/xa.out
.PRECIOUS: $(SPICE_DIR)/xa/$(ENV)/%/xa.fsdb
.PRECIOUS: $(SPICE_DIR)/xa/$(ENV)/%/xa.names
.PRECIOUS: $(SPICE_DIR)/xa/$(ENV)/%/xa.trace

$(SPICE_DIR)/xa/$(ENV)/%/xa.out: $(SPICE_DIR)/cell.spice_hsim \
			             $(CELL_DIR)/jflat$(ROUTED_SUFFIX)/hsim/$(ENV) \
			             $(CELL_DIR)/jflat$(ROUTED_SUFFIX)/env-ntpc/$(ENV)
	#TASK=xa ENV=$(call GET_NTH_RUN_PARAM,$(@D),$(DEFAULT_SPICE_PARAMS),2) CELL=$(call GET_CAST_FULL_NAME,$(@D))
	mkdir -p '$(@D)'
	$(CASTFILES_ENQUEUE_TASK) && \
	working_dir=`mktemp -d "$(WORKING_DIR)/xa.XXXXXX"`; \
	chmod 2775 "$$working_dir"; \
	time=$(subst ns,e-9,$(call GET_NTH_RUN_PARAM,$(@D),$(DEFAULT_SPICE_PARAMS),6)); \
	temp=$(subst C,,$(call GET_NTH_RUN_PARAM,$(@D),$(DEFAULT_SPICE_PARAMS),5)); \
	true=$(subst V,,$(call GET_NTH_RUN_PARAM,$(@D),$(DEFAULT_SPICE_PARAMS),4)); \
	corner=$(call GET_NTH_RUN_PARAM,$(@D),$(DEFAULT_SPICE_PARAMS),3); \
	env="$(call GET_NTH_RUN_PARAM,$(@D),$(DEFAULT_SPICE_PARAMS),2)"; \
	type=$(call GET_SPICE_TYPE,$(@D)); \
	reduce=$$([[ $$type == extracted ]] && echo 1 || echo 0); \
	nodes=$$($(GNUGAWK) '{print $$7}' '$(word 3,$^)' | sort -u | \
		$(RENAME) --type=node --from=cast --to=gds2 | \
		$(GNUGAWK) '{print "Xenv.Xtest." $$1}' | \
	        tr '\n' ',' | $(GNUSED) 's/,$$//' ); \
	QRSH_FLAGS="$(PACKAGE_FLAGS) -l xa=1" \
	QB_DIAG_FILE='$@.diag' \
	QB_RUN_NAME='lve_xa' \
	$(QB) $(XA) \
	  --run-directory="$$working_dir" \
	  --fulcrum-pdk-root=$(FULCRUM_PDK_PACKAGE_EXTRACT_ROOT) \
	  --cell-spice-file='$(word 1, $^)' \
	  --env-spice-file='$(word 2, $^)' \
	  --accurate=$(HSIM_ACCURATE) \
	  --hsim-vdd=$(HSIM_VDD) \
	  $(PRS_CAP) \
	  $(PRS_DELAY) \
	  $(PRS_MIN_RES) \
	  $(PRS_MAX_RES) \
	  --rc-reduction=$$reduce \
	  --minC=$(MINC) --minR=$(MINR) \
	  --run-time=$$time --process-corner=$$corner --vdd=$$true \
	  --measure-nodes="$$nodes" \
	  --delete=0 \
	  --lve-root-dir='$(ROOT_TARGET_DIR)' \
	  --sub-lve-root-dir='$(SUB_LVE_ROOT_DIR)' \
	  --output-dir='$(@D)' \
          --sigma-factor=$(SIGMA_FACTOR) \
	  --temp="$$temp" \
	"$(call GET_GDS2_CDL_NAME,$(@D))" "$(call GET_GDS2_CDL_NAME,$(@D))_U_env" &> '$(@D)/xa.err' \
	&& if [ "${HSIM_DELETE}" = "1" ]; then /bin/rm -rf "$${working_dir}" ; fi;
	$(CASTFILES_DEQUEUE_TASK)

$(SPICE_DIR)/xa/$(ENV)/%/xa.names $(SPICE_DIR)/xa/$(ENV)/%/xa.trace : $(SPICE_DIR)/xa/$(ENV)/%/xa.out
	$(CASTFILES_ENQUEUE_TASK)
	(cd '$(@D)'; \
	 QB_DIAG_FILE='$@.diag' QB_RUN_NAME='lve_convert_trace' \
	 $(EXEC_LOW_PACKAGE) convert_trace --scale-time 1e-6 --translate --fsdb xa.fsdb xa ;\
	)
	$(CASTFILES_DEQUEUE_TASK)

# summary for transient xa
.PRECIOUS: $(SPICE_DIR)/xa/$(ENV)/%/xa.raw
$(SPICE_DIR)/xa/$(ENV)/%/xa.raw: $(CELL_DIR)/jflat$(ROUTED_SUFFIX)/env-ntpc/$(ENV) \
				     $(SPICE_DIR)/xa/$(ENV)/%/xa.names
	#TASK=xa_raw ENV=$(call GET_NTH_RUN_PARAM,$(@D),$(DEFAULT_SPICE_PARAMS),2) CELL=$(call GET_CAST_FULL_NAME,$(@D))
	$(CASTFILES_ENQUEUE_TASK) && \
	QB_DIAG_FILE='$@.diag' QB_RUN_NAME='lve_raw_xa' \
	$(EXEC_LOW_PACKAGE) $(RAWIFY) --use-db=$(USEDB) --task=xa \
	--root='$(ROOT_TARGET_DIR)' \
	--dir='$(@D)' \
	--cell="$(call GET_CAST_FULL_NAME,$(@D))" \
	--mode="$(call GET_SPICE_TYPE,$(@D))" \
	--env-ntpc='$<' $(LVE_RAW_CONFIG); \
	$(CASTFILES_DEQUEUE_TASK)

# HSPICE
.PRECIOUS: $(SPICE_DIR)/hspice/$(ENV)/%/hspice.out
.PRECIOUS: $(SPICE_DIR)/hspice/$(ENV)/%/hspice.csdf
.PRECIOUS: $(SPICE_DIR)/hspice/$(ENV)/%/hspice.names
.PRECIOUS: $(SPICE_DIR)/hspice/$(ENV)/%/hspice.trace

$(SPICE_DIR)/hspice/$(ENV)/%/hspice.out: $(SPICE_DIR)/cell.spice_hsim \
			             $(CELL_DIR)/jflat$(ROUTED_SUFFIX)/hsim/$(ENV) \
			             $(CELL_DIR)/jflat$(ROUTED_SUFFIX)/env-ntpc/$(ENV)
	#TASK=hspice ENV=$(call GET_NTH_RUN_PARAM,$(@D),$(DEFAULT_SPICE_PARAMS),2) CELL=$(call GET_CAST_FULL_NAME,$(@D))
	mkdir -p '$(@D)'
	$(CASTFILES_ENQUEUE_TASK) && \
	working_dir=`mktemp -d "$(WORKING_DIR)/hspice.XXXXXX"`; \
	chmod 2775 "$$working_dir"; \
	time=$(subst ns,e-9,$(call GET_NTH_RUN_PARAM,$(@D),$(DEFAULT_SPICE_PARAMS),6)); \
	temp=$(subst C,,$(call GET_NTH_RUN_PARAM,$(@D),$(DEFAULT_SPICE_PARAMS),5)); \
	true=$(subst V,,$(call GET_NTH_RUN_PARAM,$(@D),$(DEFAULT_SPICE_PARAMS),4)); \
	corner=$(call GET_NTH_RUN_PARAM,$(@D),$(DEFAULT_SPICE_PARAMS),3); \
	env="$(call GET_NTH_RUN_PARAM,$(@D),$(DEFAULT_SPICE_PARAMS),2)"; \
	type=$(call GET_SPICE_TYPE,$(@D)); \
	reduce=$$([[ $$type == extracted ]] && echo 1 || echo 0); \
	nodes=$$($(GNUGAWK) '{print $$7}' '$(word 3,$^)' | sort -u | \
		$(RENAME) --type=node --from=cast --to=gds2 | \
		$(GNUGAWK) '{print "Xenv.Xtest." $$1}' | \
	        tr '\n' ',' | $(GNUSED) 's/,$$//' ); \
	QRSH_FLAGS="$(PACKAGE_FLAGS) -l hspice=1" \
	QB_DIAG_FILE='$@.diag' \
	QB_RUN_NAME='lve_hspice' \
	$(QB) $(HSPICE) \
	  --run-directory="$$working_dir" \
	  --fulcrum-pdk-root=$(FULCRUM_PDK_PACKAGE_EXTRACT_ROOT) \
	  --cell-spice-file='$(word 1, $^)' \
	  --env-spice-file='$(word 2, $^)' \
	  --accurate=$(HSIM_ACCURATE) \
	  --hsim-vdd=$(HSIM_VDD) \
	  $(PRS_CAP) \
	  $(PRS_DELAY) \
	  $(PRS_MIN_RES) \
	  $(PRS_MAX_RES) \
	  --rc-reduction=$$reduce \
	  --minC=$(MINC) --minR=$(MINR) \
	  --run-time=$$time --process-corner=$$corner --vdd=$$true \
	  --measure-nodes="$$nodes" \
	  --delete=0 \
	  --lve-root-dir='$(ROOT_TARGET_DIR)' \
	  --sub-lve-root-dir='$(SUB_LVE_ROOT_DIR)' \
	  --output-dir='$(@D)' \
          --sigma-factor=$(SIGMA_FACTOR) \
	  --temp="$$temp" \
	"$(call GET_GDS2_CDL_NAME,$(@D))" "$(call GET_GDS2_CDL_NAME,$(@D))_U_env" &> '$(@D)/hspice.err' \
	&& if [ "${HSIM_DELETE}" = "1" ]; then /bin/rm -rf "$${working_dir}" ; fi;
	$(CASTFILES_DEQUEUE_TASK)

$(SPICE_DIR)/hspice/$(ENV)/%/hspice.names $(SPICE_DIR)/hspice/$(ENV)/%/hspice.trace : $(SPICE_DIR)/hspice/$(ENV)/%/hspice.out
	$(CASTFILES_ENQUEUE_TASK)
	(cd '$(@D)'; \
	 QB_DIAG_FILE='$@.diag' QB_RUN_NAME='lve_convert_trace' \
	 $(EXEC_LOW_PACKAGE) convert_trace --translate --csdf hspice.csdf hspice ;\
	)
	$(CASTFILES_DEQUEUE_TASK)

# 10485760
# summary for transient hspice
.PRECIOUS: $(SPICE_DIR)/hspice/%/hspice.raw
$(SPICE_DIR)/hspice/$(ENV)/%/hspice.raw: $(CELL_DIR)/jflat$(ROUTED_SUFFIX)/env-ntpc/$(ENV) \
				     $(SPICE_DIR)/hspice/$(ENV)/%/hspice.names
	#TASK=hspice_raw ENV=$(call GET_NTH_RUN_PARAM,$(@D),$(DEFAULT_SPICE_PARAMS),2) CELL=$(call GET_CAST_FULL_NAME,$(@D))
	$(CASTFILES_ENQUEUE_TASK) && \
	QB_DIAG_FILE='$@.diag' QB_RUN_NAME='lve_raw_hspice' \
	$(EXEC_LOW_PACKAGE) $(RAWIFY) --use-db=$(USEDB) --task=hspice \
	--root='$(ROOT_TARGET_DIR)' \
	--dir='$(@D)' \
	--cell="$(call GET_CAST_FULL_NAME,$(@D))" \
	--mode="$(call GET_SPICE_TYPE,$(@D))" \
	--env-ntpc='$<' $(LVE_RAW_CONFIG); \
	$(CASTFILES_DEQUEUE_TASK)

.PRECIOUS: $(ROOT_TARGET_DIR)/%/totem.out
.PRECIOUS: $(SPICE_DIR)/hsim/$(ENV)/%/totem.out
$(SPICE_DIR)/hsim/$(ENV)/%/totem.out: $(SPICE_DIR)/cell.spf \
			             $(CELL_DIR)/jflat$(ROUTED_SUFFIX)/hsim/$(ENV) \
			             $(CELL_DIR)/jflat$(ROUTED_SUFFIX)/env-ntpc/$(ENV) \
			             $(CELL_DIR)/cell.portprops
	#TASK=totem ENV=$(call GET_NTH_FROM_LAST_DIR,$(@D),5) CELL=$(call GET_CAST_FULL_NAME,$(@D))
	task=totem && $(CASTFILES_ENQUEUE_TASK) && \
	mkdir -p '$(@D)'; \
	work_dir=`mktemp -d "$(WORKING_DIR)/totem.XXXXXX"`; \
	nodes=$$($(GNUGAWK) '{print $$7}' '$(word 3,$^)' | sort -u | \
		$(RENAME) --type=node --from=cast --to=gds2 | \
		$(GNUGAWK) '{print "Xenv.Xtest." $$1}' | \
	        tr '\n' ',' | $(GNUSED) 's/,$$//' ); \
	QB_DIAG_FILE='$@.diag' QB_RUN_NAME='lve_totem' \
	  $(EXEC_PACKAGE) run_totem \
	  --cast-dir='$(CAST_DIR)' \
	  --spec-dir='$(SPEC_DIR)' \
	  --fqcn='$(call GET_CAST_FULL_NAME,$(@D))' \
	  --env=$(call GET_NTH_FROM_LAST_DIR,$(@D),5) \
	  --view=$(call GET_NTH_FROM_LAST_DIR,$(@D),8) \
	  --true=$(call GET_NTH_FROM_LAST_DIR,$(@D),3) \
	  --corner=$(call GET_NTH_FROM_LAST_DIR,$(@D),4) \
	  --extract-corner=$(call GET_NTH_FROM_LAST_DIR,$(@D),7) \
	  --temp=$(call GET_NTH_FROM_LAST_DIR,$(@D),2) \
	  --dynamic-simulation-time=$(call GET_NTH_FROM_LAST_DIR,$(@D),1) \
	  --work-dir="$$work_dir" \
	  $(PRS_DELAY) \
	  $(CAP_LOAD) \
	  --port-props='$(word 4,$^)' \
	  --measure-nodes="$$nodes" \
	  --lve-dir='$(ROOT_TARGET_DIR)' \
	  --fulcrum-pdk-root=$(FULCRUM_PDK_PACKAGE_EXTRACT_ROOT) \
	   2>&1 > "$@" && \
   ploc='$(@D)/../../../../../../../cell.ploc' && \
   { [[ -e "$$ploc" ]] || cp "$$work_dir/dynamic_run/adsRpt/PG.ploc" "$$ploc"; } && \
	 cd / && ( [[ $(KEEP_TOTEM_DIR) == 1 ]] || rm -rf "$$work_dir" ) ; \
	 task=totem && $(CASTFILES_DEQUEUE_TASK)

.PRECIOUS: $(ROOT_TARGET_DIR)/%/cmm.out
$(SPICE_DIR)/hsim/$(ENV)/%/cmm.out: $(SPICE_DIR)/hsim/$(ENV)/%/totem.out \
			             $(CELL_DIR)/jflat$(ROUTED_SUFFIX)/env-ntpc/$(ENV) \
			             $(CELL_DIR)/cell.portprops
	#TASK=cmm ENV=$(call GET_NTH_FROM_LAST_DIR,$(@D),5) CELL=$(call GET_CAST_FULL_NAME,$(@D))
	task=cmm && $(CASTFILES_ENQUEUE_TASK) && \
	mkdir -p '$(@D)'; \
	work_dir=`mktemp -d "$(WORKING_DIR)/cmm.XXXXXX"`; \
	nodes=$$($(GNUGAWK) '{print $$7}' '$(word 1,$^)' | sort -u | \
		$(RENAME) --type=node --from=cast --to=gds2 | \
		$(GNUGAWK) '{print "Xenv.Xtest." $$1}' | \
	        tr '\n' ',' | $(GNUSED) 's/,$$//' ); \
	QB_DIAG_FILE='$@.diag' QB_RUN_NAME='lve_cmm' \
	  $(EXEC_PACKAGE) run_cmm \
	  --cast-dir='$(CAST_DIR)' \
	  --spec-dir='$(SPEC_DIR)' \
	  --fqcn='$(call GET_CAST_FULL_NAME,$(@D))' \
	  --env='$(call GET_NTH_FROM_LAST_DIR,$(@D),5)' \
	  --view='$(call GET_NTH_FROM_LAST_DIR,$(@D),8)' \
	  --true='$(call GET_NTH_FROM_LAST_DIR,$(@D),3)' \
	  --corner='$(call GET_NTH_FROM_LAST_DIR,$(@D),4)' \
	  --extract-corner=$(call GET_NTH_FROM_LAST_DIR,$(@D),7) \
	  --temp='$(call GET_NTH_FROM_LAST_DIR,$(@D),2)' \
	  --dynamic-simulation-time=$(call GET_NTH_FROM_LAST_DIR,$(@D),1) \
	  --work-dir="$$work_dir" \
	  $(PRS_DELAY) \
	  $(CAP_LOAD) \
	  --port-props='$(word 2,$^)' \
	  --measure-nodes="$$nodes" \
	  --lve-dir='$(ROOT_TARGET_DIR)' \
	  --fulcrum-pdk-root='$(FULCRUM_PDK_PACKAGE_EXTRACT_ROOT)' \
	   2>&1 > "$@" && \
	   if [[ -d "$$work_dir/dynamic_run/cmmPG" ]]; then tar -C "$$work_dir/dynamic_run/cmmPG" -jcf '$(@D)/cmm.tar.bz2' '$(call GET_GDS2_CDL_NAME,$(@D))_model' ; fi && \
	 cd / && ( [[ $(KEEP_TOTEM_DIR) == 1 ]] || rm -rf "$$work_dir" ) ; \
	 task=cmm && $(CASTFILES_DEQUEUE_TASK)

.PRECIOUS: $(SPICE_DIR)/hsim/%/totem.raw
.PRECIOUS: $(SPICE_DIR)/hsim/$(ENV)/%/totem.raw
$(SPICE_DIR)/hsim/$(ENV)/%/totem.raw: $(CELL_DIR)/jflat$(ROUTED_SUFFIX)/env-ntpc/$(ENV) \
                                      $(SPICE_DIR)/hsim/$(ENV)/%/totem.out
	#TASK=totem_raw ENV=$(call GET_NTH_FROM_LAST_DIR,$(@D),5) CELL=$(call GET_CAST_FULL_NAME,$(@D))
	$(CASTFILES_ENQUEUE_TASK) && \
	QB_DIAG_FILE='$@.diag' QB_RUN_NAME='lve_totem_raw' \
	$(EXEC_LOW_PACKAGE) $(RAWIFY) --use-db=$(USEDB) --task=totem \
	--root='$(ROOT_TARGET_DIR)' \
	--dir='$(@D)' \
	--cell="$(call GET_CAST_FULL_NAME,$(@D))" \
	--env-ntpc='$<' \
	--mode="$(call GET_SPICE_TYPE,$(@D))"  $(LVE_RAW_CONFIG) && \
	$(CASTFILES_DEQUEUE_TASK)


.PRECIOUS: $(SPICE_DIR)/rte/%/rte.out
.PRECIOUS: $(SPICE_DIR)/rte/%/$(ENV)/rte.out
$(SPICE_DIR)/rte/%/$(ENV)/rte.out:  $(SPICE_DIR)/alint/%/plt.raw  
	#TASK=rte ENV=$(call GET_NTH_FROM_LAST_DIR,$(@D),1) CELL=$(call GET_CAST_FULL_NAME,$(@D))
	$(CASTFILES_ENQUEUE_TASK) && \
	mkdir -p '$(@D)'; \
	work_dir=`mktemp -d "$(WORKING_DIR)/rte.XXXXXX"`; \
	temp='$(call GET_NTH_FROM_LAST_DIR,$(@D),2)'; \
	true='$(call GET_NTH_FROM_LAST_DIR,$(@D),3)'; \
	corner='$(call GET_NTH_FROM_LAST_DIR,$(@D),4)'; \
	QB_DIAG_FILE='$@.diag' QB_RUN_NAME='lve_rte' \
	$(EXEC_PACKAGE) jdsim-auto \
	--cast-path="$(CAST_DIR):$(SPEC_DIR):$(ROOT_TARGET_DIR)/plt/$$corner/$$true/$$temp" \
	--cell='plt.$(call GET_CAST_FULL_NAME,$(@D))' \
	--envs='$(call GET_NTH_FROM_LAST_DIR,$(@D),1)' \
	--basedir="$(@D)" \
	--digital-delay="$(DIGITALDELAY)" \
	--measured-delay="$(MEASUREDDELAY)" \
	"$(CYCLECOUNT)" \
	--history-per-node="$(DEPTH)" \
	--use-measured-delay="$(USEMEASUREDDELAY)" \
	2>&1 > "$@" && \
	$(CASTFILES_DEQUEUE_TASK)

.PRECIOUS: $(SPICE_DIR)/rte/%/rte.raw
.PRECIOUS: $(SPICE_DIR)/rte/%/$(ENV)/rte.raw
$(SPICE_DIR)/rte/%/$(ENV)/rte.raw: $(SPICE_DIR)/rte/%/$(ENV)/rte.out 
	#TASK=rte_raw ENV=$(call GET_NTH_FROM_LAST_DIR,$(@D),1) CELL=$(call GET_CAST_FULL_NAME,$(@D))
	$(CASTFILES_ENQUEUE_TASK) && \
	QB_DIAG_FILE='$@.diag' QB_RUN_NAME='lve_rte_raw' \
	$(EXEC_LOW_PACKAGE) $(RAWIFY) --use-db=$(USEDB) --task=rte \
	--root='$(ROOT_TARGET_DIR)' \
	--dir='$(@D)' \
	--cell="$(call GET_CAST_FULL_NAME,$(@D))" \
	--mode="$(call GET_SPICE_TYPE,$(@D))" \
	--env-ntpc='$<'  $(LVE_RAW_CONFIG); \
	$(CASTFILES_DEQUEUE_TASK)

.PRECIOUS: $(SPICE_DIR)/rte/%/rte.raw.summary
$(SPICE_DIR)/rte/%/rte.raw.summary: $(foreach e, $(RTEDEPS), $(e))
	#TASK = rte_summarize 
	echo "the dep list is $(RTEDEPS)"
	$(CASTFILES_ENQUEUE_TASK) && \
	mkdir -p '$(@D)'; \
	work_dir=`mktemp -d "$(WORKING_DIR)/rte.XXXXXX"`; \
	QB_DIAG_FILE='$@.diag' QB_RUN_NAME='lve_rteSummarize' \
	  $(EXEC_PACKAGE) rte_summarize \
	  --cast-dir='$(CAST_DIR)' \
	  --spec-dir='$(SPEC_DIR)' \
	  --fqcn='$(call GET_CAST_FULL_NAME,$(@D))' \
	  --dir ='$(@D)'  \
	  --work-dir="$$work_dir" \
	  --lve-dir='$(ROOT_TARGET_DIR)' \
	  --fulcrum-pdk-root=$(FULCRUM_PDK_PACKAGE_EXTRACT_ROOT) \
	   2>&1 > "$@" && \
	$(CASTFILES_DEQUEUE_TASK)

.PRECIOUS: $(SPICE_DIR)/aspice/$(ENV)/%/cell.spiprof
$(SPICE_DIR)/aspice/$(ENV)/%/cell.spiprof: \
				   $(SPICE_DIR)/extract.result \
				   $(SPICE_DIR)/cell.aspice \
				   $(SPICE_DIR)/aspice/$(ENV)/%/aspice.raw \
				   $(SPICE_DIR)/aspice/leakage/%/aspice.raw
	rm -f '$(@D)/$(call GET_GDS2_CDL_NAME,$(@D)).spiprof'
	#TASK=apl CELL=$(call GET_CAST_FULL_NAME,$(@D))
	rm -f '$(@D)/$(call GET_GDS2_CDL_NAME,$(@D)).cdev'
	rm -f '$(@D)/cell.spiprof'
	rm -f '$(@D)/cell.cdev'
	lve_apl \
	  --cell='$(call GET_CAST_FULL_NAME,$(@D))' \
	  --env='$(call GET_NTH_FROM_LAST_DIR,$(@D),6)' \
	  --view='$(call GET_NTH_FROM_LAST_DIR,$(@D),9)' \
	  --mode='$(call GET_NTH_FROM_LAST_DIR,$(@D),8)' \
	  --true='$(call GET_NTH_FROM_LAST_DIR,$(@D),4)' \
	  --corner='$(call GET_NTH_FROM_LAST_DIR,$(@D),5)' \
	  --temp='$(call GET_NTH_FROM_LAST_DIR,$(@D),3)' \
	  $(CAP_LOAD) \
	  --name-space=gds2 \
	  --output-dir='$(ROOT_TARGET_DIR)'
	@if [[ -s '$(@D)/$(call GET_GDS2_CDL_NAME,$(@D)).spiprof' && -s '$(@D)/$(call GET_GDS2_CDL_NAME,$(@D)).cdev' ]]; then \
	   ln -f '$(@D)/$(call GET_GDS2_CDL_NAME,$(@D)).spiprof' '$(@D)/cell.spiprof'; \
	   ln -f '$(@D)/$(call GET_GDS2_CDL_NAME,$(@D)).cdev' '$(@D)/cell.cdev'; \
	else \
	   echo "Error: Apl files not created"; \
	fi
