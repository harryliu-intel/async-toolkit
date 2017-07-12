.SECONDEXPANSION:

ifeq ("$(CASTFILES)","1")

ifeq ("$(strip $(LVE_PACKAGE_ROOT))","")
$(error You must set the variable LVE_PACKAGE_ROOT to make layout. )
endif # "$(strip $(LVE_PACKAGE_ROOT))" eq ""

ifeq ("$(strip $(FULCRUM_PDK_PACKAGE_ROOT))","")
$(error You must set the variable FULCRUM_PDK_PACKAGE_ROOT to make layout. )
endif # "$(strip $(FULCRUM_PDK_PACKAGE_ROOT))" eq ""

ifeq ("$(strip $(CAST_PATH))","")
$(error You must set the variable CAST_PATH to make layout. )
endif # "$(strip $(CAST_PATH))" eq ""

JAUTOSKIP :=
ifeq ("$(EXTRACTSKIP6T)","1")
JAUTOSKIP := --define='lib.6T.6T.attr.exclude_sram_bits:true'
endif # "$(EXTRACTSKIP6T)" eq "1"

OS_TYPE   := $(shell uname -s)
ARCH_TYPE := $(shell uname -m)

# task queuing.  Use this in rules like so:
# task=bla ; $(CASTFILES_ENQUEUE_TASK) && ... ; $(CASTFILES_DEQUEUE_TASK)
# Make note of the ;'s and &&'s
# This will create $task.wait.$(TASKLOCK_QUEUE_LENGTH) ...  $task.wait.0
# lockfiles in order, implementing a file system queue
# The names of the wait files go in the $(LVE_WAIT_FILES) file
# which for lve is $output_dir/temp/waitfiles.$$
LOCKFILE := lockfile
TASKLOCK_QUEUE_LENGTH := 1
CASTFILES_ENQUEUE_TASK = \
	(if [[ "$(TASKLOCK)" == 1 ]] ; then \
	   echo "$$task"; \
	   [ -n "$$task" ] || task='$(@F)'; \
	   [ -n "$$dir" ] || dir='$(@D)'; \
	   echo "$$task" "$$dir"; \
	   [ -e "$$dir/$$task.wait.$(TASKLOCK_QUEUE_LENGTH)" ] && exit 2; \
	   for ((pos=$(TASKLOCK_QUEUE_LENGTH);pos>0;pos--)); do \
	    next_pos=$$(($$pos-1)); \
	    waitfile="$$dir/$$task.wait.$$pos"; \
	    nextwaitfile="$$dir/$$task.wait.$$next_pos"; \
	    $(LOCKFILE) "$$waitfile" "$$nextwaitfile"; \
	    rm -f "$$waitfile"; \
	    echo "$$nextwaitfile" >> $(LVE_WAIT_FILES); \
	   done; \
	 else exit 0; fi;) || exit 2

CASTFILES_DEQUEUE_TASK = \
	  (status=$$?; if [[ "$(TASKLOCK)" == 1 ]] ; then \
	     [ -n "$$dir" ] || dir='$(@D)'; \
	        waitfile="$$dir/$$task.wait.0"; \
	        : < '$@' ; \
	        rm -f "$$waitfile"; fi; [[ $$? == 0 && $$status == 0 ]])

# signatures
# Certain files are byproducts of an included makefile
# For lve, cast.d is an included makefile, with a bunch of
# byproducts that go in $(CURR_CELL_DIR)/jflat .  We signature these files.

CASTFILES_UPDATE_SIGNATURE = \
	$(CASTFILES_ENQUEUE_TASK) && \
	if [[ ! ( -e "$@" ) ]] || ! cmp -s '$<' '$@' ; then \
	     if [[ -e "$@" ]]; then /bin/rm -f '$@'; fi; \
	     cp -p '$<' '$@' && echo '$@ changed'; \
	   fi; \
	$(CASTFILES_DEQUEUE_TASK)

SPICE_TYPES := nogeometry estimated extracted  \
    accurate custom totem \
    extracted$(EXTRACT_DIR) accurate$(EXTRACT_DIR) totem$(EXTRACT_DIR) \
    nanotime$(EXTRACT_DIR)
#1) run directory
#2) default params
#3) N
GET_SPICE_PATH = $(call GET_LAST_WORD,$(sort $(filter-out $(1),$(foreach type,$(SPICE_TYPES),$(patsubst %/$(call GET_LAST_WORD,$(subst /$(type)/, ,$(1))),%,$(1))))))
GET_RUN_PATH = $(patsubst $(call GET_SPICE_PATH,$(1))/%,%,$(1))
GET_EXTRACT_DIR = $(call GET_LAST_WORD,$(subst /, ,$(call GET_SPICE_PATH,$(1))))
GET_SPICE_TYPE = $(firstword $(subst -, ,$(call GET_EXTRACT_DIR,$(1))))
GET_NTH_RUN_PARAM_IMPL = $(word $(2),$(subst /, ,$(call GET_RUN_PATH,$(1))))
GET_NTH_RUN_PARAM = $(if $(strip $(call GET_NTH_RUN_PARAM_IMPL,$(1),$(3))),$(call GET_NTH_RUN_PARAM_IMPL,$(1),$(3)),$(word $(3),$(2)))
LVE_SKIP = $(findstring $(1),$(LVE_SKIP_TASKS))
GET_CORNER = $(word 1,$(subst _, ,$(1)))
GET_SEED = $(or $(word 2,$(subst _, ,$(1))),0)
OPTIONAL_PREREQ = $(foreach prereq,$(1),$(if $(realpath $(prereq)),$(prereq),FORCE))

FORCE:

# select how to execute linux or sun commands
QB := qb
ifeq ($(QSUB),1)
QB_LOCAL := 0
else # $(QSUB) eq 1
QB := LD_LIBRARY_PATH=
QB_LOCAL := 1
endif # $(QSUB) eq 1
EXEC := $(QB)
QEXEC := qb

# These two seem like nonsense to me but are used in cdbfiles/dir.mk
EXEC_LINUX := $(QB)
EXEC_SUN   := $(QB)


ifeq ("$(USE_ROUTED)","1")
ROUTED_SUFFIX := .routed
endif # "$(USE_ROUTED)" eq "1"

#set default QSUB_ARCH
QSUB_ARCH := x86_64
# override if specified on command line
ifneq ("$(QSUB_ARCH_ARG)","")
QSUB_ARCH := $(QSUB_ARCH_ARG)
endif # "$(QSUB_ARCH_ARG)" ne ""
QRSH_FLAGS        := -now n -cwd -nostdin -p $(PRIORITY) $(MEMORY_QSUB_FLAGS) $(QSUB_EXTRAS)
ifeq ("$(MEMORY_QSUB_FLAGS_starRC2b)","")
MEMORY_QSUB_FLAGS_starRC2b := $(MEMORY_QSUB_FLAGS)
endif # "$(MEMORY_QSUB_FLAGS_starRC2b)" eq ""
ifeq ("$(QSUB_EXTRAS_starRC2b)","")
QSUB_EXTRAS_starRC2b := $(QSUB_EXTRAS)
endif # "$(QSUB_EXTRAS_starRC2b)" eq ""
QRSH_FLAGS_starRC2b := -now n -cwd -nostdin -p $(PRIORITY) $(MEMORY_QSUB_FLAGS_starRC2b) $(QSUB_EXTRAS_starRC2b)
QRSH_LOW_FLAGS    := -now n -cwd -nostdin -p $(PRIORITY) $(MEMORY_LOW_FLAGS) $(QSUB_EXTRAS)
LINUX_FLAGS       := $(QRSH_FLAGS) -l a=$(QSUB_ARCH)
LINUX_LOW_FLAGS   := $(QRSH_LOW_FLAGS) -l a=$(QSUB_ARCH)
SUN_FLAGS         := $(QRSH_FLAGS) -l a=$(QSUB_ARCH)
SUN_LOW_FLAGS     := $(QRSH_LOW_FLAGS) -l a=$(QSUB_ARCH)
PACKAGE_FLAGS     := -l a=$(QSUB_ARCH) $(QRSH_FLAGS)
PACKAGE_FLAGS_starRC2b  := -l a=$(QSUB_ARCH) $(QRSH_FLAGS_starRC2b)
# needed because ICV is nasty to the qsub limits
PACKAGE_FLAGS_drc  := $(shell echo '$(PACKAGE_FLAGS)' | sed -e 's/,h_vmem.*M -m a//')
PACKAGE_LOW_FLAGS := -l a=$(QSUB_ARCH) $(QRSH_LOW_FLAGS)

QEXEC_LINUX       := QB_LOCAL=0 QRSH_FLAGS='$(LINUX_FLAGS)' $(QB)
QEXEC_SUN         := QB_LOCAL=0 QRSH_FLAGS='$(SUN_FLAGS)' $(QB)
QEXEC_PACKAGE     := QB_LOCAL=0 QRSH_FLAGS='$(PACKAGE_FLAGS)' $(QB)

EXEC_LINUX       := QB_LOCAL=$(QB_LOCAL) QRSH_FLAGS='$(LINUX_FLAGS)' $(EXEC_LINUX)
EXEC_LOW_LINUX   := QB_LOCAL=$(QB_LOCAL) QRSH_FLAGS='$(LINUX_FLAGS)' $(EXEC_LOW_LINUX)
EXEC_SUN         := QB_LOCAL=$(QB_LOCAL) QRSH_FLAGS='$(SUN_FLAGS)' $(EXEC_SUN)
EXEC_LOW_SUN     := QB_LOCAL=$(QB_LOCAL) QRSH_FLAGS='$(SUN_FLAGS)' $(EXEC_LOW_SUN)
EXEC_PACKAGE     := QB_LOCAL=$(QB_LOCAL) QRSH_FLAGS='$(PACKAGE_FLAGS)' $(EXEC)
EXEC_PACKAGE_ASTA:= QB_LOCAL=$(QB_LOCAL) QRSH_FLAGS='$(PACKAGE_FLAGS) -l cores=$(ASTA_THREADS)' $(EXEC)
EXEC_PACKAGE_ASP := QB_LOCAL=$(QB_LOCAL) QRSH_FLAGS='$(PACKAGE_FLAGS)' $(EXEC)
EXEC_LOW_PACKAGE := QB_LOCAL=$(QB_LOCAL) QRSH_FLAGS='$(PACKAGE_LOW_FLAGS)' $(EXEC)


# DEFINE NON-JAVA TOOLS AND SCRIPTS
LVEEXEC       := lve
RENAME        := rename
LEF_RENAME    := lef_rename
DEF_RENAME    := def_rename
SPICE_REDUCE  := spice_reduce
SPICE2SPICE   := spice2spice --probe-ports=$(PROBE_PORTS) --probe-top-ports=$(PROBE_TOP_PORTS) --probe-gates=$(PROBE_GATES)
PARSECELLNAME := $(LVE_PACKAGE_ROOT)/share/script/sh/util/parsecellname
MKCDSWD       := mkcdswd
RUNINCDSWD    := runincdswd
MK_INSTANCE   := mk_instance
EXTRACT       := extract
EXTRACT_GRAYBOX := extract_graybox
LEFDEFWRITE   := lefdefWrite
LEFWRITE      := lefWrite
HSIM          := hsim --start-time=$(START_TIME) --sim=hsim $(POWER_GROUND_RESET_ARGS)
HSPICE        := hsim --start-time=$(START_TIME) --sim=hspice $(POWER_GROUND_RESET_ARGS)
XA            := hsim --start-time=$(START_TIME) --sim=xa $(POWER_GROUND_RESET_ARGS)
RAWIFY        := lve_raw --fulcrum-pdk-root='$(FULCRUM_PDK_PACKAGE_ROOT)'
FRC           := frc --fulcrum-pdk-root='$(FULCRUM_PDK_PACKAGE_ROOT)'
ASPICE        := local_aspice --disk-space=$(DISK_SPACE)
REORDER_TRACE := reorder_trace
SPICE2ASPICE  := canonicalizing_spice2aspice --java-args "$(GLOBAL_JAVA_FLAGS) $(GLOBAL_JRE_FLAGS)" --root-target-dir '$(ROOT_TARGET_DIR)' --include $(FULCRUM_PDK_PACKAGE_ROOT)/share/Fulcrum/lve/rc_spice2aspice.config 
SPICE2ASPICE_OPTS := --isopotential-layers '$(ISOPOTENTIALLAYERS)'
ifeq ("$(strip $(ISOPOTENTIALLAYERS))","")
SPICE2ASPICE_OPTS := 
endif
CDLALIASES    = cdlaliases $(GLOBAL_JAVA_FLAGS) $(GLOBAL_JRE_FLAGS) --gates-regex='^(gate|stack)\\..*' $(if $(GRAYBOX_MODE),--graybox-list='$(@D)/graybox_list')
MAKE_ASPICE_IN := make_aspice_in
MAKE_ALINT_IN := make_alint_in
MERGE_ALINT_OUT := merge_alint_out
MAKE_ASTA_IN := make_asta_in
MAKE_SLINT_IN := make_slint_in

ifeq ("$(DO_CONVERT_TRACE)","1")
CONVERT_TRACE := convert_trace --translate
else
CONVERT_TRACE := echo SKIP: convert_trace --translate
endif

# DEFINE JAVA TOOLS (with memory allocation)
JFLAT         := jflat $(GLOBAL_JAVA_FLAGS) $(GLOBAL_JRE_FLAGS)
JAUTO         := jauto $(GLOBAL_JAVA_FLAGS) $(GLOBAL_JRE_FLAGS)
JTIMER        := jtimer $(GLOBAL_JAVA_FLAGS) $(GLOBAL_JRE_FLAGS)
JLVS          := jlvs $(GLOBAL_JAVA_FLAGS) $(GLOBAL_JRE_FLAGS)
CAST_QUERY    := cast_query $(GLOBAL_JAVA_FLAGS) $(GLOBAL_JRE_FLAGS)
CAST2CDL      := cast2cdl $(GLOBAL_JAVA_FLAGS) $(GLOBAL_JRE_FLAGS)
CDL_RENAMER   := cdl_renamer $(GLOBAL_JAVA_LOW_FLAGS) $(GLOBAL_JRE_FLAGS) --layout-net-prefix='ln\#' --layout-inst-prefix='ld\#'
CAST2SKILL    := cast2skill $(GLOBAL_JAVA_FLAGS) $(GLOBAL_JRE_FLAGS)
GENERATEPLTSUBTYPES := generate_plt_subtypes $(GLOBAL_JAVA_FLAGS) $(GLOBAL_JRE_FLAGS)
GDSIIWRITE    := gdsIIWrite $(GLOBAL_JAVA_FLAGS) $(GLOBAL_JRE_FLAGS)
GENERATELIB   := generatelib --format=real $(GLOBAL_JAVA_FLAGS) $(GLOBAL_JRE_FLAGS)
CELL_LEF      := cell_lef
JDSIM         := jdsim $(GLOBAL_JAVA_FLAGS) $(GLOBAL_JRE_FLAGS)

# macros to get various versions of cell name
GET_CAST_CDL_NAME  = $(word 1,$(call GET_CELL_NAME,$(1),.cellname))
GET_CAST_FULL_NAME = $(word 2,$(call GET_CELL_NAME,$(1),.cellname))
GET_CAST_BASE_NAME = $(word 3,$(call GET_CELL_NAME,$(1),.cellname))
GET_GDS2_CDL_NAME  = $(word 4,$(call GET_CELL_NAME,$(1),.cellname))
GET_CADENCE_BASE_NAME  = $(word 5,$(call GET_CELL_NAME,$(1),.cellname))

GET_CELL_NAME = $(shell $(BUILD)/filetypes/castfiles/getcellname "$(1)" "$(2)" )

GET_FILES_FROM_CDBDEP           = $(shell cat '$(1)' | xargs -n 1 echo | grep '^/')
GET_CELL_DIR_FROM_CDBDEP        = $(dir $(word 2,$(call GET_FILES_FROM_CDBDEP,$(1))))
GET_DRC_SIGNOFFS_FROM_CDBDEP   = $(wildcard $(call GET_CELL_DIR_FROM_CDBDEP,$(1))drc_signoff)
GET_FRC_SIGNOFFS_FROM_CDBDEP    = $(wildcard $(call GET_CELL_DIR_FROM_CDBDEP,$(1))frc_signoff)
GET_LVS_SIGNOFF_FROM_CDBDEP     = $(wildcard $(call GET_CELL_DIR_FROM_CDBDEP,$(1))lvs_signoff)
GET_DRC_CELL_SIGNOFF_FROM_CDBDEP = $(wildcard $(call GET_CELL_DIR_FROM_CDBDEP,$(1))drc_signoff)
GET_FRC_CELL_SIGNOFF_FROM_CDBDEP = $(wildcard $(call GET_CELL_DIR_FROM_CDBDEP,$(1))frc_signoff)

GET_NTH_FROM_LAST_DIR = $(word $(words $(wordlist $(2),999,$(subst /, ,$(1)))),$(subst /, ,$(1)))
GET_TRUE_IGNORE_BULK = $(shell lvs_signoff="$(call GET_LVS_SIGNOFF_FROM_CDBDEP,$(call CONONICALIZE_PATH,$(@D)/../df2.d))"; \
    if [ $(IGNORE_BULK) = 1 ]; then \
      echo 1; \
    else \
      if [ ! -e "$$lvs_signoff" ]; then \
        echo 0; \
      else \
        if [ `grep -ci 'well.*plug' "$$lvs_signoff"` -gt 0 ]; then \
          echo 1; \
        else \
          echo 0; \
        fi \
      fi \
    fi )

GET_TRUE_IGNORE_BULK_LVS = $(shell lvs_signoff="$(call GET_LVS_SIGNOFF_FROM_CDBDEP,$(call CONONICALIZE_PATH,$(@D)/df2.d))"; \
    if [ $(IGNORE_BULK) = 1 ]; then \
      echo 1; \
    else \
      if [ ! -e "$$lvs_signoff" ]; then \
        echo 0; \
      else \
        if [ `grep -ci 'well.*plug' "$$lvs_signoff"` -gt 0 ]; then \
          echo 1; \
        else \
          echo 0; \
        fi \
      fi \
    fi )

# bootstrapping
ifneq ("$(ROOT_SLURP_DIR)","")
$(ROOT_TARGET_DIR)/%/cell.mk.latest: $(LVELOG) $(ROOT_TARGET_DIR)/%/.cellname
	echo 'CURR_CELL_DIR := $(@D)' > '$@'
	echo 'CURR_SLURP_DIR := $(ROOT_SLURP_DIR)/$*' >> '$@'
	echo 'include $$(BUILD)/filetypes/castfiles/dir.mk' >> '$@'
else # "$(ROOT_SLURP_DIR)" ne ""
$(ROOT_TARGET_DIR)/%/cell.mk.latest: $(LVELOG) $(ROOT_TARGET_DIR)/%/.cellname
	echo 'CURR_CELL_DIR := $(@D)' > '$@'
	echo 'include $$(BUILD)/filetypes/castfiles/dir.mk' >> '$@'

endif # "$(ROOT_SLURP_DIR)" ne ""

$(ROOT_TARGET_DIR)/%/cell.mk: $(ROOT_TARGET_DIR)/%/cell.mk.latest
	$(CASTFILES_UPDATE_SIGNATURE)

.PRECIOUS: $(ROOT_TARGET_DIR)/%/cdl.aliases
$(ROOT_TARGET_DIR)/%/cdl.aliases: $$(call CANONICALIZE_PATH,$(ROOT_TARGET_DIR)/%/../../cell.cdl) $(if $(GRAYBOX_MODE),$(ROOT_TARGET_DIR)/%/graybox_list)
	$(CDLALIASES) --cell='$(call GET_CAST_CDL_NAME,$(@D))' < '$<' > '$@.tmp' && \
	mv '$@.tmp' '$@'

.PRECIOUS: $(ROOT_TARGET_DIR)/%/cdl.aliases.routed
$(ROOT_TARGET_DIR)/%/cdl.aliases.routed: $$(call CANONICALIZE_PATH,$(ROOT_TARGET_DIR)/%/../../cell.cdl.routed) $(if $(GRAYBOX_MODE),$(ROOT_TARGET_DIR)/%/graybox_list)
	$(CDLALIASES) --cell='$(call GET_CAST_CDL_NAME,$(@D))' < '$<' > '$@.tmp' && \
	mv '$@.tmp' '$@'

# convert cell.spice_gds2 to cell.aspice for --mode=extracted
.PRECIOUS: $(ROOT_TARGET_DIR)/%/accurate$(EXTRACT_DIR)/cell.aspice
.PRECIOUS: $(ROOT_TARGET_DIR)/%/accurate$(EXTRACT_DIR)/cell.spice
.PRECIOUS: $(ROOT_TARGET_DIR)/%/extracted$(EXTRACT_DIR)/cell.aspice
.PRECIOUS: $(ROOT_TARGET_DIR)/%/extracted$(EXTRACT_DIR)/cell.spice
.PRECIOUS: $(ROOT_TARGET_DIR)/%/totem$(EXTRACT_DIR)/cell.aspice
.PRECIOUS: $(ROOT_TARGET_DIR)/%/totem$(EXTRACT_DIR)/cell.spice

ifeq ("$(GRAYBOX_MODE)", "")
$(ROOT_TARGET_DIR)/%/extracted$(EXTRACT_DIR)/cell.spice: $(ROOT_TARGET_DIR)/%/extracted$(EXTRACT_DIR)/cell.spice_gds2
	if [[ ( -n "$(call LVE_SKIP,extract)" ) && ( -e '$@' ) ]] ; then exit; fi; \
	task=renamer && $(CASTFILES_ENQUEUE_TASK) && \
	QB_DIAG_FILE='$@.renamer.diag' QB_RUN_NAME='lve_gds2spice' \
	$(EXEC_LOW_PACKAGE) $(CDL_RENAMER) \
	  --source-cdl-file='$<' \
	  --name-in=gds2 \
	  --name-out=cast \
	  --translated-cdl='$@' \
	task=renamer && $(CASTFILES_DEQUEUE_TASK)

$(ROOT_TARGET_DIR)/%/totem$(EXTRACT_DIR)/cell.spice: $(ROOT_TARGET_DIR)/%/totem$(EXTRACT_DIR)/cell.spice_gds2
	if [[ ( -n "$(call LVE_SKIP,extract)" ) && ( -e '$@' ) ]] ; then exit; fi; \
	task=renamer && $(CASTFILES_ENQUEUE_TASK) && \
	QB_DIAG_FILE='$@.renamer.diag' QB_RUN_NAME='lve_gds2spice' \
	$(EXEC_LOW_PACKAGE) $(CDL_RENAMER) \
	  --source-cdl-file='$<' \
	  --name-in=gds2 \
	  --name-out=cast \
	  --translated-cdl='$@' \
	task=renamer && $(CASTFILES_DEQUEUE_TASK)

ifneq ("$(NOEXTRACTDEPS)", "1")
$(ROOT_TARGET_DIR)/%/extracted$(EXTRACT_DIR)/cell.aspice: $(ROOT_TARGET_DIR)/%/extracted$(EXTRACT_DIR)/cell.spice \
	$(ROOT_TARGET_DIR)/%/extracted$(EXTRACT_DIR)/cdl.aliases
	if [[ ( -n "$(call LVE_SKIP,extract)" ) && ( -e '$@' ) ]] ; then exit; fi; \
	if [ -s '$<' ] ; then \
	task=rc_spice2aspice && $(CASTFILES_ENQUEUE_TASK) && \
	QB_DIAG_FILE='$@.spice2aspice.diag' QB_RUN_NAME='lve_spice2aspice' \
	  $(EXEC_PACKAGE) $(SPICE2ASPICE) \
	  --output-dir '$(@D)' \
	  --fix-unconnected-ports \
	  --cell '$(call GET_CAST_CDL_NAME,$(@D))' \
	  --define-topcell \
	  --minC $(MINC) --minR $(MINR) --minRC $(MINRC) \
	  --alias-file '$(@D)/cdl.aliases' \
	  $(SPICE2ASPICE_OPTS) '$(@D)/cell.spice' '$@.tmp' \
	  1> '$(@D)/rc_spice2aspice.out' 2> '$(@D)/rc_spice2aspice.err' && \
	mv -f '$@.tmp' '$@'; \
	else rm -f '$@' && touch '$@'; fi; \
	task=rc_spice2aspice && $(CASTFILES_DEQUEUE_TASK)

$(ROOT_TARGET_DIR)/%/totem$(EXTRACT_DIR)/cell.aspice: $(ROOT_TARGET_DIR)/%/totem$(EXTRACT_DIR)/cell.spice \
	$(ROOT_TARGET_DIR)/%/totem$(EXTRACT_DIR)/cdl.aliases
	if [[ ( -n "$(call LVE_SKIP,extract)" ) && ( -e '$@' ) ]] ; then exit; fi; \
	if [ -s '$<' ] ; then \
	task=rc_spice2aspice && $(CASTFILES_ENQUEUE_TASK) && \
	QB_DIAG_FILE='$@.spice2aspice.diag' QB_RUN_NAME='lve_spice2aspice' \
	  $(EXEC_PACKAGE) $(SPICE2ASPICE) \
	  --output-dir '$(@D)' \
	  --fix-unconnected-ports \
	  --cell '$(call GET_CAST_CDL_NAME,$(@D))' \
	  --define-topcell \
	  --minC $(MINC) --minR $(MINR) --minRC $(MINRC) \
	  --alias-file '$(@D)/cdl.aliases' \
	  $(SPICE2ASPICE_OPTS) '$(@D)/cell.spice' '$@.tmp' \
	  1> '$(@D)/rc_spice2aspice.out' 2> '$(@D)/rc_spice2aspice.err' && \
	mv -f '$@.tmp' '$@'; \
	else rm -f '$@' && touch '$@'; fi; \
	task=rc_spice2aspice && $(CASTFILES_DEQUEUE_TASK)
endif # "$(NOEXTRACTDEPS)" ne "1" 36 lines back
# note depends on the extracted directory for the results
$(ROOT_TARGET_DIR)/%/accurate$(EXTRACT_DIR)/cell.aspice: $(ROOT_TARGET_DIR)/%/extracted$(EXTRACT_DIR)/cell.spice \
	$(ROOT_TARGET_DIR)/%/extracted$(EXTRACT_DIR)/cdl.aliases
	if [[ ( -n "$(call LVE_SKIP,extract)" ) && ( -e '$@' ) ]] ; then exit; fi; \
	if [ -s '$<' ] ; then \
	task=rc_spice2aspice && $(CASTFILES_ENQUEUE_TASK) && \
	QB_DIAG_FILE='$@.spice2aspice.diag' QB_RUN_NAME='lve_spice2aspice' \
	  $(EXEC_PACKAGE) $(SPICE2ASPICE) \
	  --output-dir '$(@D)' \
	  --fix-unconnected-ports \
	  --cell '$(call GET_CAST_CDL_NAME,$(@D))' \
          --define-topcell \
	  $(NAMED_RESISTOR_ARG) \
	  --minC $(MINACCURATEC) --minR $(MINACCURATER) --minRC $(MINACCURATERC) \
	  --alias-file '$(@D)/cdl.aliases' \
	  $(SPICE2ASPICE_OPTS) '$<' '$@.tmp' \
	  1> '$(@D)/rc_spice2aspice.out' 2> '$(@D)/rc_spice2aspice.err' && \
	mv -f '$@.tmp' '$@'; \
	else rm -f '$@' && touch '$@'; fi; \
	task=rc_spice2aspice && $(CASTFILES_DEQUEUE_TASK)

else # "$(GRAYBOX_MODE)" eq "" 80 lines back

$(ROOT_TARGET_DIR)/%/extracted$(EXTRACT_DIR)/cell.spice: $(ROOT_TARGET_DIR)/%/extracted$(EXTRACT_DIR)/cell.spice_topcell
	if [[ ( -n "$(call LVE_SKIP,extract)" ) && ( -e '$@' ) ]] ; then exit; fi; \
	task=renamer && $(CASTFILES_ENQUEUE_TASK) && \
	QB_DIAG_FILE='$@.renamer.diag' QB_RUN_NAME='lve_gds2spice' \
	$(EXEC_LOW_PACKAGE) $(CDL_RENAMER) \
	  --source-cdl-file='$<' \
	  --name-in=gds2 \
	  --name-out=cast \
	  --translated-cdl='$@' \
	task=renamer && $(CASTFILES_DEQUEUE_TASK)

ifneq ("$(NOEXTRACTDEPS)", "1")
$(ROOT_TARGET_DIR)/%/extracted$(EXTRACT_DIR)/cell.aspice: $(ROOT_TARGET_DIR)/%/extracted$(EXTRACT_DIR)/cell.spice \
	$(ROOT_TARGET_DIR)/%/extracted$(EXTRACT_DIR)/cdl.aliases
	if [[ ( -n "$(call LVE_SKIP,extract)" ) && ( -e '$@' ) ]] ; then exit; fi; \
	if [ -s '$<' ] ; then \
	task=rc_spice2aspice && $(CASTFILES_ENQUEUE_TASK) && \
	QB_DIAG_FILE='$@.spice2aspice.diag' QB_RUN_NAME='lve_spice2aspice' \
	  $(EXEC_PACKAGE) $(SPICE2ASPICE) \
	  --output-dir '$(@D)' \
	  --fix-unconnected-ports \
	  --cell '$(call GET_CAST_CDL_NAME,$(@D))' \
	  --define-topcell \
	  --minC $(MINC) --minR $(MINR) --minRC $(MINRC) \
	  --alias-file '$(@D)/cdl.aliases' \
	  $(SPICE2ASPICE_OPTS) '$<' '$@.tmp' \
	  1> '$(@D)/rc_spice2aspice.out' 2> '$(@D)/rc_spice2aspice.err' && \
	  touch '$(<D)/cell.spice_include' && \
	  sed -e '/^\*/d' -e 's:$(ROOT_TARGET_DIR)/::' -e 's/^.inc[^ ]*/.include/i' -e "s/'/"\""/g" -e 's/\.spice_gds2/.aspice/' '$(<D)/cell.spice_include' > '$@' && \
	  cat '$@.tmp' >> '$@' && rm -f '$@.tmp'; \
	else rm -f '$@' && touch '$@'; fi; \
	task=rc_spice2aspice && $(CASTFILES_DEQUEUE_TASK)

$(ROOT_TARGET_DIR)/%/totem$(EXTRACT_DIR)/cell.aspice: $(ROOT_TARGET_DIR)/%/totem$(EXTRACT_DIR)/cell.spice \
	$(ROOT_TARGET_DIR)/%/totem$(EXTRACT_DIR)/cdl.aliases
	if [[ ( -n "$(call LVE_SKIP,extract)" ) && ( -e '$@' ) ]] ; then exit; fi; \
	if [ -s '$<' ] ; then \
	task=rc_spice2aspice && $(CASTFILES_ENQUEUE_TASK) && \
	QB_DIAG_FILE='$@.spice2aspice.diag' QB_RUN_NAME='lve_spice2aspice' \
	  $(EXEC_PACKAGE) $(SPICE2ASPICE) \
	  --output-dir '$(@D)' \
	  --fix-unconnected-ports \
	  --cell '$(call GET_CAST_CDL_NAME,$(@D))' \
	  --define-topcell \
	  --minC $(MINC) --minR $(MINR) --minRC $(MINRC) \
	  --alias-file '$(@D)/cdl.aliases' \
	  $(SPICE2ASPICE_OPTS) '$<' '$@.tmp' \
	  1> '$(@D)/rc_spice2aspice.out' 2> '$(@D)/rc_spice2aspice.err' && \
	  touch '$(<D)/cell.spice_include' && \
	  sed -e '/^\*/d' -e 's:$(ROOT_TARGET_DIR)/::' -e 's/^.inc[^ ]*/.include/i' -e "s/'/"\""/g" -e 's/\.spice_gds2/.aspice/' '$(<D)/cell.spice_include' > '$@' && \
	  cat '$@.tmp' >> '$@' && rm -f '$@.tmp'; \
	else rm -f '$@' && touch '$@'; fi; \
	task=rc_spice2aspice && $(CASTFILES_DEQUEUE_TASK)
endif # "$(NOEXTRACTDEPS)" ne "1" 40 lines back

$(ROOT_TARGET_DIR)/%/accurate$(EXTRACT_DIR)/cell.aspice: $(ROOT_TARGET_DIR)/%/extracted$(EXTRACT_DIR)/cell.spice \
	$(ROOT_TARGET_DIR)/%/extracted$(EXTRACT_DIR)/cdl.aliases
	if [[ ( -n "$(call LVE_SKIP,extract)" ) && ( -e '$@' ) ]] ; then exit; fi; \
	if [ -s '$<' ] ; then \
	task=rc_spice2aspice && $(CASTFILES_ENQUEUE_TASK) && \
	QB_DIAG_FILE='$@.spice2aspice.diag' QB_RUN_NAME='lve_spice2aspice' \
	  $(EXEC_PACKAGE) $(SPICE2ASPICE) \
	  --output-dir '$(@D)' \
	  --fix-unconnected-ports \
	  --cell '$(call GET_CAST_CDL_NAME,$(@D))' \
          --define-topcell \
	  $(NAMED_RESISTOR_ARG) \
	  --minC $(MINACCURATEC) --minR $(MINACCURATER) --minRC $(MINACCURATERC) \
	  --alias-file '$(@D)/cdl.aliases' \
	  $(SPICE2ASPICE_OPTS) '$<' '$@.tmp' \
	  1> '$(@D)/rc_spice2aspice.out' 2> '$(@D)/rc_spice2aspice.err' && \
	  touch '$(<D)/cell.spice_include' && \
	  sed -e '/^\*/d' -e 's:$(ROOT_TARGET_DIR)/::' -e 's/extracted/accurate/' -e 's/^.inc[^ ]*/.include/i' -e "s/'/"\""/g" -e 's/\.spice_gds2/.aspice/' '$(<D)/cell.spice_include' > '$@' && \
	  cat '$@.tmp' >> '$@' && rm -f '$@.tmp'; \
	else rm -f '$@' && touch '$@'; fi; \
	task=rc_spice2aspice && $(CASTFILES_DEQUEUE_TASK)

endif # "$(GRAYBOX_MODE)" eq "" 156 lines back
# convert cell.spice_gds2 to cell.aspice for --mode=accurate

ifeq ("$(GRAYBOX_MODE)", "")
$(ROOT_TARGET_DIR)/%/accurate$(EXTRACT_DIR)/cell.aspice: $(ROOT_TARGET_DIR)/%/extracted$(EXTRACT_DIR)/cell.spice \
	$(ROOT_TARGET_DIR)/%/extracted$(EXTRACT_DIR)/cdl.aliases
	#TASK=rc_spice2aspice MODE=accurate$(EXTRACT_DIR) CELL=$(call GET_CAST_FULL_NAME,$(@D))
	if [[ ( -n "$(call LVE_SKIP,extract)" ) && ( -e '$@' ) ]] ; then exit; fi; \
	if [ -s '$<' ] ; then \
	task=rc_spice2aspice && $(CASTFILES_ENQUEUE_TASK) && \
	QB_DIAG_FILE='$@.spice2aspice.diag' QB_RUN_NAME='lve_spice2aspice' \
	  $(EXEC_PACKAGE) $(SPICE2ASPICE) \
	  --output-dir '$(@D)' \
	  --fix-unconnected-ports \
	  --cell '$(call GET_CAST_CDL_NAME,$(@D))' \
          --define-topcell \
	  $(NAMED_RESISTOR_ARG) \
	  --minC $(MINACCURATEC) --minR $(MINACCURATER) --minRC $(MINACCURATERC) \
	  --alias-file '$(@D)/cdl.aliases' \
	  $(SPICE2ASPICE_OPTS) '$<' '$@.tmp' \
	  1> '$(@D)/rc_spice2aspice.out' 2> '$(@D)/rc_spice2aspice.err' && \
	mv -f '$@.tmp' '$@'; \
	else rm -f '$@' && touch '$@'; fi; \
	task=rc_spice2aspice && $(CASTFILES_DEQUEUE_TASK)
else # "$(GRAYBOX_MODE)" eq ""
$(ROOT_TARGET_DIR)/%/accurate$(EXTRACT_DIR)/cell.aspice: $(ROOT_TARGET_DIR)/%/extracted$(EXTRACT_DIR)/cell.spice \
	$(ROOT_TARGET_DIR)/%/extracted$(EXTRACT_DIR)/cdl.aliases \
	$(ROOT_TARGET_DIR)/%/extracted$(EXTRACT_DIR)/cell.spice_include
	#TASK=rc_spice2aspice MODE=accurate$(EXTRACT_DIR) CELL=$(call GET_CAST_FULL_NAME,$(@D))
	if [[ ( -n "$(call LVE_SKIP,extract)" ) && ( -e '$@' ) ]] ; then exit; fi; \
	if [ -s '$<' ] ; then \
	task=rc_spice2aspice && $(CASTFILES_ENQUEUE_TASK) && \
	QB_DIAG_FILE='$@.spice2aspice.diag' QB_RUN_NAME='lve_spice2aspice' \
	  $(EXEC_PACKAGE) $(SPICE2ASPICE) \
	  --output-dir '$(@D)' \
	  --fix-unconnected-ports \
	  --cell '$(call GET_CAST_CDL_NAME,$(@D))' \
          --define-topcell \
	  $(NAMED_RESISTOR_ARG) \
	  --minC $(MINACCURATEC) --minR $(MINACCURATER) --minRC $(MINACCURATERC) \
	  --alias-file '$(@D)/cdl.aliases' \
	  $(SPICE2ASPICE_OPTS) '$<' '$@.tmp' \
	  1> '$(@D)/rc_spice2aspice.out' 2> '$(@D)/rc_spice2aspice.err' && \
	sed -e '/^\*/d' -e 's:$(ROOT_TARGET_DIR)/::' -e 's/extracted/accurate/' -e 's:$(ROOT_SLURP_DIR)/::' -e 's/^.inc[^ ]*/.include/i' -e "s/'/"\""/g" -e 's/\.spice_gds2/.aspice/' '$(<D)/cell.spice_include' > '$@' && \
	cat '$@.tmp' >> '$@' && rm -f '$@.tmp'; \
	else rm -f '$@' && touch '$@'; fi; \
	task=rc_spice2aspice && $(CASTFILES_DEQUEUE_TASK)
endif # "$(GRAYBOX_MODE)" eq "" 41 lines back

# convert cell.spice_gds2 to cell.aspice for --mode=estimated or --mode=nogeometry or --mode=custom
.PRECIOUS: $(ROOT_TARGET_DIR)/%/cell.aspice
$(ROOT_TARGET_DIR)/%/cell.aspice: $(ROOT_TARGET_DIR)/%/cell.spice $$(call CANONICALIZE_PATH,$(ROOT_TARGET_DIR)/%/../../cell.cdl)
	if [[ ( -n "$(call LVE_SKIP,extract)" ) && ( -e '$@' ) ]] ; then exit; fi; \
	if [ -s '$<' ] ; then \
	task=rc_spice2aspice && $(CASTFILES_ENQUEUE_TASK) && \
	QB_DIAG_FILE='$@.diag' QB_RUN_NAME='lve_spice2aspice' \
	  $(EXEC_PACKAGE) $(SPICE2ASPICE) \
	  --output-dir '$(@D)' \
	  --cell '$(call GET_CAST_CDL_NAME,$(@D))' \
	  --define-topcell \
	  --minC $(MINC) --minR $(MINR) --minRC $(MINRC) \
	  --estimated \
	  $(SPICE2ASPICE_OPTS) '$<' '$@.tmp' \
	  1> '$(@D)/rc_spice2aspice.out' 2> '$(@D)/rc_spice2aspice.err' && \
	  touch '$(@D)/cell.spice_include' && \
	  sed -e '/^\*/d' -e 's:$(ROOT_TARGET_DIR)/::' -e 's/^.inc[^ ]*/.include/i' -e "s/'/"\""/g" -e 's/\.spice\(_gds2\)\?/.aspice/' '$(@D)/cell.spice_include' > '$@' && \
	  cat '$@.tmp' >> '$@' && rm -f '$@.tmp'; \
	else rm -f '$@' && touch '$@'; fi; \
	task=rc_spice2aspice && $(CASTFILES_DEQUEUE_TASK)

.PRECIOUS: $(ROOT_TARGET_DIR)/%/cell.gds2_bias
$(ROOT_TARGET_DIR)/%/cell.gds2_bias: $(ROOT_TARGET_DIR)/%/cell.gds2_lambda
	#TASK=gds2_silicon CELL=$(call GET_CAST_FULL_NAME,$(@D))
	task=gds2_si && $(CASTFILES_ENQUEUE_TASK) && \
	drc_dir=`mktemp -d "$(WORKING_DIR)/drc.XXXXXX"`; \
	sync; \
	sleep 1; \
	rsf="$$drc_dir/rsf"; \
	cp '$(FULCRUM_PDK_PACKAGE_ROOT)/share/Fulcrum/assura/bias.rsf.include' "$$rsf" ; \
	ln -s '$(@D)/cell.gds2_lambda' "$$drc_dir/cell.gds2" ; \
	chmod 2775 "$$drc_dir" && cd "$$drc_dir" && \
	QB_DIAG_FILE='$@.diag' QB_RUN_NAME='lve_bias_si' \
	QB_LOCAL=0 QRSH_FLAGS='$(PACKAGE_FLAGS) -l drc=1' \
	$(QEXEC) $(ASSURA_SCRIPT) \
	$(LVE_PACKAGE_ROOT)/share/script/perl/ve/front-end/vfe \
	VerificationType=AssuraDrc \
	RunName=drc \
	LayoutCell='$(call GET_GDS2_CDL_NAME,$(@D))' \
	WorkingDir="$$drc_dir" \
	GDS2File="$$drc_dir/cell.gds2" \
	OutFile=1 \
	AssuraSet.=KEEP_NDIFF_PDIFF \
	RuleFile="$(FULCRUM_PDK_PACKAGE_ROOT)/share/Fulcrum/assura/bias.rul" \
	AssuraRsfInclude="$$rsf" \
	> "$$drc_dir/vfeout" && \
	mv "$$drc_dir/biased.gds2" '$@' && \
	cd / && ls "$$drc_dir"; \
	$(CASTFILES_DEQUEUE_TASK)

GDS2_TARGET := cell.gds2

.PRECIOUS: $(ROOT_TARGET_DIR)/%/lib_gds2.lef
$(ROOT_TARGET_DIR)/%/lib_gds2.lef: $(ROOT_TARGET_DIR)/%/lib.lef
	#TASK=lef_rename VIEW=$(call GET_NTH_FROM_LAST_DIR,$(@D),2) CELL=$(call GET_CAST_FULL_NAME,$(@D))
	mkdir -p '$(@D)' ; \
	   $(LEF_RENAME) \
	  --from="cadence" \
	  --to="gds2" \
	  --infile="$?" \
	  --outfile="$@" ;

.PRECIOUS: $(ROOT_TARGET_DIR)/%/lib.lef
$(ROOT_TARGET_DIR)/%/lib.lef: $(ROOT_TARGET_DIR)/%/df2.d
	#TASK=lib_lef VIEW=abstract CELL=$(call GET_CAST_FULL_NAME,$(@D))
	task=lib_lef && $(CASTFILES_ENQUEUE_TASK) && \
	mkdir -p '$(@D)' ; \
	QB_DIAG_FILE='$(@D)/lib_lef.diag' QB_RUN_NAME='lve_liblef' \
	  QB_LOCAL=$(QB_LOCAL) QRSH_FLAGS="$(PACKAGE_FLAGS)" \
	   $(EXEC) $(IC_SCRIPT) $(CELL_LEF) \
	  --fulcrum-pdk-root='$(FULCRUM_PDK_PACKAGE_ROOT)' \
	  --dfII-dir='$(DFII_DIR)' \
	  --cds-log='$(@D)/liblef.log' \
	  --abstract-log='$(@D)/createAbstract.log' \
	  --lef-output='$(@D)/lib.lef' \
	  --big-lef=$(BIG_LEF) \
	  --cast-dir=$(CAST_DIR) \
	  --spec-dir=$(SPEC_DIR) \
	  --max-heap-size=$(MAX_HEAP_SIZE) \
	  --cell='$(call GET_CAST_FULL_NAME,$(@D))' ;\
	task=lib_lef && $(CASTFILES_DEQUEUE_TASK)

.PRECIOUS: $(ROOT_TARGET_DIR)/%/extracted$(EXTRACT_DIR)/graybox_list
$(ROOT_TARGET_DIR)/%/totem$(EXTRACT_DIR)/graybox_list: $(ROOT_TARGET_DIR)/%/totem$(EXTRACT_DIR)/graybox_list.latest
	$(CASTFILES_UPDATE_SIGNATURE)

$(ROOT_TARGET_DIR)/%/lvs_graybox_list: $(ROOT_TARGET_DIR)/%/lvs_graybox_list.latest
	$(CASTFILES_UPDATE_SIGNATURE)

ifneq ("$(GRAYBOX_LIST)", "")
$(ROOT_TARGET_DIR)/%/extracted$(EXTRACT_DIR)/graybox_list.tmp: $(GRAYBOX_LIST)
	if [[ ( -e '$(<)' ) ]] ; then sed -e 's/ //g' '$(<)' | sort -u -o '$@' ; fi;

$(ROOT_TARGET_DIR)/%/extracted$(EXTRACT_DIR)/graybox_list: $(ROOT_TARGET_DIR)/%/extracted$(EXTRACT_DIR)/graybox_list.tmp
	$(CASTFILES_UPDATE_SIGNATURE)

else # "$(GRAYBOX_LIST)" ne ""
$(ROOT_TARGET_DIR)/%/extracted$(EXTRACT_DIR)/graybox_list: $(ROOT_TARGET_DIR)/%/extracted$(EXTRACT_DIR)/graybox_list.latest
	$(CASTFILES_UPDATE_SIGNATURE)

endif # "$(GRAYBOX_LIST)" ne ""

#NOTE BELOW: GRAYBOX_LIST is MERGED with the cast_query graybox list unless in mode==leaf
# this probably should be changed, but not now: AAG 9/27/2010

ifeq ("$(GRAYBOX_MODE)", "subcells")
GRAYBOX_LIST_OPTS = --filter=one-level
else ifeq ("$(GRAYBOX_MODE)", "routed")
GRAYBOX_LIST_OPTS = --filter=one-level --routed
else ifeq ("$(GRAYBOX_MODE)", "leaf")
GRAYBOX_LIST_OPTS = --filter=leaf --routed
else
GRAYBOX_LIST_OPTS =
endif

$(ROOT_TARGET_DIR)/%/graybox_list.latest: $$(call CANONICALIZE_PATH,$(ROOT_TARGET_DIR)/%/../df2.d) $$(call CANONICALIZE_PATH,$(ROOT_TARGET_DIR)/%/../../cast.d)
	#TASK=create_graybox_list VIEW=$(call GET_NTH_FROM_LAST_DIR,$(@D),2) CELL=$(call GET_CAST_FULL_NAME,$(@D))
	mkdir -p '$(@D)'
	QB_DIAG_FILE='$(@D)/graylist.diag' QB_RUN_NAME='lve_graylist' \
	  QB_LOCAL=$(QB_LOCAL) QRSH_FLAGS="$(PACKAGE_FLAGS)" \
	   $(EXEC) $(CAST_QUERY) --cast-path='$(CAST_PATH)' \
	 --task=subcells $(GRAYBOX_LIST_OPTS) \
	 --output='$@' \
	 --cell='$(call GET_CAST_FULL_NAME,$(@D))'; \
	 : < '$@' ; \
	 touch '$@' ; \
	 if [[ -e '$(GRAYBOX_LIST)' ]] ; then sed -e 's/ //g' '$(GRAYBOX_LIST)' '$@' | sort -u -o '$@'.tmp ; mv -f '$@'.tmp '$@'; \
	 else  sed -e 's/ //g' '$@' | sort -u -o '$@'.tmp ; mv -f '$@'.tmp '$@'; fi

# create lef & def from DFII
ifeq ("$(GRAYBOX_MODE)", "")
.PRECIOUS: $(ROOT_TARGET_DIR)/%/extracted$(EXTRACT_DIR)/cell.lef
.PRECIOUS: $(ROOT_TARGET_DIR)/%/extracted$(EXTRACT_DIR)/cell.def
$(ROOT_TARGET_DIR)/%/extracted$(EXTRACT_DIR)/cell.lef $(ROOT_TARGET_DIR)/%/extracted$(EXTRACT_DIR)/cell.def: $(ROOT_TARGET_DIR)/%/df2.d
	#TASK=lefdefOut VIEW=$(call GET_NTH_FROM_LAST_DIR,$(@D),2) CELL=$(call GET_CAST_FULL_NAME,$(@D))
	if [[ ( -n "$(call LVE_SKIP,lefdef)" ) && ( -e '$@' ) ]] ; then exit; fi; \
	task=lefdef && $(CASTFILES_ENQUEUE_TASK) ; \
	mkdir -p '$(@D)' ; \
	QB_DIAG_FILE='$(@D)/lefdef.diag' QB_RUN_NAME='lve_lefdefWrite' \
	  QB_LOCAL=$(QB_LOCAL) QRSH_FLAGS="$(PACKAGE_FLAGS) \
	   $(EXEC)  $(IC_SCRIPT) $(LEFDEFWRITE) \
	  --working-dir=/scratch \
	  --fulcrum-pdk-root='$(FULCRUM_PDK_PACKAGE_ROOT)' \
	  --cast-path='$(CAST_PATH)' \
	  --view='$(call GET_NTH_FROM_LAST_DIR,$(@D),2)' \
	  --dfII-dir='$(DFII_DIR)' \
	  --cadence-log='$(@D)/lefdefWrite.log' \
	  --lef-output='$(@D)/lef.tmp' \
	  --def-output='$(@D)/def.tmp' \
	  --cell='$(call GET_CADENCE_BASE_NAME,$(@D))' ;\
	mv -f '$(@D)/lef.tmp' '$(@D)/cell.lef' &&\
	mv -f '$(@D)/def.tmp' '$(@D)/cell.def'; \
	task=lefdef && $(CASTFILES_DEQUEUE_TASK)
else # "$(GRAYBOX_MODE)" eq "" 23 lines back
.PRECIOUS: $(ROOT_TARGET_DIR)/%/extracted$(EXTRACT_DIR)/cell.lef
$(ROOT_TARGET_DIR)/%/extracted$(EXTRACT_DIR)/cell.lef: $(ROOT_TARGET_DIR)/%/df2.d $(ROOT_TARGET_DIR)/%/extracted$(EXTRACT_DIR)/graybox_list
	#TASK=lefWrite VIEW=$(call GET_NTH_FROM_LAST_DIR,$(@D),2) CELL=$(call GET_CAST_FULL_NAME,$(@D))
	task=lefWrite && $(CASTFILES_ENQUEUE_TASK) && \
	mkdir -p '$(@D)' ; \
	QB_DIAG_FILE='$(@D)/lefWrite.diag' QB_RUN_NAME='lve_lefWrite' \
	  QB_LOCAL=$(QB_LOCAL) QRSH_FLAGS="$(PACKAGE_FLAGS)" \
	   $(EXEC)  $(IC_SCRIPT) $(LEFWRITE) \
	  --root-target-dir=$(ROOT_TARGET_DIR) \
	  --global-target-dir=$(SUB_LVE_ROOT_DIR) \
	  --fulcrum-pdk-root='$(FULCRUM_PDK_PACKAGE_ROOT)' \
	  --cast-path='$(CAST_PATH)' \
	  --dfII-dir='$(DFII_DIR)' \
	  --cadence-log='$(@D)/lefWrite.log' \
	  --lef-output='$(@D)/lef.tmp' \
	  --big-lef='$(BIG_LEF)' \
	  --extracted-view='$(call GET_NTH_FROM_LAST_DIR,$(@D),2)' \
	  --cell-list='$(@D)/graybox_list' >'$(@D)/lefWrite.err';\
	mv -f '$(@D)/lef.tmp' '$(@D)/cell.lef';\
	task=lefWrite && $(CASTFILES_DEQUEUE_TASK)
endif # "$(GRAYBOX_MODE)" eq "" 44 lines back
# use lef renamer to translate lef to gdsII names.
ifeq ("$(LEFFILE)", "")
.PRECIOUS: $(ROOT_TARGET_DIR)/%/extracted$(EXTRACT_DIR)/cell_gds2.lef
$(ROOT_TARGET_DIR)/%/extracted$(EXTRACT_DIR)/cell_gds2.lef: $(ROOT_TARGET_DIR)/%/extracted$(EXTRACT_DIR)/cell.lef
	#TASK=lef_rename VIEW=$(call GET_NTH_FROM_LAST_DIR,$(@D),2) CELL=$(call GET_CAST_FULL_NAME,$(@D))
	mkdir -p '$(@D)' ; \
	   $(LEF_RENAME) \
	  --from="cadence" \
	  --to="gds2" \
	  --infile="$?" \
	  --outfile="$@" ;
else # "$(LEFFILE)" eq ""
.PRECIOUS: $(ROOT_TARGET_DIR)/%/extracted$(EXTRACT_DIR)/cell_gds2.lef
$(ROOT_TARGET_DIR)/%/extracted$(EXTRACT_DIR)/cell_gds2.lef: $(LEFFILE) $(LVELOG)
	#TASK=lef_rename VIEW=$(call GET_NTH_FROM_LAST_DIR,$(@D),2) CELL=$(call GET_CAST_FULL_NAME,$(@D))
	mkdir -p '$(@D)' ; \
	   $(LEF_RENAME) \
	  --from="cadence" \
	  --to="gds2" \
	  --infile=$(LEFFILE) \
	  --outfile="$@" ;
endif # "$(LEFFILE)" eq ""
ifeq ("$(DEFFILE)", "")
.PRECIOUS: $(ROOT_TARGET_DIR)/%/extracted$(EXTRACT_DIR)/cell_gds2.def
$(ROOT_TARGET_DIR)/%/extracted$(EXTRACT_DIR)/cell_gds2.def: $(ROOT_TARGET_DIR)/%/extracted$(EXTRACT_DIR)/cell.def
	#TASK=def_rename VIEW=$(call GET_NTH_FROM_LAST_DIR,$(@D),2) CELL=$(call GET_CAST_FULL_NAME,$(@D))
	mkdir -p '$(@D)' ; \
	   $(DEF_RENAME) \
	  --from="cadence" \
	  --to="gds2" \
	  --infile="$?" \
	  --outfile="$@" ;
else # "$(DEFFILE)" eq ""
.PRECIOUS: $(ROOT_TARGET_DIR)/%/extracted$(EXTRACT_DIR)/cell_gds.def
$(ROOT_TARGET_DIR)/%/extracted$(EXTRACT_DIR)/cell_gds2.def: $(DEFFILE) $(LVELOG)
	#TASK=def_rename VIEW=$(call GET_NTH_FROM_LAST_DIR,$(@D),2) CELL=$(call GET_CAST_FULL_NAME,$(@D))
	mkdir -p '$(@D)' ; \
	   $(DEF_RENAME) \
	  --from="cadence" \
	  --to="gds2" \
	  --infile=$(DEFFILE) \
	  --outfile="$@" ;
endif # "$(DEFFILE)" eq ""
# create gds2 from DFII and cast
.PRECIOUS: $(ROOT_TARGET_DIR)/%/$(GDS2_TARGET)
.PRECIOUS: $(ROOT_TARGET_DIR)/%/cell.bindrul

ifneq ("$(NOEXTRACTDEPS)", "1")
$(ROOT_TARGET_DIR)/%/$(GDS2_TARGET) $(ROOT_TARGET_DIR)/%/cell.bindrul: $$(call CANONICALIZE_PATH,$(ROOT_TARGET_DIR)/%/../cell.cdl) $(ROOT_TARGET_DIR)/%/df2.d
	#TASK=gds2 VIEW=$(call GET_NTH_FROM_LAST_DIR,$(@D),1) CELL=$(call GET_CAST_FULL_NAME,$(@D))
	/bin/rm -f "$(@D)/$(GDS2_TARGET)"
	if [[ ( -n "$(call LVE_SKIP,gds2)" ) && ( -e '$@' ) ]] ; then exit; fi; \
	task=gds2 && $(CASTFILES_ENQUEUE_TASK) ; \
	working_dir=`mktemp -d "$(WORKING_DIR)/gds2.XXXXXX"`; \
	sync; \
	sleep 1; \
	( [[ $(IGNORE_DANGLING_PIN_CHECK) == 1 ]] || \
	QB_DIAG_FILE='$(@D)/$(GDS2_TARGET).diag1' QB_RUN_NAME='lve_dangling' \
	  QB_LOCAL=$(QB_LOCAL) QRSH_FLAGS="$(PACKAGE_FLAGS)" \
	   $(EXEC) $(IC_SCRIPT) check_dummy_pin \
          --fulcrum-pdk-root='$(FULCRUM_PDK_PACKAGE_ROOT)' \
          --cast-path='$(CAST_PATH)' \
          --view='$(call GET_NTH_FROM_LAST_DIR,$(@D),1)' \
          --dfII-dir='$(DFII_DIR)' \
          --cds-log='$(@D)/dangling_node.log' \
          --cell='$(call GET_CAST_FULL_NAME,$(@D))' \
          --outfile='$(@D)/dangling_node.out' \
          --java-flag=$(GLOBAL_JAVA_FLAGS) \
          --cdl='$(@D)/../cell.cdl' ) && \
	QB_DIAG_FILE='$(@D)/$(GDS2_TARGET).diag2' QB_RUN_NAME='lve_gdsIIWrite' \
	  QB_LOCAL=$(QB_LOCAL) QRSH_FLAGS="$(PACKAGE_FLAGS)" \
	   $(EXEC) $(IC_SCRIPT) $(GDSIIWRITE) \
	  --working-dir="$$working_dir" \
          --fulcrum-pdk-root='$(FULCRUM_PDK_PACKAGE_ROOT)' \
          --cast-path='$(CAST_PATH)' \
          --view='$(call GET_NTH_FROM_LAST_DIR,$(@D),1)' \
          --dfII-dir='$(DFII_DIR)' \
          --cadence-log='$(@D)/gdsIIWrite.log' \
          --assura-log='$(@D)/partial.log' \
          --output='$(@D)/$(GDS2_TARGET).tmp' \
          --bind-rul='$(@D)/cell.bindrul.tmp' \
          --cell='$(call GET_CAST_FULL_NAME,$(@D))' \
          --noproperties \
          --output-root-cell-name='$(call GET_GDS2_CDL_NAME,$(@D))' \
          --65mode=$(SIXTYFIVEMODE) \
          $(GDS_USE_TAG) \
          --tag-orientation=$(GDS_TAG_ORIENTATION) \
          --flatten-pcells && \
	isgds '$(@D)/$(GDS2_TARGET).tmp' && \
	mv -f '$(@D)/$(GDS2_TARGET).tmp' '$(@D)/$(GDS2_TARGET)' &&\
	mv -f '$(@D)/cell.bindrul.tmp' '$(@D)/cell.bindrul'; \
	sleep 1; \
	cd / && ( [[ $(DELETE_EXTRACT_DIR) == 0 ]] || rm -rf "$$working_dir" ); \
	task=gds2 && $(CASTFILES_DEQUEUE_TASK)

endif # "$(NOEXTRACTDEPS)" ne "1" 48 lines back

# create gds2 from DFII and cast
GDS2_TARGET10x := cell.gds2_lambda2
.PRECIOUS: $(ROOT_TARGET_DIR)/%/$(GDS2_TARGET10x)
.PRECIOUS: $(ROOT_TARGET_DIR)/%/cell.bindrul
$(ROOT_TARGET_DIR)/%/cell.gds2_lambda2 $(ROOT_TARGET_DIR)/%/cell.bindrul: $$(call CANONICALIZE_PATH,$(ROOT_TARGET_DIR)/%/../cell.cdl) $(ROOT_TARGET_DIR)/%/df2.d
	#TASK=gds2_10x VIEW=$(call GET_NTH_FROM_LAST_DIR,$(@D),1) CELL=$(call GET_CAST_FULL_NAME,$(@D))
	if [[ ( -n "$(call LVE_SKIP,gds2)" ) && ( -e '$@' ) ]] ; then exit; fi; \
	task=gds2_10x && $(CASTFILES_ENQUEUE_TASK) ; \
	working_dir=`mktemp -d "$(WORKING_DIR)/gds2.XXXXXX"`; \
	sync; \
	sleep 1; \
	QB_DIAG_FILE='$(@D)/$(GDS2_TARGET10x).diag' QB_RUN_NAME='lve_gdsIIWrite' \
	  QB_LOCAL=$(QB_LOCAL) QRSH_FLAGS="$(PACKAGE_FLAGS)" \
	   $(EXEC) $(IC_SCRIPT) $(GDSIIWRITE) \
	  --working-dir="$$working_dir" \
          --fulcrum-pdk-root='$(FULCRUM_PDK_PACKAGE_ROOT)' \
          --cast-path='$(CAST_PATH)' \
          --view='$(call GET_NTH_FROM_LAST_DIR,$(@D),1)' \
          --dfII-dir='$(DFII_DIR)' \
	  --scale=0.0001 \
          --cadence-log='$(@D)/cadence.log' \
          --assura-log='$(@D)/partial.log' \
          --output='$(@D)/$(GDS2_TARGET10x).tmp' \
          --bind-rul='$(@D)/cell.bindrul.tmp' \
          --cell='$(call GET_CAST_FULL_NAME,$(@D))' \
          --output-root-cell-name='$(call GET_GDS2_CDL_NAME,$(@D))' \
          --noproperties \
          --flatten-pcells && \
	isgds '$(@D)/$(GDS2_TARGET10x).tmp' && \
	mv -f '$(@D)/$(GDS2_TARGET10x).tmp' '$(@D)/$(GDS2_TARGET10x)' &&\
	cp '$(@D)/cell.bindrul.tmp' '$(@D)/cell.bindrul'; \
	cd / && ( [[ $(DELETE_EXTRACT_DIR) == 0 ]] || rm -rf "$$working_dir" ); \
	task=gds2_10x && $(CASTFILES_DEQUEUE_TASK)


# use mk_instance to extract cell geometry
.PRECIOUS: $(ROOT_TARGET_DIR)/%/estimated/instances/.instances
.PRECIOUS: $(ROOT_TARGET_DIR)/%/extracted/instances/.instances
ifneq ("$(NOEXTRACTDEPS)", "1")
$(ROOT_TARGET_DIR)/%/estimated/instances/.instances \
   $(ROOT_TARGET_DIR)/%/extracted/instances/.instances: $(ROOT_TARGET_DIR)/%/df2.d
	#TASK=mk_instances VIEW=$(call GET_NTH_FROM_LAST_DIR,$(@D),3) CELL=$(call GET_CAST_FULL_NAME,$(@D))
	if [[ ( -n "$(call LVE_SKIP,extract)" ) && ( -e '$@' ) ]] ; then exit; fi; \
	mkdir -p '$(@D)'; \
	$(CASTFILES_ENQUEUE_TASK) && \
	working_dir=`mktemp -d "$(WORKING_DIR)/cdswd.XXXXXX"`; \
	sync; \
	sleep 1; \
	chmod 2775 "$$working_dir" && cd "$$working_dir" && \
	$(MKCDSWD) \
	  --dfII-dir=$(DFII_DIR) \
	  --fulcrum-pdk-root=$(FULCRUM_PDK_PACKAGE_ROOT) \
	  --target-dir=$$working_dir \
	  --force \
	  --temp && \
	QB_DIAG_FILE='$@.diag' QB_RUN_NAME='lve_mk_instance' \
	  $(EXEC_PACKAGE) $(MK_INSTANCE) \
	  --fulcrum-pdk-root=$(FULCRUM_PDK_PACKAGE_ROOT) \
	  --cell="$(call GET_CAST_BASE_NAME,$(@D))" \
	  --cadence-log='$(@D)/cadence.log' \
	  --view=$(call GET_NTH_FROM_LAST_DIR,$(@D),3) \
	  --outdir='$(@D)' >/dev/null \
	&& touch '$@' && cd / && rm -rf "$$working_dir"; \
	$(CASTFILES_DEQUEUE_TASK)
endif # "$(NOEXTRACTDEPS)" ne "1" 26 lines back

# use Jauto to estimate RC parasitics
.PRECIOUS: $(ROOT_TARGET_DIR)/%/estimated/cell.spice
.PRECIOUS: $(ROOT_TARGET_DIR)/%/estimated/extract.result
$(ROOT_TARGET_DIR)/%/estimated/cell.spice \
$(ROOT_TARGET_DIR)/%/estimated/extract.result: \
		$(ROOT_TARGET_DIR)/%/cast.changed \
		$(ROOT_TARGET_DIR)/%/df2.d \
		$(ROOT_TARGET_DIR)/%/estimated/instances/.instances
	#TASK=extract VIEW=$(call GET_NTH_FROM_LAST_DIR,$(@D),2) MODE=estimated CELL=$(call GET_CAST_FULL_NAME,$(@D))
	if [[ ( -n "$(call LVE_SKIP,extract)" ) && ( -e '$@' ) ]] ; then exit; fi; \
	task=extract && $(CASTFILES_ENQUEUE_TASK) && \
	subtype=$$(echo '$(call GET_CAST_BASE_NAME,$(@D))' | $(GNUSED) -e 's,^.*\.,,'); \
	cell=$$(echo '$(call GET_CAST_BASE_NAME,$(@D))' | $(GNUSED) -e 's,\.[^.]*$$,,'); \
	fqcnminus=$$(echo '$(call GET_CAST_FULL_NAME,$(@D))'); \
	working_dir=`mktemp -d "$(WORKING_DIR)/jauto.XXXXXX"`; \
	sync; \
	sleep 1; \
	chmod 2775 "$$working_dir" && cd "$$working_dir" && \
	LS_COLORS= \
	QB_DIAG_FILE='$(@D)/cell.spice.diag' QB_RUN_NAME='lve_jauto' \
	  $(EXEC_PACKAGE) $(JAUTO) \
	  --config=$(FULCRUM_PDK_PACKAGE_ROOT)/share/Fulcrum/jauto/jauto.config \
	  --config=$(FULCRUM_PDK_PACKAGE_ROOT)/share/Fulcrum/jauto/process.config \
	  --cellName="$$cell" \
	  --subtype="$$subtype" \
	  --partialExtract="$$fqcnminus" \
	  --tau=0 \
	  --castInRoot='$(CAST_PATH)' \
	  --cdlRoot="$$working_dir" \
	  --outRoot="$$working_dir" \
          --layoutRoot='$(@D)/instances' \
          --layoutScaleX=$(LAYOUT_SCALE_X) \
          --layoutScaleY=$(LAYOUT_SCALE_Y) \
          --package-root=$(LVE_PACKAGE_ROOT) \
          --new-spice \
          --spice-output=cell.spice \
          --mode=size \
          --trial \
	  --skipSizing \
	  --updateDirective \
	  --disableAutomaticSizingEnvironment \
	  $(JAUTOSKIP) \
	  $(CUSTOM_JAUTO_CONFIG) && \
	mv "$$working_dir/cell.spice" '$(@D)' && \
	mv "$$working_dir/electromigration_localnets.debug" '$(@D)' && \
	mv "$$working_dir/electromigration_operators.debug" '$(@D)' && \
	mv "$$working_dir/wires.debug" '$(@D)' && \
	cd / && ( [[ $(DELETE_EXTRACT_DIR) == 0 ]] || rm -rf "$$working_dir" ) && \
	echo PASS > '$(@D)/extract.result' || \
	echo FAIL > '$(@D)/extract.result'; \
	task=extract && $(CASTFILES_DEQUEUE_TASK)

# use Jauto to emit netlist without estimated RC parasitics
.PRECIOUS: $(ROOT_TARGET_DIR)/%/nogeometry/cell.spice
.PRECIOUS: $(ROOT_TARGET_DIR)/%/nogeometry/extract.result
$(ROOT_TARGET_DIR)/%/nogeometry/cell.spice \
$(ROOT_TARGET_DIR)/%/nogeometry/extract.result: $(ROOT_TARGET_DIR)/%/cast.changed
	#TASK=extract VIEW=$(call GET_NTH_FROM_LAST_DIR,$(@D),2) MODE=nogeometry CELL=$(call GET_CAST_FULL_NAME,$(@D))
	mkdir -p '$(@D)'
	if [[ ( -n "$(call LVE_SKIP,extract)" ) && ( -e '$@' ) ]] ; then exit; fi; \
	task=extract && $(CASTFILES_ENQUEUE_TASK) && \
	subtype=$$(echo '$(call GET_CAST_BASE_NAME,$(@D))' | $(GNUSED) -e 's,^.*\.,,'); \
	cell=$$(echo '$(call GET_CAST_BASE_NAME,$(@D))' | $(GNUSED) -e 's,\.[^.]*$$,,'); \
	fqcnminus=$$(echo '$(call GET_CAST_FULL_NAME,$(@D))'); \
	working_dir=`mktemp -d "$(WORKING_DIR)/jauto.XXXXXX"`; \
	sync; \
	sleep 1; \
	chmod 2775 "$$working_dir" && cd "$$working_dir" && \
	LS_COLORS= \
	QB_DIAG_FILE='$(@D)/cell.spice.diag' QB_RUN_NAME='lve_jauto' \
	  $(EXEC_PACKAGE) $(JAUTO) \
	  --config=$(FULCRUM_PDK_PACKAGE_ROOT)/share/Fulcrum/jauto/jauto.config \
	  --config=$(FULCRUM_PDK_PACKAGE_ROOT)/share/Fulcrum/jauto/process.config \
	  --cellName="$$cell" \
	  --subtype="$$subtype" \
	  --partialExtract="$$fqcnminus" \
	  --tau=0 \
	  --castInRoot='$(CAST_PATH)' \
	  --cdlRoot="$$working_dir" \
	  --outRoot="$$working_dir" \
          --package-root=$(LVE_PACKAGE_ROOT) \
          --new-spice \
          --spice-output=cell.spice \
          --mode=size \
          --trial \
	  --skipSizing \
	  --updateDirective \
	  --noFloorplan \
	  --disableAutomaticSizingEnvironment \
	  $(JAUTOSKIP) \
	  $(CUSTOM_JAUTO_CONFIG) && \
	mv "$$working_dir/cell.spice" '$(@D)' && \
	cd / && ( [[ $(DELETE_EXTRACT_DIR) == 0 ]] || rm -rf "$$working_dir" ) && \
	echo PASS > '$(@D)/extract.result' || \
	echo FAIL > '$(@D)/extract.result'; \
	task=extract && $(CASTFILES_DEQUEUE_TASK)

# use the user supplied custom netlist as is:
.PRECIOUS: $(ROOT_TARGET_DIR)/%/custom/cell.spice
.PRECIOUS: $(ROOT_TARGET_DIR)/%/custom/extract.result
$(ROOT_TARGET_DIR)/%/custom/cell.spice \
$(ROOT_TARGET_DIR)/%/custom/cell.spice_include \
$(ROOT_TARGET_DIR)/%/custom/extract.result: $(CUSTOM_SPICE)
	#TASK=extract VIEW=$(call GET_NTH_FROM_LAST_DIR,$(@D),2) MODE=custom CELL=$(call GET_CAST_FULL_NAME,$(@D))
	mkdir -p '$(@D)'
	if [[ ( -n "$(call LVE_SKIP,extract)" ) && ( -e '$@' ) ]] ; then exit; fi; \
	task=extract && $(CASTFILES_ENQUEUE_TASK) && \
	cp '$(CUSTOM_SPICE)' '$(@D)/cell.spice' && \
	cp '$(CUSTOM_SPICE_INCLUDE)' '$(@D)/cell.spice_include' && \
	echo PASS > '$(@D)/extract.result' || \
	echo FAIL > '$(@D)/extract.result'; \
	task=extract && $(CASTFILES_DEQUEUE_TASK)

# rename cdl to gds2 names
.PRECIOUS: $(ROOT_TARGET_DIR)/%/cell.cdl_gds2
$(ROOT_TARGET_DIR)/%/cell.cdl_gds2: $$(call CANONICALIZE_PATH,$(ROOT_TARGET_DIR)/%/../cell.cdl)
	$(CASTFILES_ENQUEUE_TASK) && \
	QB_DIAG_FILE='$@.diag' QB_RUN_NAME='lve_rename' \
	$(EXEC_LOW_PACKAGE) $(CDL_RENAMER) \
	  --source-cdl-file='$<' \
	  --name-in=cadence \
	  --name-out=gds2 \
	  --translated-cdl='$@.tmp' \
	  --call-delimiter= \
	&& mv -f '$@.tmp' '$@'; \
	$(CASTFILES_DEQUEUE_TASK)

.PRECIOUS: $(ROOT_TARGET_DIR)/%/cell.cdl_pmc
$(ROOT_TARGET_DIR)/%/cell.cdl_pmc: $(ROOT_TARGET_DIR)/%/cell.cdl_gds2
	$(CASTFILES_ENQUEUE_TASK) && \
	QB_DIAG_FILE='$@.diag' QB_RUN_NAME='lve_rename' \
	$(EXEC_LOW_PACKAGE) $(CDL_RENAMER) \
	  --source-cdl-file='$<' \
	  --name-in=gds2 \
	  --name-out=pmc_hack \
	  --translated-cdl='$@.tmp' \
	&& mv -f '$@.tmp' '$@'; \
	$(CASTFILES_DEQUEUE_TASK)

# spef generation
# use STAR_RC to extract
.PRECIOUS: $(ROOT_TARGET_DIR)/%/extracted$(EXTRACT_DIR)/spef.err
.PRECIOUS: $(ROOT_TARGET_DIR)/%/extracted$(EXTRACT_DIR)/cell.spef_gds2
.PRECIOUS: $(ROOT_TARGET_DIR)/%/extracted$(EXTRACT_DIR)/cell.spef

$(ROOT_TARGET_DIR)/%/extracted$(EXTRACT_DIR)/cell.spef :\
        $(ROOT_TARGET_DIR)/%/extracted$(EXTRACT_DIR)/cell.spef_gds2
	QRSH_FLAGS="$(PACKAGE_LOW_FLAGS) $(EXTRACT_FLAGS)" \
	  QB_DIAG_FILE="$(@D)/cell.extract_spefrn.diag" QB_RUN_NAME='lve_spefrn' \
	  $(QB) spefrename --from=gds2 --to=cast '$<' '$@.tmp' \
	&& mv '$@.tmp' '$@' \
	&& : < '$@'

EXTRACT_COMMON_OPTIONS=--64bit=$(BIT64) \
	--ccp=$(CCP) \
	--cdl-cell-name="$$cell" \
	--cdl-file='$(@D)/../cell.cdl_gds2' \
	--dfII-dir='$(DFII_DIR)' \
	--extract-corner='$(EXTRACT_CORNER)' \
	--extract-power=$(EXTRACTPOWER) \
	--extracted-view='$(EXTRACTED_VIEW)' \
	--fulcrum-pdk-root=$(FULCRUM_PDK_PACKAGE_EXTRACT_ROOT) \
	--fixbulk=$(GET_TRUE_IGNORE_BULK) \
	--gds2-file='$(@D)/../cell.gds2' \
	--gray-cell-list='$(@D)/graybox_list' \
	--ignore-nvn=$(IGNORE_NVN) \
	--instance-port=$(INSTANCE_PORT) \
	--maxF=$(MAXF) \
	--minC=$(MINEXTRACTEDC) \
	--minR=$(MINEXTRACTEDR) \
	--nvn-log='$(@D)/nvn.log' \
	--swappin-log='$(@D)/swappin.err' \
	--temperature=$(TEMPERATURE) \
	--extra-temperature='$(EXTRA_TEMPERATURE)' \
	--working-dir=$$extract_dir \
	--node-props='$(@D)/../../cell.nodeprops$(ROUTED_SUFFIX)$(ACCURATE_SUFFIX)'

$(ROOT_TARGET_DIR)/%/extracted$(EXTRACT_DIR)/cell.spef_gds2 \
$(ROOT_TARGET_DIR)/%/extracted$(EXTRACT_DIR)/spef.err: \
        $(ROOT_TARGET_DIR)/%/cell.gds2 \
        $(ROOT_TARGET_DIR)/%/cell.cdl_gds2 \
        $(ROOT_TARGET_DIR)/%/extracted$(EXTRACT_DIR)/graybox_list \
        $(ROOT_TARGET_DIR)/%/extracted$(EXTRACT_DIR)/cdl.aliases \
        $(ROOT_TARGET_DIR)/%/extracted$(EXTRACT_DIR)/cdl.aliases.routed \
        $$(call CANONICALIZE_PATH,$(ROOT_TARGET_DIR)/%/../cell.nodeprops$(ROUTED_SUFFIX)$(ACCURATE_SUFFIX))
	#TASK=extract_spefRC MODE=extracted$(EXTRACT_DIR) VIEW=$(call GET_NTH_FROM_LAST_DIR,$(@D),2) CELL=$(call GET_CAST_FULL_NAME,$(@D))
	mkdir -p '$(@D)'
	status=0; \
	task=extract && $(CASTFILES_ENQUEUE_TASK) && \
	cell=$$(echo '$(call GET_GDS2_CDL_NAME,$(@D))' ); \
	extract_dir=`mktemp -d "$(WORKING_DIR)/$$cell.XXXXXX"`; \
	sync; \
	sleep 1; \
	QRSH_FLAGS="$(PACKAGE_FLAGS) ,cc=$(DRCLVS_THREADS) $(EXTRACT_FLAGS)" \
	  QB_DIAG_FILE="$(@D)/cell.extract_hercules.diag" QB_RUN_NAME='lve_spef1' \
	  $(QB) $(EXTRACT_STARRC) $(EXTRACT_COMMON_OPTIONS) \
	  --blackbox=1 \
	  --threads=$(DRCLVS_THREADS) \
	  --delete=$(DELETE_EXTRACT_DIR) \
	  --spef=1 \
	  --spice-target='$(@D)/cell.spef_gds2.tmp' \
	  --spice-topcell='$(@D)/cell.spef_topcell' \
	  --extra-extract-equiv='$(EXTRA_EQUIV)' \
	  --lvs-extra-options='$(LVS_EXTRA_OPTIONS)' \
	  --task='stage1' \
	  '$(call GET_GDS2_CDL_NAME,$(@D))' 2>&1 > '$(@D)/spef.err' \
	&& QRSH_FLAGS="$(PACKAGE_LOW_FLAGS) $(EXTRACT_FLAGS)" \
	  QB_DIAG_FILE="$(@D)/cell.extract_spef2a.diag" QB_RUN_NAME='lve_spef2a' \
	  $(QB) $(EXTRACT_STARRC) $(EXTRACT_COMMON_OPTIONS) \
	  --blackbox=1 \
	  --delete=$(DELETE_EXTRACT_DIR) \
	  --spef=1 \
	  --spice-target='$(@D)/cell.spef_gds2.tmp' \
	  --spice-topcell='$(@D)/cell.spef_topcell' \
	  --task='stage2a' \
	  '$(call GET_GDS2_CDL_NAME,$(@D))' 2>&1 >> '$(@D)/spef.err' \
	&& QRSH_FLAGS="$(PACKAGE_FLAGS) -l starrc=1,cc=$(STARRC_THREADS) $(EXTRACT_FLAGS)" \
	  QB_DIAG_FILE="$(@D)/cell.extract_spef2b.diag" QB_RUN_NAME='lve_spef2b' \
	  $(QB) $(EXTRACT_STARRC) $(EXTRACT_COMMON_OPTIONS) \
	  --blackbox=1 \
	  --threads=$(STARRC_THREADS) \
	  --delete=$(DELETE_EXTRACT_DIR) \
	  --spef=1 \
	  --spice-target='$(@D)/cell.spef_gds2.tmp' \
	  --spice-topcell='$(@D)/cell.spef_topcell' \
	  --totem-mode=0 \
	  --task='stage2b' \
	  '$(call GET_GDS2_CDL_NAME,$(@D))' 2>&1 >> '$(@D)/spef.err' \
	&& QRSH_FLAGS="$(PACKAGE_LOW_FLAGS) $(EXTRACT_FLAGS)" \
	  QB_DIAG_FILE="$(@D)/cell.extract_spef2c.diag" QB_RUN_NAME='lve_spef2c' \
	  $(QB) $(EXTRACT_STARRC) $(EXTRACT_COMMON_OPTIONS) \
	  --blackbox=1 \
	  --delete=$(DELETE_EXTRACT_DIR) \
	  --spef=1 \
	  --spice-target='$(@D)/cell.spef_gds2.tmp' \
	  --spice-topcell='$(@D)/cell.spef_topcell' \
	  --alias-file='$(@D)/cdl.aliases' \
	  --routed-alias-file='$(@D)/cdl.aliases.routed' \
	  --task='stage2c' \
	  '$(call GET_GDS2_CDL_NAME,$(@D))' 2>&1 >> '$(@D)/spef.err' && \
	/bin/mv -f "$(@D)/cell.spef_gds2.tmp" "$(@D)/cell.spef_gds2" && \
	for t in $(EXTRA_TEMPERATURE); do \
		/bin/mv -f "$(@D)/cell.spef_gds2.tmp.$$t" "$(@D)/cell.spef_gds2.$$t"; \
	done; \
	if ( grep -q "\<Error" "$(@D)/spef.err" ) ; then \
		grep "\<Error" "$(@D)/spef.err" >&2 ; \
		rm -f "$(@D)/cell.spef_gds2"; touch "$(@D)/cell.spef_gds2"; \
	fi; \
	exit $$status

# use STAR_RC to extract
ifeq ("$(GRAYBOX_MODE)", "")
.PRECIOUS: $(ROOT_TARGET_DIR)/%/accurate$(EXTRACT_DIR)/extract.err
.PRECIOUS: $(ROOT_TARGET_DIR)/%/accurate$(EXTRACT_DIR)/cell.spice_gds2
.PRECIOUS: $(ROOT_TARGET_DIR)/%/accurate$(EXTRACT_DIR)/cell.spice_topcell
.PRECIOUS: $(ROOT_TARGET_DIR)/%/extracted$(EXTRACT_DIR)/extract.err
.PRECIOUS: $(ROOT_TARGET_DIR)/%/extracted$(EXTRACT_DIR)/cell.spice_gds2
.PRECIOUS: $(ROOT_TARGET_DIR)/%/extracted$(EXTRACT_DIR)/cell.spice_topcell
.PRECIOUS: $(ROOT_TARGET_DIR)/%/totem$(EXTRACT_DIR)/extract.err
.PRECIOUS: $(ROOT_TARGET_DIR)/%/totem$(EXTRACT_DIR)/cell.spice_gds2
.PRECIOUS: $(ROOT_TARGET_DIR)/%/totem$(EXTRACT_DIR)/cell.spf
.PRECIOUS: $(ROOT_TARGET_DIR)/%/nanotime$(EXTRACT_DIR)/extract.err
.PRECIOUS: $(ROOT_TARGET_DIR)/%/nanotime$(EXTRACT_DIR)/cell.dpf
.PRECIOUS: $(ROOT_TARGET_DIR)/%/nanotime$(EXTRACT_DIR)/cell.spef
$(ROOT_TARGET_DIR)/%/extracted$(EXTRACT_DIR)/cell.spice_gds2 \
$(ROOT_TARGET_DIR)/%/extracted$(EXTRACT_DIR)/extract.err: \
        $(ROOT_TARGET_DIR)/%/cell.gds2 \
        $(ROOT_TARGET_DIR)/%/cell.cdl_gds2 \
        $$(call CANONICALIZE_PATH,$(ROOT_TARGET_DIR)/%/../cell.nodeprops$(ROUTED_SUFFIX)$(ACCURATE_SUFFIX))
	if [[ ( -n "$(call LVE_SKIP,extract)" ) && ( -e '$@' ) ]] ; then exit; fi; \
	echo "#TASK=extract_starRC MODE=extracted$(EXTRACT_DIR) VIEW=$(call GET_NTH_FROM_LAST_DIR,$(@D),2) CELL=$(call GET_CAST_FULL_NAME,$(@D))"; \
	mkdir -p '$(@D)'; \
	status=0; \
	task=extract && $(CASTFILES_ENQUEUE_TASK) && \
	cell=$$(echo '$(call GET_GDS2_CDL_NAME,$(@D))' ); \
	extract_dir=`mktemp -d "$(WORKING_DIR)/$$cell.XXXXXX"`; \
	if [[ -f "$(@D)/graybox_list" ]]; then /bin/mv -f "$(@D)/graybox_list" "$(@D)/graybox_list.orig"; fi; \
	sync; \
	sleep 1; \
	QRSH_FLAGS="$(PACKAGE_FLAGS),cc=$(DRCLVS_THREADS) $(EXTRACT_FLAGS)" \
	  QB_DIAG_FILE="$(@D)/cell.extract_hercules.diag" QB_RUN_NAME='lve_starRC1' \
	  $(QB) $(EXTRACT_STARRC) $(EXTRACT_COMMON_OPTIONS) \
	  --blackbox=$(LVS_BLACKBOX) \
	  --threads=$(DRCLVS_THREADS) \
	  --delete=$(DELETE_EXTRACT_DIR) \
	  --extractReduce=$(REDUCE_MODE) \
	  --spice-target='$(@D)/cell.spice_gds2.tmp' \
	  --spice-topcell='$(@D)/cell.spice_topcell' \
	  --extra-extract-equiv='$(EXTRA_EQUIV)' \
	  --lvs-extra-options='$(LVS_EXTRA_OPTIONS)' \
	  --task='stage1' \
	  '$(call GET_GDS2_CDL_NAME,$(@D))' &>'$(@D)/extract.err' \
	&& QRSH_FLAGS="$(PACKAGE_LOW_FLAGS) $(EXTRACT_FLAGS)" \
	  QB_DIAG_FILE="$(@D)/cell.extract_starRC2a.diag" QB_RUN_NAME='lve_starRC2a' \
	  $(QB) $(EXTRACT_STARRC) $(EXTRACT_COMMON_OPTIONS) \
	  --blackbox=$(LVS_BLACKBOX) \
	  --delete=$(DELETE_EXTRACT_DIR) \
	  --extractReduce=$(REDUCE_MODE) \
	  --spice-target='$(@D)/cell.spice_gds2.tmp' \
	  --spice-topcell='$(@D)/cell.spice_topcell' \
	  --task='stage2a' \
	  '$(call GET_GDS2_CDL_NAME,$(@D))' 2>&1 >> '$(@D)/extract.err' \
	&& QRSH_FLAGS="$(PACKAGE_FLAGS_starRC2b) -l starrc=1,cc=$(STARRC_THREADS) $(EXTRACT_FLAGS)" \
	  QB_DIAG_FILE="$(@D)/cell.extract_starRC2b.diag" QB_RUN_NAME='lve_starRC2b' \
	  $(QB) $(EXTRACT_STARRC) $(EXTRACT_COMMON_OPTIONS) \
	  --blackbox=$(LVS_BLACKBOX) \
	  --threads=$(STARRC_THREADS) \
	  --delete=$(DELETE_EXTRACT_DIR) \
	  --extractReduce=$(REDUCE_MODE) \
	  --spice-target='$(@D)/cell.spice_gds2.tmp' \
	  --spice-topcell='$(@D)/cell.spice_topcell' \
	  --totem-mode=0 \
	  --task='stage2b' \
	  '$(call GET_GDS2_CDL_NAME,$(@D))' 2>&1 >> '$(@D)/extract.err' \
	&& QRSH_FLAGS="$(PACKAGE_LOW_FLAGS) $(EXTRACT_FLAGS)" \
	  QB_DIAG_FILE="$(@D)/cell.extract_starRC2c.diag" QB_RUN_NAME='lve_starRC2c' \
	  $(QB) $(EXTRACT_STARRC) $(EXTRACT_COMMON_OPTIONS) \
	  --blackbox=$(LVS_BLACKBOX) \
	  --delete=$(DELETE_EXTRACT_DIR) \
	  --extractReduce=$(REDUCE_MODE) \
	  --spice-target='$(@D)/cell.spice_gds2.tmp' \
	  --spice-topcell='$(@D)/cell.spice_topcell' \
	  --task='stage2c' \
	  '$(call GET_GDS2_CDL_NAME,$(@D))' 2>&1 >> '$(@D)/extract.err' \
	&& QRSH_FLAGS="$(PACKAGE_FLAGS) $(EXTRACT_FLAGS)" \
	  QB_DIAG_FILE="$(@D)/cell.extract_starRC3.diag" QB_RUN_NAME='lve_starRC3a' \
	  $(QB) $(EXTRACT_STARRC) $(EXTRACT_COMMON_OPTIONS) \
	  --blackbox=$(LVS_BLACKBOX) \
	  --delete=$(DELETE_EXTRACT_DIR) \
	  --extractReduce=$(REDUCE_MODE) \
	  --spice-target='$(@D)/cell.spice_gds2.tmp' \
	  --spice-topcell='$(@D)/cell.spice_topcell' \
	  --task='stage3a' \
	  '$(call GET_GDS2_CDL_NAME,$(@D))' 2>&1 >> '$(@D)/extract.err' \
	&& QRSH_FLAGS="$(PACKAGE_FLAGS) -l lvs=1 $(EXTRACT_FLAGS)" \
	  QB_DIAG_FILE="$(@D)/cell.extract_starRC3.diag" QB_RUN_NAME='lve_starRC3b' \
	  $(QB) $(EXTRACT_STARRC) $(EXTRACT_COMMON_OPTIONS) \
	  --blackbox=$(LVS_BLACKBOX) \
	  --delete=$(DELETE_EXTRACT_DIR) \
	  --extractReduce=$(REDUCE_MODE) \
	  --spice-target='$(@D)/cell.spice_gds2.tmp' \
	  --spice-topcell='$(@D)/cell.spice_topcell' \
	  --task='stage3b' \
	  '$(call GET_GDS2_CDL_NAME,$(@D))' 2>&1 >> '$(@D)/extract.err' \
	&& QRSH_FLAGS="$(PACKAGE_LOW_FLAGS) $(EXTRACT_FLAGS)" \
	  QB_DIAG_FILE="$(@D)/cell.extract_starRC3.diag" QB_RUN_NAME='lve_starRC3c' \
	  $(QB) $(EXTRACT_STARRC) $(EXTRACT_COMMON_OPTIONS) \
	  --blackbox=$(LVS_BLACKBOX) \
	  --delete=$(DELETE_EXTRACT_DIR) \
	  --extractReduce=$(REDUCE_MODE) \
	  --spice-target='$(@D)/cell.spice_gds2.tmp' \
	  --spice-topcell='$(@D)/cell.spice_topcell' \
	  --task='stage3c' \
	  '$(call GET_GDS2_CDL_NAME,$(@D))' 2>&1 >> '$(@D)/extract.err' && \
	if [[ -f "$(@D)/cell.spice_gds2.tmp" ]]; then mv -f "$(@D)/cell.spice_gds2.tmp" "$(@D)/cell.spice_gds2"; fi; \
	if [ $$? == 0 ] ; then \
		touch '$(@D)/cell.spice_gds2'; \
	elif [[ $(IGNORE_NVN) == 0 ]] && grep -q 'NVN FAILED' "$(@D)/extract.err" ; then \
		rm -f "$(@D)/cell.spice_gds2" ; touch "$(@D)/cell.spice_gds2"; \
	fi; \
	if ( grep -q "\<Error" "$(@D)/extract.err" ) ; then \
		grep "\<Error" "$(@D)/extract.err" >&2 ; \
		rm -f "$(@D)/cell.spice_gds2"; touch "$(@D)/cell.spice_gds2"; \
	fi; \
	mkdir -p "$(ROOT_TARGET_DIR)/spicelib"; \
	QRSH_FLAGS="$(PACKAGE_LOW_FLAGS) $(EXTRACT_FLAGS)" \
	  QB_DIAG_FILE='$(@D)/cell.include.diag' QB_RUN_NAME='lve_geninc' \
	  $(QB) $(SPICE_REDUCE) --infile="$(@D)/cell.spice_gds2" --outfile="$(ROOT_TARGET_DIR)/spicelib/$(call GET_GDS2_CDL_NAME,$(@D)).inc"; \
	task=extract && $(CASTFILES_DEQUEUE_TASK) ;\
	st=$$status; \
	exit $$st

else # "$(GRAYBOX_MODE)" eq "" 262 lines back
.PRECIOUS: $(ROOT_TARGET_DIR)/%/extracted$(EXTRACT_DIR)/extract.err
# graybox
# use STAR_RC to extract
.PRECIOUS: $(ROOT_TARGET_DIR)/%/extracted$(EXTRACT_DIR)/extract.err
.PRECIOUS: $(ROOT_TARGET_DIR)/%/accurate$(EXTRACT_DIR)/extract.err
.PRECIOUS: $(ROOT_TARGET_DIR)/%/accurate$(EXTRACT_DIR)/cell.spice_gds2
.PRECIOUS: $(ROOT_TARGET_DIR)/%/accurate$(EXTRACT_DIR)/cell.spice_topcell
.PRECIOUS: $(ROOT_TARGET_DIR)/%/extracted$(EXTRACT_DIR)/cell.spice_gds2
.PRECIOUS: $(ROOT_TARGET_DIR)/%/extracted$(EXTRACT_DIR)/cell.spice_topcell
$(ROOT_TARGET_DIR)/%/extracted$(EXTRACT_DIR)/cell.spice_topcell \
$(ROOT_TARGET_DIR)/%/extracted$(EXTRACT_DIR)/extract.err: \
        $(ROOT_TARGET_DIR)/%/cell.gds2 \
        $(ROOT_TARGET_DIR)/%/cell.cdl_gds2 \
        $(ROOT_TARGET_DIR)/%/extracted$(EXTRACT_DIR)/graybox_list \
        $$(call CANONICALIZE_PATH,$(ROOT_TARGET_DIR)/%/../cell.nodeprops$(ROUTED_SUFFIX)$(ACCURATE_SUFFIX))
	if [[ ( -n "$(call LVE_SKIP,extract)" ) && ( -e '$@' ) ]] ; then exit; fi; \
	echo "#TASK=extract_starRC MODE=extracted$(EXTRACT_DIR) VIEW=$(call GET_NTH_FROM_LAST_DIR,$(@D),2) CELL=$(call GET_CAST_FULL_NAME,$(@D))"; \
	mkdir -p '$(@D)'; \
	status=0; \
	echo "$(LVS_BLACKBOX)" > '$(@D)/blackbox'; \
	task=extract && $(CASTFILES_ENQUEUE_TASK) && \
	cell=$$(echo '$(call GET_GDS2_CDL_NAME,$(@D))' ); \
	extract_dir=`mktemp -d "$(WORKING_DIR)/$$cell.XXXXXX"`; \
	sync; \
	sleep 1; \
	QRSH_FLAGS="$(PACKAGE_FLAGS) -l hercules=1,cc=$(DRCLVS_THREADS) $(EXTRACT_FLAGS)" \
	  QB_DIAG_FILE="$(@D)/cell.extract_hercules.diag" QB_RUN_NAME='lve_starRC1' \
	  $(QB) $(EXTRACT_STARRC) $(EXTRACT_COMMON_OPTIONS) \
	  --blackbox=$(LVS_BLACKBOX) \
	  --threads=$(DRCLVS_THREADS) \
	  --current-target-dir=$(ROOT_TARGET_DIR) \
	  --delete=$(DELETE_EXTRACT_DIR) \
	  --extractReduce=$(REDUCE_MODE) \
	  --root-target-dir=$(SUB_LVE_ROOT_DIR) \
	  --spice-target='$(@D)/cell.spice_gds2.tmp' \
	  --spice-topcell='$(@D)/cell.spice_topcell' \
	  --extra-extract-equiv='$(EXTRA_EQUIV)' \
	  --lvs-extra-options='$(LVS_EXTRA_OPTIONS)' \
	  --task='stage1' \
	  '$(call GET_GDS2_CDL_NAME,$(@D))' &>'$(@D)/extract.err' \
	&& QRSH_FLAGS="$(PACKAGE_LOW_FLAGS) $(EXTRACT_FLAGS)" \
	  QB_DIAG_FILE="$(@D)/cell.extract_starRC2a.diag" QB_RUN_NAME='lve_starRC2a' \
	  $(QB) $(EXTRACT_STARRC) $(EXTRACT_COMMON_OPTIONS) \
	  --blackbox=$(LVS_BLACKBOX) \
	  --current-target-dir=$(ROOT_TARGET_DIR) \
	  --delete=$(DELETE_EXTRACT_DIR) \
	  --extractReduce=$(REDUCE_MODE) \
	  --root-target-dir=$(SUB_LVE_ROOT_DIR) \
	  --spice-target='$(@D)/cell.spice_gds2.tmp' \
	  --spice-topcell='$(@D)/cell.spice_topcell' \
	  --task='stage2a' \
	  '$(call GET_GDS2_CDL_NAME,$(@D))' 2>&1 >> '$(@D)/extract.err' \
	&& QRSH_FLAGS="$(PACKAGE_FLAGS_starRC2b) -l starrc=1,cc=$(STARRC_THREADS) $(EXTRACT_FLAGS)" \
	  QB_DIAG_FILE="$(@D)/cell.extract_starRC2b.diag" QB_RUN_NAME='lve_starRC2b' \
	  $(QB) $(EXTRACT_STARRC) $(EXTRACT_COMMON_OPTIONS) \
	  --blackbox=$(LVS_BLACKBOX) \
	  --threads=$(STARRC_THREADS) \
	  --current-target-dir=$(ROOT_TARGET_DIR) \
	  --delete=$(DELETE_EXTRACT_DIR) \
	  --extractReduce=$(REDUCE_MODE) \
	  --root-target-dir=$(SUB_LVE_ROOT_DIR) \
	  --spice-target='$(@D)/cell.spice_gds2.tmp' \
	  --spice-topcell='$(@D)/cell.spice_topcell' \
	  --totem-mode=0 \
	  --task='stage2b' \
	  '$(call GET_GDS2_CDL_NAME,$(@D))' 2>&1 >> '$(@D)/extract.err' \
	&& QRSH_FLAGS="$(PACKAGE_LOW_FLAGS) $(EXTRACT_FLAGS)" \
	  QB_DIAG_FILE="$(@D)/cell.extract_starRC2c.diag" QB_RUN_NAME='lve_starRC2c' \
	  $(QB) $(EXTRACT_STARRC) $(EXTRACT_COMMON_OPTIONS) \
	  --blackbox=$(LVS_BLACKBOX) \
	  --current-target-dir=$(ROOT_TARGET_DIR) \
	  --delete=$(DELETE_EXTRACT_DIR) \
	  --extractReduce=$(REDUCE_MODE) \
	  --root-target-dir=$(SUB_LVE_ROOT_DIR) \
	  --spice-target='$(@D)/cell.spice_gds2.tmp' \
	  --spice-topcell='$(@D)/cell.spice_topcell' \
	  --task='stage2c' \
	  '$(call GET_GDS2_CDL_NAME,$(@D))' 2>&1 >> '$(@D)/extract.err' ; \
	if [[ $(SKIP_SUB_LVE) == 0 ]]; then \
	echo "Running subckt lve to generate all subckt spice files..." >&2; \
	$(LVEEXEC) \
	  --fulcrum-pdk-root=$(FULCRUM_PDK_PACKAGE_EXTRACT_ROOT) \
	  --task=extract --qsub=1 --jobs=8 --summarize=0 --star-rc=1 \
	  --minC='$(MINC)' --minR='$(MINR)' --minRC='$(MINRC)' \
	  --minExtractedC='$(MINEXTRACTEDC)' \
	  --minExtractedR='$(MINEXTRACTEDR)' \
	  --minCC='$(MINCC)' \
	  --maxF='$(MAXF)' \
	  --layoutScaleX='$(LAYOUT_SCALE_X)' \
	  --layoutScaleY='$(LAYOUT_SCALE_X)' \
	  --slurp-dir='$(SLURP_DIR)' \
	  --cast-dir='$(SUB_LVE_CAST_DIR)' \
	  --spec-dir='$(SUB_LVE_SPEC_DIR)' \
	  --dfII-dir='$(SUB_LVE_DFII_DIR)' \
	  --deleteExtractDir='$(DELETE_EXTRACT_DIR)' \
	  --output-dir="$(ROOT_TARGET_DIR)" \
	  --include='$(@D)/graybox_list' >&2 ; \
	fi;\
	QRSH_FLAGS="$(PACKAGE_LOW_FLAGS) $(EXTRACT_FLAGS)" \
	  QB_DIAG_FILE='$(@D)/cell.spice_gds2.diag' QB_RUN_NAME='lve4a_drextract' \
	  $(QB) $(EXTRACT_STARRC) $(EXTRACT_COMMON_OPTIONS) \
	  --blackbox=$(LVS_BLACKBOX) \
	  --current-target-dir=$(ROOT_TARGET_DIR) \
	  --delete=$(DELETE_EXTRACT_DIR) \
	  --extractReduce=$(REDUCE_MODE) \
	  --root-target-dir=$(SUB_LVE_ROOT_DIR) \
	  --spice-target='$(@D)/cell.spice_gds2.tmp' \
	  --spice-topcell='$(@D)/cell.spice_topcell' \
	  --task='stage4a' \
	  '$(call GET_GDS2_CDL_NAME,$(@D))' 2>&1 >> '$(@D)/extract.err'; \
	QRSH_FLAGS="$(PACKAGE_FLAGS) $(EXTRACT_FLAGS)" \
	  QB_DIAG_FILE='$(@D)/cell.spice_gds2.diag' QB_RUN_NAME='lve4b_extract' \
	  $(QB) $(EXTRACT_STARRC) $(EXTRACT_COMMON_OPTIONS) \
	  --blackbox=$(LVS_BLACKBOX) \
	  --current-target-dir=$(ROOT_TARGET_DIR) \
	  --delete=$(DELETE_EXTRACT_DIR) \
	  --extractReduce=$(REDUCE_MODE) \
	  --root-target-dir=$(SUB_LVE_ROOT_DIR) \
	  --spice-target='$(@D)/cell.spice_gds2.tmp' \
	  --spice-topcell='$(@D)/cell.spice_topcell' \
	  --task='stage4b' \
	  '$(call GET_GDS2_CDL_NAME,$(@D))' 2>&1 >> '$(@D)/extract.err' && \
	QRSH_FLAGS="$(PACKAGE_FLAGS) -l lvs=1  $(EXTRACT_FLAGS)" \
	  QB_DIAG_FILE='$(@D)/cell.spice_gds2.diag' QB_RUN_NAME='lve4c_extract' \
	  $(QB) $(EXTRACT_STARRC) $(EXTRACT_COMMON_OPTIONS) \
	  --blackbox=$(LVS_BLACKBOX) \
	  --current-target-dir=$(ROOT_TARGET_DIR) \
	  --delete=$(DELETE_EXTRACT_DIR) \
	  --extractReduce=$(REDUCE_MODE) \
	  --root-target-dir=$(SUB_LVE_ROOT_DIR) \
	  --spice-target='$(@D)/cell.spice_gds2.tmp' \
	  --spice-topcell='$(@D)/cell.spice_topcell' \
	  --task='stage4c' \
	  '$(call GET_GDS2_CDL_NAME,$(@D))' 2>&1 >> '$(@D)/extract.err' && \
	QRSH_FLAGS="$(PACKAGE_FLAGS) -l lvs=1  $(EXTRACT_FLAGS)" \
	  QB_DIAG_FILE='$(@D)/cell.spice_gds2.diag' QB_RUN_NAME='lve4d_extract' \
	  $(QB) $(EXTRACT_STARRC) $(EXTRACT_COMMON_OPTIONS) \
	  --blackbox=$(LVS_BLACKBOX) \
	  --current-target-dir=$(ROOT_TARGET_DIR) \
	  --delete=$(DELETE_EXTRACT_DIR) \
	  --extractReduce=$(REDUCE_MODE) \
	  --root-target-dir=$(SUB_LVE_ROOT_DIR) \
	  --spice-target='$(@D)/cell.spice_gds2.tmp' \
	  --spice-topcell='$(@D)/cell.spice_topcell' \
	  --task='stage4d' \
	  '$(call GET_GDS2_CDL_NAME,$(@D))' 2>&1 >> '$(@D)/extract.err' && \
	if [[ -f "$(@D)/cell.spice_gds2.tmp" ]]; then mv -f "$(@D)/cell.spice_gds2.tmp" "$(@D)/cell.spice_gds2"; fi; \
	if [ $$? == 0 ] ; then \
		touch '$(@D)/cell.spice_gds2'; \
	elif [[ $(IGNORE_NVN) == 0 ]] && grep -q 'NVN FAILED' "$(@D)/extract.err" ; then \
		rm -f "$(@D)/cell.spice_gds2" ; touch "$(@D)/cell.spice_gds2"; \
	fi; \
	if ( grep -q "\<Error" "$(@D)/extract.err" ) ; then \
		grep "\<Error" "$(@D)/extract.err" >&2 ; \
		rm -f "$(@D)/cell.spice_gds2"; touch "$(@D)/cell.spice_gds2"; \
	fi; \
	mkdir -p "$(ROOT_TARGET_DIR)/spicelib"; \
	QRSH_FLAGS="$(PACKAGE_LOW_FLAGS) $(EXTRACT_FLAGS)" \
	  QB_DIAG_FILE='$(@D)/cell.include.diag' QB_RUN_NAME='lvegeninc' \
	  $(QB) $(SPICE_REDUCE) --infile="$(@D)/cell.spice_topcell" --outfile="$(ROOT_TARGET_DIR)/spicelib/$(call GET_GDS2_CDL_NAME,$(@D)).inc"; \
	task=extract && $(CASTFILES_DEQUEUE_TASK) && \
	st=$$status; \
	exit $$st

endif # "$(GRAYBOX_MODE)" eq "" 592 lines back

# this is ALWAYS flat, no graybox here
$(ROOT_TARGET_DIR)/%/totem$(EXTRACT_DIR)/cell.spice_gds2 \
$(ROOT_TARGET_DIR)/%/totem$(EXTRACT_DIR)/extract.err $(ROOT_TARGET_DIR)/%/totem$(EXTRACT_DIR)/cell.spf: \
        $(ROOT_TARGET_DIR)/%/cell.gds2 \
        $(ROOT_TARGET_DIR)/%/cell.cdl_gds2 \
        $$(call CANONICALIZE_PATH,$(ROOT_TARGET_DIR)/%/../cell.nodeprops$(ROUTED_SUFFIX)$(ACCURATE_SUFFIX))
	if [[ ( -n "$(call LVE_SKIP,extract)" ) && ( -e '$@' ) ]] ; then exit; fi; \
	echo "#TASK=extract_starRC MODE=totem$(EXTRACT_DIR) VIEW=$(call GET_NTH_FROM_LAST_DIR,$(@D),2) CELL=$(call GET_CAST_FULL_NAME,$(@D))"; \
	mkdir -p '$(@D)'; \
	status=0; \
	task=extract && $(CASTFILES_ENQUEUE_TASK) && \
	cell=$$(echo '$(call GET_GDS2_CDL_NAME,$(@D))' ); \
	extract_dir=`mktemp -d "$(WORKING_DIR)/$$cell.XXXXXX"`; \
	if [[ -f "$(@D)/graybox_list" ]]; then /bin/mv -f "$(@D)/graybox_list" "$(@D)/graybox_list.orig"; fi; \
	sync; \
	sleep 1; \
	QRSH_FLAGS="$(PACKAGE_FLAGS) ,cc=$(DRCLVS_THREADS) $(EXTRACT_FLAGS)" \
	  QB_DIAG_FILE="$(@D)/cell.extract_hercules.diag" QB_RUN_NAME='lve_starRC1' \
	  $(QB) $(EXTRACT_STARRC) $(EXTRACT_COMMON_OPTIONS) \
	  --blackbox=0 \
	  --threads=$(DRCLVS_THREADS) \
	  --delete=0 \
	  --extractReduce=$(REDUCE_MODE) \
	  --spice-target='$(@D)/cell.spice_gds2.tmp' \
	  --spice-topcell='$(@D)/cell.spice_topcell' \
	  --extra-extract-equiv='$(EXTRA_EQUIV)' \
	  --lvs-extra-options='$(LVS_EXTRA_OPTIONS)' \
	  --task='stage1' \
	  '$(call GET_GDS2_CDL_NAME,$(@D))' &>'$(@D)/extract.err' \
	&& QRSH_FLAGS="$(PACKAGE_LOW_FLAGS) $(EXTRACT_FLAGS)" \
	  QB_DIAG_FILE="$(@D)/cell.extract_starRC2a.diag" QB_RUN_NAME='lve_starRC2a' \
	  $(QB) $(EXTRACT_STARRC) $(EXTRACT_COMMON_OPTIONS) \
	  --blackbox=0 \
	  --delete=0 \
	  --extractReduce=$(REDUCE_MODE) \
	  --spice-target='$(@D)/cell.spice_gds2.tmp' \
	  --spice-topcell='$(@D)/cell.spice_topcell' \
	  --task='stage2a' \
	  '$(call GET_GDS2_CDL_NAME,$(@D))' 2>&1 >> '$(@D)/extract.err' \
	&& QRSH_FLAGS="$(PACKAGE_FLAGS_starRC2b) -l starrc=1,cc=$(STARRC_THREADS) $(EXTRACT_FLAGS)" \
	  QB_DIAG_FILE="$(@D)/cell.extract_starRC2b.diag" QB_RUN_NAME='lve_starRC2b' \
	  $(QB) $(EXTRACT_STARRC) $(EXTRACT_COMMON_OPTIONS) \
	  --blackbox=0 \
	  --threads=$(STARRC_THREADS) \
	  --delete=0 \
	  --extractReduce=$(REDUCE_MODE) \
	  --spice-target='$(@D)/cell.spice_gds2.tmp' \
	  --spice-topcell='$(@D)/cell.spice_topcell' \
	  --totem-mode=1 \
	  --task='stage2b' \
	  '$(call GET_GDS2_CDL_NAME,$(@D))' 2>&1 >> '$(@D)/extract.err' \
	&& QRSH_FLAGS="$(PACKAGE_LOW_FLAGS) $(EXTRACT_FLAGS)" \
	  QB_DIAG_FILE="$(@D)/cell.extract_starRC2c.diag" QB_RUN_NAME='lve_starRC2c' \
	  $(QB) $(EXTRACT_STARRC) $(EXTRACT_COMMON_OPTIONS) \
	  --blackbox=0 \
	  --delete=0 \
	  --extractReduce=$(REDUCE_MODE) \
	  --spice-target='$(@D)/cell.spice_gds2.tmp' \
	  --spice-topcell='$(@D)/cell.spice_topcell' \
	  --task='stage2c' \
	   '$(call GET_GDS2_CDL_NAME,$(@D))' 2>&1 >> '$(@D)/extract.err' && \
	  rm -f "$(@D)/cell.spice_gds2.tmp" "$(@D)/cell.spice_gds2" "$(@D)/cell.spice_topcell"; \
	$(SPICE_REDUCE) --mode=totem \
	  --infile="$$extract_dir/starRC/cell.spf" \
	  --outfile='$(@D)/cell.spf'; \
	cp "$$extract_dir/starRC/cell.placement_info" '$(@D)/cell.placement_info' ; \
	if [ "$(DELETE_EXTRACT_DIR)" != "0" ]; then /bin/rm -rf "$$extract_dir"; fi; \
	if ( grep -q "\<Error" "$(@D)/extract.err" ) ; then \
		grep "\<Error" "$(@D)/extract.err" >&2 ; \
	fi; \
	mkdir -p "$(ROOT_TARGET_DIR)/spicelib"; \
	task=extract && $(CASTFILES_DEQUEUE_TASK) ;\
	st=$$status; \
	exit $$st

$(ROOT_TARGET_DIR)/%/nanotime$(EXTRACT_DIR)/cell.spef \
$(ROOT_TARGET_DIR)/%/nanotime$(EXTRACT_DIR)/cell.dpf \
$(ROOT_TARGET_DIR)/%/nanotime$(EXTRACT_DIR)/extract.err: \
	$(ROOT_TARGET_DIR)/%/cell.gds2 \
	$(ROOT_TARGET_DIR)/%/cell.cdl_gds2 \
	$$(call CANONICALIZE_PATH,$(ROOT_TARGET_DIR)/%/../cell.nodeprops$(ROUTED_SUFFIX)$(ACCURATE_SUFFIX))
	if [[ ( -n "$(call LVE_SKIP,extract)" ) && ( -e '$@' ) ]] ; then exit; fi; \
	echo "#TASK=extract_starRC MODE=nanotime$(EXTRACT_DIR) VIEW=$(call GET_NTH_FROM_LAST_DIR,$(@D),2) CELL=$(call GET_CAST_FULL_NAME,$(@D))"; \
	mkdir -p '$(@D)'; \
	task=extract && $(CASTFILES_ENQUEUE_TASK) && \
	cell=$$(echo '$(call GET_GDS2_CDL_NAME,$(@D))' ); \
	extract_dir=`mktemp -d "$(WORKING_DIR)/$$cell.XXXXXX"`; \
	if [[ -f "$(@D)/graybox_list" ]]; then /bin/mv -f "$(@D)/graybox_list" "$(@D)/graybox_list.orig"; fi; \
	sync; \
	sleep 1; \
	QRSH_FLAGS="$(PACKAGE_FLAGS) ,cc=$(DRCLVS_THREADS) $(EXTRACT_FLAGS)" \
	  QB_DIAG_FILE="$(@D)/cell.extract_hercules.diag" QB_RUN_NAME='lve_starRC1' \
	  $(QB) $(EXTRACT_STARRC) $(EXTRACT_COMMON_OPTIONS) \
	  --blackbox=0 \
	  --threads=$(DRCLVS_THREADS) \
	  --delete=0 \
	  --extractReduce=$(REDUCE_MODE) \
	  --spice-target='$(@D)/cell.spice_gds2.tmp' \
	  --spice-topcell='$(@D)/cell.spice_topcell' \
	  --extra-extract-equiv='$(EXTRA_EQUIV)' \
	  --lvs-extra-options='$(LVS_EXTRA_OPTIONS)' \
	  --task='stage1' \
	  '$(call GET_GDS2_CDL_NAME,$(@D))' &>'$(@D)/extract.err' \
	&& QRSH_FLAGS="$(PACKAGE_LOW_FLAGS) $(EXTRACT_FLAGS)" \
	  QB_DIAG_FILE="$(@D)/cell.extract_starRC2a.diag" QB_RUN_NAME='lve_starRC2a' \
	  $(QB) $(EXTRACT_STARRC) $(EXTRACT_COMMON_OPTIONS) \
	  --blackbox=0 \
	  --delete=0 \
	  --extractReduce=$(REDUCE_MODE) \
	  --spice-target='$(@D)/cell.spice_gds2.tmp' \
	  --spice-topcell='$(@D)/cell.spice_topcell' \
	  --task='stage2a' \
	  '$(call GET_GDS2_CDL_NAME,$(@D))' 2>&1 >> '$(@D)/extract.err' \
	&& QRSH_FLAGS="$(PACKAGE_FLAGS_starRC2b) -l starrc=1,cc=$(STARRC_THREADS) $(EXTRACT_FLAGS)" \
	  QB_DIAG_FILE="$(@D)/cell.extract_starRC2b.diag" QB_RUN_NAME='lve_starRC2b' \
	  $(QB) $(EXTRACT_STARRC) $(EXTRACT_COMMON_OPTIONS) \
	  --blackbox=0 \
	  --threads=$(STARRC_THREADS) \
	  --delete=0 \
	  --extractReduce=$(REDUCE_MODE) \
	  --spice-target='$(@D)/cell.spice_gds2.tmp' \
	  --spice-topcell='$(@D)/cell.spice_topcell' \
	  --nt-mode=1 \
	  --task='stage2b' \
	  '$(call GET_GDS2_CDL_NAME,$(@D))' 2>&1 >> '$(@D)/extract.err' && \
	cp "$$extract_dir/starRC/cell.spef" '$(@D)/cell.spef' && \
	cp "$$extract_dir/starRC/cell.dpf" '$(@D)/cell.dpf' && \
	if [ "$(DELETE_EXTRACT_DIR)" != "0" ]; then /bin/rm -rf "$$extract_dir"; fi; \
	status=$$?;
	if ( grep -q "\<Error" "$(@D)/extract.err" ) ; then \
		grep "\<Error" "$(@D)/extract.err" >&2 ; \
		status=1; \
	fi; \
	task=extract && $(CASTFILES_DEQUEUE_TASK) ;\
	exit $$status

# rename spice file from cast to gds2 names
.PRECIOUS: $(ROOT_TARGET_DIR)/%/estimated/cell.spice_gds2
$(ROOT_TARGET_DIR)/%/estimated/cell.spice_gds2: $(ROOT_TARGET_DIR)/%/estimated/cell.spice
	$(CASTFILES_ENQUEUE_TASK) && \
	QB_DIAG_FILE='$@.diag' QB_RUN_NAME='lve_rename' \
	$(EXEC_LOW_PACKAGE) $(CDL_RENAMER) \
	  --source-cdl-file='$<' \
	  --name-in=cast \
	  --name-out=gds2 \
	  --translated-cdl='$@'; \
	$(CASTFILES_DEQUEUE_TASK)

# rename spice file from cast to gds2 names
.PRECIOUS: $(ROOT_TARGET_DIR)/%/nogeometry/cell.spice_gds2
$(ROOT_TARGET_DIR)/%/nogeometry/cell.spice_gds2: $(ROOT_TARGET_DIR)/%/nogeometry/cell.spice
	$(CASTFILES_ENQUEUE_TASK) && \
	QB_DIAG_FILE='$@.diag' QB_RUN_NAME='lve_rename' \
	$(EXEC_LOW_PACKAGE) $(CDL_RENAMER) \
	  --source-cdl-file='$<' \
	  --name-in=cast \
	  --name-out=gds2 \
	  --translated-cdl='$@'
	$(CASTFILES_DEQUEUE_TASK)

# post-process spice file
.PRECIOUS: $(ROOT_TARGET_DIR)/%/cell.spice_rename
$(ROOT_TARGET_DIR)/%/cell.spice_rename: $(ROOT_TARGET_DIR)/%/cell.spice_gds2
	#TASK=spice2spice CELL=$(call GET_CAST_FULL_NAME,$(@D))
	mkdir -p '$(@D)'
	if [ -s '$(@D)/cell.spice_topcell' ]; then \
	   $(SPICE2SPICE) --top='$(call GET_GDS2_CDL_NAME,$(@D))' "$(@D)/cell.spice_topcell" "$@"; \
	else \
	   $(SPICE2SPICE) --top='$(call GET_GDS2_CDL_NAME,$(@D))' "$?" "$@" ;\
	fi

# delete resistor size info for hsim
.PRECIOUS: $(ROOT_TARGET_DIR)/%/cell.spice_hsim
$(ROOT_TARGET_DIR)/%/cell.spice_hsim: $(ROOT_TARGET_DIR)/%/cell.spice_rename
	#TASK=spice_reduce CELL=$(call GET_CAST_FULL_NAME,$(@D))
	mkdir -p '$(@D)'
	$(SPICE_REDUCE) --infile="$?" --outfile="$@"

# summarize starRC extract results
.PRECIOUS: $(ROOT_TARGET_DIR)/%/extracted$(EXTRACT_DIR)/extract.result
.PRECIOUS: $(ROOT_TARGET_DIR)/%/totem$(EXTRACT_DIR)/extract.result

$(ROOT_TARGET_DIR)/%/extracted$(EXTRACT_DIR)/extract.result: \
				$(ROOT_TARGET_DIR)/%/df2.d \
				$(ROOT_TARGET_DIR)/%/extracted$(EXTRACT_DIR)/extract.err \
				$(ROOT_TARGET_DIR)/%/extracted$(EXTRACT_DIR)/cell.aspice
	if (grep -q "NVN FAILED" '$(word 2,$^)') ; then \
		echo FAIL > '$@'; \
	elif [[ ! ( -s '$(word 3,$^)' ) ]] ; then \
		echo FAIL > '$@'; \
	else echo PASS > '$@'; fi;

$(ROOT_TARGET_DIR)/%/totem$(EXTRACT_DIR)/extract.result: \
				$(ROOT_TARGET_DIR)/%/df2.d \
				$(ROOT_TARGET_DIR)/%/totem$(EXTRACT_DIR)/extract.err \
				$(ROOT_TARGET_DIR)/%/totem$(EXTRACT_DIR)/cell.spf
	if (grep -q "NVN FAILED" '$(word 2,$^)') ; then \
		echo FAIL > '$@'; \
	elif [[ ! ( -s '$(word 3,$^)' ) ]] ; then \
		echo FAIL > '$@'; \
	else echo PASS > '$@'; fi;

$(ROOT_TARGET_DIR)/%/nanotime$(EXTRACT_DIR)/extract.result: \
				$(ROOT_TARGET_DIR)/%/df2.d \
				$(ROOT_TARGET_DIR)/%/nanotime$(EXTRACT_DIR)/extract.err \
				$(ROOT_TARGET_DIR)/%/nanotime$(EXTRACT_DIR)/cell.spef
	if (grep -q "NVN FAILED" '$(word 2,$^)') ; then \
		echo FAIL > '$@'; \
	elif [[ ! ( -s '$(word 3,$^)' ) ]] ; then \
		echo FAIL > '$@'; \
	else echo PASS > '$@'; fi;

# summarize starRC extract results : note depends on the extracted directory for the results
.PRECIOUS: $(ROOT_TARGET_DIR)/%/accurate$(EXTRACT_DIR)/extract.result
$(ROOT_TARGET_DIR)/%/accurate$(EXTRACT_DIR)/extract.result: \
				$(ROOT_TARGET_DIR)/%/df2.d \
				$(ROOT_TARGET_DIR)/%/extracted$(EXTRACT_DIR)/extract.err \
				$(ROOT_TARGET_DIR)/%/accurate$(EXTRACT_DIR)/cell.aspice
	if (grep -q "NVN FAILED" '$(word 2,$^)') ; then \
		echo FAIL > '$@'; \
	elif [[ ! ( -s '$(word 3,$^)' ) ]] ; then \
		echo FAIL > '$@'; \
	else echo PASS > '$@'; fi;

# JLVS -- checks CDL versus CAST (NOTE: ignore error code so make doesn't halt)
.PRECIOUS: $(ROOT_TARGET_DIR)/%/jlvs.out $(ROOT_TARGET_DIR)/%/jlvs.err
$(ROOT_TARGET_DIR)/%/jlvs.out $(ROOT_TARGET_DIR)/%/jlvs.err: $(ROOT_TARGET_DIR)/%/cell.cdl
	#TASK=jlvs CELL=$(call GET_CAST_FULL_NAME,$(@D))
	task=jlvs && $(CASTFILES_ENQUEUE_TASK) && \
	QB_DIAG_FILE='$(@D)/jlvs.err.diag' QB_RUN_NAME='lve_jlvs' \
	  $(EXEC_PACKAGE) $(JLVS) \
	  "$(call GET_CAST_FULL_NAME,$(@D))" \
	  --script-verbose \
	  --specification-type=cdl '$<' "$(call GET_CAST_CDL_NAME,$(@D))" \
	  --config=$(FULCRUM_PDK_PACKAGE_ROOT)/share/Fulcrum/jlvs/gates_stacks.config \
	  --cast-version=2 \
	  --cast-path='$(CAST_PATH)' \
	  --min-staticizer-ratio='$(MIN_STAT_RATIO)' \
	  --max-staticizer-ratio='$(MAX_STAT_RATIO)' \
	  --ignore-inline-layout \
	  1>'$(@D)/jlvs.out' 2>'$(@D)/jlvs.err' || true; \
	task=jlvs && $(CASTFILES_DEQUEUE_TASK)


.PRECIOUS: $(ROOT_TARGET_DIR)/%/lvs.result
$(ROOT_TARGET_DIR)/%/lvs.result: $(ROOT_TARGET_DIR)/%/lvs.err
	if [ -n "$$(grep 'NVN SUCCESS' '$<')" ] ; then \
		echo PASS > '$@'; \
	elif [ -e "$(call GET_LVS_SIGNOFF_FROM_CDBDEP,$(@D)/df2.d)" ] ; then \
		echo SIGNOFF > '$@'; \
	else echo FAIL > '$@'; fi;

.PRECIOUS: $(ROOT_TARGET_DIR)/%/lvs_graybox_list
.PRECIOUS: $(ROOT_TARGET_DIR)/%/lvs_custom_list

LVS_TARGET := lvs_graybox_list

ifneq ("$(LVS_GRAYBOX_LIST)","")
LVS_TARGET := lvs_custom_list
endif # "$(LVS_GRAYBOX_LIST)" ne ""

$(ROOT_TARGET_DIR)/%/lvs_graybox_list : $(ROOT_TARGET_DIR)/%/df2.d $$(call CANONICALIZE_PATH,$(ROOT_TARGET_DIR)/%/../cast.d)
	#TASK=create_lvs_list VIEW=$(call GET_NTH_FROM_LAST_DIR,$(@D),1) CELL=$(call GET_CAST_FULL_NAME,$(@D))
	mkdir -p '$(@D)'; \
	QB_DIAG_FILE='$(@D)/lvs.graylist.diag' QB_RUN_NAME='lve_lvs_graylist' \
	  QB_LOCAL=$(QB_LOCAL) QRSH_FLAGS="$(PACKAGE_FLAGS)" \
	   $(EXEC) $(CAST_QUERY) --cast-path='$(CAST_PATH)' \
	  --task=subcells --filter=one-level --routed \
	  --output='$@' \
	  --cell='$(call GET_CAST_FULL_NAME,$(@D))'; \
	  : < '$@' ; \
	  touch '$@' ; \
	  if [ -s '$@' ] ; then \
	  sed -e 's/ //g' '$@' | sort -u -o '$@'; fi;

$(ROOT_TARGET_DIR)/%/lvs_custom_list : $(LVS_GRAYBOX_LIST)
	#TASK=copy_lvs_list VIEW=$(call GET_NTH_FROM_LAST_DIR,$(@D),1) CELL=$(call GET_CAST_FULL_NAME,$(@D))
	mkdir -p '$(@D)'; \
	if [[ -f '$(LVS_GRAYBOX_LIST)' && -s '$(LVS_GRAYBOX_LIST)' ]] ; then \
	   sed -e 's/ //g' '$(LVS_GRAYBOX_LIST)' | sort -u -o '$@'; \
	else \
	   touch '$@' ; \
	fi

.PRECIOUS: $(ROOT_TARGET_DIR)/%/lvs.err

$(ROOT_TARGET_DIR)/%/lvs.err: \
        $(ROOT_TARGET_DIR)/%/cell.gds2 \
        $(ROOT_TARGET_DIR)/%/cell.cdl_gds2 \
        $(ROOT_TARGET_DIR)/%/$(LVS_TARGET) \
        $$(call CANONICALIZE_PATH,$(ROOT_TARGET_DIR)/%/../cell.nodeprops$(ROUTED_SUFFIX)$(ACCURATE_SUFFIX))
	#TASK=lvs VIEW=$(call GET_NTH_FROM_LAST_DIR,$(@D),1) CELL=$(call GET_CAST_FULL_NAME,$(@D))
	mkdir -p '$(@D)'
	status=0; \
	task=lvs && $(CASTFILES_ENQUEUE_TASK) && \
	cell=$$(echo '$(call GET_GDS2_CDL_NAME,$(@D))' ); \
	lvs_dir=`mktemp -d "$(WORKING_DIR)/lvs.XXXXXX"`; \
	sync; \
	sleep 1; \
	QRSH_FLAGS="$(PACKAGE_FLAGS) ,cc=$(DRCLVS_THREADS) $(EXTRACT_FLAGS)" \
	  QB_DIAG_FILE="$(@D)/lvs.diag" QB_RUN_NAME='lve_lvs' \
	  $(QB) lvs \
	  --fulcrum-pdk-root=$(FULCRUM_PDK_PACKAGE_EXTRACT_ROOT) \
	  --working-dir="$$lvs_dir" \
	  --threads=$(DRCLVS_THREADS) \
	  --cdl-file='$(@D)/cell.cdl_gds2' \
	  --gds2-file='$(@D)/cell.gds2' \
	  --gray-cell-list='$(@D)/$(LVS_TARGET)' \
	  --extra-extract-equiv='$(EXTRA_EQUIV)' \
	  --cdl-cell-name="$$cell" \
	  --blackbox=$(LVS_BLACKBOX) \
	  --icv-options=$(LVS_EXTRA_OPTIONS) \
	  --node-props='$(@D)/../cell.nodeprops$(ROUTED_SUFFIX)$(ACCURATE_SUFFIX)' \
	  '$(call GET_GDS2_CDL_NAME,$(@D))' &> '$(@)' && \
	cd / && ( [[ $(KEEP_LVS_DIR) == 1 ]] || rm -rf "$$lvs_dir" ); \
	task=lvs && $(CASTFILES_DEQUEUE_TASK)


# HERCULES ANTENNA
.PRECIOUS: $(ROOT_TARGET_DIR)/%/antenna.err
$(ROOT_TARGET_DIR)/%/antenna.err: $(ROOT_TARGET_DIR)/%/$(GDS2_TARGET) $(ROOT_TARGET_DIR)/%/df2.d
	#TASK=antenna CELL=$(call GET_CAST_FULL_NAME,$(@D))
	echo "$(REMOTE)"
	task=antenna && $(CASTFILES_ENQUEUE_TASK) && \
	antenna_dir=`mktemp -d "$(WORKING_DIR)/antenna.XXXXXX"`; \
	sync; \
	sleep 1; \
	ln -s '$<' "$$antenna_dir/cell.gds2" ; \
	chmod 2775 "$$antenna_dir" && cd "$$antenna_dir" && \
	   topcell=`fixhdrc --top-cell '$(call GET_GDS2_CDL_NAME,$(@D))' cell.gds2` && \
	QB_DIAG_FILE='$@.diag' QB_RUN_NAME='lve_antenna' \
	  QB_LOCAL=0 QRSH_FLAGS='$(PACKAGE_FLAGS) -cwd -l hdrc=1,cc=$(DRCLVS_THREADS)' \
	  $(QEXEC) $(ICV_SCRIPT) icv \
	  -c "$$topcell" \
	  -f GDSII \
	  -i "$$antenna_dir/cell.gds2" \
	  -vue \
	  $(FULCRUM_PDK_PACKAGE_ROOT)/share/Fulcrum/icv/antenna.rul \
	2> "$$antenna_dir/antenna.log" 1> "$$antenna_dir/antenna.log" ; \
	if [ -s "$$antenna_dir/$$topcell.LAYOUT_ERRORS" ]; then  \
	   grep violation "$$antenna_dir/$$topcell.LAYOUT_ERRORS" > "$$antenna_dir/antenna.err" ; \
	   cp -p "$$antenna_dir/$$topcell.LAYOUT_ERRORS" '$(@D)'/antenna.layout_errs ; \
	   ( ( [ -e "$$antenna_dir/antenna.err" ] && mv "$$antenna_dir/antenna.err" '$@' ) || \
	   ( cd "$$antenna_dir" && $(BZTAR) -cf '$(@D)/antenna.tar.bz2' ./* ) ); \
	fi \
	&& cd / && ( [[ $(KEEP_DRC_DIR) == 1 ]] || rm -rf "$$antenna_dir" ); \
	task=antenna && $(CASTFILES_DEQUEUE_TASK)

# summarize Antenna DRC results
.PRECIOUS: $(ROOT_TARGET_DIR)/%/antenna.result
$(ROOT_TARGET_DIR)/%/antenna.result: $(ROOT_TARGET_DIR)/%/antenna.err $(ROOT_TARGET_DIR)/%/df2.d
	if [[ ( -e '$<' ) && ! ( -s '$<') ]] ; then \
	  echo PASS > '$@'; \
	else \
	  echo FAIL > '$@'; \
	fi

# FRC
.PRECIOUS: $(ROOT_TARGET_DIR)/%/frc.err
$(ROOT_TARGET_DIR)/%/frc.err: $(ROOT_TARGET_DIR)/%/$(GDS2_TARGET) $(ROOT_TARGET_DIR)/%/df2.d
	#TASK=frc CELL=$(call GET_CAST_FULL_NAME,$(@D))
	echo "$(REMOTE)"
	task=frc && $(CASTFILES_ENQUEUE_TASK) && \
	frc_dir=`mktemp -d "$(WORKING_DIR)/frc.XXXXXX"`; \
	sync; \
	sleep 1; \
	chmod 2755 "$$frc_dir" && cd "$$frc_dir" && \
	topcell=$$(echo '$(call GET_GDS2_CDL_NAME,$(@D))' ); \
	QB_DIAG_FILE='$@.diag' QB_RUN_NAME='lve_frc' \
	  QB_LOCAL=0 QRSH_FLAGS='$(PACKAGE_FLAGS_frc) -cwd -l cc=$(DRCLVS_THREADS)' \
	  $(QEXEC) $(FRC)\
	  --working-dir="$$frc_dir" \
	  --gds2-file="$(word 1, $^)" \
	  --threads=$(DRCLVS_THREADS) \
	  --view=$(call GET_NTH_FROM_LAST_DIR,$(@D),1) \
	  --dfII-dir='$(DFII_DIR)' \
	  "$$topcell" \
	2> "$$frc_dir/frc.stderr" 1> "$$frc_dir/frc.stdout" && \
	( cp -p "$$frc_dir/$$topcell.LAYOUT_ERRORS" '$(@D)'/frc.layout_errs || true ) && \
	( awk '/violation.* found/ {print r,$$(NF-2),$$(NF-1),$$NF} /:/ {r=$$1}' "$$frc_dir/$$topcell.LAYOUT_ERRORS" > '$@' || true ) && \
	( egrep '((E)rror|(W)arning):' "$$frc_dir/skill.log" >> '$@' || true ) && \
	( cp -p "$$frc_dir/skill.log" '$(@D)'/frc.skilllog || true ) && \
	( ( [ -s "$$frc_dir/$$topcell.LAYOUT_ERRORS" ] || (echo -n "E"; echo "rror: ICV frc.rul not complete") >> '$@') ) && \
	( ( [ -s "$$frc_dir/skill.log" ] || (echo -n "E"; echo "rror: frc.il not run" ) >> '$@') ) && \
	( ( [ -e "$$frc_dir/frc.stderr" ] && cp -p "$$frc_dir/frc.stderr" '$(@D)/frc.stderr' ) || \
	( cd "$$frc_dir" && $(BZTAR) -cf '$(@D)/frc.tar.bz2' ./* ) ) \
	&& cd / && ( [[ $(KEEP_FRC_DIR) == 1 ]] || rm -rf "$$frc_dir" ); \
	task=frc && $(CASTFILES_DEQUEUE_TASK)

# summarize FRC results
.PRECIOUS: $(ROOT_TARGET_DIR)/%/frc.result
$(ROOT_TARGET_DIR)/%/frc.result: $(ROOT_TARGET_DIR)/%/frc.err $(ROOT_TARGET_DIR)/%/df2.d
	signoffFile="$(call GET_FRC_SIGNOFFS_FROM_CDBDEP,$(@D)/df2.d)"; \
	signoff=0 ; \
	fail=0 ; \
	warn=0 ; \
	if [[ ( -n "$$signoffFile" ) && ( -s "$$signoffFile" ) ]]; then \
	   if [[ `diff "$$signoffFile" '$<' | wc -l` -eq 0 ]] ; then \
	     signoff=1; \
	   fi ; \
	fi; \
	if [[ `egrep -c '(E)rror:' '$<'` -gt 0 ]]; then fail=1; fi ; \
	if [[ `egrep -c 'violation.*found' '$<'` -gt 0 ]]; then fail=1; fi ; \
	if [[ `egrep -c '(W)arning:' '$<'` -gt 0 ]]; then warn=1; fi ; \
	if [[ ! ( -e '$<' ) ]] ; then \
	  fail=1; \
	fi ;\
	echo "PASS" > '$@'; \
	if [[ $$warn -ne 0 ]]; then \
	  echo "WARNING" > '$@'; \
	fi ;\
	if [[ $$fail -ne 0 ]]; then \
	  echo "FAIL" > '$@'; \
	fi ;\
	if [[ $$signoff -ne 0 ]]; then \
	  echo "SIGNOFF" > '$@'; \
	fi

# ICV DRC
.PRECIOUS: $(ROOT_TARGET_DIR)/%/drc.err
$(ROOT_TARGET_DIR)/%/drc.err: $(ROOT_TARGET_DIR)/%/$(GDS2_TARGET) $(ROOT_TARGET_DIR)/%/df2.d
	#TASK=drc CELL=$(call GET_CAST_FULL_NAME,$(@D))
	echo "$(REMOTE)"
	task=drc && $(CASTFILES_ENQUEUE_TASK) && \
	drc_dir=`mktemp -d "$(WORKING_DIR)/drc.XXXXXX"`; \
	sync; \
	sleep 1; \
	ln -s '$<' "$$drc_dir/cell.gds2" ; \
	chmod 2755 "$$drc_dir" && cd "$$drc_dir" && \
	topcell=$$(echo '$(call GET_GDS2_CDL_NAME,$(@D))' ); \
	QB_DIAG_FILE='$@.diag' QB_RUN_NAME='lve_drc' \
	QB_LOCAL=0 QRSH_FLAGS='$(PACKAGE_FLAGS_drc) -cwd ,cc=$(DRCLVS_THREADS)' \
	$(QB) drc \
	  --fulcrum-pdk-root=$(FULCRUM_PDK_PACKAGE_EXTRACT_ROOT) \
	  --working-dir="$$drc_dir" \
	  --threads=$(DRCLVS_THREADS) \
	  --gds2-file='$(@D)/cell.gds2' \
	  --flow=$(DRC_FLOW) \
	  --icv-runset-path=$(ICV_RUNSET_PATH) \
	  --icv-options=$(DRC_EXTRA_OPTIONS) \
	  '$(call GET_GDS2_CDL_NAME,$(@D))' &> '$(@D)/drc.log' ; \
	  /bin/rm -f '$@'; \
	  for runset in  `echo "$(DRC_FLOW)" | sed -e 's/,/ /g'`; do \
            echo "RUNSet is $(runset)"; \
	    if [ -s "$$drc_dir/$$runset/$$topcell.LAYOUT_ERRORS" ]; then \
	      echo "$$drc_dir/$$runset/$$topcell.LAYOUT_ERRORS" ; \
	      awk '/violation.* found/ {print r,$$(NF-2),$$(NF-1),$$NF} /:/ {r=$$1}' "$$drc_dir/$$runset/$$topcell.LAYOUT_ERRORS" >> '$@' ; \
	      cp -p "$$drc_dir/$$runset/$$topcell.LAYOUT_ERRORS" '$(@D)'/drc.$$runset.layout_errs ; \
	    fi \
	  done \
	&& cd / && ( [[ $(KEEP_DRC_DIR) == 1 ]] || rm -rf "$$drc_dir" ); \
	task=drc && $(CASTFILES_DEQUEUE_TASK)

# summarize DRC results
.PRECIOUS: $(ROOT_TARGET_DIR)/%/drc.result
$(ROOT_TARGET_DIR)/%/drc.result: $(ROOT_TARGET_DIR)/%/drc.err $(ROOT_TARGET_DIR)/%/df2.d
	signoffFile="$(call GET_DRC_SIGNOFFS_FROM_CDBDEP,$(@D)/df2.d)"; \
	if [[ ( -e '$<' ) && ! ( -s '$<') ]] ; then \
	  echo PASS > '$@'; \
	else \
	  if [ -n "$$signoffFile" -a -s "$$signoffFile" ]; then \
	    if [ `diff "$$signoffFile" '$<' | wc -l` -eq 0 ] ; then \
	      echo SIGNOFF > '$@'; \
	    else \
	      echo FAIL > '$@'; \
	    fi; \
	  else \
	    echo FAIL > '$@'; \
	  fi; \
	fi


$(ROOT_TARGET_DIR)/%/totem$(EXTRACT_DIR)/extract.raw: $(ROOT_TARGET_DIR)/%/totem$(EXTRACT_DIR)/extract.result
	#TASK=extract_raw MODE=$(call GET_NTH_FROM_LAST_DIR,$(@D),1) VIEW=$(call GET_NTH_FROM_LAST_DIR,$(@D),2) CELL=$(call GET_CAST_FULL_NAME,$(@D))
	$(RAWIFY) --use-db=$(USEDB) --task=extract --mode=totem --root='$(ROOT_TARGET_DIR)' --dir='$(@D)' --cell="$(call GET_CAST_FULL_NAME,$(@D))"

$(ROOT_TARGET_DIR)/%/nanotime$(EXTRACT_DIR)/extract.raw: $(ROOT_TARGET_DIR)/%/nanotime$(EXTRACT_DIR)/extract.result
	#TASK=extract_raw MODE=$(call GET_NTH_FROM_LAST_DIR,$(@D),1) VIEW=$(call GET_NTH_FROM_LAST_DIR,$(@D),2) CELL=$(call GET_CAST_FULL_NAME,$(@D))
	touch '$@'

$(ROOT_TARGET_DIR)/%/extract.raw: $(ROOT_TARGET_DIR)/%/extract.result \
	                          $(ROOT_TARGET_DIR)/%/cell.aspice
	#TASK=extract_raw MODE=$(call GET_NTH_FROM_LAST_DIR,$(@D),1) VIEW=$(call GET_NTH_FROM_LAST_DIR,$(@D),2) CELL=$(call GET_CAST_FULL_NAME,$(@D))
	$(RAWIFY) --use-db=$(USEDB) --task=extract --root='$(ROOT_TARGET_DIR)' --dir='$(@D)' --cell="$(call GET_CAST_FULL_NAME,$(@D))"

$(ROOT_TARGET_DIR)/%/jlvs.raw: $(ROOT_TARGET_DIR)/%/jlvs.out $(ROOT_TARGET_DIR)/%/jlvs.err
	#TASK=jlvs_raw CELL=$(call GET_CAST_FULL_NAME,$(@D))
	$(CASTFILES_ENQUEUE_TASK) && \
	$(RAWIFY) --use-db=$(USEDB) --task=jlvs --root='$(ROOT_TARGET_DIR)' --dir='$(@D)' --cell="$(call GET_CAST_FULL_NAME,$(@D))"; \
	$(CASTFILES_DEQUEUE_TASK)

$(ROOT_TARGET_DIR)/%/lvs.raw: $(ROOT_TARGET_DIR)/%/lvs.result
	#TASK=lvs_raw CELL=$(call GET_CAST_FULL_NAME,$(@D))
	$(CASTFILES_ENQUEUE_TASK) && \
	$(RAWIFY) --use-db=$(USEDB) --task=lvs --root='$(ROOT_TARGET_DIR)' --dir='$(@D)' --cell="$(call GET_CAST_FULL_NAME,$(@D))"; \
	$(CASTFILES_DEQUEUE_TASK)


$(ROOT_TARGET_DIR)/%/drc.raw: $(ROOT_TARGET_DIR)/%/drc.result
	#TASK=drc_raw CELL=$(call GET_CAST_FULL_NAME,$(@D))
	$(CASTFILES_ENQUEUE_TASK) && \
	$(RAWIFY) --use-db=$(USEDB) --task=drc --root='$(ROOT_TARGET_DIR)' --dir='$(@D)' --cell="$(call GET_CAST_FULL_NAME,$(@D))"; \
	$(CASTFILES_DEQUEUE_TASK)

$(ROOT_TARGET_DIR)/%/antenna.raw: $(ROOT_TARGET_DIR)/%/antenna.result
	#TASK=antenna_raw CELL=$(call GET_CAST_FULL_NAME,$(@D))
	$(CASTFILES_ENQUEUE_TASK) && \
	$(RAWIFY) --use-db=$(USEDB) --task=antenna --root='$(ROOT_TARGET_DIR)' --dir='$(@D)' --cell="$(call GET_CAST_FULL_NAME,$(@D))"; \
	$(CASTFILES_DEQUEUE_TASK)


$(ROOT_TARGET_DIR)/%/frc.raw: $(ROOT_TARGET_DIR)/%/frc.result
	#TASK=frc_raw CELL=$(call GET_CAST_FULL_NAME,$(@D))
	$(CASTFILES_ENQUEUE_TASK) && \
	$(RAWIFY) --use-db=$(USEDB) --task=frc --root='$(ROOT_TARGET_DIR)' --dir='$(@D)' --cell="$(call GET_CAST_FULL_NAME,$(@D))"; \
	$(CASTFILES_DEQUEUE_TASK)


.PRECIOUS: $(ROOT_TARGET_DIR)/%/cell.captally

$(ROOT_TARGET_DIR)/%/cell.captally: $(ROOT_TARGET_DIR)/%/cell.spice
	#TASK=captally MODE=extracted$(EXTRACT_DIR) CELL=$(call GET_CAST_FULL_NAME,$(@D))
	$(CASTFILES_ENQUEUE_TASK) && \
	QB_DIAG_FILE='$@.diag' QB_RUN_NAME='lve_captally' \
	  $(EXEC_PACKAGE) captally --fulcrum-pdk-root=$(FULCRUM_PDK_PACKAGE_ROOT) --cdl "$<" --lve-dir $(ROOT_TARGET_DIR) --localnodes --wire > "$@" && \
	$(CASTFILES_DEQUEUE_TASK)

endif # "$(CASTFILES)" eq "1" 2473 lines back
