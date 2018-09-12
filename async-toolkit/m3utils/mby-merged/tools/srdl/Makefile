#
# Makefile to build <BLK> register data and models
# * BLK_reg_doc.html  - Register documentation in HTML format
# * BLK_reg_pkg.vh    - Register localparams or constants
# * BLK_reg_struct.vh - Register structs of field members
# * RTL models        - Register mgmt. modules, include shell_ctl
# * WM data           - White model data in C/perl/python
#
# Usage: make [all] |   documentation, constants, struct definitions
#              wmd  |   white model data
#              scm  |   shell controller management
#              xml  |   top level XML
#              rtl  |   RTL module
#              nqc  |   Nebulon quality check
#              nug  |   Nebulon User's Guide
#            clean  |   remove temporary outputs
#

SHELL := /usr/intel/bin/tcsh
.SUFFIXES:
.SECONDEXPANSION:
.NOTPARALLEL:
.DELETE_ON_ERROR:


export NEBULON_MAXIMIZE_CRIF := 1
NEBULON_PATH    := $(shell ToolConfig.pl get_tool_path nebulon)
NEBULON_SETUP   := source $(NEBULON_PATH)/bin/setup $(NEBULON_PATH)
SRDL_DIR1       := $(shell find -L $(MODEL_ROOT)/src -path "*/src/srdl")
SRDL_DIR2       := $(shell find -L $(MODEL_ROOT)/tools -path "*/tools/srdl")
MGM_INCLUDE_ARG = $(shell find $(MODEL_ROOT)/target/GenRTL/mgm -maxdepth 1 -type d)
MGM_INCLUDE_ARG2 = target/GenRTL/mgm/$(BLK)
#INCLUDE_ARG     = -I "$(foreach d,$(SRDL_DIR1),$(SRDL_DIR2),$(d) $(MGM_INCLUDE_ARG) $(MGM_INCLUDE_ARG2) $(IP_INCLUDE_ARG))"
INCLUDE_ARG     = -I "$(foreach d,$(SRDL_DIR1) $(SRDL_DIR2),$(d) ) $(MGM_INCLUDE_ARG) $(IP_INCLUDE_ARG)"

comma           := ,
space :=
space +=
INCLUDE_ARG_OS1 = $(foreach d,$(SRDL_DIR1) $(SRDL_DIR2),$(d)) $(MGM_INCLUDE_ARG) $(IP_INCLUDE_ARG)
INCLUDE_ARG_OS2 = $(subst $(space),$(comma),$(INCLUDE_ARG_OS1))
INCLUDE_ARG_OS  = -I "$(INCLUDE_ARG_OS2)"
NEBULON         = $(NEBULON_SETUP) && nebulon $(INCLUDE_ARG) $(PARAM_ARG)

ONESOURCE_PATH  := $(shell ToolConfig.pl get_tool_path onesource)
OS_SETUP        := source $(ONESOURCE_PATH)/setup/OneSourceBundle.setup 
RDL_TO_OSXML    := $(OS_SETUP) && $(NEBULON_SETUP) && $$RDL_IMPORTER/Run
OSXML_TO_RDL    := $(OS_SETUP) && $(NEBULON_SETUP) && $$RDL_EXPORTER/run 

XML_TO_HTML     := $(shell ToolConfig.pl get_tool_path regs2html)/regs.xsl
GENPG_ARGS      := -copyrightpath ${MODEL_ROOT}/src/txt/intelcopyright.txt -bits 28 \
                   --policygroups 8 MST_PG0 MST_PG1 MST_PG2 MST_PG3 MST_PG4 MST_PG5 MST_PG6 MST_PG7
GENPG_EXEC      := $(shell ToolConfig.pl get_tool_path meta)/bin/genpg
GENPG           := $(GENPG_EXEC) $(GENPG_ARGS)


# Input files
TOP_MAP         := $(BLK)_top_map
TOP_RDL         := $(TOP_MAP).rdl
ALL_RDL         := $(wildcard *.rdl)

ifeq ("",$(wildcard $(TOP_MAP)))
 $(error "No TOP_MAP ($(TOP_MAP)) found, or undefined")
endif

ifndef MODEL_ROOT
 $(error "No MODEL_ROOT defined, did you run setnhdk?")
endif

ifndef BLK 
 $(error "No BLK defined, did define it?")
endif

ifndef MODEL_ROOT
 $(error "No MODEL_ROOT defined, are you in an HDK environment?")
endif

BLK_UPPER := $(shell echo $(BLK) | tr a-z A-Z)

# Output files
ifndef TARGET_DIR
TARGET_DIR_RP   := target/GenRTL/regflow/$(BLK)
TARGET_DIR      := $(MODEL_ROOT)/$(TARGET_DIR_RP)
else
TARGET_DIR_RP   := $(TARGET_DIR)
endif

#Generated HDLspec file
HDLSPEC_RTL  := $(TARGET_DIR)/$(BLK)_nebulon_rtl.hdl
HDLSPEC_RAL  := $(TARGET_DIR)/$(BLK)_nebulon_ral.hdl

ifndef RESULTSDOCDIR
 RESULTSDOCDIR  := $(TARGET_DIR)
endif

$(shell mkdir -p $(TARGET_DIR))

# Output paths
RESULTS_NEB     := $(TARGET_DIR)
RESULTS_NBSV    := $(RESULTS_NEB)/output/sverilog
RESULTS_XML     := $(RESULTS_NEB)/output/xml
RESULTS_OVM     := $(RESULTS_NEB)/output/ovm
RESULTS_CRIF    := $(RESULTS_NEB)/output/crif
RESULTS_GENPG   := $(RESULTS_NEB)/output/genpg

RESULTSXMLFILE  := $(RESULTS_XML)/$(BLK)_top_map.xml
RESULTSQCDIR    := $(RESULTSDOCDIR)/output/qualitychecker
RESULTSQC       := $(RESULTSQCDIR)/$(TOP_MAP).qc.xml
CRIF_FILE       := $(RESULTS_CRIF)/$(BLK)_top_map_crif.xml
SV_FILE         := $(RESULTS_NBSV)/rtlgen_pkg_$(BLK)_top_map.vh
GENPG_SV        := $(RESULTS_GENPG)/$(BLK)_imn_decoder.sv
RAL_ENV         := $(RESULTS_OVM)/$(TOP_MAP)_ral_env.svh

HTM_TABLE   := $(TARGET_DIR)/$(BLK)_reg_doc.html
RDL_MGM_MAPS    := $(TARGET_DIR)/$(BLK)_mgm_maps.rdl
OSXML_TARG      := $(TARGET_DIR)/$(BLK)_osxml/.done
RDL_TARG        := $(TARGET_DIR)/$(TOP_RDL)

# Old Ruby generator... left for backwards compatibility
GEN             := $(MODEL_ROOT)/tools/srdl/scripts/rdl.rb

HDLSPEC_GEN     := $(MODEL_ROOT)/tools/srdl/srdl_make_hdl.pl

# Targets, skip $(GENPG_SV, for now)
all: $(PREREQS) $(HTM_TABLE) rtl scm ral_env $(HDLSPEC_RTL) nqc
everything: all xml osxml nqc gen_rdl

# Rules

# Command actually makes both RTL and RAL HDLSpec files.
$(HDLSPEC_RTL): $(SV_FILE) 
	$(HDLSPEC_GEN) --filename_rtl $(HDLSPEC_RTL) --filename_ral $(HDLSPEC_RAL) --model_root $(MODEL_ROOT) --dir $(TARGET_DIR_RP)

$(SHELL_CTL_RDL):
	$(MAKE) -C ../../tools/mgm -f $(MODEL_ROOT)/tools/mgm/Makefile rdl
	ln -fs $(MGM_OUTPUT)/$(SHELL_CTL_RDL) .

genrdl: $(SHELL_CTL_RDL)

all_shell_ctl_rdl: all_shell_ctl_rdl_pre
	$(eval MGM_INCLUDE_ARG = $(shell find $(MODEL_ROOT)/target/GenRTL/mgm -maxdepth 1 -type d))

all_shell_ctl_rdl_pre:
	$(MAKE) -C $(MODEL_ROOT)/tools/mgm -f $(MODEL_ROOT)/tools/mgm/Makefile rdl_all

genrdl: $(SHELL_CTL_RDL)

show:
	@echo RAL_ENV targets is $(RAL_ENV)
	@echo RESULTSQC target is $(RESULTSQC)
	@echo INCLUDE_ARG $(INCLUDE_ARG)
	@echo INCLUDE_ARG_OS $(INCLUDE_ARG_OS)


$(RESULTSDOCDIR):
	mkdir -p $(RESULTSDOCDIR)

# Not very elegant here (should be more like an 'include path')
mst_udp.rdl: $(MODEL_ROOT)/src/srdl/mst_udp.rdl
	ln -fs $< $@

mst_shell_ctl.rdl: $(MODEL_ROOT)/tools/srdl/mst_shell_ctl.rdl
	ln -fs $< $@

# A little silly, for some reason nebulon
# wants security.pm to be in the 'local' directory to the run
# Hopefully, this can be fixed

$(HTM_TABLE): $(RESULTSXMLFILE) $(XML_TO_HTML)
	xsltproc -o $@ $(XML_TO_HTML) $<

html: $(HTM_TABLE)

rtl: $$(SV_RTLS)

scm: $$(SV_SCMS)

#$(RESULTSXMLFILE): $(ALL_RDL) $(PREREQS)
#	mkdir -p $(RESULTSDOCDIR)
#	$(NEBULON) -crif -xml -input $(TOP_RDL) -timeout 60000 -out_dir $(RESULTSDOCDIR)

nebulon_mby: $(ALL_RDL) $(PREREQS)
	$(NEBULON) -input $(TOP_RDL) -out_dir $(RESULTS_NEB)

$(RESULTSXMLFILE) $(RAL_ENV) $(CRIF_FILE): $(ALL_RDL) $(PREREQS)
	$(info INCLUDE_ARG="$(INCLUDE_ARG)")
	$(info SRDL_DIR1="$(SRDL_DIR1)")
	$(info SRDL_DIR2="$(SRDL_DIR2)")
	$(NEBULON) -sv_no_sai_checks -sverilog -xml -chdr -crif -ovm -input $(TOP_RDL) -timeout 60000 -out_dir $(RESULTS_NEB) -rtlgencomp -log_file $(RESULTS_NEB)/nebulon_sv_output.log

# Need dependency on only one output file, since command for nebulon_all generates everything
nebulon_all: $(RAL_ENV)

ral_env: $(RAL_ENV)

wmd: $(ALL_RDL)
	cd $(MODEL_ROOT)/verif/wm && make -f Makefile.registers build

xml: $(RESULTSXMLFILE)

$(RESULTSQC): $(ALL_RDL)
	mkdir -p $(RESULTSDOCDIR)
	$(NEBULON) -timeout 60000 -input $(TOP_RDL) -out_dir $(RESULTSDOCDIR) -qualitychecker -log_file $(RESULTS_NEB)/nebulon_qc_output.log

nqc: $(RESULTSQC)

nug:
	acroread $(NEBULON_PATH)/doc/nebulon_user_guide.pdf &

$(RDL_MGM_MAPS):
	$(GEN) rdl_mgm_map $@

$(OSXML_TARG): $(ALL_RDL) $(PREREQS)
	-rm -rf $(dir $@)
	mkdir -p $(dir $@)
	$(RDL_TO_OSXML) -input $(TOP_RDL) -output $(dir $@) $(INCLUDE_ARG_OS) $(PARAM_ARG)
	touch $@

$(RDL_TARG): $(OSXML_TARG)
	$(OSXML_TO_RDL) -input $(dir $<) -top $(TOP_MAP) -output $@

$(GENPG_SV): $(CRIF_FILE) $(GENPG_EXEC) $(GENPG_TEMPLATE)
	mkdir -p $(dir $@)
	$(GENPG) -crif $< -sv $@ -D MODULE_NAME $(basename $(notdir $@))

genpg: $(GENPG_SV)


osxml: $(OSXML_TARG)

gen_rdl: $(RDL_TARG)

clean_results:
	-rm -rf $(RESULTSDOCDIR) nebulon.2017*
	-rm -rf $(TARGET_DIR)

clean: clean_results
