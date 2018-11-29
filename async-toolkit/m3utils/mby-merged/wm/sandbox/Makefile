##------------------------------------------------------------------------------
##
## INTEL CONFIDENTIAL
##
## Copyright 2018 Intel Corporation All Rights Reserved.
##
## The source code contained or described herein and all documents related to
## the source code ("Material") are owned by Intel Corporation or its suppliers
## or licensors. Title to the Material remains with Intel Corporation or its
## suppliers and licensors. The Material contains trade secrets and proprietary
## and confidential information of Intel or its suppliers and licensors. The
## Material is protected by worldwide copyright and trade secret laws and
## treaty provisions. No part of the Material may be used, copied, reproduced,
## modified, published, uploaded, posted, transmitted, distributed, or
## disclosed in any way without Intel's prior express written permission.
##
## No license under any patent, copyright, trade secret or other intellectual
## property right is granted to or conferred upon you by disclosure or delivery
## of the Materials, either expressly, by implication, inducement, estoppel or
## otherwise. Any license under such intellectual property rights must be
## express and approved by Intel in writing.
##
##------------------------------------------------------------------------------
## Author      : Michael Roberts <michael.d.roberts@intel.com>
## Project     : Madison Bay
##------------------------------------------------------------------------------

export SNPSLMD_LICENSE_FILE := $(shell getLf synopsys/vcs)
export VCS_LIC_EXPIRE_WARNING := 0
# use VCS_HOME from environment if defined
export VCS_HOME := $(shell ToolConfig.pl get_tool_path vcsmx)
export VERDI_HOME := $(shell ToolConfig.pl get_tool_path verdi3)
export PATH := $(VCS_HOME)/bin:$(PATH)

export UVM_HOME ?= $(shell ToolConfig.pl get_tool_path uvm)
export OVM_HOME ?= $(shell ToolConfig.pl get_tool_path ovm)
export XVM_HOME ?= $(shell ToolConfig.pl get_tool_path xvm)
export SAOLA_HOME ?= $(shell ToolConfig.pl get_tool_path saola)
export VCS_UVM_HOME ?= $(UVM_HOME)/src

repodir        := $(shell git rev-parse --show-toplevel)
verifdir       = $(repodir)/verif
sandboxdir     = $(repodir)/wm/sandbox
dpiclient      = $(repodir)/wm/src/client/c/
docdir         = $(verifdir)/doc

m3_root        = $(repodir)/wm/src/main/m3
serverpath     = $(m3_root)/model_server/AMD64_LINUX/modelserver

seed           =  0
verbosity      =  UVM_HIGH
model          =  mby_wm
test           =  mby_wm_reg_rw_test

defs           =  SIMULATION \
                  OVM \
                  XVM \
                  UVM_NO_DPI \
                  UVM \
                  UVM_OBJECT_DO_NOT_NEED_CONSTRUCTOR \
                  UVM_REPORT_DISABLE_FILE_LINE

libs           =  $(UVM_HOME)/src \
                  $(OVM_HOME)/src \
                  $(XVM_HOME)/src \
                  +incdir+$(SAOLA_HOME)/verilog \
                  +incdir+$(SAOLA_HOME)/verilog_uvm

intfs          =

pkgs           =  $(UVM_HOME)/src/uvm_pkg.sv \
                  $(UVM_HOME)/src/dpi/uvm_dpi.cc \
                  $(OVM_HOME)/src/ovm_pkg.sv \
                  $(XVM_HOME)/src/xvm_pkg.sv \
                  $(SAOLA_HOME)/verilog/sla_pkg.sv \
                  $(sandboxdir)/$(model)_dpi_pkg.sv \
                  $(sandboxdir)/$(model)_test_pkg.sv \
                  $(SAOLA_HOME)/libs/Linux_x86_64/libsla.so



mods           = $(sandboxdir)/$(model)_tb_top.sv

progs          = $(sandboxdir)/$(model)_prog.sv


tbs            =  $(mods)

c_files       =  $(dpiclient)/mby_tcp_client_library.c

vlog_opts_all  =  -l comp.log \
                  -full64 \
                  -sverilog \
                  -timescale=1ps/1ps \
                  +libext+.v+.sv \
                  +vcs+lic+wait \
                  -kdb \
                  -debug_access+all \
                   $(vlog_opts)
elab_opts_all  =  -l elab.log \
                  -full64 \
                  -debug_access+all \
                  -CFLAGS -DVCS \
                  +vcs+lic+wait \
                  -kdb \
                  -lca \
                  +warn=noLCA_FEATURES_ENABLED \
                  $(c_files) \
                  $(elab_opts)
simv_args_all  =  -l run.log \
                  +ntb_random_seed=$(seed) \
                  +UVM_TESTNAME=$(test) \
                  +UVM_VERBOSITY=$(verbosity) \
                  -assert nopostproc \
                  +vcs+lic+wait \
                  +warn=noLCA_FEATURES_ENABLED \
                  +server_path=$(serverpath) \
                  $(simv_args)

tb_vlog        =  +define+model=$(model) \
                  $(foreach DEF, $(defs), +define+$(DEF)) \
                  $(foreach LIB, $(abspath $(libs)), +incdir+$(LIB) -y $(LIB)) \
                  $(foreach INTF, $(abspath $(intfs)), $(INTF)) \
                  $(foreach PKG, $(abspath $(pkgs)), +incdir+$(dir $(PKG)) -y $(dir $(PKG)) $(PKG)) \
                  $(foreach MOD, $(abspath $(mods)), $(MOD)) \
                  $(foreach PROG, $(abspath $(progs)), +incdir+$(dir $(PROG)) $(PROG))

simv_args_all +=  -sv_lib $(SAOLA_HOME)/libs/mti/Linux_x86_64/libsla

targetdir = $(repodir)/target/$(model)
resultsdir = $(repodir)/results/$(model)/$(test).$(seed)
simv = $(targetdir)/$(model).simv

run: $(simv) $(serverpath)
	mkdir -p $(resultsdir)
	cd $(resultsdir); time $(simv) $(simv_args_all)

$(serverpath):
	$(MAKE) -C $(m3_root) model_server


$(simv): $(tb) $(pkgs) Makefile
	mkdir -p $(targetdir)
	cd $(targetdir); vlogan $(vlog_opts_all) $(tb_vlog)
	cd $(targetdir); grep Parsing comp.log \
	     | sed "s,.*'\([^\']*\)'.*,$(simv): \1," \
	     | sort | uniq >$(model).dep
	cd $(targetdir); vcs $(elab_opts_all) \
	     $(basename $(notdir $(tbs))) \
	     $(basename $(notdir $(progs))) \
	     -o $@

doc:
	make -f $(docdir)/Makefile

clean:
	-rm -rf $(repodir)/target/$(model)
	-rm -rf $(resultsdir)
	$(MAKE) -C $(m3_root) clean


-include $(targetdir)/$(model).dep
