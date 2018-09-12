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

# subBlock name
BLK             := mby
SHELL_CTL_RDL   := $(MODEL_ROOT)/results/mgm/gmf/mby_shell_ctl_mgmt.rdl

# TBD, whether this IP uses LSM
# LSM_ROOT := $(shell ToolConfig.pl get_tool_path ipconfig/gmf_lsm)
# IP_INCLUDE_ARG += $(LSM_ROOT)/tools/srdl/reg

#PREREQS := $(NOC_RDL)
include $(MODEL_ROOT)/tools/srdl/Makefile

# genrdl: $(SHELL_CTL_RDL)

