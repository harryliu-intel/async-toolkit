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
PPE_BLOCKS  := mby_ppe_parser_map mby_ppe_mapper_map mby_ppe_cgrp_a_map mby_ppe_cgrp_b_map mby_ppe_rx_stats_map\
               mby_ppe_entropy_map mby_ppe_nexthop_map mby_ppe_fwd_misc_map mby_ppe_mst_glort_map mby_ppe_policers_map \
               mby_ppe_cm_apply_map mby_ppe_cm_usage_map mby_ppe_trig_apply_map mby_ppe_trig_apply_misc_map mby_ppe_trig_usage_map    
              
GMS_BLOCKS  := mby_freepointer_fifo_map
MESH_BLOCKS := mby_mesh_row_map
 
UVM_RAL_BLOCKS := $(PPE_BLOCKS) $(GMS_BLOCKS) $(MESH_BLOCKS)

include $(MODEL_ROOT)/tools/srdl/Makefile

PPE_RAL_TARGS := $(addprefix $(RESULTS_SUBBLOCK)/ral_,$(addsuffix .sv,$(PPE_BLOCKS)))
GMS_RAL_TARGS := $(addprefix $(RESULTS_SUBBLOCK)/ral_,$(addsuffix .sv,$(GMS_BLOCKS)))
MESH_RAL_TARGS:= $(addprefix $(RESULTS_SUBBLOCK)/ral_,$(addsuffix .sv,$(MESH_BLOCKS)))

ppe_ral : $(PPE_RAL_TARGS)
gms_ral : $(GMS_RAL_TARGS)
mesh_ral: $(MESH_RAL_TARGS)

# genrdl: $(SHELL_CTL_RDL)

