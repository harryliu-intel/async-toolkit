##------------------------------------------------------------------------------
##
##  INTEL CONFIDENTIAL
##
##  Copyright 2017 Intel Corporation All Rights Reserved.
##
##  The source code contained or described herein and all documents related
##  to the source code (Material) are owned by Intel Corporation or its 
##  suppliers or licensors. Title to the Material remains with Intel
##  Corporation or its suppliers and licensors. The Material contains trade
##  secrets and proprietary and confidential information of Intel or its 
##  suppliers and licensors. The Material is protected by worldwide copyright
##  and trade secret laws and treaty provisions. No part of the Material may 
##  be used, copied, reproduced, modified, published, uploaded, posted,
##  transmitted, distributed, or disclosed in any way without Intel's prior
##  express written permission.
##
##  No license under any patent, copyright, trade secret or other intellectual
##  property right is granted to or conferred upon you by disclosure or
##  delivery of the Materials, either expressly, by implication, inducement,
##  estoppel or otherwise. Any license under such intellectual property rights
##  must be express and approved by Intel in writing.
##
##------------------------------------------------------------------------------

###############################################################################
# Interface definitions for HighLand Park style mangement ripple
# This for both forward and backward directory
# The backward direction is only for flow control
# Contact: Ho-Ming Leung
###############################################################################

set Ifc_Version 0.3
set Ifc_Id IMN_CSR::IMN_RPL

create_interface $Ifc_Id  \
    -version $Ifc_Version \
    -description "Ripple Mangement Interface from HLP project"

#############################
# Interface attributes
#############################
set_interface_attribute $Ifc_Id AutoConnectWhen never

#############################
# Control parameters
#############################


#############################
# Interface Parameters
#############################

create_interface_parameter W_IMN_META \
    -interface $Ifc_Id \
    -type integer \
    -specify_on consumer \
    -default 16 \
    -label "Management meta width" \
    -description \
    "Width of management meta"

set_interface_parameter_attribute $Ifc_Id W_IMN_META MinValue 8
set_interface_parameter_attribute $Ifc_Id W_IMN_META MaxValue 32
set_interface_parameter_attribute $Ifc_Id W_IMN_META InterfaceLink "<open>"

create_interface_parameter W_IMN_ADDR \
    -interface $Ifc_Id \
    -type integer \
    -specify_on consumer \
    -default 28 \
    -label "Management address width" \
    -description \
    "Width of management address"

set_interface_parameter_attribute $Ifc_Id W_IMN_ADDR MinValue 20
set_interface_parameter_attribute $Ifc_Id W_IMN_ADDR MaxValue 32
set_interface_parameter_attribute $Ifc_Id W_IMN_ADDR InterfaceLink "<open>"

create_interface_parameter W_IMN_DATA64 \
    -interface $Ifc_Id \
    -type integer \
    -specify_on consumer \
    -default 64 \
    -label "Management data width" \
    -description \
    "Width of management data"

set_interface_parameter_attribute $Ifc_Id W_IMN_DATA64 MinValue 64
set_interface_parameter_attribute $Ifc_Id W_IMN_DATA64 MaxValue 64
set_interface_parameter_attribute $Ifc_Id W_IMN_DATA64 InterfaceLink "<open>"

create_interface_parameter W_INT_ID \
    -interface $Ifc_Id \
    -type integer \
    -specify_on consumer \
    -default 10 \
    -label "Interrupt ID width" \
    -description \
    "Width of Interrupt ID"

set_interface_parameter_attribute $Ifc_Id W_INT_ID MinValue 3
set_interface_parameter_attribute $Ifc_Id W_INT_ID MaxValue 16
#set_interface_parameter_attribute $Ifc_Id W_INT_ID InterfaceLink "<open>"

#############################
# Interface Ports
#############################

####################
# Forward
####################
create_interface_port  CURR_PTOT \
    -interface $Ifc_Id \
    -direction fromProvider \
    -constant one \
    -separate \
    -size 3 \
    -description ""

create_interface_port  INTR_SET_CLR_N \
    -interface $Ifc_Id \
    -direction fromProvider \
    -constant one \
    -separate \
    -size 1 \
    -description "Interrupt clear."

create_interface_port  INTR_ID \
    -interface $Ifc_Id \
    -direction fromProvider \
    -constant zero \
    -separate \
    -size @W_INT_ID \
    -description "Interrupt id."

create_interface_port  INTR_V \
    -interface $Ifc_Id \
    -direction fromProvider \
    -constant zero \
    -separate \
    -size 1 \
    -description "Interrupt valid."

create_interface_port  INTR_SLOT \
    -interface $Ifc_Id \
    -direction fromProvider \
    -constant zero \
    -separate \
    -size 1 \
    -description "Interrupt slot."

create_interface_port  IMN_META \
    -interface $Ifc_Id \
    -direction fromProvider \
    -constant zero \
    -separate \
    -size @W_IMN_META \
    -description "Mgmt meta data"

create_interface_port  IMN_ATOMIC \
    -interface $Ifc_Id \
    -direction fromProvider \
    -constant zero \
    -separate \
    -size 1 \
    -description "Mgmt atomic"

create_interface_port  IMN_DONE \
    -interface $Ifc_Id \
    -direction fromProvider \
    -constant zero \
    -separate \
    -size 1 \
    -description "Mgmt done"

create_interface_port  IMN_UERR \
    -interface $Ifc_Id \
    -direction fromProvider \
    -constant zero \
    -separate \
    -size 1 \
    -description "Mgmt uerr"

create_interface_port  IMN_RW \
    -interface $Ifc_Id \
    -direction fromProvider \
    -constant zero \
    -separate \
    -size 1 \
    -description "Mgmt rw"

create_interface_port  IMN_ADDR \
    -interface $Ifc_Id \
    -direction fromProvider \
    -constant zero \
    -separate \
    -size @W_IMN_ADDR \
    -description "Mgmt addr"

create_interface_port  IMN_DATA \
    -interface $Ifc_Id \
    -direction fromProvider \
    -constant zero \
    -separate \
    -size @W_IMN_DATA64 \
    -description "Mgmt data"

create_interface_port  IMN_V \
    -interface $Ifc_Id \
    -direction fromProvider \
    -constant zero \
    -separate \
    -size 1 \
    -description "Mgmt valid"

create_interface_port  MEM_INIT_DONE \
    -interface $Ifc_Id \
    -direction fromProvider \
    -constant zero \
    -separate \
    -size 1 \
    -description "Mem_Init done"

create_interface_port  FUNC_INIT_DONE \
    -interface $Ifc_Id \
    -direction fromProvider \
    -constant zero \
    -separate \
    -size 1 \
    -description "Func_Init done"

#############################
# Backward
####################
create_interface_port  IMN_E \
    -interface $Ifc_Id \
    -direction fromConsumer \
    -constant zero \
    -size 1 \
    -description "Mgmt e, flow control"

complete_interface_definition $Ifc_Id
unset Ifc_Id
###############################################################################
set Ifc_Id IMN_CSR::CFG_REQ_ACK

create_interface $Ifc_Id  \
    -version $Ifc_Version \
    -description "gmf config req/ack"

#############################
# Interface attributes
#############################
set_interface_attribute $Ifc_Id AutoConnectWhen never

#############################
# Control parameters
#############################


#############################
# Interface Parameters
#############################
create_interface_parameter W_CFG_ADDR \
    -interface $Ifc_Id \
    -type integer \
    -specify_on consumer \
    -default 48 \
    -label "Config address width" \
    -description \
    "Width of config address"

set_interface_parameter_attribute $Ifc_Id W_CFG_ADDR MinValue 48
set_interface_parameter_attribute $Ifc_Id W_CFG_ADDR MaxValue 48
set_interface_parameter_attribute $Ifc_Id W_CFG_ADDR InterfaceLink "<open>"

#############################
# Interface Ports
#############################

####################
# Req
####################
create_interface_port  VALID \
    -interface $Ifc_Id \
    -direction fromProvider \
    -constant zero \
    -separate \
    -size 1 \
    -description "req valid."

create_interface_port  OPCODE \
    -interface $Ifc_Id \
    -direction fromProvider \
    -constant zero \
    -separate \
    -size 4 \
    -description "cfg opcode."

create_interface_port  ADDR \
    -interface $Ifc_Id \
    -direction fromProvider \
    -constant zero \
    -separate \
    -size @W_CFG_ADDR \
    -align right \
    -description "cfg addr."

create_interface_port  BE \
    -interface $Ifc_Id \
    -direction fromProvider \
    -constant zero \
    -separate \
    -size 8 \
    -description "byte enable."

create_interface_port  WR_DATA \
    -interface $Ifc_Id \
    -direction fromProvider \
    -constant zero \
    -separate \
    -size 64 \
    -description "wr_data."

create_interface_port  SAI \
    -interface $Ifc_Id \
    -direction fromProvider \
    -constant zero \
    -separate \
    -size 8 \
    -description "SAI security associate."

create_interface_port  FID \
    -interface $Ifc_Id \
    -direction fromProvider \
    -constant zero \
    -separate \
    -size 8 \
    -description "fid."

create_interface_port  BAR \
    -interface $Ifc_Id \
    -direction fromProvider \
    -constant zero \
    -separate \
    -size 3 \
    -description "bar."

####################
# Ack
####################
create_interface_port  READ_VALID \
    -interface $Ifc_Id \
    -direction fromConsumer \
    -constant zero \
    -size 1 \
    -description "read_valid"

create_interface_port  READ_MISS \
    -interface $Ifc_Id \
    -direction fromConsumer \
    -constant zero \
    -size 1 \
    -description "read_miss"

create_interface_port  WRITE_VALID \
    -interface $Ifc_Id \
    -direction fromConsumer \
    -constant zero \
    -size 1 \
    -description "write_valid"

create_interface_port  WRITE_MISS \
    -interface $Ifc_Id \
    -direction fromConsumer \
    -constant zero \
    -size 1 \
    -description "write_miss"

create_interface_port  SAI_SUCCESSFULL \
    -interface $Ifc_Id \
    -direction fromConsumer \
    -constant zero \
    -size 1 \
    -description "sai_successfull"

create_interface_port  RD_DATA \
    -interface $Ifc_Id \
    -direction fromConsumer \
    -constant zero \
    -size 64 \
    -description "rd_data"

complete_interface_definition $Ifc_Id
unset Ifc_Id
unset Ifc_Version
###############################################################################
