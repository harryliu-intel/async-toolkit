#------------------------------------------------------------------------------
#
#  INTEL CONFIDENTIAL
#
#  Copyright 2017 Intel Corporation All Rights Reserved.
#
#  The source code contained or described herein and all documents related
#  to the source code (Material) are owned by Intel Corporation or its 
#  suppliers or licensors. Title to the Material remains with Intel
#  Corporation or its suppliers and licensors. The Material contains trade
#  secrets and proprietary and confidential information of Intel or its 
#  suppliers and licensors. The Material is protected by worldwide copyright
#  and trade secret laws and treaty provisions. No part of the Material may 
#  be used, copied, reproduced, modified, published, uploaded, posted,
#  transmitted, distributed, or disclosed in any way without Intel's prior
#  express written permission.
#
#  No license under any patent, copyright, trade secret or other intellectual
#  property right is granted to or conferred upon you by disclosure or
#  delivery of the Materials, either expressly, by implication, inducement,
#  estoppel or otherwise. Any license under such intellectual property rights
#  must be express and approved by Intel in writing.
#
#------------------------------------------------------------------------------
# Created 2017-04-07 15:56:22 by wliu23
#------------------------------------------------------------------------------
##############################################################################
# Interface definitions for XFI_AGT Interfaces
##############################################################################
set Ifc_Version 0.3

##############################################################################
set Ifc_Id xFI_AGT::RD_REQ

create_interface $Ifc_Id \
    -version     $Ifc_Version \
    -description "xFI_AGT::RD_REQ Definition" 

##############################################################################
# Interface attributes
##############################################################################
set_interface_attribute $Ifc_Id AutoConnectWhen never

##############################################################################
# Control Parameters
##############################################################################
##############################################################################
# Parameters
##############################################################################
create_interface_parameter FABRIC_MAX_MEM_ADDR \
    -interface   $Ifc_Id \
    -type        integer \
    -specify_on  provider \
    -default     38 \
    -label       "FABRIC_MAX_MEM_ADDR" \
    -description "" 


create_interface_parameter FABRIC_ATAG_WIDTH \
    -interface   $Ifc_Id \
    -type        integer \
    -specify_on  provider \
    -default     8 \
    -label       "FABRIC_ATAG_WIDTH" \
    -description "" 


create_interface_parameter XFI_MSTR_PKTID_WIDTH \
    -interface   $Ifc_Id \
    -type        integer \
    -specify_on  provider \
    -default     8 \
    -label       "XFI_MSTR_PKTID_WIDTH" \
    -description "" 


##############################################################################
# Ports
##############################################################################
create_interface_port credit \
    -interface   $Ifc_Id \
    -direction   fromConsumer \
    -constant    zero \
    -separate    \
    -size        1 \
    -align       right \
    -description \
"Asserted by xFI to send 1 Read Request Interface credit to the Agent."

create_interface_port vld \
    -interface   $Ifc_Id \
    -direction   fromProvider \
    -constant    zero \
    -separate    \
    -size        1 \
    -align       right \
    -description \
"Asserted by Agent when it has a valid Read Request request to send to the xFI."

create_interface_port cmd \
    -interface   $Ifc_Id \
    -direction   fromProvider \
    -constant    zero \
    -separate    \
    -size        3 \
    -align       right \
    -description \
"Read request commands."

create_interface_port options \
    -interface   $Ifc_Id \
    -direction   fromProvider \
    -constant    zero \
    -separate    \
    -size        55 \
    -align       right \
    -description \
"xFI_RdPktInit and xFI_PktDealloc"

create_interface_port atag \
    -interface   $Ifc_Id \
    -direction   fromProvider \
    -constant    zero \
    -separate    \
    -size        @FABRIC_ATAG_WIDTH \
    -align       right \
    -description \
"Unique Request Tag assigned by the Agent."

create_interface_port blklen \
    -interface   $Ifc_Id \
    -direction   fromProvider \
    -constant    zero \
    -separate    \
    -size        8 \
    -align       right \
    -description \
"Number of 64-Byte blocks to read."

create_interface_port addr \
    -interface   $Ifc_Id \
    -direction   fromProvider \
    -constant    zero \
    -separate    \
    -size        @FABRIC_MAX_MEM_ADDR-6 \
    -align       left \
    -description \
"This is the 64-Byte aligned address in fabric memory or DSEG Ptr."

create_interface_port pktid \
    -interface   $Ifc_Id \
    -direction   fromProvider \
    -constant    zero \
    -separate    \
    -size        @XFI_MSTR_PKTID_WIDTH \
    -align       right \
    -description \
"PktID is assigned by the Agent. "

create_interface_port startblk \
    -interface   $Ifc_Id \
    -direction   fromProvider \
    -constant    zero \
    -separate    \
    -size        8 \
    -align       right \
    -description \
"PktID is assigned by the Agent. "

complete_interface_definition $Ifc_Id 
unset Ifc_Id 
###############################################################################

set Ifc_Id xFI_AGT::RD_RSP

create_interface $Ifc_Id \
    -version     $Ifc_Version \
    -description "xFI_AGT::RD_RSP Definition" 

##############################################################################
# Interface attributes
##############################################################################
set_interface_attribute $Ifc_Id AutoConnectWhen never

##############################################################################
# Control Parameters
##############################################################################
##############################################################################
# Parameters
##############################################################################
create_interface_parameter XFI_MSTR_PKTID_WIDTH \
    -interface   $Ifc_Id \
    -type        integer \
    -specify_on  provider \
    -default     8 \
    -label       "XFI_MSTR_PKTID_WIDTH" \
    -description "" 


create_interface_parameter FABRIC_ATAG_WIDTH \
    -interface   $Ifc_Id \
    -type        integer \
    -specify_on  provider \
    -default     8 \
    -label       "FABRIC_ATAG_WIDTH" \
    -description "" 


create_interface_parameter FABRIC_BLKOFFSET_WIDTH \
    -interface   $Ifc_Id \
    -type        integer \
    -specify_on  provider \
    -default     8 \
    -label       "FABRIC_BLKOFFSET_WIDTH" \
    -description "" 


create_interface_parameter FABRIC_RSP_STATUS_WIDTH \
    -interface   $Ifc_Id \
    -type        integer \
    -specify_on  provider \
    -default     3 \
    -label       "FABRIC_RSP_STATUS_WIDTH" \
    -description "" 


##############################################################################
# Ports
##############################################################################
create_interface_port vld \
    -interface   $Ifc_Id \
    -direction   fromConsumer \
    -constant    zero \
    -separate    \
    -size        1 \
    -align       right \
    -description \
"Asserted by xFI when it has a valid Read Response to send to the Agent."

create_interface_port atag \
    -interface   $Ifc_Id \
    -direction   fromConsumer \
    -constant    zero \
    -separate    \
    -size        @FABRIC_ATAG_WIDTH \
    -align       right \
    -description \
"The ATag (Agent Tag) is returned with the Read Response without modification to
      the Agent."

create_interface_port pktid_vld \
    -interface   $Ifc_Id \
    -direction   fromProvider \
    -constant    zero \
    -separate    \
    -size        1 \
    -align       right \
    -description \
"Indicates the PktID field is valid."

create_interface_port pktid \
    -interface   $Ifc_Id \
    -direction   fromProvider \
    -constant    zero \
    -separate    \
    -size        @XFI_MSTR_PKTID_WIDTH \
    -align       right \
    -description \
"PktID as assigned by the Agent. It specifies an entry in theFabric Meta Data
      and Packet State Cache used for all accesses to the packet."

create_interface_port blkoffset \
    -interface   $Ifc_Id \
    -direction   fromConsumer \
    -constant    zero \
    -separate    \
    -size        @FABRIC_BLKOFFSET_WIDTH \
    -align       right \
    -description \
"The Block Offset indicates which 64B of a Read Request is being returned with
      this Read Response."

create_interface_port data \
    -interface   $Ifc_Id \
    -direction   fromConsumer \
    -constant    zero \
    -separate    \
    -size        512 \
    -align       right \
    -description \
"Read Response Data (64 Bytes)"

create_interface_port status \
    -interface   $Ifc_Id \
    -direction   fromProvider \
    -constant    zero \
    -separate    \
    -size        @FABRIC_RSP_STATUS_WIDTH \
    -align       right \
    -description \
"Status field"

create_interface_port dparity \
    -interface   $Ifc_Id \
    -direction   fromProvider \
    -constant    zero \
    -separate    \
    -size        8 \
    -align       right \
    -description \
"1 parity bit for each 64-bits of Data"

complete_interface_definition $Ifc_Id 
unset Ifc_Id 
###############################################################################

set Ifc_Id xFI_AGT::WR_REQ

create_interface $Ifc_Id \
    -version     $Ifc_Version \
    -description "xFI_AGT::WR_REQ Definition" 

##############################################################################
# Interface attributes
##############################################################################
set_interface_attribute $Ifc_Id AutoConnectWhen never

##############################################################################
# Control Parameters
##############################################################################
##############################################################################
# Parameters
##############################################################################
create_interface_parameter XFI_MSTR_PKTID_WIDTH \
    -interface   $Ifc_Id \
    -type        integer \
    -specify_on  provider \
    -default     8 \
    -label       "XFI_MSTR_PKTID_WIDTH" \
    -description "" 


create_interface_parameter FABRIC_MAX_MEM_ADDR \
    -interface   $Ifc_Id \
    -type        integer \
    -specify_on  provider \
    -default     38 \
    -label       "FABRIC_MAX_MEM_ADDR" \
    -description "" 


create_interface_parameter FABRIC_ATAG_WIDTH \
    -interface   $Ifc_Id \
    -type        integer \
    -specify_on  provider \
    -default     8 \
    -label       "FABRIC_ATAG_WIDTH" \
    -description "" 


##############################################################################
# Ports
##############################################################################
create_interface_port credit \
    -interface   $Ifc_Id \
    -direction   fromConsumer \
    -constant    zero \
    -separate    \
    -size        1 \
    -align       right \
    -description \
"Asserted by xFI to send 1 Write Request Interface credit to the Agent."

create_interface_port vld \
    -interface   $Ifc_Id \
    -direction   fromProvider \
    -constant    zero \
    -separate    \
    -size        1 \
    -align       right \
    -description \
"Asserted by Agent when it has a valid Request request to send to the xFI Write
      Request Interface."

create_interface_port cmd \
    -interface   $Ifc_Id \
    -direction   fromProvider \
    -constant    zero \
    -separate    \
    -size        4 \
    -align       right \
    -description \
"Write Request Commands"

create_interface_port options \
    -interface   $Ifc_Id \
    -direction   fromProvider \
    -constant    zero \
    -separate    \
    -size        53 \
    -align       left \
    -description \
"Command specific options"

create_interface_port atag \
    -interface   $Ifc_Id \
    -direction   fromProvider \
    -constant    zero \
    -separate    \
    -size        @FABRIC_ATAG_WIDTH \
    -align       right \
    -description \
"For commands that generate a Write Response or Read Response, this is the
      unique Request Agent Tag assigned by the Agent."

create_interface_port pktid \
    -interface   $Ifc_Id \
    -direction   fromProvider \
    -constant    zero \
    -separate    \
    -size        @XFI_MSTR_PKTID_WIDTH \
    -align       right \
    -description \
"PktID is assigned by the Agent. It specifies an entry in theFabric Meta Data
      and Packet State Cache used for all accesses to the packet."

create_interface_port startdw \
    -interface   $Ifc_Id \
    -direction   fromProvider \
    -constant    zero \
    -separate    \
    -size        @FABRIC_MAX_MEM_ADDR-2 \
    -align       left \
    -description \
"4-Byte Aligned Address of the first DWord to be written."

create_interface_port dwlen \
    -interface   $Ifc_Id \
    -direction   fromProvider \
    -constant    zero \
    -separate    \
    -size        12 \
    -align       right \
    -description \
"Request DWord Length"

create_interface_port fbe \
    -interface   $Ifc_Id \
    -direction   fromProvider \
    -constant    zero \
    -separate    \
    -size        4 \
    -align       right \
    -description \
"First Byte Enables"

create_interface_port lbe \
    -interface   $Ifc_Id \
    -direction   fromProvider \
    -constant    zero \
    -separate    \
    -size        4 \
    -align       right \
    -description \
"Last Byte Enables"

create_interface_port data \
    -interface   $Ifc_Id \
    -direction   fromProvider \
    -constant    zero \
    -separate    \
    -size        512 \
    -align       right \
    -description \
"Write Data is transferred from the Agent on the xFI interface in 64 Byte
      blocks."

create_interface_port last \
    -interface   $Ifc_Id \
    -direction   fromProvider \
    -constant    zero \
    -separate    \
    -size        1 \
    -align       right \
    -description \
"This indicates that this is the last flit for the command."

create_interface_port dparity \
    -interface   $Ifc_Id \
    -direction   fromProvider \
    -constant    zero \
    -separate    \
    -size        8 \
    -align       right \
    -description \
"1 parity bit for each 64-bits of Data"

create_interface_port memprot_mode \
    -interface   $Ifc_Id \
    -direction   fromProvider \
    -constant    zero \
    -separate    \
    -size        1 \
    -align       right \
    -description \
"Memory Protection Mode"

complete_interface_definition $Ifc_Id 
unset Ifc_Id 
###############################################################################

set Ifc_Id xFI_AGT::WR_RSP

create_interface $Ifc_Id \
    -version     $Ifc_Version \
    -description "xFI_AGT::WR_RSP Definition" 

##############################################################################
# Interface attributes
##############################################################################
set_interface_attribute $Ifc_Id AutoConnectWhen never

##############################################################################
# Control Parameters
##############################################################################
##############################################################################
# Parameters
##############################################################################
create_interface_parameter FABRIC_ATAG_WIDTH \
    -interface   $Ifc_Id \
    -type        integer \
    -specify_on  provider \
    -default     8 \
    -label       "FABRIC_ATAG_WIDTH" \
    -description "" 


create_interface_parameter XFI_MSTR_PKTID_WIDTH \
    -interface   $Ifc_Id \
    -type        integer \
    -specify_on  provider \
    -default     8 \
    -label       "XFI_MSTR_PKTID_WIDTH" \
    -description "" 


create_interface_parameter FABRIC_RSP_STATUS_WIDTH \
    -interface   $Ifc_Id \
    -type        integer \
    -specify_on  consumer \
    -default     3 \
    -label       "FABRIC_RSP_STATUS_WIDTH" \
    -description "" 


##############################################################################
# Ports
##############################################################################
create_interface_port vld \
    -interface   $Ifc_Id \
    -direction   fromProvider \
    -constant    zero \
    -separate    \
    -size        1 \
    -align       right \
    -description \
"Asserted by xFI when it has a valid Request request to send to the NOC-Bridge."

create_interface_port atag \
    -interface   $Ifc_Id \
    -direction   fromProvider \
    -constant    zero \
    -separate    \
    -size        @FABRIC_ATAG_WIDTH \
    -align       right \
    -description \
"ATag (Agent Tag)"

create_interface_port pktid_vld \
    -interface   $Ifc_Id \
    -direction   fromConsumer \
    -constant    zero \
    -separate    \
    -size        1 \
    -align       right \
    -description \
"Indicates that the PktID is valid"

create_interface_port pktid \
    -interface   $Ifc_Id \
    -direction   fromConsumer \
    -constant    zero \
    -separate    \
    -size        @XFI_MSTR_PKTID_WIDTH \
    -align       right \
    -description \
"PktID as assigned by the Agent."

create_interface_port status \
    -interface   $Ifc_Id \
    -direction   fromProvider \
    -constant    zero \
    -separate    \
    -size        @FABRIC_RSP_STATUS_WIDTH \
    -align       right \
    -description \
"Response status."

create_interface_port info \
    -interface   $Ifc_Id \
    -direction   fromProvider \
    -constant    zero \
    -separate    \
    -size        78 \
    -align       right \
    -description \
"Information returned with the Write Response"

complete_interface_definition $Ifc_Id 
unset Ifc_Id 
###############################################################################

unset Ifc_Version 
