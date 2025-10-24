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
# Interface definitions for XFI Interfaces
##############################################################################
set Ifc_Version 0.3

##############################################################################
set Ifc_Id xFI::RD_REQ

create_interface $Ifc_Id \
    -version     $Ifc_Version \
    -description "xFI::RD_REQ Definition" 

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
create_interface_parameter FABRIC_ID_WIDTH \
    -interface   $Ifc_Id \
    -type        integer \
    -specify_on  provider \
    -default     8 \
    -label       "FABRIC_ID_WIDTH" \
    -description "" 


create_interface_parameter FABRIC_MAX_MEM_ADDR \
    -interface   $Ifc_Id \
    -type        integer \
    -specify_on  provider \
    -default     38 \
    -label       "FABRIC_MAX_MEM_ADDR" \
    -description "" 


create_interface_parameter FABRIC_TAG_WIDTH \
    -interface   $Ifc_Id \
    -type        integer \
    -specify_on  provider \
    -default     8 \
    -label       "FABRIC_TAG_WIDTH" \
    -description "" 


create_interface_parameter FABRIC_XTAG_WIDTH \
    -interface   $Ifc_Id \
    -type        integer \
    -specify_on  provider \
    -default     8 \
    -label       "FABRIC_XTAG_WIDTH" \
    -description "" 


create_interface_parameter FABRIC_BLKOFFSET_WIDTH \
    -interface   $Ifc_Id \
    -type        integer \
    -specify_on  provider \
    -default     8 \
    -label       "FABRIC_BLKOFFSET_WIDTH" \
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
"Asserted by NOC-Bridge to send 1 Read Request credit to the xFI."

create_interface_port vld \
    -interface   $Ifc_Id \
    -direction   fromProvider \
    -constant    zero \
    -separate    \
    -size        1 \
    -align       right \
    -description \
"Asserted by xFI when it has a valid Read Request request to send to the NOC-
      Bridge to the fabric memory map."

create_interface_port cons_id \
    -interface   $Ifc_Id \
    -direction   fromProvider \
    -constant    zero \
    -separate    \
    -size        @FABRIC_ID_WIDTH \
    -align       right \
    -description \
"Consumer ID for routing the Read Request on the NOC. Each xFI has Fabric
      Address decode logic to determine the Consumer ID for the request based on
      the Fabric Address."

create_interface_port prod_id \
    -interface   $Ifc_Id \
    -direction   fromProvider \
    -constant    zero \
    -separate    \
    -size        @FABRIC_ID_WIDTH \
    -align       right \
    -description \
"Producer ID of the Read Request, Used by the Request Consumer to route the Read
      Response back to the requester on the fabric. The Request Producer ID
      value should be use as the Response Consumer ID."

create_interface_port addr \
    -interface   $Ifc_Id \
    -direction   fromProvider \
    -constant    zero \
    -separate    \
    -size        @FABRIC_MAX_MEM_ADDR-6 \
    -align       left \
    -description \
"64-Byte Aligned Address of the 64-Byte block to be read at the request
      consumer."

create_interface_port blklen \
    -interface   $Ifc_Id \
    -direction   fromProvider \
    -constant    zero \
    -separate    \
    -size        3 \
    -align       right \
    -description \
"Read Request block length specifies the number of 64-Bytes that will be read"

create_interface_port rsp \
    -interface   $Ifc_Id \
    -direction   fromProvider \
    -constant    zero \
    -separate    \
    -size        2 \
    -align       right \
    -description \
"Encodes what type of Tag information is encoded in the Read Request and will be
      returned with Read Responses."

create_interface_port tag \
    -interface   $Ifc_Id \
    -direction   fromProvider \
    -constant    zero \
    -separate    \
    -size        @FABRIC_TAG_WIDTH \
    -align       right \
    -description \
"Tag (ATag or NTag)"

create_interface_port xtag \
    -interface   $Ifc_Id \
    -direction   fromProvider \
    -constant    zero \
    -separate    \
    -size        @FABRIC_XTAG_WIDTH \
    -align       right \
    -description \
"Extended Tag (PktID or Reserved)"

create_interface_port blkoffset \
    -interface   $Ifc_Id \
    -direction   fromProvider \
    -constant    zero \
    -separate    \
    -size        @FABRIC_BLKOFFSET_WIDTH \
    -align       right \
    -description \
"This may be either an BlkOffset or Reserved for xFI private usage based on
      xfi2n_rdreq.rsp"

create_interface_port rsv \
    -interface   $Ifc_Id \
    -direction   fromProvider \
    -constant    zero \
    -separate    \
    -size        4 \
    -align       right \
    -description \
"These bits are reserved for xFI private usage."

create_interface_port cparity \
    -interface   $Ifc_Id \
    -direction   fromProvider \
    -constant    zero \
    -separate    \
    -size        1 \
    -align       right \
    -description \
"1 parity bit for all bits of the interface (excluding RdReq_Valid,
      RdReq_Credit, and RdReq_ConsID)"

complete_interface_definition $Ifc_Id 
unset Ifc_Id 
###############################################################################

set Ifc_Id xFI::RD_RSP

create_interface $Ifc_Id \
    -version     $Ifc_Version \
    -description "xFI::RD_RSP Definition" 

##############################################################################
# Interface attributes
##############################################################################
set_interface_attribute $Ifc_Id AutoConnectWhen never

##############################################################################
# Control Parameters
##############################################################################
create_interface_parameter CONS_ID_EN \
    -interface   $Ifc_Id \
    -type        boolean \
    -specify_on  provider \
    -control      \
    -default     false \
    -label       "CONS_ID_EN" \
    -description "" 


set_interface_parameter_attribute $Ifc_Id CONS_ID_EN SymbolicNames {no yes}
set_interface_parameter_attribute $Ifc_Id CONS_ID_EN InterfaceLink "<open>"
create_interface_parameter QOS_EN \
    -interface   $Ifc_Id \
    -type        boolean \
    -specify_on  provider \
    -control      \
    -default     false \
    -label       "QOS_EN" \
    -description "" 


set_interface_parameter_attribute $Ifc_Id QOS_EN SymbolicNames {no yes}
set_interface_parameter_attribute $Ifc_Id QOS_EN InterfaceLink "<open>"
##############################################################################
# Parameters
##############################################################################
create_interface_parameter FABRIC_ID_WIDTH \
    -interface   $Ifc_Id \
    -type        integer \
    -specify_on  provider \
    -default     8 \
    -label       "FABRIC_ID_WIDTH" \
    -description "" 


create_interface_parameter FABRIC_TAG_WIDTH \
    -interface   $Ifc_Id \
    -type        integer \
    -specify_on  provider \
    -default     8 \
    -label       "FABRIC_TAG_WIDTH" \
    -description "" 


create_interface_parameter FABRIC_XTAG_WIDTH \
    -interface   $Ifc_Id \
    -type        integer \
    -specify_on  provider \
    -default     8 \
    -label       "FABRIC_XTAG_WIDTH" \
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
create_interface_port credit \
    -interface   $Ifc_Id \
    -direction   fromConsumer \
    -constant    zero \
    -separate    \
    -size        1 \
    -align       right \
    -description \
"Asserted by the NOC-Bridge to send a Read Response credit to the xFI or FSU."

create_interface_port vld \
    -interface   $Ifc_Id \
    -direction   fromProvider \
    -constant    zero \
    -separate    \
    -size        1 \
    -align       right \
    -description \
"Read Responses are returned for requests sent on the NOC Read interface and
      some requests sent on the NOC Write interface"

create_interface_port cons_id \
    -interface   $Ifc_Id \
    -direction   fromProvider \
    -constant    zero \
    -used        @CONS_ID_EN \
    -optional    \
    -separate    \
    -size        @FABRIC_ID_WIDTH \
    -align       right \
    -description \
"Consumer ID for routing the Read Response on the NOC. The Producer ID of the
      Request becomes the Consumer ID of the Response."

create_interface_port qos \
    -interface   $Ifc_Id \
    -direction   fromProvider \
    -constant    zero \
    -used        @QOS_EN \
    -optional    \
    -separate    \
    -size        4 \
    -align       right \
    -description \
"Quality of Service."

create_interface_port rsp \
    -interface   $Ifc_Id \
    -direction   fromProvider \
    -constant    zero \
    -separate    \
    -size        2 \
    -align       right \
    -description \
"Encodes what type of Tag information is returned with the Response."

create_interface_port tag \
    -interface   $Ifc_Id \
    -direction   fromProvider \
    -constant    zero \
    -separate    \
    -size        @FABRIC_TAG_WIDTH \
    -align       right \
    -description \
"Tag (ATag or NTag)"

create_interface_port xtag \
    -interface   $Ifc_Id \
    -direction   fromProvider \
    -constant    zero \
    -separate    \
    -size        @FABRIC_XTAG_WIDTH \
    -align       right \
    -description \
"Extended Tag (PktID or Reserved)"

create_interface_port blkoffset \
    -interface   $Ifc_Id \
    -direction   fromProvider \
    -constant    zero \
    -separate    \
    -size        @FABRIC_BLKOFFSET_WIDTH \
    -align       right \
    -description \
"This may be either an BlkOffset or Reserved for xFI private usage based on
      xfin_rdrsp.rsp"

create_interface_port data \
    -interface   $Ifc_Id \
    -direction   fromConsumer \
    -constant    zero \
    -separate    \
    -size        512 \
    -align       right \
    -description \
"Read Response Data (64 Bytes)"

create_interface_port blkstatus \
    -interface   $Ifc_Id \
    -direction   fromProvider \
    -constant    zero \
    -separate    \
    -size        1 \
    -align       right \
    -description \
"Block Status for the 64 Bytes of Data to be stored in the FSU and used by the
      xFI for global ordering of Packet Writes from 1 Agent followed by Packet
      Reads from another Agent"

create_interface_port status \
    -interface   $Ifc_Id \
    -direction   fromProvider \
    -constant    zero \
    -separate    \
    -size        @FABRIC_RSP_STATUS_WIDTH \
    -align       right \
    -description \
""

create_interface_port rsv \
    -interface   $Ifc_Id \
    -direction   fromProvider \
    -constant    zero \
    -separate    \
    -size        4 \
    -align       right \
    -description \
"Reserved. These bits are reserved for xFI private usage."

create_interface_port dparity \
    -interface   $Ifc_Id \
    -direction   fromProvider \
    -constant    zero \
    -separate    \
    -size        8 \
    -align       right \
    -description \
"1 parity bit for each 64-bits of Data"

create_interface_port cparity \
    -interface   $Ifc_Id \
    -direction   fromProvider \
    -constant    zero \
    -separate    \
    -size        1 \
    -align       right \
    -description \
"1 parity bit for all non-data bits of the interface "

complete_interface_definition $Ifc_Id 
unset Ifc_Id 
###############################################################################

set Ifc_Id xFI::WR_REQ

create_interface $Ifc_Id \
    -version     $Ifc_Version \
    -description "xFI::WR_REQ Definition" 

##############################################################################
# Interface attributes
##############################################################################
set_interface_attribute $Ifc_Id AutoConnectWhen never

##############################################################################
# Control Parameters
##############################################################################
create_interface_parameter CONS_ID_EN \
    -interface   $Ifc_Id \
    -type        boolean \
    -specify_on  provider \
    -control      \
    -default     false \
    -label       "CONS_ID_EN" \
    -description "" 


set_interface_parameter_attribute $Ifc_Id CONS_ID_EN SymbolicNames {no yes}
set_interface_parameter_attribute $Ifc_Id CONS_ID_EN InterfaceLink "<open>"
##############################################################################
# Parameters
##############################################################################
create_interface_parameter FABRIC_ID_WIDTH \
    -interface   $Ifc_Id \
    -type        integer \
    -specify_on  provider \
    -default     8 \
    -label       "FABRIC_ID_WIDTH" \
    -description "" 


create_interface_parameter FABRIC_MAX_MEM_ADDR \
    -interface   $Ifc_Id \
    -type        integer \
    -specify_on  provider \
    -default     38 \
    -label       "FABRIC_MAX_MEM_ADDR" \
    -description "" 


create_interface_parameter FABRIC_NTAG_WIDTH \
    -interface   $Ifc_Id \
    -type        integer \
    -specify_on  provider \
    -default     8 \
    -label       "FABRIC_NTAG_WIDTH" \
    -description "" 


create_interface_parameter FABRIC_XTAG_WIDTH \
    -interface   $Ifc_Id \
    -type        integer \
    -specify_on  provider \
    -default     8 \
    -label       "FABRIC_XTAG_WIDTH" \
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
"Asserted by NOC-Bridge to send a Write Request request credit to the xFI."

create_interface_port vld \
    -interface   $Ifc_Id \
    -direction   fromProvider \
    -constant    zero \
    -separate    \
    -size        1 \
    -align       right \
    -description \
"Asserted by xFI when it has a valid Request request to send to the NOC-Bridge."

create_interface_port cons_id \
    -interface   $Ifc_Id \
    -direction   fromProvider \
    -constant    zero \
    -used        @CONS_ID_EN \
    -optional    \
    -separate    \
    -size        @FABRIC_ID_WIDTH \
    -align       right \
    -description \
"Consumer ID for routing the Write Request on the NOC."

create_interface_port prod_id \
    -interface   $Ifc_Id \
    -direction   fromProvider \
    -constant    zero \
    -separate    \
    -size        @FABRIC_ID_WIDTH \
    -align       right \
    -description \
"Producer ID of the Request."

create_interface_port cmd \
    -interface   $Ifc_Id \
    -direction   fromProvider \
    -constant    zero \
    -separate    \
    -size        3 \
    -align       right \
    -description \
"Write Request Commands"

create_interface_port addr \
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
    -size        8 \
    -align       right \
    -description \
"Write Request Dword Length"

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
"Write Data"

create_interface_port last \
    -interface   $Ifc_Id \
    -direction   fromProvider \
    -constant    zero \
    -separate    \
    -size        1 \
    -align       right \
    -description \
"This indicates that this is the last flit for the command."

create_interface_port memprot_mode \
    -interface   $Ifc_Id \
    -direction   fromProvider \
    -constant    zero \
    -separate    \
    -size        1 \
    -align       right \
    -description \
"Agent indicates to the FSU the memory protection mode it is setting on the
      64-Byte block of data."

create_interface_port memprot_init \
    -interface   $Ifc_Id \
    -direction   fromProvider \
    -constant    zero \
    -separate    \
    -size        1 \
    -align       right \
    -description \
"xFI asserts this signal to indicates to the FSU to initialize the memory
      protection mode for the 64-Byte block of data."

create_interface_port blkstatus \
    -interface   $Ifc_Id \
    -direction   fromProvider \
    -constant    zero \
    -separate    \
    -size        1 \
    -align       right \
    -description \
"Block Status for the 64 Bytes of Data to be stored in the FSU and used by the
      XFI, NFI, and HFI for global ordering of Packet Writes and Packet Reads"

create_interface_port rsp \
    -interface   $Ifc_Id \
    -direction   fromProvider \
    -constant    zero \
    -separate    \
    -size        2 \
    -align       right \
    -description \
"Encodes whether a Write Response is returned by the Request Consumer and what
      type of Tag information is returned with the Write Response"

create_interface_port ntag \
    -interface   $Ifc_Id \
    -direction   fromProvider \
    -constant    zero \
    -separate    \
    -size        @FABRIC_NTAG_WIDTH \
    -align       right \
    -description \
"NTag (NOC Tag)."

create_interface_port xtag \
    -interface   $Ifc_Id \
    -direction   fromProvider \
    -constant    zero \
    -separate    \
    -size        @FABRIC_XTAG_WIDTH \
    -align       right \
    -description \
"Extended Tag (ATag or PktID)"

create_interface_port dparity \
    -interface   $Ifc_Id \
    -direction   fromProvider \
    -constant    zero \
    -separate    \
    -size        8 \
    -align       right \
    -description \
"1 parity bit for each 64-bits of Data"

create_interface_port cparity \
    -interface   $Ifc_Id \
    -direction   fromProvider \
    -constant    zero \
    -separate    \
    -size        1 \
    -align       right \
    -description \
"1 parity bit for all non-Data bits of the interface (excluding WrReq_Valid,
      WrReq_Credit, and WrReq_ConsID)"

complete_interface_definition $Ifc_Id 
unset Ifc_Id 
###############################################################################

set Ifc_Id xFI::WR_RSP

create_interface $Ifc_Id \
    -version     $Ifc_Version \
    -description "xFI::WR_RSP Definition" 

##############################################################################
# Interface attributes
##############################################################################
set_interface_attribute $Ifc_Id AutoConnectWhen never

##############################################################################
# Control Parameters
##############################################################################
create_interface_parameter CONS_ID_EN \
    -interface   $Ifc_Id \
    -type        boolean \
    -specify_on  provider \
    -control      \
    -default     false \
    -label       "CONS_ID_EN" \
    -description "" 


set_interface_parameter_attribute $Ifc_Id CONS_ID_EN SymbolicNames {no yes}
set_interface_parameter_attribute $Ifc_Id CONS_ID_EN InterfaceLink "<open>"
create_interface_parameter QOS_EN \
    -interface   $Ifc_Id \
    -type        boolean \
    -specify_on  provider \
    -control      \
    -default     false \
    -label       "QOS_EN" \
    -description "" 


set_interface_parameter_attribute $Ifc_Id QOS_EN SymbolicNames {no yes}
set_interface_parameter_attribute $Ifc_Id QOS_EN InterfaceLink "<open>"
##############################################################################
# Parameters
##############################################################################
create_interface_parameter FABRIC_ID_WIDTH \
    -interface   $Ifc_Id \
    -type        integer \
    -specify_on  provider \
    -default     8 \
    -label       "FABRIC_ID_WIDTH" \
    -description "" 


create_interface_parameter FABRIC_NTAG_WIDTH \
    -interface   $Ifc_Id \
    -type        integer \
    -specify_on  provider \
    -default     8 \
    -label       "FABRIC_NTAG_WIDTH" \
    -description "" 


create_interface_parameter FABRIC_XTAG_WIDTH \
    -interface   $Ifc_Id \
    -type        integer \
    -specify_on  provider \
    -default     8 \
    -label       "FABRIC_XTAG_WIDTH" \
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
create_interface_port credit \
    -interface   $Ifc_Id \
    -direction   fromConsumer \
    -constant    zero \
    -separate    \
    -size        1 \
    -align       right \
    -description \
"Asserted by NOC-Bridge to send a Write Request request credit to the xFI."

create_interface_port cons_id \
    -interface   $Ifc_Id \
    -direction   fromProvider \
    -constant    zero \
    -used        @CONS_ID_EN \
    -optional    \
    -separate    \
    -size        @FABRIC_ID_WIDTH \
    -align       right \
    -description \
"Consumer ID for routing the Write Response on the NOC. The Producer ID of the
      Request becomes the Consumer ID of the Response."

create_interface_port vld \
    -interface   $Ifc_Id \
    -direction   fromProvider \
    -constant    zero \
    -separate    \
    -size        1 \
    -align       right \
    -description \
"Asserted by xFI when it has a valid Request request to send to the NOC-Bridge."

create_interface_port qos \
    -interface   $Ifc_Id \
    -direction   fromProvider \
    -constant    zero \
    -used        @QOS_EN \
    -optional    \
    -separate    \
    -size        4 \
    -align       right \
    -description \
"Quality of Service."

create_interface_port rsp \
    -interface   $Ifc_Id \
    -direction   fromProvider \
    -constant    zero \
    -separate    \
    -size        2 \
    -align       right \
    -description \
"Encodes whether a Write Response is returned by the Request Consumer and what
      type of Tag information is returned with the Write Response"

create_interface_port ntag \
    -interface   $Ifc_Id \
    -direction   fromProvider \
    -constant    zero \
    -separate    \
    -size        @FABRIC_NTAG_WIDTH \
    -align       right \
    -description \
"NTag (NOC Tag)."

create_interface_port xtag \
    -interface   $Ifc_Id \
    -direction   fromProvider \
    -constant    zero \
    -separate    \
    -size        @FABRIC_XTAG_WIDTH \
    -align       right \
    -description \
"Extended Tag (ATag or PktID)"

create_interface_port status \
    -interface   $Ifc_Id \
    -direction   fromProvider \
    -constant    zero \
    -separate    \
    -size        @FABRIC_RSP_STATUS_WIDTH \
    -align       right \
    -description \
"Response status."

create_interface_port a2a_credit \
    -interface   $Ifc_Id \
    -direction   fromProvider \
    -constant    zero \
    -separate    \
    -size        2 \
    -align       right \
    -description \
"Agent2Agent Credit Return"

create_interface_port a2a_credit_wdf \
    -interface   $Ifc_Id \
    -direction   fromProvider \
    -constant    zero \
    -separate    \
    -size        4 \
    -align       right \
    -description \
"Agent2Agent Write Data Flit Credit Return"

create_interface_port cparity \
    -interface   $Ifc_Id \
    -direction   fromProvider \
    -constant    zero \
    -separate    \
    -size        1 \
    -align       right \
    -description \
"1 parity bit for all non-Data bits of the interface (excluding WrReq_Valid,
      WrReq_Credit, and WrReq_ConsID)"

complete_interface_definition $Ifc_Id 
unset Ifc_Id 
###############################################################################

unset Ifc_Version 
