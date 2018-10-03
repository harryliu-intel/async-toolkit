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
# Interface definitions for EVT Interfaces
##############################################################################
set Ifc_Version 0.3

##############################################################################
set Ifc_Id EVT::EVT

create_interface $Ifc_Id \
    -version     $Ifc_Version \
    -description "EVT::EVT Definition" 

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
create_interface_parameter REALLOC_EN \
    -interface   $Ifc_Id \
    -type        boolean \
    -specify_on  consumer \
    -control      \
    -default     false \
    -label       "REALLOC_EN" \
    -description "" 


set_interface_parameter_attribute $Ifc_Id REALLOC_EN SymbolicNames {no yes}
set_interface_parameter_attribute $Ifc_Id REALLOC_EN InterfaceLink "<open>"
create_interface_parameter PROD_ID_EN \
    -interface   $Ifc_Id \
    -type        boolean \
    -specify_on  provider \
    -control      \
    -default     false \
    -label       "PROD_ID_EN" \
    -description "" 


set_interface_parameter_attribute $Ifc_Id PROD_ID_EN SymbolicNames {no yes}
set_interface_parameter_attribute $Ifc_Id PROD_ID_EN InterfaceLink "<open>"
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
"Asserted by NOC-Bridge to send an Event credit to the XFI, NFI, or HFI."

create_interface_port vld \
    -interface   $Ifc_Id \
    -direction   fromProvider \
    -constant    zero \
    -separate    \
    -size        1 \
    -align       right \
    -description \
"Asserted by the xFI when it has a valid Event to send to the NOC-Bridge."

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
"Consumer ID for routing the Event on the NOC."

create_interface_port prod_id \
    -interface   $Ifc_Id \
    -direction   fromProvider \
    -constant    zero \
    -used        @PROD_ID_EN \
    -optional    \
    -separate    \
    -size        @FABRIC_ID_WIDTH \
    -align       right \
    -description \
"Producer ID for the Event."

create_interface_port prod_id_parity \
    -interface   $Ifc_Id \
    -direction   fromProvider \
    -constant    zero \
    -used        @PROD_ID_EN \
    -optional    \
    -separate    \
    -size        1 \
    -align       right \
    -description \
"Parity for the Producer ID."

create_interface_port realloc \
    -interface   $Ifc_Id \
    -direction   fromConsumer \
    -constant    zero \
    -used        @REALLOC_EN \
    -optional    \
    -separate    \
    -size        12 \
    -align       right \
    -description \
""

create_interface_port data \
    -interface   $Ifc_Id \
    -direction   fromProvider \
    -constant    zero \
    -separate    \
    -size        128 \
    -align       right \
    -description \
"Event Data"

create_interface_port dparity \
    -interface   $Ifc_Id \
    -direction   fromProvider \
    -constant    zero \
    -separate    \
    -size        2 \
    -align       right \
    -description \
"Quadword parity for the Event data."

complete_interface_definition $Ifc_Id 
unset Ifc_Id 
###############################################################################

set Ifc_Id EVT::FLW

create_interface $Ifc_Id \
    -version     $Ifc_Version \
    -description "EVT::FLW Definition" 

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
    -specify_on  consumer \
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
"Asserted by NOC-Bridge to send an Event credit to the XFI, NFI, or HFI."

create_interface_port vld \
    -interface   $Ifc_Id \
    -direction   fromProvider \
    -constant    zero \
    -separate    \
    -size        1 \
    -align       right \
    -description \
"Asserted by the xFI when it has a valid Event to send to the NOC-Bridge."

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
"Consumer ID for routing the Event on the NOC."

create_interface_port data \
    -interface   $Ifc_Id \
    -direction   fromProvider \
    -constant    zero \
    -separate    \
    -size        16 \
    -align       right \
    -description \
"Event Flow Control Data"

create_interface_port dparity \
    -interface   $Ifc_Id \
    -direction   fromProvider \
    -constant    zero \
    -separate    \
    -size        1 \
    -align       right \
    -description \
"Parity for the Event Flow Control Data."

complete_interface_definition $Ifc_Id 
unset Ifc_Id 
###############################################################################

unset Ifc_Version 
