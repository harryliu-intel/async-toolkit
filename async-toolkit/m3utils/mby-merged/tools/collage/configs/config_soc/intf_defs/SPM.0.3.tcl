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
# Interface definitions for SPM Interfaces
##############################################################################
set Ifc_Version 0.3

##############################################################################
set Ifc_Id xFI::SPM

create_interface $Ifc_Id \
    -version     $Ifc_Version \
    -description "xFI::SPM Definition" 

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
##############################################################################
# Ports
##############################################################################
create_interface_port dealloc_cmd \
    -interface   $Ifc_Id \
    -direction   fromProvider \
    -constant    zero \
    -separate    \
    -size        2 \
    -align       right \
    -description \
"Dealloc  Command.  Indicates how to interpret  the data on the data bus"

create_interface_port dealloc_data \
    -interface   $Ifc_Id \
    -direction   fromProvider \
    -constant    zero \
    -separate    \
    -size        32 \
    -align       right \
    -description \
"Dealloc Data.  3 different formats (Returned Segment, Accounting Update, NOP)
      which is determined  by the CMD."

create_interface_port dealloc_sync \
    -interface   $Ifc_Id \
    -direction   fromProvider \
    -constant    zero \
    -separate    \
    -size        1 \
    -align       right \
    -description \
"Dealloc Beginning of sync cycle."

create_interface_port dealloc_parity \
    -interface   $Ifc_Id \
    -direction   fromProvider \
    -constant    zero \
    -separate    \
    -size        1 \
    -align       right \
    -description \
"Dealloc Parity.  Even Parity  "

create_interface_port alloc_cmd \
    -interface   $Ifc_Id \
    -direction   fromProvider \
    -constant    zero \
    -separate    \
    -size        2 \
    -align       right \
    -description \
"Alloc Command.  Indicates how to interpret  the data on the data bus"

create_interface_port alloc_sync \
    -interface   $Ifc_Id \
    -direction   fromProvider \
    -constant    zero \
    -separate    \
    -size        1 \
    -align       right \
    -description \
"Alloc  Beginning of sync cycle."

create_interface_port alloc_data \
    -interface   $Ifc_Id \
    -direction   fromProvider \
    -constant    zero \
    -separate    \
    -size        20 \
    -align       right \
    -description \
"Alloc Data.  3 different formats (Available Segment, Consumed Segment, NOP)
      which is determined  by the CMD."

create_interface_port alloc_parity \
    -interface   $Ifc_Id \
    -direction   fromProvider \
    -constant    zero \
    -separate    \
    -size        1 \
    -align       right \
    -description \
"Alloc  Parity.  Even Parity  "

complete_interface_definition $Ifc_Id 
unset Ifc_Id 
###############################################################################

unset Ifc_Version 
