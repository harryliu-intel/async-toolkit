#!/usr/intel/pkgs/bash/4.2/bin/bash
##############################################################################
# File:          batch_gen.sh 
# Creation Date: 05/18/2017
# Description:   Generate register code for all RTL versions
#                
#                -------------------------------------------------
#                INTERNAL USE ONLY - NOT FOR EXTERNAL DISTRIBUTION
#                -------------------------------------------------
#
# INTEL CONFIDENTIAL
# Copyright 2017 Intel Corporation. All Rights Reserved.
#
# The source code contained or described herein and all documents related
# to the source code ("Material") are owned by Intel Corporation or its
# suppliers or licensors. Title to the Material remains with Intel
# Corporation or its suppliers and licensors. The Material contains trade
# secrets and proprietary and confidential information of Intel or its
# suppliers and licensors. The Material is protected by worldwide copyright
# and trade secret laws and treaty provisions. No part of the Material may
# be used, copied, reproduced, modified, published, uploaded, posted,
# transmitted, distributed, or disclosed in any way without Intel's prior
# express written permission.
#
# No license under any patent, copyright, trade secret or other intellectual
# property right is granted to or conferred upon you by disclosure or
# delivery of the Materials, either expressly, by implication, inducement,
# estoppel or otherwise. Any license under such intellectual property rights
# must be express and approved by Intel in writing.
##############################################################################

function findParentDir() {
    local path_to_find=$1
    SPATH=`pwd`;
    while [[ "$SPATH" != "" ]]; do
        if [[ -e "$SPATH/$path_to_find" ]]; then
            break;
        else
            SPATH=`echo $SPATH | sed -e 's/\(.*\)\/.*/\1/'`;
        fi
    done
}

# Default to default WM repository name if not specified.
if [ "${WM_DIR}" == "" ]; then
    findParentDir "nd_ies-hlp_wm"
    WM_DIR=${SPATH}/nd_ies-hlp_wm
fi

# Specify each manually because each will contain different arguments

RTL_VERSION=0P8-16ww51e_p1 FPPS_REG_VERSION=0x0807 FPPS_REG_TAG=0x10033 make gen

RDL_DIR=${WM_DIR}/srdl RTL_VERSION=wm FPPS_REG_VERSION=0x1008 FPPS_REG_TAG=0x12086 make gen

