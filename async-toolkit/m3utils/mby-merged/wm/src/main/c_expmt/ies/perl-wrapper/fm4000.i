/* vim:et:sw=4:syntax=c:ts=4:tw=79:
 */
/******************************************************************************
 * File:            fm4000.i
 * Creation Date:   June 10, 2008
 * Description:     
 *
 * INTEL CONFIDENTIAL
 * Copyright 2008 - 2012 Intel Corporation. All Rights Reserved. 
 *
 * The source code contained or described herein and all documents related
 * to the source code ("Material") are owned by Intel Corporation or its
 * suppliers or licensors. Title to the Material remains with Intel
 * Corporation or its suppliers and licensors. The Material contains trade
 * secrets and proprietary and confidential information of Intel or its
 * suppliers and licensors. The Material is protected by worldwide copyright
 * and trade secret laws and treaty provisions. No part of the Material may
 * be used, copied, reproduced, modified, published, uploaded, posted,
 * transmitted, distributed, or disclosed in any way without Intel's prior
 * express written permission.
 *
 * No license under any patent, copyright, trade secret or other intellectual
 * property right is granted to or conferred upon you by disclosure or 
 * delivery of the Materials, either expressly, by implication, inducement,
 * estoppel or otherwise. Any license under such intellectual property rights
 * must be express and approved by Intel in writing.
 *****************************************************************************/

%module "SDK::FM4000";

%{

#include "fm_sdk_fm4000_int.h"

#include "sdk_types.h"

%}

%include "sdk_library.i"
%include "sdk_fragments.i"
#define FM_SWIG_AUXILARY_MODULE
%include "sdk_perlcode.i"
%include "sdk_typemaps.i"
%include <reference.i>
%apply int *REFERENCE { int * };

%import "sdk.i"
%import "sdk_int.i"

/* The following headers are parsed for conversion.  */
%include "fm_sdk_fm4000_int.h"
