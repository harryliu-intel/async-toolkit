/* vim:ts=4:sw=4:expandtab
 * (No tabs, indent level is 4 spaces)  */
/*****************************************************************************
 * File:            fm_alos_dynamic_load.h
 * Creation Date:   2011
 * Description:     ALOS routines for dynamically loading code libraries.
 *
 * INTEL CONFIDENTIAL
 * Copyright 2011  Intel Corporation. All Rights Reserved. 
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

#ifndef __FM_FM_ALOS_DYNAMIC_LOAD_H
#define __FM_FM_ALOS_DYNAMIC_LOAD_H


extern fm_status fmOpenDynamicLoadLibrary(fm_text filePath, fm_int *handle);
extern fm_status fmCloseDynamicLoadLibrary(fm_int handle);
extern fm_status fmGetDynamicLoadSymbol(fm_int  handle,
                                        fm_text symName,
                                        void ** symAddr);
extern fm_status fmLoadDynamicLoadLibrary(fm_int handle);
extern fm_uint64 fmProcessDynLoadLibStatus;

#define FM_CALL_DYN_FUNC(handle, funcPtr, status, ...)      \
    if ( !( fmProcessDynLoadLibStatus & (1 << handle) ) )   \
    {                                                       \
        status = fmLoadDynamicLoadLibrary(handle);          \
    }                                                       \
    else                                                    \
    {                                                       \
        status = FM_OK;                                     \
    }                                                       \
    if (status == FM_OK)                                    \
    {                                                       \
        if (funcPtr != NULL)                                \
        {                                                   \
            status = funcPtr(__VA_ARGS__);                  \
        }                                                   \
        else                                                \
        {                                                   \
            status = FM_ERR_UNSUPPORTED;                    \
        }                                                   \
    }


#endif /* __FM_FM_ALOS_DYNAMIC_LOAD_H */
