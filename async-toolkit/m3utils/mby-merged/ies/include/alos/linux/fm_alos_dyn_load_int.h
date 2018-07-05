/* vim:ts=4:sw=4:expandtab
 * (No tabs, indent level is 4 spaces)  */
/*****************************************************************************
 * File:            fm_alos_dyn_load_int.h
 * Creation Date:   2011
 * Description:     Internal definitions related to dynamic-load-libraries.
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

#ifndef __FM_FM_ALOS_DYN_LOAD_INT_H
#define __FM_FM_ALOS_DYN_LOAD_INT_H


/******************************************************************************
 *  Structure used to support dynamic load libraries.
 *****************************************************************************/
typedef struct _fm_dynLoadLib
{
    /* Path to the dynamic-load library. */
    fm_text             filePath;

    /* Usage Count: the number of times the library has been opened
     * minus the number of times the library has been closed. */
    fm_int              useCount;

} fm_dynLoadLib;


extern fm_status fmInitDynamicLoadLibs(void);



#endif /* __FM_FM_ALOS_DYN_LOAD_INT_H */
