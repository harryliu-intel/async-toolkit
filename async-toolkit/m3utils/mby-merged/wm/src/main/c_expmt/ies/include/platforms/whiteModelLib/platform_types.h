/* vim:et:sw=4:ts=4:tw=80:
 * (No tabs, indent level is 4 spaces)  */
/*****************************************************************************
 * File:            platform_types.h
 * Creation Date:   June 5, 2012
 * Description:     Platform specific definitions
 *
 * INTEL CONFIDENTIAL
 * Copyright 2005 - 2013 Intel Corporation. All Rights Reserved.
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

#ifndef __FM_PLATFORM_TYPES_H
#define __FM_PLATFORM_TYPES_H

#include <platforms/common/model/fm_model_message.h>
#include <platforms/common/model/hlp/hlp_model.h>

typedef struct _fm_libCfg
{
    /* log debug level */
    fm_int  logLevel;

    /* Callback for displaying log messages */
    void (*logHandler)(fm_uint64 level, char *log);
} fm_libCfg;


extern void * fmRootChipModel[];


fm_status fmModelLibInit(fm_int sw, fm_libCfg *cfg);
fm_text fmModelGetVersionTag(void);
fm_status fmModelReset(fm_int sw);
void fmModelLibSetLogLevel(fm_int level);
void fmModelLibSetLogCat(char *cat);

#define fmModelReadCSR  hlpModelReadCSR

#define fmModelReadCSR64 hlpModelReadCSR64

#define fmModelReadCSRMult hlpModelReadCSRMult

#define fmModelReadCSRMult64 hlpModelReadCSRMult64

#define fmModelWriteCSRMult64 hlpModelWriteCSRMult64

#define fmModelReceivePacket hlpModelReceivePacket

#define fmModelSendPacket hlpModelSendPacket

#define fmModelTick hlpModelTick

#define fmModelWriteCSR hlpModelWriteCSR

#define fmModelWriteCSRMult hlpModelWriteCSRMult

#define fmModelWriteCSR64 hlpModelWriteCSR64

#define fmModelWriteCSRAbsolute64 hlpModelWriteCSRForce64

#endif /* __FM_PLATFORM_TYPES_H */

