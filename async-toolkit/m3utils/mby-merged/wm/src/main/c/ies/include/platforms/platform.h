/* vim:ts=4:sw=4:expandtab
 * (No tabs, indent level is 4 spaces)  */
/*****************************************************************************
 * File:            platform.h
 * Creation Date:   2005
 * Description:     Platform definitions.
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

#ifndef __FM_PLATFORM_H
#define __FM_PLATFORM_H

/* This is the platform-specific header file */
#include <platform_types.h>

/* Default macro definitions for older platforms. */
#include <platforms/platform_defaults.h>

/* Platform layer API services. */
#include <platforms/platform_api.h>

/* Platform layer application services. */
#include <platforms/platform_app.h>

/**************************************************
 * Internal functions
 **************************************************/

/* Interrupt Management */
fm_status fmPlatformTriggerInterrupt(fm_int sw, fm_uint intrTypes);

/* Port and PHY Management */
fm_status fmPlatformSetPortInterruptMask(fm_int switchNum, fm_int port);
fm_status fmPlatformClearPortInterruptMask(fm_int switchNum, fm_int port);
fm_status fmPlatformGetPortInterruptMask(fm_int switchNum, fm_uint32 *portMask);
fm_status fmInitializePhysicalInterfaces(fm_int switchNum, fm_switch *pSwitch);

/* Packet Transfer Functions */
fm_status fmPlatformReceiveProcess(fm_int     sw,
                                   fm_buffer *buffer,
                                   fm_uint32 *pIslTag,
                                   fm_uint    flags);

/* Raw CSR access */
fm_status fmPlatformReadRawCSR(fm_int sw, fm_uint32 addr, fm_uint32 *value);
fm_status fmPlatformWriteRawCSR(fm_int sw, fm_uint32 addr, fm_uint32 value);

#endif	/* __FM_PLATFORM_H */
