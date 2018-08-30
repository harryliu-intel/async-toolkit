/* vim:ts=4:sw=4:expandtab
 * (No tabs, indent level is 4 spaces)  */
/*****************************************************************************
 * File:            platform_defaults.h
 * Creation Date:   January 18, 2003.
 * Description:     Default platform macro definitions.
 *
 * INTEL CONFIDENTIAL
 * Copyright 2012 - 2013 Intel Corporation. All Rights Reserved. 
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

#ifndef __FM_PLATFORM_DEFAULTS_H
#define __FM_PLATFORM_DEFAULTS_H

/*****************************************************************************
 * These definitions are for backward compatibility with older platforms.
 * For newer platforms, they are defined in the platform-specific header file.
 *****************************************************************************/

#ifndef IS_PLAT_STATE_INITED
#define IS_PLAT_STATE_INITED (fmRootPlatform->fmPlatformState)
#endif

#ifndef GET_PLAT_STATE
#define GET_PLAT_STATE(sw) (&fmRootPlatform->fmPlatformState[sw])
#endif

#ifndef GET_PLAT_PKT_STATE
#define GET_PLAT_PKT_STATE(sw) (&fmRootPlatform->fmPlatformState[sw].packetState)
#endif

#endif	/* __FM_PLATFORM_DEFAULTS_H */

