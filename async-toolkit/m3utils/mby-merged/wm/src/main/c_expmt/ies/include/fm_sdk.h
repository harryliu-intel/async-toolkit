/* vim:ts=4:sw=4:expandtab
 * (No tabs, indent level is 4 spaces)  */
/*****************************************************************************
 * File:            fm_sdk.h
 * Creation Date:   2005
 * Description:     Wrapper to include all relevant files needed by user code
 *
 * INTEL CONFIDENTIAL
 * Copyright 2005 - 2011 Intel Corporation. All Rights Reserved. 
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

#ifndef __FM_FM_SDK_H
#define __FM_FM_SDK_H

#define FM_SDK_VERSION  3

/**********************************************************************
 * This file is the primary include file for the externally-exposed
 * portions of the Fulcrum ControlPoint SDK.
 *
 * Any order-dependencies among the top-level subsystems must be clearly
 * documented and handled here, not buried inside subordinate include files.
 * Internal dependencies inside a particular subsystem should be documented
 * in that sub-systems top-level include file.
 **********************************************************************/

/*
 * Include wrapper to OS & Standard C include files
 *
 * No dependencies upon Fulcrum ControlPoint SDK definitions
 *
 * No known order dependencies
 *
 */

#include <fm_alos_sys.h>        

/*
 * architecture specific header for type defs
 *
 * has no dependencies
 *
 * All other include files are permitted to assume that all
 * architecture definitions are available.
 */
#include "fm_std.h"

/*
 * Include the common subsystem.
 *
 * May only rely upon availability of architecture files
 */
#include <common/fm_common.h>

/*
 * customer-configurable constants.
 *
 * May only rely upon availability of architecture and common files.
 *
 * all ALOS, API and customer include files are permitted to assume that all
 * constants defined in this file are available.
 *
 */
#include <platform_defines.h>



/*
 * Default some system constants not specified by the
 * platform layer
 *
 */

/* Enable ALOS lock inversion defense by default */
#ifndef FM_LOCK_INVERSION_DEFENSE
#define FM_LOCK_INVERSION_DEFENSE       FM_ENABLED
#endif


/*
 * Include the ALOS subsystem.
 *
 * May only rely upon availability of architecture and common files,
 * except as explicitly documented here:
 *
 * alos/fm_alos_event_queue references fm_event typedef (pointer
 * references only).  fm_event is pretty tightly bound into
 * api/fm_api_events.h.  This reference is explicitly permitted here
 * by forward-defining the typedef at this point.
 */
typedef struct _fm_event    fm_event;
#include <fm_alos.h>

/*
 * Include the API subsystem.
 *
 * may rely upon architecture, common, and alos subsystems
 *
 */
#include <api/fm_api.h>

/*
 * Include the debug subsystem
 *
 * may rely upon architecture, common, alos, api, and drv subsystems
 *
 */
#include <debug/fm_debug.h>


#endif /* __FM_FM_SDK_H */
