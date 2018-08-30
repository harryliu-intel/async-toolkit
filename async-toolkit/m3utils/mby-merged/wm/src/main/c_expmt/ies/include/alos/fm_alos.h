/* vim:ts=4:sw=4:expandtab
 * (No tabs, indent level is 4 spaces)  */
/*****************************************************************************
 * File:            fm_alos.h
 * Creation Date:   2005
 * Description:     Wrapper file for all of the ALOS headers
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

#ifndef __FM_FM_ALOS_H
#define __FM_FM_ALOS_H


/**********************************************************************
 * This file is the primary include file for the externally-exposed
 * "common" portions of the Fulcrum ControlPoint SDK.
 *
 * Internal dependencies inside this subsystem should be documented here.
 * The only external requirements allowed here are the architectural
 * definitions and the "common" subsystem.
 *
 * As documented in fm_sdk.h, fm_alos_event_queue.h  and fm_alos_threads.h
 * assume that the fm_event typedef has been forward-defined.
 * This is assumed to have been done prior to inclusion of this file.
 *
 * As of 04/17/2007, the following order-dependencies exist amongst the files
 * referenced here:
 *
 *    fm_alos_threads.h depends upon fm_alos_event_queue.h
 *    fm_alos_threads.h depends upon fm_alos_lock.h
 *    fm_alos_threads.h depends upon fm_alos_time.h
 **********************************************************************/

#include <fm_alos_logging.h>
#include <fm_alos_init.h>
#include <fm_alos_time.h>
#include <fm_alos_lock.h>
#include <fm_alos_rwlock.h>
#include <fm_alos_sem.h>
#include <fm_alos_event_queue.h>
#include <fm_alos_threads.h>
#include <fm_alos_alloc.h>
#include <fm_alos_dynamic_load.h>
#include <fm_alos_rand.h>
#include <fm_alos_debughash.h>

#endif /* __FM_FM_ALOS_H */
