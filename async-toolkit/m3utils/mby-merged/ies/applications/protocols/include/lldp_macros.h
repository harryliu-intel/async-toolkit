/* vim:ts=4:sw=4:expandtab
 * (No tabs, indent level is 4 spaces)  */
/*****************************************************************************
 * File:            lldp_macros.h
 * Creation Date:   September 1, 2010
 * Description:     Prototypes and structure definitions for LLDP MACROS.
 *
 * INTEL CONFIDENTIAL
 * Copyright 2010 - 2011 Intel Corporation. All Rights Reserved. 
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

#ifndef LLDP_MACROS_H
#define LLDP_MACROS_H

#define BEGIN_CRITICAL() \
    pthread_mutex_lock(&lldpDB->lock);

#define BEGIN_CRITICAL_PORT_CONFIG(PORTNUM) \
    struct lldpPortConfig *portConfig; \
    pthread_mutex_lock(&lldpDB->lock); \
    if ((portConfig = lldpPortConfigGet(PORTNUM)) == NULL) { \
        pthread_mutex_unlock(&lldpDB->lock); \
        return LLDP_ERR_NOT_FOUND; \
    }

#define BEGIN_CRITICAL_LOCAL_PORT(PORTNUM) \
    struct lldpLocPort *locPort; \
    pthread_mutex_lock(&lldpDB->lock); \
    if ((locPort = lldpLocPortGet(PORTNUM)) == NULL) { \
        pthread_mutex_unlock(&lldpDB->lock); \
        return LLDP_ERR_NOT_FOUND; \
    }

#define END_CRITICAL(PORTNUM) \
    pthread_mutex_unlock(&lldpDB->lock); \
    return lldpHandleLocalChange(PORTNUM);

#define TIMER_A(TIMER) \
    (((TIMER) <= now) ? 0 : ((TIMER) - now))

#define TIMER_S(TIMER) \
    (((TIMER) >= now) ? 0 : (now - (TIMER)))

#endif /* LLDP_MACROS_H */

