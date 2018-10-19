/* vim:ts=4:sw=4:expandtab
 * (No tabs, indent level is 4 spaces)  */
/*****************************************************************************
 * File:            dcbx_macros.h
 * Creation Date:   September 1, 2010
 * Description:     Prototypes and structure definitions for DCBX MACROS.
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

#ifndef DCBX_MACROS_H
#define DCBX_MACROS_H

#define BEGIN_CRITICAL() \
    pthread_mutex_lock(&dcbxDB->lock);

#define BEGIN_CRITICAL_PORT_CONFIG_RET(PORTNUM,RETVAL) \
    struct lldpXdot1dcbxConfig *config; \
    bool changed __attribute__((__unused__)) = false; \
    pthread_mutex_lock(&dcbxDB->lock); \
    if ((config = dcbxConfigGet(PORTNUM)) == NULL) { \
        pthread_mutex_unlock(&dcbxDB->lock); \
        return RETVAL; \
    }

#define BEGIN_CRITICAL_PORT_CONFIG(PORTNUM) \
    BEGIN_CRITICAL_PORT_CONFIG_RET(PORTNUM,LLDP_ERR_NOT_FOUND)

#define BEGIN_CRITICAL_REMOTE_PORT_RET(PORTNUM,RETVAL) \
    struct lldpXdot1dcbxRemoteData *remote; \
    bool changed __attribute__((__unused__)) = false; \
    pthread_mutex_lock(&dcbxDB->lock); \
    if ((remote = dcbxRemoteDataGet(PORTNUM)) == NULL) { \
        pthread_mutex_unlock(&dcbxDB->lock); \
        return RETVAL; \
    }

#define BEGIN_CRITICAL_REMOTE_PORT(PORTNUM) \
    BEGIN_CRITICAL_REMOTE_PORT_RET(PORTNUM,LLDP_ERR_NOT_FOUND)

#define BEGIN_CRITICAL_LOCAL_PORT_RET(PORTNUM,RETVAL) \
    struct lldpXdot1dcbxLocalData *local; \
    bool changed __attribute__((__unused__)) = false; \
    pthread_mutex_lock(&dcbxDB->lock); \
    if ((local = dcbxLocalDataGet(PORTNUM)) == NULL) { \
        pthread_mutex_unlock(&dcbxDB->lock); \
        return RETVAL; \
    }

#define BEGIN_CRITICAL_LOCAL_PORT(PORTNUM) \
    BEGIN_CRITICAL_LOCAL_PORT_RET(PORTNUM,LLDP_ERR_NOT_FOUND)

#define BEGIN_CRITICAL_ADMIN_PORT_RET(PORTNUM,RETVAL) \
    struct lldpXdot1dcbxAdminData *admin; \
    struct lldpXdot1dcbxLocalData *local; \
    bool changed __attribute__((__unused__)) = false; \
    pthread_mutex_lock(&dcbxDB->lock); \
    if ((admin = dcbxAdminDataGet(PORTNUM)) == NULL) { \
        pthread_mutex_unlock(&dcbxDB->lock); \
        return RETVAL; \
    } \
    if ((local = dcbxLocalDataGet(PORTNUM)) == NULL) { \
        pthread_mutex_unlock(&dcbxDB->lock); \
        return RETVAL; \
    }

#define BEGIN_CRITICAL_ADMIN_PORT(PORTNUM) \
    BEGIN_CRITICAL_ADMIN_PORT_RET(PORTNUM,LLDP_ERR_NOT_FOUND)

#define END_CRITICAL(PORTNUM) \
    pthread_mutex_unlock(&dcbxDB->lock); \
    return changed ? lldpHandleLocalChange(PORTNUM) : LLDP_OK;

#define END_CRITICAL_RET(PORTNUM,RETVAL) \
    pthread_mutex_unlock(&dcbxDB->lock); \
    return RETVAL;

#define TIMER_A(TIMER) \
    (((TIMER) <= now) ? 0 : ((TIMER) - now))

#define TIMER_S(TIMER) \
    (((TIMER) >= now) ? 0 : (now - (TIMER)))

#endif /* DCBX_MACROS_H */

