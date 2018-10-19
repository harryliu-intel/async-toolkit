/* vim:ts=4:sw=4:expandtab
 * (No tabs, indent level is 4 spaces)  */
/*****************************************************************************
 * File:            dcbx_mib.c
 * Creation Date:   September 1, 2010
 * Description:     Implementation of DCBX database.
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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <pthread.h>

#include <dcbx_mib.h>


/*****************************************************************************
 * Macros, Constants & Types
 *****************************************************************************/


/*****************************************************************************
 * Global Variables
 *****************************************************************************/

struct lldpXdot1dcbxObjects *dcbxDB = NULL;


/*****************************************************************************
 * Local Variables
 *****************************************************************************/


/*****************************************************************************
 * Local function prototypes.
 *****************************************************************************/


/*****************************************************************************
 * Local Functions
 *****************************************************************************/


/*****************************************************************************
 * Public Functions
 *****************************************************************************/

/*****************************************************************************/
/** dcbxObjectsCreate
 * \ingroup dcbx
 *
 * \desc            Creates DCBX database object.
 *
 * \return          Pointer to newly created DCBX database if successful.
 * \return          NULL on failure.
 *
 *****************************************************************************/
struct lldpXdot1dcbxObjects *dcbxObjectsCreate()
{
    struct lldpXdot1dcbxObjects *obj;
    pthread_mutexattr_t attr;

    obj = (struct lldpXdot1dcbxObjects *)calloc(1, sizeof(struct lldpXdot1dcbxObjects));

    lldpListInit(&obj->lldpXdot1dcbxConfigList);
    lldpListInit(&obj->lldpXdot1dcbxLocalDataList);
    lldpListInit(&obj->lldpXdot1dcbxRemoteDataList);
    lldpListInit(&obj->lldpXdot1dcbxAdminDataList);

    /* create DCBX data object lock */
    pthread_mutexattr_init(&attr);
    pthread_mutexattr_settype(&attr, PTHREAD_MUTEX_RECURSIVE);
    pthread_mutex_init(&obj->lock, &attr);
    pthread_mutexattr_destroy(&attr);

    /* reset remote index */
    obj->remoteIndex = 0;

    return obj;
}

/*****************************************************************************/
/** dcbxObjectsDestroy
 * \ingroup dcbx
 *
 * \desc            Destroys DCBX database.
 *
 * \param[in]       obj is the DCBX database object to be destroyed
 *
 * \return          LLDP_OK if successful.
 * \return          LLDP_ERR_UNKNOWN on failure.
 *
 *****************************************************************************/
int dcbxObjectsDestroy(struct lldpXdot1dcbxObjects *obj)
{
    struct lldpXdot1dcbxConfig     *citer, *cnext;
    struct lldpXdot1dcbxLocalData  *liter, *lnext;
    struct lldpXdot1dcbxRemoteData *riter, *rnext;
    struct lldpXdot1dcbxAdminData  *aiter, *anext;

    /* destroy port-config entries */
    LLDP_LIST_FOR_EACH_SAFE(citer, cnext,
                            struct lldpXdot1dcbxConfig, node,
                            &dcbxDB->lldpXdot1dcbxConfigList) {
        dcbxConfigDestroy(citer);
    }

    /* destroy local-port entries */
    LLDP_LIST_FOR_EACH_SAFE(liter, lnext,
                            struct lldpXdot1dcbxLocalData, node,
                            &dcbxDB->lldpXdot1dcbxLocalDataList) {
        dcbxLocalDataDestroy(liter);
    }

    /* destroy remote-port entries */
    LLDP_LIST_FOR_EACH_SAFE(riter, rnext,
                            struct lldpXdot1dcbxRemoteData, node,
                            &dcbxDB->lldpXdot1dcbxRemoteDataList) {
        dcbxRemoteDataDestroy(riter);
    }

    /* destroy admin-port entries */
    LLDP_LIST_FOR_EACH_SAFE(aiter, anext,
                            struct lldpXdot1dcbxAdminData, node,
                            &dcbxDB->lldpXdot1dcbxAdminDataList) {
        dcbxAdminDataDestroy(aiter);
    }

    /* destroy data-object lock */
    pthread_mutex_destroy(&obj->lock);

    free(obj);

    return LLDP_OK;
}


/*****************************************************************************
 * Public Functions (lldpXdot1dcbxConfig)
 *****************************************************************************/

/*****************************************************************************/
/** dcbxConfigCreate
 * \ingroup dcbx
 *
 * \desc            Creates Port Configuration Entry.
 *
 * \param[in]       portNum is the port number for which the object need to be
 *                  created.
 *
 * \return          Pointer to lldpXdot1dcbxConfig object if successful.
 * \return          NULL on failure.
 *
 *****************************************************************************/
struct lldpXdot1dcbxConfig *dcbxConfigCreate(int portNum)
{
    struct lldpXdot1dcbxConfig *obj;

    /* validate that port-number is valid */
    if (portNum < 0 || portNum >= LLDP_MAX_LOCAL_PORTS)
        return NULL;

    /* validate that db is initialized and that local-data is not
       already allocated. */
    if (!dcbxDB || dcbxDB->lldpXdot1dcbxConfig[portNum])
        return NULL;

    /* allocate data struct */
    obj = (struct lldpXdot1dcbxConfig *)calloc(1, sizeof(struct lldpXdot1dcbxConfig));

    /* initialize index */
    obj->lldpV2LocPortIfIndex = portNum;

    /* setup default values */
    obj->lldpXdot1dcbxConfigETSConfiguration.lldpXdot1dcbxConfigETSConfigurationTxEnable = 1;
    obj->lldpXdot1dcbxConfigETSRecommendation.lldpXdot1dcbxConfigETSRecommendationTxEnable = 0;
    obj->lldpXdot1dcbxConfigPFC.lldpXdot1dcbxConfigPFCTxEnable = 1;
    obj->lldpXdot1dcbxConfigApplicationPriority.lldpXdot1dcbxConfigApplicationPriorityTxEnable = 1;
    obj->lldpXdot1dcbxConfigCongestionNotification.lldpXdot1dcbxConfigCongestionNotificationTxEnable = 1;

    /* attach to array */
    dcbxDB->lldpXdot1dcbxConfig[portNum] = obj;

    /* attach to link-list */
    lldpListPush(&dcbxDB->lldpXdot1dcbxConfigList, &obj->node);

    return obj;
}

/*****************************************************************************/
/** dcbxConfigDestroy
 * \ingroup dcbx
 *
 * \desc            Destroys Port Configuration Entry.
 *
 * \param[in]       obj is the port configuration entry to be destroyed.
 *
 * \return          LLDP_OK if successful.
 * \return          LLDP_ERR_UNKNOWN on failure.
 *
 *****************************************************************************/
int dcbxConfigDestroy(struct lldpXdot1dcbxConfig *obj)
{
    int portNum;

    if (!obj)
        return LLDP_ERR_INVALID_ARG;

    portNum = obj->lldpV2LocPortIfIndex;

    /* validate that port-number is valid */
    if (portNum < 0 || portNum >= LLDP_MAX_LOCAL_PORTS)
        return LLDP_ERR_INVALID_ARG;

    /* dettach from link-list */
    lldpListRemove(&obj->node);

    /* dettach from array */
    dcbxDB->lldpXdot1dcbxConfig[portNum] = NULL;

    /* release local data allocation */
    free(obj);

    return LLDP_OK;
}

/*****************************************************************************/
/** dcbxConfigGet
 * \ingroup dcbx
 *
 * \desc            Gets Local Port Entry by port number.
 *
 * \param[in]       portNum is the port number for which the object need to be
 *                  retrieved.
 *
 * \return          Pointer to lldpXdot1dcbxConfig object if successful.
 * \return          NULL on failure.
 *
 *****************************************************************************/
struct lldpXdot1dcbxConfig *dcbxConfigGet(int portNum)
{
    /* validate that port-number is valid */
    if (portNum < 0 || portNum >= LLDP_MAX_LOCAL_PORTS)
        return NULL;

    return dcbxDB->lldpXdot1dcbxConfig[portNum];
}


/*****************************************************************************
 * Public Functions (lldpXdot1dcbxLocalData)
 *****************************************************************************/

/*****************************************************************************/
/** dcbxLocalDataCreate
 * \ingroup dcbx
 *
 * \desc            Creates Local-Port Entry.
 *
 * \param[in]       portNum is the port number for which the object need to be
 *                  created.
 *
 * \return          Pointer to lldpXdot1dcbxLocalData object if successful.
 * \return          NULL on failure.
 *
 *****************************************************************************/
struct lldpXdot1dcbxLocalData *dcbxLocalDataCreate(int portNum)
{
    int i;
    struct lldpXdot1dcbxLocalData *obj;

    /* validate that port-number is valid */
    if (portNum < 0 || portNum >= LLDP_MAX_LOCAL_PORTS)
        return NULL;

    /* validate that db is initialized and that local-data is not
       already allocated. */
    if (!dcbxDB || dcbxDB->lldpXdot1dcbxLocalData[portNum])
        return NULL;

    /* allocate data struct */
    obj = (struct lldpXdot1dcbxLocalData *)calloc(1, sizeof(struct lldpXdot1dcbxLocalData));

    /* initialize index */
    obj->lldpV2LocPortIfIndex = portNum;

    /* setup default Values */

    /* default: use IEEE DCBX as default protocol */
    obj->lldpXdot1dcbxVersion = DCBX_VERSION_IEEE;

    /* default: support credit based shaper */
    obj->lldpXdot1dcbxLocETSConfiguration.
        lldpXdot1dcbxLocETSConCreditBasedShaperSupport = true;

    /* default: support 8 traffic classes */
    obj->lldpXdot1dcbxLocETSConfiguration.
         lldpXdot1dcbxLocETSConTrafficClassesSupported = 8;

    /* default: assign all bandwidth to traffic-class 0 */
    const int defaultBandwidth[8] = { 100, 0, 0, 0, 0, 0, 0, 0 };
    for (i=0; i<8; i++) {
        obj->lldpXdot1dcbxLocETSConfiguration.
             lldpXdot1dcbxLocETSConTrafficClassBandwidthTable[i] = defaultBandwidth[i];

        obj->lldpXdot1dcbxLocETSRecommendation.
             lldpXdot1dcbxLocETSRecoTrafficClassBandwidthTable[i] = defaultBandwidth[i];
    }

    /* default: map 1-1 priority to traffic-class */
    const int defaultPriorityAssignment[8] = { 0, 1, 2, 3, 4, 5, 6, 7 };
    for (i=0; i<8; i++) {
        obj->lldpXdot1dcbxLocETSConfiguration.
             lldpXdot1dcbxLocETSConPriorityAssignmentTable[i] = defaultPriorityAssignment[i];

        obj->lldpXdot1dcbxLocETSRecommendation.
             lldpXdot1dcbxLocETSRecoPriorityAssignmentTable[i] = defaultPriorityAssignment[i];
    }

    /* attach to array */
    dcbxDB->lldpXdot1dcbxLocalData[portNum] = obj;

    /* attach to link-list */
    lldpListPush(&dcbxDB->lldpXdot1dcbxLocalDataList, &obj->node);

    return obj;
}

/*****************************************************************************/
/** dcbxLocalDataDestroy
 * \ingroup dcbx
 *
 * \desc            Destroys Local-Port Entry.
 *
 * \param[in]       obj is the local port entry to be destroyed.
 *
 * \return          LLDP_OK if successful.
 * \return          LLDP_ERR_UNKNOWN on failure.
 *
 *****************************************************************************/
int dcbxLocalDataDestroy(struct lldpXdot1dcbxLocalData *obj)
{
    int portNum;

    if (!obj)
        return LLDP_ERR_INVALID_ARG;

    portNum = obj->lldpV2LocPortIfIndex;

    /* validate that port-number is valid */
    if (portNum < 0 || portNum >= LLDP_MAX_LOCAL_PORTS)
        return LLDP_ERR_INVALID_ARG;

    /* dettach from link-list */
    lldpListRemove(&obj->node);

    /* dettach from array */
    dcbxDB->lldpXdot1dcbxLocalData[portNum] = NULL;

    /* release local data allocation */
    free(obj);

    return LLDP_OK;
}

/*****************************************************************************/
/** dcbxLocalDataGet
 * \ingroup dcbx
 *
 * \desc            Gets Local-Port Entry by port number.
 *
 * \param[in]       portNum is the port number for which the object need to be
 *                  retrieved.
 *
 * \return          Pointer to lldpXdot1dcbxLocalData object if successful.
 * \return          NULL on failure.
 *
 *****************************************************************************/
struct lldpXdot1dcbxLocalData *dcbxLocalDataGet(int portNum)
{
    /* validate that port-number is valid */
    if (portNum < 0 || portNum >= LLDP_MAX_LOCAL_PORTS)
        return NULL;

    return dcbxDB->lldpXdot1dcbxLocalData[portNum];
}


/*****************************************************************************
 * Public Functions (lldpXdot1dcbxRemoteData)
 *****************************************************************************/

/*****************************************************************************/
/** dcbxRemoteDataCreate
 * \ingroup dcbx
 *
 * \desc            Creates Remote-Port Entry.
 *
 * \param[in]       portNum is the port number for which the object need to be
 *                  created.
 *
 * \param[in]       remoteIndex is the LLDP remote entry index.
 *
 * \return          Pointer to lldpXdot1dcbxRemoteData object if successful.
 * \return          NULL on failure.
 *
 *****************************************************************************/
struct lldpXdot1dcbxRemoteData *dcbxRemoteDataCreate(int portNum, int remoteIndex)
{
    struct lldpXdot1dcbxRemoteData *obj;

    /* validate that port-number is valid */
    if (portNum < 0 || portNum >= LLDP_MAX_LOCAL_PORTS)
        return NULL;

    /* validate that db is initialized and that remote-data is not
       already allocated. */
    if (!dcbxDB || dcbxDB->lldpXdot1dcbxRemoteData[portNum])
        return NULL;

    /* allocate data struct */
    obj = (struct lldpXdot1dcbxRemoteData *)calloc(1, sizeof(struct lldpXdot1dcbxRemoteData));

    /* initialize index */
    obj->lldpV2RemLocalIfIndex = portNum;
    obj->lldpV2RemIndex = remoteIndex;

    /* attach to array */
    dcbxDB->lldpXdot1dcbxRemoteData[portNum] = obj;

    /* attach to link-list */
    lldpListPush(&dcbxDB->lldpXdot1dcbxRemoteDataList, &obj->node);

    LLDP_INFO("DCBX: Remote entry created (portNum:%d remoteIndex:%d).\n", portNum, remoteIndex);

    return obj;
}

/*****************************************************************************/
/** dcbxRemoteDataDestroy
 * \ingroup dcbx
 *
 * \desc            Destroys Remote-Port Entry.
 *
 * \param[in]       obj is the remote-port entry to be destroyed.
 *
 * \return          LLDP_OK if successful.
 * \return          LLDP_ERR_UNKNOWN on failure.
 *
 *****************************************************************************/
int dcbxRemoteDataDestroy(struct lldpXdot1dcbxRemoteData *obj)
{
    int remoteIndex;
    int portNum;

    if (!obj)
        return LLDP_ERR_INVALID_ARG;

    remoteIndex = obj->lldpV2RemIndex;
    portNum = obj->lldpV2RemLocalIfIndex;

    /* validate that port-number is valid */
    if (remoteIndex < 0 || remoteIndex >= LLDP_MAX_REMOTE_PORTS)
        return LLDP_ERR_INVALID_ARG;

    /* dettach from link-list */
    lldpListRemove(&obj->node);

    /* dettach from array */
    dcbxDB->lldpXdot1dcbxRemoteData[portNum] = NULL;

    /* release remote data allocation */
    free(obj);

    LLDP_INFO("DCBX: Remote entry destroyed (portNum:%d remoteIndex:%d).\n", portNum, remoteIndex);

    return LLDP_OK;
}

/*****************************************************************************/
/** dcbxRemoteDataGet
 * \ingroup dcbx
 *
 * \desc            Gets Remote-Port Entry by local port number.
 *
 * \param[in]       portNum is the port number for which the object need to be
 *                  retrieved.
 *
 * \return          Pointer to lldpXdot1dcbxRemoteData object if successful.
 * \return          NULL on failure.
 *
 *****************************************************************************/
struct lldpXdot1dcbxRemoteData *dcbxRemoteDataGet(int portNum)
{
    /* validate that port-number is valid */
    if (portNum < 0 || portNum >= LLDP_MAX_LOCAL_PORTS)
        return NULL;

    return dcbxDB->lldpXdot1dcbxRemoteData[portNum];
}


/*****************************************************************************
 * Public Functions (lldpXdot1dcbxAdmin)
 *****************************************************************************/

/*****************************************************************************/
/** dcbxAdminDataCreate
 * \ingroup dcbx
 *
 * \desc            Creates Admin-Port Entry.
 *
 * \param[in]       portNum is the port number for which the object need to be
 *                  created.
 *
 * \return          Pointer to lldpXdot1dcbxAdminData object if successful.
 * \return          NULL on failure.
 *
 *****************************************************************************/
struct lldpXdot1dcbxAdminData *dcbxAdminDataCreate(int portNum)
{
    int i;
    struct lldpXdot1dcbxAdminData *obj;

    /* validate that port-number is valid */
    if (portNum < 0 || portNum >= LLDP_MAX_LOCAL_PORTS)
        return NULL;

    /* validate that db is initialized and that remote-data is not
       already allocated. */
    if (!dcbxDB || dcbxDB->lldpXdot1dcbxAdminData[portNum])
        return NULL;

    /* allocate data struct */
    obj = (struct lldpXdot1dcbxAdminData *)calloc(1, sizeof(struct lldpXdot1dcbxAdminData));

    /* initialize index */
    obj->lldpV2LocPortIfIndex = portNum;

    /* setup default Values */

    /* default: enable version auto-detect */
    obj->lldpXdot1dcbxVersion = DCBX_VERSION_AUTO;

    /* default: support credit based shaper */
    obj->lldpXdot1dcbxAdminETSConfiguration.
        lldpXdot1dcbxAdminETSConCreditBasedShaperSupport = true;

    /* default: support 8 traffic classes */
    obj->lldpXdot1dcbxAdminETSConfiguration.
         lldpXdot1dcbxAdminETSConTrafficClassesSupported = 8;

    /* default: assign all bandwidth to traffic-class 0 */
    const int defaultBandwidth[8] = { 100, 0, 0, 0, 0, 0, 0, 0 };
    for (i=0; i<8; i++) {
        obj->lldpXdot1dcbxAdminETSConfiguration.
             lldpXdot1dcbxAdminETSConTrafficClassBandwidthTable[i] = defaultBandwidth[i];

        obj->lldpXdot1dcbxAdminETSRecommendation.
             lldpXdot1dcbxAdminETSRecoTrafficClassBandwidthTable[i] = defaultBandwidth[i];
    }

    /* default: map 1-1 priority to traffic-class */
    const int defaultPriorityAssignment[8] = { 0, 1, 2, 3, 4, 5, 6, 7 };
    for (i=0; i<8; i++) {
        obj->lldpXdot1dcbxAdminETSConfiguration.
             lldpXdot1dcbxAdminETSConPriorityAssignmentTable[i] = defaultPriorityAssignment[i];

        obj->lldpXdot1dcbxAdminETSRecommendation.
             lldpXdot1dcbxAdminETSRecoPriorityAssignmentTable[i] = defaultPriorityAssignment[i];
    }

    /* attach to array */
    dcbxDB->lldpXdot1dcbxAdminData[portNum] = obj;

    /* attach to link-list */
    lldpListPush(&dcbxDB->lldpXdot1dcbxAdminDataList, &obj->node);

    return obj;
}

/*****************************************************************************/
/** dcbxAdminDataDestroy
 * \ingroup dcbx
 *
 * \desc            Destroys Admin-Port Entry.
 *
 * \param[in]       obj is the admin port entry to be destroyed.
 *
 * \return          LLDP_OK if successful.
 * \return          LLDP_ERR_UNKNOWN on failure.
 *
 *****************************************************************************/
int dcbxAdminDataDestroy(struct lldpXdot1dcbxAdminData *obj)
{
    int portNum;

    if (!obj)
        return LLDP_ERR_INVALID_ARG;

    portNum = obj->lldpV2LocPortIfIndex;

    /* validate that port-number is valid */
    if (portNum < 0 || portNum >= LLDP_MAX_LOCAL_PORTS)
        return LLDP_ERR_INVALID_ARG;

    /* dettach from link-list */
    lldpListRemove(&obj->node);

    /* dettach from array */
    dcbxDB->lldpXdot1dcbxAdminData[portNum] = NULL;

    /* release remote data allocation */
    free(obj);

    return LLDP_OK;
}

/*****************************************************************************/
/** dcbxAdminDataGet
 * \ingroup dcbx
 *
 * \desc            Gets Admin-Port Entry by port number.
 *
 * \param[in]       portNum is the port number for which the object need to be
 *                  retrieved.
 *
 * \return          Pointer to lldpXdot1dcbxAdminData object if successful.
 * \return          NULL on failure.
 *
 *****************************************************************************/
struct lldpXdot1dcbxAdminData *dcbxAdminDataGet(int portNum)
{
    /* validate that port-number is valid */
    if (portNum < 0 || portNum >= LLDP_MAX_LOCAL_PORTS)
        return NULL;

    return dcbxDB->lldpXdot1dcbxAdminData[portNum];
}

