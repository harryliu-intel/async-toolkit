/* vim:ts=4:sw=4:expandtab
 * (No tabs, indent level is 4 spaces)  */
/*****************************************************************************
 * File:            lldp_mib.c
 * Creation Date:   September 1, 2010
 * Description:     Implementation of LLDP database.
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

#include <lldp_mib.h>


/*****************************************************************************
 * Macros, Constants & Types
 *****************************************************************************/


/*****************************************************************************
 * Global Variables
 *****************************************************************************/

struct lldpObjects *lldpDB = NULL;


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
/** lldpObjectsCreate
 * \ingroup lldp
 *
 * \desc            Creates LLDP database object.
 *
 * \return          Pointer to newly created LLDP database if successful.
 * \return          NULL on failure.
 *
 *****************************************************************************/
struct lldpObjects *lldpObjectsCreate()
{
    struct lldpObjects *obj;
    pthread_mutexattr_t attr;

    obj = (struct lldpObjects *)calloc(1, sizeof(struct lldpObjects));

    lldpListInit(&obj->lldpConfiguration.lldpPortConfigTable);
    lldpListInit(&obj->lldpStatistics.lldpStatsPortTable);
    lldpListInit(&obj->lldpLocalSystemData.lldpLocPortTable);
    lldpListInit(&obj->lldpLocalSystemData.lldpLocManAddrTable);
    lldpListInit(&obj->lldpRemoteSystemsData.lldpRemTable);

    /* create event queue */
    lldpEventQueueInit(&obj->queue);

    /* create lldp data object lock */
    pthread_mutexattr_init(&attr);
    pthread_mutexattr_settype(&attr, PTHREAD_MUTEX_RECURSIVE);
    pthread_mutex_init(&obj->lock, &attr);
    pthread_mutexattr_destroy(&attr);

    /* reset remote index */
    obj->remoteIndex = 0;

    /* setup default values */
    obj->lldpConfiguration.lldpMessageTxInterval = 30;
    obj->lldpConfiguration.lldpMessageTxHoldMultiplier = 4;
    obj->lldpConfiguration.lldpReinitDelay = 2;
    obj->lldpConfiguration.lldpTxDelay = 2;
    obj->lldpConfiguration.lldpNotificationInterval = 5;

	obj->lldpStatistics.lldpStatsRemTablesLastChangeTime = (uint32_t)time(NULL);

    return obj;
}

/*****************************************************************************/
/** lldpObjectsDestroy
 * \ingroup lldp
 *
 * \desc            Destroys LLDP database.
 *
 * \param[in]       obj is the LLDP database object to be destroyed
 *
 * \return          LLDP_OK if successful.
 * \return          LLDP_ERR_UNKNOWN on failure.
 *
 *****************************************************************************/
int lldpObjectsDestroy(struct lldpObjects *obj)
{
    struct lldpPortConfig *citer, *cnext;
    struct lldpStatsPort  *siter, *snext;
    struct lldpLocPort    *liter, *lnext;
    struct lldpLocManAddr *miter, *mnext;
    struct lldpRem        *riter, *rnext;

    /* destroy port-config entries */
    LLDP_LIST_FOR_EACH_SAFE(citer, cnext,
                            struct lldpPortConfig, node,
                            &lldpDB->lldpConfiguration.lldpPortConfigTable) {
        lldpPortConfigDestroy(citer);
    }

    /* destroy port-stats entries */
    LLDP_LIST_FOR_EACH_SAFE(siter, snext,
                            struct lldpStatsPort, node,
                            &lldpDB->lldpStatistics.lldpStatsPortTable) {
        lldpStatsPortDestroy(siter);
    }

    /* destroy local-port entries */
    LLDP_LIST_FOR_EACH_SAFE(liter, lnext,
                            struct lldpLocPort, node,
                            &lldpDB->lldpLocalSystemData.lldpLocPortTable) {
        lldpLocPortDestroy(liter);
    }

    /* destroy local-mgmt entries */
    LLDP_LIST_FOR_EACH_SAFE(miter, mnext,
                            struct lldpLocManAddr, node,
                            &lldpDB->lldpLocalSystemData.lldpLocManAddrTable) {
        lldpLocManAddrDestroy(miter);
    }

    /* destroy remote-port entries */
    LLDP_LIST_FOR_EACH_SAFE(riter, rnext,
                            struct lldpRem, node,
                            &lldpDB->lldpRemoteSystemsData.lldpRemTable) {
        lldpRemDestroy(riter);
    }

    /* destroy data-object lock */
    pthread_mutex_destroy(&obj->lock);

    lldpEventQueueDestroy(&obj->queue);

    free(obj);

    return LLDP_OK;
}


/*****************************************************************************
 * Public Functions (lldpPortConfig)
 *****************************************************************************/

/*****************************************************************************/
/** lldpPortConfigCreate
 * \ingroup lldp
 *
 * \desc            Creates Port Configuration Entry.
 *
 * \param[in]       portNum is the port number for which the object need to be
 *                  created.
 *
 * \return          Pointer to lldpPortConfig object if successful.
 * \return          NULL on failure.
 *
 *****************************************************************************/
struct lldpPortConfig *lldpPortConfigCreate(int portNum)
{
    struct lldpPortConfig *obj;

    /* validate that port-number is valid */
    if (portNum < 0 || portNum >= LLDP_MAX_LOCAL_PORTS)
        return NULL;

    /* validate that db is initialized and that local-data is not
       already allocated. */
    if (!lldpDB || lldpDB->lldpConfiguration.lldpPortConfig[portNum])
        return NULL;

    /* allocate data struct */
    obj = (struct lldpPortConfig *)calloc(1, sizeof(struct lldpPortConfig));

    /* initialize index */
    obj->lldpPortConfigPortNum = portNum;

    /* setup default values */
    obj->lldpPortConfigAdminStatus = LLDP_ADMIN_TX_AND_RX;
    obj->lldpPortConfigNotificationEnable = true;
    obj->lldpPortConfigTLVsTxEnable = LLDP_TLV_ENABLE_ALL;
    obj->lldpPortConfigEnabled = true;
    obj->lldpConfigManAddrPortsTxEnable = true;

    /* attach to array */
    lldpDB->lldpConfiguration.lldpPortConfig[portNum] = obj;

    /* attach to link-list */
    lldpListPush(&lldpDB->lldpConfiguration.lldpPortConfigTable, &obj->node);

    return obj;
}

/*****************************************************************************/
/** lldpPortConfigDestroy
 * \ingroup lldp
 *
 * \desc            Destroys Port Configuration Entry.
 *
 * \param[in]       obj is the port configuration entry to be destroyed.
 *
 * \return          LLDP_OK if successful.
 * \return          LLDP_ERR_UNKNOWN on failure.
 *
 *****************************************************************************/
int lldpPortConfigDestroy(struct lldpPortConfig *obj)
{
    int portNum;

    if (!obj)
        return LLDP_ERR_INVALID_ARG;

    portNum = obj->lldpPortConfigPortNum;

    /* validate that port-number is valid */
    if (portNum < 0 || portNum >= LLDP_MAX_LOCAL_PORTS)
        return LLDP_ERR_INVALID_ARG;

    /* detach from link-list */
    lldpListRemove(&obj->node);

    /* detach from array */
    lldpDB->lldpConfiguration.lldpPortConfig[portNum] = NULL;

    /* release local data allocation */
    free(obj);

    return LLDP_OK;
}

/*****************************************************************************/
/** lldpPortConfigGet
 * \ingroup lldp
 *
 * \desc            Gets Local Port Entry by port number.
 *
 * \param[in]       portNum is the port number for which the object need to be
 *                  retrieved.
 *
 * \return          Pointer to lldpPortConfig object if successful.
 * \return          NULL on failure.
 *
 *****************************************************************************/
struct lldpPortConfig *lldpPortConfigGet(int portNum)
{
    /* validate that port-number is valid */
    if (portNum < 0 || portNum >= LLDP_MAX_LOCAL_PORTS)
        return NULL;

    return lldpDB->lldpConfiguration.lldpPortConfig[portNum];
}


/*****************************************************************************
 * Public Functions (lldpLocPort)
 *****************************************************************************/

/*****************************************************************************/
/** lldpLocPortCreate
 * \ingroup lldp
 *
 * \desc            Creates Local-Port Entry.
 *
 * \param[in]       portNum is the port number for which the object need to be
 *                  created.
 *
 * \param[in]       portIdSubtype is the port-id subtype (one of 
 *                  lldpPortIdSubtype enum).
 *
 * \param[in]       portId is a pointer to the port-id buffer/string.
 *
 * \param[in]       portIdLen is the port-id length.
 *
 * \param[in]       portDesc is a pointer to the port-description string.
 *
 * \return          Pointer to lldpLocPort object if successful.
 * \return          NULL on failure.
 *
 *****************************************************************************/
struct lldpLocPort *lldpLocPortCreate(int portNum, enum lldpPortIdSubtype portIdSubtype, const char *portId, size_t portIdLen, const char *portDesc)
{
    struct lldpLocPort *obj;

    /* validate that port-number is valid */
    if (portNum < 0 || portNum >= LLDP_MAX_LOCAL_PORTS)
        return NULL;

    /* validate that db is initialized and that local-data is not
       already allocated. */
    if (!lldpDB || lldpDB->lldpLocalSystemData.lldpLocPort[portNum])
        return NULL;

    /* allocate data struct */
    obj = (struct lldpLocPort *)calloc(1, sizeof(struct lldpLocPort));

    /* initialize index */
    obj->lldpLocPortNum = portNum;

    /* setup default values */
    obj->lldpLocPortIdSubtype = portIdSubtype;
    obj->lldpLocPortIdLen = portIdLen;
    memcpy(obj->lldpLocPortId, portId, portIdLen);
    obj->lldpLocPortId[obj->lldpLocPortIdLen] = 0;

    lldp_strcpy(obj->lldpLocPortDesc, portDesc);

    obj->rxState = LLDP_RX_STATE_WAIT_FOR_PORT_OPERATIONAL;
    obj->rxStateLast = LLDP_RX_STATE_UNKNOWN;
    obj->rxInfoAged = false;
    obj->rcvFrame = false;
    obj->badFrame = false;

    obj->txState = LLDP_TX_STATE_TX_LLDP_INITIALIZE;
    obj->txStateLast = LLDP_TX_STATE_UNKNOWN;
    obj->txTTL = 0;
    obj->txTTR = 0;
    obj->txDelayWhile = 0;
    obj->txShutdownWhile = 0;

    obj->somethingChangedRemote = false;
    obj->somethingChangedLocal = true;

    obj->portNeighbors = 0;
    obj->mgmtNeighbors = 0;

    obj->tooManyNeighbors = false;
    obj->tooManyNeighborsTimer = 0;

    /* attach to array */
    lldpDB->lldpLocalSystemData.lldpLocPort[portNum] = obj;

    /* attach to link-list */
    lldpListPush(&lldpDB->lldpLocalSystemData.lldpLocPortTable, &obj->node);

    return obj;
}

/*****************************************************************************/
/** lldpLocPortDestroy
 * \ingroup lldp
 *
 * \desc            Destroys Local-Port Entry.
 *
 * \param[in]       obj is the local port entry to be destroyed.
 *
 * \return          LLDP_OK if successful.
 * \return          LLDP_ERR_UNKNOWN on failure.
 *
 *****************************************************************************/
int lldpLocPortDestroy(struct lldpLocPort *obj)
{
    int portNum;

    if (!obj)
        return LLDP_ERR_INVALID_ARG;

    portNum = obj->lldpLocPortNum;

    /* validate that port-number is valid */
    if (portNum < 0 || portNum >= LLDP_MAX_LOCAL_PORTS)
        return LLDP_ERR_INVALID_ARG;

    /* detach from link-list */
    lldpListRemove(&obj->node);

    /* detach from array */
    lldpDB->lldpLocalSystemData.lldpLocPort[portNum] = NULL;

    /* release local data allocation */
    free(obj);

    return LLDP_OK;
}

/*****************************************************************************/
/** lldpLocPortGet
 * \ingroup lldp
 *
 * \desc            Gets Local-Port Entry by port number.
 *
 * \param[in]       portNum is the port number for which the object need to be
 *                  retrieved.
 *
 * \return          Pointer to lldpLocPort object if successful.
 * \return          NULL on failure.
 *
 *****************************************************************************/
struct lldpLocPort *lldpLocPortGet(int portNum)
{
    /* validate that port-number is valid */
    if (portNum < 0 || portNum >= LLDP_MAX_LOCAL_PORTS)
        return NULL;

    return lldpDB->lldpLocalSystemData.lldpLocPort[portNum];
}


/*****************************************************************************
 * Public Functions (lldpLocManAddr)
 *****************************************************************************/

/*****************************************************************************/
/** lldpLocManAddrCreate
 * \ingroup lldp
 *
 * \desc            Creates Local-Port Management-Address Entry.
 *
 * \param[in]       addrSubtype is the addr-id subtype.
 *
 * \param[in]       addr is a pointer to the managment address string.
 *
 * \return          Pointer to lldpLocManAddr object if successful.
 * \return          NULL on failure.
 *
 *****************************************************************************/
struct lldpLocManAddr *lldpLocManAddrCreate(uint32_t addrSubtype, const char *addr)
{
    struct lldpLocManAddr *obj;

    /* validate that db is initialized and that remote-data is not
       already allocated. */
    if (!lldpDB || lldpLocManAddrGet(addr))
        return NULL;

    /* allocate data struct */
    obj = (struct lldpLocManAddr *)calloc(1, sizeof(struct lldpLocManAddr));

    /* initialize index */
    obj->lldpLocManAddrSubtype = addrSubtype;
    memcpy(obj->lldpLocManAddr, addr, 32);

    /* attach to link-list */
    lldpListPush(&lldpDB->lldpLocalSystemData.lldpLocManAddrTable, &obj->node);

    return obj;
}

/*****************************************************************************/
/** lldpLocManAddrDestroy
 * \ingroup lldp
 *
 * \desc            Destroys Local-Port Management-Address Entry.
 *
 * \param[in]       obj is the local managment-address entry to be destroyed.
 *
 * \return          LLDP_OK if successful.
 * \return          LLDP_ERR_UNKNOWN on failure.
 *
 *****************************************************************************/
int lldpLocManAddrDestroy(struct lldpLocManAddr *obj)
{
    if (!obj)
        return LLDP_ERR_INVALID_ARG;

    /* detach from link-list */
    lldpListRemove(&obj->node);

    /* release remote data allocation */
    free(obj);

    return LLDP_OK;
}

/*****************************************************************************/
/** lldpLocManAddrGet
 * \ingroup lldp
 *
 * \desc            Gets Local Port Management-Address Entry by address.
 *
 * \param[in]       addr is the address for which the object need to be
 *                  retrieved.
 *
 * \return          Pointer to lldpLocManAddr object if successful.
 * \return          NULL on failure.
 *
 *****************************************************************************/
struct lldpLocManAddr *lldpLocManAddrGet(const char *addr)
{
    struct lldpLocManAddr *iter, *next;

    LLDP_LIST_FOR_EACH_SAFE(iter, next, 
                            struct lldpLocManAddr, node,
                            &lldpDB->lldpLocalSystemData.lldpLocManAddrTable) {
        if (!strcmp(iter->lldpLocManAddr, addr))
            return iter;
    }

    return NULL;
}


/*****************************************************************************
 * Public Functions (lldpStatsPort)
 *****************************************************************************/

/*****************************************************************************/
/** lldpStatsPortCreate
 * \ingroup lldp
 *
 * \desc            Creates Port-Statistics Entry.
 *
 * \param[in]       portNum is the port number for which the object need to be
 *                  created.
 *
 * \return          Pointer to lldpStatsPort object if successful.
 * \return          NULL on failure.
 *
 *****************************************************************************/
struct lldpStatsPort *lldpStatsPortCreate(int portNum)
{
    struct lldpStatsPort *obj;

    /* validate that port-number is valid */
    if (portNum < 0 || portNum >= LLDP_MAX_LOCAL_PORTS)
        return NULL;

    /* validate that db is initialized and that local-data is not
       already allocated. */
    if (!lldpDB || lldpDB->lldpStatistics.lldpStatsPort[portNum])
        return NULL;

    /* allocate data struct */
    obj = (struct lldpStatsPort *)calloc(1, sizeof(struct lldpStatsPort));

    /* initialize index */
    obj->lldpStatsPortNum = portNum;

    /* attach to array */
    lldpDB->lldpStatistics.lldpStatsPort[portNum] = obj;

    /* attach to link-list */
    lldpListPush(&lldpDB->lldpStatistics.lldpStatsPortTable, &obj->node);

    return obj;
}

/*****************************************************************************/
/** lldpStatsPortDestroy
 * \ingroup lldp
 *
 * \desc            Destroys Port-Statistics Entry.
 *
 * \param[in]       obj is the port-statistics entry to be destroyed.
 *
 * \return          LLDP_OK if successful.
 * \return          LLDP_ERR_UNKNOWN on failure.
 *
 *****************************************************************************/
int lldpStatsPortDestroy(struct lldpStatsPort *obj)
{
    int portNum;

    if (!obj)
        return LLDP_ERR_INVALID_ARG;

    portNum = obj->lldpStatsPortNum;

    /* validate that port-number is valid */
    if (portNum < 0 || portNum >= LLDP_MAX_LOCAL_PORTS)
        return LLDP_ERR_INVALID_ARG;

    /* detach from link-list */
    lldpListRemove(&obj->node);

    /* detach from array */
    lldpDB->lldpStatistics.lldpStatsPort[portNum] = NULL;

    /* release local data allocation */
    free(obj);

    return LLDP_OK;
}

/*****************************************************************************/
/** lldpStatsPortGet
 * \ingroup lldp
 *
 * \desc            Gets Port-Statistics Entry by port number.
 *
 * \param[in]       portNum is the port number for which the object need to be
 *                  retrieved.
 *
 * \return          Pointer to lldpStatsPort object if successful.
 * \return          NULL on failure.
 *
 *****************************************************************************/
struct lldpStatsPort *lldpStatsPortGet(int portNum)
{
    /* validate that port-number is valid */
    if (portNum < 0 || portNum >= LLDP_MAX_LOCAL_PORTS)
        return NULL;

    return lldpDB->lldpStatistics.lldpStatsPort[portNum];
}


/*****************************************************************************
 * Public Functions (lldpRem)
 *****************************************************************************/

/*****************************************************************************/
/** lldpRemCreate
 * \ingroup lldp
 *
 * \desc            Creates Remote-Port Entry.
 *
 * \param[in]       portNum is the port number for which the object need to be
 *                  created.
 *
 * \param[in]       msap is the remote entry MSAP identifier.
 *
 * \return          Pointer to lldpRem object if successful.
 * \return          NULL on failure.
 *
 *****************************************************************************/
struct lldpRem *lldpRemCreate(int portNum, const struct lldpMsap *msap)
{
    int i;
    struct lldpRem *obj;

    /* validate that port-number is valid */
    if (portNum < 0 || portNum >= LLDP_MAX_LOCAL_PORTS)
        return NULL;

    /* validate that db is initialized and that remote-data is not
       already allocated. */
    if (!lldpDB || lldpRemGetByMsap(msap))
        return NULL;

    /* allocate remote-index */
    for (i=0; i<LLDP_MAX_REMOTE_PORTS && 
              lldpDB->lldpRemoteSystemsData.lldpRem[lldpDB->remoteIndex]; i++)
        lldpDB->remoteIndex = (lldpDB->remoteIndex + 1) % LLDP_MAX_REMOTE_PORTS;
    if (lldpDB->lldpRemoteSystemsData.lldpRem[lldpDB->remoteIndex])
        return NULL;

    /* allocate data struct */
    obj = (struct lldpRem *)calloc(1, sizeof(struct lldpRem));

    /* initialize index */
    obj->lldpRemLocalPortNum = portNum;
    obj->lldpRemIndex = lldpDB->remoteIndex;

    /* initialize internal link-lists */
    lldpListInit(&obj->lldpRemManAddrTable);

    /* setup default values */
    obj->lldpRemTimeMark = (uint32_t)time(NULL);

    obj->lldpRemChassisIdSubtype = msap->lldpMsapChassisIdSubtype;
    obj->lldpRemChassisIdLen = msap->lldpMsapChassisIdLen;
    memcpy(obj->lldpRemChassisId, msap->lldpMsapChassisId, obj->lldpRemChassisIdLen);
    obj->lldpRemChassisId[obj->lldpRemChassisIdLen] = '\0';

    obj->lldpRemPortIdSubtype = msap->lldpMsapPortIdSubtype;
    obj->lldpRemPortIdLen = msap->lldpMsapPortIdLen;
    memcpy(obj->lldpRemPortId, msap->lldpMsapPortId, obj->lldpRemPortIdLen);
    obj->lldpRemPortId[obj->lldpRemPortIdLen] = '\0';

    obj->lldpRemSysCapSupported = 0;
    obj->lldpRemSysCapEnabled = 0;
    obj->rxInfoTTL = 0;

    /* attach to array */
    lldpDB->lldpRemoteSystemsData.lldpRem[lldpDB->remoteIndex] = obj;

    /* attach to link-list */
    lldpListPush(&lldpDB->lldpRemoteSystemsData.lldpRemTable, &obj->node);

    return obj;
}

/*****************************************************************************/
/** lldpRemDestroy
 * \ingroup lldp
 *
 * \desc            Destroys Remote-Port Entry.
 *
 * \param[in]       obj is the remote-port entry to be destroyed.
 *
 * \return          LLDP_OK if successful.
 * \return          LLDP_ERR_UNKNOWN on failure.
 *
 *****************************************************************************/
int lldpRemDestroy(struct lldpRem *obj)
{
    int remoteIndex;
    struct lldpRemManAddr *miter, *mnext;

    if (!obj)
        return LLDP_ERR_INVALID_ARG;

    remoteIndex = obj->lldpRemIndex;

    /* validate that port-number is valid */
    if (remoteIndex < 0 || remoteIndex >= LLDP_MAX_REMOTE_PORTS)
        return LLDP_ERR_INVALID_ARG;

    /* destroy local-mgmt entries */
    LLDP_LIST_FOR_EACH_SAFE(miter, mnext,
                            struct lldpRemManAddr, node,
                            &obj->lldpRemManAddrTable) {
        lldpRemManAddrDestroy(miter);
    }

    /* detach from link-list */
    lldpListRemove(&obj->node);

    /* detach from array */
    lldpDB->lldpRemoteSystemsData.lldpRem[remoteIndex] = NULL;

    /* release remote data allocation */
    free(obj);

    return LLDP_OK;
}

/*****************************************************************************/
/** lldpRemGetByIndex
 * \ingroup lldp
 *
 * \desc            Gets Remote-Port Entry by remote index.
 *
 * \param[in]       remoteIndex is the remote-index for which the object need
 *                  to be retrieved.
 *
 * \return          Pointer to lldpRem object if successful.
 * \return          NULL on failure.
 *
 *****************************************************************************/
struct lldpRem *lldpRemGetByIndex(int remoteIndex)
{
    /* validate that port-number is valid */
    if (remoteIndex < 0 || remoteIndex >= LLDP_MAX_REMOTE_PORTS)
        return NULL;

    return lldpDB->lldpRemoteSystemsData.lldpRem[remoteIndex];
}

/*****************************************************************************/
/** lldpRemGetByMsap
 * \ingroup lldp
 *
 * \desc            Gets Remote-Port Entry by MSAP.
 *
 * \param[in]       msap is the remote-MSAP for which the object need to be
 *                  retrieved.
 *
 * \return          Pointer to lldpRem object if successful.
 * \return          NULL on failure.
 *
 *****************************************************************************/
struct lldpRem *lldpRemGetByMsap(const struct lldpMsap *msap)
{
    struct lldpRem *iter, *next;

    LLDP_LIST_FOR_EACH_SAFE(iter, next, struct lldpRem, node,
                            &lldpDB->lldpRemoteSystemsData.lldpRemTable) {
        if ((iter->lldpRemChassisIdLen == msap->lldpMsapChassisIdLen) &&
            (iter->lldpRemPortIdLen == msap->lldpMsapPortIdLen) &&
            !memcmp(iter->lldpRemChassisId, msap->lldpMsapChassisId, iter->lldpRemChassisIdLen) &&
            !memcmp(iter->lldpRemPortId, msap->lldpMsapPortId, iter->lldpRemPortIdLen)) return iter;
    }
    return NULL;
}


/*****************************************************************************
 * Public Functions (lldpRemManAddr)
 *****************************************************************************/

/*****************************************************************************/
/** lldpRemManAddrCreate
 * \ingroup lldp
 *
 * \desc            Creates Remote-Port Management-Address Entry.
 *
 * \param[in]       remPort is the remote port for which the managment address
 *                  entry need to be created.
 *
 * \param[in]       addrSubtype is the addr-id subtype.
 *
 * \param[in]       addr is a pointer to the managment address string.
 *
 * \return          Pointer to lldpRemManAddr object if successful.
 * \return          NULL on failure.
 *
 *****************************************************************************/
struct lldpRemManAddr *lldpRemManAddrCreate(struct lldpRem *remPort, uint32_t addrSubtype, const char *addr)
{
    struct lldpRemManAddr *obj;

    /* validate that db is initialized and that remote-data is not
       already allocated. */
    if (!lldpDB || lldpRemManAddrGet(remPort, addr))
        return NULL;

    /* allocate data struct */
    obj = (struct lldpRemManAddr *)calloc(1, sizeof(struct lldpRemManAddr));

    /* initialize index */
    obj->lldpRemManAddrSubtype = addrSubtype;
    memcpy(obj->lldpRemManAddr, addr, 32);

    /* attach to link-list */
    lldpListPush(&remPort->lldpRemManAddrTable, &obj->node);

    return obj;

}

/*****************************************************************************/
/** lldpRemManAddrDestroy
 * \ingroup lldp
 *
 * \desc            Destroys Remote-Port Management-Address Entry.
 *
 * \param[in]       obj is the remote-port managment-address entry to be
 *                  destroyed.
 *
 * \return          LLDP_OK if successful.
 * \return          LLDP_ERR_UNKNOWN on failure.
 *
 *****************************************************************************/
int lldpRemManAddrDestroy(struct lldpRemManAddr *obj)
{
    if (!obj)
        return LLDP_ERR_INVALID_ARG;

    /* detach from link-list */
    lldpListRemove(&obj->node);

    /* release remote data allocation */
    free(obj);

    return LLDP_OK;
}

/*****************************************************************************/
/** lldpRemManAddrGet
 * \ingroup lldp
 *
 * \desc            Gets Remote-Port Management-Address Entry by remote-port
 *                  and address.
 *
 * \param[in]       remPort is the remote-port for which the object need
 *                  to be retrieved.
 *
 * \param[in]       addr is the remote-address to be found.
 *
 * \return          Pointer to lldpRemManAddr object if successful.
 * \return          NULL on failure.
 *
 *****************************************************************************/
struct lldpRemManAddr *lldpRemManAddrGet(const struct lldpRem *remPort, const char *addr)
{
    struct lldpRemManAddr *iter, *next;

    LLDP_LIST_FOR_EACH_SAFE(iter, next, struct lldpRemManAddr, node,
                            &remPort->lldpRemManAddrTable) {
        if (!strcmp(iter->lldpRemManAddr, addr))
            return iter;
    }

    return NULL;
}

/*****************************************************************************/
/** lldpRemManAddrGetByIndex
 * \ingroup lldp
 *
 * \desc            Gets Remote-Port Management-Address Entry by remote-port
 *                  and address.
 *
 * \param[in]       remoteIndex is the remote-index for which the object need
 *                  to be retrieved.
 *
 * \param[in]       addr is the remote-address to be found.
 *
 * \return          Pointer to lldpRemManAddr object if successful.
 * \return          NULL on failure.
 *
 *****************************************************************************/
struct lldpRemManAddr *lldpRemManAddrGetByIndex(int remoteIndex, const char *addr)
{
    struct lldpRem *remPort = lldpRemGetByIndex(remoteIndex);
    if (!remPort) return NULL;

    return lldpRemManAddrGet(remPort, addr);
}

/*****************************************************************************/
/** lldpRemManAddrGetByMsap
 * \ingroup lldp
 *
 * \desc            Gets Remote-Port Management-Address Entry by remote-port
 *                  and address.
 *
 * \param[in]       msap is the remote-MSAP for which the object need to be
 *                  retrieved.
 *
 * \param[in]       addr is the remote-address to be found.
 *
 * \return          Pointer to lldpRemManAddr object if successful.
 * \return          NULL on failure.
 *
 *****************************************************************************/
struct lldpRemManAddr *lldpRemManAddrGetByMsap(const struct lldpMsap *msap, const char *addr)
{
    struct lldpRem *remPort = lldpRemGetByMsap(msap);
    if (!remPort) return NULL;

    return lldpRemManAddrGet(remPort, addr);
}


/*****************************************************************************
 * Public Functions (lldpMsap)
 *****************************************************************************/

/*****************************************************************************/
/** lldpMsapFromRemPort
 * \ingroup lldp
 *
 * \desc            Initializes MSAP object from remote-port entry.
 *
 * \param[in]       msap is a pointer to the MSAP object to be initialized.
 *
 * \param[in]       remPort is the remote-port entry.
 *
 * \return          Pointer to the initialized lldpMsap object.
 *
 *****************************************************************************/
struct lldpMsap *lldpMsapFromRemPort(struct lldpMsap *msap, const struct lldpRem *remPort)
{
    msap->lldpMsapChassisIdSubtype = remPort->lldpRemChassisIdSubtype;
    msap->lldpMsapChassisIdLen = remPort->lldpRemChassisIdLen;
    memcpy(msap->lldpMsapChassisId, remPort->lldpRemChassisId, msap->lldpMsapChassisIdLen);
    msap->lldpMsapChassisId[msap->lldpMsapChassisIdLen] = '\0';

    msap->lldpMsapPortIdSubtype = remPort->lldpRemPortIdSubtype;
    msap->lldpMsapPortIdLen = remPort->lldpRemPortIdLen;
    memcpy(msap->lldpMsapPortId, remPort->lldpRemPortId, msap->lldpMsapPortIdLen);
    msap->lldpMsapPortId[msap->lldpMsapPortIdLen] = '\0';

    /* additional indexing data */

    msap->lldpRemIndex = remPort->lldpRemIndex;
    msap->lldpRemLocalPortNum = remPort->lldpRemLocalPortNum;

    return msap;
}

/*****************************************************************************/
/** lldpMsapClear
 * \ingroup lldp
 *
 * \desc            Clears MSAP object.
 *
 * \param[in]       msap is a pointer to the MSAP object to be cleared.
 *
 *****************************************************************************/
void lldpMsapClear(struct lldpMsap *msap)
{
    msap->lldpMsapChassisId[0] = 0;
    msap->lldpMsapChassisIdSubtype = 0;
    msap->lldpMsapChassisIdLen = 0;

    msap->lldpMsapPortId[0] = 0;
    msap->lldpMsapPortIdSubtype = 0;
    msap->lldpMsapPortIdLen = 0;

    /* additional indexing data */

    msap->lldpRemIndex = 0;
    msap->lldpRemLocalPortNum = 0;
}

/*****************************************************************************/
/** lldpChassisIdString
 * \ingroup lldp
 *
 * \desc            Format chassis-id into string.
 *
 * \param[in]       chassisIdSubtype is the chassis-id type (one of 
 *                  lldpChassisIdSubtype enum).
 *
 * \param[in]       chassisId is a pointer to the chassis-id buffer/string.
 *
 * \param[in]       chassisIdLen is the chassis-id length.
 *
 * \param[in]       buf is a pointer to string buffer.
 *
 * \param[in]       len is the size of the string buffer.
 *
 * \return          Pointer to the formatted string buffer.
 *
 *****************************************************************************/
char *lldpChassisIdString(int chassisIdSubtype, char *chassisId, size_t chassisIdLen, char *buf, size_t len)
{
    switch (chassisIdSubtype) {
    case LLDP_CHASSIS_ID_CHASSIS_COMPONENT:
        snprintf(buf, len, "%s (chassis-component)", chassisId);
        break;
    case LLDP_CHASSIS_ID_INTERFACE_ALIAS:
        snprintf(buf, len, "%s (interface-alias)", chassisId);
        break;
    case LLDP_CHASSIS_ID_PORT_COMPONENT:
        snprintf(buf, len, "%s (port-component)", chassisId);
        break;
    case LLDP_CHASSIS_ID_MAC_ADDRESS:
        snprintf(buf, len, "%02x:%02x:%02x:%02x:%02x:%02x (mac-address)",
                 (uint8_t)chassisId[0], (uint8_t)chassisId[1], (uint8_t)chassisId[2],
                 (uint8_t)chassisId[3], (uint8_t)chassisId[4], (uint8_t)chassisId[5]);
        break;
    case LLDP_CHASSIS_ID_NETWORK_ADDRESS:
        snprintf(buf, len, "<...> (network-address)");
        break;
    case LLDP_CHASSIS_ID_INTERFACE_NAME:
        snprintf(buf, len, "%s (if-name)", chassisId);
        break;
    case LLDP_CHASSIS_ID_LOCALLY_ASSIGNED:
        snprintf(buf, len, "%s (local)", chassisId);
        break;
    }

    return buf;
}

/*****************************************************************************/
/** lldpPortIdString
 * \ingroup lldp
 *
 * \desc            Format port-id into string.
 *
 * \param[in]       portIdSubtype is the port-id type (one of 
 *                  lldpPortIdSubtype enum).
 *
 * \param[in]       portId is a pointer to the port-id buffer/string.
 *
 * \param[in]       portIdLen is the port-id length.
 *
 * \param[in]       buf is a pointer to string buffer.
 *
 * \param[in]       len is the size of the string buffer.
 *
 * \return          Pointer to the formatted string buffer.
 *
 *****************************************************************************/
char *lldpPortIdString(int portIdSubtype, char *portId, size_t portIdLen, char *buf, size_t len)
{
    switch (portIdSubtype) {
    case LLDP_PORT_ID_INTERFACE_ALIAS:
        snprintf(buf, len, "%s (interface-alias)", portId);
        break;
    case LLDP_PORT_ID_PORT_COMPONENT:
        snprintf(buf, len, "%s (port-component)", portId);
        break;
    case LLDP_PORT_ID_MAC_ADDRESS:
        snprintf(buf, len, "%02x:%02x:%02x:%02x:%02x:%02x (mac-address)",
                 (uint8_t)portId[0], (uint8_t)portId[1], (uint8_t)portId[2],
                 (uint8_t)portId[3], (uint8_t)portId[4], (uint8_t)portId[5]);
        break;
    case LLDP_PORT_ID_NETWORK_ADDRESS:
        snprintf(buf, len, "<...> (network-address)");
        break;
    case LLDP_PORT_ID_INTERFACE_NAME:
        snprintf(buf, len, "%s (if-name)", portId);
        break;
    case LLDP_PORT_ID_AGENT_CIRCUIT_ID:
        snprintf(buf, len, "<...> (circuit-id)");
        break;
    case LLDP_PORT_ID_LOCALLY_ASSIGNED:
        snprintf(buf, len, "%s (local)", portId);
        break;
    }

    return buf;
}
