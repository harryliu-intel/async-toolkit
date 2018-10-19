/* vim:ts=4:sw=4:expandtab
 * (No tabs, indent level is 4 spaces)  */
/*****************************************************************************
 * File:            fm_api_lport.c
 * Creation Date:   Mar 8, 2007
 * Description:     Logical port (GloRT) management.
 *
 * INTEL CONFIDENTIAL
 * Copyright 2007 - 2012 Intel Corporation. All Rights Reserved. 
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

#include <fm_sdk_int.h>

/*****************************************************************************
 * Macros, Constants & Types
 *****************************************************************************/

/* Helper structure to sort ports by glort */
typedef struct 
{
    fm_int    logicalPort;
    fm_uint32 glort;

} fmPortToGlort;


/*****************************************************************************
 * Global Variables
 *****************************************************************************/


/*****************************************************************************
 * Local Variables
 *****************************************************************************/


/*****************************************************************************
 * Local function prototypes.
 *****************************************************************************/


/*****************************************************************************
 * Local Functions
 *****************************************************************************/


/*****************************************************************************/
/** MaxGlortsPerCamEntry
 * \ingroup intPort
 *
 * \desc            Computes the max number of ports that can be associated
 *                  with a CAM entry configuration.
 *
 * \param[in]       cam is a pointer to the CAM entry.
 *
 * \return          The maximum value, or 0 on error.
 *
 *****************************************************************************/
static fm_int MaxGlortsPerCamEntry(fm_glortCamEntry *cam)
{

    switch (cam->strict)
    {
        case FM_GLORT_ENTRY_TYPE_STRICT:
            return (1 << cam->rangeALength) + (1 << cam->rangeBLength);

            /***************************************************
             * Since the ISL value could be either depending on
             * the ISL tag, we assume the most conservative
             * option.
             **************************************************/
        case FM_GLORT_ENTRY_TYPE_HASHED:
        case FM_GLORT_ENTRY_TYPE_ISL:
            return 1 << cam->rangeALength;

        default:
            break;

    }   /* end switch (cam->strict) */

    return 0;

}   /* end MaxGlortsPerCamEntry */




/*****************************************************************************/
/** CompareByGlort
 * \ingroup intPort
 *
 * \desc            Compares two glort numbers to determine which
 *                  one precedes the other. This function is passed as
 *                  argument to qsort().
 *
 * \param[in]       aPtr is a void pointer to an fmPortToGlort
 *
 * \param[in]       bPtr is a void pointer to an fmPortToGlort
 *
 * \return          -1 if a should be ordered before b
 * \return          0 if a and b should be considered equal
 * \return          1 if a should be ordered after b
 *
 *****************************************************************************/
static int CompareByGlort(const void *aPtr, const void *bPtr)
{
    fmPortToGlort *ptrA = (fmPortToGlort *) aPtr;
    fmPortToGlort *ptrB = (fmPortToGlort *) bPtr;

    if (ptrA->glort < ptrB->glort)
    {
        return -1;
    }
    else if (ptrA->glort > ptrB->glort)
    {
        return 1;
    }
    else
    {
        return 0;
    }

}   /* end CompareByGlort */




/*****************************************************************************/
/** SelectLogicalPorts
 * \ingroup intPort
 *
 * \desc            Selects the port numbers for the logical port(s)
 *                  we are creating.
 *
 * \param[in]       switchPtr points to the switch state structure.
 *
 * \param[in,out]   parmPtr points to the port parameter structure.
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_INVALID_ARGUMENT if the port range is invalid.
 * \return          FM_ERR_LOG_PORT_UNAVAILABLE if the logical port table
 *                  does not have enough free ports.
 *
 *****************************************************************************/
static fm_status SelectLogicalPorts(fm_switch *   switchPtr,
                                    fm_portParms *parmPtr)
{
    fm_int    port;
    fm_int    endPort;
    fm_status err = FM_OK;

    if (parmPtr->basePort != FM_LOGICAL_PORT_ANY)
    {
        /***************************************************
         * The range of logical ports was specified by the
         * caller. Make sure they're available.
         **************************************************/

        endPort = parmPtr->basePort + parmPtr->numPorts;

        /* Make sure the requested port range is valid. */
        if ((parmPtr->basePort < 0) || (parmPtr->numPorts <= 0) ||
            (endPort > switchPtr->maxPort))
        {
            err = FM_ERR_INVALID_ARGUMENT;
            FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PORT, err);
        }

        /* And that the requested ports are available. */
        for (port = parmPtr->basePort ; port < endPort ; ++port)
        {
            if (switchPtr->portTable[port])
            {
                err = FM_ERR_INVALID_ARGUMENT;
                FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PORT, err);
            }
        }
    }
    else
    {
        /***************************************************
         * Find a block of available ports.
         **************************************************/

        parmPtr->basePort = fmFindUnusedLogicalPorts(switchPtr->switchNumber,
                                                     parmPtr->numPorts);
        if (parmPtr->basePort < 0)
        {
            err = FM_ERR_LOG_PORT_UNAVAILABLE;
            FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PORT, err);
        }
    }

    FM_LOG_DEBUG(FM_LOG_CAT_PORT,
                 "Allocating %d logical ports starting at port %d\n",
                 parmPtr->numPorts,
                 parmPtr->basePort);

ABORT:
    return err;

}   /* end SelectLogicalPorts */




/*****************************************************************************/
/** AssignHardwarePortResources
 * \ingroup intPort
 *
 * \desc            Assigns resources for a logical port that maps
 *                  to a single hardware port.
 *
 * \param[in]       switchPtr points to the switch state structure.
 *
 * \param[in,out]   parmPtr points to the port parameter structure.
 *
 * \return          FM_OK if successful.
 *
 *****************************************************************************/
static fm_status AssignHardwarePortResources(fm_switch *   switchPtr,
                                             fm_portParms *parmPtr)
{
    fm_logicalPortInfo *lportInfo;
    fm_glortCamEntry *  camEntry;

    lportInfo = &switchPtr->logicalPortInfo;

    switch (parmPtr->portType)
    {
        case FM_PORT_TYPE_PHYSICAL:
            parmPtr->camIndex       = lportInfo->physicalPortCamIndex;
            camEntry                = &lportInfo->camEntries[parmPtr->camIndex];
            parmPtr->baseGlort      = switchPtr->glortRange.portBaseGlort + 1;
            parmPtr->baseDestIndex  = camEntry->destIndex + 1;
            parmPtr->numDestEntries = 1;
            break;

        case FM_PORT_TYPE_CPU:
            parmPtr->camIndex       = lportInfo->cpuPortCamIndex;
            camEntry                = &lportInfo->camEntries[parmPtr->camIndex];
            parmPtr->baseGlort      = FM_GLORT_CPU_BASE(switchPtr->glortRange.glortBase);
            parmPtr->baseDestIndex  = camEntry->destIndex;
            break;

        case FM_PORT_TYPE_CPU_MGMT:
            parmPtr->camIndex       = lportInfo->cpuMgmtPortCamIndex;
            camEntry                = &lportInfo->camEntries[parmPtr->camIndex];
            parmPtr->baseGlort      = switchPtr->glortRange.cpuMgmtGlort;
            parmPtr->baseDestIndex  = camEntry->destIndex;
            break;

        default:
            FM_LOG_FATAL(FM_LOG_CAT_PORT,
                         "Invalid port type: %d (%s)\n",
                         parmPtr->portType,
                         fmPortTypeToText(parmPtr->portType));

            return FM_ERR_INVALID_ARGUMENT;

    }   /* end switch (parmPtr->portType) */

    FM_LOG_DEBUG(FM_LOG_CAT_PORT,
                 "Assigned hardware port resources: "
                 "portType %s baseGlort=0x%x baseDest=0x%x cam=0x%x\n",
                 fmPortTypeToText(parmPtr->portType),
                 parmPtr->baseGlort, 
                 parmPtr->baseDestIndex,
                 parmPtr->camIndex);

    return FM_OK;

}   /* end AssignHardwarePortResources */




/*****************************************************************************/
/** AssignSpecialPortResources
 * \ingroup intPort
 *
 * \desc            Assigns resources for one of the special logical ports.
 *
 * \param[in]       switchPtr points to the switch state structure.
 *
 * \param[in,out]   parmPtr points to the port parameter structure.
 *
 * \return          FM_OK if successful.
 *
 *****************************************************************************/
static fm_status AssignSpecialPortResources(fm_switch *   switchPtr,
                                            fm_portParms *parmPtr)
{
    fm_logicalPortInfo *lportInfo;
    fm_glortCamEntry *  camEntry;
    fm_int              i;
    fm_status           err;

    lportInfo = &switchPtr->logicalPortInfo;

    parmPtr->camIndex       = lportInfo->specialPortCamIndex;
    camEntry                = &lportInfo->camEntries[parmPtr->camIndex];
    parmPtr->baseDestIndex  = camEntry->destIndex;

    FM_API_CALL_FAMILY(err, 
                       switchPtr->GetGlortForSpecialPort,
                       switchPtr->switchNumber,
                       parmPtr->basePort,
                       &parmPtr->baseGlort);

    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PORT, err);

    /* Only support one port for now */
    if (parmPtr->numPorts != 1)
    {
        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PORT, err = FM_ERR_INVALID_ARGUMENT);
    }

    /* Glort should not be in use at this time */
    if ( FM_IS_GLORT_TAKEN(lportInfo, parmPtr->baseGlort) )
    {
        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PORT, err = FM_ERR_GLORT_IN_USE);
    }

    /* Assign the correct dest entry by masking the glort # */
    parmPtr->baseDestIndex = camEntry->destIndex +
                             (parmPtr->baseGlort & ~FM_GLORT_SPECIAL_BASE);

    /* Make sure the required dest entrie are available */
    i = fmFindUnusedDestEntries(switchPtr->switchNumber,
                                parmPtr->numPorts,
                                parmPtr->baseDestIndex);

    if (i != parmPtr->baseDestIndex)
    {
        err = FM_ERR_LPORT_DESTS_UNAVAILABLE;
        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PORT, err);
    }

    FM_LOG_DEBUG(FM_LOG_CAT_PORT,
                 "Assigned special port resources: "
                 "baseGlort=0x%x baseDest=0x%x basePort=%d cam=0x%x\n",
                 parmPtr->baseGlort, 
                 parmPtr->baseDestIndex,
                 parmPtr->basePort, 
                 parmPtr->camIndex);

ABORT:
    return err;

}   /* end AssignSpecialPortResources */




/*****************************************************************************/
/** AssignLagPortResources
 * \ingroup intPort
 *
 * \desc            Assigns resources for a logical port that maps to a LAG.
 *
 * \param[in]       switchPtr points to the switch state structure.
 *
 * \param[in,out]   parmPtr points to the port parameter structure.
 *
 * \return          FM_OK if successful.
 *
 *****************************************************************************/
static fm_status AssignLagPortResources(fm_switch *   switchPtr,
                                        fm_portParms *parmPtr)
{
    fm_logicalPortInfo *lportInfo;
    fm_glortDestEntry * destEntry[FM_MAX_NUM_LAG_MEMBERS];
    fm_status           err;
    fm_int              firstGlort = 0;
    fm_int              firstDestEntry = 0;
    fm_int              firstLogicalPort = 0;
    fm_int              sw = switchPtr->switchNumber;
    fm_int              maxGlortsPerLag;

    FM_API_CALL_FAMILY(err, 
                       switchPtr->GetMaxGlortsPerLag,
                       switchPtr->switchNumber,
                       &maxGlortsPerLag);
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PORT, err);

    if (parmPtr->numPorts != maxGlortsPerLag)
    {
        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PORT, err = FM_ERR_INVALID_ARGUMENT);
    }

    /* First find unused glorts in the reserved LAG space */
    err = fmFindUnusedLagGlorts(sw,
                                parmPtr->numPorts,
                                parmPtr->useHandle,
                                maxGlortsPerLag,
                                &firstLogicalPort,
                                &firstGlort);
    if (err == FM_OK)
    {
        /* Save the logical port in the port parameter structure */
        parmPtr->basePort = firstLogicalPort;
    }
    else if (!parmPtr->useHandle)
    {
        /* We might be able to find one in the unreserved space.
         * In this case the firstLogicalPort would have been previously save
         * in basePort in SelectLogicalPorts(). 
         */ 
        firstGlort = fmFindUnusedGlorts(sw,
                                        parmPtr->numPorts,
                                        switchPtr->glortRange.lagBaseGlort,
                                        NULL);
        if ( (firstGlort == -1) ||
             ((fm_uint32)firstGlort >= (switchPtr->glortRange.lagBaseGlort + 
                                        switchPtr->glortRange.lagCount)) )
        {
            err = FM_ERR_LOG_PORT_UNAVAILABLE;
        }
        else
        {
            err = FM_OK;
        }
    }
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PORT, err);

    err = fmAllocDestEntries(sw, 1, NULL, destEntry, parmPtr->portType);
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PORT, err);

    firstDestEntry = destEntry[0]->destIndex;

    /* Create an entry to hash the canonical LAG glort */
    err = fmCreateGlortCamEntry(sw,
                                0xFFFF,                 /* cam mask */
                                firstGlort,
                                FM_GLORT_ENTRY_TYPE_HASHED,
                                firstDestEntry,
                                1,                      /* hash count */
                                0,                      /* A length */
                                0,                      /* A offset */
                                0,                      /* B length */
                                0,                      /* B offset */
                                FM_GLORT_ENTRY_HASH_A,
                                0,                      /* dglortTag */
                                &parmPtr->camIndex);

    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PORT, err);

    /* Save information in the port parameter structure */
    parmPtr->numDestEntries = 1;
    parmPtr->baseDestIndex  = firstDestEntry;
    parmPtr->baseGlort      = firstGlort;

    lportInfo = &switchPtr->logicalPortInfo;
    destEntry[0]->owner = &lportInfo->camEntries[parmPtr->camIndex];

    FM_LOG_DEBUG(FM_LOG_CAT_PORT | FM_LOG_CAT_LAG,
                 "Assigned LAG port resources: "
                 "baseGlort=0x%x baseDest=0x%x basePort=%d cam=0x%x\n",
                 parmPtr->baseGlort, 
                 parmPtr->baseDestIndex,
                 parmPtr->basePort, 
                 parmPtr->camIndex);

ABORT:
    return err;

}   /* end AssignLagPortResources */




/*****************************************************************************/
/** AssignMcgPortResources
 * \ingroup intPort
 *
 * \desc            Assigns resources for a logical port that maps to a
 *                  multicast group.
 *
 * \param[in]       switchPtr points to the switch state structure.
 *
 * \param[in,out]   parmPtr points to the port parameter structure.
 *
 * \return          FM_OK if successful.
 *
 *****************************************************************************/
static fm_status AssignMcgPortResources(fm_switch *   switchPtr,
                                        fm_portParms *parmPtr)
{
    fm_logicalPortInfo *lportInfo;
    fm_glortCamEntry *  camEntry;
    fm_status           err;
    fm_int              sw = switchPtr->switchNumber;
    fm_mcgAllocEntry *  allocEntry;
    fm_uint             offsetBase;
    fm_uint             offsetEntry;
    fm_int              cnt;
    fm_int              index;
    fm_int              numDestEntries;
    fm_int              firstDestIndex;


    lportInfo = &switchPtr->logicalPortInfo;

    /* First find unused glorts in the reserved multicast space */
    err = fmFindUnusedMcgGlorts(sw,
                                parmPtr->numPorts,
                                parmPtr->useHandle,
                                &allocEntry,
                                &offsetBase);
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PORT, err);
    
    /* Save the logical port in the port parameter structure */
    parmPtr->baseGlort = allocEntry->baseGlort + offsetBase;
    parmPtr->basePort  = allocEntry->baseHandle + offsetBase;

    index = offsetBase / switchPtr->mcastMaxEntryPerGlort;
    offsetEntry = offsetBase % switchPtr->mcastMaxEntryPerGlort;

    parmPtr->camIndex = allocEntry->mcgCamIndex[index];

    if (!allocEntry->mcgDestIndex[index])
    {
        /* Dest block is not allocated. Find unused dest block 
         * and update the corresponding cam entry accordingly
         */
        numDestEntries = allocEntry->glortSize;

        if (numDestEntries > switchPtr->mcastMaxEntryPerGlort)
        {
            numDestEntries = switchPtr->mcastMaxEntryPerGlort;
        }

        /* We want to get entry from upper space to lower, to prevent
         * conflicting with LAG, thus reduce fragmentations.  The
         * following function will take a negative ammount for the
         * number of dest. entries and count that amount from the
         * end of the range instead.
         */
        firstDestIndex = fmFindUnusedDestEntries(sw,
                                                 numDestEntries,
                                                 -lportInfo->numDestEntries);
        if (firstDestIndex == -1)
        {
            err = FM_ERR_LPORT_DESTS_UNAVAILABLE;
            FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PORT, err);
        }

        camEntry = &lportInfo->camEntries[parmPtr->camIndex];

        /* Update the dest index of the corresponding cam */
        camEntry->destIndex = firstDestIndex;

        FM_API_CALL_FAMILY(err,
                           switchPtr->WriteGlortCamEntry,
                           sw,
                           camEntry,
                           FM_UPDATE_RAM_ONLY);

        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PORT, err);

        for ( cnt = 0 ; cnt < numDestEntries ; cnt++ )
        {
            fm_glortDestEntry *currentDestEntry;

            currentDestEntry = &lportInfo->destEntries[firstDestIndex + cnt];
            memset(currentDestEntry, 0, sizeof(*currentDestEntry));

            currentDestEntry->destIndex = firstDestIndex + cnt;
            currentDestEntry->owner     = camEntry;
            currentDestEntry->usedBy    = FM_USED_BY_TYPE(FM_PORT_TYPE_MULTICAST);
        }

        allocEntry->mcgDestIndex[index] = firstDestIndex;
   
        FM_LOG_DEBUG( 
            FM_LOG_CAT_PORT | FM_LOG_CAT_MULTICAST,
            "Update camIndex %d with destIndex 0x%x numDestEntries %u "
            "index %d offsetEntry %d\n",
            parmPtr->camIndex,
            allocEntry->mcgDestIndex[index],
            numDestEntries,
            index,
            offsetEntry);
    }

    /* Save information in the port parameter structure */
    parmPtr->numDestEntries = 1;
    parmPtr->baseDestIndex  = allocEntry->mcgDestIndex[index] + offsetEntry;

    FM_LOG_DEBUG(FM_LOG_CAT_PORT | FM_LOG_CAT_MULTICAST,
                 "Assigned multicast port resources: "
                 "baseGlort=0x%x baseDest=0x%x cam=0x%x\n",
                 parmPtr->baseGlort, 
                 parmPtr->baseDestIndex,
                 parmPtr->camIndex);

    /* Mark dest entry in use */
    err = fmSetBitArrayBit(&allocEntry->dstInUse[index], offsetEntry, TRUE);

ABORT:
    return err;

}   /* end AssignMcgPortResources */




/*****************************************************************************/
/** AssignPortResources
 * \ingroup intPort
 *
 * \desc            Assigns resources for a logical port.
 *
 * \param[in]       switchPtr points to the switch state structure.
 *
 * \param[in,out]   parmPtr points to the port parameter structure.
 *
 * \return          FM_OK if successful.
 *
 *****************************************************************************/
static fm_status AssignPortResources(fm_switch *   switchPtr,
                                     fm_portParms *parmPtr)
{
    fm_logicalPortInfo *lportInfo;
    fm_status           err;
    fm_int              j;

    parmPtr->baseGlort     = -1;
    parmPtr->camIndex      = -1;
    parmPtr->baseDestIndex = -1;

    switch (parmPtr->portType)
    {
        case FM_PORT_TYPE_PHYSICAL:
        case FM_PORT_TYPE_CPU:
        case FM_PORT_TYPE_CPU_MGMT:
            err = AssignHardwarePortResources(switchPtr, parmPtr);
            FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PORT, err);
            break;

        case FM_PORT_TYPE_SPECIAL:
            err = AssignSpecialPortResources(switchPtr, parmPtr);
            FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PORT, err);
            break;

        case FM_PORT_TYPE_LAG:
            err = AssignLagPortResources(switchPtr, parmPtr);
            FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PORT, err);
            break;

        case FM_PORT_TYPE_LBG:
            FM_API_CALL_FAMILY(err, 
                               switchPtr->AssignLBGPortResources, 
                               switchPtr->switchNumber, 
                               parmPtr);
            FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PORT, err);
            break;

        case FM_PORT_TYPE_MULTICAST:
            err = AssignMcgPortResources(switchPtr, parmPtr);
            FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PORT, err);
            break;

        default:
            FM_LOG_FATAL(FM_LOG_CAT_PORT,
                         "Invalid port type: %d (%s)\n",
                         parmPtr->portType,
                         fmPortTypeToText(parmPtr->portType));
            err = FM_ERR_INVALID_ARGUMENT;
            FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PORT, err);
            break;

    }   /* end switch (parmPtr->portType) */


    FM_LOG_DEBUG(FM_LOG_CAT_PORT,"Using cam entry #%d\n", parmPtr->camIndex);


    if ( (parmPtr->baseGlort < 0) || (parmPtr->baseDestIndex < 0) )
    {
        err = FM_ERR_LOG_PORT_UNAVAILABLE;
        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PORT, err);
    }

    FM_LOG_DEBUG(FM_LOG_CAT_PORT,
                 "Using %d glorts beginning at glort %d\n",
                 parmPtr->numPorts,
                 parmPtr->baseGlort);

    /***************************************************
     * Mark the destination glorts used.
     **************************************************/
    lportInfo = &switchPtr->logicalPortInfo;

    for (j = 0 ; j < parmPtr->numPorts ; ++j)
    {
        FM_SET_GLORT_IN_USE(lportInfo, parmPtr->baseGlort + j);
    }

ABORT:
    return err;

}   /* end AssignPortResources */




/*****************************************************************************/
/** CreateLogicalPort
 * \ingroup intPort
 *
 * \desc            Allocates and initializes a logical port structure.
 *
 * \param[in]       switchPtr points to the switch state structure.
 *
 * \param[in]       parmPtr points to the port parameter structure.
 *
 * \param[in]       subIndex is the index of this port in the block of
 *                  logical ports described by the port parameter structure.
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_NO_MEM if the structure could not be allocated.
 *
 *****************************************************************************/
static fm_status CreateLogicalPort(fm_switch *   switchPtr,
                                   fm_portParms *parmPtr,
                                   fm_int        subIndex)
{
    fm_logicalPortInfo *lportInfo;
    fm_glortDestEntry * destEntry;
    fm_glortCamEntry *  camEntry;
    fm_port *           portPtr;
    fm_status           err;
    fm_int              port;
    fm_uint32           glort;
    fm_int              destIndex;
    fm_int              cpi;
    fm_int              physPort;

    if (parmPtr->portType == FM_PORT_TYPE_PHYSICAL ||
        parmPtr->portType == FM_PORT_TYPE_CPU)
    {
        cpi = parmPtr->basePort + subIndex;

        err = fmMapCardinalPortInternal(switchPtr,
                                        cpi,
                                        &port,
                                        &physPort);
        if (err != FM_OK)
        {
            FM_LOG_ERROR(FM_LOG_CAT_PORT,
                         "fmMapCardinalPortInternal(%d,%d): %s\n",
                         switchPtr->switchNumber,
                         cpi,
                         fmErrorMsg(err));
            FM_LOG_EXIT(FM_LOG_CAT_PORT, err);
        }
    }
    else
    {
        port      = parmPtr->basePort + subIndex;
        cpi       = -1;
        physPort  = -1;
    }

    FM_LOG_ENTRY(FM_LOG_CAT_PORT,
                 "sw=%d port=%d type=%s\n",
                 switchPtr->switchNumber,
                 port,
                 fmPortTypeToText(parmPtr->portType));

    glort = parmPtr->baseGlort + subIndex;

    if (parmPtr->portType == FM_PORT_TYPE_LAG)
    {
        /* All logical ports allocated for a specific LAG must point
         * to the LAG's first destination entry.  portIndex = 0 is
         * the lag port index */
        destIndex = parmPtr->baseDestIndex;
        if (subIndex != 0)
        {
            parmPtr->numDestEntries = 0;
        }
    }
    else if (parmPtr->portType == FM_PORT_TYPE_LBG)
    {
        destIndex = parmPtr->baseDestIndex + (subIndex * parmPtr->numDestEntries);
    }
    else
    {
        destIndex = parmPtr->baseDestIndex + subIndex;
    }

    lportInfo = &switchPtr->logicalPortInfo;
    camEntry  = &lportInfo->camEntries[parmPtr->camIndex];
    destEntry = &lportInfo->destEntries[destIndex];


    /***************************************************
     * Allocate and initialize the fm_port structure. 
     *  
     * It's required to initialize the fm_port structure 
     * and save the portPtr in the portTable[] before 
     * calling switchPtr->CreateLogicalPort since it 
     * uses the fm_port structure.
     **************************************************/

    portPtr = fmAlloc(sizeof(fm_port));
    if (portPtr == NULL)
    {
        FM_LOG_FATAL(FM_LOG_CAT_PORT,
                     "Unable to allocate memory for logical port\n");
        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PORT, err = FM_ERR_NO_MEM);
    }

    memset(portPtr, 0, sizeof(*portPtr));

    portPtr->switchPtr      = switchPtr;
    portPtr->portNumber     = port;
    portPtr->portType       = parmPtr->portType;
    portPtr->glort          = glort;
    portPtr->numDestEntries = parmPtr->numDestEntries;
    portPtr->freeCount      = (subIndex == 0) ? parmPtr->freeCount : 1;
    portPtr->camEntry       = camEntry;
    portPtr->destEntry      = destEntry;
    portPtr->portIndex      = cpi;
    portPtr->physicalPort   = physPort;

    switchPtr->portTable[port] = portPtr;

    /* Allocate and initialize the port extension structure. */
    FM_API_CALL_FAMILY(err, 
                       switchPtr->CreateLogicalPort, 
                       switchPtr->switchNumber, 
                       port);

    if (err != FM_OK)
    {
        /* Unable to create the port extension structure,
         * so free the fm_port structure allocated above and return an error.
         */
        switchPtr->portTable[port] = NULL;
        fmFree(portPtr);
        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PORT, err);
    }


    /***************************************************
     * Increment CAM entry use count.
     **************************************************/

    if (camEntry->useCount < 0)
    {
        camEntry->useCount = 1;
    }
    else
    {
        ++camEntry->useCount;
    }

    FM_LOG_DEBUG(FM_LOG_CAT_PORT,
                 "Allocated logical port %d mapping to cam %d, glort %d\n",
                 port,
                 camEntry->camIndex,
                 glort);

    /***************************************************
     * Initialize destination entry. 
     **************************************************/

    if (parmPtr->portType != FM_PORT_TYPE_LBG)
    {
        memset(destEntry, 0, sizeof(*destEntry));
    
        destEntry->destIndex = destIndex;
        destEntry->owner     = camEntry;
        destEntry->usedBy    = FM_USED_BY_TYPE(parmPtr->portType);
    }

    /***************************************************
     * Get the port's hardware capabilities.
     **************************************************/

    if (parmPtr->portType == FM_PORT_TYPE_PHYSICAL ||
        parmPtr->portType == FM_PORT_TYPE_CPU)
    {
        fmPlatformGetPortCapabilities(switchPtr->switchNumber,
                                      physPort,
                                      &portPtr->capabilities);

        /* indicate to the platform code that we are changing state */
        fmPlatformAddGlortToPortMapping(switchPtr->switchNumber, 
                                        glort,
                                        physPort);
    }

    /***************************************************
     * Perform port-specific initialization.
     **************************************************/

    err = fmInitPort(switchPtr->switchNumber, portPtr);
    if (err != FM_OK)
    {
        switchPtr->portTable[port] = NULL;

        /* Free the port extension structure, which has been allocated in
         * switchPtr->CreateLogicalPort above. */
        fmFree(portPtr->extension);
        fmFree(portPtr);
    }

ABORT:
    FM_LOG_EXIT(FM_LOG_CAT_PORT, err);

}   /* end CreateLogicalPort */




/*****************************************************************************
 * Public Functions
 *****************************************************************************/

/*****************************************************************************/
/** fmInitializeLogicalPorts
 * \ingroup intPort
 *
 * \desc            For FM4000 and FM6000 only.                             \lb
 *                  Initializes the logical port management structures.
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \return          FM_OK if successful.
 *
 *****************************************************************************/
fm_status fmInitializeLogicalPorts(fm_int sw)
{
    fm_switch *         switchPtr;
    fm_logicalPortInfo *lportInfo;
    fm_int              destBase = 0;
    fm_int              destLimit;
    fm_uint32           rangeMask;
    fm_int              index;
    fm_int              block;
    fm_uint32           entryType;
    fm_status           err = FM_OK;

    FM_LOG_ENTRY(FM_LOG_CAT_PORT, "sw=%d\n", sw);

    PROTECT_SWITCH(sw);

    /***************************************************
     * If the switch is not up yet, keep going.  Note
     * that we don't check for bootComplete because we
     * are called before that variable is set.
     **************************************************/

    if ( ( switchPtr = GET_SWITCH_PTR(sw) ) == NULL )
    {
        err = FM_ERR_INVALID_SWITCH;
        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PORT, err);
    }

    lportInfo = &switchPtr->logicalPortInfo;
    rangeMask = switchPtr->glortRange.glortMask;

    /***************************************************
     * To initialize the subsystem, create the cam
     * entries for the relevant static partitions.
     **************************************************/

    fmResetLogicalPortInfo(lportInfo);

    /***************************************************
     * Allocate dest entries for physical ports based
     * on the size.  
     **************************************************/

    destBase = fmFindUnusedDestEntries(sw, 
                                       switchPtr->glortRange.portCount, 
                                       destBase);
    if (destBase < 0)
    {
        err = FM_ERR_LPORT_DESTS_UNAVAILABLE;
        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PORT, err);
    }

    destLimit = destBase + switchPtr->glortRange.portCount;

    /***************************************************
     * Mark those entries as used to reserve them for 
     * physical ports. This way these entries will not 
     * be available in subsequent call to function 
     * fmFindUnusedDestEntries. 
     **************************************************/

    for ( index = destBase; index < destLimit; ++index )
    {
        lportInfo->destEntries[index].usedBy = 
                                        FM_USED_BY_TYPE(FM_PORT_TYPE_PHYSICAL);
    }

    /***************************************************
     * Physical ports get one cam entry for their block.
     **************************************************/
    if ( fmGetBoolApiAttribute(FM_AAK_API_STRICT_GLORT_PHYSICAL, 
                               FM_AAD_API_STRICT_GLORT_PHYSICAL) )
    {
        entryType = FM_GLORT_ENTRY_TYPE_STRICT;
    }
    else
    {
        entryType = FM_GLORT_ENTRY_TYPE_ISL;
    }

    err = fmCreateGlortCamEntry(sw,
                                (rangeMask>0xFF)?0xFF00:(~rangeMask),
                                switchPtr->glortRange.portBaseGlort,
                                entryType,
                                destBase,              
                                1,                      /* hash count */
                                switchPtr->glortRange.portALength,
                                0,                      /* A offset */
                                0,                      /* B length */
                                0,                      /* B offset */
                                FM_GLORT_ENTRY_HASH_A,
                                0,                      /* dglortTag */
                                &lportInfo->physicalPortCamIndex);

    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PORT, err);

    destBase += switchPtr->glortRange.portCount;

    /***************************************************
     * Allocate dest entries for CPU port.  
     **************************************************/

    destBase = fmFindUnusedDestEntries(sw, 
                                       switchPtr->glortRange.cpuPortCount, 
                                       destBase);
    if (destBase < 0)
    {
        err = FM_ERR_LOG_PORT_UNAVAILABLE;
        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PORT, err);
    }

    /***************************************************
     * Now add the CAM entry for the CPU port(s).
     **************************************************/

    err = fmCreateGlortCamEntry(sw,
                                0xFF00,
                                FM_GLORT_CPU_BASE(switchPtr->glortRange.glortBase),
                                FM_GLORT_ENTRY_TYPE_ISL,
                                destBase,
                                1,                      /* hash count */
                                0,                      /* A length */
                                0,                      /* A offset */
                                0,                      /* B length */
                                0,                      /* B offset */
                                FM_GLORT_ENTRY_HASH_A,
                                0,                      /* dglortTag */
                                &lportInfo->cpuPortCamIndex);

    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PORT, err);

    destBase += switchPtr->glortRange.cpuPortCount;

    /***************************************************
     * Allocate dest entries for special ports based
     * on the size.
     *
     * NOTE: Due to the fact that the special ports
     * are a subset of the reserved CPU port range
     * and we don't have code yet to sort the cam
     * in mask order, so we will need to add this
     * cam entry after the CPU cam for this to have
     * higher precedence.
     **************************************************/

    destBase = fmFindUnusedDestEntries(sw, FM_GLORT_SPECIAL_SIZE, destBase);
    if (destBase < 0)
    {
        err = FM_ERR_LPORT_DESTS_UNAVAILABLE;
        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PORT, err);
    }

    /***************************************************
     * Special glorts have one cam entry for the entire
     * block.
     **************************************************/

    err = fmCreateGlortCamEntry(sw,
                                FM_GLORT_SPECIAL_MASK,  /* cam mask */
                                FM_GLORT_SPECIAL_BASE,
                                FM_GLORT_ENTRY_TYPE_ISL,
                                destBase,
                                1,                         /* hash count */
                                FM_GLORT_SPECIAL_A_LENGTH, 
                                0,                         /* A offset */
                                0,                         /* B length */
                                0,                         /* B offset */
                                FM_GLORT_ENTRY_HASH_A,
                                0,                         /* dglortTag */
                                &lportInfo->specialPortCamIndex);

    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PORT, err);

    destBase += FM_GLORT_SPECIAL_SIZE;

    /***************************************************
     * Allocate dest entries for CPU management port.
     **************************************************/

    destBase = fmFindUnusedDestEntries(sw, 1, destBase);
    if (destBase < 0)
    {
        err = FM_ERR_LPORT_DESTS_UNAVAILABLE;
        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PORT, err);
    }

    /***************************************************
     * Now add the CAM entry for the CPU Mgmt port.
     * This entry will always point to CPU port 0,
     * whereas the CPU port might be redirect to another
     * physical or LAG port
     **************************************************/

    err = fmCreateGlortCamEntry(sw,
                                0xFFFF,                 /* CAM mask*/
                                switchPtr->glortRange.cpuMgmtGlort,
                                FM_GLORT_ENTRY_TYPE_ISL,
                                destBase,
                                1,                      /* hash count */
                                0,                      /* A length */
                                0,                      /* A offset */
                                0,                      /* B length */
                                0,                      /* B offset */
                                FM_GLORT_ENTRY_HASH_A,
                                0,                      /* dglortTag */
                                &lportInfo->cpuMgmtPortCamIndex);

    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PORT, err);

    /***************************************************
     * Create bit arrays that will be used to manage 
     * the multicast glort dest table entry usage.
     **************************************************/

    for (block = 0 ; block < FM_MCG_ALLOC_TABLE_SIZE ; block++)
    {
        for (index = 0 ; index < FM_RESV_MCAST_CAM_MAX ; index++)
        {
            err = fmCreateBitArray(&lportInfo->mcgAllocTable[block].dstInUse[index],
                                   switchPtr->mcastMaxEntryPerGlort);

            FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PORT, err);
        }
    }

ABORT:
    UNPROTECT_SWITCH(sw);

    FM_LOG_EXIT(FM_LOG_CAT_PORT, err);

}   /* end fmInitializeLogicalPorts */




/*****************************************************************************/
/** fmCommonAllocLogicalPort
 * \ingroup intPort
 *
 * \desc            For FM4000 and FM6000 only.                             \lb
 *                  Allocates a contiguous block of logical port entries,
 *                  optionally using the given hint as a starting port number,
 *                  or specifying a handle to associated with a given glort.
 *                                                                      \lb\lb
 *                  Called via the AllocLogicalPort function pointer.
 *
 * \note            To prevent fragmentation of the destination table,
 *                  single physical port allocations will be made from the
 *                  top of the table and should be preallocated at
 *                  initialization time. LAG entries are always allocated in
 *                  blocks of FM_MAX_PORTS_PER_LAG_PER_SWITCH, starting from
 *                  the top of the table. Multicast entries are single entries
 *                  allocated from the bottom of the table.
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \param[in]       portType is the allocation type (see ''fm_portType'').
 *
 * \param[in]       numPorts is the number of logical ports to allocate in the
 *                  block.
 *
 * \param[in,out]   firstPort is the allocated logical port number for
 *                  the block.  On entry, firstPort contains a hint
 *                  value or FM_LOGICAL_PORT_ANY if no hint is to be provided.
 *                  On exit, firstPort will contain the actual allocated
 *                  value, which may be different from the hint value.
 *
 * \param[in]       useHandle is the handle specifying a specific allocated
 *                  resources. This is mainly used in stacking configuration. 
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_INVALID_SWITCH if sw is invalid.
 * \return          FM_ERR_LOG_PORT_UNAVAILABLE if a block of the specified
 *                  type is not available.
 *
 *****************************************************************************/
fm_status fmCommonAllocLogicalPort(fm_int      sw,
                                   fm_portType portType,
                                   fm_int      numPorts,
                                   fm_int *    firstPort,
                                   fm_int      useHandle)
{
    fm_switch *  switchPtr;
    fm_portParms parms;
    fm_int       subIndex;
    fm_status    err;

    FM_LOG_ENTRY(FM_LOG_CAT_PORT,
                 "sw=%d type=%d numPorts=%d firstPort=%p useHandle=%d\n",
                 sw,
                 portType,
                 numPorts,
                 (void *) firstPort,
                 useHandle);

    switchPtr = GET_SWITCH_PTR(sw);

    /* Initialize the port parameter structure used to keep track of the
     * resources that have been or that will be assigned to the port. 
     */
    memset(&parms, 0, sizeof(parms));

    parms.portType       = portType;
    parms.numPorts       = numPorts;
    parms.basePort       = *firstPort;
    parms.numDestEntries = numPorts;
    parms.useHandle      = useHandle;
    parms.freeCount      = 1;

    if (portType == FM_PORT_TYPE_LBG)
    {
        FM_API_CALL_FAMILY(err,
                           switchPtr->GetPortParametersForLBG,
                           sw,
                           &parms.numPorts,
                           &parms.numDestEntries);
        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PORT, err);

        parms.basePort = FM_LOGICAL_PORT_ANY;
        parms.freeCount = parms.numPorts;
    }

    if (portType == FM_PORT_TYPE_LAG)
    {
        parms.freeCount = numPorts;
    }

    /* Select the logical port(s) to assign if handle is NULL, otherwise
       the logical port is specified in the handle. */
    if (!useHandle)
    {
        err = SelectLogicalPorts(switchPtr, &parms);
        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PORT, err);
    }

    /* Assign hardware resources based on port type. */
    err = AssignPortResources(switchPtr, &parms);
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PORT, err);

    /* Allocate the port structures and initialize them. */
    for (subIndex = 0 ; subIndex < parms.numPorts ; subIndex++)
    {
        err = CreateLogicalPort(switchPtr, &parms, subIndex);
        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PORT, err);
    }

    *firstPort = parms.basePort;

ABORT:
    FM_LOG_EXIT(FM_LOG_CAT_PORT, err);

}   /* end fmCommonAllocLogicalPort */




/*****************************************************************************/
/** fmCommonFreeLogicalPort
 * \ingroup intPort
 *
 * \desc            For FM4000 and FM6000 only.                             \lb
 *                  Deallocates a logical port or a block of logical ports.
 *                  Note that the number of logical ports freed is determined
 *                  by the port type.
 *                                                                      \lb\lb
 *                  Called via the FreeLogicalPort function pointer.
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \param[in]       logicalPort is the logical port to free.
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_INVALID_SWITCH if sw is invalid.
 * \return          FM_ERR_INVALID_ARGUMENT if the logical port is invalid.
 *                  value.
 * \return          FM_ERR_PORT_IS_IN_LAG if the logical port is currently 
 *                  a LAG member.
 *
 *****************************************************************************/
fm_status fmCommonFreeLogicalPort(fm_int sw, fm_int logicalPort)
{
    fm_switch *          switchPtr;
    fm_port *            portPtr;
    fm_maPurge *         purgePtr;
    fm_logicalPortInfo * lportInfo;
    fm_status            rc = FM_OK;
    fm_status            err;
    fm_int               freeCount;
    fm_int               endPort;
    fm_int               port;
    fm_int               j;
    fm_maPurgeListEntry *purgeEntry;

    FM_LOG_ENTRY(FM_LOG_CAT_PORT,
                 "sw=%d, port=%d\n",
                 sw,
                 logicalPort);

    switchPtr = GET_SWITCH_PTR(sw);
    lportInfo = &switchPtr->logicalPortInfo;

    portPtr = GET_SWITCH_PORT_PTR(switchPtr, logicalPort);
    if (portPtr == NULL)
    {
        FM_LOG_EXIT(FM_LOG_CAT_PORT, FM_ERR_INVALID_ARGUMENT);
    }

    /* Only check when switch is in working state */
    if (FM_IS_STATE_ALIVE(switchPtr->state) && FM_IS_PORT_IN_A_LAG(portPtr))
    {
        /* Should remove port from LAG before freeing it. */
        FM_LOG_EXIT(FM_LOG_CAT_PORT, FM_ERR_PORT_IS_IN_LAG);
    }

    /* portPtr->freeCount has been set in ''fmCommonAllocLogicalPort'' */
    if ((freeCount = portPtr->freeCount) == 0)
    {
        freeCount = 1;
    }
    endPort = logicalPort + freeCount;

    /***************************************************
     * Free this logical port and its subsidiaries.
     **************************************************/

    for (port = logicalPort ; port < endPort ; ++port)
    {
        portPtr = GET_SWITCH_PORT_PTR(switchPtr, port);

        if (!portPtr)
        {
            FM_LOG_ERROR(FM_LOG_CAT_PORT,
                         "Logical port %d is already free\n",
                         port);
            rc = FM_ERR_INVALID_PORT;
            continue;
        }

        FM_LOG_DEBUG(FM_LOG_CAT_PORT,
                     "Freeing logical port %d mapping to cam %d, glort %d\n",
                     port,
                     portPtr->camEntry->camIndex,
                     portPtr->glort);

        /***************************************************
         * Free the glort cam entry.
         **************************************************/

        err = fmRemoveGlortCamEntry(sw, portPtr->camEntry->camIndex);

        if (err != FM_OK)
        {
            FM_LOG_ERROR(FM_LOG_CAT_PORT,
                         "Error removing cam entry for port %d: %s\n",
                         port,
                         fmErrorMsg(err));
            rc = err;
        }

        /***************************************************
         * Free the dmask table entries.
         **************************************************/

        switch (portPtr->portType)
        {
            case FM_PORT_TYPE_MULTICAST:
                err = fmFreeMcastLogicalPort(sw, port);

                if (err != FM_OK)
                {
                    FM_LOG_ERROR(FM_LOG_CAT_PORT, 
                                 "Unable to free mcast logical port: %s\n", 
                                 fmErrorMsg(err));
                    rc = err;
                }
                break;

            default:
                for (j = 0 ; j < portPtr->numDestEntries ; ++j)
                {
                    FM_API_CALL_FAMILY(err,
                                       switchPtr->FreeDestEntry,
                                       sw,
                                       &portPtr->destEntry[j]);
            
                    if (err != FM_OK)
                    {
                        FM_LOG_ERROR(FM_LOG_CAT_PORT, 
                                     "Unable to free dest entry for port %d: %s\n",
                                     port,
                                     fmErrorMsg(err));
                        rc = err;
                    }
                }
                break;
                
        }   /* end switch (portPtr->portType) */
        
        /**************************************************
         * If port has a purge list entry allocated and
         * there are no purges pending for this port,
         * then free the purge list entry.
         **************************************************/
        
        FM_TAKE_MA_PURGE_LOCK(sw);
        purgePtr = &switchPtr->maPurge;

        if (portPtr->maPurgeListEntry)
        {
            purgeEntry = portPtr->maPurgeListEntry;
            purgeEntry->portExists = FALSE;
            
            if ( fmMaybeDestroyMAPurgeListEntry(sw, purgeEntry) )
            {
                /* Entry was destroyed. */
                ++purgePtr->stats.numEntriesFreedWithLogicalPort;
            }
            else
            {
                /**************************************************
                 * There is a pending purge for this port, so we
                 * can't delete it now. It will be deleted when
                 * the pending purges are completed.
                 **************************************************/
                ;
                
            }   /* end if (!purgeEntry->allVlansPending &&... */
            
        }   /* end if (portPtr->maPurgeListEntry) */

        FM_DROP_MA_PURGE_LOCK(sw);

        /***************************************************
         * Free the logical port and glort, but preserve
         * the reserved bit.
         **************************************************/

        FM_SET_LPORT_FREE(lportInfo, port);
        FM_SET_GLORT_FREE(lportInfo, portPtr->glort);

        /***************************************************
         * Free the data structures.
         **************************************************/

        fmFree(portPtr->extension);
        fmFree(portPtr);

        switchPtr->portTable[port] = NULL;

    }   /* end for (i = port ; i < endPort ; ++i) */

    FM_LOG_EXIT(FM_LOG_CAT_PORT, rc);

}   /* end fmCommonFreeLogicalPort */




/*****************************************************************************/
/** fmAllocateLogicalPortDataStructures
 * \ingroup intPort
 *
 * \desc            For FM4000 and FM6000 only.                             \lb
 *                  Allocates data structures upon switch insertion.
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \param[in]       numCamEntries is the number of CAM entries to allocate.
 *
 * \param[in]       numDestEntries is the number of dest table entries to
 *                  allocate.
 *
 * \return          FM_OK on success.
 *
 *****************************************************************************/
fm_status fmAllocateLogicalPortDataStructures(fm_int sw,
                                              fm_int numCamEntries,
                                              fm_int numDestEntries)
{
    fm_logicalPortInfo *lportInfo;
    fm_switch *         switchPtr;
    fm_uint             nbytes;

    FM_LOG_ENTRY(FM_LOG_CAT_PORT, 
                 "sw = %d numCamEntries = %d numDestEntries %d\n",
                 sw,
                 numCamEntries,
                 numDestEntries);

    switchPtr = GET_SWITCH_PTR(sw);
    lportInfo = &switchPtr->logicalPortInfo;

    /***************************************************
     * Allocate glort cam table.
     **************************************************/

    lportInfo->numCamEntries = numCamEntries;
    nbytes = sizeof(fm_glortCamEntry) * lportInfo->numCamEntries;

    lportInfo->camEntries = fmAlloc(nbytes);
    if (lportInfo->camEntries == NULL)
    {
        FM_LOG_EXIT(FM_LOG_CAT_PORT, FM_ERR_NO_MEM);
    }

    memset(lportInfo->camEntries, 0, nbytes);

    /***************************************************
     * Allocate glort destination table.
     **************************************************/

    lportInfo->numDestEntries = numDestEntries;
    nbytes = sizeof(fm_glortDestEntry) * lportInfo->numDestEntries;

    lportInfo->destEntries = fmAlloc(nbytes);
    if (lportInfo->destEntries == NULL)
    {
        FM_LOG_EXIT(FM_LOG_CAT_PORT, FM_ERR_NO_MEM);
    }

    memset(lportInfo->destEntries, 0, nbytes);

    FM_LOG_EXIT(FM_LOG_CAT_PORT, FM_OK);

}   /* end fmAllocateLogicalPortDataStructures */




/*****************************************************************************/
/** fmAllocLogicalPort
 * \ingroup intlport
 *
 * \desc            Allocate a continguous block of logical port entries,
 *                  optionally using the given hint as a starting port number.
 *
 * \note            To prevent fragmentation of the destination table,
 *                  single physical port allocations will be made from the
 *                  top of the table and should be preallocated at
 *                  initialization time; LAG entries are always allocated in
 *                  blocks of FM_MAX_PORTS_PER_LAG_PER_SWITCH, starting from
 *                  the top of the table; multicast entries are single entries
 *                  allocated from the bottom of the table.
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \param[in]       type is the allocation type (see ''fm_portType'').
 *
 * \param[in]       numPorts indicates the number of ports to allocate.  For
 *                  LAG type allocations, this argument is not used, and 
 *                  internally the value FM_MAX_PORTS_PER_LAG_PER_SWITCH is 
 *                  used.
 *
 * \param[in,out]   firstPortNumber is the allocated logical port number for
 *                  the block.  On entry, firstPortNumber contains a hint
 *                  value or FM_LOGICAL_PORT_ANY if no hint is to be provided.
 *                  On exit, firstPortNumber will contain the actual allocated
 *                  value, which may be different from the hint value.
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_INVALID_SWITCH if sw is invalid.
 * \return          FM_ERR_LOG_PORT_UNAVAILABLE if a block of the specified
 *                  type is not available.
 *
 *****************************************************************************/
fm_status fmAllocLogicalPort(fm_int      sw,
                             fm_portType type,
                             fm_int      numPorts,
                             fm_int *    firstPortNumber)
{
    fm_status  err;
    fm_switch *switchPtr;

    VALIDATE_AND_PROTECT_SWITCH(sw);

    switchPtr = GET_SWITCH_PTR(sw);

    FM_API_CALL_FAMILY(err,
                       switchPtr->AllocLogicalPort,
                       sw,
                       type,
                       numPorts,
                       firstPortNumber,
                       0);

    UNPROTECT_SWITCH(sw);

    return err;

}   /* end fmAllocLogicalPort */




/*****************************************************************************/
/** fmFreeLogicalPortDataStructures
 * \ingroup intPort
 *
 * \desc            Frees data structures upon switch removal.
 *
 * \param[in]       switchPtr points to the switch state structure.
 *
 * \return          FM_OK on success.
 *
 *****************************************************************************/
fm_status fmFreeLogicalPortDataStructures(fm_switch *switchPtr)
{
    fm_logicalPortInfo *lportInfo;
    fm_status           err;
    fm_status           rc = FM_OK;

    FM_LOG_ENTRY(FM_LOG_CAT_PORT, "sw = %d\n", switchPtr->switchNumber);

    lportInfo = &switchPtr->logicalPortInfo;

    /***************************************************
     * Free logical ports.
     **************************************************/

    err = fmFreeLogicalPortResources(switchPtr->switchNumber);
    if (err != FM_OK && rc == FM_OK)
    {
        rc = err;
    }

    /***************************************************
     * Free logical port table.
     **************************************************/

    if (switchPtr->portTable)
    {
        fmFree(switchPtr->portTable);
        switchPtr->portTable = NULL;
    }

    /***************************************************
     * Free glort cam table.
     **************************************************/

    if (lportInfo->camEntries)
    {
        fmFree(lportInfo->camEntries);
        lportInfo->camEntries = NULL;
    }

    /***************************************************
     * Free glort destination table.
     **************************************************/

    if (lportInfo->destEntries)
    {
        fmFree(lportInfo->destEntries);
        lportInfo->destEntries = NULL;
    }

    FM_LOG_EXIT(FM_LOG_CAT_PORT, rc);

}   /* end fmFreeLogicalPortDataStructures */




/*****************************************************************************/
/** fmFreeLogicalPortResources
 * \ingroup intPort
 *
 * \desc            This is called when the switch goes down, to free the 
 *                  currently allocated logical ports. 
 *
 * \param[in]       sw is the switch number.
 *
 * \return          FM_OK if successful.
 *
 *****************************************************************************/
fm_status fmFreeLogicalPortResources(fm_int sw)
{
    fm_logicalPortInfo *lportInfo;
    fm_switch *         switchPtr;
    fm_int              port;
    fm_int              block;
    fm_int              index;
    fm_status           err;
    fm_status           rc = FM_OK;

    FM_LOG_ENTRY(FM_LOG_CAT_PORT, "sw = %d\n", sw);

    switchPtr = GET_SWITCH_PTR(sw);

    /***************************************************
     * Free all logical ports.
     **************************************************/

    if (switchPtr->portTable)
    {
        for (port = 0 ; port < switchPtr->maxPort ; ++port)
        {
            if (switchPtr->portTable[port])
            {
                FM_API_CALL_FAMILY(err, switchPtr->FreeLogicalPort, sw, port);

                if (err != FM_OK && rc == FM_OK)
                {
                    /* Remember the first error code we see. */
                    rc = err;
                }
            }
        }
    }

    lportInfo = &switchPtr->logicalPortInfo;

    for (block = 0 ; block < FM_MCG_ALLOC_TABLE_SIZE ; block++)
    {
        for (index = 0 ; index < FM_RESV_MCAST_CAM_MAX ; index++)
        {
            fmDeleteBitArray(&lportInfo->mcgAllocTable[block].dstInUse[index]);
        }
    }

    FM_LOG_EXIT(FM_LOG_CAT_PORT, rc);

}   /* end fmFreeLogicalPortResources */




/*****************************************************************************/
/** fmFreeLogicalPort
 * \ingroup intlport
 *
 * \desc            Deallocate a logical port or a block of logical ports.
 *                  Note that the number of logical port freed is determined
 *                  by the port type.
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \param[in]       port is the logical port to free.
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_INVALID_SWITCH if sw is invalid.
 * \return          FM_ERR_INVALID_ARGUMENT if the logical port is invalid.
 *                  value.
 *
 *****************************************************************************/
fm_status fmFreeLogicalPort(fm_int sw, fm_int port)
{
    fm_status  err;
    fm_switch *switchPtr;
    fm_port *  portPtr;

    VALIDATE_AND_PROTECT_SWITCH(sw);

    switchPtr = GET_SWITCH_PTR(sw);
    portPtr   = GET_PORT_PTR(sw, port);

    if (portPtr == NULL)
    {
        UNPROTECT_SWITCH(sw);

        return FM_ERR_INVALID_ARGUMENT;
    }

    FM_API_CALL_FAMILY(err, switchPtr->FreeLogicalPort, sw, port);

    UNPROTECT_SWITCH(sw);

    return err;

}   /* end fmFreeLogicalPort */




/*****************************************************************************/
/** fmSetLogicalPortAttribute
 * \ingroup intPort
 *
 * \desc            Set a logical port attribute, preserving an internal
 *                  record of some attributes.
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \param[in]       port is the logical port on which to operate.
 *
 * \param[in]       attr is the logical port attribute to set.
 *
 * \param[in]       value points to the attribute value to set.
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_INVALID_SWITCH if sw is invalid.
 * \return          FM_ERR_INVALID_PORT if port is invalid.
 * \return          FM_ERR_INVALID_ARGUMENT if unrecognized attribute.
 * \return          FM_ERR_INVALID_ATTRIB if read-only attribute.
 *
 *****************************************************************************/
fm_status fmSetLogicalPortAttribute(fm_int sw,
                                    fm_int port,
                                    fm_int attr,
                                    void * value)
{
    fm_status  err;
    fm_switch *switchPtr;

    VALIDATE_AND_PROTECT_SWITCH(sw);

    switchPtr = GET_SWITCH_PTR(sw);

    err = switchPtr->SetLogicalPortAttribute(sw, port, attr, value);

    UNPROTECT_SWITCH(sw);

    return err;

}   /* end fmSetLogicalPortAttribute */




/*****************************************************************************/
/** fmGetLogicalPortAttribute
 * \ingroup intPort
 *
 * \desc            Gets a logical port attribute, preserving an internal
 *                  record of some attributes.
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \param[in]       port is the logical port on which to operate.
 *
 * \param[in]       attr is the logical port attribute to set.
 *
 * \param[in]       value points to caller allocated storage where the value
 *                  of the attribute will be written.
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_INVALID_SWITCH if sw is invalid.
 * \return          FM_ERR_INVALID_PORT if port is invalid.
 * \return          FM_ERR_INVALID_ARGUMENT if unrecognized attribute.
 * \return          FM_ERR_INVALID_ATTRIB if read-only attribute.
 *
 *****************************************************************************/
fm_status fmGetLogicalPortAttribute(fm_int sw,
                                    fm_int port,
                                    fm_int attr,
                                    void * value)
{
    fm_status  err = FM_OK;
    fm_switch *switchPtr;

    VALIDATE_AND_PROTECT_SWITCH(sw);

    switchPtr = GET_SWITCH_PTR(sw);

    err = switchPtr->GetLogicalPortAttribute(sw, port, attr, value);

    UNPROTECT_SWITCH(sw);

    return err;

}   /* end fmGetLogicalPortAttribute */




/*****************************************************************************/
/** fmCreateLogicalPortForGlort
 * \ingroup intlport
 *
 * \desc            Creates a logical port for the given glort.  This function
 *                  is used when the switch must be able to address a glort
 *                  that exists on a remote switch. 
 *                                                                      \lb\lb
 *                  If a logical port has already been assigned to this glort, 
 *                  then this function will return the existing logical port 
 *                  instead of allocating a new one.
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \param[in]       glort is the glort to which the new logical port is to
 *                  be assigned.
 *
 * \param[out]      logicalPort is a pointer to caller-allocated memory where
 *                  this function should place the newly created logical
 *                  port.
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_INVALID_SWITCH if sw is invalid.
 * \return          FM_ERR_INVALID_ARGUMENT if logicalPort is NULL.
 * \return          FM_ERR_GLORT_IN_USE if the specified is already being used
 *                  by others.
 * \return          FM_ERR_NO_FORWARDING_RULES if there is no forwarding rule 
 *                  associated with glort.
 * \return          FM_ERR_NO_FREE_RESOURCES if no logical port numbers 
 *                  available for allocation.
 * \return          FM_ERR_NO_MEM if no memory available to store logical
 *                  port data structure.
 *
 *****************************************************************************/
fm_status fmCreateLogicalPortForGlort(fm_int    sw,
                                      fm_uint32 glort,
                                      fm_int *  logicalPort)
{
    fm_status  err;
    fm_switch *switchPtr;

    FM_LOG_ENTRY(FM_LOG_CAT_PORT,
                 "sw=%d, glort = 0x%x, logicalPort=%p\n",
                 sw,
                 glort,
                 (void *) logicalPort);

    switchPtr = GET_SWITCH_PTR(sw);

    if (glort != (fm_uint32) ~0)
    {
        /* See if logical port already exist for this glort */
        err = fmGetGlortLogicalPort(sw, glort, logicalPort);

        if (err == FM_OK)
        {
            /* Yes the logical port exists */
            FM_LOG_EXIT(FM_LOG_CAT_PORT, FM_OK);
        }
        else if (err != FM_ERR_INVALID_PORT)
        {
            FM_LOG_EXIT(FM_LOG_CAT_PORT, err);
        }
    }

    /* The logical port doesn't exist, so create a new one */
    FM_API_CALL_FAMILY(err, 
                       switchPtr->CreateLogicalPortForGlort,
                       sw,
                       glort,
                       logicalPort);

    FM_LOG_EXIT(FM_LOG_CAT_PORT, err);

}   /* end fmCreateLogicalPortForGlort */




/*****************************************************************************/
/** fmGetLogicalPortGlort
 * \ingroup intlport
 *
 * \desc            Retrieves the glort assigned to the given logical port.
 *
 * \note            This function assumes that the caller has taken the
 *                  appropriate lock and made sure switch state is valid.
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \param[in]       logicalPort is the port number. 
 *
 * \param[out]      glort points to caller-allocated storage where this
 *                  function should write the glort associated with 
 *                  logicalPort.
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_INVALID_SWITCH if sw is invalid.
 * \return          FM_ERR_INVALID_PORT if logicalPort is invalid.
 * \return          FM_ERR_INVALID_ARGUMENT if glort is NULL.
 *
 *****************************************************************************/
fm_status fmGetLogicalPortGlort(fm_int sw, fm_int logicalPort, fm_uint32 *glort)
{
    fm_port *  portPtr;
    fm_status  err;

    FM_LOG_ENTRY_VERBOSE(FM_LOG_CAT_PORT,
                         "sw=%d, logicalPort=%d, glort=%p\n",
                         sw,
                         logicalPort,
                         (void *) glort);

    if ( !fmIsValidPort(sw, logicalPort, ALLOW_ALL) )
    {
        *glort = ~0;
        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PORT, err = FM_ERR_INVALID_PORT);
    }

    portPtr = GET_PORT_PTR(sw, logicalPort);
    *glort  = portPtr->glort;
    err     = FM_OK;

ABORT:
    FM_LOG_EXIT_CUSTOM_VERBOSE(FM_LOG_CAT_PORT,
                               err,
                               "*glort = 0x%x\n",
                               *glort);

}   /* end fmGetLogicalPortGlort */



/*****************************************************************************/
/** fmIsLagPort
 * \ingroup intSwitch
 *
 * \desc            Determines whether a logical port is a LAG port.
 * 
 * \note            Assumes that the switch number is valid and the
 *                  appropriate lock has been taken.
 *
 * \param[in]       sw is the switch on which to operate.
 * 
 * \param[in]       port is the logical port number.
 *
 * \return          TRUE if the port is a LAG port, FALSE otherwise.
 *
 *****************************************************************************/
fm_bool fmIsLagPort(fm_int sw, fm_int port)
{
    fm_switch * switchPtr;
    fm_port * portPtr;

    switchPtr = GET_SWITCH_PTR(sw);
    portPtr = GET_SWITCH_PORT_PTR(switchPtr, port);

    return (portPtr && portPtr->portType == FM_PORT_TYPE_LAG);

}   /* end fmIsLagPort */




/*****************************************************************************/
/** fmIsRemotePort
 * \ingroup intlport
 *
 * \desc            Helper function to check where a logical port is a 
 *                  remote port on the switch.
 *
 * \note            This function assumes that the caller has taken the
 *                  appropriate lock and made sure switch state is valid.
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \param[in]       logicalPort is the logical port on which to operate.
 *
 * \return          TRUE if logical port is a remote port.
 * \return          FALSE if otherwise
 *
 *****************************************************************************/
fm_bool fmIsRemotePort(fm_int sw, fm_int logicalPort)
{
    fm_port *portPtr;

    portPtr = GET_PORT_PTR(sw, logicalPort);

    return (portPtr && portPtr->portType == FM_PORT_TYPE_REMOTE);

}   /* end fmIsRemotePort */




/*****************************************************************************/
/** fmIsMgmtPort
 * \ingroup intlport
 *
 * \desc            Helper function to check if a logical port is a
 *                  management port on the switch.
 *
 * \note            This function assumes that the caller has taken the
 *                  appropriate lock and made sure switch state is valid.
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \param[in]       logicalPort is the logical port on which to operate.
 *
 * \return          TRUE if logical port is a internal port.
 * \return          FALSE if otherwise
 *
 *****************************************************************************/
fm_bool fmIsMgmtPort(fm_int sw, fm_int logicalPort)
{
    fm_status err;
    fm_switch *switchPtr;
    fm_int     physSwitch;
    fm_int     physPort;

    switchPtr = GET_SWITCH_PTR(sw);

    err = fmPlatformMapLogicalPortToPhysical(sw, 
                                             logicalPort, 
                                             &physSwitch, 
                                             &physPort);
    FM_LOG_ASSERT(FM_LOG_CAT_PORT, 
                  err == FM_OK, 
                  "Could not convert the port %d (logical port) to "
                   "the coresponding physical port\n",
                  logicalPort);

    if (physPort < switchPtr->numMgmtPorts)
    {
        return TRUE;
    }
    else
    {
        return FALSE;
    }
    
}   /* end fmIsMgmtPort */




/*****************************************************************************/
/** fmIsInternalPort
 * \ingroup intlport
 *
 * \desc            Helper function to check where a logical port is an
 *                  internal port on the switch.
 *
 * \note            This function assumes that the caller has taken the
 *                  appropriate lock and made sure switch state is valid.
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \param[in]       logicalPort is the logical port on which to operate.
 *
 * \return          TRUE if logical port is a internal port.
 * \return          FALSE if otherwise
 *
 *****************************************************************************/
fm_bool fmIsInternalPort(fm_int sw, fm_int logicalPort)
{
    fm_port *portPtr;

    portPtr = GET_PORT_PTR(sw, logicalPort);

    return (portPtr && portPtr->attributes.internalPort);

}   /* end fmIsInternalPort */



/*****************************************************************************/
/** fmIsPortLinkUp
 * \ingroup intlport
 *
 * \desc            Helper function to check whether a logical port link is up.
 *
 * \note            This function assumes that the caller has taken the
 *                  appropriate lock and made sure switch state is valid.
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \param[in]       logicalPort is the logical port on which to operate.
 *
 * \return          TRUE if logical port link is up.
 * \return          FALSE if otherwise
 *
 *****************************************************************************/
fm_bool fmIsPortLinkUp(fm_int sw, fm_int logicalPort)
{
    fm_port *portPtr;

    portPtr = GET_PORT_PTR(sw, logicalPort);

    return (portPtr && portPtr->linkUp);

}   /* end fmIsPortLinkUp */




/*****************************************************************************/
/** fmIsValidPort
 * \ingroup intlport
 *
 * \desc            Determines whether a logical port is valid.
 *                  Logs a DEBUG message and returns FALSE if the port
 *                  does not meet the specified criteria.
 *
 * \note            This function assumes that the caller has taken the
 *                  appropriate lock and made sure the switch state is valid.
 *
 * \param[in]       sw is the switch to operate on.
 * 
 * \param[in]       port is the logical port number to be validated.
 *
 * \param[out]      mode is a bit mask indicating which logical port
 *                  types are considered valid.
 *
 * \return          TRUE if port is valid, FALSE otherwise.
 *
 *****************************************************************************/
fm_bool fmIsValidPort(fm_int sw, fm_int port, fm_int mode)
{
    fm_switch * switchPtr;
    fm_port   * portPtr;
    fm_int      maxPort;
    fm_int      minPort;

    switchPtr = GET_SWITCH_PTR(sw);

    maxPort =
        (mode & (ALLOW_LAG | ALLOW_REMOTE)) ?
        switchPtr->maxPort - 1 :
        switchPtr->cardinalPortInfo.maxLogicalPort;

    minPort = (mode & ALLOW_CPU) ? 0 : 1;

    if (port < minPort || port > maxPort)
    {
        FM_LOG_DEBUG(FM_LOG_CAT_PORT,
                     "port %d invalid, not in [%d, %d]\n",
                     port,
                     minPort,
                     maxPort);
        return FALSE;
    }

    portPtr = switchPtr->portTable[port];

    if (portPtr == NULL)
    {
        FM_LOG_DEBUG(FM_LOG_CAT_PORT,
                     "port %d invalid, not initialized\n",
                     port);
        return FALSE;
    }

    switch (portPtr->portType)
    {
        case FM_PORT_TYPE_LAG:
            if (mode & ALLOW_LAG)
            {
                return TRUE;
            }
            break;

        case FM_PORT_TYPE_REMOTE:
            if (mode & ALLOW_REMOTE)
            {
                return TRUE;
            }
            break;

        case FM_PORT_TYPE_CPU:
            if (mode & ALLOW_CPU)
            {
                return TRUE;
            }
            break;

        case FM_PORT_TYPE_PHYSICAL:
            return TRUE;

        default:
            if (mode == ALLOW_ALL)
            {
                return TRUE;
            }
            break;
    }

    FM_LOG_DEBUG(FM_LOG_CAT_PORT,
                 "%s port %d is not allowed\n",
                 fmPortTypeToText(portPtr->portType),
                 port);

    return FALSE;

}   /* end fmIsValidPort */




/*****************************************************************************/
/** fmSortPortByGlort
 * \ingroup intPort
 *
 * \desc            Sorts a list of logical ports by their glorts.
 *
 * \note            This function assumes that the caller has taken the
 *                  appropriate lock and made sure switch state is valid.
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \param[in]       ports is a list of logical port on which to operate.
 *
 * \param[in]       nPorts is a number of ports in the list.
 *
 * \param[out]      sortedPorts points to caller allocated array where the
 *                  sorted port list will be placed. This can be the same
 *                  memory as the input port list.
 *
 * \return          FM_OK if successful.
 *
 *****************************************************************************/
fm_status fmSortPortByGlort(fm_int sw,
                            fm_int *ports,
                            fm_int nPorts,
                            fm_int *sortedPorts)
{
    fmPortToGlort portToGlort[nPorts]; 
    fm_status     err;
    fm_int        i;

    FM_LOG_ENTRY(FM_LOG_CAT_PORT, "sw=%d nPorts=%d\n", sw, nPorts);

    for (i = 0 ; i < nPorts ; i++)
    {
        portToGlort[i].logicalPort = ports[i];

        err = fmGetLogicalPortGlort(sw, ports[i], &portToGlort[i].glort);
        FM_LOG_EXIT_ON_ERR(FM_LOG_CAT_PORT, err);
    }

    qsort(portToGlort, nPorts, sizeof(fmPortToGlort), CompareByGlort);

    for (i = 0 ; i < nPorts ; i++)
    {
        sortedPorts[i] = portToGlort[i].logicalPort;
    }

    FM_LOG_EXIT(FM_LOG_CAT_PORT, FM_OK);

}   /* end fmSortPortByGlort */




/*****************************************************************************/
/** fmGetGlortLogicalPort
 * \ingroup intPort
 *
 * \desc            Find the logical port for a given glort.
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \param[in]       glort is the glort on which to operate.
 *
 * \param[out]      logicalPort points to caller-allocated storage where this
 *                  function should store the logical port associated with
 *                  the glort.
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_INVALID_PORT if the port is not found.
 * \return          FM_ERR_INVALID_ARGUMENT if the glort is invalid.
 *
 *****************************************************************************/
fm_status fmGetGlortLogicalPort(fm_int sw, fm_uint32 glort, fm_int *logicalPort)
{
    fm_status  err;
    fm_port *  portPtr;
    fm_int     i;

    FM_LOG_ENTRY(FM_LOG_CAT_PORT, "sw=%d glort=0x%x\n", sw, glort);

    if (glort == (fm_uint32) ~0)
    {
        FM_LOG_EXIT(FM_LOG_CAT_PORT, FM_ERR_INVALID_ARGUMENT);
    }

    for (i = 0 ; i < FM_MAX_LOGICAL_PORT ; i++)
    {
        portPtr = GET_PORT_PTR(sw, i);

        if (portPtr != NULL)
        {
            if (portPtr->glort == glort)
            {
                /* portNumber could point to different port than entry
                 * For LAG member port, it points to remote or physical
                 */
                *logicalPort = portPtr->portNumber;
                 FM_LOG_EXIT(FM_LOG_CAT_PORT, FM_OK);
            }
        }
    }

    err = FM_ERR_INVALID_PORT;

    FM_LOG_EXIT(FM_LOG_CAT_PORT, err);

}   /* end fmGetGlortLogicalPort */




/*****************************************************************************/
/** fmAllocDestEntries
 * \ingroup intPort
 *
 * \desc            Allocates free dest table entries in a contiguous block.
 *
 * \param[in]       sw is the switch number.
 *
 * \param[in]       numDestEntries is the number of entries to allocate.
 *
 * \param[in]       camEntry is the glortCamEntry to set for the dest entries.
 *
 * \param[out]      destEntry is a pre-allocated array to store the allocated
 *                  entries.
 *
 * \param[in]       type is the allocation type (see ''fm_portType'').
 *
 * \return          FM_OK if successful.
 *                  FM_ERR_LPORT_DESTS_UNAVAILABLE if no more dest entries are
 *                  available.
 *
 *****************************************************************************/
fm_status fmAllocDestEntries(fm_int              sw,
                             fm_int              numDestEntries,
                             fm_glortCamEntry *  camEntry,
                             fm_glortDestEntry **destEntry,
                             fm_portType         type)
{
    fm_switch *         switchPtr;
    fm_logicalPortInfo *lportInfo;
    fm_int              firstDestEntry;
    fm_int              i;

    FM_LOG_ENTRY(FM_LOG_CAT_PORT,
                 "sw=%d numDestEntries=%d destEntry=%p\n",
                 sw, 
                 numDestEntries, 
                 (void *) destEntry);

    switchPtr = GET_SWITCH_PTR(sw);
    lportInfo = &switchPtr->logicalPortInfo;

    firstDestEntry = fmFindUnusedDestEntries(sw, numDestEntries, 0);
    if (firstDestEntry == -1)
    {
        FM_LOG_EXIT(FM_LOG_CAT_PORT, FM_ERR_LPORT_DESTS_UNAVAILABLE);
    }

    for (i = 0 ; i < numDestEntries ; ++i)
    {
        fm_glortDestEntry *currentDestEntry;

        currentDestEntry = &lportInfo->destEntries[firstDestEntry + i];
        memset(currentDestEntry, 0, sizeof(*currentDestEntry));

        currentDestEntry->destIndex = firstDestEntry + i;
        currentDestEntry->owner     = camEntry;
        currentDestEntry->usedBy    = FM_USED_BY_TYPE(type);

        *destEntry++ = currentDestEntry;
    }

    FM_LOG_EXIT(FM_LOG_CAT_PORT, FM_OK);

}   /* end fmAllocDestEntries */




/*****************************************************************************/
/** fmCreateGlortCamEntry
 * \ingroup intPort
 *
 * \desc            Writes an entry to the glort CAM/RAM.
 * 
 * Note             Assumes switch protection lock has already been acquired
 *                  and that the switch pointer is valid.
 *
 * \param[in]       sw is the switch number.
 *
 * \param[in]       camMask is the mask value to write into the glort CAM.
 *
 * \param[in]       camKey is the key value to write into the glort CAM.
 *
 * \param[in]       strict indicates whether dest table index is hashed
 *                  or not.
 *
 * \param[in]       baseIndex is the starting index used to compute the dest
 *                  index.
 *
 * \param[in]       destCount is the number of indices the hash is computed over.
 *
 * \param[in]       rangeALength is the size of a bitslice of the glort number
 *                  used to calculate the final index.
 *
 * \param[in]       rangeAOffset is the start bit of a bitslice of the glort
 *                  number used to calculate the final index.
 *
 * \param[in]       rangeBLength is the size of a bitslice of the glort number
 *                  used to calculate the final index in strict mode.
 *
 * \param[in]       rangeBOffset is the start bit of a bitslice of the glort
 *                  number used to calculate the final index in strict mode.
 *
 * \param[in]       hashRotation specifies a hash parameter.
 *
 * \param[in]       dglortTag is a one-bit tag associated with the DGLORT.
 *                  It may be used as a key by L2AR. (FM6000 only)
 *
 * \param[out]      camIndexPtr points to caller-allocated storage where this
 *                  function should place the CAM entry number.
 *
 * \return          FM_OK if successful.
 *
 *****************************************************************************/
fm_status fmCreateGlortCamEntry(fm_int      sw,
                                fm_uint32   camMask,
                                fm_uint32   camKey,
                                fm_uint32   strict,
                                fm_uint32   baseIndex,
                                fm_uint32   destCount,
                                fm_uint32   rangeALength,
                                fm_uint32   rangeAOffset,
                                fm_uint32   rangeBLength,
                                fm_uint32   rangeBOffset,
                                fm_uint32   hashRotation,
                                fm_uint32   dglortTag,
                                fm_uint32 * camIndexPtr)
{
    fm_status           err = FM_OK;
    fm_switch *         switchPtr;
    fm_logicalPortInfo *lportInfo;
    fm_glortCamEntry *  camEntryPtr;
    fm_int              camIndex;

    FM_LOG_ENTRY(FM_LOG_CAT_PORT,
                 "sw=%d camMask=0x%04x camKey=0x%04x strict=%d "
                 "destIndex=%d count=%d A=%d(len=%d) B=%d(len=%d) hash=%d\n",
                 sw,
                 camMask,
                 camKey,
                 strict,
                 baseIndex,
                 destCount,
                 rangeAOffset,
                 rangeALength,
                 rangeBOffset,
                 rangeBLength,
                 hashRotation);

    switchPtr = GET_SWITCH_PTR(sw);
    lportInfo = &switchPtr->logicalPortInfo;

    /***************************************************
     * Find an unused CAM entry.
     **************************************************/
    camIndex = fmFindUnusedCamEntry(sw);
    if (camIndex == -1)
    {
        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PORT, err = FM_ERR_LPORT_CAM_UNAVAILABLE);
    }

    FM_LOG_DEBUG(FM_LOG_CAT_PORT,
                 "Using free CAM entry at index %d\n",
                 camIndex);

    camEntryPtr = &lportInfo->camEntries[camIndex];

    /***************************************************
     * Fill in the software table.
     **************************************************/

    camEntryPtr->camIndex     = camIndex;
    camEntryPtr->camKey       = camKey;
    camEntryPtr->camMask      = camMask;
    camEntryPtr->strict       = strict;
    camEntryPtr->destIndex    = baseIndex;
    camEntryPtr->destCount    = destCount;
    camEntryPtr->rangeAOffset = rangeAOffset;
    camEntryPtr->rangeALength = rangeALength;
    camEntryPtr->rangeBOffset = rangeBOffset;
    camEntryPtr->rangeBLength = rangeBLength;
    camEntryPtr->hashRotation = hashRotation;
    camEntryPtr->dglortTag    = dglortTag;
    camEntryPtr->useCount     = -1; /* reserved */

    /**************************************************
     * Write CAM & RAM
     **************************************************/

    FM_API_CALL_FAMILY(err,
                       switchPtr->WriteGlortCamEntry,
                       sw,
                       camEntryPtr,
                       FM_UPDATE_CAM_AND_RAM);

ABORT:
    *camIndexPtr = camIndex;

    FM_LOG_EXIT(FM_LOG_CAT_PORT, err);

}   /* end fmCreateGlortCamEntry */




/*****************************************************************************/
/** fmAllocateMcastHandles
 * \ingroup intPort
 *
 * \desc            Allocates multicast resources given a glort range. This
 *                  function is used by stacking and non-stacking configuration.
 *                  In non-stacking configuration, the resources are
 *                  automatically selected, whereas in stacking, it is
 *                  specified by the handle.
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \param[in]       startGlort is the start glort to reserve for multicast.
 *
 * \param[in]       glortSize is the size of the glort space to reserve. This
 *                  value must be a power of two.
 *
 * \param[out]      baseMcastGroupHandle points to caller-allocated storage
 *                  where this function should store base handle for this
 *                  resource space.
 *
 * \param[out]      numMcastGroups points to caller-allocated storage where this
 *                  function should store the number of handles from the base,
 *                  where the caller can use to create multicast groups.
 *
 * \param[out]      step points to caller-allocated storage where this function
 *                  should store the value by which baseMcastGroupHandle
 *                  should be incremented in order to get the next resource
 *                  handle.
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_INVALID_ARGUMENT if input parameters fail checking.
 * \return          FM_ERR_LOG_PORT_UNAVAILABLE if any glort in the given
 *                  glort range is being used.
 * \return          FM_ERR_NO_MCAST_RESOURCES if no more resources are available
 *
 *****************************************************************************/
fm_status fmAllocateMcastHandles(fm_int  sw,
                                 fm_uint startGlort,
                                 fm_int  glortSize,
                                 fm_int *baseMcastGroupHandle,
                                 fm_int *numMcastGroups,
                                 fm_int *step)
{
    fm_status           err;
    fm_switch *         switchPtr;
    fm_logicalPortInfo *lportInfo;
    fm_int              i;
    fm_int              allocIndex;
    fm_int              cnt;
    fm_int              camLen;
    fm_int              steps;
    fm_uint             glort;
    fm_uint             endGlort;
    fm_int              port;
    fm_int              endPort;
    fm_mcgAllocEntry *  allocEntry;

    FM_LOG_ENTRY(FM_LOG_CAT_PORT,
                 "sw=%d startGlort=0x%x glortSize=0x%x "
                 "baseMcastGroupHandle=%p numMcastGroups=%p\n",
                 sw, 
                 startGlort, 
                 glortSize, 
                 (void *) baseMcastGroupHandle,
                 (void *) numMcastGroups);

    switchPtr = GET_SWITCH_PTR(sw);
    lportInfo = &switchPtr->logicalPortInfo;

    if ( fmVerifyGlortRange(startGlort, glortSize) != FM_OK )
    {
        FM_LOG_EXIT(FM_LOG_CAT_PORT, FM_ERR_INVALID_ARGUMENT);
    } 

    camLen = 0;
    for (i = 0 ; i < 32 ; i++)
    {
        if (glortSize & (1 << i))
        {
            camLen = i;
        }
    }

    if (camLen > switchPtr->mcastMaxEntryPerCam)
    {
        steps = switchPtr->mcastMaxEntryPerCam;
    }
    else
    {
        steps = camLen;
    }

    if (startGlort % (1 << steps))
    {
        /* glort is not multiple of size */
        FM_LOG_DEBUG(FM_LOG_CAT_PORT,
                     "startGlort %d is not a multiple of size %d\n",
                     startGlort,
                     (1 << steps));

        FM_LOG_EXIT(FM_LOG_CAT_PORT, FM_ERR_INVALID_ARGUMENT);
    }

    if ( glortSize > switchPtr->mcastMaxDestTableEntries )
    {
        /* glortSize is larger than allowable size to store in array */

        /* This will guarantee that the code below won't overflow
         * the mcastCamEntries array when creating CAM entries
         */
        FM_LOG_EXIT(FM_LOG_CAT_PORT, FM_ERR_INVALID_ARGUMENT);
    }

    endGlort = startGlort + glortSize;

    if (endGlort > FM_MAX_GLORT)
    {
        FM_LOG_EXIT(FM_LOG_CAT_PORT, FM_FAIL);
    }

    /* Check for glort range is not in use or reserved. */
    for ( glort = startGlort ; glort < endGlort ; ++glort )
    {
        if (FM_IS_GLORT_TAKEN(lportInfo, glort))
        {
            FM_LOG_EXIT(FM_LOG_CAT_PORT, FM_ERR_LOG_PORT_UNAVAILABLE);
        }
    }

    /* Find a free reserved entry */
    allocIndex = fmFindFreeMcgEntry(sw);
    if (allocIndex < 0)
    {
        FM_LOG_EXIT(FM_LOG_CAT_PORT, FM_ERR_NO_MCAST_RESOURCES);
    }

    allocEntry = &lportInfo->mcgAllocTable[allocIndex];

    /* Find a free logical port range */
    allocEntry->baseHandle = fmFindUnusedLogicalPorts(sw, glortSize);
    if (allocEntry->baseHandle == -1)
    {
        FM_LOG_EXIT(FM_LOG_CAT_PORT, FM_ERR_LOG_PORT_UNAVAILABLE);
    }

    /* create the cam entries */
    for ( glort = startGlort, cnt = 0 ;
          glort < endGlort ;
          glort += switchPtr->mcastMaxEntryPerGlort, cnt++ )
    {
        /* destination entries are allocated on demand
         * since this is a limited resources even more
         * so than CAM entries
         */
        allocEntry->mcgDestIndex[cnt] = 0;
        err = fmCreateGlortCamEntry(sw,
                                    ~((1 << steps) - 1),    /* cam mask */
                                    glort,
                                    FM_GLORT_ENTRY_TYPE_ISL,
                                    allocEntry->mcgDestIndex[cnt],
                                    1,                      /* hash count */
                                    steps,                  /* A length */
                                    0,                      /* A offset */
                                    0,                      /* B length */
                                    0,                      /* B length */
                                    FM_GLORT_ENTRY_HASH_A,
                                    0,                      /* dglortTag */
                                    &allocEntry->mcgCamIndex[cnt]);
        if (err != FM_OK)
        {
            /* Free the already allocated cam resources */
            cnt--;
            while (cnt >= 0)
            {
                fmRemoveGlortCamEntry(sw, allocEntry->mcgCamIndex[cnt]);
                cnt--;
            }
            allocEntry->glortSize = 0;
            FM_LOG_EXIT(FM_LOG_CAT_PORT, err);
        }

        /* Set the use count to one here so it can be freed properly */
        lportInfo->camEntries[allocEntry->mcgCamIndex[cnt]].useCount = 1;

        FM_LOG_DEBUG(FM_LOG_CAT_PORT | FM_LOG_CAT_MULTICAST,
                     "AllocMcastIndex#%d allocCamIndex#%u camIndex %u "
                     "glort 0x%x mask 0x%x steps %u baseHandle %d\n",
                     allocIndex,
                     cnt,
                     allocEntry->mcgCamIndex[cnt],
                     glort,
                     ~((1 << steps) - 1),
                     steps,
                     allocEntry->baseHandle);
     }

    allocEntry->baseGlort = startGlort;
    allocEntry->glortSize = glortSize;

    /* One glort per mcast entries */
    allocEntry->numHandles = glortSize;

    endPort = allocEntry->baseHandle + allocEntry->numHandles;
    err = FM_OK;

    FM_LOG_DEBUG(FM_LOG_CAT_PORT | FM_LOG_CAT_MULTICAST,
                 "Reserve port (%d -> %d) and "
                 "glort (0x%x -> 0x%x) for Multicast\n",
                 allocEntry->baseHandle,
                 endPort - 1,
                 startGlort,
                 endGlort - 1);

    /* Reserve the logical ports */
    for ( port = allocEntry->baseHandle ; port < endPort ; ++port )
    {
        if (FM_IS_LPORT_TAKEN(lportInfo, port))
        {
            /* Probably improperly locking causes this problem */
            FM_LOG_FATAL(FM_LOG_CAT_PORT,
                         "Port %u should be available, but state is %d\n",
                         port,
                         lportInfo->lportState[port]);
            err = FM_ERR_PORT_IN_USE;
        }
        FM_RESERVE_LPORT_MCG(lportInfo, port);
    }

    /* Reserve the glort */
    for ( glort = startGlort ; glort < endGlort ; ++glort )
    {
        if (FM_IS_GLORT_TAKEN(lportInfo, glort))
        {
            /* Probably improperly locking causes this problem */
            FM_LOG_FATAL(FM_LOG_CAT_PORT,
                         "Glort 0x%x should be available, but state is %d\n",
                         glort,
                         lportInfo->glortState[glort]);
            err = FM_ERR_PORT_IN_USE;
        }
        FM_RESERVE_GLORT_MCG(lportInfo, glort);
    }

    *baseMcastGroupHandle = allocEntry->baseHandle;
    *numMcastGroups       = allocEntry->numHandles;
    *step                 = 1;

    FM_LOG_EXIT(FM_LOG_CAT_PORT, err);

}   /* end fmAllocateMcastHandles */




/*****************************************************************************/
/** fmFreeMcastHandles
 * \ingroup intPort
 *
 * \desc            Frees the multicast resources allocated by
 *                  ''fmAllocateMcastHandles''.
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \param[in]       handle is the base handle returned from
 *                  ''fm6000AllocateMcastResources''.
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_INVALID_MULTICAST_GROUP if handle is not found
 *                  in allocated entries.
 * \return          FM_ERR_PORT_IN_USE if any port resources are still being
 *                  used.
 *
 *****************************************************************************/
fm_status fmFreeMcastHandles(fm_int sw, fm_int handle)
{
    fm_status           err;
    fm_status           retErr = FM_OK;
    fm_switch *         switchPtr;
    fm_logicalPortInfo *lportInfo;
    fm_mcgAllocEntry *  allocEntry;
    fm_uint32           glort;
    fm_uint             i;
    fm_uint             cnt;
    fm_int              j;

    FM_LOG_ENTRY(FM_LOG_CAT_PORT, "sw=%d handle=%d\n", sw, handle);

    switchPtr = GET_SWITCH_PTR(sw);
    lportInfo = &switchPtr->logicalPortInfo;

    /* Find an entry based on a specified handle */
    allocEntry = fmFindMcgEntryByHandle(sw, handle);
    if (!allocEntry)
    {
        /* Incorrect handle passed in */
        FM_LOG_EXIT(FM_LOG_CAT_PORT, FM_ERR_INVALID_MULTICAST_GROUP);
    }

    for (cnt = 0 ; cnt < allocEntry->glortSize ; cnt++)
    {
        if (!FM_IS_GLORT_MCG_FREE(lportInfo, allocEntry->baseGlort+cnt))
        {
            /* Cannot delete when any port in the range is in use */
            /* This is one to one corresponding to logical port */
            FM_LOG_EXIT(FM_LOG_CAT_PORT, FM_ERR_PORT_IN_USE);
        }
    }

    /* Now free the cam and associated dest entries */
    cnt = allocEntry->glortSize / switchPtr->mcastMaxEntryPerGlort;

    /* At least there is one entry */
    if (!cnt)
    {
        cnt = 1;
    }

    for (i = 0 ; i < cnt ; i++ )
    {
        /* Free the cam entries */
        err = fmRemoveGlortCamEntry(sw, allocEntry->mcgCamIndex[i]); 
        if (err != FM_OK)
        {
            FM_LOG_ERROR(FM_LOG_CAT_PORT,
                         "Unable to free cam entry %d\n",
                         allocEntry->mcgCamIndex[i]);
        }
        
        /* Free dest entries if reserved */
        if (allocEntry->mcgDestIndex[i])
        {
            fm_uint destIndex;
            fm_uint off;
            fm_uint numDestEntries;

            destIndex = allocEntry->mcgDestIndex[i];
            if ( (i * switchPtr->mcastMaxEntryPerGlort) <= allocEntry->glortSize )
            {
                 numDestEntries = switchPtr->mcastMaxEntryPerGlort;
            }
            else
            {
                /* Get the remaining */
                numDestEntries = 
                    allocEntry->glortSize % switchPtr->mcastMaxEntryPerGlort;
            }

            for (off = 0 ; off < numDestEntries ; off++)
            {
                /* Free destination entries */
                FM_API_CALL_FAMILY(err,
                                   switchPtr->FreeDestEntry,
                                   sw,
                                   &lportInfo->destEntries[destIndex + off]);

                if (err != FM_OK)
                {
                    FM_LOG_ERROR(FM_LOG_CAT_PORT,
                                 "Unable to free dest entry %d\n",
                                 destIndex + off);
                    /* Save the first error to return */
                    if (retErr == FM_OK)
                    {
                        retErr = err;
                    }
                }
            }
        }
    }

    /* un-reserve the logical ports */
    for ( j = allocEntry->baseHandle ;
          j < (allocEntry->baseHandle + allocEntry->numHandles) ;
          j++ )
    {
        if (!FM_IS_LPORT_MCG_FREE(lportInfo, j))
        {
            /* Probably something is not cleanup properly */
            FM_LOG_FATAL(FM_LOG_CAT_PORT,
                         "Port %u should be reserved, but state is %d.\n",
                         j,
                         lportInfo->lportState[j]);
        }
        /* Now any one can use this */
        FM_RELEASE_LPORT(lportInfo, j);
    }

    /* Set the glort to free */
    for (cnt = 0 ; cnt < allocEntry->glortSize ; cnt++)
    {
        glort = allocEntry->baseGlort+cnt;
        if (!FM_IS_GLORT_MCG_FREE(lportInfo, glort))
        {
            FM_LOG_FATAL(FM_LOG_CAT_PORT,
                         "Glort 0x%x should be reserved, but state is %d.\n",
                         glort,
                         lportInfo->glortState[glort]);
            /* Save the first error to return */
            if (retErr == FM_OK)
            {
                retErr = FM_ERR_PORT_IN_USE;
            }
        }
        FM_RELEASE_GLORT(lportInfo, glort);
    }
    
    /* Set this entry to not-used */
    allocEntry->glortSize = 0;

    FM_LOG_EXIT(FM_LOG_CAT_PORT, retErr);

}   /* end fmFreeMcastHandles */ 




/*****************************************************************************/
/** fmAllocateLagHandles
 * \ingroup intPort
 *
 * \desc            Allocates LAG resources given a glort range. This
 *                  function could be used by stacking and non-stacking
 *                  configuration, but it is actually used by stacking mode
 *                  only.
 *                  In non-stacking configuration, the resources are
 *                  automatically selected, whereas in stacking, it is
 *                  specified by the handle.
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \param[in]       startGlort is the start glort to reserve for multicast.
 *
 * \param[in]       glortSize is the size of the glort space to reserve. This
 *                  value must be a power of two.
 *
 * \param[in]       glortsPerLag is the unit size (number of glorts at a time)
 *                  in which glorts are to be assigned to a lag.
 *
 * \param[in]       lagMaskSize is the width of the LAG mask, in bits.
 *
 * \param[out]      baseLagHandle points to caller-allocated storage where this
 *                  function should store base handle for this resource space.
 *
 * \param[out]      numHandles points to caller-allocated storage where this
 *                  function should store the number of handles from the base,
 *                  where the caller can use to create LAGs.
 *
 * \param[out]      step points to caller-allocated storage where this function
 *                  should store the value by which baseLagHandle should be
 *                  incremented in order to get the next resource handle.
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_INVALID_ARGUMENT if input parameters fail checking.
 * \return          FM_ERR_LOG_PORT_UNAVAILABLE if any glort in the given
 *                  glort range is being used.
 * \return          FM_ERR_NO_LAG_RESOURCES if no more resources are available
 *
 *****************************************************************************/
fm_status fmAllocateLagHandles(fm_int  sw,
                               fm_uint startGlort,
                               fm_int  glortSize,
                               fm_int  glortsPerLag,
                               fm_int  lagMaskSize,
                               fm_int *baseLagHandle,
                               fm_int *numHandles,
                               fm_int *step)
{
    fm_status           err;
    fm_switch *         switchPtr;
    fm_logicalPortInfo *lportInfo;
    fm_int              allocIndex;
    fm_uint             glort;
    fm_uint             endGlort;
    fm_int              port;
    fm_int              endPort;
    fm_lagAllocEntry *  allocEntry;

    FM_LOG_ENTRY(FM_LOG_CAT_PORT,
                 "sw=%d startGlort=0x%x glortSize=0x%x "
                 "baseLagHandle=%p numHandles=%p\n",
                 sw, 
                 startGlort, 
                 glortSize, 
                 (void *) baseLagHandle,
                 (void *) numHandles);

    switchPtr     = fmRootApi->fmSwitchStateTable[sw];
    lportInfo     = &switchPtr->logicalPortInfo;

    if ( fmVerifyGlortRange(startGlort, glortSize) != FM_OK )
    {
        FM_LOG_EXIT(FM_LOG_CAT_PORT, FM_ERR_INVALID_ARGUMENT);
    }

    if (glortSize % glortsPerLag)
    {
        /* glortSize is not multiple of glortsPerLag */
        FM_LOG_DEBUG(FM_LOG_CAT_PORT,
                     "glortSize %d is not a multiple of size %d\n",
                     glortSize,
                     glortsPerLag);

        FM_LOG_EXIT(FM_LOG_CAT_PORT, FM_ERR_INVALID_ARGUMENT);
    }

    if (startGlort % glortsPerLag)
    {
        /* glort is not multiple of glortsPerLag */
        FM_LOG_DEBUG(
            FM_LOG_CAT_PORT,
            "startGlort %d is not a multiple of size %d\n",
            startGlort,
            glortsPerLag);

        FM_LOG_EXIT(FM_LOG_CAT_PORT, FM_ERR_INVALID_ARGUMENT);
    }

    endGlort = startGlort + glortSize;

    if (endGlort > FM_MAX_GLORT)
    {
        FM_LOG_EXIT(FM_LOG_CAT_PORT, FM_FAIL);
    }

    /* Check for glort range is not in use */
    for (glort = startGlort ; glort < endGlort ; ++glort)
    {
        if (FM_IS_GLORT_TAKEN(lportInfo, glort))
        {
            FM_LOG_EXIT(FM_LOG_CAT_PORT, FM_ERR_LOG_PORT_UNAVAILABLE);
        }
    }

    /* Find a free reserved entry */
    allocIndex = fmFindFreeLagEntry(sw);
    if (allocIndex < 0)
    {
        FM_LOG_EXIT(FM_LOG_CAT_PORT, FM_ERR_NO_LAG_RESOURCES);
    }

    allocEntry = &lportInfo->lagAllocTable[allocIndex];

    /* Find a free logical port range */
    allocEntry->baseHandle = fmFindUnusedLogicalPorts(sw, glortSize);
    if (allocEntry->baseHandle == -1)
    {
        FM_LOG_EXIT(FM_LOG_CAT_PORT, FM_ERR_LOG_PORT_UNAVAILABLE);
    }

    /* Create the LAG canonical CAM entries */
    FM_API_CALL_FAMILY(err, 
                       switchPtr->CreateCanonicalCamEntries,
                       sw,
                       startGlort,
                       glortSize,
                       lagMaskSize);
    FM_LOG_EXIT_ON_ERR(FM_LOG_CAT_PORT, err);

    allocEntry->baseGlort  = startGlort;
    allocEntry->glortSize  = glortSize;
    allocEntry->numPorts   = glortSize;
    allocEntry->numHandles = allocEntry->numPorts / glortsPerLag;

    endPort = allocEntry->baseHandle + allocEntry->numPorts;
    err = FM_OK;

    FM_LOG_DEBUG(
        FM_LOG_CAT_PORT | FM_LOG_CAT_LAG,
        "Reserve port (%d -> %d) and glort (0x%x -> 0x%x) for LAG\n",
        allocEntry->baseHandle,
        endPort - 1,
        startGlort,
        endGlort - 1);

    /* Reserve the logical ports */
    for ( port = allocEntry->baseHandle ; port < endPort ; ++port )
    {
        if (FM_IS_LPORT_TAKEN(lportInfo, port))
        {
            /* Probably improperly locking causes this problem */
            FM_LOG_FATAL(FM_LOG_CAT_PORT,
                         "Port %u should be available, but state is %d\n",
                         port,
                         lportInfo->lportState[port]);
            err = FM_ERR_PORT_IN_USE;
        }
        FM_RESERVE_LPORT_LAG(lportInfo, port);
    }

    /* Reserve the glort */
    for ( glort = startGlort ; glort < endGlort ; ++glort )
    {
        if (FM_IS_GLORT_TAKEN(lportInfo, glort))
        {
            /* Probably improperly locking causes this problem */
            FM_LOG_FATAL(FM_LOG_CAT_PORT,
                         "Glort 0x%x should be available, but state is %d\n",
                         glort,
                         lportInfo->glortState[glort]);
            err = FM_ERR_PORT_IN_USE;
        }
        FM_RESERVE_GLORT_LAG(lportInfo, glort);
    }

    *baseLagHandle = allocEntry->baseHandle;
    *numHandles    = allocEntry->numHandles;
    *step          = glortsPerLag;

    FM_LOG_EXIT(FM_LOG_CAT_PORT, err);

}   /* end fmAllocateLagHandles */




/*****************************************************************************/
/** fmFreeLagHandles
 * \ingroup intPort
 *
 * \desc            Frees the LAG resources allocated by
 *                  ''fmAllocateLagHandles''.
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \param[in]       handle is the base handle returned from
 *                  ''fmAllocateLagResources''.
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_INVALID_LAG if handle is not found
 *                  in allocated entries.
 * \return          FM_ERR_PORT_IN_USE if any port resources are still being
 *                  used.
 *
 *****************************************************************************/
fm_status fmFreeLagHandles(fm_int sw, fm_int handle)
{
    fm_status                   err = FM_OK;
    fm_switch *                 switchPtr;
    fm_logicalPortInfo *        lportInfo;
    fm_uint32                   glort, endGlort;
    fm_int                      port, endPort;
    fm_lagAllocEntry *          allocEntry;

    FM_LOG_ENTRY(FM_LOG_CAT_PORT,
                 "sw=%d handle=%d\n",
                 sw, handle);

    switchPtr     = fmRootApi->fmSwitchStateTable[sw];
    lportInfo     = &switchPtr->logicalPortInfo;

    /* Find an entry based on a specified handle */
    allocEntry = fmFindLagEntryByHandle(sw, handle);
    if (!allocEntry)
    {
        /* Incorrect handle passed in */
        FM_LOG_EXIT(FM_LOG_CAT_PORT, FM_ERR_INVALID_LAG);
    }

    endGlort = allocEntry->baseGlort + allocEntry->glortSize;
    endPort = allocEntry->baseHandle + allocEntry->numPorts;

    for ( glort = allocEntry->baseGlort ; glort < endGlort ; glort++ )
    {
        if (!FM_IS_GLORT_LAG_FREE(lportInfo, glort) &&
            !FM_IS_GLORT_FREE_PEND(lportInfo, glort))
        {
            /* Cannot delete when any port in the range is in use */
            /* This is one to one corresponding to logical port */
            FM_LOG_EXIT(FM_LOG_CAT_PORT, FM_ERR_PORT_IN_USE);
        }
    }

    /* Delete the LAG canonical CAM entries */
    FM_API_CALL_FAMILY(err, 
                       switchPtr->DeleteCanonicalCamEntries,
                       sw,
                       allocEntry->baseGlort,
                       allocEntry->glortSize);
    FM_LOG_EXIT_ON_ERR(FM_LOG_CAT_PORT, err);

    /* un-reserve the logical ports */
    for ( port = allocEntry->baseHandle ; port < endPort ; ++port )
    {
        if (!FM_IS_LPORT_LAG_FREE(lportInfo, port) &&
            !FM_IS_LPORT_FREE_PEND(lportInfo, port) )
        {
             FM_LOG_FATAL(FM_LOG_CAT_PORT,
                          "Port %u should be reserved, but state is %d.\n",
                          port,
                          lportInfo->lportState[port]);
            err = FM_ERR_PORT_IN_USE;
        }

        /* Now any one can use this */
        FM_RELEASE_LPORT(lportInfo, port);
    }

    /* Set the glorts to free */
    for ( glort = allocEntry->baseGlort ; glort < endGlort ; glort++ )
    {
        if (!FM_IS_GLORT_LAG_FREE(lportInfo, glort) &&
            !FM_IS_GLORT_FREE_PEND(lportInfo, glort))
        {
            FM_LOG_FATAL(FM_LOG_CAT_PORT,
                         "Glort 0x%x should be reserved, but state is %d.\n",
                         glort,
                         lportInfo->glortState[glort]);
            err = FM_ERR_PORT_IN_USE;
        }
        FM_RELEASE_GLORT(lportInfo, glort);
    }
    
    /* Set this entry to not-used */
    allocEntry->glortSize = 0;

    FM_LOG_EXIT(FM_LOG_CAT_PORT, err);

}   /* end fmFreeLagHandles */




/*****************************************************************************/
/** fmAllocateLbgHandles
 * \ingroup intPort
 *
 * \desc            Allocates LBG resources given a glort range. This
 *                  function is used by stacking and non-stacking configuration.
 *                  In non-stacking configuration, the resources are
 *                  automatically selected, whereas in stacking, it is
 *                  specified by the handle.
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \param[in]       startGlort is the start glort to reserve for multicast.
 *
 * \param[in]       glortSize is the size of the glort space to reserve. This
 *                  value must be a power of two.
 *
 * \param[out]      baseLbgHandle points to caller-allocated storage where this
 *                  function should store base handle for this resource space.
 *
 * \param[out]      numHandles points to caller-allocated storage where this
 *                  function should store the number of handles from the base,
 *                  where the caller can use to create LBGs.
 *
 * \param[out]      step points to caller-allocated storage where this
 *                  function should store the incremental value to get next LBG.
 *
 * \param[out]      step points to caller-allocated storage where this function
 *                  should store the value by which baseMcastGroupHandle
 *                  should be incremented in order to get the next resource
 *                  handle.
 * 
 * \return          FM_OK if successful.
 * \return          FM_ERR_INVALID_ARGUMENT if input parameters fail checking.
 * \return          FM_ERR_LOG_PORT_UNAVAILABLE if any glort in the given
 *                  glort range is being used.
 * \return          FM_ERR_NO_LBG_RESOURCES if no more resources are available
 *
 *****************************************************************************/
fm_status fmAllocateLbgHandles(fm_int  sw,
                               fm_uint startGlort,
                               fm_int  glortSize,
                               fm_int *baseLbgHandle,
                               fm_int *numHandles,
                               fm_int *step)
{
    fm_status           err;
    fm_switch *         switchPtr;
    fm_logicalPortInfo *lportInfo;
    fm_int              allocIndex;
    fm_int              port, endPort;
    fm_uint32           glort, endGlort;
    fm_int              glortsPerLbg;
    fm_int              unused;
    fm_lbgAllocEntry   *allocEntry;

    FM_LOG_ENTRY(FM_LOG_CAT_PORT,
                 "sw=%d startGlort=0x%x glortSize=0x%x "
                 "baseLbgHandle=%p numHandles=%p step=%p\n",
                 sw, startGlort, glortSize, 
                 (void *) baseLbgHandle,
                 (void *) numHandles,
                 (void *) step);

    switchPtr     = fmRootApi->fmSwitchStateTable[sw];
    lportInfo     = &switchPtr->logicalPortInfo;

    if ( fmVerifyGlortRange(startGlort, glortSize) != FM_OK )
    {
        FM_LOG_EXIT(FM_LOG_CAT_PORT, FM_ERR_INVALID_ARGUMENT);
    }

    FM_API_CALL_FAMILY(err, 
                       switchPtr->GetPortParametersForLBG, 
                       sw,
                       &glortsPerLbg, 
                       &unused);
    FM_LOG_EXIT_ON_ERR(FM_LOG_CAT_PORT, err);

    if (glortSize % glortsPerLbg)
    {
        /* glortSize is not multiple of glorts per LBG */
        FM_LOG_DEBUG(
            FM_LOG_CAT_PORT,
            "glortSize %d is not a multiple of size %d\n",
            glortSize,
            glortsPerLbg);

        FM_LOG_EXIT(FM_LOG_CAT_PORT, FM_ERR_INVALID_ARGUMENT);
    }

    if (startGlort % glortsPerLbg)
    {
        /* glort is not multiple of glorts per LBG */
        FM_LOG_DEBUG(
            FM_LOG_CAT_PORT,
            "startGlort %d is not a multiple of size %d\n",
            startGlort,
            glortsPerLbg);

        FM_LOG_EXIT(FM_LOG_CAT_PORT, FM_ERR_INVALID_ARGUMENT);
    }

    endGlort = startGlort + glortSize;

    if (endGlort > FM_MAX_GLORT)
    {
        FM_LOG_EXIT(FM_LOG_CAT_PORT, FM_FAIL);
    }

    /* Check for glort range not in use */
    for ( glort = startGlort ; glort < endGlort ; ++glort )
    {
        if (FM_IS_GLORT_TAKEN(lportInfo, glort))
        {
            FM_LOG_EXIT(FM_LOG_CAT_PORT, FM_ERR_LOG_PORT_UNAVAILABLE);
        }
    }

    /* Find a free reserved entry */
    allocIndex = fmFindFreeLbgEntry(sw);
    if (allocIndex < 0)
    {
        FM_LOG_EXIT(FM_LOG_CAT_PORT, FM_ERR_NO_LBG_RESOURCES);
    }

    allocEntry = &lportInfo->lbgAllocTable[allocIndex];

    /* Find a free logical port range */
    allocEntry->baseHandle = fmFindUnusedLogicalPorts(sw, glortSize);
    if (allocEntry->baseHandle == -1)
    {
        FM_LOG_EXIT(FM_LOG_CAT_PORT, FM_ERR_LOG_PORT_UNAVAILABLE);
    }

    allocEntry->baseGlort  = startGlort;
    allocEntry->glortSize  = glortSize;
    allocEntry->numPorts   = glortSize;
    allocEntry->numHandles = allocEntry->numPorts / glortsPerLbg;

    endPort = allocEntry->baseHandle + allocEntry->numPorts;
    err = FM_OK;

    FM_LOG_DEBUG(
        FM_LOG_CAT_PORT | FM_LOG_CAT_LBG,
        "Reserve port (%d -> %d) and glort (0x%x -> 0x%x) for LBG\n",
        allocEntry->baseHandle,
        endPort - 1,
        startGlort,
        endGlort - 1);

    /* Reserve the logical ports */
    for ( port = allocEntry->baseHandle ; port < endPort ; ++port )
    {
        if (FM_IS_LPORT_TAKEN(lportInfo, port))
        {
            /* Probably improperly locking causes this problem */
            FM_LOG_FATAL(FM_LOG_CAT_PORT,
                         "Port %u was available, but state is %d\n",
                         port,
                         lportInfo->lportState[port]);
            err = FM_ERR_PORT_IN_USE;
        }
        FM_RESERVE_LPORT_LBG(lportInfo, port);
    }

    /* Reserve the glort */
    for ( glort = startGlort ; glort < endGlort ; ++glort )
    {
        if (FM_IS_GLORT_TAKEN(lportInfo, glort))
        {
            /* Probably improperly locking causes this problem */
            FM_LOG_FATAL(FM_LOG_CAT_PORT,
                         "Glort 0x%x was available, but state is %d\n",
                         glort,
                         lportInfo->glortState[glort]);
            err = FM_ERR_PORT_IN_USE;
        }
        FM_RESERVE_GLORT_LBG(lportInfo, glort);
    }

    *baseLbgHandle = allocEntry->baseHandle;
    *numHandles    = allocEntry->numHandles;
    *step          = glortsPerLbg;

    FM_LOG_EXIT(FM_LOG_CAT_PORT, err);

}   /* end fmAllocateLbgHandles */




/*****************************************************************************/
/** fmFreeLbgHandles
 * \ingroup intPort
 *
 * \desc            Frees the load-balancing group resources allocated by
 *                  ''fmAllocateLbgHandles''.
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \param[in]       handle is the base handle returned from
 *                  ''fmAllocateLbgResources''.
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_INVALID_LBG if handle is not found
 *                  in allocated entries.
 * \return          FM_ERR_PORT_IN_USE if any port resources are still being
 *                  used.
 *
 *****************************************************************************/
fm_status fmFreeLbgHandles(fm_int sw, fm_int handle)
{
    fm_status                   err = FM_OK;
    fm_switch *                 switchPtr;
    fm_logicalPortInfo *        lportInfo;
    fm_uint32                   glort, endGlort;
    fm_int                      port, endPort;
    fm_lbgAllocEntry *          allocEntry;

    FM_LOG_ENTRY(FM_LOG_CAT_PORT,
                 "sw=%d handle=%d\n",
                 sw, handle);

    switchPtr     = fmRootApi->fmSwitchStateTable[sw];
    lportInfo     = &switchPtr->logicalPortInfo;

    /* Find an entry based on a specified handle */
    allocEntry = fmFindLbgEntryByHandle(sw, handle);
    if (!allocEntry)
    {
        /* Incorrect handle passed in */
        FM_LOG_EXIT(FM_LOG_CAT_PORT, FM_ERR_INVALID_LAG);
    }

    endGlort = allocEntry->baseGlort + allocEntry->glortSize;
    endPort = allocEntry->baseHandle + allocEntry->numPorts;

    for ( glort = allocEntry->baseGlort ; glort < endGlort ; ++glort )
    {
        if (!FM_IS_GLORT_LBG_FREE(lportInfo, glort))
        {
            /* Cannot delete when any port in the range is in use */
            /* This is one to one corresponding to logical port */
            FM_LOG_EXIT(FM_LOG_CAT_PORT, FM_ERR_PORT_IN_USE);
        }
    }

    /* un-reserve the logical ports */
    for ( port = allocEntry->baseHandle ; port < endPort ; ++port )
    {
        if (!FM_IS_LPORT_LBG_FREE(lportInfo, port))
        {
            FM_LOG_FATAL(FM_LOG_CAT_PORT,
                         "Port %u should be reserved, but state is %d.\n",
                         port,
                         lportInfo->lportState[port]);
            err = FM_ERR_PORT_IN_USE;
        }
        /* Now any one can use this */
        FM_RELEASE_LPORT(lportInfo, port);
    }

    /* Set the glort to free */
    for ( glort = allocEntry->baseGlort ; glort < endGlort ; ++glort )
    {
        if (!FM_IS_GLORT_LBG_FREE(lportInfo, glort))
        {
            FM_LOG_FATAL(FM_LOG_CAT_PORT,
                         "Glort 0x%x should be reserved, but state is %d.\n",
                         glort,
                         lportInfo->glortState[glort]);
            err = FM_ERR_PORT_IN_USE;
        }
        FM_RELEASE_GLORT(lportInfo, glort);
    }
    
    /* Set this entry to not-used */
    allocEntry->glortSize = 0;

    FM_LOG_EXIT(FM_LOG_CAT_PORT, err);

}   /* end fmFreeLbgHandles */ 




/*****************************************************************************/
/** fmFindFreeLagEntry
 * \ingroup intPort
 *
 * \desc            Returns the index of a free LAG allocation table
 *                  entry.
 *
 * \param[in]       sw is the switch number.
 *
 * \return          Index of a free entry, or -1 if no entries are
 *                  available.
 *
 *****************************************************************************/
fm_int fmFindFreeLagEntry(fm_int sw)
{
    fm_switch *         switchPtr;
    fm_logicalPortInfo *lportInfo;
    fm_int              index;

    switchPtr = GET_SWITCH_PTR(sw);
    lportInfo = &switchPtr->logicalPortInfo;

    for (index = 0 ; index < FM_LAG_ALLOC_TABLE_SIZE ; ++index)
    {
        if (!lportInfo->lagAllocTable[index].glortSize)
        {
            return index;
        }
    }

    return -1;

}   /* end fmFindFreeLagEntry */




/*****************************************************************************/
/** fmFindFreeLbgEntry
 * \ingroup intPort
 *
 * \desc            Returns the index of a free LBG allocation table
 *                  entry.
 *
 * \param[in]       sw is the switch number.
 *
 * \return          Index of a free entry, or -1 if no entries are
 *                  available.
 *
 *****************************************************************************/
fm_int fmFindFreeLbgEntry(fm_int sw)
{
    fm_switch *         switchPtr;
    fm_logicalPortInfo *lportInfo;
    fm_int              index;

    switchPtr = GET_SWITCH_PTR(sw);
    lportInfo = &switchPtr->logicalPortInfo;

    for (index = 0 ; index < FM_LBG_ALLOC_TABLE_SIZE ; ++index)
    {
        if (!lportInfo->lbgAllocTable[index].glortSize)
        {
            return index;
        }
    }

    return -1;

}   /* end fmFindFreeLbgEntry */




/*****************************************************************************/
/** fmFindFreeMcgEntry
 * \ingroup intPort
 *
 * \desc            Returns the index of a free MCG allocation table
 *                  entry.
 *
 * \param[in]       sw is the switch number.
 *
 * \return          Index of a free entry, or -1 if no entries are
 *                  available.
 *
 *****************************************************************************/
fm_int fmFindFreeMcgEntry(fm_int sw)
{
    fm_switch *         switchPtr;
    fm_logicalPortInfo *lportInfo;
    fm_int              index;

    switchPtr = GET_SWITCH_PTR(sw);
    lportInfo = &switchPtr->logicalPortInfo;

    for (index = 0 ; index < FM_MCG_ALLOC_TABLE_SIZE ; ++index)
    {
        if (!lportInfo->mcgAllocTable[index].glortSize)
        {
            return index;
        }
    }

    return -1;

}   /* end fmFindFreeMcgEntry */




/*****************************************************************************/
/** fmFindLagEntryByHandle
 * \ingroup intPort
 *
 * \desc            Returns the LAG allocation entry for the specified
 *                  handle.
 *
 * \param[in]       sw is the switch number.
 *
 * \param[in]       handle is the base handle returned by
 *                  ''fmAllocateLagHandles''.
 *
 * \return          Pointer to the LAG allocation entry, or NULL if the
 *                  handle was not found.
 *
 *****************************************************************************/
fm_lagAllocEntry * fmFindLagEntryByHandle(fm_int sw, fm_int handle)
{
    fm_switch *         switchPtr;
    fm_logicalPortInfo *lportInfo;
    fm_int              i;

    switchPtr = GET_SWITCH_PTR(sw);
    lportInfo = &switchPtr->logicalPortInfo;

    for (i = 0 ; i < FM_LAG_ALLOC_TABLE_SIZE ; ++i)
    {
        fm_lagAllocEntry* entryPtr = &lportInfo->lagAllocTable[i];

        if (!entryPtr->glortSize)
        {
            continue;
        }

        /* Find an entry based on a specified handle */
        if ( (handle >= entryPtr->baseHandle) &&
             (handle < (fm_int)(entryPtr->baseHandle + entryPtr->glortSize)) )
        {
            return entryPtr;
        }
    }

    return NULL;

}   /* end fmFindLagEntryByHandle */




/*****************************************************************************/
/** fmFindLbgEntryByHandle
 * \ingroup intPort
 *
 * \desc            Returns the LBG allocation entry for the specified
 *                  handle.
 *
 * \param[in]       sw is the switch number.
 *
 * \param[in]       handle is the base handle returned by
 *                  ''fmAllocateLbgHandles''.
 *
 * \return          Pointer to the LBG allocation entry, or NULL if the
 *                  handle was not found.
 *
 *****************************************************************************/
fm_lbgAllocEntry * fmFindLbgEntryByHandle(fm_int sw, fm_int handle)
{
    fm_switch *         switchPtr;
    fm_logicalPortInfo *lportInfo;
    fm_int              i;

    switchPtr = GET_SWITCH_PTR(sw);
    lportInfo = &switchPtr->logicalPortInfo;

    for (i = 0 ; i < FM_LBG_ALLOC_TABLE_SIZE ; ++i)
    {
        fm_lbgAllocEntry* entryPtr = &lportInfo->lbgAllocTable[i];

        if (!entryPtr->glortSize)
        {
            continue;
        }

        /* Find an entry based on a specified handle */
        if ( (handle >= entryPtr->baseHandle) &&
             (handle < (fm_int)(entryPtr->baseHandle + entryPtr->glortSize)) )
        {
            return entryPtr;
        }
    }

    return NULL;

}   /* end fmFindLbgEntryByHandle */




/*****************************************************************************/
/** fmFindMcgEntryByHandle
 * \ingroup intPort
 *
 * \desc            Returns the MCG allocation entry for the specified
 *                  handle.
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \param[in]       handle is the base handle returned by
 *                  ''fmAllocateMcastHandles''.
 *
 * \return          Pointer to the multicast allocation entry, or NULL if
 *                  the handle was not found.
 *
 *****************************************************************************/
fm_mcgAllocEntry * fmFindMcgEntryByHandle(fm_int sw, fm_int handle)
{
    fm_switch *         switchPtr;
    fm_logicalPortInfo *lportInfo;
    fm_int              i;

    switchPtr = GET_SWITCH_PTR(sw);
    lportInfo = &switchPtr->logicalPortInfo;

    for (i = 0 ; i < FM_MCG_ALLOC_TABLE_SIZE ; ++i)
    {
        fm_mcgAllocEntry* entryPtr = &lportInfo->mcgAllocTable[i];

        if (!entryPtr->glortSize)
        {
            continue;
        }

        /* Find an entry based on a specified handle */
        if ( (handle >= entryPtr->baseHandle) &&
             (handle < (fm_int)(entryPtr->baseHandle + entryPtr->glortSize)) )
        {
            return entryPtr;
        }
    }

    return NULL;

}   /* end fmFindMcgEntryByHandle */




/*****************************************************************************/
/** fmFindUnusedCamEntry
 * \ingroup intPort
 *
 * \desc            Finds an unused cam entry in the table.
 *
 * \param[in]       sw is the switch number.
 *
 * \return          The unused entry index, or -1 if no free entries found.
 *
 *****************************************************************************/
fm_int fmFindUnusedCamEntry(fm_int sw)
{
    fm_switch *         switchPtr;
    fm_logicalPortInfo *lportInfo;
    fm_int              camIndex;

    switchPtr = GET_SWITCH_PTR(sw);
    lportInfo = &switchPtr->logicalPortInfo;

    /***************************************************
     * Find an unused CAM entry.
     **************************************************/

    for (camIndex = 0 ; camIndex < lportInfo->numCamEntries ; ++camIndex)
    {
        if (lportInfo->camEntries[camIndex].useCount == 0)
        {
            return camIndex;
        }
    }

    return -1;

}   /* end fmFindUnusedCamEntry */




/*****************************************************************************/
/** fmFindUnusedDestEntries
 * \ingroup intPort
 *
 * \desc            Finds an unused block of dest entries.
 *
 * \param[in]       sw is the switch number.
 *
 * \param[in]       numEntries is the number of consecutive entries to find.
 *
 * \param[in]       first is the starting index for the search. If negative,
 *                  then search backward.
 *
 * \return          The starting index of the block or -1 if none found.
 *
 *****************************************************************************/
fm_int fmFindUnusedDestEntries(fm_int sw, fm_int numEntries, fm_int first)
{
    fm_switch *         switchPtr;
    fm_logicalPortInfo *lportInfo;
    fm_int              destIndex;
    fm_int              freeCount = 0;
    fm_int              start = -1;
    fm_int              init;
    fm_int              end;
    fm_int              incr;

    switchPtr = GET_SWITCH_PTR(sw);
    lportInfo = &switchPtr->logicalPortInfo;

    /***************************************************
     * Decide whether to go forward or backward
     **************************************************/

    if (first < 0)
    {
        init = lportInfo->numDestEntries - 1;
        end  = 0;
        incr = -1;
    }
    else
    {
        init = first;
        end  = lportInfo->numDestEntries;
        incr = 1;
    }
    
    /***************************************************
     * Find an unused dest table entry.
     * Use (entry != end) for the loop to go both 
     * directions
     **************************************************/

    for (destIndex = init ; destIndex != end ; destIndex += incr)
    {
        if (!lportInfo->destEntries[destIndex].usedBy && destIndex)
        {
            if (freeCount == 0)
            {
                start = destIndex;
            }

            ++freeCount;

            if (freeCount >= numEntries)
            {
                if (first < 0)
                {
                    return (start - numEntries + 1);
                }
                return start;
            }
        }
        else
        {
            freeCount = 0;
        }
    }

    return (freeCount >= numEntries) ? start : -1;

}   /* end fmFindUnusedDestEntries */




/*****************************************************************************/
/** fmFindUnusedGlorts
 * \ingroup intPort
 *
 * \desc            Finds an unused block of glorts.
 *
 * \param[in]       sw is the switch number.
 *
 * \param[in]       numGlorts is the required number number of glorts in
 *                  the block.
 *
 * \param[in]       first is the starting index for the search.
 *
 * \param[in]       camEntry points to the cam entry associated with this
 *                  glort.
 *
 * \return          The starting index of the block or -1 if none found.
 *
 *****************************************************************************/
fm_int fmFindUnusedGlorts(fm_int            sw,
                          fm_int            numGlorts,
                          fm_int            first,
                          fm_glortCamEntry* camEntry)
{
    fm_switch *         switchPtr;
    fm_logicalPortInfo* lportInfo;
    fm_glortRange *     glorts;
    fm_int              glort;
    fm_int              freeCount = 0;
    fm_int              start = -1;
    fm_int              firstEntry;
    fm_int              lastEntry;
    fm_uint32           rangeBase;
    fm_uint32           rangeMax;

    switchPtr = GET_SWITCH_PTR(sw);
    lportInfo = &switchPtr->logicalPortInfo;
    glorts    = &switchPtr->glortRange;
    rangeBase = glorts->glortBase;
    rangeMax  = rangeBase | glorts->glortMask;

    if (camEntry)
    {
        /* Assume key is already implicitly >= rangeBase */
        firstEntry = camEntry->camKey & camEntry->camMask;

        lastEntry  = firstEntry + MaxGlortsPerCamEntry(camEntry) - 1;
        if (lastEntry > (fm_int)rangeMax)
        {
            lastEntry = (fm_int) rangeMax;
        }

        if ( (first != -1) && ( (first < firstEntry) || (first > lastEntry) ) )
        {
            FM_LOG_FATAL(FM_LOG_CAT_PORT, 
                         "first glort %d out of range [%d, %d]\n",
                         first, 
                         firstEntry, 
                         lastEntry);

            return -1;
        }

        if (lastEntry > FM_MAX_GLORT)
        {
            FM_LOG_FATAL(FM_LOG_CAT_PORT, 
                         "Bad cam entry: range [%d, %d]\n",
                         firstEntry, 
                         lastEntry);

            return -1;
        }
    }
    else
    {
        firstEntry = rangeBase;
        lastEntry  = rangeMax;
    }

    /***************************************************
     * Find an unused block of glorts.
     **************************************************/

    for (glort = (first == -1) ? firstEntry : first ;
         glort <= lastEntry ;
         glort++)
    {
        /***************************************************
         * Two conditions must be met:
         *   (a) The glort is not zero (it is reserved)
         *   (b) The glort is unused
         **************************************************/

        if (glort && !FM_IS_GLORT_TAKEN(lportInfo, glort) )
        {
            if (freeCount == 0)
            {
                start = glort;
            }

            ++freeCount;

            if (freeCount >= numGlorts)
            {
                return start;
            }
        }
        else
        {
            freeCount = 0;
            start = -1;
        }
    }

    return (freeCount >= numGlorts) ? start : -1;

}   /* end fmFindUnusedGlorts */




/*****************************************************************************/
/** fmFindUnusedLogicalPorts
 * \ingroup intPort
 *
 * \desc            Finds an unused block of logical ports.
 *
 * \param[in]       sw is the switch number.
 *
 * \param[in]       numPorts is the number of consecutive entries to find.
 *
 * \return          The starting index of the block or -1 if none found.
 *
 *****************************************************************************/
fm_int fmFindUnusedLogicalPorts(fm_int sw, fm_int numPorts)
{
    fm_switch *         switchPtr;
    fm_logicalPortInfo *lportInfo;
    fm_int              port;
    fm_int              freeCount = 0;
    fm_int              start = -1;

    switchPtr = GET_SWITCH_PTR(sw);
    lportInfo = &switchPtr->logicalPortInfo;

    /***************************************************
     * Find an unused logical port entry.
     **************************************************/

    for (port = 0 ; port < switchPtr->maxPort ; ++port)
    {
        /* Free if not being used and not reserved */
        if (!switchPtr->portTable[port] &&
            !FM_IS_LPORT_TAKEN(lportInfo, port))
        {
            if (freeCount == 0)
            {
                start = port;
            }

            ++freeCount;

            if (freeCount >= numPorts)
            {
                return start;
            }
        }
        else
        {
            freeCount = 0;
        }
    }

    return -1;

}   /* end fmFindUnusedLogicalPorts */




/*****************************************************************************/
/** fmFindUnusedLagGlorts
 * \ingroup intPort
 *
 * \desc            Finds an unused glort in a reserved lag space. If
 *                  useHandle is non-zero, it return the logical port associated
 *                  with the handle. Otherwise it will just automatically find a
 *                  free entry.
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \param[in]       numPorts is the number of consecutive ports requested.
 *
 * \param[in]       useHandle contains the desired handle.  Use zero to
 *                  cause a free entry to be located.
 *
 * \param[in]       glortsPerLag specifies the lag size.
 *
 * \param[in,out]   logicalPort points to caller allocated storage where the
 *                  returned logical port is placed
 *
 * \param[in]       firstGlort points to caller allocated storage where the
 *                  returned first glort is placed
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_LOG_PORT_UNAVAILABLE if port is already in use.
 * \return          FM_ERR_INVALID_LAG if the specified useHandle is invalid.
 * \return          FM_ERR_NO_LAG_RESOURCES is there is no more allocated
 *                  entries.
 *
 *****************************************************************************/
fm_status fmFindUnusedLagGlorts(fm_int  sw,
                                fm_int  numPorts,
                                fm_int  useHandle,
                                fm_int  glortsPerLag,
                                fm_int *logicalPort,
                                fm_int *firstGlort)
{
    fm_logicalPortInfo *lportInfo;
    fm_lagAllocEntry *  allocEntry;
    fm_switch *         switchPtr;
    fm_status           err = FM_OK;
    fm_uint             i;
    fm_uint             offBase;
    fm_uint             glort;
    fm_int              cnt;

    FM_LOG_ENTRY(FM_LOG_CAT_PORT,
                 "sw=%d numPorts=%d useHandle=%d\n",
                 sw, numPorts, useHandle);

    switchPtr = GET_SWITCH_PTR(sw);
    lportInfo = &switchPtr->logicalPortInfo;

    if (useHandle)
    {
        /* Find an entry based on a specific handle. */
        allocEntry = fmFindLagEntryByHandle(sw, useHandle);
        if (!allocEntry)
        {
            FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PORT, err = FM_ERR_INVALID_LAG);
        }

        offBase = useHandle - allocEntry->baseHandle;

        /* Must be multiple of lag size */
        if (offBase % glortsPerLag)
        {
            FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PORT, err = FM_ERR_INVALID_LAG);
        }

        glort = allocEntry->baseGlort + offBase;

        for (cnt = 0 ; cnt < numPorts ; cnt++)
        {
            if (!FM_IS_GLORT_LAG_FREE(lportInfo, glort+cnt))
            {
                /* Glort is already in use */
                err = FM_ERR_LOG_PORT_UNAVAILABLE;
                FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PORT, err);
            }
        }

        *firstGlort = glort;

        /* For now the handle is the same as logical port */
        *logicalPort = allocEntry->baseHandle + offBase;
    }
    else
    {
        /* Find an entry based on any free entry. */
        for ( i = 0 ; i < FM_LAG_ALLOC_TABLE_SIZE ; ++i )
        {
            fm_int freeCount = 0;
    
            allocEntry = &lportInfo->lagAllocTable[i];
            if (!allocEntry->glortSize)
            {
                continue;
            }
    
            for (offBase =  0 ;
                 offBase < allocEntry->glortSize ; 
                 offBase++)
            {
                glort = allocEntry->baseGlort + offBase;
    
                /* Find free glort range reserved for LAG */
                if (FM_IS_GLORT_LAG_FREE(lportInfo, glort))
                {
                    if (freeCount == 0)
                    {
                        *firstGlort = glort;
                        *logicalPort = allocEntry->baseHandle + offBase;
                    }
                    
                    ++freeCount;
                    
                    if (freeCount >= numPorts) 
                    {
                        /* Done */
                        goto ABORT;
                    }
                }
                else
                {
                    freeCount = 0;
                }
            }
        }

        err = FM_ERR_NO_LAG_RESOURCES;
    }

ABORT:
    FM_LOG_EXIT(FM_LOG_CAT_PORT, err);

}   /* end fmFindUnusedLagGlorts */




/*****************************************************************************/
/** fmFindUnusedLbgGlorts
 * \ingroup intPort
 *
 * \desc            Finds an unused glort in a reserved lbg space. If
 *                  useHandle is non-zero, it return the logical port associated
 *                  with the handle. Otherwise it will just automatically find a
 *                  free entry.
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \param[in]       numPorts is the number of consecutive ports requested.
 *
 * \param[in]       useHandle contains the desired handle.  Use zero to
 *                  cause a free entry to be located.
 *
 * \param[in]       logicalPort points to caller allocated storage where the
 *                  returned logical port is placed
 *
 * \param[in]       firstGlort points to caller allocated storage where the
 *                  returned first glort is placed
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_LOG_PORT_UNAVAILABLE if port is already in use.
 * \return          FM_ERR_INVALID_LBG if the specified useHandle is invalid.
 * \return          FM_ERR_NO_LBG_RESOURCES is there is no more allocated
 *                  entries.
 *
 *****************************************************************************/
fm_status fmFindUnusedLbgGlorts(fm_int  sw,
                                fm_int  numPorts,
                                fm_int  useHandle,
                                fm_int *logicalPort,
                                fm_int *firstGlort)
{
    fm_logicalPortInfo *lportInfo;
    fm_lbgAllocEntry *  allocEntry;
    fm_switch *         switchPtr;
    fm_status           err = FM_OK;
    fm_uint             i;
    fm_uint             offBase;
    fm_uint             glort;
    fm_int              cnt;

    FM_LOG_ENTRY(FM_LOG_CAT_PORT,
                 "sw=%d numPorts=%d useHandle=%d\n",
                 sw, 
                 numPorts, 
                 useHandle);

    switchPtr = GET_SWITCH_PTR(sw);
    lportInfo = &switchPtr->logicalPortInfo;

    if (useHandle)
    {
        /* Find an entry based on a specified handle */
        allocEntry = fmFindLbgEntryByHandle(sw, useHandle);
        if (!allocEntry)
        {
            FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PORT, err = FM_ERR_INVALID_LBG);
        }

        offBase = useHandle - allocEntry->baseHandle;

        /* Must be multiple of lbg size */
        if (offBase % numPorts)
        {
            FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PORT, err = FM_ERR_INVALID_LBG);
        }

        glort = allocEntry->baseGlort + offBase;

        for (cnt = 0 ; cnt < numPorts ; cnt++)
        {
            if (!FM_IS_GLORT_LBG_FREE(lportInfo, glort+cnt))
            {
                /* Glort is already in use */
                err = FM_ERR_LOG_PORT_UNAVAILABLE;
                FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PORT, err);
            }
        }

        *firstGlort  = glort;

        /* For now the handle is the same as logical port */
        *logicalPort = allocEntry->baseHandle + offBase;
    }
    else
    {
        /* Find an entry based on any free entry */
        for ( i = 0 ; i < FM_LBG_ALLOC_TABLE_SIZE ; ++i )
        {
            fm_int freeCount = 0;
    
            allocEntry = &lportInfo->lbgAllocTable[i];
            if (!allocEntry->glortSize)
            {
                continue;
            }
    
            for (offBase =  0 ;
                 offBase < allocEntry->glortSize ; 
                 offBase++)
            {
                glort = allocEntry->baseGlort + offBase;
    
                /* Find free glort range reserved for LBG */
                if (FM_IS_GLORT_LBG_FREE(lportInfo, glort))
                {
                    if (freeCount == 0)
                    {
                        *firstGlort = glort;
                        *logicalPort = allocEntry->baseHandle + offBase;
                    }
                    
                    ++freeCount;
                    
                    if (freeCount >= numPorts) 
                    {
                        /* Done */
                        goto ABORT;
                    }
                }
                else
                {
                    freeCount = 0;
                }
            }
        }

        err = FM_ERR_NO_LBG_RESOURCES;
    }

ABORT:
    FM_LOG_EXIT(FM_LOG_CAT_PORT, err);

}   /* end fmFindUnusedLbgGlorts */




/*****************************************************************************/
/** fmFindUnusedMcgGlorts
 * \ingroup intPort
 *
 * \desc            Finds an unused glort in a reserved multicast space. If
 *                  useHandle is non-zero, it return the logical port associated
 *                  with the handle. Otherwise it will just automatically find a
 *                  free entry.
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \param[in]       numPorts is the number of consecutive ports requested.
 *
 * \param[in]       useHandle contains the desired handle.  Use zero to
 *                  cause a free entry to be located.
 *
 * \param[out]      allocEntryPtr points to a caller-supplied variable where
 *                  a pointer to the allocation table entry is stored.
 *
 * \param[out]      offBasePtr points to a caller-supplied variable to receive
 *                  the offset (index) of the first glort/port in the allocation
 *                  table entry.
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_LOG_PORT_UNAVAILABLE if port is already in use.
 * \return          FM_ERR_INVALID_MULTICAST_GROUP if the specified useHandle
 *                  is invalid.
 * \return          FM_ERR_NO_MCAST_RESOURCES is there is no more allocated
 *                  entries.
 * \return          FM_ERR_LPORT_DESTS_UNAVAILABLE if there is no more dest
 *                  entries available
 *
 *****************************************************************************/
fm_status fmFindUnusedMcgGlorts(fm_int             sw,
                                fm_int             numPorts,
                                fm_int             useHandle,
                                fm_mcgAllocEntry **allocEntryPtr,
                                fm_uint *          offBasePtr)
{
    fm_uint             i;
    fm_uint             offBase;
    fm_uint             glort;
    fm_int              cnt;
    fm_status           err = FM_OK;
    fm_switch *         switchPtr;
    fm_logicalPortInfo *lportInfo;
    fm_mcgAllocEntry *  allocEntry;

    FM_LOG_ENTRY(FM_LOG_CAT_PORT,
                 "sw=%d numPorts=%d useHandle=%d\n",
                 sw, 
                 numPorts, 
                 useHandle);

    switchPtr = GET_SWITCH_PTR(sw);
    lportInfo = &switchPtr->logicalPortInfo;

    if (useHandle) 
    {
        /* Find an entry based on a specified handle */
        allocEntry = fmFindMcgEntryByHandle(sw, useHandle);
        if (!allocEntry)
        {
            err = FM_ERR_INVALID_MULTICAST_GROUP;
            FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PORT, err);
        }

        offBase = useHandle - allocEntry->baseHandle;
        glort = allocEntry->baseGlort + offBase;

        for (cnt = 0 ; cnt < numPorts ; cnt++)
        {
            if (!FM_IS_GLORT_MCG_FREE(lportInfo, glort+cnt))
            {
                /* Glort is already in use */
                err = FM_ERR_LOG_PORT_UNAVAILABLE;
                FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PORT, err);
            }
        }

        *allocEntryPtr = allocEntry;
        *offBasePtr = offBase;
    }
    else
    {
        /* Find an entry based on any free entry */
        for ( i = 0 ; i < FM_MCG_ALLOC_TABLE_SIZE ; ++i )
        {
            fm_int freeCount = 0;
    
            allocEntry = &lportInfo->mcgAllocTable[i];
            if (!allocEntry->glortSize)
            {
                continue;
            }
    
            for (offBase =  0 ;
                 offBase < allocEntry->glortSize ; 
                 offBase++)
            {
                glort = allocEntry->baseGlort + offBase;
    
                /* Find a free glort reserved for mcast 
                 * Any glort used for mcast must be reserved
                 */
                if (FM_IS_GLORT_MCG_FREE(lportInfo, glort))
                {
                    if (freeCount == 0)
                    {
                        *offBasePtr = offBase;
                    }
                    
                    ++freeCount;
                    
                    if (freeCount >= numPorts) 
                    {
                        *allocEntryPtr = allocEntry;
                        /* Done */
                        goto ABORT;
                    }
                }
                else
                {
                    freeCount = 0;
                }
            }
        }

        err = FM_ERR_NO_MCAST_RESOURCES;
    }

ABORT:
    FM_LOG_EXIT(FM_LOG_CAT_PORT, err);

}   /* end fmFindUnusedMcgGlorts */




/*****************************************************************************/
/** fmPortTypeToText
 * \ingroup intPort
 *
 * \desc            Returns the text representation of a logical port type.
 *
 * \param[in]       type is the allocation type (see ''fm_portType'').
 *
 * \return          Pointer to a string representing the logical port type.
 *
 *****************************************************************************/
const char* fmPortTypeToText(fm_portType type)
{

    switch (type)
    {
        case FM_PORT_TYPE_PHYSICAL:
            return "PHYSICAL";

        case FM_PORT_TYPE_LAG:
            return "LAG";

        case FM_PORT_TYPE_MULTICAST:
            return "MULTICAST";

        case FM_PORT_TYPE_LBG:
            return "LBG";

        case FM_PORT_TYPE_CPU:
            return "CPU";

        case FM_PORT_TYPE_CPU_MGMT:
            return "CPU_MGMT";

        case FM_PORT_TYPE_SPECIAL:
            return "SPECIAL";

        case FM_PORT_TYPE_REMOTE:
            return "REMOTE";

        default:
            FM_LOG_ERROR(FM_LOG_CAT_PORT,
                         "Unknown port type (%d)\n",
                         type);
            return "UNKNOWN";
    }

}   /* end fmPortTypeToText */




/*****************************************************************************/
/** fmRemoveGlortCamEntry
 * \ingroup intPort
 *
 * \desc            Removes an entry from the glort CAM/RAM.
 *                  Assumes switch protection lock has already been acquired
 *                  and that the switch pointer is valid.
 *
 * \param[in]       sw is the switch number.
 *
 * \param[in]       camIndex is the index of the CAM entry to remove.
 *
 * \return          FM_OK if successful.
 *
 *****************************************************************************/
fm_status fmRemoveGlortCamEntry(fm_int sw, fm_uint32 camIndex)
{
    fm_switch *         switchPtr;
    fm_logicalPortInfo *lportInfo;
    fm_glortCamEntry *  camEntry;
    fm_status           err = FM_OK;

    FM_LOG_ENTRY(FM_LOG_CAT_PORT, "sw=%d camIndex=%d\n", sw, camIndex);

    switchPtr = GET_SWITCH_PTR(sw);
    lportInfo = &switchPtr->logicalPortInfo;
    camEntry  = &lportInfo->camEntries[camIndex];

    if (camEntry->useCount == 0)
    {
        FM_LOG_EXIT(FM_LOG_CAT_PORT, FM_OK);
    }

    --camEntry->useCount;

    if (camEntry->useCount <= 0)
    {
        camEntry->useCount = 0;

        /* Only need to zero out the CAM entry, not the RAM. */
        camEntry->camKey = 0;
        camEntry->camMask = 0;

        FM_API_CALL_FAMILY(err,
                           switchPtr->WriteGlortCamEntry,
                           sw,
                           camEntry,
                           FM_UPDATE_CAM_ONLY);
    }

    FM_LOG_EXIT(FM_LOG_CAT_PORT, err);

}   /* end fmRemoveGlortCamEntry */




/*****************************************************************************/
/** fmResetLogicalPortInfo
 * \ingroup intPort
 *
 * \desc            Resets the logical port information structure.
 *
 * \param[in,out]   lportInfo points to the logical port information structure.
 *
 * \return          Nothing.
 *
 *****************************************************************************/
void fmResetLogicalPortInfo(fm_logicalPortInfo* lportInfo)
{
    fm_glortCamEntry * camEntries;
    fm_glortDestEntry *destEntries;
    fm_int             numCamEntries;
    fm_int             numDestEntries;

    camEntries      = lportInfo->camEntries;
    destEntries     = lportInfo->destEntries;
    numCamEntries   = lportInfo->numCamEntries;
    numDestEntries  = lportInfo->numDestEntries;

    memset(lportInfo, 0, sizeof(*lportInfo));

    if (camEntries)
    {
        memset(camEntries, 0, sizeof(fm_glortCamEntry) * numCamEntries);
        lportInfo->camEntries    = camEntries;
        lportInfo->numCamEntries = numCamEntries;
    }

    if (destEntries)
    {
        memset(destEntries, 0, sizeof(fm_glortDestEntry) * numDestEntries);
        lportInfo->destEntries    = destEntries;
        lportInfo->numDestEntries = numDestEntries;
    }

}   /* end fmResetLogicalPortInfo */




/*****************************************************************************/
/** fmVerifyGlortRange
 * \ingroup intPort
 *
 * \desc            Verifies that glort range is valid.
 *
 * \param[in]       glort is the start glort.
 *
 * \param[in]       size is the size of the range.
 *
 * \return          FM_OK if range is okay.
 * \return          FM_FAIL if out of physical range.
 *
 *****************************************************************************/
fm_status fmVerifyGlortRange(fm_uint32 glort, fm_int size)
{

    if ((size <= 0) || (size > FM_MAX_GLORT))
    {
        return FM_FAIL;
    }

    if ((glort == 0) || (glort > FM_MAX_GLORT))
    {
        return FM_FAIL;
    }

    if ((glort + size) > FM_MAX_GLORT)
    {
        return FM_FAIL;
    }

    return FM_OK;

}   /* end fmVerifyGlortRange */





/*****************************************************************************/
/** fmFreeMcastLogicalPort
 * \ingroup intPort
 *
 * \desc            Free the CAM/Dest Table resources used by a multicast
 *                  logical port.
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \param[in]       port is the logical port to free.
 *
 * \return          FM_OK if successful.
 *
 *****************************************************************************/
fm_status fmFreeMcastLogicalPort(fm_int sw, fm_int port)
{
    fm_switch *         switchPtr;
    fm_port *           portPtr;
    fm_logicalPortInfo *lportInfo;
    fm_status           err = FM_OK;
    fm_int              block;
    fm_int              i;
    fm_int              numDestEntries;
    fm_mcgAllocEntry *  mcgEntry;
    fm_uint             offBase;
    fm_uint             index;
    fm_uint             offEntry;
    fm_glortDestEntry * currentDestEntry;
    fm_int              firstDestEntry;
    fm_glortCamEntry *  camEntry;

    FM_LOG_ENTRY(FM_LOG_CAT_PORT, "sw=%d, port=%d\n", sw, port);

    switchPtr = GET_SWITCH_PTR(sw);
    lportInfo = &switchPtr->logicalPortInfo;
    portPtr   = GET_PORT_PTR(sw, port);

    for (block = 0 ; block < FM_MCG_ALLOC_TABLE_SIZE ; block++ )
    {
        mcgEntry = &lportInfo->mcgAllocTable[block];
    
        if (!mcgEntry->glortSize)
        {
            continue;
        }

        if ( (portPtr->glort >= mcgEntry->baseGlort)
            && (portPtr->glort < (mcgEntry->baseGlort + mcgEntry->glortSize) ) )
        {
            break;
        }
    }

    if (block >= FM_MCG_ALLOC_TABLE_SIZE)
    {
        /* Couldn't find the allocation block, skip it */
        FM_LOG_EXIT(FM_LOG_CAT_PORT, FM_OK);
    }

    offBase  = portPtr->glort - mcgEntry->baseGlort;
    index    = offBase / switchPtr->mcastMaxEntryPerGlort;
    offEntry = offBase % switchPtr->mcastMaxEntryPerGlort;

    err = fmSetBitArrayBit(&mcgEntry->dstInUse[index],
                           offEntry,
                           FALSE);

    FM_LOG_EXIT_ON_ERR(FM_LOG_CAT_PORT, err);

    err = fmGetBitArrayNonZeroBitCount(&mcgEntry->dstInUse[index], &i);

    if (err != FM_OK)
    {
        FM_LOG_EXIT(FM_LOG_CAT_PORT, FM_OK);
    }

    if (i != 0)
    {
        FM_LOG_EXIT(FM_LOG_CAT_PORT, FM_OK);
    }

    numDestEntries = mcgEntry->glortSize;

    if (numDestEntries > switchPtr->mcastMaxEntryPerGlort)
    {
        numDestEntries = switchPtr->mcastMaxEntryPerGlort;
    }

    camEntry       = &lportInfo->camEntries[mcgEntry->mcgCamIndex[index]];
    firstDestEntry = camEntry->destIndex;

    for (i = 0 ; i < numDestEntries ; i++)
    {
        currentDestEntry = &lportInfo->destEntries[firstDestEntry + i];

        currentDestEntry->destIndex = 0;
        currentDestEntry->owner     = NULL;
        currentDestEntry->usedBy    = 0;
    }

    camEntry->destIndex           = 0;
    mcgEntry->mcgDestIndex[index] = 0;

    FM_LOG_EXIT(FM_LOG_CAT_PORT, FM_OK);

}   /* end fmFreeMcastLogicalPort */
