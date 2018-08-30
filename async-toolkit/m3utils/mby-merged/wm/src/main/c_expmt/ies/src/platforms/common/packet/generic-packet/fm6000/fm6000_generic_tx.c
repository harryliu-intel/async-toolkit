/* vim:ts=4:sw=4:expandtab
 * (No tabs, indent level is 4 spaces)  */
/*****************************************************************************
 * File:            fm6000_generic_tx.c
 * Creation Date:   Jan 5, 2009
 * Description:     Generic packet sending code.
 *
 * INTEL CONFIDENTIAL
 * Copyright 2006 - 2012 Intel Corporation. All Rights Reserved. 
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


#include <fm_sdk_fm6000_int.h>

/*****************************************************************************
 * Macros, Constants & Types
 *****************************************************************************/

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
/** fm6000GetTrapGlort
 * \ingroup intPlatformCommon
 *
 * \desc            Return the stag type
 *
 * \param[in]       sw is the switch number.
 *
 * \param[out]      trapGlort is the pointer to hold trap glort.
 *
 * \return          FM_OK if successful.
 * \return          Other ''Status Codes'' as appropriate in case of
 *                  failure.
 *
 *****************************************************************************/
fm_status fm6000GetTrapGlort(fm_int     sw,
                             fm_uint32 *trapGlort)
{
    *trapGlort = FM6000_TRAP_GLORT(sw);
    FM_LOG_EXIT(FM_LOG_CAT_EVENT_PKT_TX, FM_OK);

}   /* end fm6000GetTrapGlort */




/*****************************************************************************
 * Public Functions
 *****************************************************************************/

/*****************************************************************************/
/** fm6000GetPortStagType
 * \ingroup intPlatformCommon
 *
 * \desc            Return the stag type
 *
 * \param[in]       sw is the switch number.
 *
 * \param[out]      stagTypeA is the pointer to hold second ether type
 *                  for inner vlan.
 *
 * \param[out]      stagTypeB is the pointer to hold second ether type
 *                  for outer vlan.
 *
 * \return          FM_OK if successful.
 * \return          Other ''Status Codes'' as appropriate in case of
 *                  failure.
 *
 *****************************************************************************/
fm_status fm6000GetStagType(fm_int     sw,
                            fm_uint32 *stagTypeA,
                            fm_uint32 *stagTypeB)
{
    fm_status         err = FM_OK;

    err = fm6000GetSwitchAttribute(sw, FM_SWITCH_VLAN1_ETHERTYPE_B, stagTypeA);
    FM_LOG_EXIT_ON_ERR(FM_LOG_CAT_EVENT_PKT_TX, err);

    err = fm6000GetSwitchAttribute(sw, FM_SWITCH_VLAN2_ETHERTYPE_B, stagTypeB);
    FM_LOG_EXIT(FM_LOG_CAT_EVENT_PKT_TX, err);

} /* end fm6000GetPortStagType */




/*****************************************************************************/
/** fm6000GenericSendPacketDirected
 * \ingroup intPlatformCommon
 *
 * \desc            Called to add a packet to the TX packet queue.
 *
 * \param[in]       sw is the switch on which to send the packet.
 *
 * \param[in]       portList points to an array of logical port numbers the
 *                  switch is to send the packet.
 *
 * \param[in]       numPorts is the number of elements in portList.
 *
 * \param[in]       packet points to the packet buffer's first ''fm_buffer''
 *                  structure in a chain of one or more buffers.
 * 
 * \param[in]       info is a pointer to the packet information structure which
 *                  contains some relevant information describing the packet.
 *                  See 'fm_packetInfoV2' for more information.
 *                  Note: the structure pointed to by info must have all fields
 *                  initialized. Any unused field should be set to zero.
 *                  Failure to initialize all fields can result in the packet
 *                  being mishandled.
 *
 * \return          FM_OK if successful.
 * \return          Other ''Status Codes'' as appropriate in case of
 *                  failure.
 *
 *****************************************************************************/
fm_status fm6000GenericSendPacketDirected(fm_int           sw,
                                          fm_int *         portList,
                                          fm_int           numPorts,
                                          fm_buffer *      packet,
                                          fm_packetInfoV2 *info)
{
    fm_status   err;
    fm_int      cpuPort;
    fm_uint32   stagTypeA;
    fm_uint32   stagTypeB;
    fm_uint32   trapGlort;
    fm_uint32   fcsValue;

    FM_LOG_ENTRY(FM_LOG_CAT_EVENT_PKT_TX,
                 "sw = %d, "
                 "portList = %p, "
                 "numPorts = %d, "
                 "packet->index = 0x%x\n",
                 sw,
                 (void *) portList,
                 numPorts,
                 packet->index);

    switch (info->fcsMode)
    {
        case FM_FCS_MODE_DEFAULT:
        case FM_FCS_MODE_VALUE:
            fcsValue = info->fcsValue;
            break;

        case FM_FCS_MODE_ZERO:
            fcsValue = 0;
            break;

        case FM_FCS_MODE_TIMESTAMP:
            fcsValue = 0x00000080;
            break;

        default:
            FM_LOG_EXIT(FM_LOG_CAT_EVENT_PKT_TX, FM_ERR_INVALID_ARGUMENT);
    }

    err = fmGetCpuPortInt(sw, &cpuPort);
    FM_LOG_EXIT_ON_ERR(FM_LOG_CAT_EVENT_PKT_TX, err);

    err = fm6000GetStagType(sw, &stagTypeA, &stagTypeB);
    FM_LOG_EXIT_ON_ERR(FM_LOG_CAT_EVENT_PKT_TX, err);

    err = fm6000GetTrapGlort(sw, &trapGlort);
    FM_LOG_EXIT_ON_ERR(FM_LOG_CAT_EVENT_PKT_TX, err);

    /* Call the shared code */
    err = fmGenericSendPacketDirected(sw, portList, numPorts,
                                      packet, fcsValue,
                                      cpuPort, stagTypeA, stagTypeB,
                                      info->switchPriority, trapGlort, FALSE);

    FM_LOG_EXIT(FM_LOG_CAT_EVENT_PKT_TX, err);

}   /* end fm6000GenericSendPacketDirected */




/*****************************************************************************/
/** fm6000GenericSendPacketSwitched
 * \ingroup intPlatformCommon
 *
 * \desc            Called to add a packet to the TX packet queue.
 *
 * \param[in]       sw is the switch on which to send the packet.
 *
 * \param[in]       packet points to the packet buffer's first ''fm_buffer''
 *                  structure in a chain of one or more buffers.
 *
 * \return          FM_OK if successful.
 * \return          Other ''Status Codes'' as appropriate in case of
 *                  failure.
 *
 *****************************************************************************/
fm_status fm6000GenericSendPacketSwitched(fm_int sw, fm_buffer *packet)
{
    fm_status               err = FM_OK;
    fm_int                  cpuPort;
    fm_uint32               stagTypeA;
    fm_uint32               stagTypeB;
    fm_uint32               trapGlort;

    FM_LOG_ENTRY(FM_LOG_CAT_EVENT_PKT_TX,
                 "sw = %d, "
                 "packet->index = 0x%x\n",
                 sw,
                 packet->index);

    err = fmGetCpuPortInt(sw, &cpuPort);
    FM_LOG_EXIT_ON_ERR(FM_LOG_CAT_EVENT_PKT_TX, err);

    err = fm6000GetStagType(sw, &stagTypeA, &stagTypeB);
    FM_LOG_EXIT_ON_ERR(FM_LOG_CAT_EVENT_PKT_TX, err);

    err = fm6000GetTrapGlort(sw, &trapGlort);
    FM_LOG_EXIT_ON_ERR(FM_LOG_CAT_EVENT_PKT_TX, err);

    /* Call the shared code */
    err = fmGenericSendPacketSwitched(sw, packet, cpuPort, stagTypeA, stagTypeB,
                                      FM_USE_VLAN_PRIORITY, trapGlort, TRUE);

    FM_LOG_EXIT(FM_LOG_CAT_EVENT_PKT_TX, err);

}   /* end fm6000GenericSendPacketSwitched */




/*****************************************************************************/
/** fm6000GenericSendPacket
 * \ingroup intPlatformCommon
 *
 * \desc            Called to add a packet to the TX packet queue.
 *
 * \param[in]       sw is the switch on which to send the packet.
 *
 * \param[in]       info is a pointer to associated information about
 *                  the packet including where it is going.
 *
 * \param[in]       packet is a pointer to a chain of fm_buffer structures
 *                  containing the payload.
 *
 * \return          FM_OK if successful.
 * \return          Other ''Status Codes'' as appropriate in case of
 *                  failure.
 *
 *****************************************************************************/
fm_status fm6000GenericSendPacket(fm_int         sw,
                                  fm_packetInfo *info,
                                  fm_buffer *    packet)
{
    fm_status               err = FM_OK;
    fm_int                  cpuPort;
    fm_uint32               stagTypeA;
    fm_uint32               stagTypeB;
    fm_uint32               trapGlort = 0;

    FM_LOG_ENTRY(FM_LOG_CAT_EVENT_PKT_TX,
                 "sw = %d, "
                 "info->destMask = 0x%08x, "
                 "info->port %d, "
                 "info->vlanId = %d, "
                 "info->vlanPriority = %d, "
                 "info->switchPriority = %d, "
                 "info->useEgressRules = %s, "
                 "packet->index = 0x%x\n",
                 sw,
                 info->destMask,
                 info->logicalPort,
                 info->vlanId,
                 info->vlanPriority,
                 info->switchPriority,
                 FM_BOOLSTRING(info->useEgressRules),
                 packet->index);


    err = fmGetCpuPortInt(sw, &cpuPort);
    FM_LOG_EXIT_ON_ERR(FM_LOG_CAT_EVENT_PKT_TX, err);

    err = fm6000GetStagType(sw, &stagTypeA, &stagTypeB);
    FM_LOG_EXIT_ON_ERR(FM_LOG_CAT_EVENT_PKT_TX, err);

    /* Trap glort only used when user does not provide source glort */
    if (info->sourcePort == 0)
    {
        err = fm6000GetTrapGlort(sw, &trapGlort);
        FM_LOG_EXIT_ON_ERR(FM_LOG_CAT_EVENT_PKT_TX, err);
    }

    /* Call the shared code */
    err = fmGenericSendPacket(sw, info,packet, cpuPort, stagTypeA, stagTypeB,
                              FM_USE_VLAN_PRIORITY, trapGlort, TRUE);

    FM_LOG_EXIT(FM_LOG_CAT_EVENT_PKT_TX, err);

}   /* end fm6000GenericSendPacket */




/*****************************************************************************/
/** fm6000SetIslVType
 * \ingroup intPlatformCommon
 *
 * \desc            Set the ISL Tag's VTYPE field.
 * 
 * \note            There is no VTYPE field in the FM6000.
 * 
 * \param[in]       sw is the switch number.
 *
 * \param[in]       cpuPort contains the logical port number for the CPU port. 
 *
 * \param[in]       stagTypeA is the SVLAN tag type A.
 *
 * \param[in]       stagTypeB is the SVLAN tag type B.
 *
 * \param[in]       payloadVlanTag is the packet Vlan tag type.
 *
 * \param[in]       pTag indicates if the frame is tagged or not.
 *
 * \param[out]      value is the first word of the ISL tag.
 *
 * \return          FM_OK if successful.
 * \return          Other ''Status Codes'' as appropriate in case of
 *                  failure.
 *
 *****************************************************************************/
fm_status fm6000SetIslVType(fm_int     sw,
                            fm_int     cpuPort,
                            fm_uint32  stagTypeA,
                            fm_uint32  stagTypeB,
                            fm_uint16  payloadVlanTag,
                            fm_bool    pTag,
                            fm_uint32 *value)
{

    FM_NOT_USED(sw);
    FM_NOT_USED(cpuPort);
    FM_NOT_USED(stagTypeA);
    FM_NOT_USED(stagTypeB);
    FM_NOT_USED(payloadVlanTag);
    FM_NOT_USED(pTag);
    FM_NOT_USED(value);

    return FM_OK;

}   /* end fm6000SetIslVType */

