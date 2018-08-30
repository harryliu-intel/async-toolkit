/* vim:ts=4:sw=4:expandtab
 * (No tabs, indent level is 4 spaces)  */
/*****************************************************************************
 * File:            fm4000_generic_tx.c
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


#include <fm_sdk_fm4000_int.h>

/*****************************************************************************
 * Macros, Constants & Types
 *****************************************************************************/
/* The number of buffers to leave for the send side */
#define FM_RECV_BUFFER_THRESHOLD  1

#define FM_RECV_PKT_MAX_BURST     32

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
/** fm4000GetTrapGlort
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
static fm_status fm4000GetTrapGlort(fm_int     sw,
                                    fm_uint32 *trapGlort)
{
    fm_switch         *switchPtr;

    switchPtr = GET_SWITCH_PTR(sw);

    /* This must read from cached for fibm enabled switches */
    return switchPtr->ReadUINT32(sw, FM4000_TRAP_GLORT, trapGlort);

}   /* end fm4000GetTrapGlort */


/*****************************************************************************
 * Public Functions
 *****************************************************************************/


/*****************************************************************************/
/** fm4000GetPortStagType
 * \ingroup intPlatformCommon
 *
 * \desc            Return the stag type
 *
 * \param[in]       sw is the switch number.
 *
 * \param[in]       port is the logical port number.
 *
 * \param[out]      stagTypeA is the pointer to hold stag type A.
 *
 * \param[out]      stagTypeB is the pointer to hold stag type B.
 *
 * \return          FM_OK if successful.
 * \return          Other ''Status Codes'' as appropriate in case of
 *                  failure.
 *
 *****************************************************************************/
fm_status fm4000GetPortStagType(fm_int     sw,
                                fm_int     port,
                                fm_uint32 *stagTypeA,
                                fm_uint32 *stagTypeB)
{
    fm_status         err;
    fm_uint32         rv;
    fm_switch         *switchPtr;
    fm_int            switchNum;
    fm_int            physPort;

    switchPtr = GET_SWITCH_PTR(sw);

    err = fmPlatformMapLogicalPortToPhysical(sw,
                                             port,
                                             &switchNum,
                                             &physPort);
    if (err != FM_OK)
    {
        return err;
    }

    /* This must be read from cached for fibm enabled switches */
    err = switchPtr->ReadUINT32(sw, FM4000_MAC_VLAN_ETYPE_2(physPort), &rv);
    if (err != FM_OK)
    {
        return err;
    }

    *stagTypeA = FM_GET_FIELD(rv, FM4000_MAC_VLAN_ETYPE_2, STagTypeA);
    *stagTypeB = FM_GET_FIELD(rv, FM4000_MAC_VLAN_ETYPE_2, STagTypeB);

    return FM_OK;

}   /* end fm4000GetPortStagType */




/*****************************************************************************/
/** fm4000GenericSendPacketDirected
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
fm_status fm4000GenericSendPacketDirected(fm_int           sw,
                                          fm_int *         portList,
                                          fm_int           numPorts,
                                          fm_buffer *      packet,
                                          fm_packetInfoV2 *info)
{
    fm_status               err = FM_OK;
    fm_int                  cpuPort;
    fm_uint32               stagTypeA;
    fm_uint32               stagTypeB;
    fm_uint32               trapGlort;

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
        case FM_FCS_MODE_AUTO:
            break;

        default:
            FM_LOG_EXIT(FM_LOG_CAT_EVENT_PKT_TX, FM_ERR_INVALID_ARGUMENT);
    }

    err = fmGetCpuPortInt(sw, &cpuPort);
    FM_LOG_EXIT_ON_ERR(FM_LOG_CAT_EVENT_PKT_TX, err);

    err = fm4000GetPortStagType(sw, cpuPort, &stagTypeA, &stagTypeB);
    FM_LOG_EXIT_ON_ERR(FM_LOG_CAT_EVENT_PKT_TX, err);

    err = fm4000GetTrapGlort(sw, &trapGlort);
    FM_LOG_EXIT_ON_ERR(FM_LOG_CAT_EVENT_PKT_TX, err);

    /* Call the shared code */
    err = fmGenericSendPacketDirected(sw, portList, numPorts,
                                      packet, FM_USE_DEFAULT_FCS,
                                      cpuPort, stagTypeA, stagTypeB,
                                      info->switchPriority, trapGlort, TRUE);

    FM_LOG_EXIT(FM_LOG_CAT_EVENT_PKT_TX, err);

}   /* end fm4000GenericSendPacketDirected */




/*****************************************************************************/
/** fm4000GenericSendPacketSwitched
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
fm_status fm4000GenericSendPacketSwitched(fm_int sw, fm_buffer *packet)
{
    fm_status               err = FM_OK;
    fm_int                  cpuPort;
    fm_uint32               stagTypeA;
    fm_uint32               stagTypeB;
    fm_uint32               trapGlort = 0;

    FM_LOG_ENTRY(FM_LOG_CAT_EVENT_PKT_TX,
                 "sw = %d, "
                 "packet->index = 0x%x\n",
                 sw,
                 packet->index);

    err = fmGetCpuPortInt(sw, &cpuPort);
    FM_LOG_EXIT_ON_ERR(FM_LOG_CAT_EVENT_PKT_TX, err);

    err = fm4000GetPortStagType(sw, cpuPort, &stagTypeA, &stagTypeB);
    FM_LOG_EXIT_ON_ERR(FM_LOG_CAT_EVENT_PKT_TX, err);

    err = fm4000GetTrapGlort(sw, &trapGlort);
    FM_LOG_EXIT_ON_ERR(FM_LOG_CAT_EVENT_PKT_TX, err);

    /* Call the shared code */
    err = fmGenericSendPacketSwitched(sw, packet, cpuPort, stagTypeA, stagTypeB,
                                      FM_USE_VLAN_PRIORITY, trapGlort, TRUE);

    FM_LOG_EXIT(FM_LOG_CAT_EVENT_PKT_TX, err);

}   /* end fm4000GenericSendPacketSwitched */




/*****************************************************************************/
/** fm4000GenericSendPacket
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
fm_status fm4000GenericSendPacket(fm_int         sw,
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

    err = fm4000GetPortStagType(sw, cpuPort, &stagTypeA, &stagTypeB);
    FM_LOG_EXIT_ON_ERR(FM_LOG_CAT_EVENT_PKT_TX, err);

    /* Trap glort only used when user does not provide source glort */
    if (info->sourcePort == 0)
    {
        err = fm4000GetTrapGlort(sw, &trapGlort);
        FM_LOG_EXIT_ON_ERR(FM_LOG_CAT_EVENT_PKT_TX, err);
    }
    
    /* Call the shared code */
    err = fmGenericSendPacket(sw, info,packet, cpuPort, stagTypeA, stagTypeB,
                              FM_USE_VLAN_PRIORITY, trapGlort, TRUE);

    FM_LOG_EXIT(FM_LOG_CAT_EVENT_PKT_TX, err);

}   /* end fm4000GenericSendPacket */




/*****************************************************************************/
/** fm4000SetIslVType
 * \ingroup intPlatformCommon
 *
 * \desc            Set the ISL Tag's VTYPE field.
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
fm_status fm4000SetIslVType(fm_int     sw,
                            fm_int     cpuPort,
                            fm_uint32  stagTypeA,
                            fm_uint32  stagTypeB,
                            fm_uint16  payloadVlanTag,
                            fm_bool    pTag,
                            fm_uint32 *value)
{
    fm_status err;

    if ( pTag )
    {
        if (*value != 0)
        {
            if (payloadVlanTag == stagTypeA)
            {
                /* vlan type is set to user defined type A */
                *value |= (FM_VTYPE_USER_A << 28);
            }
            else if (payloadVlanTag == stagTypeB)
            {
                /* vlan type is set to user defined type B */
                *value |= (FM_VTYPE_USER_B << 28);
            }
            else
            {
                /* vlan type is set to 0x8100 */
                *value |= (FM_VTYPE_8100 << 28);
            }
        }
        else
        {
            fm_uint16 vlanId;

            /* For an untagged frame use the pvid of port 0 */
            err = fmGetPortDefVlanInt(sw, cpuPort, &vlanId);
            FM_LOG_EXIT_ON_ERR(FM_LOG_CAT_EVENT_PKT_TX, err);

            *value = vlanId | (FM_VTYPE_8100 << 28);
        }
    }
    else
    {
        /* egress port is untagging */
        *value |= (FM_VTYPE_NONE << 28);
    }

    return FM_OK;

}   /* end ffm4000SetIslVType */

