/* vim:ts=4:sw=4:expandtab
 * (No tabs, indent level is 4 spaces)  */
/*****************************************************************************
 * File:            hlp_model_platform.c
 * Creation Date:   June 18, 2012
 * Description:     HLP specific functions for white model platform.
 *
 * INTEL CONFIDENTIAL
 * Copyright 2012 Intel Corporation. All Rights Reserved.
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

#include <fm_sdk_hlp_int.h>
#include <platforms/common/model/hlp/hlp_model.h>

/*****************************************************************************
 * Public Functions
 *****************************************************************************/

/*****************************************************************************/
/** hlpModelPlatformCheckBpduDropping
 * \ingroup intPlatform
 *
 * \desc            Checks whether the received packet is a BPDU packet and if
 *                  so, whether the BPDU packet should be trapped to the CPU or
 *                  silently dropped.
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \param[in]       pktEvent points to the ''fm_eventPktRecv'' structure
 *                  describing the received packet.
 *
 * \param[in]       vlan is the VLAN number on which the packet has been
 *                  received.
 *
 * \param[out]      dropBpdu points to caller-allocated storage in which this
 *                  function stores the drop decision.
 *
 * \return          FM_OK if successful.
 * \return          Other ''Status Codes'' as appropriate in case of failure.
 *
 *****************************************************************************/
fm_status hlpModelPlatformCheckBpduDropping(fm_int           sw,
                                               fm_eventPktRecv *pktEvent,
                                               fm_int           vlan,
                                               fm_bool *        dropBpdu)
{
    FM_NOT_USED(sw);
    FM_NOT_USED(pktEvent);
    FM_NOT_USED(vlan);

    /* Never silently drop BPDU packets. */
    *dropBpdu = FALSE;
    return FM_OK;

}   /* end hlpModelPlatformCheckBpduDropping */




/*****************************************************************************/
/** hlpModelPlatformGetCpuMaxFrameSize
 * \ingroup intPlatform
 *
 * \desc            Returns the maximum frame size supported by the CPU port.
 *
 * \param[in]       sw is the switch on which to operate. The switch number
 *                  must have already been validated.
 *
 * \param[out]      maxFrameSize points to caller-allocated storage where this
 *                  function places the maximum frame size supported by the CPU
 *                  port.
 *
 * \return          FM_OK if successful.
 * \return          Other ''Status Codes'' as appropriate in case of failure.
 *
 *****************************************************************************/
fm_status hlpModelPlatformGetCpuMaxFrameSize(fm_int     sw,
                                                fm_uint32 *maxFrameSize)
{
    FM_NOT_USED(sw);

    *maxFrameSize = 0x180;
    return FM_OK;

}   /* end hlpModelPlatformGetCpuMaxFrameSize */




/*****************************************************************************/
/** hlpModelPlatformGetCpuVlanTag
 * \ingroup intPlatform
 *
 * \desc            Returns the VLAN tagging mode for the CPU port.
 *
 * \param[in]       sw is the switch on which to operate. The switch number
 *                  must have already been validated.
 *
 * \param[in]       vlan is the VLAN number whose tagging mode is to be
 *                  returned.
 *
 * \param[out]      tag points to caller-allocated storage where this function
 *                  places the VLAN tagging mode for the CPU port.
 *
 * \return          FM_OK if successful.
 * \return          Other ''Status Codes'' as appropriate in case of failure.
 *
 *****************************************************************************/
fm_status hlpModelPlatformGetCpuVlanTag(fm_int   sw,
                                           fm_int   vlan,
                                           fm_bool *tag)
{
    FM_NOT_USED(sw);
    FM_NOT_USED(vlan);

    *tag = FALSE;
    return FM_OK;

}   /* end hlpModelPlatformGetCpuVlanTag */




/*****************************************************************************/
/** hlpModelPlatformGetNumPorts
 * \ingroup intPlatform
 *
 * \desc            Returns the number of ports supported by the HLP switch
 *                  model.
 *
 * \param[in]       sw is the switch on which to operate. The switch number
 *                  must already have been validated.
 *
 * \param[out]      numPorts points to caller-allocated storage where this
 *                  function places the number of supported ports.
 *
 * \return          FM_OK if successful.
 * \return          Other ''Status Codes'' as appropriate in case of failure.
 *
 *****************************************************************************/
fm_status hlpModelPlatformGetNumPorts(fm_int sw, fm_int *numPorts)
{
    FM_NOT_USED(sw);

    /* need to return number of fabric physical ports for fm_model_packet_queue
     * to create the correct number of sockets, see also bug 24962 */
    *numPorts = HLP_MAX_FABRIC_PHYS_PORT + 1;

    return FM_OK;

}   /* end hlpModelPlatformGetNumPorts */

