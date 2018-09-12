/* vim:ts=4:sw=4:expandtab
 * (No tabs, indent level is 4 spaces)  */
/*****************************************************************************
 * File:            fm_api_attr.c
 * Creation Date:   2005
 * Description:     Functions for manipulating high level attributes of a switch
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


#include <fm_sdk_int.h>


/*****************************************************************************
 * Macros, Constants & Types
 *****************************************************************************/
/* Hardware constants (SYS_CFG_1) */

#define SC1_BROADCAST_DISABLE  0x8000
#define SC1_FLOOD_CTRL_MCAST   0x4000
#define SC1_FLOOD_CTRL_UCAST   0x2000
#define SC1_DROP_PAUSE         0x0400
#define SC1_REMAP_ET_SP15      0x0200
#define SC1_REMAP_CPU_SP15     0x0100
#define SC1_REMAP_IEEE_SP15    0x0080
#define SC1_BROADCAST_CTRL     0x0040
#define SC1_TRAP_8021X         0x0020
#define SC1_TRAP_IGMPV3        0x0010
#define SC1_TRAP_GARP          0x0008
#define SC1_TRAP_BPDU          0x0004
#define SC1_TRAP_LACP          0x0002
#define SC1_TRAP_OTHER         0x0001


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


/*****************************************************************************
 * Public Functions
 *****************************************************************************/


/*****************************************************************************/
/** fmGetSwitchAttribute
 * \ingroup switch
 *
 * \chips           FM2000, FM3000, FM4000, FM6000
 *
 * \desc            Retrieve a switch attribute value.
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \param[in]       attr is the switch attribute to retrieve
 *                  (see 'Switch Attributes').
 *
 * \param[out]      value points to caller-allocated storage where this
 *                  function is to place the attribute value.
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_INVALID_SWITCH if sw is invalid.
 * \return          FM_ERR_INVALID_ATTRIB unrecognized attr.
 *
 *****************************************************************************/
fm_status fmGetSwitchAttribute(fm_int sw, fm_int attr, void *value)
{
    fm_status  err;
    fm_switch *switchPtr;

    FM_LOG_ENTRY_API(FM_LOG_CAT_ATTR, "sw=%d attr=%d value=%p\n",
                     sw, attr, value);

    VALIDATE_AND_PROTECT_SWITCH(sw);

    switchPtr = fmRootApi->fmSwitchStateTable[sw];

    FM_API_CALL_FAMILY(err, switchPtr->GetSwitchAttribute, sw, attr, value);

    UNPROTECT_SWITCH(sw);

    FM_LOG_EXIT_API(FM_LOG_CAT_ATTR, err);

}   /* end fmGetSwitchAttribute */




/*****************************************************************************/
/** fmSetSwitchAttribute
 * \ingroup switch
 *
 * \chips           FM2000, FM3000, FM4000, FM6000
 *
 * \desc            Set a switch attribute value.
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \param[in]       attr is the switch attribute to set (see 'Switch Attributes').
 *
 * \param[in]       value points to the attribute value to set.
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_INVALID_SWITCH if sw is invalid.
 * \return          FM_ERR_INVALID_ARGUMENT if value is invalid.
 * \return          FM_ERR_INVALID_ATTRIB if attr is not recognized.
 *
 *****************************************************************************/
fm_status fmSetSwitchAttribute(fm_int sw, fm_int attr, void *value)
{
    fm_status               err;
    fm_switch *             switchPtr;
    fm_bool                 switchLocked = FALSE;
    fm_ffuSliceAllocations *sliceAllocs;

    FM_LOG_ENTRY_API(FM_LOG_CAT_ATTR, "sw=%d attr=%d value=%p\n",
                     sw, attr, value);

    VALIDATE_AND_PROTECT_SWITCH(sw);

    switchPtr = fmRootApi->fmSwitchStateTable[sw];

    switch (attr)
    {
        case FM_FFU_SLICE_ALLOCATIONS:
            sliceAllocs = (fm_ffuSliceAllocations *) value;

            if ( ( (sliceAllocs->ipv4UnicastFirstSlice < 0)
                    && (sliceAllocs->ipv4UnicastLastSlice >= 0) )
                || ( (sliceAllocs->ipv4UnicastFirstSlice >= 0)
                    && (sliceAllocs->ipv4UnicastLastSlice < 0) ) )
            {
                err = FM_ERR_INVALID_ARGUMENT;
                goto ABORT;
            }

            if ( ( (sliceAllocs->ipv4MulticastFirstSlice < 0)
                    && (sliceAllocs->ipv4MulticastLastSlice >= 0) )
                || ( (sliceAllocs->ipv4MulticastFirstSlice >= 0)
                    && (sliceAllocs->ipv4MulticastLastSlice < 0) ) )
            {
                err = FM_ERR_INVALID_ARGUMENT;
                goto ABORT;
            }

            if ( ( (sliceAllocs->ipv6UnicastFirstSlice < 0)
                    && (sliceAllocs->ipv6UnicastLastSlice >= 0) )
                || ( (sliceAllocs->ipv6UnicastFirstSlice >= 0)
                    && (sliceAllocs->ipv6UnicastLastSlice < 0) ) )
            {
                err = FM_ERR_INVALID_ARGUMENT;
                goto ABORT;
            }

            if ( ( (sliceAllocs->ipv6MulticastFirstSlice < 0)
                    && (sliceAllocs->ipv6MulticastLastSlice >= 0) )
                || ( (sliceAllocs->ipv6MulticastFirstSlice >= 0)
                    && (sliceAllocs->ipv6MulticastLastSlice < 0) ) )
            {
                err = FM_ERR_INVALID_ARGUMENT;
                goto ABORT;
            }

            if ( ( (sliceAllocs->aclFirstSlice < 0)
                    && (sliceAllocs->aclLastSlice >= 0) )
                || ( (sliceAllocs->aclFirstSlice >= 0)
                    && (sliceAllocs->aclLastSlice < 0) ) )
            {
                err = FM_ERR_INVALID_ARGUMENT;
                goto ABORT;
            }

            if ( ( (sliceAllocs->cVlanFirstSlice < 0)
                    && (sliceAllocs->cVlanLastSlice >= 0) )
                || ( (sliceAllocs->cVlanFirstSlice >= 0)
                    && (sliceAllocs->cVlanLastSlice < 0) ) )
            {
                err = FM_ERR_INVALID_ARGUMENT;
                goto ABORT;
            }

            LOCK_SWITCH(sw);
            switchLocked = TRUE;
            break;
    }

    FM_API_CALL_FAMILY(err, switchPtr->SetSwitchAttribute, sw, attr, value);

ABORT:

    if (switchLocked)
    {
        UNLOCK_SWITCH(sw);
    }

    UNPROTECT_SWITCH(sw);


    FM_LOG_EXIT_API(FM_LOG_CAT_ATTR, err);

}   /* end fmSetSwitchAttribute */




/*****************************************************************************/
/** fmStopSwitchTraffic
 * \ingroup switch
 *
 * \chips           FM2000, FM3000, FM4000, FM6000
 *
 * \desc            Stop the traffic going through the switch and drain the memory.
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \return          FM_OK if successful.
 *
 *****************************************************************************/
fm_status fmStopSwitchTraffic(fm_int sw)
{
    fm_status               err;
    fm_switch *             switchPtr;

    FM_LOG_ENTRY_API(FM_LOG_CAT_SWITCH, "sw=%d\n", sw);

    VALIDATE_AND_PROTECT_SWITCH(sw);

    switchPtr = fmRootApi->fmSwitchStateTable[sw];

    FM_API_CALL_FAMILY(err, switchPtr->StopTraffic, sw);

    UNPROTECT_SWITCH(sw);

    FM_LOG_EXIT_API(FM_LOG_CAT_SWITCH, err);

}   /* end fmStopSwitchTraffic */




/*****************************************************************************/
/** fmResartSwitchTraffic
 * \ingroup switch
 *
 * \chips           FM2000, FM3000, FM4000, FM6000
 *
 * \desc            Restarts traffic on the specified switch following 
 *                  a call to fmStopSwitchTraffic.
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \return          FM_OK if successful.
 *
 *****************************************************************************/
fm_status fmRestartSwitchTraffic(fm_int sw)
{
    fm_status               err;
    fm_switch *             switchPtr;

    FM_LOG_ENTRY_API(FM_LOG_CAT_SWITCH, "sw=%d\n", sw);

    VALIDATE_AND_PROTECT_SWITCH(sw);

    switchPtr = fmRootApi->fmSwitchStateTable[sw];

    FM_API_CALL_FAMILY(err, switchPtr->RestartTraffic, sw);

    UNPROTECT_SWITCH(sw);

    FM_LOG_EXIT_API(FM_LOG_CAT_SWITCH, err);

}   /* end fmRestartSwitchTraffic */


