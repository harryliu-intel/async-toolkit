/* vim:et:sw=4:ts=4:tw=79:
 * (No tabs, indent level is 4 spaces)  */
/*****************************************************************************
 * File:            hlp_api_init.c
 * Creation Date:   December 3, 2012
 * Description:     FM5xxx specific VLAN functions
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
 * Local Function Prototypes
 *****************************************************************************/

/*****************************************************************************
 * Local Functions
 *****************************************************************************/

/*****************************************************************************
 * Public Functions
 *****************************************************************************/

/*****************************************************************************/
/** hlpAllocateVlanTableDataStructures
 * \ingroup intVlan
 *
 * \desc            Allocates the state structure for VLAN management.
 *
 * \param[in]       switchPtr points to the switch state table
 *
 * \return          FM_OK if successful.
 *
 *****************************************************************************/
fm_status hlpAllocateVlanTableDataStructures(fm_switch *switchPtr)
{
    FM_LOG_ENTRY( FM_LOG_CAT_VLAN_FM4000,
                  "switchPtr=%p<sw=%d>\n",
                  (void *) switchPtr,
                  switchPtr != NULL ? switchPtr->switchNumber : -1 );

    if (switchPtr == NULL)
    {
        FM_LOG_EXIT(FM_LOG_CAT_VLAN_FM4000, FM_ERR_INVALID_ARGUMENT);
    }

    /* TODO Allocation of the HLP specific VLAN entry must be added here. */

    FM_LOG_EXIT(FM_LOG_CAT_VLAN, FM_OK);

}   /* end hlpAllocateVlanTableDataStructures */

