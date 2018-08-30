/* vim:ts=4:sw=4:expandtab
 * (No tabs, indent level is 4 spaces)  */
/*****************************************************************************
 * File:            fm_api_fibm.c
 * Creation Date:   June 20, 2008
 * Description:     Helper Functions for FIBM
 *
 * INTEL CONFIDENTIAL
 * Copyright 2006 - 2011 Intel Corporation. All Rights Reserved. 
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
/** fmFibmSlaveGetMasterSwitch
 * \ingroup intFibm
 *
 * \desc            A helper function to return the master switch
 *                  given a slave switch.
 *
 * \param[in]       slaveSw is switch number.
 *
 * \return          master switch or the same input value if FIBM not valid.
 *
 *****************************************************************************/
fm_int fmFibmSlaveGetMasterSwitch(fm_int slaveSw)
{
    fm_switch *         switchPtr;
    fm_status           err;
    fm_fibmSwitchConfig cfg;

    switchPtr = GET_SWITCH_PTR(slaveSw);

    if (switchPtr == NULL || !switchPtr->GetFibmSwitchConfig)
    {
        return slaveSw;
    }

    err = switchPtr->GetFibmSwitchConfig(slaveSw, &cfg);

    if (err != FM_OK)
    {
        return slaveSw;
    }

    /* For standalone NIC, it will be -1, but we just return
     * -1 and let the caller handle appropriately
     */

    return cfg.masterSwitch;

} /* end fmFibmSlaveGetMasterSwitch */




/*****************************************************************************/
/** fmFibmSlaveIsLogicalPortMgmt
 * \ingroup intFibm
 *
 * \desc            A helper function to return whether a logical port on
 *                  the slave switch is used for FIBM mgmt traffic.
 *
 * \param[in]       slaveSw is switch number.
 *
 * \param[in]       logicalPort is logical port number.
 *
 * \return          TRUE if the port is used for FIBM mgmt traffic.
 * \return          FALSE if the port is not used for FIBM mgmt traffic.
 *
 *****************************************************************************/
fm_bool fmFibmSlaveIsLogicalPortMgmt(fm_int slaveSw, fm_int logicalPort)
{
    fm_switch *         switchPtr;
    fm_status           err;
    fm_fibmSwitchConfig cfg;

    switchPtr = GET_SWITCH_PTR(slaveSw);

    if (switchPtr == NULL || !switchPtr->GetFibmSwitchConfig)
    {
        return FALSE;
    }

    err = switchPtr->GetFibmSwitchConfig(slaveSw, &cfg);

    if (err != FM_OK)
    {
        return FALSE;
    }

    if (cfg.slaveMgmtPort == logicalPort)
    {
        return TRUE;
    }

    return FALSE;
} /* end fmFibmSlaveIsLogicalPortMgmt */




/*****************************************************************************/
/** fmFibmSlaveIsPortMgmt
 * \ingroup intFibm
 *
 * \desc            A helper function to return whether a physical port on
 *                  the slave switch is used for FIBM mgmt traffic.
 *
 * \param[in]       slaveSw is switch number.
 *
 * \param[in]       physPort is physical port number.
 *
 * \return          TRUE if the port is used for FIBM mgmt traffic.
 * \return          FALSE if the port is not used for FIBM mgmt traffic.
 *
 *****************************************************************************/
fm_bool fmFibmSlaveIsPortMgmt(fm_int slaveSw, fm_int physPort)
{
    fm_switch *         switchPtr;
    fm_status           err;
    fm_fibmSwitchConfig cfg;
    fm_int              port;

    switchPtr = GET_SWITCH_PTR(slaveSw);

    if (switchPtr == NULL || !switchPtr->GetFibmSwitchConfig)
    {
        return FALSE;
    }

    err = switchPtr->GetFibmSwitchConfig(slaveSw, &cfg);

    if (err != FM_OK)
    {
        return FALSE;
    }

    if ( FM_OK != fmMapLogicalPortToPhysical(switchPtr, 
                                             cfg.slaveMgmtPort, 
                                             &port) )
    {
        return FALSE;
    }

    if (physPort == port)
    {
        return TRUE;
    }

    return FALSE;
} /* end fmFibmSlaveIsPortMgmt */

/*****************************************************************************/
/** fmFibmStartBatching
 * \ingroup intFibm
 *
 * \desc            Start batching of write commands. Specify TRUE to start
 *                  batching of write commands, and FALSE to stop batching
 *                  write commands and flush the pending buffer.
 *
 * \note            It is assumed that the caller has already validated and
 *                  protected the switch prior to calling this function.
 *
 * \param[in]       sw is switch number.
 *
 * \param[in]       start specifies where to TRUE or FALSE
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_UNSUPPORTED if the platform does not support FIBM
 *                  batching.
 * \return          FM_ERR_UNINITIALIZED if switch is not initialized as an
 *                  FIBM slave.
 * \return          FM_ERR_FIBM_TIMEOUT if no response from remote switch.
 * \return          FM_ERR_NO_MEM if FIBM initialization error.
 * \return          FM_FAIL if FIBM internal processing error.
 *****************************************************************************/
fm_status fmFibmStartBatching(fm_int sw, fm_bool start)
{
    fm_status   err;
    fm_switch  *switchPtr;

    FM_LOG_ENTRY_VERBOSE(FM_LOG_CAT_FIBM, "sw = %d start %d\n", sw, start);

    switchPtr = GET_SWITCH_PTR(sw);

    if (fmRootApi->isSwitchFibmSlave[sw])
    {
        FM_API_CALL_FAMILY(err, switchPtr->EnableFibmBatching, sw, start);
    }
    else
    {
        err = FM_ERR_UNINITIALIZED;
    }

    FM_LOG_EXIT_VERBOSE(FM_LOG_CAT_FIBM, err);

} /* end fmFibmStartBatching */
