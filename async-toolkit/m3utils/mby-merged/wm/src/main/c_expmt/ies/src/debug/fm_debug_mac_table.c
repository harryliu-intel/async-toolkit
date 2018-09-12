/* vim:ts=4:sw=4:expandtab
 * (No tabs, indent level is 4 spaces)  */
/*****************************************************************************
 * File:            fm_debug_mac_table.c
 * Creation Date:   April 27, 2006
 * Description:     Provide debugging functions.
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
#define FUNC_DBG_LVL             \
    (FM_DBG_LVL_ALL_FUNC_TRACE | \
     FM_DBG_LVL_DEBUG_FUNC_TRACE)


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
/** fmDbgTableUpdateStatsDump
 * \ingroup diagMATable 
 *
 * \chips           FM2000, FM3000, FM4000
 *
 * \desc            Display benchmarking statistics on the MA Table maintenance
 *                  handler. These statistics indicate how long it takes to
 *                  update the memory-based copy of the MA Table from the
 *                  hardware.
 *
 * \note            Statistics are not switch-specific.
 *
 * \param           None.
 *
 * \return          None.
 *
 *****************************************************************************/
void fmDbgTableUpdateStatsDump(void)
{
    fm_status err;
    fm_int    i;
    fm_int    avgCount   = 0;
    fm_uint   average    = 0;
    fm_uint   historyMax = 0;
    fm_uint   historyMin = (fm_uint) - 1;

    err = fmCaptureLock(&fmRootApi->tableUpdateStats.lck, FM_WAIT_FOREVER);

    if (err == FM_OK)
    {
        /**************************************************
         * Calculate running average.
         **************************************************/

        for (i = 0 ; i < TABLE_UPDATE_STATS_HISTORY_SIZE ; i++)
        {
            if (fmRootApi->tableUpdateStats.history[i] != (fm_uint) - 1)
            {
                average += fmRootApi->tableUpdateStats.history[i];
                ++avgCount;

                if (historyMax < fmRootApi->tableUpdateStats.history[i])
                {
                    historyMax = fmRootApi->tableUpdateStats.history[i];
                }

                if (historyMin > fmRootApi->tableUpdateStats.history[i])
                {
                    historyMin = fmRootApi->tableUpdateStats.history[i];
                }
            }
        }

        if (avgCount != 0)
        {
            average /= avgCount;
        }

        /**************************************************
         * Dump the results
         **************************************************/

        FM_LOG_PRINT("\n");

        FM_LOG_PRINT("Average time of last %3d scans (ms) %10d\n", 
                     avgCount,
                     average / 1000);

        FM_LOG_PRINT("Maximum of last %3d scans (ms)      %10d\n", 
                     avgCount,
                     historyMax / 1000);

        FM_LOG_PRINT("Minimum of last %3d scans (ms)      %10d\n", 
                     avgCount,
                     historyMin / 1000);

        FM_LOG_PRINT("Maximum time since last clear (ms)  %10d\n", 
                     fmRootApi->tableUpdateStats.maxTime / 1000);

        FM_LOG_PRINT("Minimum time since last clear (ms)  %10d\n", 
                     fmRootApi->tableUpdateStats.minTime / 1000);

        FM_LOG_PRINT("\n");

        err = fmReleaseLock(&fmRootApi->tableUpdateStats.lck);

        if (err != FM_OK)
        {
            FM_LOG_PRINT(
                "ERROR: Unable to release table update statistics sempahore.\n");
        }
    }
    else
    {
        FM_LOG_PRINT(
            "ERROR: Unable to capture table update statistics sempahore.\n");
    }

}   /* end fmDbgTableUpdateStatsDump */




/*****************************************************************************/
/** fmDbgTableUpdateStatsClear
 * \ingroup diagMATable 
 *
 * \chips           FM2000, FM3000, FM4000
 *
 * \desc            Reset the benchmarking statistics for the MA Table
 *                  maintenance handler.
 *
 * \note            Statistics are not switch-specific.
 *
 * \param           None.
 *
 * \return          None.
 *
 *****************************************************************************/
void fmDbgTableUpdateStatsClear(void)
{
    fm_status err;
    fm_int    i;

    err = fmCaptureLock(&fmRootApi->tableUpdateStats.lck, FM_WAIT_FOREVER);

    if (err == FM_OK)
    {
        /**************************************************
         * Reset the max and min and clear out existing
         * entries.
         **************************************************/

        fmRootApi->tableUpdateStats.maxTime = 0;
        fmRootApi->tableUpdateStats.minTime = (fm_uint) -1;

        for (i = 0 ; i < TABLE_UPDATE_STATS_HISTORY_SIZE ; i++)
        {
            fmRootApi->tableUpdateStats.history[i] = (fm_uint) -1;
        }

        err = fmReleaseLock(&fmRootApi->tableUpdateStats.lck);

        if (err != FM_OK)
        {
            FM_LOG_PRINT(
                "ERROR: Unable to release table update statistics sempahore.\n");
        }
    }
    else
    {
        FM_LOG_PRINT(
            "ERROR: Unable to capture table update statistics sempahore.\n");
    }

}   /* end fmDbgTableUpdateStatsClear */




/*****************************************************************************/
/** fmDbgDumpMACTable
 * \ingroup diagMATable 
 *
 * \chips           FM2000, FM3000, FM4000, FM6000
 *
 * \desc            Display a given number of entries or count the number of
 *                  entries in the switch's MAC Address Table.
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \param[in]       numEntries is the number of entries to display.  Specify
 *                  0 for the first 10 entries only.  Specify -1 to just count
 *                  the number of entries in the table without displaying them.
 *
 * \return          None.
 *
 *****************************************************************************/
void fmDbgDumpMACTable(fm_int sw, fm_int numEntries)
{
    fm_switch *switchPtr;
    fm_status  err;
    
    VALIDATE_AND_PROTECT_SWITCH_NO_RETURN(err, sw);
    
    if (err != FM_OK)
    {
        FM_LOG_PRINT("Switch %d does not exist or is down.\n",
                     sw);
        return;
    }

    switchPtr = fmRootApi->fmSwitchStateTable[sw];

    FM_API_CALL_FAMILY_VOID(switchPtr->DbgDumpMACTable, sw, numEntries);

    UNPROTECT_SWITCH(sw);
        
}   /* end fmDbgDumpMACTable */




/*****************************************************************************/
/** fmDbgDumpMACCache
 * \ingroup diagMATable
 *
 * \chips           FM2000, FM3000, FM4000
 *
 * \desc            Display a given number of entries or count the number of
 *                  entries in the memory-based copy of the switch's MAC
 *                  Address Table.
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \param[in]       numEntries is the number of entries to display.  Specify
 *                  0 for the first 10 entries only.  Specify -1 to just count
 *                  the number of entries in the table without displaying them.
 *
 * \return          None.
 *
 *****************************************************************************/
void fmDbgDumpMACCache(fm_int sw, fm_int numEntries)
{
    fm_switch *switchPtr;

    switchPtr = fmRootApi->fmSwitchStateTable[sw];

    FM_API_CALL_FAMILY_VOID(switchPtr->DbgDumpMACCache, sw, numEntries);

}   /* end fmDbgDumpMACCache */




/*****************************************************************************/
/** fmDbgDumpMACTableEntry
 * \ingroup diagMATable 
 *
 * \chips           FM2000, FM3000, FM4000, FM6000
 *
 * \desc            Display a specific entry in the switch's MAC Address Table.
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \param[in]       vlan is the VLAN ID of the entry to display.
 *
 * \param[in]       addressStr is the MAC address of the entry to display
 *                  formatted as "XX:XX:XX:XX:XX:XX" or "XXXXXXXXXXXX".
 *
 * \return          None.
 *
 *****************************************************************************/
void fmDbgDumpMACTableEntry(fm_int sw, fm_uint16 vlan, fm_text addressStr)
{
    fm_int     i;
    fm_int     scanCount;
    fm_uint32  words[6];
    fm_macaddr address;
    fm_switch *switchPtr;

    switchPtr = fmRootApi->fmSwitchStateTable[sw];

    /**************************************************
     * Parse the specified MAC address.
     **************************************************/

    scanCount = FM_SSCANF_S(addressStr,
                            "%2x:%2x:%2x:%2x:%2x:%2x",
                            &words[0],
                            &words[1],
                            &words[2],
                            &words[3],
                            &words[4],
                            &words[5]);

    if (scanCount != 6)
    {
        /**************************************************
         * Try again without colons.
         **************************************************/
        scanCount = FM_SSCANF_S(addressStr,
                                "%2x%2x%2x%2x%2x%2x",
                                &words[0],
                                &words[1],
                                &words[2],
                                &words[3],
                                &words[4],
                                &words[5]);

        if (scanCount != 6)
        {
            FM_LOG_PRINT("%s is not a valid MAC address.\n",
                         addressStr);
            return;
        }
    }

    address = FM_LITERAL_64(0);

    /* Pack bytes into a single address variable */
    for (i = 0 ; i < 6 ; i++)
    {
        address <<= 8;
        address  |= words[i];
    }

    FM_API_CALL_FAMILY_VOID(switchPtr->DbgDumpMACTableEntry,
                            sw,
                            address,
                            vlan);

}   /* end fmDbgDumpMACTableEntry */




/*****************************************************************************/
/** fmDbgTraceMACAddress
 * \ingroup diagMATable 
 *
 * \chips           FM2000, FM3000, FM4000, FM6000
 *
 * \desc            Set a MAC address that should be traced. Diagnostic
 *                  messages will be output for all MA Table operations on
 *                  the specified MAC address.
 *
 * \param[in]       macAddr is the MAC address to trace. Set to zero to
 *                  turn off tracing.
 *
 * \return          None.
 *
 *****************************************************************************/
void fmDbgTraceMACAddress(fm_macaddr macAddr)
{
    fmRootApi->testTraceMacAddress = macAddr;

}   /* end fmDbgTraceMACAddress */
