/* vim:ts=4:sw=4:expandtab
 * (No tabs, indent level is 4 spaces)  */
/*****************************************************************************
 * File:            platform_app_stubs.c
 * Creation Date:   January 11, 2013
 * Description:     Default implementations of Platform Application
 * 					service functions.
 *
 * INTEL CONFIDENTIAL
 * Copyright 2006 - 2013 Intel Corporation. All Rights Reserved. 
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

static int dummyVariable;


/*****************************************************************************
 * Local function prototypes.
 *****************************************************************************/


/*****************************************************************************
 * Local Functions
 *****************************************************************************/


/*****************************************************************************
 * Public Functions
 *****************************************************************************/




#if !defined(FM_HAVE_fmPlatformMdioRead)

/*****************************************************************************/
/** fmPlatformMdioRead
 * \ingroup platformApp
 *
 * \desc            Read 16 bits of data from the MDIO bus.
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \param[in]       type is a bit mask specifying MDIO access options.
 *                  See ''MDIO Management Options''.
 *
 * \param[in]       addr is the MDIO address.
 *
 * \param[in]       dev is the device on the MDIO port. dev is only used if
 *                  type includes the FM_SMGMT_MDIO_10G bit, otherwise it
 *                  is ignored.
 *
 * \param[in]       reg is the register number on the MDIO device.
 *
 * \param[out]      data contains the value read from the MDIO device.
 *
 * \return          FM_OK if successful.
 *
 *****************************************************************************/
fm_int fmPlatformMdioRead(fm_int     sw,
                          fm_int     type,
                          fm_int     addr,
                          fm_int     dev,
                          fm_int     reg,
                          fm_uint16 *data)
{
    FM_NOT_USED(sw);
    FM_NOT_USED(type);
    FM_NOT_USED(addr);
    FM_NOT_USED(dev);
    FM_NOT_USED(reg);
    FM_NOT_USED(data);

    FM_LOG_EXIT_VERBOSE(FM_LOG_CAT_PLATFORM, FM_ERR_UNSUPPORTED);

}   /* end fmPlatformMdioRead */

#endif




#if !defined(FM_HAVE_fmPlatformMdioWrite)

/*****************************************************************************/
/** fmPlatformMdioWrite
 * \ingroup platformApp
 *
 * \desc            Write 16 bits of data to the MDIO bus.
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \param[in]       type is a bit mask specifying MDIO access options.
 *                  See ''MDIO Management Options''.
 *
 * \param[in]       addr is the MDIO physical address.
 *
 * \param[in]       dev is the device on the MDIO port. dev is only used if
 *                  type includes the FM_SMGMT_MDIO_10G bit, otherwise it
 *                  is ignored.
 *
 * \param[in]       reg is the register number on the MDIO device.
 *
 * \param[in]       data contains the value to write to the MDIO device.
 *
 * \return          FM_OK if successful.
 *
 *****************************************************************************/
fm_int fmPlatformMdioWrite(fm_int    sw,
                           fm_int    type,
                           fm_int    addr,
                           fm_int    dev,
                           fm_int    reg,
                           fm_uint16 data)
{
    FM_NOT_USED(sw);
    FM_NOT_USED(type);
    FM_NOT_USED(addr);
    FM_NOT_USED(dev);
    FM_NOT_USED(reg);
    FM_NOT_USED(data);

    FM_LOG_EXIT_VERBOSE(FM_LOG_CAT_PLATFORM, FM_ERR_UNSUPPORTED);

}   /* end fmPlatformMdioWrite */

#endif

