/* vim:ts=4:sw=4:expandtab
 * (No tabs, indent level is 4 spaces)  */
/*****************************************************************************
 * File:            platform_mgmt.c
 * Creation Date:   July 2, 2012
 * Description:
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

#include <fm_sdk_int.h>

/*****************************************************************************/
/** fmPlatformI2cWriteLong
 * \ingroup platformApp
 *
 * \desc            Write up to 8 bytes to an I2C device.
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \param[in]       bus is the I2C bus number.
 *
 * \param[in]       address is the I2C device address (0x00 - 0x7F).
 *
 * \param[in]       data points to an aggregate of 8-bit data values to write to
 *                  the device. The aggregate must be length bytes long.
 *
 * \param[in]       length is the number of data bytes to write.
 *
 * \return          FM_OK if successful.
 * \return          Other ''Status Codes'' as appropriate in case of
 *                  failure.
 *
 *****************************************************************************/
fm_status fmPlatformI2cWriteLong(fm_int    sw,
                                 fm_int    bus,
                                 fm_int    address,
                                 fm_uint64 data,
                                 fm_int    length)
{
    FM_NOT_USED(sw);
    FM_NOT_USED(bus);
    FM_NOT_USED(address);
    FM_NOT_USED(data);
    FM_NOT_USED(length);

    return FM_ERR_UNSUPPORTED;

}   /* end fmPlatformI2cWriteLong */




/*****************************************************************************/
/** fmPlatformReadRawCSR
 * \ingroup platform
 *
 * \desc            Read a CSR register without taking the platform lock.
 *                                                                      \lb\lb
 *                  The caller must have taken the platform lock, or a write
 *                  lock on the entire switch, prior to calling this function
 *                  and is responsible for releasing the lock.
 *                                                                      \lb\lb
 *                  This function may be called multiple times consecutively
 *                  for multiple word width registers since it is assumed that
 *                  the caller has taken any locks necessary to assure
 *                  atomicity.
 *                                                                      \lb\lb
 *                  On platforms supporting multiple switch types where a
 *                  single function cannot be used on different switches,
 *                  different versions of this function may be implemented
 *                  for different switch types and the function pointer in
 *                  the per-switch state initialized in 
 *                  ''fmPlatformSwitchInitialize'' to point to the appropriate
 *                  version of the function for each switch type.
 *
 * \note            This function is used by the switch self test function,
 *                  ''fmDbgSwitchSelfTest'' and need not be implemented on 
 *                  platforms where the self test will not be used.
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \param[in]       addr contains the CSR register address to read
 *
 * \param[out]      value points to storage where this function will place
 *                  the read register value.
 *
 * \return          FM_OK if successful.
 * \return          Other ''Status Codes'' as appropriate in case of
 *                  failure.
 *
 *****************************************************************************/
fm_status fmPlatformReadRawCSR(fm_int sw, fm_uint32 addr, fm_uint32 *value)
{
    FM_NOT_USED(sw);
    FM_NOT_USED(addr);
    FM_NOT_USED(value);

    return FM_ERR_UNSUPPORTED;

}   /* end fmPlatformReadRawCSR */




/*****************************************************************************/
/** fmPlatformWriteRawCSR
 * \ingroup platform
 *
 * \desc            Write a CSR register without taking the platform lock.
 *                                                                      \lb\lb
 *                  The caller must have taken the platform lock, or a write
 *                  lock on the entire switch, prior to calling this function
 *                  and is responsible for releasing the lock.
 *                                                                      \lb\lb
 *                  This function may be called multiple times consecutively
 *                  for multiple word width registers since it is assumed that
 *                  the caller has taken any locks necessary to assure
 *                  atomicity.
 *                                                                      \lb\lb
 *                  On platforms supporting multiple switch types where a
 *                  single function cannot be used on different switches,
 *                  different versions of this function may be implemented
 *                  for different switch types and the function pointer in
 *                  the per-switch state initialized in 
 *                  ''fmPlatformSwitchInitialize'' to point to the appropriate
 *                  version of the function for each switch type.
 *
 * \note            This function is used by the switch self test function,
 *                  ''fmDbgSwitchSelfTest'' and need not be implemented on 
 *                  platforms where the self test will not be used.
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \param[in]       addr contains the CSR register address to write.
 *
 * \param[in]       value is the data value to write to the register.
 *
 * \return          FM_OK if successful.
 * \return          Other ''Status Codes'' as appropriate in case of
 *                  failure.
 *
 *****************************************************************************/
fm_status fmPlatformWriteRawCSR(fm_int sw, fm_uint32 addr, fm_uint32 value)
{
    FM_NOT_USED(sw);
    FM_NOT_USED(addr);
    FM_NOT_USED(value);

    return FM_ERR_UNSUPPORTED;

}   /* end fmPlatformWriteRawCSR */

