/* vim:ts=4:sw=4:expandtab
 * (No tabs, indent level is 4 spaces)  */
/*****************************************************************************
 * File:            platform_app.h
 * Creation Date:   January 8, 2013
 * Description:     Platform layer application services.
 * 
 * Defines services that are generally needed by all platforms and may be
 * called upon directly by the system application software.
 * 
 * INTEL CONFIDENTIAL
 * Copyright 2005 - 2013 Intel Corporation. All Rights Reserved. 
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

#ifndef __FM_PLATFORM_APP_H
#define __FM_PLATFORM_APP_H


/**************************************************
 * Platform attributes
 **************************************************/

fm_status fmPlatformGetAttribute(fm_int    sw,
                                 fm_int    index,
                                 fm_uint32 attr,
                                 void *    value);

fm_status fmPlatformSetAttribute(fm_int    sw,
                                 fm_int    index,
                                 fm_uint32 attr,
                                 void *    value);


/**************************************************
 * I2C services
 **************************************************/

fm_status fmPlatformI2cRead(fm_int     sw,
                            fm_int     bus,
                            fm_int     address,
                            fm_uint32 *value,
                            fm_int     length);

fm_status fmPlatformI2cWrite(fm_int    sw,
                             fm_int    bus,
                             fm_int    address,
                             fm_uint32 value,
                             fm_int    length);

fm_status fmPlatformI2cWriteLong(fm_int    sw,
                                 fm_int    bus,
                                 fm_int    address,
                                 fm_uint64 data,
                                 fm_int    length);

fm_status fmPlatformI2cWriteRead(fm_int     sw,
                                 fm_int     bus,
                                 fm_int     address,
                                 fm_uint32 *data,
                                 fm_int     write_length,
                                 fm_int     read_length);


/**************************************************
 * MDIO services
 **************************************************/

fm_status fmPlatformMdioRead(fm_int     sw,
                             fm_int     bus,
                             fm_int     address,
                             fm_int     device,
                             fm_int     reg,
                             fm_uint16 *value);

fm_status fmPlatformMdioWrite(fm_int    sw,
                              fm_int    bus,
                              fm_int    address,
                              fm_int    device,
                              fm_int    reg,
                              fm_uint16 value);


#endif /* __FM_PLATFORM_APP_H */

