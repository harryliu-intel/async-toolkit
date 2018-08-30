/* vim:ts=4:sw=4:expandtab
 * (No tabs, indent level is 4 spaces)  */
/*****************************************************************************
 * File:            platform_xcvr.h
 * Creation Date:   2013
 * Description:     Platform transceiver specific definitions
 *
 * INTEL CONFIDENTIAL
 * Copyright 2011-2013  Intel Corporation. All Rights Reserved.
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

#ifndef __FM_PLATFORM_XCVR_H
#define __FM_PLATFORM_XCVR_H

typedef enum
{
    FM_PLATFORM_XCVR_TYPE_UNKNOWN,        /* Unable to decode tranceiver type */

    FM_PLATFORM_XCVR_TYPE_SX,             /* Optical transceiver */

    FM_PLATFORM_XCVR_TYPE_DAC,            /* Direct Attached Copper cable */

    FM_PLATFORM_XCVR_TYPE_NOT_PRESENT,    /* Transceiver is not present */

} fm_platformXcvrType;


fm_text fmPlatformXcvrTypeGetName(fm_platformXcvrType type);
fm_uint fmPlatformXcvrEepromIsBaseCsumValid(fm_byte *eeprom);
fm_uint fmPlatformXcvrEepromIsExtCsumValid(fm_byte *eeprom);

void fmPlatformXcvrEepromDumpBaseExt(fm_byte *eeprom);
void fmPlatformXcvrSfppEepromDumpPage1(fm_byte *eeprom);
void fmPlatformXcvrQsfpEepromDumpPage0(fm_byte *eeprom);

fm_uint fmPlatformXcvrEepromGetLen(fm_byte *eeprom);
fm_platformXcvrType fmPlatformXcvrEepromGetType(fm_byte *eeprom);
fm_bool fmPlatformXcvrIs10G1G(fm_byte *eeprom);

#endif /* __FM_PLATFORM_XCVR_H */
