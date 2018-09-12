/* vim:ts=4:sw=4:expandtab
 * (No tabs, indent level is 4 spaces)  */
/*****************************************************************************
 * File:            platform_event.c
 * Creation Date:   November 1, 2012
 * Description:     Platform Event Notification functions.
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

#include <fm_sdk_int.h>
#include <platforms/common/event/fm_platform_event.h>

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


static void PrintBytesName(fm_text name, fm_int addr, fm_byte buf[], fm_int len)
{
    fm_int i;

    if (name)
    {
        FM_LOG_PRINT("%20s[%02x]: ", name, addr);
    }

    for (i = 0 ; i < len ; i++)
    {
        if (isprint(buf[i]))
        {
            FM_LOG_PRINT("%c", buf[i]);
        }
    }
    FM_LOG_PRINT("\n");

}   /* end PrintBytesName */



/*****************************************************************************
 * Public Functions
 *****************************************************************************/


 /*****************************************************************************/
/* fmPlatformXcvrTypeGetName
 * \ingroup intPlatformXcvr
 *
 * \desc            Returns the string representation of the transceiver type
 *
 * \param[in]       type is the transceiver type.
 *
 * \return          String representation of the value.
 *
 *****************************************************************************/
fm_text fmPlatformXcvrTypeGetName(fm_platformXcvrType type)
{
    switch (type)
    {
        case FM_PLATFORM_XCVR_TYPE_UNKNOWN:
            return "XCVR_TYPE_UNKNOWN";
        case FM_PLATFORM_XCVR_TYPE_SX:
            return "XCVR_TYPE_SX";
        case FM_PLATFORM_XCVR_TYPE_DAC:
            return "XCVR_TYPE_DAC";
        case FM_PLATFORM_XCVR_TYPE_NOT_PRESENT:
            return "XCVR_TYPE_NOT_PRESENT";
    }

    return "XCVR_TYPE_UNHANDLED";

} /* fmPlatformXcvrTypeGetName */




/*****************************************************************************/
/** fmPlatformXcvrEepromIsBaseCsumValid
 * \ingroup intPlatformXcvr
 *
 * \desc            Returns whether base eeprom checksum is valid.
 *
 * \param[out]      eeprom points to storage where the transceiver eeprom
 *                  is stored.
 *
 * \return          TRUE is base eeprom check is valid.
 *
 *****************************************************************************/
fm_uint fmPlatformXcvrEepromIsBaseCsumValid(fm_byte *eeprom)
{
    fm_byte   cs;
    fm_int    k;

    for (k = 0, cs = 0 ; k <= 62 ; k++)
    {
        cs += eeprom[k];
    }

    return (cs == eeprom[63]);

} /* fmPlatformXcvrEepromIsBaseCsumValid */




/*****************************************************************************/
/** fmPlatformXcvrEepromIsExtCsumValid
 * \ingroup intPlatformXcvr
 *
 * \desc            Returns whether ext eeprom checksum is valid.
 *
 * \param[out]      eeprom points to storage where the transceiver eeprom
 *                  is stored.
 *
 * \return          TRUE is ext eeprom check is valid.
 *
 *****************************************************************************/
fm_uint fmPlatformXcvrEepromIsExtCsumValid(fm_byte *eeprom)
{
    fm_byte   cs;
    fm_int    k;

    for (k = 64, cs = 0 ; k <= 94 ; k++)
    {
        cs += eeprom[k];
    }

    return (cs == eeprom[95]);

} /* fmPlatformXcvrEepromIsExtCsumValid */




/*****************************************************************************/
/** fmPlatformXcvrEepromGetLen
 * \ingroup intPlatformXcvr
 *
 * \desc            Returns the transceiver cable length in the transceiver eeprom.
 *                  This assumes the eeprom checksum has been verified.
 *
 * \param[out]      eeprom points to storage where the transceiver eeprom
 *                  is stored.
 *
 * \return          Transceiver cable length, if applicable.
 *
 *****************************************************************************/
fm_uint fmPlatformXcvrEepromGetLen(fm_byte *eeprom)
{
    return eeprom[18];

} /* fmPlatformXcvrEepromGetLen */





/*****************************************************************************/
/** fmPlatformXcvrEepromGetType
 * \ingroup intPlatformXcvr
 *
 * \desc            Returns the transceiver type given the transceiver eeprom.
 *                  This assumes the eeprom checksum has been verified.
 *
 * \param[out]      eeprom points to storage where the transceiver eeprom
 *                  is stored.
 *
 * \return          fm_platformXcvrType
 *
 *****************************************************************************/
fm_platformXcvrType fmPlatformXcvrEepromGetType(fm_byte *eeprom)
{
    if (eeprom[0] == 0xD)
    {
        /* QSFP */
        /* Only bit3 at index 0 indicate non-optical */
        if ( (eeprom[3+0] & ~(1 << 3)) ||
             (eeprom[3+3] & (1 << 0)) )
        {
            return FM_PLATFORM_XCVR_TYPE_SX;
        }
        else
        {
            return FM_PLATFORM_XCVR_TYPE_DAC;
        }
    }
    if (eeprom[0] == 0x3)
    {
        /* 1000BASE-SX and 10GBASE-SR */
        if ( (eeprom[3+0] & (1 << 4)) ||
             (eeprom[3+3] & (1 << 0)) )
        {
            return FM_PLATFORM_XCVR_TYPE_SX;
        }
        else
        {
            return FM_PLATFORM_XCVR_TYPE_DAC;
        }
    }
    else
    {
        return FM_PLATFORM_XCVR_TYPE_UNKNOWN;
    }

} /* fmPlatformXcvrEepromGetType */



/*****************************************************************************/
/** fmPlatformXcvrEepromGetLen
 * \ingroup intPlatformXcvr
 *
 * \desc            Returns TRUE if the module support 10G and 1G speed.
 *                  This assumes the eeprom checksum has been verified.
 *
 * \param[out]      eeprom points to storage where the transceiver eeprom
 *                  is stored.
 *
 * \return          TRUE if dual rate module.
 *
 *****************************************************************************/
fm_bool fmPlatformXcvrIs10G1G(fm_byte *eeprom)
{
    /* 1000BASE-SX and 10GBASE-SR */
    return ( (eeprom[3+0] & (1 << 4)) &&
             (eeprom[3+3] & (1 << 0)) );
} /* fmPlatformXcvrIs10G1G */



/*****************************************************************************/
/** fmPlatformXcvrEepromDumpSpecCompliance
 * \ingroup intPlatformXcvr
 *
 * \desc            Dumps transceiver spec compliance.
 *                  This assumes the eeprom checksum has been verified.
 *
 * \param[out]      eeprom points to storage where the transceiver eeprom
 *                  is stored.
 *
 * \return          NONE.
 *
 *****************************************************************************/
void fmPlatformXcvrEepromDumpSpecCompliance(fm_byte *eeprom)
{

    FM_LOG_PRINT("%20s[%02x]:", "Spec Comp", 3);

    if (eeprom[3+0] & (1 << 7)) FM_LOG_PRINT(" Reserved(7)");
    if (eeprom[3+0] & (1 << 6)) FM_LOG_PRINT(" 10GBASE-LRM");
    if (eeprom[3+0] & (1 << 5)) FM_LOG_PRINT(" 10GBASE-LR");
    if (eeprom[3+0] & (1 << 4)) FM_LOG_PRINT(" 10GBASE-SR");
    if (eeprom[3+0] & (1 << 3)) FM_LOG_PRINT(" 40GBASE-CR4");
    if (eeprom[3+0] & (1 << 2)) FM_LOG_PRINT(" 40GBASE-SR4");
    if (eeprom[3+0] & (1 << 1)) FM_LOG_PRINT(" 40GBASE-LR4");
    if (eeprom[3+0] & (1 << 0)) FM_LOG_PRINT(" 40G-XLPPI");

    if (eeprom[3+1] & (1 << 3)) FM_LOG_PRINT(" 40G-OTN");
    if (eeprom[3+1] & (1 << 2)) FM_LOG_PRINT(" OC48-LongReach");
    if (eeprom[3+1] & (1 << 1)) FM_LOG_PRINT(" OC48-IntReach");
    if (eeprom[3+1] & (1 << 0)) FM_LOG_PRINT(" OC48-ShortReach");

    if (eeprom[3+2] & (1 << 5)) FM_LOG_PRINT(" SAS-6.0G");
    if (eeprom[3+2] & (1 << 4)) FM_LOG_PRINT(" SAS-3.0G");

    if (eeprom[3+3] & (1 << 3)) FM_LOG_PRINT(" 1000BASE-T");
    if (eeprom[3+3] & (1 << 2)) FM_LOG_PRINT(" 1000BASE-CX");
    if (eeprom[3+3] & (1 << 1)) FM_LOG_PRINT(" 1000BASE-LX");
    if (eeprom[3+3] & (1 << 0)) FM_LOG_PRINT(" 1000BASE-SX");

    if (eeprom[3+4] & (1 << 7)) FM_LOG_PRINT(" VLD");
    if (eeprom[3+4] & (1 << 6)) FM_LOG_PRINT(" SD");
    if (eeprom[3+4] & (1 << 5)) FM_LOG_PRINT(" ID");
    if (eeprom[3+4] & (1 << 4)) FM_LOG_PRINT(" LD");
    if (eeprom[3+4] & (1 << 3)) FM_LOG_PRINT(" M");
    if (eeprom[3+4] & (1 << 1)) FM_LOG_PRINT(" LC");
    if (eeprom[3+4] & (1 << 0)) FM_LOG_PRINT(" EL");

    if (eeprom[3+5] & (1 << 7)) FM_LOG_PRINT(" AL");
    if (eeprom[3+5] & (1 << 6)) FM_LOG_PRINT(" SN");
    if (eeprom[3+5] & (1 << 5)) FM_LOG_PRINT(" SL");
    if (eeprom[3+5] & (1 << 4)) FM_LOG_PRINT(" LL");

    if (eeprom[3+6] & (1 << 7)) FM_LOG_PRINT(" TwinAxial(TW)");
    if (eeprom[3+6] & (1 << 6)) FM_LOG_PRINT(" Shielded(TP)");
    if (eeprom[3+6] & (1 << 5)) FM_LOG_PRINT(" MiniCoax(MI)");
    if (eeprom[3+6] & (1 << 4)) FM_LOG_PRINT(" VideoCoax(TV)");
    if (eeprom[3+6] & (1 << 3)) FM_LOG_PRINT(" MMode62.5m(M6)");
    if (eeprom[3+6] & (1 << 2)) FM_LOG_PRINT(" MMode50m(M5)");
    if (eeprom[3+6] & (1 << 1)) FM_LOG_PRINT(" MMode50um(OM3)");
    if (eeprom[3+6] & (1 << 0)) FM_LOG_PRINT(" SingleMode(SM)");

    FM_LOG_PRINT("\n");

}   /* end fmPlatformXcvrEepromDumpSpecCompliance */





/*****************************************************************************/
/** fmPlatformXcvrEepromDumpBaseExt
 * \ingroup intPlatformXcvr
 *
 * \desc            Dumps transceiver BASE and EEPROM eeprom.
 *                  This assumes the eeprom checksum has been verified.
 *
 * \param[out]      eeprom points to storage where the transceiver eeprom
 *                  is stored.
 *
 * \return          NONE.
 *
 *****************************************************************************/
void fmPlatformXcvrEepromDumpBaseExt(fm_byte *eeprom)
{
    fm_int    j;
    fm_byte   cs;
    fm_int    addr;

    addr = 0;
    FM_LOG_PRINT("%20s[%02x]: ", "Identifier", addr);
    switch (eeprom[addr])
    {
        case 0x3: FM_LOG_PRINT("SFP(0x3)\n"); break;
        case 0xD: FM_LOG_PRINT("QSFP+(0xD)\n"); break;
        default: FM_LOG_PRINT("0x%02x\n", eeprom[addr]); break;
    }
    fmPlatformXcvrEepromDumpSpecCompliance(eeprom);
    addr = 18;
    FM_LOG_PRINT("%20s[%02x]: %dm\n", "Length(Copper)", addr, eeprom[addr]);
    addr = 20;
    PrintBytesName("VendorName", addr, &eeprom[addr], 16);
    addr = 37;
    FM_LOG_PRINT("%20s[%02x]: 0x%02x%02x%02x ", "VendorOUI", addr,
                 eeprom[addr], eeprom[addr+1], eeprom[addr+2]);
    PrintBytesName(NULL, addr, &eeprom[addr], 3);
    addr = 40;
    PrintBytesName("VendorPN", addr, &eeprom[addr], 16);
    addr = 56;
    PrintBytesName("VendorRev", addr, &eeprom[addr], 2);
    for (j = 0, cs = 0 ; j <= 62 ; j++) cs += eeprom[j];
    addr = 63;
    FM_LOG_PRINT("%20s[%02x]: %02x Calculated: %02x\n", "CC_Base", addr, eeprom[addr], cs);

    addr = 68;
    PrintBytesName("VendorSN", addr, &eeprom[addr], 16);
    addr = 84;
    PrintBytesName("DateCode", addr, &eeprom[addr], 8);
    for (j = 64, cs = 0 ; j <= 94 ; j++) cs += eeprom[j];
    addr = 95;
    FM_LOG_PRINT("%20s[%02x]: %02x Calculated: %02x\n", "CC_EXT", addr, eeprom[addr], cs);

}   /* end fmPlatformXcvrEepromDumpBaseExt */




/*****************************************************************************/
/** fmPlatformSfppXcvrEepromDumpPage1
 * \ingroup intPlatformXcvr
 *
 * \desc            Dumps transceiver SFP+ page 1 eeprom.
 *
 * \param[out]      eeprom points to storage where the transceiver page 1 eeprom
 *                  is stored.
 *
 * \return          NONE.
 *
 *****************************************************************************/
void fmPlatformXcvrSfppEepromDumpPage1(fm_byte *eeprom)
{
    fm_int    addr;
    fm_int16  val16;
    fm_uint16 uval16;

    if (!(((eeprom[96] == 0xFF) && (eeprom[97] == 0xFF)) || 
          ((eeprom[96] == 0x00) && (eeprom[97] == 0x00)) ))
    {
        addr = 96;
        val16 =  (eeprom[addr] << 8) | eeprom[addr+1];
        FM_LOG_PRINT("%20s[%02x]: %.1fC\n", "Temperature", addr, val16/256.0);
        addr = 98;
        uval16 =  (eeprom[addr] << 8) | eeprom[addr+1];
        FM_LOG_PRINT("%20s[%02x]: %.2fV\n", "Vcc", addr, uval16/10000.0);
        addr = 100;
        uval16 =  (eeprom[addr] << 8) | eeprom[addr+1];
        FM_LOG_PRINT("%20s[%02x]: %.2fmA\n", "Tx Bias Current", addr, uval16/500.0);
        addr = 102;
        uval16 =  (eeprom[addr] << 8) | eeprom[addr+1];
        FM_LOG_PRINT("%20s[%02x]: %.1fuW\n", "Tx Output Power", addr, uval16/10.0);
        addr = 104;
        uval16 =  (eeprom[addr] << 8) | eeprom[addr+1];
        FM_LOG_PRINT("%20s[%02x]: %.1fuW\n", "Rx Power", addr, uval16/10.0);
    }

}   /* end fmPlatformSfppXcvrEepromDumpPage1 */





/*****************************************************************************/
/** fmPlatforQsfpXcvrEepromDumpPage0
 * \ingroup intPlatformXcvr
 *
 * \desc            Dumps transceiver QSFP page 0 eeprom.
 *
 * \param[out]      eeprom points to storage where the transceiver page 0 eeprom
 *                  is stored.
 *
 * \return          NONE.
 *
 *****************************************************************************/
void fmPlatformXcvrQsfpEepromDumpPage0(fm_byte *eeprom)
{
    fm_int    addr;
    fm_int16  val16;
    fm_uint16 uval16;

    if (!(((eeprom[24] == 0xFF) && (eeprom[25] == 0xFF)) || 
          ((eeprom[24] == 0x00) && (eeprom[25] == 0x00)) ))
    {
        addr = 3;
        val16 =  (eeprom[addr] << 8) | eeprom[addr+1];
        FM_LOG_PRINT("%20s[%02x]: ", "LOS", addr);
        if (eeprom[addr] & (1 << 0)) FM_LOG_PRINT(" RX1");
        if (eeprom[addr] & (1 << 1)) FM_LOG_PRINT(" RX2");
        if (eeprom[addr] & (1 << 2)) FM_LOG_PRINT(" RX3");
        if (eeprom[addr] & (1 << 3)) FM_LOG_PRINT(" RX4");
        if (eeprom[addr] & (1 << 4)) FM_LOG_PRINT(" TX1");
        if (eeprom[addr] & (1 << 5)) FM_LOG_PRINT(" TX2");
        if (eeprom[addr] & (1 << 6)) FM_LOG_PRINT(" TX3");
        if (eeprom[addr] & (1 << 7)) FM_LOG_PRINT(" TX4");
        FM_LOG_PRINT("\n");

        addr = 4;
        val16 =  (eeprom[addr] << 8) | eeprom[addr+1];
        FM_LOG_PRINT("%20s[%02x]: ", "FAULT", addr);
        if (eeprom[addr] & (1 << 0)) FM_LOG_PRINT(" TX1");
        if (eeprom[addr] & (1 << 1)) FM_LOG_PRINT(" TX2");
        if (eeprom[addr] & (1 << 2)) FM_LOG_PRINT(" TX3");
        if (eeprom[addr] & (1 << 3)) FM_LOG_PRINT(" TX4");
        FM_LOG_PRINT("\n");

        addr = 22;
        val16 =  (eeprom[addr] << 8) | eeprom[addr+1];
        FM_LOG_PRINT("%20s[%02x]: %.1fC\n", "Temperature", addr, val16/256.0);
        addr = 26;
        uval16 =  (eeprom[addr] << 8) | eeprom[addr+1];
        FM_LOG_PRINT("%20s[%02x]: %.2fV\n", "Vcc", addr, uval16/10000.0);

        addr = 34;
        FM_LOG_PRINT("%20s[%02x]: %.1f %.1f %.1f %.1f uW\n", "Rx Power", addr,
            ((eeprom[addr+0] << 8) | eeprom[addr+1])/10.0,
            ((eeprom[addr+2] << 8) | eeprom[addr+3])/10.0,
            ((eeprom[addr+4] << 8) | eeprom[addr+4])/10.0,
            ((eeprom[addr+6] << 8) | eeprom[addr+5])/10.0);

        addr = 42;
        FM_LOG_PRINT("%20s[%02x]: %.2f %.2f %.2f %.2f mA\n", "Tx Bias Current", addr,
            ((eeprom[addr+0] << 8) | eeprom[addr+1])/500.0,
            ((eeprom[addr+2] << 8) | eeprom[addr+3])/500.0,
            ((eeprom[addr+4] << 8) | eeprom[addr+4])/500.0,
            ((eeprom[addr+6] << 8) | eeprom[addr+5])/500.0);

    }

}   /* end fmPlatformQsfpXcvrEepromDumpPage0 */

