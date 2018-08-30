/* vim:ts=4:sw=4:expandtab
 * (No tabs, indent level is 4 spaces)  */
/*****************************************************************************
 * File:            fm6000_voltage_scaling.c
 * Creation Date:   April 4, 2013
 * Description:     Support for FM6000 power supply voltage scaling.
 *
 * INTEL CONFIDENTIAL
 * Copyright 2013 Intel Corporation. All Rights Reserved. 
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

/* Fusebox word type VRM INFO */
#define FBWT_VRM_INFO         0x9

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


/*****************************************************************************/
/** fm6000DecodeVID
 * \ingroup intSwitch
 *
 * \desc            Decodes voltage VR12 VID format to millivolts.
 *
 * \param[in]       Voltage in VR12 VID format
 *
 * \return          Voltage in millivolts
 *
 *****************************************************************************/
static fm_uint fm6000DecodeVID(fm_uint vid)
{
    if (vid == 0)
    {
        return 0;
    }
    return 5 * (vid - 1) + 250;
}


/*****************************************************************************
 * Public Functions
 *****************************************************************************/

/*****************************************************************************/
/** fm6000GetNominalSwitchVoltages
 * \ingroup intPlatform
 *
 * \desc            Retrieve nominal switch voltages.
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \param[out]      nominalVoltages is a pointer to a caller-allocated
 *                   ''fm_fm6000NominalVoltages'' structure to be filled in by
 *                   this function.
 *
 * \param[in]       readFunction is a pointer to function reading a 32-bit wide
 *                   register.
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_INVALID_ARGUMENTS if one of the argument pointers is
 *                   NULL.
 *
 *****************************************************************************/
fm_status fm6000GetNominalSwitchVoltages(fm_int                    sw,
                                         fm_fm6000NominalVoltages *nominalVoltages,
                                         fm_registerReadUINT32Func readFunction)
{
    fm_status      status;
    fm_uint32      regValue;
    fm_int i;

    FM_LOG_ENTRY(FM_LOG_CAT_PLATFORM, "sw=%d\n", sw);

    if ( (nominalVoltages == NULL) || (readFunction == NULL) )
    {
        FM_LOG_EXIT(FM_LOG_CAT_PLATFORM, FM_ERR_INVALID_ARGUMENT);
    }

    FM_CLEAR(*nominalVoltages);

    status = readFunction(sw, FM6000_BM_VRM, &regValue);
    FM_LOG_EXIT_ON_ERR(FM_LOG_CAT_PLATFORM, status);
      
    nominalVoltages->VDD =
        fm6000DecodeVID(FM_GET_FIELD(regValue, FM6000_BM_VRM, Value));

    if (nominalVoltages->VDD == 0)
    {
        FM_LOG_DEBUG(FM_LOG_CAT_PLATFORM,
                     "Nominal switch voltages are not available\n");
        FM_LOG_EXIT(FM_LOG_CAT_PLATFORM, FM_OK);
    }
 
    for (i = (FM6000_FUSEBOX_ENTRIES/sizeof(fm_uint32)) - 1; i >= 0; i--)
    {
        status = readFunction(sw,
                              FM6000_FUSEBOX(i * sizeof(fm_uint32) + 3),
                              &regValue);
        FM_LOG_EXIT_ON_ERR(FM_LOG_CAT_PLATFORM, status);
            
        if (((regValue >> 4) & 0xF) == FM6000_FBWT_VRM_INFO)
        {
            status = readFunction(sw,
                                  FM6000_FUSEBOX(i * sizeof(fm_uint32) + 1),
                                  &regValue);
            FM_LOG_EXIT_ON_ERR(FM_LOG_CAT_PLATFORM, status);
            nominalVoltages->VDDS = fm6000DecodeVID(regValue);
            break;
        }
    }
    
    if (nominalVoltages->VDDS == 0)
    {
         FM_LOG_DEBUG(FM_LOG_CAT_PLATFORM,
                     "Nominal switch voltage VDDS is not available\n");
    }

    FM_LOG_EXIT(FM_LOG_CAT_PLATFORM, status);
}   /* end fm6000GetNominalSwitchVoltages */
