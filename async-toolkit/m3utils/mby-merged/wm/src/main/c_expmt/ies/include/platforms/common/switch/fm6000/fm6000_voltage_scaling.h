/* vim:ts=4:sw=4:expandtab
 * (No tabs, indent level is 4 spaces)  */
/*****************************************************************************
 * File:            fm6000_voltage_scaling.h
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

#ifndef __FM_FM6000_VOLTAGE_SCALING_H
#define __FM_FM6000_VOLTAGE_SCALING_H


/**************************************************/
/* This structure represents fm6000 nominal voltages: VDD and VDDS.
 * Used as an argument to ''fm6000GetNominalSwitchVoltages''.
 **************************************************/
typedef struct _fm_fm6000NominalVoltages
{
    fm_uint32 VDD;     /* in millivolts */
    fm_uint32 VDDS;    /* in millivolts */

} fm_fm6000NominalVoltages;


fm_status fm6000GetNominalSwitchVoltages(fm_int                    sw,
                                         fm_fm6000NominalVoltages *nominalVoltages,
                                         fm_registerReadUINT32Func readFunction);


#endif /* __FM_FM6000_VOLTAGE_SCALING_H */
