/* vim:ts=4:sw=4:expandtab
 * (No tabs, indent level is 4 spaces)  */
/*****************************************************************************
 * File:            platform_instrument.h
 * Creation Date:   July 3, 2012
 * Description:     Platform Instrumentation header functions
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
#ifndef __FM_PLATFORM_INSTRUMENT_H
#define __FM_PLATFORM_INSTRUMENT_H

/*****************************************************
 * INSTRUMENT_LOG_LEVEL is used to enable some
 * Intel-internal test instrumentation:
 *
 *  0   Normal setting (disable instrumentation)
 *  1   Enable binary instrumentation (normal instrumentation)
 *  2   Enable text instrumentation (for testing instrumentation)
 *
 *****************************************************/

#if INSTRUMENT_LOG_LEVEL
#define INSTRUMENT_REG_WRITE(sw, addr, val)  \
        fmPlatformInstrumentWriteCSR( (sw), (addr), (val) )
#else
#define INSTRUMENT_REG_WRITE(sw, addr, val)
#endif


/**************************************************
 * Functions.
 **************************************************/

void fmPlatformCloseInstrumentation(void);
void fmPlatformOpenInstrumentation(void);
void fmPlatformInstrumentWriteCSR(fm_int sw, fm_uint32 addr, fm_uint32 newValue);

#endif /* __FM_PLATFORM_INSTRUMENT_H */
