/* vim:ts=4:sw=4:expandtab
 * (No tabs, indent level is 4 spaces)  */
/*****************************************************************************
 * File:            fm_alos_logging_int.h
 * Creation Date:   June 18, 2007
 * Description:     The SDK logging subsystem
 *
 * INTEL CONFIDENTIAL
 * Copyright 2005 - 2011 Intel Corporation. All Rights Reserved. 
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

#ifndef __FM_FM_ALOS_LOGGING_INT_H
#define __FM_FM_ALOS_LOGGING_INT_H

/* global state structure for logging facility */
typedef struct
{
    /***************************************************
     * When checking to see if we have initialized yet,
     * we check this value against FM_LOG_MAGIC_NUMBER.
     * This ensures that even if the memory for this
     * structure is zeroed, we will not assign the state
     * to "initialized" until code has explicitly set
     * this to FM_LOG_MAGIC_NUMBER.
     **************************************************/

    fm_uint32      initMagicNumber;

    /***************************************************
     * General logging state
     **************************************************/

    /* The destination type */
    fm_loggingType logType;

    /* The flags for verbosity */
    fm_uint32      verbosityMask;

    /* Global enable/disable */
    fm_bool        enabled;

    /* Filter conditions */
    fm_uint64      categoryMask;
    fm_uint64      levelMask;
    fm_char        functionFilter[FM_LOG_MAX_FILTER_LEN];
    fm_char        fileFilter[FM_LOG_MAX_FILTER_LEN];

    /* protects access to the logging destination */
    void *         accessLock;

    /***************************************************
     * Logging state for the file mode
     **************************************************/

    fm_char        logFileName[FM_MAX_FILENAME_LENGTH];

    /***************************************************
     * Logging state for the memory buffer mode
     **************************************************/

    fm_char        logBuffer[FM_LOG_MAX_LINES][FM_LOG_MAX_LINE_SIZE];
    fm_uint32      currentPos;
    fm_uint32      maxLines;

    /***************************************************
     * Logging state for the call-back mode
     **************************************************/

    fm_logCallBack fmLogCallback;
    fm_voidptr     fmLogCookie1;
    fm_voidptr     fmLogCookie2;

} fm_loggingState;

#endif /* __FM_FM_ALOS_LOGGING_INT_H */
