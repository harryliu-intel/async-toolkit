/* vim:ts=4:sw=4:expandtab
 * (No tabs, indent level is 4 spaces)  */
/*****************************************************************************
 * File:            fm_api_fibm_int.h
 * Creation Date:   May 22, 2008
 * Description:     Helper FIBM functions
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

#ifndef __FM_FM_API_FIBM_INT_H
#define __FM_FM_API_FIBM_INT_H

/**************************************************
 * The following macros are used to enable and
 * disable FIBM batch processing. They should be
 * used where multiple register writes appear
 * together, usually in a loop. 
 *
 * Note that a register read flushes the batch, so
 * these macros will not help accelerate alternating 
 * reads and writes.
 **************************************************/
    
#define FM_BEGIN_FIBM_BATCH(sw, err, cat)                                   \
            (err) = fmFibmStartBatching( (sw), TRUE );                      \
            if ( (err) != FM_OK &&                                          \
                 (err) != FM_ERR_UNINITIALIZED &&                           \
                 (err) != FM_ERR_UNSUPPORTED )                              \
            {                                                               \
                FM_LOG_ABORT_ON_ERR( (cat), (err) );                        \
            }                                                               \
            else                                                            \
            {                                                               \
                (err) = FM_OK;                                              \
            }

#define FM_END_FIBM_BATCH(sw, err)                                          \
            (err) = fmFibmStartBatching( (sw), FALSE );                     \
            if ( (err) == FM_ERR_UNINITIALIZED ||                           \
                 (err) == FM_ERR_UNSUPPORTED )                              \
            {                                                               \
                (err) = FM_OK;                                              \
            }


/* Helper FIBM functions */
fm_bool fmFibmSlaveIsPortMgmt(fm_int sw, fm_int physPort);
fm_bool fmFibmSlaveIsLogicalPortMgmt(fm_int sw, fm_int logicalPort);
fm_int fmFibmSlaveGetMasterSwitch(fm_int slaveSw);

fm_status fmFibmStartBatching(fm_int sw, fm_bool start);

#endif /* __FM_FM_API_FIBM_INT_H */
