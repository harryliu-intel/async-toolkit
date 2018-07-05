/* vim:et:sw=4:ts=4:tw=80:
 * (No tabs, indent level is 4 spaces)  */
/*****************************************************************************
 * File:            fm10000_model_debug.h
 * Creation Date:   June 5, 2012
 * Description:     FM10000 white model debugging function prototypes.
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

#ifndef __FM10000_MODEL_DEBUG_H
#define __FM10000_MODEL_DEBUG_H

#define FM10000_MODEL_LOG_ENTRY_CSR(cat, ...)                                  \
    FM_LOG_ENTRY_VERBOSE(cat, __VA_ARGS__)

#define FM10000_MODEL_LOG_EXIT_CSR(cat, ...)                                   \
    FM_LOG_EXIT_VERBOSE(cat, __VA_ARGS__)

#define FM10000_MODEL_DBG_DUMP(model, stageFunc)                               \
{                                                                              \
    fm_status __cerr, __lerr;                                                  \
    fm_uint64 __cmask, __lmask;                                                \
                                                                               \
    __cerr = fmGetLoggingAttribute(FM_LOG_ATTR_CATEGORY_MASK, 0, &__cmask);    \
    if ( ( __cerr == FM_OK ) && ( __cmask & FM_LOG_CAT_PLATFORM ) )            \
    {                                                                          \
        __lerr = fmGetLoggingAttribute(FM_LOG_ATTR_LEVEL_MASK, 0, &__lmask);   \
        if ( ( __lerr == FM_OK ) )                                             \
        {                                                                      \
            if (__lmask & FM_LOG_LEVEL_DEBUG_VERBOSE)                          \
            {                                                                  \
                fm10000ModelDbgDump(__FILE__, __func__, __LINE__, (model));    \
            }                                                                  \
            else if (__lmask & FM_LOG_LEVEL_DEBUG)                             \
            {                                                                  \
                (stageFunc)(__FILE__, __func__, __LINE__, (model));            \
            }                                                                  \
        }                                                                      \
    }                                                                          \
}

void fm10000ModelDbgDump(const char *  srcFile,
                         const char *  srcFunction,
                         fm_uint32     srcLine,
                         fm10000_model *model);
void fm10000ModelDbgDumpEplRx(const char *  srcFile,
                              const char *  srcFunction,
                              fm_uint32     srcLine,
                              fm10000_model *model);
void fm10000ModelDbgDumpParser(const char *  srcFile,
                               const char *  srcFunction,
                               fm_uint32     srcLine,
                               fm10000_model *model);
void fm10000ModelDbgDumpAssoc(const char *  srcFile,
                              const char *  srcFunction,
                              fm_uint32     srcLine,
                              fm10000_model *model);
void fm10000ModelDbgDumpMapper(const char *  srcFile,
                               const char *  srcFunction,
                               fm_uint32     srcLine,
                               fm10000_model *model);
void fm10000ModelDbgDumpFfu(const char *  srcFile,
                            const char *  srcFunction,
                            fm_uint32     srcLine,
                            fm10000_model *model);
void fm10000ModelDbgDumpNextHop(const char *  srcFile,
                                const char *  srcFunction,
                                fm_uint32     srcLine,
                                fm10000_model *model);
void fm10000ModelDbgDumpHash(const char *  srcFile,
                             const char *  srcFunction,
                             fm_uint32     srcLine,
                             fm10000_model *model);
void fm10000ModelDbgDumpL2Lookup(const char *  srcFile,
                                 const char *  srcFunction,
                                 fm_uint32     srcLine,
                                 fm10000_model *model);
void fm10000ModelDbgDumpGlort(const char *  srcFile,
                              const char *  srcFunction,
                              fm_uint32     srcLine,
                              fm10000_model *model);
void fm10000ModelDbgDumpGenMask1(const char *  srcFile,
                                 const char *  srcFunction,
                                 fm_uint32     srcLine,
                                 fm10000_model *model);
void fm10000ModelDbgDumpCm(const char *  srcFile,
                           const char *  srcFunction,
                           fm_uint32     srcLine,
                           fm10000_model *model);
void fm10000ModelDbgDumpTriggers(const char *  srcFile,
                                 const char *  srcFunction,
                                 fm_uint32     srcLine,
                                 fm10000_model *model);
void fm10000ModelDbgDumpGenMask2(const char *  srcFile,
                                 const char *  srcFunction,
                                 fm_uint32     srcLine,
                                 fm10000_model *model);
void fm10000ModelDbgDumpLearning(const char *  srcFile,
                                 const char *  srcFunction,
                                 fm_uint32     srcLine,
                                 fm10000_model *model);
void fm10000ModelDbgDumpFsched(const char *  srcFile,
                               const char *  srcFunction,
                               fm_uint32     srcLine,
                               fm10000_model *model);
void fm10000ModelDbgDumpModify(const char *  srcFile,
                               const char *  srcFunction,
                               fm_uint32     srcLine,
                               fm10000_model *model);
void fm10000ModelDbgDumpEplTx(const char *  srcFile,
                              const char *  srcFunction,
                              fm_uint32     srcLine,
                              fm10000_model *model);

#endif	/* __FM10000_MODEL_DEBUG_H */

