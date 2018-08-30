/* vim:et:sw=4:ts=4:tw=80:
 * (No tabs, indent level is 4 spaces)  */
/*****************************************************************************
 * File:            hlp_model_debug.h
 * Creation Date:   June 5, 2012
 * Description:     HLP white model debugging function prototypes.
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

#ifndef __HLP_MODEL_DEBUG_H
#define __HLP_MODEL_DEBUG_H

#define HLP_MODEL_LOG_ENTRY_CSR(cat, ...)                                  \
    FM_LOG_ENTRY_VERBOSE(cat, __VA_ARGS__)

#define HLP_MODEL_LOG_EXIT_CSR(cat, ...)                                   \
    FM_LOG_EXIT_VERBOSE(cat, __VA_ARGS__)

#define HLP_MODEL_DBG_DUMP(model, stageFunc)                               \
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
                hlpModelDbgDump(__FILE__, __func__, __LINE__, (model));    \
            }                                                                  \
            else if (__lmask & FM_LOG_LEVEL_DEBUG)                             \
            {                                                                  \
                (stageFunc)(__FILE__, __func__, __LINE__, (model));            \
            }                                                                  \
        }                                                                      \
    }                                                                          \
}

void hlpModelDbgDump(const char *  srcFile,
                         const char *  srcFunction,
                         fm_uint32     srcLine,
                         hlp_model *model);
void hlpModelDbgDumpMacRx(const char *  srcFile,
                              const char *  srcFunction,
                              fm_uint32     srcLine,
                              hlp_model *model);
void hlpModelDbgDumpXbarRx(const char *  srcFile,
                              const char *  srcFunction,
                              fm_uint32     srcLine,
                              hlp_model *model);
void hlpModelDbgDumpParser(const char *  srcFile,
                               const char *  srcFunction,
                               fm_uint32     srcLine,
                               hlp_model *model);
void hlpModelDbgDumpParserState(hlp_model *model,
                                fm_byte *hit_idx,
                                fm_bool *hit_v);
void hlpModelDbgDumpAssoc(const char *  srcFile,
                              const char *  srcFunction,
                              fm_uint32     srcLine,
                              hlp_model *model);
void hlpModelDbgDumpMapper(const char *  srcFile,
                               const char *  srcFunction,
                               fm_uint32     srcLine,
                               hlp_model *model);
void hlpModelDbgDumpFfuClassifier(const char *  srcFile,
                            const char *  srcFunction,
                            fm_uint32     srcLine,
                            hlp_model *model);
void hlpModelDbgDumpFfuFinalActions(const char *  srcFile,
                            const char *  srcFunction,
                            fm_uint32     srcLine,
                            hlp_model *model);
void hlpModelDbgDumpNextHop(const char *  srcFile,
                                const char *  srcFunction,
                                fm_uint32     srcLine,
                                hlp_model *model);
void hlpModelDbgDumpHash(const char *  srcFile,
                             const char *  srcFunction,
                             fm_uint32     srcLine,
                             hlp_model *model);
void hlpModelDbgDumpL2Lookup(const char *  srcFile,
                                 const char *  srcFunction,
                                 fm_uint32     srcLine,
                                 hlp_model *model);
void hlpModelDbgDumpGlort(const char *  srcFile,
                              const char *  srcFunction,
                              fm_uint32     srcLine,
                              hlp_model *model);
void hlpModelDbgDumpGenMask1(const char *  srcFile,
                                 const char *  srcFunction,
                                 fm_uint32     srcLine,
                                 hlp_model *model);
void hlpModelDbgDumpCm(const char *  srcFile,
                           const char *  srcFunction,
                           fm_uint32     srcLine,
                           hlp_model *model);
void hlpModelDbgDumpTriggers(const char *  srcFile,
                                 const char *  srcFunction,
                                 fm_uint32     srcLine,
                                 hlp_model *model);
void hlpModelDbgDumpGenMask2(const char *  srcFile,
                                 const char *  srcFunction,
                                 fm_uint32     srcLine,
                                 hlp_model *model);
void hlpModelDbgDumpLearning(const char *  srcFile,
                                 const char *  srcFunction,
                                 fm_uint32     srcLine,
                                 hlp_model *model);
void hlpModelDbgDumpFsched(const char *  srcFile,
                               const char *  srcFunction,
                               fm_uint32     srcLine,
                               hlp_model *model);
void hlpModelDbgDumpModify(const char *  srcFile,
                               const char *  srcFunction,
                               fm_uint32     srcLine,
                               hlp_model *model);
void hlpModelDbgDumpXbarTx(const char *  srcFile,
                              const char *  srcFunction,
                              fm_uint32     srcLine,
                              hlp_model *model);
void hlpModelDbgDumpMacTx(const char *  srcFile,
                              const char *  srcFunction,
                              fm_uint32     srcLine,
                              hlp_model *model);

#endif	/* __HLP_MODEL_DEBUG_H */

