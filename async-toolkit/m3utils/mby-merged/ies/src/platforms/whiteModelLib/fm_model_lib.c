/* vim:ts=4:sw=4:expandtab
 * (No tabs, indent level is 4 spaces)  */
/*****************************************************************************
 * File:            model_lib.c
 * Creation Date:   2015
 * Description:     Wrappers for WhiteModel
 *
 *
 * Copyright (c) 2015 - 2016, Intel Corporation
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 *     * Redistributions of source code must retain the above copyright notice,
 *       this list of conditions and the following disclaimer.
 *     * Redistributions in binary form must reproduce the above copyright
 *       notice, this list of conditions and the following disclaimer in the
 *       documentation and/or other materials provided with the distribution.
 *     * Neither the name of Intel Corporation nor the names of its contributors
 *       may be used to endorse or promote products derived from this software
 *       without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
 * AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
 * SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
 * CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
 * OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 * OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*****************************************************************************/

#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <stdlib.h>
#include <time.h>

#include <fm_sdk.h>
#include <api/internal/hlp/hlp_api_regs_int.h>
#include <platforms/whiteModelLib/platform_types.h>
#include <platforms/common/model/hlp/hlp_model.h>


#define MAX_STR_LEN                     10000

/* Use this register for storing register version tag */
#define REG_VERSION_OFF                 HLP_BSM_SCRATCH_3(511,0)
/* Update after changes to WM based on same register version */
#define MODEL_MINOR_VERSION             0xd1

static fm_char verStr[64];

void *fmRootChipModel[FM_MAX_NUM_FOCALPOINTS];
static fm_libCfg libCfg;
static fm_char logBuf[MAX_STR_LEN + 1];
static fm_bool lastLineHasCr = FALSE;

static const char *allowed_plus_args[] = {
    "HLP_WM_PRINT_EN",
    "HLP_WM_VPRINT_EN",
    "PA_HLP_WM_PRINT_EN",
    "HLP_CM_WM_PRINT_VERBOSE",
    "HLP_PARSER_WM_PRINT_VERBOSE",
    "HLP_MAPPER_WM_PRINT_VERBOSE",
    "HLP_FFU_CLASSIFIER_WM_PRINT_VERBOSE",
    "HLP_FFU_FINAL_ACTIONS_WM_PRINT_VERBOSE",
    "HLP_NEXT_HOP_WM_PRINT_VERBOSE",
    "HLP_L2_LOOKUP_WM_PRINT_VERBOSE",
    "HLP_GLORT_WM_PRINT_VERBOSE",
    "HLP_GEN_MASK_WM_PRINT_VERBOSE",
    "HLP_TRIGGERS_WM_PRINT_VERBOSE",
    "HLP_MOD_WM_PRINT_VERBOSE",
    "HLP_HASH_WM_PRINT_VERBOSE",
    "HLP_FWD_WM_PRINT_VERBOSE",
    "HLP_LEARNING_WM_PRINT_VERBOSE",
    "HLP_FGRP_WM_PRINT_VERBOSE",
    NULL
};

/* The following two variables must be kept in sync:
 * the order of the arguments is important and must be maintained
 */
static const fm_uint64 log_categories[] = {
    FM_LOG_CAT_EVENT,
    FM_LOG_CAT_PLATFORM,
    FM_LOG_CAT_MODEL_FFU,
    FM_LOG_CAT_MODEL_GEN_MASK,
    FM_LOG_CAT_MODEL_GLORT,
    FM_LOG_CAT_MODEL_L2_LOOKUP,
    FM_LOG_CAT_MODEL_LEARNING,
    FM_LOG_CAT_MODEL_TRIGGERS,
    FM_LOG_CAT_MODEL_MAC,
    FM_LOG_CAT_MODEL_MAPPER,
    FM_LOG_CAT_MODEL_MODIFY,
    FM_LOG_CAT_MODEL_PARSER,
    FM_LOG_CAT_MODEL_NEXT_HOP,
    FM_LOG_CAT_MODEL_STATS,
    FM_LOG_CAT_MODEL_MACSEC,
    0
};

const char *log_cat_names[] = {
    "event",
    "platform",
    "ffu",
    "gen_mask",
    "glort",
    "l2_lookup",
    "learning",
    "triggers",
    "mac",
    "mapper",
    "modify",
    "parser",
    "next_hop",
    "stats",
    "macsec",
    NULL
};


/* Default: output from all categories is enabled */
fm_uint64 log_cat_mask = 0xFFFFFFFFFFFFFFFF;

/* Support functions to build the HLP model as a standalone library */
fm_int fmRand(void)
{
    static fm_uint rand_seed = 1;
    fm_int randResult;

    randResult = rand_r(&rand_seed);

    return randResult;
}

void* fmAllocWrap(fm_uint size, const char *file, const int line)
{
    FM_NOT_USED(file);
    FM_NOT_USED(line);

    return malloc(size);
}

void fmFreeWrap(void *obj, const char *file, const int line)
{
    FM_NOT_USED(file);
    FM_NOT_USED(line);

    free(obj);
}

/* No locking support, assume single threaded */
fm_status fmCreateLockV2(fm_text lockName, 
                         fm_int  sw,
                         fm_int  precedence,
                         fm_lock *lck)
{
    FM_NOT_USED(lockName);
    FM_NOT_USED(sw);
    FM_NOT_USED(precedence);
    FM_NOT_USED(lck);

    return FM_OK;
}

fm_status fmCaptureLock(fm_lock *lck, fm_timestamp *timeout)
{
    FM_NOT_USED(lck);
    FM_NOT_USED(timeout);

    return FM_OK;
}

fm_status fmReleaseLock(fm_lock *lck)
{
    FM_NOT_USED(lck);

    return FM_OK;
}

fm_status fmGetLoggingAttribute(fm_int  attr,
                                fm_int  size,
                                void *  value)
{
    FM_NOT_USED(attr);
    FM_NOT_USED(size);

    *(fm_uint64*)value =  -1ULL;
    if (FM_LOG_ATTR_LEVEL_MASK)
    {
        *(fm_uint64*)value &= ~FM_LOG_LEVEL_DEBUG_VERBOSE;
    }

    return FM_OK;
}

int testPlusArgs(const char *arg)
{
    int i = 0;
    while (allowed_plus_args[i])
    {
        if (!strcmp(arg, allowed_plus_args[i])) {
            /* Always return the maximum debug level here, that is 3.
             * Further filtering is done in fmVLogMessage
             */
            return 3;
        }
        ++i;
    }
    return 0;
}

fm_int fmGetIntApiAttribute(fm_text key, fm_int defaultValue)
{
    FM_NOT_USED(key);

    return defaultValue;
}

fm_status fmVLogMessage(fm_uint64   categories,
                        fm_uint64   logLevel,
                        const char *srcFile,
                        const char *srcFunction,
                        fm_uint32   srcLine,
                        const char *format,
                        va_list     ap)
{
    fm_char tag[32];
    fm_int  len;
    fm_int  remLen;
    fm_bool doLog;
    fm_int  i = 0;

    FM_NOT_USED(srcFile);
    FM_NOT_USED(srcFunction);
    FM_NOT_USED(srcLine);

    if (libCfg.logLevel < 0)
        return FM_OK;


    switch (logLevel)
    {
        case FM_LOG_LEVEL_FUNC_ENTRY:
            doLog = (libCfg.logLevel >= 6);
            FM_SNPRINTF_S(tag, 32, "%s", "ENTRY");
            break;
        case FM_LOG_LEVEL_FUNC_EXIT:
            doLog = (libCfg.logLevel >= 6);
            FM_SNPRINTF_S(tag, 32, "%s", "EXIT");
            break;
        case FM_LOG_LEVEL_FUNC_ENTRY_VERBOSE:
            doLog = (libCfg.logLevel >= 6);
            FM_SNPRINTF_S(tag, 32, "%s", "ENTRY");
            break;
        case FM_LOG_LEVEL_FUNC_EXIT_VERBOSE:
            doLog = (libCfg.logLevel >= 6);
            FM_SNPRINTF_S(tag, 32, "%s", "EXIT");
            break;
        case FM_LOG_LEVEL_DEBUG_VERBOSE:
            doLog = (libCfg.logLevel >= 5);
            FM_SNPRINTF_S(tag, 32, "%s", "DEBUG_VERBOSE");
            break;
        case FM_LOG_LEVEL_DEBUG:
            doLog = (libCfg.logLevel >= 2);
            FM_SNPRINTF_S(tag, 32, "%s", "DEBUG");
            break;
        case FM_LOG_LEVEL_DEBUG2:
            doLog = (libCfg.logLevel >= 3);
            FM_SNPRINTF_S(tag, 32, "%s", "DEBUG2");
            break;
        case FM_LOG_LEVEL_DEBUG3:
            doLog = (libCfg.logLevel >= 4);
            FM_SNPRINTF_S(tag, 32, "%s", "DEBUG3");
            break;
        case FM_LOG_LEVEL_PRINT:
            doLog = (libCfg.logLevel >= 1);
            FM_SNPRINTF_S(tag, 32, "%s", "PRINT");
            break;
        case FM_LOG_LEVEL_INFO:
            doLog = (libCfg.logLevel >= 1);
            FM_SNPRINTF_S(tag, 32, "%s", "INFO");
            break;
        case FM_LOG_LEVEL_WARNING:
            doLog = TRUE;
            FM_SNPRINTF_S(tag, 32, "%s", "WARN");
            break;
        case FM_LOG_LEVEL_ERROR:
            doLog = TRUE;
            FM_SNPRINTF_S(tag, 32, "%s", "ERROR");
            break;
        case FM_LOG_LEVEL_FATAL:
            doLog = TRUE;
            FM_SNPRINTF_S(tag, 32, "%s", "FATAL");
            break;
        case FM_LOG_LEVEL_ASSERT:
            doLog = TRUE;
            FM_SNPRINTF_S(tag, 32, "%s", "ASSERT");
            break;
        default:
            doLog = TRUE;
            FM_SNPRINTF_S(tag, 32, "UNKNOWN(%llx)", logLevel);
            break;

    }

    while (doLog && log_cat_names[i]) {
        if ((categories == log_categories[i]) && !(log_cat_mask & (1 << i))) {
             /* Log for this category is disabled */
            doLog = 0;
            break;
        }
        i++;
    }

    if (doLog)
    {
        len = 0;
        if (lastLineHasCr) {
            switch (logLevel)
            {
                case FM_LOG_LEVEL_PRINT:
                case FM_LOG_LEVEL_INFO:
                    break;
                default:
                    FM_SNPRINTF_S(logBuf, MAX_STR_LEN, "%s-<%s>: ",
                                  srcFunction, tag);
                    break;
            }
        }

        /* Append the message to the existing logBuf */
        len = strlen(logBuf);
        len += vsnprintf(logBuf + len, MAX_STR_LEN - len, format, ap);

        if (len > 0)
        {
            if (len > MAX_STR_LEN) len = MAX_STR_LEN;
            lastLineHasCr = isspace(logBuf[len-1]) && !isblank(logBuf[len-1]);
            if (lastLineHasCr)
            {
                /* Remove the \n and clean logBuf */
                logBuf[len-1] = '\0';
                if (libCfg.logHandler)
                    libCfg.logHandler(logLevel, logBuf);
                logBuf[0] = '\0';
            }
            else
            {
                logBuf[len] = '\0';
            }
        }
    }

    return FM_OK;

}   /* end fmLogMessage */

fm_status fmLogMessage(fm_uint64   categories,
                       fm_uint64   logLevel,
                       const char *srcFile,
                       const char *srcFunction,
                       fm_uint32   srcLine,
                       const char *format,
                       ...)
{
    fm_status status;
    va_list va;
    va_start(va, format);
    status =
        fmVLogMessage(categories, logLevel, srcFile, srcFunction, srcLine,
                      format, va);
    va_end(va);
    return status;

}

const char *fmErrorMsg(fm_int err)
{
    static fm_char errMsg[64];

    FM_SNPRINTF_S(errMsg, sizeof(errMsg) - 1, "ERROR(%d)", err);

    return errMsg;

}   /* end fmErrorMsg */


void fmGetCallerName(fm_text buf,
                     fm_int  bufSize,
                     fm_int  callerCount,
                     fm_text delimiter)
{
    FM_NOT_USED(callerCount);
    FM_NOT_USED(delimiter);

    if (bufSize >= 1)
    {
        buf[0] = '\0';
    }
}   /* end fmGetCallerName */



/*****************************************************************************/
/* fmModelLibInit
 * \ingroup intModelLib
 *
 * \desc            Model library initialization.
 *
 * \param[in]       sw is the switch number
 *
 * \param[in]       cfg is the fm_libCfg to initialize the library with.
 *
 * \return          0 for success
 *
 *****************************************************************************/
fm_status fmModelLibInit(fm_int sw, fm_libCfg *cfg)
{

    if (cfg)
    {
        FM_MEMCPY_S(&libCfg, sizeof(libCfg), cfg, sizeof(*cfg));
    }

    return hlpModelInitialize(&fmRootChipModel[0], sw, NULL);
}

fm_text fmModelGetVersionTag(void)
{
    FM_SNPRINTF_S(verStr, sizeof(verStr) -1, "TURNIN_%08x",
                  HLP_REG_TAG << 8 | MODEL_MINOR_VERSION);
    return verStr;
}

fm_status fmModelReset(fm_int sw)
{
    fm_status status;
    time_t    start;
    time_t    end;

    start = time(NULL);

    printf("Resetting chip...\n");

    status = hlpModelReset(sw);

    end = time(NULL);
    printf("Reseting chip in %lld seconds\n", (long long)(end - start));

    printf("Model Version: %s\n", fmModelGetVersionTag());

    /*Temporary to indicate register set version */
    fmModelWriteCSR64(0, REG_VERSION_OFF, HLP_REG_TAG << 8 | MODEL_MINOR_VERSION);

    return status;
}

/*****************************************************************************/
/* fmModelLibSetLogLevel
 * \ingroup intModelLib
 *
 * \desc            Controls logging outputs..
 *
 * \param[in]       level is the log level to set.
 *                  -1: disable all logging
 *                   0: show WARNING and higher
 *                   1: show PRINT, INFO, and higher
 *                   2: show DEBUG, and higher
 *                   3: show DEBUG2, and higher
 *                   4: show DEBUG3, and higher
 *                   5: show VERBOSE, and higher
 *                   6: show all logging
 *
 *****************************************************************************/
void fmModelLibSetLogLevel(fm_int level)
{
    libCfg.logLevel = level;
}

/*****************************************************************************/
/* fmModelLibSetLogCat
 * \ingroup intModelLib
 *
 * \desc            Controls output log for WM cateories
 *
 * \param[in]       cat is a comma separated list of categories
 *                  all : enable all
 *                  none: disable all
 *
 *****************************************************************************/
void fmModelLibSetLogCat(char *cat)
{
    if (!strcasecmp(cat, "none")) {
        log_cat_mask = 0;
        return;
    }

    if (!strcasecmp(cat, "all")) {
        log_cat_mask = 0xFFFFFFFFFFFFFFFF;
        return;
    }

    log_cat_mask = 0;
    char *tok = strtok(cat, ",");
    while (tok != NULL) {
        int j = 0;
        while (log_cat_names[j] && j < 64) {
            if (!strcasecmp(log_cat_names[j], tok)) {
                log_cat_mask |= 1 << j;
                break;
            }
            j++;
        }
        tok = strtok(NULL, ",");
    }
}

