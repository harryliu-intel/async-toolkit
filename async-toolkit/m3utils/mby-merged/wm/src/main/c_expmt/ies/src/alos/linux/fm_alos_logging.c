/* vim:ts=4:sw=4:expandtab
 * (No tabs, indent level is 4 spaces)  */
/*****************************************************************************
 * File:            fm_alos_logging.c
 * Creation Date:   June 18, 2007
 * Description:     SDK logging facility implementation.
 *
 * INTEL CONFIDENTIAL
 * Copyright 2005 - 2012 Intel Corporation. All Rights Reserved.
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


/*****************************************************************************
 * Macros, Constants & Types
 *****************************************************************************/

#define LOG_INITIALIZED(ls) \
    ( (ls) && (ls)->initMagicNumber == FM_LOG_MAGIC_NUMBER )

/*****************************************************************************
 * Global Variables
 *****************************************************************************/

/*****************************************************************************
 * Local Variables
 *****************************************************************************/

/*****************************************************************************
 * Local function prototypes.
 *****************************************************************************/

static fm_bool ApplyLoggingFilter(const char *haystack, const char *needle);

/*****************************************************************************
 * Local Functions
 *****************************************************************************/

/*****************************************************************************/
/** ApplyLoggingFilter
 * \ingroup intLogging
 *
 * \desc            Determine whether a (partial) match can be found for the
 *                  string needle in the set of (partial) filter expressions
 *                  haystack.
 *
 * \param[in]       haystack points to the set of (partial) filter expressions
 *
 * \param[in]       needle points to the string for a match is to be found
 *
 * \return          TRUE if a match is found
 * \return          FALSE otherwise
 *
 *****************************************************************************/
static fm_bool ApplyLoggingFilter(const char *haystack, const char *needle)
{
    fm_bool found = FALSE;
    char    buffer[FM_LOG_MAX_FILTER_LEN];
    char *  end;
    char *  token;

    /***************************************************
     * The strtok_r function CANNOT be used on constant
     * strings. To prevent depending on logic that is
     * outside the scope of this function, a local,
     * non-constant copy of haystack is made here.
     **************************************************/
    FM_STRNCPY_S(buffer, sizeof(buffer), haystack, sizeof(buffer) );

    buffer[sizeof(buffer) - 1] = '\0';

    token = strtok_r(buffer, " ,", &end);

    while (token)
    {
        if (strstr(needle, token))
        {
            found = TRUE;

            break;
        }

        token = strtok_r(NULL, " ,", &end);
    }

    return found;

}   /* end ApplyLoggingFilter */




/*****************************************************************************
 * Public Functions
 *****************************************************************************/

/*****************************************************************************/
/** fmAlosLoggingInit
 * \ingroup intLogging
 *
 * \desc            Initializes the logging subsystem.
 *
 * \note            This is an internal function that should only be called
 *                  from fm_alos_init.c.
 *
 * \return          FM_OK on success.
 *
 *****************************************************************************/
fm_status fmAlosLoggingInit(void)
{
    fm_loggingState *   ls = (fmRootAlos ? &(fmRootAlos->fmLoggingState) : NULL);
    pthread_mutexattr_t attr;

    if (ls == NULL)
    {
        return FM_ERR_UNINITIALIZED;
    }

    /* clear state */
    FM_CLEAR(*ls);

    ls->logType       = FM_LOG_TYPE_CONSOLE;
    ls->enabled       = TRUE;
    ls->verbosityMask = FM_LOG_VERBOSITY_DEFAULT;

    /* initially dump anything fatal from anyone */
    ls->categoryMask = FM_LOG_CAT_DEFAULT;
    ls->levelMask    = FM_LOG_LEVEL_DEFAULT;

    ls->functionFilter[0] = 0;
    ls->fileFilter[0] = 0;

    if ( pthread_mutexattr_init(&attr) )
    {
        FM_LOG_EXIT(FM_LOG_CAT_LOGGING, FM_ERR_LOCK_INIT);
    }

    if ( pthread_mutexattr_settype(&attr, PTHREAD_MUTEX_RECURSIVE) ||
        pthread_mutexattr_setpshared(&attr, PTHREAD_PROCESS_SHARED) )
    {
        pthread_mutexattr_destroy(&attr);
        FM_LOG_EXIT(FM_LOG_CAT_LOGGING, FM_ERR_LOCK_INIT);
    }

    ls->accessLock = fmAlloc( sizeof(pthread_mutex_t) );
    if (ls->accessLock == NULL)
    {
        pthread_mutexattr_destroy(&attr);
        FM_LOG_EXIT(FM_LOG_CAT_LOGGING, FM_ERR_NO_MEM);
    }
    if (pthread_mutex_init( (pthread_mutex_t *) ls->accessLock, &attr ) != 0)
    {
        pthread_mutexattr_destroy(&attr);
        FM_LOG_EXIT(FM_LOG_CAT_LOGGING, FM_ERR_LOCK_INIT);
    }

    pthread_mutexattr_destroy(&attr);

    /* indicate that we have finished initializing */
    ls->initMagicNumber = FM_LOG_MAGIC_NUMBER;

    return FM_OK;

}   /* end fmAlosLoggingInit */




/*****************************************************************************/
/** fmLoggingEnable
 * \ingroup alosLog
 *
 * \desc            Enable the logging subsystem (enabled by default).
 *
 * \param           None.
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_UNINITIALIZED if the logging subsystem has not been
 *                  initialized.
 *
 *****************************************************************************/
fm_status fmLoggingEnable()
{
    fm_loggingState *ls = (fmRootAlos ? &(fmRootAlos->fmLoggingState) : NULL);

    FM_LOG_ENTRY_API(FM_LOG_CAT_LOGGING,
                     "\n");

    if ( !LOG_INITIALIZED(ls) )
    {
        FM_LOG_EXIT_API(FM_LOG_CAT_LOGGING, FM_ERR_UNINITIALIZED);
    }

    ls->enabled = TRUE;

    FM_LOG_EXIT_API(FM_LOG_CAT_LOGGING, FM_OK);

}   /* end fmLoggingEnable */




/*****************************************************************************/
/** fmLoggingDisable
 * \ingroup alosLog
 *
 * \desc            Disable the logging subsystem.
 *
 * \param           None.
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_UNINITIALIZED if the logging subsystem has not been
 *                  initialized.
 *
 *****************************************************************************/
fm_status fmLoggingDisable()
{
    fm_loggingState *ls = (fmRootAlos ? &(fmRootAlos->fmLoggingState) : NULL);

    FM_LOG_ENTRY_API(FM_LOG_CAT_LOGGING, "\n");

    if ( !LOG_INITIALIZED(ls) )
    {
        FM_LOG_EXIT_API(FM_LOG_CAT_LOGGING, FM_ERR_UNINITIALIZED);
    }

    ls->enabled = FALSE;

    FM_LOG_EXIT_API(FM_LOG_CAT_LOGGING, FM_OK);

}   /* end fmLoggingDisable */




/*****************************************************************************/
/** fmSetLoggingVerbosity
 * \ingroup alosLog
 *
 * \desc            Each log message is output with a preamble comprised of
 *                  a time stamp and the location in the source code from
 *                  which the log message was generated. The preamble can make
 *                  the total log message quite long. This function provides
 *                  control over the several components of the preamble.
 *
 * \param[in]       verbosityMask is a bit mask comprised of a set of flag
 *                  bits, one for each component of the preamble, that may be
 *                  ORed together in any combination. See ''Log Verbosity Flags''
 *                  for a list of flag bits.
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_UNINITIALIZED if the logging subsystem has not been
 *                  initialized.
 *
 *****************************************************************************/
fm_status fmSetLoggingVerbosity(fm_uint32 verbosityMask)
{
    fm_loggingState *ls = (fmRootAlos ? &(fmRootAlos->fmLoggingState) : NULL);

    FM_LOG_ENTRY_API(FM_LOG_CAT_LOGGING,
                     "verbosityMask=%08x\n",
                     verbosityMask);

    if ( !LOG_INITIALIZED(ls) )
    {
        FM_LOG_EXIT_API(FM_LOG_CAT_LOGGING, FM_ERR_UNINITIALIZED);
    }

    ls->verbosityMask = verbosityMask;

    FM_LOG_EXIT_API(FM_LOG_CAT_LOGGING, FM_OK);

}   /* end fmSetLoggingVerbosity */




/*****************************************************************************/
/** fmGetLoggingVerbosity
 * \ingroup alosLog
 *
 * \desc            Retrieve the current log message preamble verbosity mask.
 *
 * \param[in]       verbosityMask points to caller-allocated storage where 
 *                  this function should place the current verbosity bit 
 *                  mask. See ''Log Verbosity Flags'' for the meaning of
 *                  each bit.
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_UNINITIALIZED if the logging subsystem has not been
 *                  initialized.
 *
 *****************************************************************************/
fm_status fmGetLoggingVerbosity(fm_uint32 *verbosityMask)
{
    fm_loggingState *ls = (fmRootAlos ? &(fmRootAlos->fmLoggingState) : NULL);

    FM_LOG_ENTRY_API(FM_LOG_CAT_LOGGING,
                     "verbosityMask=%p\n",
                     (void *) verbosityMask);

    if ( !LOG_INITIALIZED(ls) )
    {
        FM_LOG_EXIT_API(FM_LOG_CAT_LOGGING, FM_ERR_UNINITIALIZED);
    }

    *verbosityMask = ls->verbosityMask;

    FM_LOG_EXIT_API(FM_LOG_CAT_LOGGING, FM_OK);

}   /* end fmGetLoggingVerbosity */




/*****************************************************************************/
/** fmSetLoggingType
 * \ingroup alosLog
 *
 * \desc            Set the logging destination to the given type.  The clear
 *                  argument allows logging to continue appending to a previously
 *                  created type.
 *
 * \param[in]       logType is the destination type (see ''fm_loggingType'').
 *
 * \param[in]       clear should be TRUE to reset the destination, otherwise
 *                  logging will be appended to the last position.
 *
 * \param[in]       arg is a parameter for the type (dependent on the type).
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_UNINITIALIZED if the logging subsystem has not been
 *                  initialized.
 *
 *****************************************************************************/
fm_status fmSetLoggingType(fm_loggingType logType,
                           fm_bool        clear,
                           void *         arg)
{
    FILE *              log;
    fm_status           err;
    fm_logCallBackSpec *cbSpec;
    fm_loggingState *   ls;
    int                 posixErr;

    FM_LOG_ENTRY_API(FM_LOG_CAT_LOGGING,
                     "logType=%d clear=%d arg=%p\n",
                     logType,
                     clear,
                     arg);

    err = FM_OK;
    ls  = (fmRootAlos != NULL) ? &fmRootAlos->fmLoggingState : NULL;

    if ( LOG_INITIALIZED(ls) )
    {
        posixErr = pthread_mutex_lock( (pthread_mutex_t *) ls->accessLock );
        if (posixErr != 0)
        {
            FM_LOG_EXIT_API(FM_LOG_CAT_LOGGING, FM_ERR_UNABLE_TO_LOCK);
        }

        ls->logType = logType;

        switch (logType)
        {
            case FM_LOG_TYPE_CONSOLE:
                break;

            case FM_LOG_TYPE_FILE:

                if (clear)
                {
                    fmStringCopy(ls->logFileName, (fm_text) arg,
                                 FM_MAX_FILENAME_LENGTH);

                    /* Create the file to empty it */
                    log = fopen(ls->logFileName, "wt");

                    if (log)
                    {
                        fclose(log);
                    }
                    else
                    {
                        /* In the event of a failure, default to console */
                        ls->logType = FM_LOG_TYPE_CONSOLE;

                        FM_LOG_ERROR(FM_LOG_CAT_LOGGING,
                                     "Unable to create logfile %s",
                                     ls->logFileName);
                    }
                }

                break;

            case FM_LOG_TYPE_MEMBUF:

                if (clear)
                {
                    ls->maxLines = FM_LOG_MAX_LINES;

                    FM_CLEAR(ls->logBuffer);
                    ls->currentPos = 0;
                }

                break;

            case FM_LOG_TYPE_CALLBACK:
                cbSpec = (fm_logCallBackSpec *) arg;

                ls->fmLogCallback = cbSpec->callBack;
                ls->fmLogCookie1  = cbSpec->cookie1;
                ls->fmLogCookie2  = cbSpec->cookie2;

                break;

        }   /* end switch (logType) */

        posixErr = pthread_mutex_unlock( (pthread_mutex_t *) ls->accessLock );
        if (posixErr != 0)
        {
            FM_LOG_EXIT_API(FM_LOG_CAT_LOGGING, FM_ERR_UNABLE_TO_UNLOCK);
        }
    }
    else
    {
        err = FM_ERR_UNINITIALIZED;
    }

    FM_LOG_EXIT_API(FM_LOG_CAT_LOGGING, err);

}   /* end fmSetLoggingType */




/*****************************************************************************/
/** fmEnableLoggingCategory
 * \ingroup alosLog
 *
 * \desc            Enables logging for specific log categories.
 *
 * \param[in]       categories is a bitmask of category flags (see
 *                  ''Log Categories'').
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_UNINITIALIZED if the logging subsystem has not been
 *                  initialized.
 *
 *****************************************************************************/
fm_status fmEnableLoggingCategory(fm_uint64 categories)
{
    fm_loggingState *ls = (fmRootAlos ? &(fmRootAlos->fmLoggingState) : NULL);

    FM_LOG_ENTRY_API(FM_LOG_CAT_LOGGING,
                     "categories=%15" FM_FORMAT_64 "u\n",
                     categories);

    if ( !LOG_INITIALIZED(ls) )
    {
        FM_LOG_EXIT_API(FM_LOG_CAT_LOGGING, FM_ERR_UNINITIALIZED);
    }

    ls->categoryMask |= categories;

    FM_LOG_EXIT_API(FM_LOG_CAT_LOGGING, FM_OK);

}   /* end fmEnableLoggingCategory */




/*****************************************************************************/
/** fmDisableLoggingCategory
 * \ingroup alosLog
 *
 * \desc            Disables logging for specific log categories.
 *
 * \param[in]       categories is a bitmask of category flags (see
 *                  ''Log Categories'').
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_UNINITIALIZED if the logging subsystem has not been
 *                  initialized.
 *
 *****************************************************************************/
fm_status fmDisableLoggingCategory(fm_uint64 categories)
{
    fm_loggingState *ls = (fmRootAlos ? &(fmRootAlos->fmLoggingState) : NULL);

    FM_LOG_ENTRY_API(FM_LOG_CAT_LOGGING,
                     "categories=%15" FM_FORMAT_64 "u\n",
                     categories);

    if ( !LOG_INITIALIZED(ls) )
    {
        FM_LOG_EXIT_API(FM_LOG_CAT_LOGGING, FM_ERR_UNINITIALIZED);
    }

    ls->categoryMask &= ~categories;

    FM_LOG_EXIT_API(FM_LOG_CAT_LOGGING, FM_OK);

}   /* end fmDisableLoggingCategory */




/*****************************************************************************/
/** fmEnableLoggingLevel
 * \ingroup alosLog
 *
 * \desc            Enables logging for specific log levels.
 *
 * \param[in]       levels is a bitmask of level flags (see
 *                  ''Log Levels'').
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_UNINITIALIZED if the logging subsystem has not been
 *                  initialized.
 *
 *****************************************************************************/
fm_status fmEnableLoggingLevel(fm_uint64 levels)
{
    fm_loggingState *ls = (fmRootAlos ? &(fmRootAlos->fmLoggingState) : NULL);

    FM_LOG_ENTRY_API(FM_LOG_CAT_LOGGING,
                     "levels=%15" FM_FORMAT_64 "u\n",
                     levels);

    if ( !LOG_INITIALIZED(ls) )
    {
        FM_LOG_EXIT_API(FM_LOG_CAT_LOGGING, FM_ERR_UNINITIALIZED);
    }

    ls->levelMask |= levels;

    FM_LOG_EXIT_API(FM_LOG_CAT_LOGGING, FM_OK);

}   /* end fmEnableLoggingLevel */




/*****************************************************************************/
/** fmDisableLoggingLevel
 * \ingroup alosLog
 *
 * \desc            Disables logging for specific log levels.
 *
 * \param[in]       levels is a bitmask of level flags (see
 *                  ''Log Levels'').
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_UNINITIALIZED if the logging subsystem has not been
 *                  initialized.
 *
 *****************************************************************************/
fm_status fmDisableLoggingLevel(fm_uint64 levels)
{
    fm_loggingState *ls = (fmRootAlos ? &(fmRootAlos->fmLoggingState) : NULL);

    FM_LOG_ENTRY_API(FM_LOG_CAT_LOGGING,
                     "levels=%15" FM_FORMAT_64 "u\n",
                     levels);

    if ( !LOG_INITIALIZED(ls) )
    {
        FM_LOG_EXIT_API(FM_LOG_CAT_LOGGING, FM_ERR_UNINITIALIZED);
    }

    ls->levelMask &= ~levels;

    FM_LOG_EXIT_API(FM_LOG_CAT_LOGGING, FM_OK);

}   /* end fmDisableLoggingLevel */




/*****************************************************************************/
/** fmSetLoggingFilter
 * \ingroup alosLog
 *
 * \desc            Set the logging filter by category, level, source code
 *                  function name and source code filename.
 *
 * \param[in]       categoryMask is a bitmask of category flags (see
 *                  ''Log Categories'').
 *
 * \param[in]       levelMask is a bitmask of level flags (see
 *                  ''Log Levels'').
 *
 * \param[in]       functionFilter is the name of the source code function
 *                  on which to filter, or NULL to not filter by function
 *                  name.
 *
 * \param[in]       fileFilter is the name of the source code file
 *                  on which to filter, or NULL to not filter by file name.
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_UNINITIALIZED if the logging subsystem has not been
 *                  initialized.
 *
 *****************************************************************************/
fm_status fmSetLoggingFilter(fm_uint64   categoryMask,
                             fm_uint64   levelMask,
                             const char *functionFilter,
                             const char *fileFilter)
{
    int              posixErr;
    fm_loggingState *ls = (fmRootAlos ? &(fmRootAlos->fmLoggingState) : NULL);

    FM_LOG_ENTRY_API(FM_LOG_CAT_LOGGING,
                     "categoryMask=%15" FM_FORMAT_64 "u, "
                     "levelMask=%15" FM_FORMAT_64 "u, "
                     "functionFilter=%s, "
                     "fileFilter=%s\n",
                     categoryMask,
                     levelMask,
                     functionFilter,
                     fileFilter);

    ls = (fmRootAlos != NULL) ? &fmRootAlos->fmLoggingState : NULL;

    if ( !LOG_INITIALIZED(ls) )
    {
        FM_LOG_EXIT_API(FM_LOG_CAT_LOGGING, FM_ERR_UNINITIALIZED);
    }

    if ( LOG_INITIALIZED(ls) )
    {
        posixErr = pthread_mutex_lock( (pthread_mutex_t *) ls->accessLock );
        if (posixErr != 0)
        {
            FM_LOG_EXIT_API(FM_LOG_CAT_LOGGING, FM_ERR_UNABLE_TO_LOCK);
        }
    }

    ls->categoryMask = categoryMask;
    ls->levelMask    = levelMask;

    if (functionFilter && (strcmp(functionFilter, "") != 0))
    {
        fmStringCopy(ls->functionFilter, functionFilter, FM_LOG_MAX_FILTER_LEN);
    }
    else
    {
        ls->functionFilter[0] = 0;
    }

    if (fileFilter && (strcmp(fileFilter, "") != 0))
    {
        fmStringCopy(ls->fileFilter, fileFilter, FM_LOG_MAX_FILTER_LEN);
    }
    else
    {
        ls->fileFilter[0] = 0;
    }

    if ( LOG_INITIALIZED(ls) )
    {
        posixErr = pthread_mutex_unlock( (pthread_mutex_t *) ls->accessLock );
        if (posixErr != 0)
        {
            FM_LOG_EXIT_API(FM_LOG_CAT_LOGGING, FM_ERR_UNABLE_TO_UNLOCK);
        }
    }

    FM_LOG_EXIT_API(FM_LOG_CAT_LOGGING, FM_OK);

}   /* end fmSetLoggingFilter */




/*****************************************************************************/
/** fmLogMessage
 * \ingroup alosLog
 *
 * \desc            Generate a log message if all filter criteria are met.
 *                  This function is normally invoked by helper macros.
 *
 * \note            This function may be called prior to the logging subsystem
 *                  being initialized. In that case, logging output will be to
 *                  the console only.
 *
 * \param[in]       categories is a bitmask of category flags (see
 *                  ''Log Categories'') which indicate which categories this
 *                  log message belongs to.
 *
 * \param[in]       logLevel is a bitmask of level flags (see
 *                  ''Log Levels'') which indicate which log levels this
 *                  log message belongs to.
 *
 * \param[in]       srcFile is the name of the source code file generating
 *                  the log message. Usually invoked as __FILE__.
 *
 * \param[in]       srcFunction is the name of the source code function
 *                  generating the log message. Usually invoked as __func__.
 *
 * \param[in]       srcLine is the source code line number generating the
 *                  log message. Usually invoked as __LINE__.
 *
 * \param[in]       format is the printf-style format.
 *
 * \param[in]       ... is the printf var-args argument list.
 *
 * \return          FM_OK if successful.
 *
 *****************************************************************************/

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
    fmVLogMessage(categories, logLevel, srcFile, srcFunction, srcLine, format,
                  va);
  va_end(va);
  return status;
}

fm_status fmVLogMessage(fm_uint64   categories,
                       fm_uint64   logLevel,
                       const char *srcFile,
                       const char *srcFunction,
                       fm_uint32   srcLine,
                       const char *format,
                       va_list     ap)
{
    fm_loggingState *ls = (fmRootAlos ? &(fmRootAlos->fmLoggingState) : NULL);
    fm_loggingType   logType;
    fm_bool          enabled;
    fm_char          dateStr[64];
    fm_text          threadName;
    fm_text          threadNamePtr;
    fm_char          thread[64];
    FILE *           log;
    fm_char          logLevelStr[64];
    fm_char          callBackStr[1024];
    fm_uint64        categoryMask;
    fm_uint64        levelMask;
    fm_text          functionFilter;
    fm_text          fileFilter;
    fm_uint32        verbosityMask;
    fm_bool          filter;
    fm_text          levelStr;
    fm_bool          bypassFilter = FALSE;
    int              posixErr;

    static fm_bool   lastLineWithCr = TRUE;
    fm_int           len;

    if (ls && LOG_INITIALIZED(ls) && !ls->enabled)
    {
        return FM_OK;
    }

    if ( ls && LOG_INITIALIZED(ls) )
    {
        categoryMask   = ls->categoryMask;
        levelMask      = ls->levelMask;
        logType        = ls->logType;
        enabled        = ls->enabled;
        functionFilter = ls->functionFilter;
        fileFilter     = ls->fileFilter;
        verbosityMask  = ls->verbosityMask;
    }
    else
    {
        /* use defaults if the logging facility is not initialized */
        categoryMask = FM_LOG_CAT_ALL;
        levelMask    = FM_LOG_LEVEL_FATAL |
                       FM_LOG_LEVEL_ERROR |
                       FM_LOG_LEVEL_WARNING;
        logType        = FM_LOG_TYPE_CONSOLE;
        enabled        = TRUE;
        functionFilter = NULL;
        fileFilter     = NULL;
        verbosityMask  = ~0;
    }

    if (logLevel & FM_LOG_LEVEL_PRINT)
    {
        /**************************************************
         * Make sure FM_LOG_PRINT shows up unconditionally
         * and unadorned.
         **************************************************/
        
        enabled       = TRUE;
        verbosityMask = 0;
        levelMask     = logLevel;
        categoryMask  = FM_LOG_CAT_ALL;
        categories    = FM_LOG_CAT_LOGGING;
        bypassFilter  = TRUE;
    }
    else if (logLevel & FM_LOG_LEVEL_DEFAULT)
    {
        /**************************************************
         * Make sure we don't inadvertently suppress
         * error messages.
         **************************************************/
        
        categoryMask  = FM_LOG_CAT_ALL;
        bypassFilter  = TRUE;
    }

    /**************************************************
     * Generate log output if the log level is set to
     * the print unconditionally, or if the logging
     * system is enabled and we have a category and
     * level match.
     **************************************************/

    if ( enabled
        && (categoryMask & categories)
        && ( (levelMask & logLevel) == logLevel ) )
    {

        if ( ls && LOG_INITIALIZED(ls) )
        {
            posixErr = pthread_mutex_lock( (pthread_mutex_t *) ls->accessLock );
            if (posixErr != 0)
            {
                FM_LOG_EXIT_API(FM_LOG_CAT_LOGGING, FM_ERR_UNABLE_TO_LOCK);
            }
        }

        /* check secondary filters */
        filter = TRUE;

        if (!bypassFilter)
        {
            if (filter && functionFilter && *functionFilter != '\0')
            {
                filter = ApplyLoggingFilter(functionFilter, srcFunction);
            }
    
            if (filter && fileFilter && *fileFilter != '\0')
            {
                filter = ApplyLoggingFilter(fileFilter, srcFile);
            }
        }
        
        if (filter)
        {
            /**************************************************
             * Identify the log level at which this log message
             * was generated.
             **************************************************/
            
            switch (logLevel)
            {
                case FM_LOG_LEVEL_FUNC_ENTRY:
                case FM_LOG_LEVEL_FUNC_ENTRY_API:
                case FM_LOG_LEVEL_FUNC_ENTRY_VERBOSE:
                case FM_LOG_LEVEL_FUNC_ENTRY_API_VERBOSE:
                    levelStr = "ENTRY";
                    break;
                    
                case FM_LOG_LEVEL_FUNC_EXIT:
                case FM_LOG_LEVEL_FUNC_EXIT_API:
                case FM_LOG_LEVEL_FUNC_EXIT_VERBOSE:
                case FM_LOG_LEVEL_FUNC_EXIT_API_VERBOSE:
                    levelStr = "EXIT";
                    break;
                    
                case FM_LOG_LEVEL_WARNING:
                    levelStr = "WARNING";
                    break;
                
                case FM_LOG_LEVEL_ERROR:
                    levelStr = "ERROR";
                    break;
                    
                case FM_LOG_LEVEL_FATAL:
                    levelStr = "FATAL";
                    break;
                    
                case FM_LOG_LEVEL_INFO:
                    levelStr = "INFO";
                    break;
                    
                case FM_LOG_LEVEL_DEBUG:
                case FM_LOG_LEVEL_DEBUG_VERBOSE:
                    levelStr = "DEBUG";
                    break;
                    
                case FM_LOG_LEVEL_PRINT:
                    levelStr = "PRINT";
                    break;
                    
                case FM_LOG_LEVEL_DEBUG2:
                    levelStr = "DEBUG2";
                    break;
                    
                case FM_LOG_LEVEL_DEBUG3:
                    levelStr = "DEBUG3";
                    break;
                    
                case FM_LOG_LEVEL_ASSERT:
                    levelStr = "ASSERT";
                    break;
                    
                default:
                    levelStr = "UNKNOWN_LVL";
                    break;
                    
            }   /* end switch (logLevel) */
            
            FM_SNPRINTF_S(logLevelStr, sizeof(logLevelStr), "%s", levelStr);

            fmGetFormattedTime(dateStr);

            threadNamePtr = fmGetCurrentThreadName();

            if (threadNamePtr)
            {
                threadName = threadNamePtr;
            }
            else
            {
                FM_SNPRINTF_S( thread,
                               sizeof(thread),
                               "<%p>",
                               fmGetCurrentThreadId() );
                threadName = thread;
            }
    
            /* No preappending other info if previous line does not have CR*/
            if(!lastLineWithCr)
                verbosityMask = 0;
            len = strlen(format) - 1;
            if (len < 0)
                len = 0;
            lastLineWithCr = (format[len] == '\n');

            switch (logType)
            {
                case FM_LOG_TYPE_CONSOLE:

                    if (verbosityMask & FM_LOG_VERBOSITY_DATE_TIME)
                    {
                        FM_PRINTF_S("%s:", dateStr);
                    }

                    if (verbosityMask & FM_LOG_VERBOSITY_LOG_LEVEL)
                    {
                        FM_PRINTF_S("%s:", logLevelStr);
                    }

                    if (verbosityMask & FM_LOG_VERBOSITY_THREAD)
                    {
                        FM_PRINTF_S("%s:", threadName);
                    }

                    if (verbosityMask & FM_LOG_VERBOSITY_FILE)
                    {
                        FM_PRINTF_S("%s:", srcFile);
                    }

                    if (verbosityMask & FM_LOG_VERBOSITY_FUNC)
                    {
                        FM_PRINTF_S("%s:", srcFunction);
                    }

                    if (verbosityMask & FM_LOG_VERBOSITY_LINE)
                    {
                        FM_PRINTF_S("%d:", srcLine);
                    }

                    FM_VPRINTF_S(format, ap);
                    fflush(stdout);
                    break;

                case FM_LOG_TYPE_FILE:
                    log = fopen(ls->logFileName, "at");

                    if (log)
                    {
                        if (verbosityMask & FM_LOG_VERBOSITY_DATE_TIME)
                        {
                            FM_FPRINTF_S(log, "%s:", dateStr);
                        }

                        if (verbosityMask & FM_LOG_VERBOSITY_LOG_LEVEL)
                        {
                            FM_FPRINTF_S(log, "%s:", logLevelStr);
                        }

                        if (verbosityMask & FM_LOG_VERBOSITY_THREAD)
                        {
                            FM_FPRINTF_S(log, "%s:", threadName);
                        }

                        if (verbosityMask & FM_LOG_VERBOSITY_FILE)
                        {
                            FM_FPRINTF_S(log, "%s:", srcFile);
                        }

                        if (verbosityMask & FM_LOG_VERBOSITY_FUNC)
                        {
                            FM_FPRINTF_S(log, "%s:", srcFunction);
                        }

                        if (verbosityMask & FM_LOG_VERBOSITY_LINE)
                        {
                            FM_FPRINTF_S(log, "%d:", srcLine);
                        }

                        FM_VFPRINTF_S(log, format, ap);
                        fclose(log);
                    }
                    else
                    {
                        /* Unable to open log file. */
                        FM_LOG_ERROR(FM_LOG_CAT_LOGGING,
                                     "Unable to open logfile %s for appending",
                                     ls->logFileName);

                    }   /* end if (log) */

                    break;

                case FM_LOG_TYPE_MEMBUF:
                    {
                        int lineLen;

                        FM_SNPRINTF_S(ls->logBuffer[ls->currentPos],
                                      FM_LOG_MAX_LINE_SIZE,
                                      "%s:%s:%s:%s:%s:%d:",
                                      dateStr,
                                      logLevelStr,
                                      threadName,
                                      srcFile,
                                      srcFunction,
                                      srcLine);

                        lineLen = strlen(ls->logBuffer[ls->currentPos]);

                        FM_VSNPRINTF_S(&ls->logBuffer[ls->currentPos][lineLen],
                                       FM_LOG_MAX_LINE_SIZE - lineLen,
                                       format,
                                       ap);

                        if (++ls->currentPos >= FM_LOG_MAX_LINES)
                        {
                            ls->currentPos = 0;

                            FM_LOG_INFO(FM_LOG_CAT_LOGGING,
                                        "-- Log Buffer Wrapped --\n");
                        }

                        break;
                    }

                case FM_LOG_TYPE_CALLBACK:

                    if (ls->fmLogCallback)
                    {
                        if (verbosityMask & FM_LOG_VERBOSITY_DATE_TIME)
                        {
                            FM_SNPRINTF_S(callBackStr,
                                          sizeof(callBackStr),
                                          "%s:",
                                          dateStr);
                            ls->fmLogCallback(callBackStr,
                                              ls->fmLogCookie1,
                                              ls->fmLogCookie2);
                        }

                        if (verbosityMask & FM_LOG_VERBOSITY_LOG_LEVEL)
                        {
                            FM_SNPRINTF_S(callBackStr,
                                          sizeof(callBackStr),
                                          "%s:",
                                          logLevelStr);
                            ls->fmLogCallback(callBackStr,
                                              ls->fmLogCookie1,
                                              ls->fmLogCookie2);
                        }

                        if (verbosityMask & FM_LOG_VERBOSITY_THREAD)
                        {
                            FM_SNPRINTF_S(callBackStr,
                                          sizeof(callBackStr),
                                          "%s:",
                                          threadName);
                            ls->fmLogCallback(callBackStr,
                                              ls->fmLogCookie1,
                                              ls->fmLogCookie2);
                        }

                        if (verbosityMask & FM_LOG_VERBOSITY_FILE)
                        {
                            FM_SNPRINTF_S(callBackStr,
                                          sizeof(callBackStr),
                                          "%s:",
                                          srcFile);
                            ls->fmLogCallback(callBackStr,
                                              ls->fmLogCookie1,
                                              ls->fmLogCookie2);
                        }

                        if (verbosityMask & FM_LOG_VERBOSITY_FUNC)
                        {
                            FM_SNPRINTF_S(callBackStr,
                                          sizeof(callBackStr),
                                          "%s:",
                                          srcFunction);
                            ls->fmLogCallback(callBackStr,
                                              ls->fmLogCookie1,
                                              ls->fmLogCookie2);
                        }

                        if (verbosityMask & FM_LOG_VERBOSITY_LINE)
                        {
                            FM_SNPRINTF_S(callBackStr,
                                          sizeof(callBackStr),
                                          "%d:",
                                          srcLine);
                            ls->fmLogCallback(callBackStr,
                                              ls->fmLogCookie1,
                                              ls->fmLogCookie2);
                        }

                        FM_VSNPRINTF_S(callBackStr, sizeof(callBackStr), format, ap);
                        ls->fmLogCallback(callBackStr,
                                          ls->fmLogCookie1,
                                          ls->fmLogCookie2);

                    }   /* end if (ls->fmLogCallback) */

                    break;

            }   /* end switch (logType) */

        }   /* end if (filter) */

        if ( ls && LOG_INITIALIZED(ls) )
        {
            posixErr = pthread_mutex_unlock( (pthread_mutex_t *) ls->accessLock );
            if (posixErr != 0)
            {
                FM_LOG_EXIT_API(FM_LOG_CAT_LOGGING, FM_ERR_UNABLE_TO_UNLOCK);
            }
        }

    }

    return FM_OK;

}   /* end fmVLogMessage */




/*****************************************************************************/
/** fmLogBufferDump
 * \ingroup alosLog
 *
 * \desc            Dump the in-memory log buffer, potentially filtering by
 *                  any of the given arguments.
 *
 * \note            The in-memory log buffer is enabled by calling
 *                  ''fmSetLoggingType'' with a ''fm_loggingType'' argument
 *                  of ''FM_LOG_TYPE_MEMBUF''.
 *
 * \param[in]       threadID is the thread ID to filter by, or NULL to
 *                  not filter by thread ID.
 *
 * \param[in]       srcFile is the source code filename to filter by,
 *                  or NULL to not filter by filename.
 *
 * \param[in]       srcFunction is the source code function to filter by, or
 *                  NULL not filter by function.
 *
 * \param[in]       srcLine is the source code line number to filter by,
 *                  or -1 to not filter by line number.
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_UNINITIALIZED if the logging subsystem has not been
 *                  initialized.
 *
 *****************************************************************************/
fm_status fmLogBufferDump(void *    threadID,
                          fm_text   srcFile,
                          fm_text   srcFunction,
                          fm_uint32 srcLine)
{
    fm_loggingState *ls    = (fmRootAlos ? &(fmRootAlos->fmLoggingState) : NULL);
    fm_bool          valid = TRUE;
    fm_char          idTmp[16], lineTmp[16];

    if ( !LOG_INITIALIZED(ls) )
    {
        return FM_ERR_UNINITIALIZED;
    }

    if (threadID)
    {
        FM_SNPRINTF_S(idTmp, sizeof(idTmp), "<%p", threadID);
    }

    if (srcLine > 0)
    {
        FM_SNPRINTF_S(lineTmp, sizeof(lineTmp), "%d", srcLine);
    }

    for (int i = 0 ; i < FM_LOG_MAX_LINES ; i++)
    {
        valid = TRUE;

        if (strlen(ls->logBuffer[i]) > 0)
        {
            if ( threadID && !strstr(ls->logBuffer[i], idTmp) )
            {
                valid = FALSE;
            }

            if ( srcFunction && !strstr(ls->logBuffer[i], srcFunction) )
            {
                valid = FALSE;
            }

            if ( srcFile && !strstr(ls->logBuffer[i], srcFile) )
            {
                valid = FALSE;
            }

            if ( (srcLine > 0) && !strstr(ls->logBuffer[i], lineTmp) )
            {
                valid = FALSE;
            }

            if (valid)
            {
                printf("%s", ls->logBuffer[i]);
            }
        }
    }

    return FM_OK;

}   /* end fmLogBufferDump */




/*****************************************************************************/
/** fmGetLogBuffer
 * \ingroup alosLog
 *
 * \desc            Copy the in-memory log buffer to a caller-supplied
 *                  character buffer
 *
 * \note            The in-memory log buffer is enabled by calling
 *                  ''fmSetLoggingType'' with a ''fm_loggingType'' argument
 *                  of ''FM_LOG_TYPE_MEMBUF''.
 *
 * \param[in]       buffer points to caller-allocated memory where the 
 *                  memory-based logging output may be copied.
 *
 * \param[in]       size is the size in bytes of buffer.
 *
 * \return          FM_OK if successful
 * \return          FM_ERR_UNINITIALIZED if the logging subsystem has not been
 *                  initialized.
 *
 *****************************************************************************/
fm_status fmGetLogBuffer(fm_text buffer, fm_int size)
{
    fm_loggingState *ls;
    fm_int           i;
    fm_int           length;
    fm_int           position;
    fm_char *        cursor;

    ls = fmRootAlos ? &(fmRootAlos->fmLoggingState) : NULL;

    if ( !LOG_INITIALIZED(ls) )
    {
        return FM_ERR_UNINITIALIZED;
    }

    position = ls->currentPos;

    cursor = buffer;

    for (i = 0 ; i < position ; i++)
    {
        length = (fm_int) strlen(ls->logBuffer[i]);

        length = size > length ? length : size;

        FM_STRNCPY_S(cursor,
                     size,
                     ls->logBuffer[i],
                     length);

        cursor += length;

        size -= length;

        if (size == 0)
        {
            break;
        }

    }   /* end for (i = 0 ; i < position ; i++) */

    buffer[size - 1] = '\0';

    return FM_OK;

}   /* end fmGetLogBuffer */




/*****************************************************************************/
/** fmGetLoggingAttribute
 * \ingroup alosLog
 *
 * \desc            Returns an attribute of the logging subsystem.
 *
 * \param[in]       attr is the attribute to be returned. See
 *                  ''Log Attributes'' for a list of available
 *                  attributes.
 *
 * \param[in]       size is the size in bytes of the buffer pointed to by
 *                  value. Used only for attributes whose value is of 
 *                  variable length.
 *
 * \param[out]      value points to a caller-allocated storage where this
 *                  function should place the attribute value. The data
 *                  type of value is indicated for each attribute (see
 *                  ''Log Attributes'').
 *
 * \return          FM_OK if successful
 * \return          FM_ERR_INVALID_ATTRIB if attr is not recognized.
 * \return          FM_ERR_UNINITIALIZED if the logging subsystem has not been
 *                  initialized.
 *
 *****************************************************************************/
fm_status fmGetLoggingAttribute(fm_int  attr,
                                fm_int  size,
                                void *  value)
{
    fm_loggingState *ls = (fmRootAlos ? &(fmRootAlos->fmLoggingState) : NULL);
    fm_status err = FM_OK;

    FM_LOG_ENTRY_API(FM_LOG_CAT_LOGGING,
                     "attr=%d, size=%d, value=%p\n",
                     attr,
                     size,
                     (void *) value);

    if ( !LOG_INITIALIZED(ls) )
    {
        FM_LOG_EXIT_API(FM_LOG_CAT_LOGGING, FM_ERR_UNINITIALIZED);
    }

    switch (attr)
    {
        case FM_LOG_ATTR_ENABLED:
            *( (fm_bool *) value ) = ls->enabled;
            break;

        case FM_LOG_ATTR_LOG_TYPE:
            *( (fm_int *) value ) = ls->logType;
            break;

        case FM_LOG_ATTR_CATEGORY_MASK:
            *( (fm_uint64 *) value ) = ls->categoryMask;
            break;

        case FM_LOG_ATTR_LEVEL_MASK:
            *( (fm_uint64 *) value ) = ls->levelMask;
            break;

        case FM_LOG_ATTR_VERBOSITY_MASK:
            *( (fm_uint32 *) value ) = ls->verbosityMask;
            break;

        case FM_LOG_ATTR_FILE_FILTER:
            fmStringCopy( (char *) value, ls->fileFilter, size );
            break;

        case FM_LOG_ATTR_FUNCTION_FILTER:
            fmStringCopy( (char *) value, ls->functionFilter, size );
            break;

        case FM_LOG_ATTR_LOG_FILENAME:
            fmStringCopy( (char *) value, ls->logFileName, size );
            break;

        default:
            err = FM_ERR_INVALID_ATTRIB;
            break;
    }

    FM_LOG_EXIT_API(FM_LOG_CAT_LOGGING, err);

}   /* end fmGetLoggingAttribute */




/*****************************************************************************/
/** fmResetLogging
 * \ingroup alosLog
 *
 * \desc            Resets the logging subsystem to its default state.
 *
 * \return          FM_OK if successful
 * \return          FM_ERR_UNINITIALIZED if the logging subsystem has not been
 *                  initialized.
 *
 *****************************************************************************/
fm_status fmResetLogging(void)
{
    fm_loggingState *ls = (fmRootAlos ? &(fmRootAlos->fmLoggingState) : NULL);

    FM_LOG_ENTRY_API(FM_LOG_CAT_LOGGING, "\n");

    if ( !LOG_INITIALIZED(ls) )
    {
        FM_LOG_EXIT_API(FM_LOG_CAT_LOGGING, FM_ERR_UNINITIALIZED);
    }

    ls->logType       = FM_LOG_TYPE_CONSOLE;
    ls->enabled       = TRUE;
    ls->verbosityMask = FM_LOG_VERBOSITY_DEFAULT;

    ls->categoryMask = FM_LOG_CAT_DEFAULT;
    ls->levelMask    = FM_LOG_LEVEL_DEFAULT;

    ls->functionFilter[0] = 0;
    ls->fileFilter[0] = 0;

    FM_LOG_EXIT_API(FM_LOG_CAT_LOGGING, FM_OK);

}   /* end fmResetLogging */



/*****************************************************************************/
/** fmGetCallerName
 * \ingroup intLogging
 *
 * \desc            Generate a string containing the call stack. The stack
 *                  will be listed starting with the caller of this
 *                  function, followed by the caller of that function, etc.
 *                  Used for diagnostic purposes only.
 *
 * \note            The Fulcrum ALOS implementation relies on GNU extensions.
 *                  For non-GNU environments, this implementation will always
 *                  return only "!".
 *
 * \param[out]      buf points to a caller-allocated character array into
 *                  which this function should place the call stack string.
 *
 * \param[in]       bufSize is the length of buf in bytes.
 *
 * \param[in]       callerCount is the maximum number of stack frame levels
 *                  to return.
 *
 * \param[in]       delimiter is the string to insert between reported call
 *                  stack entries (e.g., "\n").
 *
 * \return          None.
 *
 *****************************************************************************/
void fmGetCallerName(fm_text buf,
                     fm_int  bufSize,
                     fm_int  callerCount,
                     fm_text delimiter)
{
#ifndef __gnu_linux__
    FM_NOT_USED(callerCount);
    
    if (bufSize >= 2)
    {
        buf[0] = '!';
        buf[1] = '\0';
    }
#else
    char **names;
    void *btBuffer[callerCount + 1];
    int btNum;
    int stackDepth;
    fm_int i;
    char *shortName;
    char *first;
    char *last;
    
    stackDepth = (int) callerCount + 1;
    
    /* Get the call stack as addresses */
    btNum = backtrace(btBuffer, stackDepth);
    
    /* Convert addresses to symbols */
    names = backtrace_symbols(btBuffer, btNum);

    if (names == NULL)
    {
        if (bufSize >= 2)
        {
            buf[0] = '!';
            buf[1] = '\0';
        }
    }
    else
    {
        fmStringCopy(buf, "", bufSize);
        
        /**************************************************
         * Call stack will be listed from latest to 
         * earliest. Skip the first entry since that is
         * for this function.
         **************************************************/
        
        for (i = 1 ; i < btNum - 1 ; i++)
        {
            shortName = names[i];

            first = strchr(shortName, '(');
            last  = strchr(shortName, ')');

            if (first == NULL || last == NULL)
            {
                first = strchr(shortName, '[');
                last  = strchr(shortName, ']');
            }

            if (first != NULL && last != NULL)
            {
                *last     = '\0';
                shortName = first + 1;
            }

            fmStringAppend(buf, shortName, bufSize);
            
            /* If not the second-to-last entry... */
            if (i < btNum - 2)
            {
                /* ...insert a delimiter string. */
                fmStringAppend(buf, delimiter, bufSize);
            }
            
        }   /* end for (i = 1 ; i < btNum - 1 ; i++) */
        
        /**************************************************
         * We use free instead of fm_free, because names
         * was allocated with malloc by backtrace_symbols.
         **************************************************/
        
        free(names);
        
    }   /* end if (names == NULL) */
    
#endif

}   /* end fmGetCallerName */



