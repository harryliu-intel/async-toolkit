
#ifndef _MBY_SRV_LOG_H_
#define _MBY_SRV_LOG_H_

#include <stdio.h>
#include "mby_srv_errno.h"

//#define VERBOSE
#define DEBUG_PRINT_RECEIVED_PACKET


// TODO this is currently unused
typedef struct _fm_libCfg
{
    /* log debug level */
    fm_int  logLevel;

    /* Callback for displaying log messages */
    void (*logHandler)(fm_uint64 level, char *log);
} fm_libCfg;

/* Dummy logging functions
 * At the moment they all go straight to std out
 */
#define DUMMY_LOG(cat, ...) printf(__VA_ARGS__);

#define FM_LOG_FATAL DUMMY_LOG
#define FM_LOG_ERROR DUMMY_LOG
#define FM_LOG_WARNING DUMMY_LOG
#define FM_LOG_INFO DUMMY_LOG
#define FM_LOG_DEBUG DUMMY_LOG
#define FM_LOG_DEBUG2 DUMMY_LOG
#define FM_LOG_DEBUG_VERBOSE DUMMY_LOG
#define FM_LOG_PRINTF DUMMY_LOG
#define FM_LOG_PRINT(...) printf(__VA_ARGS__);
#define FM_LOG_ENTRY DUMMY_LOG

#define FM_LOG_ENTRY_VERBOSE(cat, ...) \
    printf("Entering... " __VA_ARGS__ );

#define FM_LOG_EXIT(cat, result) \
{ \
    printf("Exit function %s - result = %d\n", __func__, result); \
    return result; \
} \

#define FM_LOG_EXIT_VERBOSE(cat, errcode)                                  \
{                                                                          \
    printf( "Exit Status %d (%s)\n",                                       \
            (errcode),                                                     \
            fmErrorMsg( (errcode) ) );                                     \
    return errcode;                                                        \
}

#define FM_LOG_ASSERT(cat, cond, ...) \
    if (!(cond)) \
{ \
    FM_LOG_PRINT( __VA_ARGS__ ); \
}

#define FM_LOG_ABORT_ON_ERR(cat, errcode)                                  \
{                                                                          \
    fm_status localError = (errcode);                                      \
    if ( (localError) != FM_OK )                                           \
    {                                                                      \
        FM_LOG_DEBUG((cat), "Break to abort handler: %s\n",                \
                     fmErrorMsg((localError)));                            \
        goto ABORT;                                                        \
    }                                                                      \
}

#if 0
#define FM_LOG_SYS_EXIT_ON_COND(cat, cond)                  \
    if ((cond)) {                                           \
        strErrNum = FM_STRERROR_S(strErrBuf,                \
                                  FM_STRERROR_BUF_SIZE,     \
                                  errno);                   \
        if (strErrNum == 0)                                 \
        {                                                   \
            FM_LOG_FATAL((cat),                             \
                         "System error %d: %s\n",           \
                         errno, strErrBuf);                 \
        }                                                   \
        else                                                \
        {                                                   \
            FM_LOG_FATAL((cat),                             \
                         "System error %d\n", errno);       \
        }                                                   \
        FM_LOG_EXIT_VERBOSE((cat), FM_FAIL);                \
    }
#else
#define FM_LOG_SYS_EXIT_ON_COND(cat, cond)                  \
    if ((cond)) {                                           \
        strerror_r(errno, strErrBuf,                        \
                   FM_STRERROR_BUF_SIZE);                   \
        FM_LOG_FATAL((cat),                                 \
                     "System error %d: %s\n",               \
                     errno, strErrBuf);                     \
        FM_LOG_EXIT_VERBOSE((cat), FM_FAIL);                \
    }
#endif


/* Dummy logging categories
 * Defined only to solve compilation erros, at the moment they are ignored.
 */
#define FM_LOG_CAT_PLATFORM 0

#endif /* _MBY_SRV_LOG_H_ */
