/* vim:ts=4:sw=4:expandtab
 * (No tabs, indent level is 4 spaces)  */
/*****************************************************************************
 * File:            fm_alos_time.c
 * Creation Date:   2005
 * Description:     Timestamp wrapping and comparison functions
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


#include <time.h>
#include <string.h>
#include <errno.h>
#include "mby_srv_time.h"
#include "mby_srv_log.h"

/*****************************************************************************
 * Macros, Constants & Types
 *****************************************************************************/


/*****************************************************************************
 * Global Variables
 *****************************************************************************/

const fm_timestamp fmNoWaitTimeConstant =
{
    0, 0
};

/*****************************************************************************
 * Local Variables
 *****************************************************************************/


/*****************************************************************************
 * Local function prototypes.
 *****************************************************************************/


/*****************************************************************************
 * Local Functions
 *****************************************************************************/


/*****************************************************************************
 * Public Functions
 *****************************************************************************/


/*****************************************************************************/
/** fmCompareTimestamps
 * \ingroup alosTime
 *
 * \desc            Compare two timestamp objects.
 *
 * \param[in]       t1 is a pointer to one fm_timestamp object.
 *
 * \param[in]       t2 is a pointer to the other fm_timestamp object.
 *
 * \return          -1 if t1 < t2
 * \return          0 if t1 = t2
 * \return          1 if t1 > t2
 *
 *****************************************************************************/
fm_int fmCompareTimestamps(fm_timestamp *t1, fm_timestamp *t2)
{
    if ((t1->sec < t2->sec) || ((t1->sec == t2->sec) && (t1->usec < t2->usec)))
    {
        return -1;
    }
    else if (t1->sec == t2->sec && t1->usec == t2->usec)
    {
        return 0;
    }
    else
    {
        return 1;
    }

}   /* end fmCompareTimestamps */




/*****************************************************************************/
/** fmAddTimestamps
 * \ingroup intAlosTime
 *
 * \desc            Add two timestamp objects together: t1 = t1 + t2.
 *
 * \param[in,out]   t1 is a pointer to one fm_timestamp object.
 *
 * \param[in]       t2 is a pointer to the other fm_timestamp object.
 *
 * \return          None
 *
 *****************************************************************************/
void fmAddTimestamps(fm_timestamp *t1, fm_timestamp *t2)
{
    t1->sec  += t2->sec;
    t1->usec += t2->usec;

    if (t1->usec > 1000000)
    {
        t1->sec += (t1->usec / 1000000);
        t1->usec = (t1->usec % 1000000);
    }

}   /* end fmAddTimestamps */




/*****************************************************************************/
/** fmSubTimestamps
 * \ingroup alosTime
 *
 * \desc            Subtract one timestamp object from another: t3 = t1 - t2.
 *
 * \param[in]       t1 is a pointer to the minuend fm_timestamp object.
 *
 * \param[in]       t2 is a pointer to the subtrahend fm_timestamp object.
 *
 * \param[out]      t3 is a pointer to the difference fm_timestamp object.
 *
 * \return          None
 *
 *****************************************************************************/
void fmSubTimestamps(const fm_timestamp *t1,
                     const fm_timestamp *t2,
                     fm_timestamp *      t3)
{
    fm_int64 usecDif;

    t3->sec  = t1->sec - t2->sec;
    t3->usec = t1->usec - t2->usec;
    usecDif  = (fm_int64) t3->usec;

    if (usecDif < 0)
    {
        t3->sec  -= 1;
        t3->usec += 1000000;
    }

}   /* end fmSubTimestamps */




/*****************************************************************************/
/** fmGetTime
 * \ingroup alosTime
 *
 * \desc            Get the number of seconds and microseconds since
 *                  the Epoch.
 *
 * \param[out]      ts points to a caller-allocated fm_timestamp structure
 *                  where this function should place the current time.
 *
 * \return          FM_OK if successful.
 * \return          FM_FAIL if not successful.
 *
 *****************************************************************************/
fm_status fmGetTime(fm_timestamp *ts)
{
    struct timespec tv;

    FM_LOG_ENTRY(FM_LOG_CAT_ALOS, "ts=%p\n", (void *) ts);

    /* use CLOCK_MONOTONIC to be not sesitive to sysTime change */
    if (clock_gettime(CLOCK_MONOTONIC, &tv) != 0)
    {
        FM_LOG_EXIT(FM_LOG_CAT_ALOS, FM_FAIL);
    }

    ts->sec  = tv.tv_sec;
    ts->usec = tv.tv_nsec/1000;

    FM_LOG_EXIT(FM_LOG_CAT_ALOS, FM_OK);

}   /* end fmGetTime */




/*****************************************************************************/
/** fmGetFormattedTime
 * \ingroup intAlosTime
 *
 * \desc            Fills the provided buffer with a string containing the
 *                  formatted current time.
 *
 * \param[out]      dateStr will be filled with a string representing the
 *                  current time, in the same format as the standard library
 *                  function ctime, except without a newline on the end.
 *                  The string is 24 characters long, but the provided
 *                  buffer must be 26 characters long.  (One for the
 *                  terminating NUL character, and one for internal
 *                  scratch purposes.)
 *
 * \return          FM_OK if successful.
 * \return          FM_FAIL if not successful.
 *
 *****************************************************************************/
fm_status fmGetFormattedTime(char *dateStr)
{
    time_t timeSinceEpoch = time(NULL);

    /***************************************************
     * Specifically do *NOT* log this function since it
     * is called by the logging facility itself.
     **************************************************/

    if ( !ctime_r(&timeSinceEpoch, dateStr) )
    {
        return FM_FAIL;
    }

    /* chop off newline */
    *(dateStr + strlen(dateStr) - 1) = 0;

    return FM_OK;

}   /* end fmGetFormattedTime */




/*****************************************************************************/
/** fmDelay
 * \ingroup alosTime
 *
 * \desc            Blocks for a given amount of time.  The delay is given as
 *                  two arguments, seconds (s) and nanoseconds (n), where the
 *                  total delay would be s + (1.0e-9 * n) seconds.  Note that
 *                  most systems will only be able to block for a minimum
 *                  duration of 1us or sometimes even higher.
 *
 * \param[in]       seconds is the number of seconds to wait
 *
 * \param[in]       nanoseconds is the additional amount of nanoseconds to
 *                  wait.
 *
 * \return          FM_OK if successful.
 * \return          FM_FAIL if not successful.
 *
 *****************************************************************************/
fm_status fmDelay(fm_int seconds, fm_int nanoseconds)
{
    struct timespec delaytime;
    struct timespec remaintime;
    int rc=0;

    FM_LOG_ENTRY(FM_LOG_CAT_ALOS, "seconds=%d nanoseconds=%d\n",
                 seconds, nanoseconds);

    delaytime.tv_sec  = seconds;
    delaytime.tv_nsec = nanoseconds;

    do
    {
        remaintime.tv_sec  = 0;
        remaintime.tv_nsec = 0;
        rc = nanosleep(&delaytime, &remaintime);
        delaytime = remaintime;
    } while ((rc == EINTR) && (remaintime.tv_sec != 0 || remaintime.tv_nsec != 0));

    FM_LOG_EXIT(FM_LOG_CAT_ALOS, (rc == 0) ? FM_OK : FM_FAIL);

}   /* end fmDelay */


