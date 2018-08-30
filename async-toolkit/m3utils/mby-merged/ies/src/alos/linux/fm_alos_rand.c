/* vim:ts=4:sw=4:expandtab
 * (No tabs, indent level is 4 spaces)  */
/*****************************************************************************
 * File:           fm_alos_rand.c
 * Creation Date:  January 30, 2013
 * Description:    ALOS routines for dealing with rand abstractly
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


#include <fm_sdk_int.h>


/*****************************************************************************
 * Macros, Constants & Types
 *****************************************************************************/


/*****************************************************************************
 * Global Variables
 *****************************************************************************/


/*****************************************************************************
 * Local Variables
 *****************************************************************************/
static pthread_mutex_t rand_lock;
static fm_uint         rand_seed = 1;
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
/** fmRand
 * \ingroup intAlosRand
 *
 * \desc            pseudo-random number generator 
 *
 * \return          a pseudo-random integer between 0 and RAND_MAX inclusive
 *                  (i.e., the mathematical range [0, RAND_MAX]). 
 *
 *
 *****************************************************************************/
fm_int fmRand(void)
{ 
    fm_int randResult;
    int ret;

    ret = pthread_mutex_lock(&rand_lock);
    if (ret == 0)
    {
        randResult = rand_r(&rand_seed);
        ret = pthread_mutex_unlock(&rand_lock);
        if (ret != 0)
        {
             FM_LOG_ERROR(FM_LOG_CAT_ALOS,
                          "Error %d from pthread_mutex_unlock\n",
                          ret);
        }
    }
    else
    {
        randResult = 0;
        FM_LOG_ERROR(FM_LOG_CAT_ALOS,
                     "Error %d from pthread_mutex_lock\n",
                     ret);
    }
    return randResult;
}


/*****************************************************************************/
/** fmAlosRandInit
 * \ingroup intAlosRand
 *
 * \desc            Initializes the rand subsystem.
 *
 * \note            This is an internal function that should only be called
 *                  from fm_alos_init.c.
 *
 * \return          FM_OK on success.
 *
 *****************************************************************************/
fm_status fmAlosRandInit(void)
{

    if (pthread_mutex_init(&rand_lock, NULL) != 0)
    {
         FM_LOG_EXIT(FM_LOG_CAT_ALOS, FM_ERR_LOCK_INIT);
    }
    return FM_OK;
}
