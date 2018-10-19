/* vim:ts=4:sw=4:expandtab
 * (No tabs, indent level is 4 spaces)  */
/*****************************************************************************
 * File:            fm_alos_init.c
 * Creation Date:   September 16, 2005
 * Description:     Initialization for ALOS module.
 *
 * INTEL CONFIDENTIAL
 * Copyright 2005 - 2013 Intel Corporation. All Rights Reserved.
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
#include <dlfcn.h>

/*****************************************************************************
 * Macros, Constants & Types
 *****************************************************************************/


/*****************************************************************************
 * Global Variables
 *****************************************************************************/

fm_rootAlos *fmRootAlos = NULL;

/*****************************************************************************
 * Local Variables
 *****************************************************************************/


/*****************************************************************************
 * Local function prototypes.
 *****************************************************************************/


/*****************************************************************************
 * Local Functions
 *****************************************************************************/

/*****************************************************************************/
/** fmAlosRootInit
 * \ingroup intAlosInit
 *
 * \desc            Initialize ALOS state.
 *
 * \param[in]       None.
 *
 * \return          FM_OK if successful. 
 * \return          FM_ERR_NO_MEM if unable to allocate memory.
 *
 *****************************************************************************/
static fm_status fmAlosRootInit(void)
{
    fm_status           err;
    int                 ret;
    pthread_mutexattr_t attr;
    fm_bool             attrInit;

    FM_LOG_ENTRY(FM_LOG_CAT_ALOS, "(no arguments)\n");

    attrInit = FALSE;

    /* Allocate memory for ALOS state. */
    fmRootAlos = fmAlloc( sizeof(fm_rootAlos) );

    if (fmRootAlos == NULL)
    {
        /* Could not get memory. */
        err = FM_ERR_NO_MEM;
        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_ALOS, err);
    }

    err = fmAlosLockInit();
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_ALOS, err);

    if ( pthread_mutexattr_init(&attr) )
    {
        err = FM_ERR_LOCK_INIT;
        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_ALOS_SEM, err);
    }

    attrInit = TRUE;

    if ( pthread_mutexattr_setpshared(&attr, PTHREAD_PROCESS_SHARED) )
    {
        err = FM_ERR_LOCK_INIT;
        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_ALOS_SEM, err);
    }

    ret = pthread_mutex_init(&fmRootAlos->treeTreeLock, &attr);

    if (ret != 0)
    {
        err = FM_ERR_LOCK_INIT;
        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_DEBUG, err);
    }

    /* Initialize the tree of trees */
    fmTreeInit(&fmRootAlos->treeTree);

    err = fmAlosRwlockInit();
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_ALOS, err);

    err = fmAlosSemInit();
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_ALOS, err);

    err = fmAlosLoggingInit();
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_ALOS, err);

    err = fmInitializeApiAttributes();
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_ALOS, err);

    err = fmInitDynamicLoadLibs();
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_ALOS, err);

    err = fmAlosRandInit();
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_ALOS, err);


ABORT:

    if (attrInit)
    {
        pthread_mutexattr_destroy(&attr);
    }

    FM_LOG_EXIT(FM_LOG_CAT_ALOS, err);

}   /* end fmAlosRootInit */




/*****************************************************************************
 * Public Functions
 *****************************************************************************/


/*****************************************************************************/
/** fmOSInitialize
 * \ingroup alosInit
 *
 * \desc            Initialize the operating system abstraction module.
 *                                                                      \lb\lb
 *                  One of the design goals of the API is to have it
 *                  support multiple simultaneous client processes. To this
 *                  end, it is necessary that a common API state be shared
 *                  among all calling processes. This is achieved by 
 *                  sharing the API's data memory so it is accessible from
 *                  any calling process. This function calls shmget and shmat
 *                  for this purpose. 
 *
 * \param           None.
 *
 * \return          FM_OK
 *
 *****************************************************************************/
fm_status fmOSInitialize()
{
    fm_status      err;
    static fm_bool initializedAlready = FALSE;

    FM_LOG_ENTRY(FM_LOG_CAT_ALOS, "(no arguments)\n");

    if (initializedAlready)
    {
        /* ALOS has already been initialized. */
        FM_LOG_EXIT(FM_LOG_CAT_ALOS, FM_OK);
    }
    else
    {
        initializedAlready = TRUE;
    }

    /* Map API memory as shared for the calling process. */
    err = fmMemInitialize();
    FM_LOG_EXIT_ON_ERR(FM_LOG_CAT_ALOS, err);

    /* Register the calling proccess */
    err = fmAlosThreadInit();
    FM_LOG_EXIT_ON_ERR(FM_LOG_CAT_ALOS, err);

    /* Initialize fmRootAlos */
    err = fmGetRoot("alos", (void **) &fmRootAlos, fmAlosRootInit);
    FM_LOG_EXIT_ON_ERR(FM_LOG_CAT_ALOS, err);

    FM_LOG_EXIT(FM_LOG_CAT_ALOS, err);

}   /* end fmOSInitialize */


/*****************************************************************************/
/** fmInitProcess
 * \ingroup alosInit
 *
 * \desc            Performs per-calling process ALOS initialization.
 *                  Presently, all that is done is to initialize thread
 *                  local storage, which is needed by the lock inversion
 *                  defense.
 *
 * \param           None.
 *
 * \return          FM_OK if successful.
 * \return          FM_FAIL if not successful.
 *
 *****************************************************************************/
fm_status fmInitProcess(void)
{
    fm_status err;
    
    FM_LOG_ENTRY(FM_LOG_CAT_ALOS, "(no arguments)\n");
    
    /* Initialize thread lock collection */
    err = fmInitThreadLockCollection();
    
    FM_LOG_EXIT(FM_LOG_CAT_ALOS, err);
    
}   /* end fmInitProcess */



