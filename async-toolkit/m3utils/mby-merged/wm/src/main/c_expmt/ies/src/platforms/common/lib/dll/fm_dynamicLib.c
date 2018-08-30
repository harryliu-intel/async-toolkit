/* vim:ts=4:sw=4:expandtab
 * (No tabs, indent level is 4 spaces)  */
/*****************************************************************************
 * File:            fm_dynamicLib.c
 * Creation Date:   Mar. 15, 2011
 * Description:     This file provides dynamic library support for the API
 *                  and platform.
 *              
 * INTEL CONFIDENTIAL
 * Copyright 2011  Intel Corporation. All Rights Reserved. 
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
 * Local function prototypes.
 *****************************************************************************/


/*****************************************************************************
 * Global Variables
 *****************************************************************************/


/*****************************************************************************
 * Local Variables
 *****************************************************************************/

 
/*****************************************************************************
 * Local Functions
 *****************************************************************************/
 

/*****************************************************************************
 * Public Functions
 *****************************************************************************/




/*****************************************************************************/
/** fmLoadAndInitDynamicLib
 * \ingroup dynLib
 *
 * \desc            This function loads a dynamic-load library then optionally
 *                  calls an initialization function.
 *
 * \param[in]       libPath contains the path to the library to be loaded.
 *
 * \param[in]       initFunc contains the name of the initialization function
 *                  to be called. NULL if no initialization function should
 *                  be called.
 *
 * \param[in]       funcArg is passed through to the init function.
 *
 * \param[out]      libHandle points to caller-provided storage into which
 *                  the library's handle will be stored.
 *
 * \return          Description of one possible return value.
 * \return          Description of another possible return value.
 *
 *****************************************************************************/
fm_status fmLoadAndInitDynamicLib(fm_text libPath,
                                  fm_text initFunc,
                                  void *  funcArg,
                                  fm_int *libHandle)
{
    fm_status          status;
    fm_int             handle;
    void *             initAddr;
    fm_sdlInitFunction initFuncPtr;
    union
    {
        fm_sdlInitFunction func;
        void *             obj;
    } alias;

    FM_LOG_ENTRY( FM_LOG_CAT_PLATFORM,
                  "libPath = %p (%s), initFunc = %p (%s), funcArg = %p, "
                  "libHandle = %p\n",
                  (void *) libPath,
                  (libPath != NULL) ? libPath : "<NULL>",
                  (void *) initFunc,
                  (initFunc != NULL) ? initFunc : "<NULL>",
                  (void *) funcArg,
                  (void *) libHandle );

    status = fmOpenDynamicLoadLibrary(libPath, &handle);

    FM_LOG_EXIT_ON_ERR(FM_LOG_CAT_PLATFORM, status);

    *libHandle = handle;

    if (initFunc != NULL)
    {
        status = fmGetDynamicLoadSymbol(handle, initFunc, &initAddr);

        FM_LOG_EXIT_ON_ERR(FM_LOG_CAT_PLATFORM, status);

        alias.obj   = initAddr;
        initFuncPtr = alias.func;

        status = initFuncPtr(funcArg);
    }

    FM_LOG_EXIT(FM_LOG_CAT_PLATFORM, status);

}   /* end fmLoadAndInitDynamicLib */




/*****************************************************************************/
/** fmCallDynamicLibFunc
 * \ingroup dynLib
 *
 * \desc            This calls a function in a dynamic library.
 *
 * \param[in]       libHandle contains the library's handle.
 *
 * \param[in]       funcName contains the name of the function to be called.
 *
 * \param[in]       funcArg is passed through to the function.
 *
 * \return          Description of one possible return value.
 * \return          Description of another possible return value.
 *
 *****************************************************************************/
fm_status fmCallDynamicLibFunc(fm_int libHandle,
                               fm_text funcName,
                               void *  funcArg)
{
    fm_status          status;
    void *             funcAddr;
    fm_sdlInitFunction funcPtr;
    union
    {
        fm_sdlInitFunction func;
        void *             obj;
    } alias;

    FM_LOG_ENTRY( FM_LOG_CAT_PLATFORM,
                  "libHandle = %d, funcName = %p (%s), funcArg = %p\n",
                  libHandle,
                  (void *) funcName,
                  (funcName != NULL) ? funcName : "<NULL>",
                  (void *) funcArg );

    if (funcName != NULL)
    {
        status = fmGetDynamicLoadSymbol(libHandle, funcName, &funcAddr);

        FM_LOG_EXIT_ON_ERR(FM_LOG_CAT_PLATFORM, status);

        alias.obj = funcAddr;
        funcPtr   = alias.func;

        status = funcPtr(funcArg);
    }
    else
    {
        status = FM_OK;
    }

    FM_LOG_EXIT(FM_LOG_CAT_PLATFORM, status);

}   /* end fmCallDynamicLibFunc */
