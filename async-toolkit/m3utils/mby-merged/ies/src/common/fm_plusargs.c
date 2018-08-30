/* vim:ts=4:sw=4:expandtab
 * (No tabs, indent level is 4 spaces)  */
/*****************************************************************************
 * File:           fm_plusargs.c
 * Creation Date:  March 3, 2016
 * Description:    Functions to access SystemVerilog plus arguments
 *
 * INTEL CONFIDENTIAL
 * Copyright 2007 - 2012 Intel Corporation. All Rights Reserved.
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
typedef int (*fm_testPlusargsHandler)(const char *arg);
/*****************************************************************************
 * Global Variables
 *****************************************************************************/

/*****************************************************************************
 * Local Variables
 *****************************************************************************/

/*****************************************************************************/
/** testPlusargsHandler
 *
 * \desc            Handler to hold function pointer from dpi world.
 *
 *****************************************************************************/
fm_testPlusargsHandler testPlusargsHandler=(void *)0;

/*****************************************************************************
 * Local function prototypes.
 *****************************************************************************/

/*****************************************************************************
 * Local Functions
 *****************************************************************************/

/*****************************************************************************/
/** printThePointer
 * \ingroup modelHLP
 *
 * \desc            Used to print out debug messages for testPlusargsHandler.
 *                  Prints address of testPlusArgsHandler and the contents of
 *                  testPlusargsHandler which is an address to a function.
 *
 * \param[in]       db_message is used to print out where printThePointer is
 *                  is being called from.
 *
 *****************************************************************************/
static void printThePointer(const char *db_message)
{
  unsigned char *p;
  p = (unsigned char *)&testPlusargsHandler;
  FM_LOG_PRINTF(0, FM_LOG_CAT_PLATFORM, "%s &testPlusArgsHandler=0x%lx "
    "testPlusArgsHandler", db_message, (unsigned long)p);
  for (int i = (sizeof testPlusargsHandler)-1; i >=0 ; --i)
  {
    FM_LOG_PRINTF(0, FM_LOG_CAT_PLATFORM, "%02x", p[i]);
  }
  FM_LOG_PRINTF(0, FM_LOG_CAT_PLATFORM, "\n");
} /* end printThePointer */

/*****************************************************************************
 * Public Functions
 *****************************************************************************/

/*****************************************************************************/
/** set_testPlusArgs
 * \ingroup modelHLP
 *
 * \desc            Used set testPlusargsHandler to the function pointer that
 *                  will be used by testPlusArgs()
 *
 * \param[in]       h is a function pointer.
 *
 *****************************************************************************/
void set_testPlusArgs(fm_testPlusargsHandler h)
{
  testPlusargsHandler=h;
  if(testPlusargsHandler == (void *)0)
    FM_LOG_DEBUG(FM_LOG_CAT_PLATFORM, "set_testPlusArgs(handler) handler not "
        "valid\n");
} /* end set_testPlusArgs */

/*****************************************************************************/
/** testPlusArgs
 * \ingroup modelHLP
 *
 * \desc            Uses testPlusargsHanlder to see if arg is a command line
 *                  plusarg.
 *
 * \param[in]       arg is the string we are testing against.
 *
 *****************************************************************************/
int testPlusArgs(const char *arg)
{
  if(testPlusargsHandler == (void *)0)
    return -1;
  else
    return (testPlusargsHandler)(arg);
} /* end testPlusArgs */

