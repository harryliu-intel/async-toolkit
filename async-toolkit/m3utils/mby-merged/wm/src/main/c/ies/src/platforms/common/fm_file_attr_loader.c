/* vim:ts=4:sw=4:expandtab
 * (No tabs, indent level is 4 spaces)  */
/*****************************************************************************
 * File:            fm_file_attr_loader.c
 * Creation Date:   September 10, 2007
 * Description:     A file based attribute loader for general use from
 *                  the platform layer.
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

/*****************************************************************************
 * Global Variables
 *****************************************************************************/

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
/** fmPlatformLoadAttributes
 * \ingroup intPlatform
 *
 * \desc            Loads attributes into the attribute subsystem by calling
 *                  the set and get methods on attributes read in from a text
 *                  file.
 *                                                                      \lb\lb
 *                  The expected file format for the database is as follows:
 *                                                                      \lb\lb
 *                  [key] [type] [value]
 *                                                                      \lb\lb
 *                  Where key is a dotted string, type is one of int, bool or
 *                  float, and value is the value to set. Space is the only
 *                  valid separator, but multiple spaces are allowed.
 *
 * \param[in]       fileName is the full path to a text file to load.
 *
 * \return          FM_OK if successful.
 *
 *****************************************************************************/
fm_status fmPlatformLoadAttributes(fm_text fileName)
{
    fm_status err;
    FILE *    fp     = fopen(fileName, "rt");
    int       lineNo = 0;
    char      line[1024];
    char *    key;
    char *    type;
    char *    value;
    long int  cvt;
    int       numAttr = 0;
    char *    context;
    uint      s1max;

    /* attribute values */
    fm_int    intValue;
    fm_bool   boolValue;
    fm_float  floatValue;
    fm_text   textValue;

    FM_LOG_ENTRY(FM_LOG_CAT_PLATFORM, "fileName=%s\n", fileName);

    if (!fp)
    {
        FM_LOG_DEBUG(FM_LOG_CAT_PLATFORM,
                     "Unable to open attribute database %s\n",
                     fileName);

        FM_LOG_EXIT(FM_LOG_CAT_PLATFORM, FM_ERR_INVALID_ARGUMENT);
    }


    while ( fgets(line, 1024, fp) )
    {
        lineNo++;

        context = NULL;
        s1max   = sizeof(line);
        key     = FM_STRTOK_S(line, &s1max, " \n", &context);
        type    = FM_STRTOK_S(NULL, &s1max, " \n", &context);
        value   = FM_STRTOK_S(NULL, &s1max, " \n", &context);

        if (key && type && value)
        {
            err = FM_OK;

            if (strcmp(type, "int") == 0)
            {
                cvt = strtol(value, NULL, 0);

                if (errno == ERANGE)
                {
                    FM_LOG_FATAL(FM_LOG_CAT_PLATFORM,
                                 "Conversion error on line %d\n", lineNo);

                    continue;
                }

                if (cvt > INT_MAX)
                {
                    FM_LOG_FATAL(FM_LOG_CAT_PLATFORM,
                                 "Overflow error on line %d\n", lineNo);

                    continue;
                }

                intValue = (fm_int) cvt;

                err = fmSetApiAttribute(key, FM_API_ATTR_INT, &intValue);

                numAttr++;
            }
            else if (strcmp(type, "bool") == 0)
            {
                if (strcmp(value, "true") == 0)
                {
                    boolValue = TRUE;
                }
                else if (strcmp(value, "false") == 0)
                {
                    boolValue = FALSE;
                }
                else
                {
                    FM_LOG_FATAL(FM_LOG_CAT_PLATFORM,
                                 "Conversion error on line %d\n", lineNo);
                }

                err = fmSetApiAttribute(key, FM_API_ATTR_BOOL, &boolValue);

                numAttr++;
            }
            else if (strcmp(type, "float") == 0)
            {
                floatValue = strtod(value, NULL);

                if (floatValue == HUGE_VAL)
                {
                    FM_LOG_FATAL(FM_LOG_CAT_PLATFORM,
                                 "Conversion error on line %d\n", lineNo);

                    continue;
                }

                err = fmSetApiAttribute(key, FM_API_ATTR_FLOAT, &floatValue);

                numAttr++;
            }
            else if (strcmp(type, "text") == 0)
            {
                textValue = value;
               
                err = fmSetApiAttribute(key, FM_API_ATTR_TEXT, textValue); 

                numAttr++;
            }

            if (err != FM_OK)
            {
                FM_LOG_ERROR(FM_LOG_CAT_PLATFORM,
                             "fmSetApiAttribute returned '%s' on line %d\n",
                             fmErrorMsg(err), lineNo);
            }
        }
        else
        {
            FM_LOG_DEBUG(FM_LOG_CAT_PLATFORM,
                         "Parse error on line %d, skipping\n", lineNo);

            continue;
        }

    }   /* end while ( fgets(line, 1024, fp) ) */

    fclose(fp);

    FM_LOG_DEBUG(FM_LOG_CAT_PLATFORM, "Loaded %d attributes from %s\n",
                 numAttr, fileName);

    FM_LOG_EXIT(FM_LOG_CAT_PLATFORM, FM_OK);

}   /* end fmPlatformLoadAttributes */
