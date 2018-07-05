/* vim:ts=4:sw=4:expandtab
 * (No tabs, indent level is 4 spaces)  */
/*****************************************************************************
 * File:            fm_attr.c
 * Creation Date:   September 10, 2007
 * Description:     Contains a subsystem for containing run-time accessible
 *                  properties.  The subsystem begins empty and is intended
 *                  to be set by other components (platform, ALOS).
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

/* A union to hold the attribute values */
typedef union _fm_apiAttrValue
{
    fm_int   intValue;
    fm_bool  boolValue;
    fm_float floatValue;
    fm_char  textValue[FM_API_ATTR_TEXT_MAX_LENGTH];

} fm_apiAttrValue;


typedef union _fm_apiAttrKey
{
    unsigned char digest[16];
    fm_uint64     u64Key[2];
} fm_apiAttrKey;


/* Holds an attribute entry */
typedef struct _fm_apiAttr
{
    /***************************************************
     * The dotted key name.  A key name is hierarchical
     * in the form a.b.c
     **************************************************/
    fm_text         key;
    fm_apiAttrKey   attrKey;

    /* The type of the value held in this key */
    fm_apiAttrType  type;

    fm_apiAttrValue values;

} fm_apiAttr;




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


static fm_int CompareHashKeys(const void *key1, const void *key2)
{
    fm_apiAttrKey *key1Ptr;
    fm_apiAttrKey *key2Ptr;
    fm_uint64  key1a;
    fm_uint64  key1b;
    fm_uint64  key2a;
    fm_uint64  key2b;

    key1Ptr = (fm_apiAttrKey *) key1;
    key2Ptr = (fm_apiAttrKey *) key2;

    key1a = key1Ptr->u64Key[0];
    key1b = key1Ptr->u64Key[1];
    key2a = key2Ptr->u64Key[0];
    key2b = key2Ptr->u64Key[1];

    if (key1a < key2a)
    {
        return -1;
    }
    else if (key1a > key2a)
    {
        return 1;
    }
    else
    {
        if (key1b < key2b)
        {
            return -1;
        }
        else if (key1b > key2b)
        {
            return 1;
        }
    }

    return 0;

}   /* end CompareHashKeys */




/*****************************************************************************
 * Public Functions
 *****************************************************************************/

/*****************************************************************************/
/** fmInitializeApiAttributes
 * \ingroup intApi
 *
 * \desc            Initializes the API attribute subsystem These attributes
 *                  are used to provide run-time and boot-time access to
 *                  configuration parameters at the API level.
 *
 * \param           None.
 *
 * \return          FM_OK always.
 *
 *****************************************************************************/
fm_status fmInitializeApiAttributes(void)
{
    fm_status err = FM_OK;

    FM_LOG_ENTRY_NOARGS(FM_LOG_CAT_ATTR);

    fmCustomTreeInit(&fmRootAlos->attributeTree, CompareHashKeys);
    err = fmCreateLock("API Attribute Lock", 
                       &fmRootAlos->attributeLock);

    FM_LOG_EXIT(FM_LOG_CAT_ATTR, err);

}   /* end fmInitializeApiAttributes */




/*****************************************************************************/
/** fmSetApiAttribute
 * \ingroup api
 *
 * \desc            Adds the given attribute to the database via the given key.
 *
 * \param[in]       key is the dotted string key (see ''API Attributes'').
 *
 * \param[in]       attrType is one of the valid type constants.
 *
 * \param[in]       value points to caller allocated storage where the specific
 *                  value is stored.  It's type is assumed to match attrType.
 *
 * \return          FM_OK on success.
 *
 *****************************************************************************/
fm_status fmSetApiAttribute(fm_text        key,
                            fm_apiAttrType attrType,
                            void *         value)
{
    fm_apiAttr *   attrEntry = NULL;
    fm_status      err       = FM_OK;
    fm_MD5Context  md5Context;
    fm_apiAttrKey  attrKey;

    FM_LOG_ENTRY_API(FM_LOG_CAT_ATTR,
                     "key=%s type=%d value=%p\n",
                     key,
                     attrType,
                     value);

    fmMD5Init(&md5Context);
    fmMD5Update( &md5Context, (unsigned char const *) key, strlen(key) );
    fmMD5Final(&md5Context, attrKey.digest);

    err = fmCaptureLock(&fmRootAlos->attributeLock, FM_WAIT_FOREVER);
    if (err != FM_OK)
    {
        FM_LOG_EXIT_API(FM_LOG_CAT_ATTR, err);
    }

    err = fmCustomTreeFind(&fmRootAlos->attributeTree,
                           &attrKey,
                           (void **) &attrEntry);

    if (err == FM_ERR_NOT_FOUND)
    {
        attrEntry = (fm_apiAttr *) fmAlloc( sizeof(fm_apiAttr) );

        if (attrEntry == NULL)
        {
            fmReleaseLock(&fmRootAlos->attributeLock);
            FM_LOG_EXIT_API(FM_LOG_CAT_ATTR, FM_ERR_NO_MEM);
        }

        FM_CLEAR(*attrEntry);

        /* allocate and store the key */
        attrEntry->key = fmStringDuplicate(key);

        if (attrEntry->key == NULL)
        {
            fmFree(attrEntry);
            fmReleaseLock(&fmRootAlos->attributeLock);
            FM_LOG_EXIT_API(FM_LOG_CAT_ATTR, FM_ERR_NO_MEM);
        }

        attrEntry->attrKey = attrKey;

        err = fmCustomTreeInsert(&fmRootAlos->attributeTree,
                                 (void *) &attrEntry->attrKey,
                                 (void *) attrEntry);

        if (err != FM_OK)
        {
            fmFree(attrEntry);
            fmReleaseLock(&fmRootAlos->attributeLock);
            FM_LOG_EXIT_API(FM_LOG_CAT_ATTR, err);
        }
    }
    else if (err != FM_OK)
    {
        fmReleaseLock(&fmRootAlos->attributeLock);
        FM_LOG_EXIT_API(FM_LOG_CAT_ATTR, err);
    }
    else
    {
        if (strcmp(attrEntry->key, key) != 0)
        {
            FM_LOG_ERROR(FM_LOG_CAT_ATTR,
                         "Different strings have identical MD5 hashes! "
                         "attrEntry->key=%s, key=%s\n",
                         attrEntry->key,
                         key);
            fmReleaseLock(&fmRootAlos->attributeLock);
            FM_LOG_EXIT_API(FM_LOG_CAT_ATTR, FM_FAIL);
        }
    }

    /***************************************************
     * Note that even if we find the entry, we let the
     * user override everything.
     **************************************************/

    attrEntry->type = attrType;

    switch (attrType)
    {
        case FM_API_ATTR_INT:
            attrEntry->values.intValue = *( (fm_int *) value );
            break;

        case FM_API_ATTR_BOOL:
            attrEntry->values.boolValue = *( (fm_bool *) value );
            break;

        case FM_API_ATTR_FLOAT:
            attrEntry->values.floatValue = *( (fm_float *) value );
            break;

        case FM_API_ATTR_TEXT:
            fmStringCopy(attrEntry->values.textValue, (fm_text) value,
                         FM_API_ATTR_TEXT_MAX_LENGTH);
            break;

        default:
            FM_LOG_FATAL(FM_LOG_CAT_ATTR,
                         "Unknown attribute type %d for set of attribute %s\n",
                         attrType, key);

    }   /* end switch (attrType) */

    err = fmReleaseLock(&fmRootAlos->attributeLock);
    FM_LOG_EXIT_API(FM_LOG_CAT_ATTR, err);

}   /* end fmSetApiAttribute */




/*****************************************************************************/
/** fmGetApiAttribute
 * \ingroup api
 *
 * \desc            Retrieves the value of the given key from the database.
 *                  Note that for text valued keys, the returned pointer
 *                  points to the internally stored value of the key,
 *                  so the caller must make sure to not modify this string.
 *
 * \param[in]       key is the dotted string key (see ''API Attributes'').
 *
 * \param[in]       attrType is the expected type of the key. See
 *                  ''fm_apiAttrType''.
 *
 * \param[in]       value points to caller allocated storage where the key's
 *                  value will be stored.
 *
 * \return          FM_OK on success.
 * \return          FM_ERR_KEY_NOT_FOUND if the key is invalid.
 *
 *****************************************************************************/
fm_status fmGetApiAttribute(fm_text        key,
                            fm_apiAttrType attrType,
                            void *         value)
{
    fm_apiAttr *   attrEntry = NULL;
    fm_status      err       = FM_OK;
    fm_MD5Context  md5Context;
    fm_apiAttrKey  attrKey;

    FM_LOG_ENTRY_VERBOSE(FM_LOG_CAT_ATTR,
                         "key=%s value=%p\n",
                         key,
                         value);

    fmMD5Init(&md5Context);
    fmMD5Update( &md5Context, (unsigned char const *) key, strlen(key) );
    fmMD5Final(&md5Context, attrKey.digest);

    err = fmCaptureLock(&fmRootAlos->attributeLock, FM_WAIT_FOREVER);
    if (err != FM_OK)
    {
        FM_LOG_EXIT_VERBOSE(FM_LOG_CAT_ATTR, err);
    }

    err = fmCustomTreeFind(&fmRootAlos->attributeTree,
                           (void *) &attrKey,
                           (void **) &attrEntry);

    if (err == FM_ERR_NOT_FOUND)
    {
        fmReleaseLock(&fmRootAlos->attributeLock);
        FM_LOG_EXIT_VERBOSE(FM_LOG_CAT_ATTR, FM_ERR_KEY_NOT_FOUND);
    }

    if (attrType != attrEntry->type)
    {
        fmReleaseLock(&fmRootAlos->attributeLock);
        FM_LOG_EXIT_VERBOSE(FM_LOG_CAT_ATTR, FM_ERR_INVALID_ARGUMENT);
    }

    switch (attrEntry->type)
    {
        case FM_API_ATTR_INT:
            *( (fm_int *) value ) = attrEntry->values.intValue;
            break;

        case FM_API_ATTR_BOOL:
            *( (fm_bool *) value ) = attrEntry->values.boolValue;
            break;

        case FM_API_ATTR_FLOAT:
            *( (fm_float *) value ) = attrEntry->values.floatValue;
            break;

        case FM_API_ATTR_TEXT:
            *( (fm_text *) value ) = attrEntry->values.textValue;
            break;

        default:
            FM_LOG_FATAL(FM_LOG_CAT_ATTR,
                         "Unknown attribute type %d for get of attribute %s\n",
                         attrEntry->type, key);

    }   /* end switch (attrEntry->type) */

    err = fmReleaseLock(&fmRootAlos->attributeLock);
    FM_LOG_EXIT_VERBOSE(FM_LOG_CAT_ATTR, err);

}   /* end fmGetApiAttribute */




/*****************************************************************************/
/** fmGetIntApiAttribute
 * \ingroup intApi
 *
 * \desc            Retrieves the value of the given integer key from the database.
 *                  If the key has a different type then an error is returned.
 *
 * \param[in]       key is the dotted string key.
 *
 * \param[in]       defaultValue is the value to return on an error.
 *
 * \return          The integer value (or the default if an error occurred).
 *
 *****************************************************************************/
fm_int fmGetIntApiAttribute(fm_text key, fm_int defaultValue)
{
    fm_status err;
    fm_int    value;

    err = fmGetApiAttribute(key, FM_API_ATTR_INT, &value);

    if (err != FM_OK)
    {
        return defaultValue;
    }

    return value;

}   /* end fmGetIntApiAttribute */




/*****************************************************************************/
/** fmGetBoolApiAttribute
 * \ingroup intApi
 *
 * \desc            Retrieves the value of the given boolean key from the database.
 *                  If the key has a different type then an error is returned.
 *
 * \param[in]       key is the dotted string key.
 *
 * \param[in]       defaultValue is the value to return on an error.
 *
 * \return          The boolean value (or the default if an error occurred).
 *
 *****************************************************************************/
fm_bool fmGetBoolApiAttribute(fm_text key, fm_bool defaultValue)
{
    fm_status err;
    fm_bool   value;

    err = fmGetApiAttribute(key, FM_API_ATTR_BOOL, &value);

    if (err != FM_OK)
    {
        return defaultValue;
    }

    return value;

}   /* end fmGetBoolApiAttribute */




/*****************************************************************************/
/** fmGetFloatApiAttribute
 * \ingroup intApi
 *
 * \desc            Retrieves the value of the given float key from the database.
 *                  If the key has a different type then an error is returned.
 *
 * \param[in]       key is the dotted string key.
 *
 * \param[in]       defaultValue is the value to return on an error.
 *
 * \return          The float value (or the default if an error occurred).
 *
 *****************************************************************************/
fm_float fmGetFloatApiAttribute(fm_text key, fm_float defaultValue)
{
    fm_status err;
    fm_float  value;

    err = fmGetApiAttribute(key, FM_API_ATTR_FLOAT, &value);

    if (err != FM_OK)
    {
        return defaultValue;
    }

    return value;

}   /* end fmGetFloatApiAttribute */




/*****************************************************************************/
/** fmGetTextApiAttribute
 * \ingroup intApi
 *
 * \desc            Retrieves the value of the given text key from the database.
 *                  If the key has a different type then an error is returned.
 *
 * \param[in]       key is the dotted string key.
 *
 * \param[in]       defaultValue is the value to return on an error.
 *
 * \return          The text value (or the default if an error occurred).
 *
 *****************************************************************************/
fm_text fmGetTextApiAttribute(fm_text key, fm_text defaultValue)
{
    fm_status err;
    fm_text   value;

    err = fmGetApiAttribute(key, FM_API_ATTR_TEXT, &value);

    if (err != FM_OK)
    {
        return defaultValue;
    }

    return value;

}   /* end fmGetTextApiAttribute */




/*****************************************************************************/
/** fmDbgDumpApiAttributes
 * \ingroup diagMisc
 *
 * \desc            Dump all API attributes.
 *
 * \param           None.
 *
 * \return          None.
 *
 *****************************************************************************/
void fmDbgDumpApiAttributes(void)
{
    fm_status err;
    fm_customTreeIterator iter;
    fm_apiAttr *   attr;
    fm_apiAttrKey  key;

    fmCustomTreeIterInit(&iter, &fmRootAlos->attributeTree);

    while (1)
    {
        err = fmCustomTreeIterNext(&iter,
                                   (void **) key.u64Key,
                                   (void **) &attr);

        if (err != FM_OK)
        {
            break;
        }

        FM_LOG_PRINT("key=%s ", attr->key);

        switch (attr->type)
        {
            case FM_API_ATTR_INT:
                FM_LOG_PRINT("value=%d\n", attr->values.intValue);
                break;

            case FM_API_ATTR_BOOL:
                FM_LOG_PRINT("value=%d\n", attr->values.boolValue);
                break;

            case FM_API_ATTR_FLOAT:
                FM_LOG_PRINT("value=%f\n", attr->values.floatValue);
                break;

            case FM_API_ATTR_TEXT:
                FM_LOG_PRINT("value=%s\n", attr->values.textValue);
                break;

            default:
                break;

        }   /* end switch (attr->type) */
    }

}   /* end fmDbgDumpApiAttributes */
