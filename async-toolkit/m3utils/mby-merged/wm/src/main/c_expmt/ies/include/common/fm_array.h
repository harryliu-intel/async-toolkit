/* vim:ts=4:sw=4:expandtab
 * (No tabs, indent level is 4 spaces) */
/*****************************************************************************
 * File:            fm_array.h
 * Creation Date:   April 12, 2010
 * Description:     File containing declarations for miscellaneous utility
 *                  macros and functions
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
#ifndef __FM_FM_ARRAY_H
#define __FM_FM_ARRAY_H

/***************************************************
 * Helper macros and structs for declaring large 
 * local arrays.
 **************************************************/

/**
 * fm_cleanupListEntry 
 *  
 * Defines an entry in the cleanup list for this function. Used by the 
 * FM_ALLOC_TEMP_ARRAY and FM_FREE_TEMP_ARRAY macros. 
 */
typedef struct _fm_cleanupListEntry
{
    void *memory;               /* to be freed with free() */
    struct _fm_cleanupListEntry *next;

} fm_cleanupListEntry;


/**
 * Allocates a temporary array and adds it to the cleanup list for the 
 * current function. 
 *  
 * \note            The calling function should define a local variable
 *                  with a statement of the form
 *                      <i>fm_cleanupListEntry* cleanupList = NULL;</i>
 *  
 * \param[out]      lvalue is the pointer variable that receives the 
 *                  address of the temporary array.
 *  
 * \param[in]       type is the type of each element of the array. 
 *  
 * \param[in]       num is the number of elements to allocate. 
 */
#define FM_ALLOC_TEMP_ARRAY(lvalue, type, num)                  \
    {                                                           \
        fm_cleanupListEntry *cleanupEntry =                     \
            (fm_cleanupListEntry *) malloc(sizeof(fm_cleanupListEntry));                \
        if (cleanupEntry == NULL)                               \
        {                                                       \
            err = FM_ERR_NO_MEM;                                \
            goto ABORT;                                         \
        }                                                       \
        (lvalue) = (type *) malloc( (num) * sizeof(type) );     \
        if ((lvalue) == NULL)                                   \
        {                                                       \
            free(cleanupEntry);                                 \
            err = FM_ERR_NO_MEM;                                \
            goto ABORT;                                         \
        }                                                       \
        cleanupEntry->memory = (lvalue);                        \
        cleanupEntry->next = cleanupList;                       \
        cleanupList = cleanupEntry;                             \
    }                                                           \
    memset( (lvalue), 0, (num) * sizeof(type) )


/**
 * Frees the temporary arrays on the cleanup list for this function. 
 *  
 * Should be called on exit from any function that uses the
 * FM_ALLOC_TEMP_ARRAY macro. 
 */
#define FM_FREE_TEMP_ARRAYS()                \
    {                                        \
        fm_cleanupListEntry *tmp;            \
        while (cleanupList != NULL)          \
        {                                    \
            tmp = cleanupList;               \
            cleanupList = tmp->next;         \
            free(tmp->memory);               \
            free(tmp);                       \
        }                                    \
    }

#endif /* __FM_FM_ARRAY_H */
