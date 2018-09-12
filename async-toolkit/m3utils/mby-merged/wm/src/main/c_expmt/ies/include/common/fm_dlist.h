/* vim:ts=4:sw=4:expandtab
 * (No tabs, indent level is 4 spaces)  */
/*****************************************************************************
 * File:            fm_dlist.h
 * Creation Date:   2006
 * Description:     Structures and functions for manipulating doubly-linked
 *                  lists
 *
 * INTEL CONFIDENTIAL
 * Copyright 2006 - 2011 Intel Corporation. All Rights Reserved. 
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

#ifndef __FM_FM_DLIST_H
#define __FM_FM_DLIST_H


/* Low-level double-linked-list macros */

#define FM_DLL_DEFINE_LIST(nodeStructType, head, tail) \
    struct nodeStructType *head;                       \
    struct nodeStructType *tail

#define FM_DLL_DEFINE_NODE(nodeStructType, next, prev) \
    struct nodeStructType *next;                       \
    struct nodeStructType *prev

#define FM_DLL_INIT_LIST(list, head, tail) \
    (list)->head = NULL;                     \
    (list)->tail = NULL

#define FM_DLL_INIT_NODE(node, next, prev) \
    (node)->next = NULL;                     \
    (node)->prev = NULL

#define FM_DLL_INSERT_FIRST(list, head, tail, newNode, next, prev) \
    {                                                              \
        (newNode)->prev = NULL;                                    \
        (newNode)->next = (list)->head;                            \
        (list)->head    = (newNode);                               \
        if ((newNode)->next == NULL)                               \
        {                                                          \
            (list)->tail = (newNode);                              \
        }                                                          \
        else                                                       \
        {                                                          \
            (newNode)->next->prev = (newNode);                     \
        }                                                          \
    }

#define FM_DLL_INSERT_LAST(list, head, tail, newNode, next, prev) \
    {                                                             \
        (newNode)->next = NULL;                                   \
        (newNode)->prev = (list)->tail;                           \
        (list)->tail    = (newNode);                              \
        if ((newNode)->prev == NULL)                              \
        {                                                         \
            (list)->head = (newNode);                             \
        }                                                         \
        else                                                      \
        {                                                         \
            (newNode)->prev->next = (newNode);                    \
        }                                                         \
    }

#define FM_DLL_INSERT_BEFORE(list, head, tail, node, next, prev, newNode) \
    {                                                                     \
        if ((node) == NULL)                                               \
        {                                                                 \
            FM_DLL_INSERT_LAST(list, head, tail, newNode, next, prev);    \
        }                                                                 \
        else                                                              \
        {                                                                 \
            (newNode)->prev = (node)->prev;                               \
            (newNode)->next = (node);                                     \
            if ((node)->prev == NULL)                                     \
            {                                                             \
                (list)->head = (newNode);                                 \
            }                                                             \
            else                                                          \
            {                                                             \
                (node)->prev->next = (newNode);                           \
            }                                                             \
            (node)->prev = (newNode);                                     \
        }                                                                 \
    }

#define FM_DLL_INSERT_AFTER(list, head, tail, node, next, prev, newNode) \
    {                                                                    \
        if ((node) == NULL)                                              \
        {                                                                \
            FM_DLL_INSERT_FIRST(list, head, tail, newNode, next, prev);  \
        }                                                                \
        else                                                             \
        {                                                                \
            (newNode)->prev = (node);                                    \
            (newNode)->next = (node)->next;                              \
            if ((node)->next == NULL)                                    \
            {                                                            \
                (list)->tail = (newNode);                                \
            }                                                            \
            else                                                         \
            {                                                            \
                (node)->next->prev = (newNode);                          \
            }                                                            \
            (node)->next = (newNode);                                    \
        }                                                                \
    }

#define FM_DLL_REMOVE_NODE(list, head, tail, node, next, prev) \
    {                                                          \
        if ((node)->prev == NULL)                              \
        {                                                      \
            (list)->head = (node)->next;                       \
        }                                                      \
        else                                                   \
        {                                                      \
            (node)->prev->next = (node)->next;                 \
        }                                                      \
        if ((node)->next == NULL)                              \
        {                                                      \
            (list)->tail = (node)->prev;                       \
        }                                                      \
        else                                                   \
        {                                                      \
            (node)->next->prev = (node)->prev;                 \
        }                                                      \
        (node)->prev = NULL;                                   \
        (node)->next = NULL;                                   \
    }

#define FM_DLL_UNLINK_NODES(list, head, tail, firstNode, lastNode, \
                            next, prev)                            \
    {                                                              \
        if ((firstNode) == NULL)                                   \
        {                                                          \
            (firstNode) = (list)->head;                            \
        }                                                          \
        if ((lastNode) == NULL)                                    \
        {                                                          \
            (lastNode) = (list)->tail;                             \
        }                                                          \
        if ((firstNode) != NULL)                                   \
        {                                                          \
            if ((firstNode)->prev == NULL)                         \
            {                                                      \
                (list)->head = (lastNode)->next;                   \
            }                                                      \
            else                                                   \
            {                                                      \
                (firstNode)->prev->next = (lastNode)->next;        \
            }                                                      \
                                                                   \
            if ((lastNode)->next == NULL)                          \
            {                                                      \
                (list)->tail = (firstNode)->prev;                  \
            }                                                      \
            else                                                   \
            {                                                      \
                (lastNode)->next->prev = (firstNode)->prev;        \
            }                                                      \
            (firstNode)->prev = NULL;                              \
            (lastNode)->next  = NULL;                              \
        }                                                          \
    }

#define FM_DLL_GET_FIRST(list, head)     (list)->head

#define FM_DLL_GET_LAST(list, tail)      (list)->tail

#define FM_DLL_GET_NEXT(node, next)      (node)->next

#define FM_DLL_GET_PREVIOUS(node, prev)  (node)->prev


/* doubly linked list node */
typedef struct _fm_dlist_node
{
    void *data;

    FM_DLL_DEFINE_NODE(_fm_dlist_node, next, prev);

} fm_dlist_node;

/* wrapper structure for list */
typedef struct
{
    FM_DLL_DEFINE_LIST(_fm_dlist_node, head, tail);

} fm_dlist;


void fmDListInit(fm_dlist *list);

fm_status fmDListInsertEnd(fm_dlist *list, void *data);
fm_status fmDListInsertBegin(fm_dlist *list, void *data);
fm_status fmDListRemoveEnd(fm_dlist *list, void **dataPtr);
fm_status fmDListRemoveBegin(fm_dlist *list, void **dataPtr);

fm_status fmDListInsertSorted(fm_dlist *list,
                              int (*key)(void *, void *),
                              void *data);

void *fmDListFind(fm_dlist *list, int (*find)(void *, void *), void *key);

void *fmDListRemove(fm_dlist *list, fm_dlist_node *node);
void fmDListFree(fm_dlist *list); /* CAUTION: does not free data! */
void fmDListFreeWithDestructor(fm_dlist *list, fmFreeFunc delfunc);
fm_uint fmDListSize(fm_dlist *list);


#endif /* __FM_FM_DLIST_H */
