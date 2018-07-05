/* vim:ts=4:sw=4:expandtab
 * (No tabs, indent level is 4 spaces)  */
/*****************************************************************************
 * File:            fm_dlist.c
 * Creation Date:   2005
 * Description:     Functions to work with doubly-linked lists
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


void fmDListInit(fm_dlist *list)
{
    FM_DLL_INIT_LIST(list, head, tail);

}   /* fmDListInit */




fm_status fmDListInsertEnd(fm_dlist *list, void *data)
{
    fm_dlist_node *nnode = (fm_dlist_node *) fmAlloc( sizeof(fm_dlist_node) );

    if (!nnode)
    {
        return FM_ERR_NO_MEM;
    }

    nnode->data = data;

    FM_DLL_INSERT_LAST(list, head, tail, nnode, next, prev);

    return FM_OK;

}   /* end fmDListInsertEnd */




fm_status fmDListInsertBegin(fm_dlist *list, void *data)
{
    fm_dlist_node *nnode = (fm_dlist_node *) fmAlloc( sizeof(fm_dlist_node) );

    if (!nnode)
    {
        return FM_ERR_NO_MEM;
    }

    nnode->data = data;

    FM_DLL_INSERT_FIRST(list, head, tail, nnode, next, prev);

    return FM_OK;

}   /* end fmDListInsertBegin */




fm_status fmDListRemoveEnd(fm_dlist *list, void **dataPtr)
{
    fm_dlist_node *nnode;
    void *         data;

    nnode = FM_DLL_GET_LAST(list, tail);
    
    if (!nnode)
    {
        return FM_ERR_NO_MORE;
    }

    data = fmDListRemove(list, nnode);

    *dataPtr = data;

    return FM_OK;

}   /* end fmDListRemoveEnd */




fm_status fmDListRemoveBegin(fm_dlist *list, void **dataPtr)
{
    fm_dlist_node *nnode;
    void *         data;

    nnode = FM_DLL_GET_FIRST(list, head);
    
    if (!nnode)
    {
        return FM_ERR_NO_MORE;
    }

    data = fmDListRemove(list, nnode);

    *dataPtr = data;

    return FM_OK;

}   /* end fmDListRemoveBegin */




/**
 * assumes the list is already sorted, and the key function is defined as:
 *
 * key(d1, d2) >  0 => d1 > d2
 * key(d1, d2) == 0 => d1 == d2
 * key(d1, d2) <  0 => d1 < d2
 *
 * the new node is inserted prior to the node T such that T_data > data
 */
int fmDListInsertSorted(fm_dlist *list,
                        int (*key)(void *, void *),
                        void *data)
{
    fm_dlist_node *p, *nnode;

    nnode = (fm_dlist_node *) fmAlloc( sizeof(fm_dlist_node) );

    if (!nnode)
    {
        return FM_ERR_NO_MEM;
    }

    nnode->data = data;

    for (p = list->head ;
         p && (key(p->data, nnode->data) <= 0) ;
         p = p->next)
    {
        ;
    }

    /* p is now at a node that is "greater" than the new one */
    FM_DLL_INSERT_BEFORE(list, head, tail, p, next, prev, nnode);

    return FM_OK;

}   /* end fmDListInsertSorted */




void *fmDListFind(fm_dlist *list, int (*find)(void *, void *), void *key)
{
    fm_dlist_node *node;

    for (node = list->head ; node ; node++)
    {
        if ( find(node->data, key) )
        {
            return node->data;
        }
    }

    return NULL;

}   /* end fmDListFind */




/* assumes node is already in the list */
void *fmDListRemove(fm_dlist *list, fm_dlist_node *node)
{
    void *data;

    if (node)
    {
        FM_DLL_REMOVE_NODE(list, head, tail, node, next, prev);

        data = node->data;
        fmFree(node);

        return data;
    }

    return NULL;

}   /* end fmDListRemove */




/*****************************************************************************/
/** fmDListFree
 * \ingroup intList
 *
 * \desc            Frees the space used by a dlist itself, but not the
 *                  data it points to.
 *
 * \note            This function is equivalent to
 *                  fmDListFreeWithDestructor(list, NULL).
 *                  It should only be used if the data you are pointing to
 *                  does not need to be freed.
 *
 * \param[in]       list is the dlist on which to operate.
 *
 * \return          None
 *
 *****************************************************************************/
void fmDListFree(fm_dlist *list)
{
    fm_dlist_node *p;

    if (list->head)
    {
        do
        {
            p = list->head->next;
            fmFree(list->head);
            list->head = p;
        }
        while (p);
    }

}   /* end fmDListFree */




/*****************************************************************************/
/** fmDListFreeWithDestructor
 * \ingroup intList
 *
 * \desc            Frees all space used by a dlist.
 *
 * \param[in]       list is the dlist on which to operate.
 *
 * \param[in]       delfunc is a function which is called once on each
 *                  "data" pointer in the list, if it is not NULL.
 *
 * \return          None
 *
 *****************************************************************************/
void fmDListFreeWithDestructor(fm_dlist *list, fmFreeFunc delfunc)
{
    fm_dlist_node *p;

    if (list->head)
    {
        do
        {
            if (delfunc != NULL)
            {
                delfunc(list->head->data);
            }

            p = list->head->next;
            fmFree(list->head);
            list->head = p;
        }
        while (p);
    }

}   /* end fmDListFreeWithDestructor */


/*****************************************************************************/
/** fmDListSize
 * \ingroup intList
 *
 * \desc            Returns the number of items in the list.
 *
 * \param[in]       list is the dlist on which to operate.
 *
 * \return          the number of items in the list.
 *
 *****************************************************************************/
fm_uint fmDListSize(fm_dlist *list)
{
    fm_uint size = 0;
    fm_dlist_node *p;

    for (p = list->head ; p != NULL ; p = p->next)
    {
        size++;
    }

    return size;

}   /* end fmDListSize */
