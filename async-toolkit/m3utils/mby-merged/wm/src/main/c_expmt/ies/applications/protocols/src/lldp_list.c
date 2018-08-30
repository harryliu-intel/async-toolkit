/* vim:ts=4:sw=4:expandtab
 * (No tabs, indent level is 4 spaces)  */
/*****************************************************************************
 * File:            lldp_list.c
 * Creation Date:   September 1, 2010
 * Description:     Implementation of LLDP Link-List.
 *
 * INTEL CONFIDENTIAL
 * Copyright 2010 - 2011 Intel Corporation. All Rights Reserved. 
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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <lldp_list.h>

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
/** lldpListInit
 * \ingroup lldp
 *
 * \desc            Initializes link-list object.
 *
 * \param[in]       list is a pointer to the link-list object to be
 *                  initialized.
 *
 *****************************************************************************/
void lldpListInit(lldp_list *list)
{
    list->next = list->prev = list;
}

/*****************************************************************************/
/** lldpListPush
 * \ingroup lldp
 *
 * \desc            Pushes object to the link-list.
 *
 * \param[in]       list is a pointer to the link-list object.
 *
 * \param[in]       elem is a pointer to the link-list object to be pushed.
 *
 *****************************************************************************/
void lldpListPush(lldp_list *list, lldp_elem *elem)
{
    elem->prev = list->prev;
    elem->next = list;
    list->prev->next = elem;
    list->prev = elem;
}

/*****************************************************************************/
/** lldpListRemove
 * \ingroup lldp
 *
 * \desc            Removes object from the link-list.
 *
 * \param[in]       elem is a pointer to the link-list object to be removed.
 *
 * \return          link-list element.
 *
 *****************************************************************************/
lldp_elem *lldpListRemove(lldp_elem *elem)
{
    elem->prev->next = elem->next;
    elem->next->prev = elem->prev;
    return elem->next;
}

/*****************************************************************************/
/** lldpListPop
 * \ingroup lldp
 *
 * \desc            Pops and Removes object from the link-list.
 *
 * \param[in]       list is a pointer to the link-list object.
 *
 * \return          link-list element.
 *
 *****************************************************************************/
lldp_elem *lldpListPop(lldp_list *list)
{
    lldp_list *elem = list->next;
    if (elem == list) return NULL;
    lldpListRemove(elem);
    return elem;
}

/*****************************************************************************/
/** lldpListSize
 * \ingroup lldp
 *
 * \desc            Returns number of elements in link-list.
 *
 * \param[in]       list is a pointer to the link-list object.
 *
 * \return          link-list element.
 *
 *****************************************************************************/
size_t lldpListSize(const lldp_list *list)
{
    const lldp_list *elem;
    size_t size = 0;
    for (elem = list->next; elem != list; elem = elem->next)
        size++;
    return size;
}

/*****************************************************************************/
/** lldpListIsEmpty
 * \ingroup lldp
 *
 * \desc            Checks if link-list is empty.
 *
 * \param[in]       list is a pointer to the link-list object.
 *
 * \return          true if list is empty.
 *
 * \return          true if list is not-empty.
 *
 *****************************************************************************/
int lldpListIsEmpty(const lldp_list * list)
{
    return list->next == list;
}
