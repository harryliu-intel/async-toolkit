/* vim:ts=4:sw=4:expandtab
 * (No tabs, indent level is 4 spaces)  */
/*****************************************************************************
 * File:            lldp_list.h
 * Creation Date:   September 1, 2010
 * Description:     Prototypes and structure definitions for Link-List.
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

#ifndef LLDP_LIST_H
#define LLDP_LIST_H

#include <stdint.h>
#include <pthread.h>
#include <stddef.h>

#ifdef  __cplusplus
extern "C" {
#endif

/** Double linked list head / element */
typedef struct lldp_list {
    /** previous element */
    struct lldp_list *prev;

    /** next element */
    struct lldp_list *next;
} lldp_list;

typedef lldp_list lldp_elem;

/** Initialize link-list */
void lldpListInit(lldp_list *list);

/** Push element to link-list */
void lldpListPush(lldp_list *list, lldp_elem *elem);

/** Pop element from link-list */
lldp_elem *lldpListPop(lldp_list *list);

/** Remove element from link-list */
lldp_elem *lldpListRemove(lldp_elem *elem);

/** Get link-list size */
size_t lldpListSize(const lldp_list *list);

/** Check if link-list is empty */
int lldpListIsEmpty(const lldp_list *list);

/** Static initialization of link-list */
#define LLDP_LIST_INITIALIZER(list_) { list_, list_ }

/** Cast container object from link-list object */
#define CONTAINER(pointer_, struct_, member_) \
    ((pointer_) ? ((struct_ *)(void *)((char *)(pointer_) - offsetof(struct_, member_))) : NULL)

/** Link-List iterator */
#define LLDP_LIST_FOR_EACH(iter_, struct_, member_, list_) \
    for (iter_ = CONTAINER((list_)->next, struct_, member_); \
         &(iter_)->member_ != (list_); \
         iter_ = CONTAINER((iter_)->member_.next, struct_, member_))

/** Link-List reverse iterator */
#define LLDP_LIST_FOR_EACH_REVERSE(iter_, struct_, member_, list_) \
    for (iter_ = CONTAINER((list_)->prev, struct_, member_); \
         &(iter_)->member_ != (list_); \
         iter_ = CONTAINER((iter_)->member_.prev, struct_, member_))

/** Link-List safe iterator (object can be deleted) */
#define LLDP_LIST_FOR_EACH_SAFE(iter_, next_, struct_, member_, list_) \
    for (iter_ = CONTAINER((list_)->next, struct_, member_); \
         (next_ = CONTAINER((iter_)->member_.next, struct_, member_), \
          &(iter_)->member_ != (list_)); \
         iter_ = next_)

#ifdef  __cplusplus
}
#endif

#endif /* LLDP_LIST_H */
