/* vim:ts=4:sw=4:expandtab
 * (No tabs, indent level is 4 spaces)  */
/*****************************************************************************
 * File:            lldp_hash.h
 * Creation Date:   September 1, 2010
 * Description:     Prototypes and structure definitions for Hash-List.
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

#ifndef LLDP_HASH_H
#define LLDP_HASH_H

#include <stdint.h>
#include <pthread.h>
#include <stddef.h>
#include <string.h>

#include <lldp_list.h>

#ifdef  __cplusplus
extern "C" {
#endif

/** Hash-list */
typedef struct lldp_hash {
    /** Number of bins in hash */
    size_t size;

    /** Array of link-lists (one for each bin) */
    struct lldp_list *lists;
} lldp_hash;

/** Calculate hash code from words */
uint32_t lldpHashWords(const uint32_t *data, size_t n_word, uint32_t basis);

/** Calculate hash code from two words */
uint32_t lldpHash2Words(const uint32_t *data, uint32_t basis);

/** Calculate hash code from bytes */
uint32_t lldpHashBytes(const uint8_t *data, size_t n_bytes, uint32_t basis);

/** Calculate hash code from string */
static inline uint32_t lldpHashString(const char *s, uint32_t basis)
{
    return lldpHashBytes((const uint8_t *)s, strlen(s), basis);
}

/** Calculate hash code from int (This is Bob Jenkins' integer hash from 
    http://burtleburtle.net/bob/hash/integer.html). */
static inline uint32_t lldpHashInt(uint32_t x, uint32_t basis)
{
    x -= x << 6;
    x ^= x >> 17;
    x -= x << 9;
    x ^= x << 4;
    x += basis;
    x -= x << 3;
    x ^= x << 10;
    x ^= x >> 15;
    return x;
}

/** Initialize Hash-List Object */
void lldpHashInit(lldp_hash *hash, int size);

/** Clean-up Hash-List Object */
void lldpHashDestroy(lldp_hash *hash);

/** Push element to hash-list */
void lldpHashInsert(lldp_hash *hash, lldp_elem *elem, uint32_t hash_code);

/** Pop element from hash-list */
lldp_elem *lldpHashRemove(lldp_elem *elem);

/* Hash-list iterator */
#define LLDP_HASH_FOR_EACH(iter_, struct_, member_, hash_, hash_code_) \
    for (iter_ = CONTAINER((hash_->lists[hash_code_ % hash_->size])->next, struct_, member_); \
         &(iter_)->member_ != (hash_->lists[hash_code_ % hash_->size]); \
         iter_ = CONTAINER((iter_)->member_.next, struct_, member_))

#endif /* LLDP_HASH_H */
