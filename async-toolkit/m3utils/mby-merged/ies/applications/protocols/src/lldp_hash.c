/* vim:ts=4:sw=4:expandtab
 * (No tabs, indent level is 4 spaces)  */
/*****************************************************************************
 * File:            lldp_hash.c
 * Creation Date:   September 1, 2010
 * Description:     Implementation of LLDP Hash-List.
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

#include <stdlib.h>

#include <lldp_hash.h>

/*****************************************************************************
 * Macros, Constants & Types
 *****************************************************************************/

/* This is the public domain lookup3 hash by Bob Jenkins from
 * http://burtleburtle.net/bob/c/lookup3.c, modified for style. */

#define HASH_ROT(x, k) (((x) << (k)) | ((x) >> (32 - (k))))

#define HASH_MIX(a, b, c)                       \
    do {                                        \
      a -= c; a ^= HASH_ROT(c,  4); c += b;     \
      b -= a; b ^= HASH_ROT(a,  6); a += c;     \
      c -= b; c ^= HASH_ROT(b,  8); b += a;     \
      a -= c; a ^= HASH_ROT(c, 16); c += b;     \
      b -= a; b ^= HASH_ROT(a, 19); a += c;     \
      c -= b; c ^= HASH_ROT(b,  4); b += a;     \
    } while (0)

#define HASH_FINAL(a, b, c)                     \
    do {                                        \
      c ^= b; c -= HASH_ROT(b, 14);             \
      a ^= c; a -= HASH_ROT(c, 11);             \
      b ^= a; b -= HASH_ROT(a, 25);             \
      c ^= b; c -= HASH_ROT(b, 16);             \
      a ^= c; a -= HASH_ROT(c,  4);             \
      b ^= a; b -= HASH_ROT(a, 14);             \
      c ^= b; c -= HASH_ROT(b, 24);             \
    } while (0)


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
/** lldpHashWords
 * \ingroup lldp
 *
 * \desc            Calculate hash code from words.
 *
 * \param[in]       p is a pointer to the words buffer to be hashed.
 *
 * \param[in]       n is the size of the words buffer.
 *
 * \param[in]       basis to the base code for the hash calculation.
 *
 * \return          hash code.
 *
 *****************************************************************************/
uint32_t lldpHashWords(const uint32_t *p, size_t n, uint32_t basis)
{
    uint32_t a, b, c;

    a = b = c = 0xdeadbeef + (((uint32_t) n) << 2) + basis;

    while (n > 3) {
        a += p[0];
        b += p[1];
        c += p[2];
        HASH_MIX(a, b, c);
        n -= 3;
        p += 3;
    }

    switch (n) {
    case 3:
        c += p[2];
        /* fall through */
    case 2:
        b += p[1];
        /* fall through */
    case 1:
        a += p[0];
        HASH_FINAL(a, b, c);
        /* fall through */
    case 0:
        break;
    }
    return c;
}

/*****************************************************************************/
/** lldpHash2Words
 * \ingroup lldp
 *
 * \desc            Calculate hash code from two words.
 *
 * \param[in]       p is a pointer to the two words buffer to be hashed.
 *
 * \param[in]       basis is the base code for the hash calculation.
 *
 * \return          hash code.
 *
 *****************************************************************************/
uint32_t lldpHash2Words(const uint32_t *p, uint32_t basis)
{
    uint32_t a, b, c;

    a = b = c = 0xdeadbeef + (2 << 2) + basis;
    b += p[1];
    a += p[0];
    HASH_FINAL(a, b, c);

    return c;
}

/*****************************************************************************/
/** lldpHashBytes
 * \ingroup lldp
 *
 * \desc            Calculate hash code from bytes.
 *
 * \param[in]       p is a pointer to the bytes buffer to be hashed.
 *
 * \param[in]       n is the size of the bytes buffer.
 *
 * \param[in]       basis is the base code for the hash calculation.
 *
 * \return          hash code.
 *
 *****************************************************************************/
uint32_t lldpHashBytes(const uint8_t *p, size_t n, uint32_t basis)
{
    uint32_t a, b, c;
    uint32_t tmp[3];

    a = b = c = 0xdeadbeef + n + basis;

    while (n >= sizeof tmp) {
        memcpy(tmp, p, sizeof tmp);
        a += tmp[0];
        b += tmp[1];
        c += tmp[2];
        HASH_MIX(a, b, c);
        n -= sizeof tmp;
        p += sizeof tmp;
    }

    if (n) {
        tmp[0] = tmp[1] = tmp[2] = 0;
        memcpy(tmp, p, n);
        a += tmp[0];
        b += tmp[1];
        c += tmp[2];
        HASH_FINAL(a, b, c);
    }

    return c;
}

/*****************************************************************************/
/** lldpHashInit
 * \ingroup lldp
 *
 * \desc            Initializes hash-list object.
 *
 * \param[in]       hash is a pointer to the hash-list object to be
 *                  initialized.
 *
 * \param[in]       size the number of bins top be created.
 *
 *****************************************************************************/
void lldpHashInit(lldp_hash *hash, int size)
{
    int i;

    hash->size  = size;
    hash->lists = calloc(size, sizeof(lldp_list));

    for (i=0; i<size; i++)
        lldpListInit(&hash->lists[i]);
}

/*****************************************************************************/
/** lldpHashDestroy
 * \ingroup lldp
 *
 * \desc            Cleans-up hash-list object.
 *
 * \param[in]       hash is a pointer to the hash-list object to be
 *                  cleared.
 *
 *****************************************************************************/
void lldpHashDestroy(lldp_hash *hash)
{
    free(hash->lists);
    hash->size = 0;
}

/*****************************************************************************/
/** lldpHashInsert
 * \ingroup lldp
 *
 * \desc            Inserts object to hash-list.
 *
 * \param[in]       hash is a pointer to the hash-list.
 *
 * \param[in]       elem is a pointer to the object that to be instered into
 *                  the hash-list.
 *
 * \param[in]       hash_code is the hash-code of the inserted object.
 *
 *****************************************************************************/
void lldpHashInsert(lldp_hash *hash, lldp_elem *elem, uint32_t hash_code)
{
    int i = hash_code % hash->size;
    lldpListPush(&hash->lists[i], elem);
}

/*****************************************************************************/
/** lldpHashRemove
 * \ingroup lldp
 *
 * \desc            Removes object to hash-list.
 *
 * \param[in]       elem is a pointer to the object that to be removed from
 *                  the hash-list.
 *
 *****************************************************************************/
lldp_elem *lldpHashRemove(lldp_elem *elem)
{
    return lldpListRemove(elem);
}

