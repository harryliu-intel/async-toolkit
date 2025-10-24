/* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. */
/* SPDX-License-Identifier: Apache-2.0 */

/*
  fm_alos_debughash.c

*/

/* old copyright notice below -- code has been EXTENSIVELY modified
   from the original */

/*************************************************************************
 *
 *  hash.c
 *
 *  hash tables
 *
 *  Copyright (c) 1999 California Institute of Technology
 *  All rights reserved.
 *  Department of Computer Science
 *  Pasadena, CA 91125.
 *
 *  Author: Mika Nystrom <mika@cs.caltech.edu>
 *
 *  Permission to use, copy, modify, and distribute this software
 *  and its documentation for any purpose and without fee is hereby
 *  granted, provided that the above copyright notice appear in all
 *  copies. The California Institute of Technology makes no representations
 *  about the suitability of this software for any purpose. It is
 *  provided "as is" without express or implied warranty. Export of this
 *  software outside of the United States of America may require an
 *  export license.
 *
 *  $Id: hash.c,v 1.5 1999/12/16 21:46:47 mika Exp $
 *
 *************************************************************************/

#include "fm_alos_debughash.h" /* should be first #include */

#include <stdlib.h>
#include <string.h>

/* if you wish to run a test program, simply compile this file with
   cc -I<path to include file> -D DEBUGHASH_TEST_MAIN fm_alos_debughash.c
*/

#include <assert.h>
#include <stdio.h>

typedef struct hashtable_bucket_t {
    void *data;
    struct hashtable_bucket_t *next;
} hashtable_bucket_t;

/* hash table descriptor structure */
struct hashtable_t {
    int nbuckets;                            /* how many buckets          */
    int bucketmask;                          /* mask for hash value       */
    int nentries;                            /* how many entries in table */
    hashtable_bucket_t **buckets;            /* array of root buckets     */
    unsigned  (*hash_func  )(const void *);
    unsigned  (*comp_func  )(const void *, const void *);
    alloc_func_t alloc;
    dealloc_func_t dealloc;
};

/* private prototypes -- needed? */
void hashtable_destroy(hashtable_t *hhp);

/* ---------------------------------------------------------------------- */

int
hashtable_size(hashtable_t *h)
{
    return h->nentries;
}

unsigned int
hashtable_hash_string (const char *name)
{
    /* hashing function for names.
       should be fast and have a reasonable distribution */

    const char *cp=name;
    unsigned int hashv=13;

    while(*cp) {
        hashv+=*cp+(*cp<<8)+(*cp<<16)+(*cp<<24)+3;
        hashv*=0xc0edbabe;
        ++cp;
    }
    return hashv;
}

/* ---------------------------------------------------------------------- */

static hashtable_t *
hashtable_init_(unsigned  (*hash_func  )(const void *),
                unsigned  (*comp_func  )(const void *, const void *),
                alloc_func_t allocator,
                dealloc_func_t deallocator,
                int         nbuckets)
{
    hashtable_t *res;

    res = allocator(sizeof (hashtable_t));
    memset(res, 0, sizeof(hashtable_t));
    res->buckets = allocator(nbuckets * sizeof(void *));
    memset(res->buckets, 0, nbuckets * sizeof(void *));
    res->nbuckets  = nbuckets;

    res->hash_func = hash_func;
    res->comp_func = comp_func;
    res->alloc     = allocator;
    res->dealloc   = deallocator;

    return res;
}

hashtable_t *
hashtable_init(unsigned  (*hash_func  )(const void *),
               unsigned  (*comp_func  )(const void *, const void *),
               alloc_func_t allocator,
               dealloc_func_t deallocator)
{
    return hashtable_init_(hash_func, comp_func, allocator, deallocator, 2);
}

static void
hashtable_rehash(hashtable_t *hhp, int nbuckets)
{
    hashtable_t *newtab = hashtable_init_(hhp->hash_func,
                                          hhp->comp_func,
                                          hhp->alloc,
                                          hhp->dealloc,
                                          hhp->nbuckets * 2);

    {
        hashtable_iterator_t *iter = hashtable_iterate(hhp);
        void *elem;

        while (hashtable_iterator_next(iter, &elem))
            hashtable_put(newtab, elem);

        hhp->dealloc(iter);
    }

    hashtable_destroy(hhp);
    *hhp = *newtab;
    hhp->dealloc(newtab);
}

void
hashtable_destroy(hashtable_t *hhp)
{
    /* must dealloc all the buckets */
    int i;
    hashtable_bucket_t *p,*q;

    for (i=0; i<hhp->nbuckets; ++i ) {
        p=hhp->buckets[i];
        while(p) {
            q = p; /* save it before advancing */
            p = p->next;
            hhp->dealloc(q); /* dealloc record we just left */
        }
    }

    hhp->dealloc(hhp->buckets);
}

static float
hashtable_ave_depth(hashtable_t *h)
{
    return ((double)h->nentries) / ((double)h->nbuckets);
}

void
hashtable_put(hashtable_t *h, void *data)
{
    /* add an entry for name, pointing to data or node */
    unsigned hv = h->hash_func(data);
    unsigned idx = hv % h->nbuckets;
    hashtable_bucket_t *newbucket = (h->alloc)(sizeof(hashtable_bucket_t));

    newbucket->next = h->buckets[idx];
    newbucket->data = data;
    h->buckets[idx] = newbucket;
    ++(h->nentries);

    if(hashtable_ave_depth(h) > 2.0)
        hashtable_rehash(h,h->nbuckets*2);
}

int
hashtable_get(hashtable_t *h, void *search, void **find)
{
    unsigned hv = (h->hash_func)(search);
    unsigned idx = hv % h->nbuckets;
    struct hashtable_bucket_t *hbp = h->buckets[idx];

    while(hbp) {
        if ((h->comp_func)(search,hbp->data)) {
            *find = hbp->data;
            return 1;
        }
        hbp = hbp->next;
    }
    return 0;
}

void
hashtable_del(hashtable_t *h, void *search, dealloc_func_t destructor)
{
    unsigned hv = (h->hash_func)(search);
    unsigned idx = hv % h->nbuckets;

    struct hashtable_bucket_t **hbpp = &(h->buckets[idx]);

    /* pretty nasty to handle the head of the list */
    /* a sentinel would be cleaner but requires more code elsewhere */
    while(*hbpp) {
        if ((h->comp_func)(search,(*hbpp)->data)) {
            if (destructor) (destructor)((*hbpp)->data);
            *hbpp = (*hbpp)->next;
            --(h->nentries);
        } else {
            hbpp = &(*hbpp)->next;
        }
    }
}

struct hashtable_iterator_t {
    /* data structure invariant (to be maintained by users of this struct):
       the fields of this structure indicate the NEXT element to return. */
    hashtable_t         *h;  /* parent hash table */
    int                  li; /* list index */
    hashtable_bucket_t  *p;  /* bucket ptr */
};

hashtable_iterator_t *
hashtable_iterate(hashtable_t *h)
{
    hashtable_iterator_t *res = (h->alloc)(sizeof(hashtable_iterator_t));

    res->h = h;
    res->li = 0;
    res->p = h->buckets[0];
    return res;
}

int
hashtable_iterator_next(hashtable_iterator_t *iter, void **elem)
{
    for(;;) {
        if (iter->p != NULL) {
            *elem = iter->p->data;
            iter->p = iter->p->next;
            return 1;
        }

        /* iter->p is NULL */

        ++iter->li;

        if (iter->li == iter->h->nbuckets) return 0;

        iter->p = iter->h->buckets[iter->li];
    }
}

/* ---------------------------------------------------------------------- */

#ifdef DEBUGHASH_TEST_MAIN

/* test program below this line */

typedef struct testhash_t {
    char *nm;
    int   val;
    int   idx;
} testhash_t;

unsigned
testhash(const void *x)
{
    /* check name for equality -- only */
    const testhash_t *q = x;
    return hashtable_hash_string(q->nm);
}

unsigned
testcomp(const void *x, const void *y)
{
    const testhash_t *q = x, *r = y;
    return !strcmp(q->nm, r->nm);
}

int
main(int argc, char **argv)
{
    hashtable_t *tbl = hashtable_init(testhash, testcomp, malloc, free);
    testhash_t *rec;
    const int num=5000;
    const int nincr=20;
    int i;

    for(i=0; i<num; ++i) {
        char nm[2];
        nm[0] = (char)i;
        nm[1] = '\000';

        rec = malloc(sizeof(testhash_t));
        rec->nm = strdup(nm);
        rec->val = 0;
        rec->idx = i;

        hashtable_put(tbl, rec);
        assert(hashtable_size(tbl) == i + 1);
    }

    printf("hashtable_size %d\n", hashtable_size(tbl));
    assert(hashtable_size(tbl) == num);


    for (i=0; i<nincr; ++i) {
        hashtable_iterator_t *iter=hashtable_iterate(tbl);
        void *vp;

        while(hashtable_iterator_next(iter, &vp))
            ++((testhash_t *)vp)->val;
    }

    {
        hashtable_iterator_t *iter=hashtable_iterate(tbl);
        void *vp;

        while(hashtable_iterator_next(iter, &vp)) {
            assert(((testhash_t *)vp)->val == nincr);
        }
    }

    {
        for(i=0; i<num; ++i) {
            char nm[2];
            rec->nm = nm;
            nm[0] = (char)i;
            nm[1] = '\000';

            hashtable_del(tbl, rec, NULL);
        }

        printf("hashtable_size %d\n", hashtable_size(tbl));
        assert(hashtable_size(tbl) == 0);
    }
    return 0;
}

#endif
