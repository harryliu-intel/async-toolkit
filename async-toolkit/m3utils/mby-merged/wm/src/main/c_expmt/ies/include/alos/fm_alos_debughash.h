/* 
   fm_alos_debughash.h 

 */

#ifndef _FM_ALOS_DEBUGHASH_H
#define _FM_ALOS_DEBUGHASH_H

typedef struct hashtable_t hashtable_t;

/* generic hash table code.

   Loosely based on old Caltech implementation of hash.c/hash.h, and on 
   DEC SRC Modula-3 generic hash tables Table.ig, Table.mg 

   Author: Mika Nystrom, September 2016 <mika.nystroem@intel.com>
*/

/* initialization */
#ifndef SWIGPERL /* Bugzilla 32216 */
hashtable_t *hashtable_init(unsigned  (*hash_func  )(const void *),
                            unsigned  (*comp_func  )(const void *, const void *),
                            void     *(*allocator  )(size_t),
                            void      (*deallocator)(void *));
#endif
/* return a new generic hash table

   hash_func shall return a hash value calculated in any way convenient to
   the client.  comp_func shall return whether or not the two entries are 
   equal as far as indexing is concerned.  If comp_func returns nonzero for
   two entries a and b, it is required that hash_func return the exact same
   value for the two.

   A standard pattern is to NOT hash every field of the entry, but instead 
   hash only the part of the entry for indexing.  For example, if what is
   desired to be stored is of type * struct { key keyT, value valueT }, 
   hashing and equality checking can be performed on only the key field.
   For searches, a dummy record with no specific value can be used.

   allocator and deallocator are used by the implementation for all internal
   memory management.
*/

void hashtable_destroy(hashtable_t *h);
/* destroy hash table, does not free stored elements themselves, nor does it 
   free the hashtable_t data structure, that has to be done separately */

/* normal interactions---add an entry, get data from table */

void hashtable_put(hashtable_t *h, void *data);
/* add a record */

void hashtable_del(hashtable_t *h, void *data, void (*destructor)(void *));
/* delete every value that matches, freeing internal memory and passing
   the value to the destructor unless destructor is NULL.

   Does not free h.  Client needs to do that. */

int  hashtable_get(hashtable_t *h, void *search, void **find);
/* return any value that matches in value via find, 1 if there was any match;
   if there is more than one entry that matches, it is unspecified which of 
   the entries is returned. */

int hashtable_size(hashtable_t *h);
/* return the number of entries in table */

typedef struct hashtable_iterator_t hashtable_iterator_t;
/* iterator used for iterating through table */

hashtable_iterator_t *hashtable_iterate(hashtable_t *h);
/* return an iterator, allocated using the alloc function passed to init.
   When done with iterator, dealloc with the appropriate deallocation routine.
 */

int hashtable_iterator_next(hashtable_iterator_t *iter, void **elem);
/* returns 0 if there are no more entries; returns 1 if there are more
   entries to follow.

   entry itself is returned via the elem pointer.

   MODIFIES iter

   modifying the underlying hashtable makes the iterator unsafe to use 
   further. 
*/

/**********************************************************************/

unsigned int hashtable_hash_string(const char *str);
/* a helper for hashing strings */

#endif /* !_FM_ALOS_DEBUGHASH_H */
