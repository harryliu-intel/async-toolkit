#ifndef _VARCHAR_H
#define _VARCHAR_H

#include "alloc_dealloc.h"

typedef unsigned char varchar_base_t;

typedef struct {
  const varchar_base_t *data;
  unsigned int         length;
} varchar_t;

varchar_base_t varchar_get(const varchar_t *v, unsigned int i);

typedef struct varchar_builder varchar_builder_t;

varchar_builder_t *varchar_builder_init(
     varchar_builder_t    *builder, // new, uninitialized varchar_builder_t
     varchar_t            *tgt,     // new, uninitialized varchar_t
     alloc_func_t          alloc,   // e.g., malloc(3)
     dealloc_func_t        free     // e.g., free(3)
                                       );

void varchar_builder_put(varchar_builder_t *builder, varchar_base_t c);



/**********************************************************************/
/* INTERNAL STRUCTURES FOLLOW -- DO NOT TOUCH OR ACCESS DIRECTLY      */

struct varchar_builder {
  varchar_base_t *buf;
  unsigned int    bufsiz;
  varchar_t      *res;
  alloc_func_t    alloc;
  dealloc_func_t  free;
};

#endif
