/*****************************************************
 * Copyright 2005 Fulcrum Microsystems               *
 * Copyright 1999 California Institute of Technology *
 * All rights reserved.  Written by Andrew Lines.    *
 *****************************************************/

#ifndef GOT_MISC
#include "misc.h"
#define GOT_MISC
#endif

#ifdef LEAK_CHECK
void *_leak_malloc16(size_t size, char *file, unsigned line);
void *_leak_malloc64(size_t size, char *file, unsigned line);
void *_leak_malloc(size_t size, char *file, unsigned line);
void *_leak_strdup(char *p, char *file, unsigned line);
void *_leak_realloc(void *p, size_t size, char *file, unsigned line);
void leak_free(void *p);
void leak_check();
#define leak_malloc16(a) check_alloc(_leak_malloc16(a,__FILE__,__LINE__),__FILE__,__LINE__)
#define leak_malloc64(a) check_alloc(_leak_malloc64(a,__FILE__,__LINE__),__FILE__,__LINE__)
#define leak_malloc(a) check_alloc(_leak_malloc(a,__FILE__,__LINE__),__FILE__,__LINE__)
#define leak_strdup(a) check_alloc(_leak_strdup(a,__FILE__,__LINE__),__FILE__,__LINE__)
#define leak_realloc(a,b) check_alloc(_leak_realloc(a,b,__FILE__,__LINE__),__FILE__,__LINE__)
#define leak_malloc_no_check(a) _leak_malloc(a,__FILE__,__LINE__)
#else
#define leak_malloc16(a) check_alloc(memalign(16,a),__FILE__,__LINE__)
#define leak_malloc64(a) check_alloc(memalign(64,a),__FILE__,__LINE__)
#define leak_malloc(a) check_alloc(malloc(a),__FILE__,__LINE__)
#define leak_strdup(a) check_alloc(strdup(a),__FILE__,__LINE__)
#define leak_realloc(a,b) check_alloc(realloc(a,b),__FILE__,__LINE__)
#define leak_malloc_no_check(a) malloc(a)
#define leak_free(a) free(a)
#define leak_check()
#endif
