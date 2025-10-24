/*****************************************************
 * Copyright 2005 Fulcrum Microsystems               *
 * Copyright 1999 California Institute of Technology *
 * All rights reserved.  Written by Andrew Lines.    *
 *****************************************************/

#include <stdlib.h>
#include <stdio.h>
#include <memory.h>
#include <malloc.h>
#include <assert.h>
#include <string.h>
#include "misc.h"
#include "leak.h"

#ifdef LEAK_CHECK
#define LEAK_MAX 1048576

typedef struct _leak_entry
  {
  void *p;
  char *file;
  unsigned line;
  } LEAK_ENTRY;
 
LEAK_ENTRY leak_entry[LEAK_MAX];
unsigned leak_num=0;

/* malloc with 16 byte alignment, needed for SIMD */
void *_leak_malloc16(size_t size, char *file, unsigned line)
  {
  leak_num++;
  assert(leak_num<LEAK_MAX);
  leak_entry[leak_num-1].p=memalign(16,size);
  leak_entry[leak_num-1].file=file;
  leak_entry[leak_num-1].line=line;
  assert(leak_entry[leak_num-1].p!=NULL);
  return leak_entry[leak_num-1].p;
  }

/* malloc with 64 byte alignment, useful for prefetch */
void *_leak_malloc64(size_t size, char *file, unsigned line)
  {
  leak_num++;
  assert(leak_num<LEAK_MAX);
  leak_entry[leak_num-1].p=memalign(64,size);
  leak_entry[leak_num-1].file=file;
  leak_entry[leak_num-1].line=line;
  assert(leak_entry[leak_num-1].p!=NULL);
  return leak_entry[leak_num-1].p;
  }

void *_leak_malloc(size_t size, char *file, unsigned line)
  {
  leak_num++;
  assert(leak_num<LEAK_MAX);
  leak_entry[leak_num-1].p=malloc(size);
  leak_entry[leak_num-1].file=file;
  leak_entry[leak_num-1].line=line;
  assert(leak_entry[leak_num-1].p!=NULL);
  return leak_entry[leak_num-1].p;
  }

void *_leak_strdup(char *p, char *file, unsigned line)
  {
  leak_num++;
  assert(leak_num<LEAK_MAX);
  leak_entry[leak_num-1].p=strdup(p);
  leak_entry[leak_num-1].file=file;
  leak_entry[leak_num-1].line=line;
  assert(leak_entry[leak_num-1].p!=NULL);
  return leak_entry[leak_num-1].p;
  }

void *_leak_realloc(void *p, size_t size, char *file, unsigned line)
  {
  int i;
  for (i=0; i<leak_num; i++) if (leak_entry[i].p==p) break;
  assert(i<leak_num);
  leak_entry[i].p=realloc(p,size);
  leak_entry[i].file=file;
  leak_entry[i].line=line;
  assert(leak_entry[i].p!=NULL);
  return leak_entry[i].p;
  }

void leak_free(void *p)
  {
  int i,j;
  free(p);
  for (i=0; i<leak_num; i++) if (leak_entry[i].p==p) break;
  assert(i<leak_num);
  for (j=i; j<leak_num-1; j++) leak_entry[j]=leak_entry[j+1];
  leak_num--;
  }

void leak_check()
  {
  int i;
  fprintf(stderr,"%u MEMORY LEAKS\n",leak_num);
  for (i=0; i<leak_num; i++)
    fprintf(stderr,"MEMORY LEAK: %p allocated in %s at line %u\n",
      leak_entry[i].p,leak_entry[i].file,leak_entry[i].line);
  }
#endif
