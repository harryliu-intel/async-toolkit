/* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. */
/* SPDX-License-Identifier: Apache-2.0 */

#ifndef __IES_EXCEPTION_H
#define __IES_EXCEPTION_H

/*
  Simple exceptions (no FINALLY) for C
   
  Author : mika.nystroem@intel.com
  December, 2018 
*/

#include <setjmp.h>
#include <stdarg.h>
#include <pthread.h> /* TLS */
#include <stdlib.h>  /* size_t */

#define IES_TRY(q, ex, ...)                                               \
  do {                                                                    \
    ies_push_exception_handler_va(&(q), __VA_ARGS__, IES_EXCEPTION_NONE); \
    if (!(ex = setjmp(q.jb)))                                             \

#define IES_EXCEPT else

#define IES_EXEND(q)                                                      \
    ies_pop_exception_handler(&(q));                                      \
  } while(0)

#define IES_EXRETURN(q,retval)                                            \
  do { ies_pop_exception_handler(&(q)); return (retval); } while(0)

/* usage of above macros

  ies_exception_context_t q;
  ies_exception_t ex;

  IES_TRY(q, ex, EX1, EX2, ...)
    <try-statement>
  IES_EXCEPT
    <except-statement, ex is defined>
  IES_EXEND(q)

  <except-statement> may if desired re-raise the exception

  exceptions are raised with ies_raise_exception

  return statements are disallowed within try-statement and except-statement;
  return may be effected with IES_EXRETURN(q,<retval>)

  if underlying implementation provides thread-local storage (TLS) with
  __thread, and malloc/free are re-entrant, then the implementation is also 
  thread-safe
*/

#define IES_EXCEPTION_ANY     -1
#define IES_EXCEPTION_NONE     0
#define IES_EXCEPTION_1        1 /* example */
#define IES_EXCEPTION_2        2 /* example */
#define IES_EXCEPTION_3        3 /* example */
#define IES_EXCEPTION_INVARG 256 /* change? */
#define IES_EXCEPTION_OOM    257 /* change? */
typedef int ies_exception_t;

typedef struct ies_exception_context {
  jmp_buf                            jb;
  /* the exception handler target itself -- if this frame is to handle
     an exception, this will be the target of longjmp */

  struct ies_exception_context      *up;
  /* next frame below this one on the stack */
  
  struct ex_list                    *exceptions;
  /* list of exceptions handled by frame */

  struct destroy_list               *destroy;
  /* list of destructors associated with frame */

  int                                active;
  /* frame is inactive if it is already handling an exception,
     allows for re-raising an exception from the EXCEPT clause */
} ies_exception_context_t;

void ies_raise_exception(ies_exception_t ex);

void ies_push_exception_handler_va(ies_exception_context_t *ctx, ...);
/* internal function used by IES_ macros */

void ies_pop_exception_handler(ies_exception_context_t *ctx);
/* internal function used by IES_ macros */

#ifndef NULL
#define NULL ((void *)0)
#endif

extern __thread ies_exception_context_t *__ies_exception_top;

void *ies_ex_alloc(size_t size);
/* like malloc, but automatically reclaimed on exception or block exit */

void  ies_ex_destroy(void *p);
/* free matching ies_ex_alloc */

/* allow changing malloc and free used by module; 
   must be called before any other functions in this interface */

void ies_use_malloc(void *(*f)(size_t));
/* change underlying malloc */

void ies_use_free(void (*f)(void *));
/* change underlying free */

#endif /* !__IES_EXCEPTION_H */
