/* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. */
/* SPDX-License-Identifier: Apache-2.0 */

#include "ies_exception.h"
#include <malloc.h>
#include <assert.h>

/*
  Simple exceptions (no FINALLY) for C
   
  Author : mika.nystroem@intel.com
  December, 2018 
*/

typedef struct ex_list {
  ies_exception_t ex;
  struct ex_list *next;
} ex_list_t;

typedef struct destroy_list {
  void *arg;
  void (*f)(void *);
  struct destroy_list *next;
} destroy_list_t;

__thread ies_exception_context_t *__ies_exception_top=NULL;

/* local malloc and free */

static void *(*module_malloc)(size_t) = malloc;
static void (*module_free)(void *) = free;

void
ies_use_malloc(void *(*f)(size_t))
{
  module_malloc = f;
}

void
ies_use_free(void (*f)(void *))
{
  module_free = f;
}

void
ies_raise_exception(ies_exception_t ex)
{
  jmp_buf *tgt = NULL;
  
  for(;;) {
    if (__ies_exception_top->active) {
      ex_list_t *p = __ies_exception_top->exceptions;
      /* search for exception stack frame that is listening for 
         the current exception and jump to its jmp_buf
         doing so, we need to clean up every exception frame
         not including the one we jump to (is this really right?)
      */
      while (p) {
        if (p->ex == ex || p->ex == IES_EXCEPTION_ANY) {
          tgt = &(__ies_exception_top->jb);
          break;
        }
        p = p->next;
      }
      if (!__ies_exception_top->up)
        tgt = &(__ies_exception_top->jb);
    }

    if(tgt) {
      __ies_exception_top->active = 0;
      longjmp(*tgt, ex);
    }

    ies_pop_exception_handler(__ies_exception_top);
  }
}

/* notes on memory management

   ies_exception_context_t is allocated on the client's stack

   ex_list is allocated here using ies_ex_alloc; therefore records are
   freed as a matter of course in pop

   destroy_list itself is traversed, executed, and freed in pop
 */

void
ies_push_exception_handler_va(ies_exception_context_t *ctx, ...)
{
  va_list ap;
  va_start(ap, ctx);
  ies_exception_t next;
  
  ctx->exceptions   = NULL;
  ctx->destroy      = NULL;
  ctx->up = __ies_exception_top;

  __ies_exception_top = ctx;
  
  while((next = va_arg(ap, int)) != IES_EXCEPTION_NONE) {
    ex_list_t *new = ies_ex_alloc(sizeof(ex_list_t));
    new->ex = next;
    new->next = ctx->exceptions;
    ctx->exceptions = new;
  } 
  va_end(ap);

  ctx->active = 1;
}

void
ies_pop_exception_handler(ies_exception_context_t *ctx)
{
  ies_exception_context_t *top = __ies_exception_top;
  ies_exception_context_t *up  = top->up;
  
  assert(ctx == __ies_exception_top);
  
  destroy_list_t *p = ctx->destroy;

  while(p) {
    destroy_list_t *nxt = p->next;
    p->f(p->arg);
    p->arg = NULL;
    module_free(p);
    p = nxt;
  }

  __ies_exception_top = up;
}

void *
ies_ex_alloc(size_t size)
{
  destroy_list_t *rec = module_malloc(sizeof(destroy_list_t));
  void *res           = module_malloc(size);
  ies_exception_context_t *top = __ies_exception_top;
  
  if (!res || !rec) {
    module_free(res);
    module_free(rec);
    ies_raise_exception(IES_EXCEPTION_OOM);
  }

  rec->arg = res;
  rec->f = module_free;
  rec->next = top->destroy;
  top->destroy = rec;
  return res;
}

void
ies_ex_destroy(void *p)
{
  ies_exception_context_t *ctx = __ies_exception_top;
  destroy_list_t *d;
  
  while(ctx) {
    d = ctx->destroy;
    while(d) {
      if (d->arg == p) {
        d->f(p);
        d->arg = NULL;
        return;
      }
      d = d->next;
    }
    ctx = ctx->up;
  }
  ies_raise_exception(IES_EXCEPTION_INVARG);
}
