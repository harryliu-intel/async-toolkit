/*
 * Coroutines for Modula-3 !
 *
 * Author : Mika Nystrom <mika.nystroem@intel.com>
 * April, 2018
 *
 */

#include <ucontext.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/mman.h>
#include <errno.h>
#include <stdio.h>
#include <string.h>

typedef struct {
  void (*p)(void *);
  void *arg;
} Closure;

typedef struct {
  stack_t ss;
  ucontext_t uc;
  Closure cl;
} Context;

static void
trampoline(int lo, int hi)
{
  Closure *cl = (Closure *)(((long)hi << 32UL) | (long)lo);
  
  cl->p(cl->arg);
}
  
void *
ContextC__New(void)
{
  return calloc(1,sizeof(Context));
}

void *
ContextC__MakeContext(void    (*p)(void *),
                      void     *stack,
                      int       ssize,
                      Context  *resume,
                      void     *arg)
{
  int psize = getpagesize();
  Context *c=ContextC__New();
  int size;
  int pages;
  void *roundstack;
  unsigned long int roundsize;
  
  /* allocated range is from stack to stack + ssize - 1 */
  /* round stack up */
  roundstack = (void*)((((long)stack-1) / psize) * psize + psize);
  roundsize  = (ssize - (roundstack - stack)) / psize * psize;
  
  if (c == NULL) goto Error;
  if (roundsize < MINSIGSTKSZ) return NULL;

  if(mprotect(roundstack+psize,
              roundsize-2*psize,
              PROT_READ | PROT_WRITE))
    goto Abort;
  
  c->ss.ss_sp = roundstack;
  c->ss.ss_size = roundsize;
  if (mprotect(roundstack, psize, PROT_NONE)) goto Abort;
  if (mprotect(roundstack + roundsize - psize, psize, PROT_NONE)) goto Abort;

  if (getcontext(&(c->uc))) goto Abort;

  /* leave space for redzones at beg and end */
  c->uc.uc_stack.ss_sp   = roundstack + psize;
  c->uc.uc_stack.ss_size = roundsize - 2 * psize;
  c->uc.uc_link          = &(resume->uc);

  c->cl.p =p;
  c->cl.arg = arg;
  {
    Closure *clp= &(c->cl);
    unsigned long int cla =(unsigned long int)clp;
    
    makecontext(&(c->uc),
                (void (*)(void))trampoline,
                2,
                cla & ((1UL<<32UL)-1UL), /* lo */
                cla >> 32                /* hi */
                );
  }

  return c;

 Error:
  {
    int er = errno;
    if (c != NULL) free(c);
    errno = er;
    return NULL;
  }

 Abort:
  fprintf(stderr, "abort : stack %#x align %#x errno %d : %s\n",
          stack,
          (long int)stack % psize,
          errno,
          strerror(errno));
}

void
ContextC__SwapContext (Context *from, Context *to)
{
  if (swapcontext(&(from->uc), &(to->uc))) abort();
}

void
ContextC__DisposeContext (Context *c)
{
  free(c);
}

void *
ContextC__Current(void)
{
  Context *new=ContextC__New();
  if (getcontext(&(new->uc))) abort();
  return new;
}
