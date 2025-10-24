/* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. */
/* SPDX-License-Identifier: Apache-2.0 */

#include <stdio.h>
#include <stdarg.h>
#include <malloc.h>

#ifndef NULL
#define NULL ((void *)0)
#endif

#define ifdebug(x) \
	if ( messy_stuff(#x, x, __func__, __LINE__, __FILE__), (x) )

int
messy_stuff(const char *guard_string, int guard_value, const char *func, int l, const char *fnm)
{
  printf("%s , %d : (%s) : (%s) = %d\n", fnm, l, func, guard_string, guard_value);
  return 0;
}


#define ifdebug2(x, ...)                                          \
  if ( messy_stuff_va(#x, x, __func__, __LINE__, __FILE__, __VA_ARGS__), (x) )

#define DBGBUFSIZ 16 // in reality would probably be 1K at least

int
messy_stuff_va(const char *guard_string, 
               int         guard_value,
               const char *func,
               int         l,
               const char *fnm,
               ...
               )
{
  va_list ap;
  va_start(ap, fnm);
  const char *fmt;
  char buf[DBGBUFSIZ];
  

  fmt = va_arg(ap, const char *);
  size_t sz=vsnprintf(buf, DBGBUFSIZ, fmt, ap);
  buf[DBGBUFSIZ-1] = '\000';

  if (sz >= DBGBUFSIZ) {
    buf[DBGBUFSIZ-2] = '.';
    buf[DBGBUFSIZ-3] = '.';
    buf[DBGBUFSIZ-4] = '.';
  }
  
  printf("buf=%s\n", buf);
  
  printf("%s , %d : (%s) : %s : (%s) = %d\n",
         fnm, l, func, buf, guard_string, guard_value);

  va_end(ap);

  return 0;
}


int
main(void)
{
  int a=0;
  int b=1;
  int c=1;

  ifdebug( a + b + c > 2 )  {
    printf("greater than 2\n");
  } else ifdebug ( a + b + c > 1) {
    printf("greater than 1\n");
  } else {
    printf("not greater than 2\n");
  }

  ifdebug((a + b > 2, a + b > 0)) // double parens necessary
    printf("greater than 0.\n");

  int zero=0;
  
  ifdebug2 ( a + b > zero, "is it > %d?", zero )
    printf("greater than 0.\n");
  
  ifdebug2 ( a + b > zero, "is it greater than zero = %d?", zero )
    printf("greater than 0.\n");

  ifdebug2 ( a + b > zero, "%d + %d > %d?", a, b, zero )
    printf("greater than 0.\n");

}
