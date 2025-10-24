/* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. */
/* SPDX-License-Identifier: Apache-2.0 */

#include "ies_exception.h"
#include <stdio.h>
#include <assert.h>

static ies_exception_t next_ex;

void
do_calc(void)
{
  printf("do_calc: next_ex = %d\n", next_ex);
  ies_raise_exception(next_ex);
}

void
do_test(void)
{
  ies_exception_context_t q;
  ies_exception_t         ex;

  IES_TRY(q,
          ex,
          IES_EXCEPTION_1,
          IES_EXCEPTION_2)
  {
    do_calc();
  }
  IES_EXCEPT
  {
    printf("Caught exception %d in do_test.\n", ex);
    switch(ex) {
    case IES_EXCEPTION_1:
      printf("EXCEPTION 1\n");
      break;
    case IES_EXCEPTION_2:
      printf("EXCEPTION 2\n");
      break;
    default:
      assert(0);
      break;
    }
  }
  IES_EXEND(q);
}

void
do_test_otr(void)
{
  ies_exception_context_t q;
  ies_exception_t         ex;

  IES_TRY(q, ex, IES_EXCEPTION_ANY)
    do_test();
  IES_EXCEPT
  {
    printf("Caught exception %d in do_test_otr.\n", ex);
    switch(ex) {
    case IES_EXCEPTION_1:
      printf("EXCEPTION 1\n");
      break;
    default:
      ies_raise_exception(ex);
      break;
    }
  }
  IES_EXEND(q);
}

int
main(void)
{
  ies_exception_context_t q;
  ies_exception_t         ex;

  for (next_ex = IES_EXCEPTION_1; next_ex <= IES_EXCEPTION_3; ++next_ex) 
    IES_TRY(q,
           ex,
           IES_EXCEPTION_ANY)
    { 
      do_test_otr();
    }
    IES_EXCEPT
    {
      printf("Caught exception %d in main.\n", ex);
    }
    IES_EXEND(q);
}
  
