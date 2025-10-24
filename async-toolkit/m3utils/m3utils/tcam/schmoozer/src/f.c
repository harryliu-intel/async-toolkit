/* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. */
/* SPDX-License-Identifier: Apache-2.0 */

#include <math.h>
#include <stdio.h>

int
fib(int n)
{ 
  return (int)(pow((1.0+sqrt(5.0))/2.0, (double)n)/sqrt(5)+0.5);
}

int
main(void)
{
  int i;
  for (i=0; i<40; ++i)
    printf("%10d %20d\n", i, fib(i));
}
