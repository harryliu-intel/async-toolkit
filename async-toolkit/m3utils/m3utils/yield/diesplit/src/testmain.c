/* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. */
/* SPDX-License-Identifier: Apache-2.0 */

#include <dlfcn.h>
#include <assert.h>
#include <stdio.h>

int
main(void)
{
  void *handle;
  void *fp;
  double (*f)(double);

  handle = dlopen("./liba.so", RTLD_NOW);
  assert(handle);

  fp = dlsym(handle, "f");
  assert(fp);

  f = fp;
  for (int i=0; i< 100; ++i) {
    double z = i/100.;
    printf("f(%e) = %e\n", z, f(z));
  }
  
}
  
