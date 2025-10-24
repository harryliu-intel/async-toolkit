/* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. */
/* SPDX-License-Identifier: Apache-2.0 */

#include <time.h>

unsigned long int
get_nanoclock()
{
  /* to sort-of match the version from Java */
  struct timespec ts;

  (void)clock_gettime(CLOCK_MONOTONIC_RAW, &ts);

  return (ts.tv_sec * 1000 * 1000 * 1000) + ts.tv_nsec;
}
