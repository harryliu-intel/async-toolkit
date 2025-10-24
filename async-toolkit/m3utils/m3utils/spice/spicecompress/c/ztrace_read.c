/* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. */
/* SPDX-License-Identifier: Apache-2.0 */

/*
 *  Binary I/O
 *  Assumes little-endian IEEE!
 *
 * 
 *  Author: mika.nystroem@intel.com
 *  January 2023
 *
 */

#include <stdint.h>
#include <stdio.h>

int
ztrace_read_float(FILE *stream, float *x)
{
  if (!fread(x, sizeof(float), 1, stream)) return 0;
  return 1;
}

int
ztrace_read_double(FILE *stream, double *x)
{
  float y;
  if (!fread(&y, sizeof(float), 1, stream)) return 0;
  *x = y;
  return 1;
}

int
ztrace_read_int(FILE *stream, int *x)
{
  int res;
  res = fread(x, sizeof(int), 1, stream);
  return res;
}

int
ztrace_read_uint64_t(FILE *stream, uint64_t *x)
{
  int res;
  res = fread(x, sizeof(uint64_t), 1, stream);
  return res;
}
