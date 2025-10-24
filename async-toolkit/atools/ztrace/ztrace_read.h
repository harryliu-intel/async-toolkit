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

#ifndef __ZTRACE_READ_H
#define __ZTRACE_READ_H

#include <stdint.h>
#include <stdio.h>

int ztrace_read_float(FILE *stream, float *x);

int ztrace_read_double(FILE *stream, double *x);

int ztrace_read_int(FILE *stream, int *x);

int ztrace_read_uint64_t(FILE *stream, uint64_t *x);

#endif /* !__ZTRACE_READ_H */
