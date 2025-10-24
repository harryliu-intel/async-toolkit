/* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. */
/* SPDX-License-Identifier: Apache-2.0 */


/*
 *  Spice trace waveform decompression
 *
 * 
 *  Author: mika.nystroem@intel.com
 *  January 2023
 *
 */

#include <assert.h>
#include <stdint.h>
#include <string.h>
#include <math.h>
#include "rep16.h"
#include "minmax.h"

#define LastBase   (1 << REP16_BITS) - 1
#define FirstBase  0

static const double Max0 = LastBase;
static const double Min0 = FirstBase;

#define FirstSigned (-(1 << (REP16_BITS - 1)) + 1)
#define LastSigned  ( (1 << (REP16_BITS - 1)) - 1)

static const double SRange = LastSigned - FirstSigned;
static const double SMin   = FirstSigned;

static const double Range[4] = { -1e100, 2.0, 1.0, 1.0 / (1 << 1) };

static double
ToFloat(rep16_signed_t x, unsigned pow)
{
  double f      = x;
  double normed = (f - SMin) / SRange;
  double signd  = normed * 2.0 - 1.0;

  assert(pow <= 3);
  
  return signd * Range[pow];
}

static double
ToFloat0(rep16_base_t x)
{
  double f = x;
  return (f - Min0) / Max0;
}

rep16_unsigned_t
Rep16_FromFloat0(double x)
{
  int try = lround(x * Max0 + Min0 + 0.5);

  return MIN(MAX(try, Min0), Max0);
}


double
Rep16_EvalPoly(const rep16_t *t, unsigned int x0)
{
  double xf;
  double yf;
  double y0f;
  int p;

  xf  = x0;
  yf  = 0.0;
  y0f = ToFloat0(t->c0);

  for (p = t->order; p >= 1; --p)
    yf = yf * xf + ToFloat(t->c[p], p);

  yf = yf * xf + y0f;

  return MAX(0.0, MIN(yf, 1.0));
}

/**********************************************************************/

size_t
Rep16Stream_ReadHeader(const char *buf, size_t n, rep16_header_t *header)
{
  size_t p = 0;
  
  header->nwords  = *((int *)  (buf + p));
  p += sizeof(int);

  header->npoints = *((int *)  (buf + p));
  p += sizeof(int);

  header->min     = *((float *)(buf + p));
  p += sizeof(float);

  header->max     = *((float *)(buf + p));
  p += sizeof(float);

  return p;
}

static size_t
read_unsigned(const char *buf, rep16_unsigned_t *x)
{
  size_t p = 0;
  unsigned char b0, b1;

  b0 = *(buf + p);
  p += 1;

  b1 = *(buf + p);
  p += 1;

  *x = (b1 << 8) | b0;

  return p;
}

static size_t
read_signed(const char *buf, rep16_signed_t *x)
{
  size_t p = 0;
  rep16_unsigned_t u;

  p += read_unsigned(buf, &u);

  /* the following is some screwy Modula-3 coding of this */
  if (u > (1 << 15))
    *x = ((int)u - (1 << 16));
  else
    *x = u;
  
  return p;
}

size_t
Rep16Stream_ReadT(const char *buf, size_t n, rep16_t *t)
{
  size_t           p      = 0;
  const int        oMask  = (1 << REP16_ORDER_BITS) - 1;
  rep16_unsigned_t w0;
  Boolean_t        reset;
  size_t           cnt;
  int              i;

  p += read_unsigned(buf + p, &w0);

  reset = !!((w0 >> REP16_ORDER_BITS) & 1);

  t->reset = reset;

  cnt = (w0 >> (REP16_ORDER_BITS + 1));

  t->count = cnt;

  t->order = w0 & oMask;

  memset(t->c, 0, sizeof(t->c));

  if (t->order == 0 || t->reset)
    p += read_unsigned(buf + p, &(t->c0));

  for (i = 1; i <= t->order; ++i)
    p += read_signed(buf + p, &(t->c[i]));

  return p;
}
