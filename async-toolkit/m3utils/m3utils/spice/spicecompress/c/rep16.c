
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
