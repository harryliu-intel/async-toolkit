
/*
 *  Spice trace waveform decompression
 *
 * 
 *  Author: mika.nystroem@intel.com
 *  January 2023
 *
 */

#include <stdint.h>
#include "rep16.h"
#include "minmax.h"

static double
ToFloat(rep16_signed_t x, unsigned pow)
{
}

static double
ToFloat0(rep16_base_t x)
{
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
