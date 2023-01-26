
/*
 *  Fixed-point polynomial representation
 *
 * 
 *  Author: mika.nystroem@intel.com
 *  January 2023
 *
 */

#ifndef __REP16_H
#define __REP16_H

#include <stddef.h>
#include <stdint.h>
#include "boolean.h"

typedef uint16_t           rep16_count_t;
typedef uint16_t           rep16_order_t;
typedef uint16_t           rep16_unsigned_t;
typedef  int16_t           rep16_signed_t;

typedef rep16_unsigned_t   rep16_base_t;

#define REP16_BITS              16
#define REP16_COUNT_BITS        13
#define REP16_ORDER_BITS        (REP16_BITS - REP16_COUNT_BITS - 1)

#define REP16_MAXCOUNT          ((1 << REP16_COUNT_BITS) - 1)
#define REP16_MAXPOWER          ((1 << REP16_ORDER_BITS) - 1)

typedef struct {
  rep16_count_t       count;
  rep16_order_t       order;
  rep16_unsigned_t    c0;
  rep16_signed_t      c[REP16_MAXPOWER + 1]; /* c[0] is unused */
  Boolean_t           reset;
} rep16_t;

typedef struct {
  size_t nwords;
  size_t npoints;
  double min;
  double max;
} rep16_header_t;

typedef struct {
  rep16_t         r;
  int             lo;
  rep16_count_t   n;
} poly_segment16_t;

double Rep16_EvalPoly(const rep16_t *t, unsigned int x0);

rep16_unsigned_t Rep16_FromFloat0(double x);

size_t Rep16Stream_ReadHeader(const char *buf, size_t n, rep16_header_t *header);
/* returns bytes read */

size_t Rep16Stream_ReadT(const char *buf, size_t n, rep16_t *t);

#endif /* !__REP16_H */


