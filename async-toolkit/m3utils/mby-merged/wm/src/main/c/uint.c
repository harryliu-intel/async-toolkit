#include <assert.h>
#include "uint.h"

uint64
uint_extract_wide_bits(const uint1 *u, uint p, uint w)
{  
  /* return the w bits of u starting from bit p */
  uint64 res;
  
  assert (w <= 64);

  res = 0;
  for (int i = p + w - 1; i > p; --i)
    res = (res << 1) | (u[i] ? 1 : 0);

  return res;
}

void
uint_insert_wide_bits(uint1 *x, const uint64 y, uint i, uint n)
{
  /* overwite the n bits of x starting at bit i with the n LSBs of y */

  assert (n <= 64);
  
  for (int k = 0; k < n; ++k)
    x[i+k] = (y >> k) & 1;
}
