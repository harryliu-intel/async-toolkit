#include "varchar.h"
#include <assert.h>

unsigned char
varchar_get(const varchar_t *v, unsigned int i)
{
  assert(i < v->length);
  return v->data[i];
}
