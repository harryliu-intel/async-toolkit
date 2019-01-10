#include "varchar.h"
#include <string.h>
#include <assert.h>

unsigned char
varchar_get(const varchar_t *v, unsigned int i)
{
  assert(i < v->length);
  return v->data[i];
}

static const unsigned int defsiz = 10;

varchar_builder_t *
varchar_builder_init(varchar_builder_t *res,
                     varchar_t         *tgt,
                     alloc_func_t       alloc,
                     dealloc_func_t     free)
{
  res->buf = (varchar_base_t *)alloc(defsiz);
  res->bufsiz = defsiz;
  res->res = tgt;
  res->res->data = res->buf;
  res->res->length = 0;
  res->alloc = alloc;
  res->free  = free;

  return res;
}

void
varchar_builder_put(varchar_builder_t *b, varchar_base_t c)
{
  if (b->res->length == b->bufsiz) {
    unsigned int newsiz = b->bufsiz * 2;
    varchar_base_t *newbuf = (varchar_base_t *)b->alloc(newsiz);
    memcpy(newbuf, b->buf, b->bufsiz);
    b->bufsiz = newsiz;
    b->free(b->buf);
    b->buf = newbuf;
    b->res->data = b->buf;
  }
  b->buf[b->res->length++] = c;
}
