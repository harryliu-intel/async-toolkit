#ifndef _VARCHAR_H
#define _VARCHAR_H

typedef struct {
  const unsigned char *data;
  unsigned int   length;
} varchar_t;

unsigned char varchar_get(const varchar_t *v, unsigned int i);
#endif
