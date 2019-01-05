#ifndef _VARCHAR_H
#define _VARCHAR_H

typedef struct {
  const unsigned char *data;
  unsigned int   length;
} varchar_t;

#endif
