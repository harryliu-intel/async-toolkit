#ifndef _SEQTYPE_H
#define _SEQTYPE_H

#define MAXDEPTH 32

typedef struct {
  int d[MAXDEPTH]; /* terminated by -1 */
} seqtype_t;

void init_seqtype(seqtype_t *p); /* sets p->d[0] to -1 */

#endif /* !_SEQTYPE_H */
