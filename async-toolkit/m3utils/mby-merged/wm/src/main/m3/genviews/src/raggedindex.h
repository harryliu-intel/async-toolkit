#ifndef _SEQTYPE_H
#define _SEQTYPE_H

#define MAXDEPTH 32

/* this is the ragged-index type */

typedef struct {
  int d[MAXDEPTH]; /* terminated by -1 */
} seqtype_t;

void init_seqtype(seqtype_t *p); /* sets p->d[0] to -1 */

/* the memory address */

typedef long chipaddr_t;

/* make an address from a literal */

#define ADDR_LITERAL(x) (x##L)

/* an arc */
typedef char structarc_t;

typedef struct {
  int size;
} arrayarc_t;

typedef struct {
  const structarc_t *sym;
  const arrayarc_t  *arr;
} arc_t;

#ifndef NULL
# define NULL ((void *)0)
#endif

#endif /* !_SEQTYPE_H */
