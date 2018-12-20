#include <malloc.h>
#include <stdio.h>
#include <string.h>

#define __USE_BSD /* random() */
#include <stdlib.h>
#undef __USE_BSD

#include <sys/time.h>

#include "hohum.h"
#include "mby_top_map.h"
#include "seqtype.h"

void
print_ragged(const seqtype_t *s)
{
  int i=0;
  printf("(");
  while(s->d[i] !=-1) {
    printf(" %d", s->d[i]);
    ++i;
  }
  printf(" )");
}

#define ASPRINTF(x, ...)                                   \
  do {                                                     \
    size_t _sz = 1 + snprintf(NULL, 0, __VA_ARGS__);       \
    char *_buff = malloc(_sz);                             \
    sprintf(_buff, __VA_ARGS__);                           \
    x = _buff;                                             \
  } while(0)

int
inc_ragged_last(seqtype_t *seq)
{
  /* increment ragged in the last valid position */
  const arc_t **arcs;
  int p;
  
  while(seq->d[p] != -1) ++p;
  /* p points to end marker */

  if(p == 0)
    return 0;                /* we were given the null address */
  
  arcs = ragged2arcs(seq);
    
  /* p != 0 */
  assert(arcs[seq->d[p-1]]); /* d[p-1] is valid */

  if (arcs[seq->d[p-1]+1]) 
    return ++(seq->d[p-1]);  /* return last index incremented */
  else
    return 0;                /* at the last index already */
}

#define MATCH_NONE       0
#define MATCH_PARTIAL    1
#define MATCH_COMPLETE   2

int
name2ragged(const char *name, seqtype_t *seq_a)
{
  seqtype_t qqq, *seq=&qqq;
  const char *p=name;
  const arc_t **arcs;
  int k;

  k = 0;
  seq->d[k] = -1;

  while(*p) {
    arcs = ragged2arcs(seq);
    
    if (!arcs) {
      if(*p)
        return MATCH_NONE; /* more string -- no match found */
      else {
        /* entire string consumed but arcs not */
        *seq_a = *seq;
        return MATCH_PARTIAL;
      }
    }
    
    if (arcs && arcs[0]->arr) {
      // array case
      int idx = 0;
      if (*p++ != '[')
        return MATCH_NONE; /* not an array index */
      
      while ('0' <= *p && *p <= '9')
        idx = idx * 10 + (*p++ - '0');

      if (*p++ != ']')
        return MATCH_NONE; /* syntax error */
      
      if (idx >= arcs[0]->arr->size)
        return MATCH_NONE; /* out of range */
      
      printf("match! [%d]\n", idx);
      seq->d[k] = idx;
      seq->d[++k] = -1;
    } else {
      // non-array case
      int i = 0, matched = 0;

      while(arcs[i]) {
        size_t len;
        assert(arcs[i]->sym);

        len = strlen(arcs[i]->sym);
        
        printf("matching p = %s  against  arc .%s\n", p, arcs[i]->sym);
        if (*p=='.' && strncmp(p+1, arcs[i]->sym, len) == 0) {
          printf("match! .%s\n", arcs[i]->sym);
          p += len+1;
          seq->d[k] = i;
          seq->d[++k] = -1;
          matched = 1;
          break;
        }
        ++i;
      }/*elihw*/
      if (!matched)
        return MATCH_NONE; /* component not found */
    }
  }
  /* get here if both arcs and *p are exhausted */
  *seq_a = *seq;
  return MATCH_COMPLETE;
}


void
ragged2nameseq(const seqtype_t *s, const char **seq)
{
  seqtype_t ss;
  int i = 0;

  while(s->d[i] != -1) {
    const arc_t **arcs;
    
    ss = *s;
    ss.d[i] = -1;
    arcs = ragged2arcs(&ss);

    if (arcs && arcs[0]->arr) {
      // array case
      assert(s->d[i] < arcs[0]->arr->size);
      ASPRINTF(seq[i],"[%d]",s->d[i]);
    } else {
      ASPRINTF(seq[i], ".%s", arcs[s->d[i]]->sym);
    }
    ++i;
  }
  seq[i] = NULL;
}

void
test_print_ragged(void)
{
  for (int i=0; i<1000*100; ++i) {
    chipaddr_t a=random() % 1000*1000*10, rem;
    seqtype_t rp;
    const char *seq[MAXDEPTH];

    rem = addr2ragged(a, &rp);
    print_ragged(&rp);
    printf(" rem=%ld\n", rem);

    ragged2nameseq(&rp, seq);

    for (const char **s=seq; *s; ++s)
      printf("%s", *s);
    printf("\n");
  }
}

void
print_time_delta(const struct timeval *tv0,
                 const struct timeval *tv1,
                 const char *msg,
                 long int ops)
{
  double delta = (tv1->tv_usec-tv0->tv_usec)*1e-6 + tv1->tv_sec - tv0->tv_sec;

  assert(msg);
  printf("time_delta %s : %lf ms per op: %le s ops/s: %le\n",
         msg,
         delta*1000,
         delta/ops,
         ops/delta);

}

void
test_time_ragged(void)
{
  const int ops = 10*1000*1000;
  
  struct timeval tv0, tv1;
  gettimeofday(&tv0,NULL);
  for (int i=0; i<ops; ++i) {
    chipaddr_t a=random() % 1000*1000, rem;
    seqtype_t rp;

    rem = addr2ragged(a, &rp);
  }
  gettimeofday(&tv1,NULL);
  print_time_delta(&tv0,&tv1,"test_time_ragged", ops);
}
  

int
main(int argc, char **argv)
{
  seqtype_t s;
  const arc_t **arcs;

  init_seqtype(&s);
  arcs = ragged2arcs(&s);

  {
    int i=0;
    while(arcs[i]) {
      const structarc_t *sym = arcs[i]->sym;
      const arrayarc_t  *arr = arcs[i]->arr;
      
      printf("arcs[%d] sym %s arr %d\n",
             i, sym ? sym : "NULL", arr ? arr->size : -1);
      i++;
    }
  }

  {
    seqtype_t seq;
    init_seqtype(&seq);
    const char *tgt =
      ".mpp[0].mgp[0].rx_ppe.mapper.MAP_DOMAIN_ACTION1[3668].L3_POLICER";
    
    int matched = name2ragged(tgt, &seq);

    printf("tgt %s match? %d -> ", tgt, matched);
    print_ragged(&seq);
    printf("\n");
  }
  
#if 0
  test_print_ragged();
#endif
  test_time_ragged();

  return 0;
}
