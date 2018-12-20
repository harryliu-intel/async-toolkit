#include <malloc.h>
#include <stdio.h>
#define __USE_BSD /* random() */
#include <stdlib.h>
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

#define ASPRINTF(x, ...)                      \
  do {                                       \
    size_t _sz = 1 + snprintf(NULL, 0, __VA_ARGS__);      \
    char *_buff = malloc(_sz);                             \
    sprintf(_buff, __VA_ARGS__);                         \
    x = _buff;                                           \
  } while(0)




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

    

  return 0;
}
