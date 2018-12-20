#include <stdio.h>
#include "hohum.h"
#include "mby_top_map.h"
#include "seqtype.h"

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
    
    

  return 0;
}
