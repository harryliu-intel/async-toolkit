#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include "leak.h"

typedef unsigned char byte;
typedef struct _list
  {
  int max,size,mem,shrink;
  union
    {
    byte *b;
    char **pc;
    } p;
  } LIST;
#include "list.h"

int pstrcmp (void *p1, void *p2) {
  return strcmp(*((char **)p1),*((char **)p2));
}

int main() {
  int i;
  char buf[4096],*line=buf,*str;
  LIST *seen;
  seen = list_create(sizeof(char *));
  while (fgets(buf,4096,stdin)) {
    if (strncmp(line,"   + RECT ",10)!=0) {
      fputs(line,stdout);
      for (i=0; i<seen->max; i++) leak_free(seen->p.pc[i]);
      list_realloc(seen,0);
    } else if (find_element_lazy_sort(seen,&line,&pstrcmp)<0) {
      fputs(line,stdout);
      str = leak_strdup(line);
      list_insert_element_lazy_sort(seen,&str,&pstrcmp);
    }
  }
  list_free(seen);
  return 0;
}
