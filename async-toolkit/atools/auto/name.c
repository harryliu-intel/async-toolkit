#include "auto.h"

LIST *namespace=NULL;

/*** create shared namespace ***/
void create_names()
  {
  namespace=list_create(sizeof(char *));
  }

/*** free shared namespace ***/
void free_names()
  {
  int i;
  for (i=0; i<namespace->max; i++) leak_free(namespace->p.pc[i]);
  list_free(namespace);
  }

/*** returns a pointer to a name in namespace ***/
char *get_name(char *name)
  {
  int i;
  i=find_element_lazy_sort(namespace,&name,&str_cmp);
  if (i<0)
    {
    name=leak_strdup(name);
    list_insert_element_lazy_sort(namespace,&name,&str_cmp);
    return name;
    }
  else return namespace->p.pc[i];
  }
