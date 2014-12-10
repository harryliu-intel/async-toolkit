/*****************************************************
 * Copyright 2005 Fulcrum Microsystems               *
 * Copyright 1999 California Institute of Technology *
 * All rights reserved.  Written by Andrew Lines.    *
 *****************************************************/

/******************* DATA TYPES ********************/ 
/******** add additional types to the union ********/
/*
typedef unsigned char byte;
typedef struct _list
  {
  int max,size,mem,shrink;
  union
    {
    byte *b;
    } p;
  } LIST;
*/

/********************** Whole Set Or List Routines ***********************/
void list_realloc (LIST *l, int max);
LIST *list_create (int size);
void list_free (LIST *list);
LIST *temp_list(int size);
void free_temp_list();
void list_copy (LIST *l1, LIST *l2);
void list_move (LIST *l1, LIST *l2);
LIST *list_dup (LIST *l2);
void list_fill(LIST *l, void *p);

/********************** List Routines *********************************/
void list_append_element (LIST *l, void *p);
void list_insert_element (LIST *l, void *p, int n);
void list_remove_element (LIST *l, int n);
void list_append_list (LIST *l1, LIST *l2);
void list_insert_list (LIST *l, LIST *subl, int n);
void list_remove_list (LIST *l, int n, int c);

/********************** Search and Sort Routines ********************/
int list_compare (LIST *l1, LIST *l2, int compare (void *, void *));
int find_element (LIST *l, void *p, int compare(void *, void *));
int find_element_sorted (LIST *l, void *p, int compare (void *, void *));
int list_insert_element_sorted (LIST *l, void *p, int compare (void *, void *));
void list_sort(LIST *l0, int compare (void *, void *));
void list_lazy_sort(LIST *l0, int compare (void *, void *));
void list_finish_lazy_sort(LIST *l0, int compare (void *, void *));
void list_insert_element_lazy_sort(LIST *l0, void *p, int compare (void *, void *));
int find_element_lazy_sort(LIST *l, void *p, int compare (void *, void *));

/********************** Set Routines *********************************/
void set_add_element (LIST *l, void *p, int compare (void *, void *));
void set_remove_element (LIST *l, void *p, int compare (void *, void *));
LIST *set_exclude (LIST *l1, LIST *l2, int compare (void *, void *));
LIST *set_union (LIST *l1, LIST *l2, int compare (void *, void *));
LIST *set_intersect (LIST *l1, LIST *l2, int compare (void *, void *));
int set_contains_set (LIST *l1, LIST *l2, int compare (void *, void *));
