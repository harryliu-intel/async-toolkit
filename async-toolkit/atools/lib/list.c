/*****************************************************
 * Copyright 2005 Fulcrum Microsystems               *
 * Copyright 1999 California Institute of Technology *
 * All rights reserved.  Written by Andrew Lines.    *
 *****************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <memory.h>
#include <assert.h>
#include "misc.h"
#include "leak.h"

/**
 * While still using 32-bit integers to track the number of elements
 * and memory used by a list, allow the real memory of a list to grow
 * in multiples of CHUNK up to CHUNK*(2^31-1) total size.
 **/
#define CHUNK 16

typedef unsigned char byte;
typedef struct _list
  {
  int max,size,mem,shrink;
  union
    {
    byte *b;
    } p;
  } LIST;
#include "list.h"

LIST *temp=NULL;

/********************** Whole Set Or List Routines ***********************/

void list_realloc (LIST *l, int max)
  {
  size_t newmem;
  assert((max>=0)&&(l->size>0));
  newmem=max;
  newmem=(newmem*l->size + CHUNK - 1)/CHUNK;
  if (newmem>=0x7FFFFFFF) error("list too long");
  if (newmem>l->mem)
    {
    newmem+=newmem>>2;
    l->p.b=(byte *) leak_realloc(l->p.b,newmem*CHUNK);
    l->mem=newmem;
    }
  else if ((l->mem>newmem+(newmem>>2))&&(l->shrink))
    {
    if (newmem<1) newmem=1;
    l->p.b=(byte *) leak_realloc(l->p.b,newmem*CHUNK);
    l->mem=newmem;
    }
  l->max=max;
  }

LIST *list_create (int size)
  {
  LIST *list;
  assert(size>0);
  list=(LIST *) leak_malloc(sizeof(LIST));
  list->max=0;
  list->size=size;
  list->mem=1;
  list->shrink=1;
  list->p.b=(byte *) leak_malloc(CHUNK);
  return list;
  }

LIST *temp_list(int size)
  {
  if (temp==NULL) {temp=list_create(size); temp->shrink=0;}
  else            {temp->size=size; list_realloc(temp,0);}
  return temp;
  }

void free_temp_list()
  {
  if (temp!=NULL) list_free(temp);
  temp=NULL;
  }

void list_free (LIST *list)
  {
  leak_free(list->p.b);
  leak_free(list);
  }

void list_copy (LIST *l1, LIST *l2)
  {
  assert(l1->size==l2->size);
  list_realloc(l1,l2->max);
  memcpy(l1->p.b,l2->p.b,l2->max*l2->size);
  }

void list_move (LIST *l1, LIST *l2)
  {
  assert(l1->size==l2->size);
  leak_free(l1->p.b);
  *l1=*l2;
  leak_free(l2);
  }

LIST *list_dup (LIST *l2)
  {
  LIST *l1;
  l1=(LIST *) leak_malloc(sizeof(LIST));
  *l1=*l2;
  l1->p.b=(byte *) leak_malloc(CHUNK * (size_t) l2->mem);
  memcpy(l1->p.b,l2->p.b,l2->max*l2->size);
  return l1;
  }

void list_fill(LIST *l, void *p)
  {
  int i;
  assert((l->max>=0)&&(l->size>0));
  for (i=0; i<l->max; i++) memcpy(l->p.b+l->size*i,p,l->size);
  }

/********************** List Routines *********************************/

void list_append_element (LIST *l, void *p)
  {
  list_realloc(l,l->max+1);
  memcpy(l->p.b+l->size*(l->max-1),p,l->size);
  }

void list_insert_element (LIST *l, void *p, int n)
  {
  int i,oldmax;
  assert ((n<=l->max)&&(n>=0));
  oldmax=l->max;
  list_realloc(l,l->max+1);
  for (i=oldmax-1; i>=n; i--)
    memcpy(l->p.b+(i+1)*l->size,l->p.b+i*l->size,l->size);
  memcpy(l->p.b+n*l->size,p,l->size);
  }

void list_remove_element (LIST *l, int n)
  {
  assert ((n<l->max)&&(n>=0));
  if (n<l->max-1) memmove(l->p.b+l->size*n,l->p.b+l->size*(n+1),l->size*(l->max-n-1));
  list_realloc(l,l->max-1);
  }

void list_append_list (LIST *l1, LIST *l2)
  {
  int oldmax;
  assert(l1->size==l2->size);
  oldmax=l1->max;
  list_realloc(l1,l1->max+l2->max);
  memcpy(l1->p.b+oldmax*l1->size,l2->p.b,l2->max*l2->size);
  }

void list_insert_list (LIST *l, LIST *subl, int n)
  {
  int i,oldmax;
  assert(l->size==subl->size);
  assert((n<=l->max)&&(n>=0));
  if (subl->max==0) return;
  oldmax=l->max;
  list_realloc(l,l->max+subl->max);
  for (i=oldmax-1; i>=n; i--)
    memcpy(l->p.b+(i+subl->max)*l->size,l->p.b+i*l->size,l->size);
  memcpy(l->p.b+n*l->size,subl->p.b,subl->max*l->size);
  }

void list_remove_list (LIST *l, int n, int c)
  {
  int i;
  assert((n>=0)&&(n+c<l->max)&&(c>=0));
  for (i=n; i+c<l->max; i++)
    memmove(l->p.b+i*l->size,l->p.b+(i+c)*l->size,l->size);
  list_realloc(l,l->max-c);
  }

/********************** Search and Sort Routines ********************/

int list_compare (LIST *l1, LIST *l2, int compare (void *, void *))
  {
  int i,r;
  assert(l1->size==l2->size);
  for (i=0; (i<l1->max)&&(i<l2->max); i++)
    {
    r=compare(l1->p.b+i*l1->size,l2->p.b+i*l2->size);
    if (r!=0) return r;
    }
  if (l1->max<l2->max) return -1;
  if (l1->max>l2->max) return  1;
  else return 0;
  }

int find_element (LIST *l, void *p, int compare(void *, void *))
  {
  int i;
  for (i=0; i<l->max; i++) if (compare(l->p.b+l->size*i,p)==0) break;
  if (i<l->max) return i;
  else return -1;
  }

int find_element_sorted (LIST *l, void *p, int compare (void *, void *))
  {
  int i,lo,hi,mid;
  lo=0; hi=l->max-1;
  assert(l->max>=0);
  while (lo<=hi)
    {
    mid=(lo+hi)/2;
    i=compare(l->p.b+l->size*mid,p);
    if (i>0) hi=mid-1;
    else if (i<0) lo=mid+1;
    else return mid;
    }
  return -1;
  }

int list_insert_element_sorted (LIST *l, void *p, int compare (void *, void *))
  {
  int i=0,lo,hi,mid=0;
  lo=0; hi=l->max-1;
  while (lo<=hi)
    {
    mid=(lo+hi)/2;
    i=compare(l->p.b+l->size*mid,p);
    if (i>0) hi=mid-1;
    else if (i<0) lo=mid+1;
    else break;
    }
  if (i>=0) {list_insert_element(l,p,mid); return mid;}
  else {list_insert_element(l,p,mid+1); return mid+1;}
  }

void list_sort(LIST *l0, int compare (void *, void *))
  {
  int i,new,old,size,s,step,max;
  byte *p0,*p1,*p2,*p0max,*p1max,*p2max,*pmax[2];
  LIST *l[2];

  max=l0->max;
  size=l0->size;
  l[0]=l0;
  l[1]=temp_list(l0->size);
  list_realloc(l[1],l[0]->max);
  pmax[0]=l[0]->p.b+max*size;
  pmax[1]=l[1]->p.b+max*size;
  
  for (step=1, old=0, new=1; step<max; step*=2, new=1-new, old=1-old)
    for (i=0; i<max; i+=2*step)
      {
      p0=l[old]->p.b+i*size;
      p0max=p0+step*size;
      p1=p0max;
      p1max=p1+step*size;
      p2=l[new]->p.b+i*size;
      p2max=p2+2*step*size;
      if (p0max>pmax[old]) p0max=pmax[old];
      if (p1max>pmax[old]) p1max=pmax[old];
      if (p2max>pmax[new]) p2max=pmax[new];

      while (p2<p2max)
        {
        if ((p0>=p0max)&&(p1<p1max)) s=1;
        else if ((p1>=p1max)&&(p0<p0max)) s=0;
        else s=(compare(p1,p0)<0);
        if (s) {memcpy(p2,p1,size); p1+=size;}
        else   {memcpy(p2,p0,size); p0+=size;}
        p2+=size;
        }
      }
  if (old==1) list_copy(l0,l[1]);
  }

void list_lazy_sort(LIST *l0, int compare (void *, void *))
  {
  int i,size,s,step,max;
  byte *p0,*p1,*p2,*p0max,*p1max,*p2max;
  LIST *l1;

  max=l0->max;
  size=l0->size;
  l1=temp_list(l0->size);
  list_realloc(l1,max);
  
  for (step=1; step<max; step*=2)
    {
    for (i=0; i+2*step<=max; i+=2*step)
      {
      p0=l0->p.b+i*size;
      p0max=p0+step*size;
      p1=p0max;
      p1max=p1+step*size;
      p2=l1->p.b+i*size;
      p2max=p2+2*step*size;

      while (p2<p2max)
        {
        if      (p0>=p0max) s=1;
        else if (p1>=p1max) s=0;
        else s=(compare(p1,p0)<0);
        if (s) {memcpy(p2,p1,size); p1+=size;}
        else   {memcpy(p2,p0,size); p0+=size;}
        p2+=size;
        }
      }
    memcpy(l0->p.b,l1->p.b,i*size);
    }
  }

void list_finish_lazy_sort(LIST *l0, int compare (void *, void *))
  {
  int i,size,s,step,max;
  byte *p0,*p1,*p2,*p0max,*p1max,*p2max,*pmax[2];
  LIST *l1;

  max=l0->max;
  size=l0->size;
  l1=temp_list(l0->size);
  list_realloc(l1,max);
  pmax[0]=l0->p.b+max*size;
  pmax[1]=l1->p.b+max*size;
  
  for (step=1; step<max; step*=2)
    {
    for (i=0; i+2*step<=max; i+=2*step);
    p0=l0->p.b+i*size;
    p0max=p0+step*size;
    p1=p0max;
    p1max=p1+step*size;
    p2=l1->p.b+i*size;
    p2max=p2+2*step*size;
    if (p0max>pmax[0]) p0max=pmax[0];
    if (p1max>pmax[0]) p1max=pmax[0];
    if (p2max>pmax[1]) p2max=pmax[1];

    while (p2<p2max)
      {
      if ((p0>=p0max)&&(p1<p1max)) s=1;
      else if ((p1>=p1max)&&(p0<p0max)) s=0;
      else s=(compare(p1,p0)<0);
      if (s) {memcpy(p2,p1,size); p1+=size;}
      else   {memcpy(p2,p0,size); p0+=size;}
      p2+=size;
      }
    memcpy(l0->p.b+i*size,l1->p.b+i*size,(max-i)*size);
    }
  }

void list_insert_element_lazy_sort(LIST *l0, void *p, int compare (void *, void *))
  {
  int step,i,max,size,s;
  byte *p0,*p1,*p2,*p0max,*p1max,*p2max;
  LIST *l1;

  list_append_element(l0,p);
  max=l0->max;
  if (max&1) return;
  size=l0->size;
  for (step=1; !(step&max); step<<=1);
  l1=temp_list(size);
  list_realloc(l1,step);

  for (step=1; !(step&max); step<<=1)
    {
    i=max-2*step;
    p0=l0->p.b+i*size;
    p0max=p0+step*size;
    p1=p0max;
    p1max=p1+step*size;
    p2=l1->p.b;
    p2max=p2+2*step*size;

    while (p2<p2max)
      {
      if ((p0>=p0max)&&(p1<p1max)) s=1;
      else if ((p1>=p1max)&&(p0<p0max)) s=0;
      else s=(compare(p1,p0)<0);
      if (s) {memcpy(p2,p1,size); p1+=size;}
      else   {memcpy(p2,p0,size); p0+=size;}
      p2+=size;
      }
    memcpy(l0->p.b+i*size,l1->p.b,(max-i)*size);
    }
  }

int find_element_lazy_sort(LIST *l, void *p, int compare (void *, void *))
  {
  int i,lo,hi,mid,uppermask,bitmask;
 
  assert(l->max>=0);
  for (bitmask=1, uppermask=~0; l->max&uppermask; bitmask<<=1, uppermask<<=1)
    if (l->max&bitmask) 
      {
      lo=l->max&(uppermask<<1);
      hi=(l->max&uppermask)-1;
      while (lo<=hi)
        {
        mid=(lo+hi)/2;
        i=compare(l->p.b+l->size*mid,p);
        if (i>0) hi=mid-1;
        else if (i<0) lo=mid+1;
        else return mid;
        }
      }
  return -1;
  }

/********************** Set Routines *********************************/

void set_add_element (LIST *l, void *p, int compare (void *, void *))
  {
  if (find_element(l,p,compare)<0) list_append_element(l,p);
  }

void set_remove_element (LIST *l, void *p, int compare (void *, void *))
  {
  int i;
  while ((i=find_element(l,p,compare))>=0) list_remove_element(l,i);
  }

LIST *set_exclude (LIST *l1, LIST *l2, int compare (void *, void *))
  {
  int i;
  LIST *l3;
  assert(l1->size==l2->size);
  l3=list_create(l1->size);
  for (i=0; i<l1->max; i++)
    if (find_element(l2,l1->p.b+i*l1->size,compare)<0)
      list_append_element(l3,l1->p.b+i*l1->size);
  return l3;
  }

LIST *set_union (LIST *l1, LIST *l2, int compare (void *, void *))
  {
  int i;
  LIST *l3;
  assert(l1->size==l2->size);
  l3=list_create(l1->size);
  for (i=0; i<l1->max; i++) set_add_element(l3,l1->p.b+i*l1->size,compare);
  for (i=0; i<l2->max; i++) set_add_element(l3,l2->p.b+i*l2->size,compare);
  return l3;
  }

LIST *set_intersect (LIST *l1, LIST *l2, int compare (void *, void *))
  {
  int i;
  LIST *l3;
  assert(l1->size==l2->size);
  l3=list_create(l1->size);
  for (i=0; i<l1->max; i++)
    if (find_element(l2,l1->p.b+i*l1->size,compare)>=0)
      set_add_element(l3,l1->p.b+i*l1->size,compare);
  return l3;
  }

int set_contains_set (LIST *l1, LIST *l2, int compare (void *, void *))
  {
  int i;
  assert(l1->size==l2->size);
  for (i=0; i<l1->max; i++)
    if (find_element(l2,l1->p.b+i*l1->size,compare)<0) break;
  return (i==l1->max);
  }
