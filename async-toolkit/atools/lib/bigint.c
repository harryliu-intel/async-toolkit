/*****************************************************
 * Copyright 2005 Fulcrum Microsystems               *
 * Copyright 1999 California Institute of Technology *
 * All rights reserved.  Written by Andrew Lines.    *
 *****************************************************/

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <assert.h>
#include "misc.h"
#include "leak.h"
#include "newlex.h"
#include "bigint.h"

typedef unsigned char byte;
typedef struct _list
  {
  int max,size,mem,shrink;
  union
    {
    byte *b;
    BIOP *op;
    } p;
  } LIST;

#include "list.h"

#define STACKMAX 1024
#define ALLOC_BIGINTS 1024

/********************* create and free BIGINT's *********************/

/*** linked list of free BIGINT's ***/
BIGINT *available_bigint = NULL;

/*** linked list of allocated BIGINT memory blocks ***/
BIGINT *allocated_blocks = NULL;

/*** reuse or malloc a BIGINT link segment ***/
BIGINT *alloc_bigint()
  {
  BIGINT *b;
  int i;

  /*** allocate ALLOC_BIGINTS more BIGINT's if none available ***/
  if (available_bigint==NULL)
    {
    b = leak_malloc(ALLOC_BIGINTS*sizeof(BIGINT));
    for (i=0; i<ALLOC_BIGINTS; i++)
      {
      b[i].value=0;
      b[i].next=b+i+1;
      }
    b[ALLOC_BIGINTS-1].next = NULL;
    b[0].next = allocated_blocks;
    allocated_blocks = b;
    available_bigint = b+1;
    }

  /*** grab next BIGINT from available linked list ***/
  b = available_bigint;
  available_bigint = available_bigint->next;
  b->value = 0;
  b->next = NULL;
  return b;
  }

/*** return a BIGINT to the free linked list ***/
void free_bigint(BIGINT *b)
  {
  BIGINT *b2;
  b2 = b;
  while (b2->next!=NULL) b2 = b2->next;
  b2->next = available_bigint;
  available_bigint = b;
  }

/*** free all allocated BIGINT memory blocks ***/
void free_bigint_memory()
  {
  BIGINT *b,*next;
  b = allocated_blocks;
  while (b!=NULL)
    {
    next = b->next;
    leak_free(b);
    b = next;
    }
  allocated_blocks = NULL;
  available_bigint = NULL;
  }

/*** report total memory allocated in BIGINT blocks ***/
int total_bigint_memory()
  {
  int mem=0;
  BIGINT *b,*next;
  b = allocated_blocks;
  while (b!=NULL)
    {
    next = b->next;
    mem += ALLOC_BIGINTS * sizeof(BIGINT);
    b = next;
    }
  return mem;
  }

/*** add another sign extended 32 bits to an existing BIGINT ***/
void extend_bigint(BIGINT *b)
  {
  int v = b->value;
  while (b->next!=NULL) { v = b->value; b = b->next; }
  b->next = alloc_bigint();
  if (v<0) b->next->value = -1;
  }

/*** create a BIGINT given an integer value ***/
BIGINT *bigint_from_int(int value)
  {
  BIGINT *b;
  b = alloc_bigint();
  b->value = value;
  return b;
  }

/*** create a BIGINT given a unsinged long long value ***/
BIGINT *bigint_from_ull(unsigned long long value)
  {
  BIGINT *b;
  b = alloc_bigint();
  extend_bigint(b);
  extend_bigint(b);
  b->value = (int) value;
  b->next->value = (int) (value>>32);
  b->next->next->value = 0;
  return b;
  }

/*** assign new value to existing bigint ***/
void assign_bigint(BIGINT *b, BIGINT *new) 
  {
  if (b->next!=NULL) free_bigint(b->next);
  b->next=NULL;
  while (new!=NULL) 
    {
    b->value = new->value;
    new = new->next;
    if (new!=NULL) { extend_bigint(b); b = b->next; }
    }
  }

/*** assign new value to existing bigint, free old value ***/
void move_bigint(BIGINT *b, BIGINT *new) 
  {
  assign_bigint(b,new);
  free_bigint(new);
  }

/*** identity function ***/
BIGINT *copy_bigint(BIGINT *b1)
  {
  BIGINT *b,*res;
  b = res = alloc_bigint();
  while (b1!=NULL)
    {
    b->value = b1->value;
    b1 = b1->next;
    if (b1!=NULL) { extend_bigint(b); b = b->next; }
    }
  return res;
  }

/********************* arithmetic on BIGINT's ************************/

/*** convert int to long long without sign extension ***/
unsigned long long int_to_ull(int x)
  {
  return ((unsigned long long) x) & 0xFFFFFFFFL;
  }

/*** test for 0 ***/
int bigint_is_zero(BIGINT *b)
  {
  return (b->next==NULL) && (b->value==0);
  }

/*** test for not 0 ***/
int bigint_is_nonzero(BIGINT *b)
  {
  return (b->next!=NULL) || (b->value!=0);
  }

/*** test for 1 ***/
int bigint_is_one(BIGINT *b)
  {
  return (b->next==NULL) && (b->value==1);
  }

/*** test for 2 ***/
int bigint_is_two(BIGINT *b)
  {
  return (b->next==NULL) && (b->value==2);
  }

/*** test for negative ***/
int bigint_is_negative(BIGINT *b)
  {
  int v=0;
  while (b!=NULL) { v = b->value; b = b->next; }
  return v<0;
  }

/*** cast a bigint to an int, warn if it doesn't fit ***/
int int_from_bigint(BIGINT *b)
  {
  if (b->next!=NULL)
    {
    fprintf(stderr,"WARNING: truncating bigint ");
    print_bigint_hex(stderr,b);
    fprintf(stderr," to int %X.\n",b->value);
    }
  return b->value;
  }

/*** trim unnecessary sign extension ***/
void canonicalize_bigint(BIGINT *b)
  {
  if (b->next!=NULL) canonicalize_bigint(b->next);
  if ((b->next!=NULL) && (b->next->next==NULL))
    {
    if (((b->value< 0) && (b->next->value==-1)) ||
        ((b->value>=0) && (b->next->value== 0)))
      {
      free_bigint(b->next);
      b->next=NULL;
      }
    }
  }

/*** add ***/
BIGINT *add_bigint(BIGINT *b1, BIGINT *b2)
  {
  int cin=0,v1=0,v2=0;
  unsigned long long l;
  BIGINT *b,*sum;
  b = sum = alloc_bigint();
  while ((b1!=NULL) || (b2!=NULL))
    {
    /*** get b1/b2 values and advance to next segment ***/
    if (b1==NULL) v1 = (v1<0) ? -1 : 0;
    else { v1 = b1->value; b1 = b1->next; }
    if (b2==NULL) v2 = (v2<0) ? -1 : 0;
    else { v2 = b2->value; b2 = b2->next; }

    /*** add ***/
    l = int_to_ull(v1) + int_to_ull(v2) + int_to_ull(cin);
    b->value = (int) l;
    cin = ((int) (l>>32))&1;

    /*** extend ***/
    extend_bigint(b);
    b = b->next;
    }

  /*** handle overflow segment ***/
  v1 = (v1<0) ? -1 : 0;
  v2 = (v2<0) ? -1 : 0;
  b->value = v1 + v2 + cin;
  canonicalize_bigint(sum);
  return sum;
  }

/*** subtract ***/
BIGINT *sub_bigint(BIGINT *b1, BIGINT *b2)
  {
  int cin=1,v1=0,v2=0;
  unsigned long long l;
  BIGINT *b,*diff;
  b = diff = alloc_bigint();
  while ((b1!=NULL) || (b2!=NULL))
    {
    /*** get b1/b2 values and advance to next segment ***/
    if (b1==NULL) v1 = (v1<0) ? -1 : 0;
    else { v1 = b1->value; b1 = b1->next; }
    if (b2==NULL) v2 = (v2<0) ? -1 : 0;
    else { v2 = b2->value; b2 = b2->next; }

    /*** subtract ***/
    l = int_to_ull(v1) + int_to_ull(~v2) + int_to_ull(cin);
    b->value = (int) l;
    cin = ((int) (l>>32))&1;

    /*** extend ***/
    extend_bigint(b);
    b = b->next;
    }

  /*** handle overflow segment ***/
  v1 = (v1<0) ? -1 : 0;
  v2 = (v2<0) ? -1 : 0;
  b->value = v1 + ~v2 + cin;
  canonicalize_bigint(diff);
  return diff;
  }

/*** negate ***/
BIGINT *neg_bigint(BIGINT *b1)
  {
  int cin=1,v1=0;
  unsigned long long l;
  BIGINT *b,*neg;
  b = neg = alloc_bigint();
  while (b1!=NULL)
    {
    /*** get b value and advance to next segment ***/
    if (b1==NULL) v1 = (v1<0) ? -1 : 0;
    else { v1 = b1->value; b1 = b1->next; }

    /*** negate ***/
    l = int_to_ull(~v1) + int_to_ull(cin);
    b->value = (int) l;
    cin = ((int) (l>>32))&1;

    /*** extend ***/
    extend_bigint(b);
    b = b->next;
    }

  /*** handle overflow segment ***/
  v1 = (v1<0) ? -1 : 0;
  b->value = ~v1 + cin;
  canonicalize_bigint(neg);
  return neg;
  }

/*** bitwise or ***/
BIGINT *bit_or_bigint(BIGINT *b1, BIGINT *b2)
  {
  int v1=0,v2=0;
  BIGINT *b,*or;
  b = or = alloc_bigint();
  while ((b1!=NULL) || (b2!=NULL))
    {
    /*** get b1/b2 values and advance to next segment ***/
    if (b1==NULL) v1 = (v1<0) ? -1 : 0;
    else { v1 = b1->value; b1 = b1->next; }
    if (b2==NULL) v2 = (v2<0) ? -1 : 0;
    else { v2 = b2->value; b2 = b2->next; }

    /*** or ***/
    b->value = v1 | v2;

    /*** extend ***/
    if ((b1!=NULL) || (b2!=NULL)) { extend_bigint(b); b = b->next; }
    }
  return or;
  }

/*** bitwise and ***/
BIGINT *bit_and_bigint(BIGINT *b1, BIGINT *b2)
  {
  int v1=0,v2=0;
  BIGINT *b,*and;
  b = and = alloc_bigint();
  while ((b1!=NULL) || (b2!=NULL))
    {
    /*** get b1/b2 values and advance to next segment ***/
    if (b1==NULL) v1 = (v1<0) ? -1 : 0;
    else { v1 = b1->value; b1 = b1->next; }
    if (b2==NULL) v2 = (v2<0) ? -1 : 0;
    else { v2 = b2->value; b2 = b2->next; }

    /*** and ***/
    b->value = v1 & v2;

    /*** extend ***/
    if ((b1!=NULL) || (b2!=NULL)) { extend_bigint(b); b = b->next; }
    }
  return and;
  }

/*** bitwise xor ***/
BIGINT *bit_xor_bigint(BIGINT *b1, BIGINT *b2)
  {
  int v1=0,v2=0;
  BIGINT *b,*xor;
  b = xor = alloc_bigint();
  while ((b1!=NULL) || (b2!=NULL))
    {
    /*** get b1/b2 values and advance to next segment ***/
    if (b1==NULL) v1 = (v1<0) ? -1 : 0;
    else { v1 = b1->value; b1 = b1->next; }
    if (b2==NULL) v2 = (v2<0) ? -1 : 0;
    else { v2 = b2->value; b2 = b2->next; }

    /*** xor ***/
    b->value = (v1 & ~v2) | (~v1 & v2);
    
    /*** extend ***/
    if ((b1!=NULL) || (b2!=NULL)) { extend_bigint(b); b = b->next; }
    }
  return xor;
  }

/*** bitwise not ***/
BIGINT *bit_not_bigint(BIGINT *b1)
  {
  BIGINT *b,*res;
  b = res = alloc_bigint();
  while (b1!=NULL)
    {
    b->value = ~b1->value;
    b1 = b1->next;
    if (b1!=NULL) { extend_bigint(b); b = b->next; }
    }
  return res;
  }

/*** boolean or ***/
BIGINT *bool_or_bigint(BIGINT *b1, BIGINT *b2)
  {
  return bigint_from_int(bigint_is_nonzero(b1) || bigint_is_nonzero(b2));
  }

/*** boolean and ***/
BIGINT *bool_and_bigint(BIGINT *b1, BIGINT *b2)
  {
  return bigint_from_int(bigint_is_nonzero(b1) && bigint_is_nonzero(b2));
  }

/*** boolean not ***/
BIGINT *bool_not_bigint(BIGINT *b)
  {
  return bigint_from_int(!bigint_is_nonzero(b));
  }

/*** return -1, 0, 1 for comparison result ***/
int compare_bigint(BIGINT *b1, BIGINT *b2)
  {
  int c = 0, v1 = 0, v2 = 0;
  while ((b1!=NULL) || (b2!=NULL))
    {
    /*** get b1/b2 values and advance to next segment ***/
    if (b1==NULL) v1 = (v1<0) ? -1 : 0;
    else { v1 = b1->value; b1 = b1->next; }
    if (b2==NULL) v2 = (v2<0) ? -1 : 0;
    else { v2 = b2->value; b2 = b2->next; }

    /*** unsigned compare ***/
    if      ((unsigned) v1 > (unsigned) v2) c =  1;
    else if ((unsigned) v1 < (unsigned) v2) c = -1;
    }

  /*** signed compare and return ***/
  if      (v1>v2) c =  1;
  else if (v1<v2) c = -1;
  return c;
  }
  
/*** compare equal ***/
BIGINT *eq_bigint(BIGINT *b1, BIGINT *b2)
  {
  return bigint_from_int(compare_bigint(b1,b2)==0);
  }

/*** compare not equal ***/
BIGINT *neq_bigint(BIGINT *b1, BIGINT *b2)
  {
  return bigint_from_int(compare_bigint(b1,b2)!=0);
  }

/*** compare greater than ***/
BIGINT *gt_bigint(BIGINT *b1, BIGINT *b2)
  {
  return bigint_from_int(compare_bigint(b1,b2)>0);
  }

/*** compare less than ***/
BIGINT *lt_bigint(BIGINT *b1, BIGINT *b2)
  {
  return bigint_from_int(compare_bigint(b1,b2)<0);
  }

/*** compare greater than or equal ***/
BIGINT *gte_bigint(BIGINT *b1, BIGINT *b2)
  {
  return bigint_from_int(compare_bigint(b1,b2)>=0);
  }

/*** compare less than or equal ***/
BIGINT *lte_bigint(BIGINT *b1, BIGINT *b2)
  {
  return bigint_from_int(compare_bigint(b1,b2)<=0);
  }

/*** shift left (NOTE: shift amount is only 32 bits) ***/
BIGINT *shl_bigint(BIGINT *b1, BIGINT *b2)
  {
  BIGINT *res,*b,*b1o;
  int a,v1 = 0,i2;
  unsigned long long l = 0;

  /*** treat negative numbers as a shr instead ***/
  if (bigint_is_negative(b2))
    {
    b = neg_bigint(b2);
    res = shr_bigint(b1,b);
    free_bigint(b);
    return res;
    }

  /*** shift in 32 bit multiples ***/
  b1 = copy_bigint(b1);
  i2 = int_from_bigint(b2);
  while (i2>=32)
    {
    b = alloc_bigint();
    b->next = b1;
    b1 = b;
    i2 -= 32;
    }
  b1o = b1;

  /*** shift by remaining amount ***/
  a = i2;
  b = res = alloc_bigint();
  while (b1!=NULL)
    {
    v1 = b1->value;
    l = (int_to_ull(v1)<<a) | l;
    b->value = (int) l;
    b1 = b1->next;
    l = (l>>32) & 0xFFFFFFFFL;
    extend_bigint(b);
    b = b->next;
    }

  /*** handle overflow ***/
  v1 = (v1<0) ? -1 : 0;
  b->value = (int_to_ull(v1)<<a) | l;

  /*** canonicalize and return ***/
  free_bigint(b1o);
  canonicalize_bigint(res);
  return res;
  }

/*** shift right (NOTE: shift amount is only 32 bits) ***/
BIGINT *shr_bigint(BIGINT *b1, BIGINT *b2)
  {
  BIGINT *res,*b;
  int a,v,i2;
  unsigned long long l;

  /*** treat negative numbers as a shl instead ***/
  if (bigint_is_negative(b2))
    {
    b = neg_bigint(b2);
    res = shl_bigint(b1,b);
    free_bigint(b);
    return res;
    }

  /*** shift in 32 bit multiples ***/
  i2=int_from_bigint(b2);
  while (i2>=32)
    {
    b1 = b1->next;
    i2 -= 32;
    }

  /*** shift by remaining amount ***/
  a = i2;
  b = res = alloc_bigint();
  while (b1!=NULL)
    {
    /*** get next b1 segment, advance ***/
    if (b1->next!=NULL) v = b1->next->value;
    else v = (b1->value<0) ? -1 : 0;

    /*** shift right ***/
    l = (int_to_ull(v) << 32) | int_to_ull(b1->value);
    l = l>>a;
    b->value = (int) l;

    /*** advance b1,b ***/
    b1 = b1->next;
    if (b1!=NULL) { extend_bigint(b); b = b->next; }
    }

  /*** canonicalize and return ***/
  canonicalize_bigint(res);
  return res;
  }

/*** bitfield insert (NOTE: bit positions are only 32 bits) ***/
BIGINT *bit_ins_bigint(BIGINT *b1, BIGINT *b2, BIGINT *lo, BIGINT *hi)
  {
  int ilo,ihi,len,mask,first,v;
  unsigned long long l=0;
  BIGINT *res, *b2o;

  /*** check for legal bit positions ***/
  if (bigint_is_negative(lo)||bigint_is_negative(hi)||(compare_bigint(lo,hi)>0))
    return copy_bigint(b1);
  ilo = int_from_bigint(lo);
  ihi = int_from_bigint(hi);
  len = ihi-ilo+1;

  /*** skip to first relevant word ***/
  res = b1 = copy_bigint(b1);
  while (ilo>=32)
    {
    if (b1->next==NULL) extend_bigint(b1);
    b1 = b1->next;
    ilo -= 32;
    ihi -= 32;
    }

  /*** start inserting bits of b2 into b1 ***/
  b2o = b2 = copy_bigint(b2);
  first=1;
  while (len>0)
    {
    /*** extend if necessary ***/
    if (b1->next==NULL) extend_bigint(b1);
    if (b2->next==NULL) extend_bigint(b2);

    /*** pick mask ***/
    if (first)
      {
      if (len<32) mask = (~(-1<<len)) << ilo;
      else        mask = -1<<ilo;
      }
    else
      {
      if (len<32) mask = ~(-1<<len);
      else        mask = -1;
      }

    /*** pick new value, adjust l ***/
    l = (int_to_ull(b2->value)<<32) | ((l>>32) & 0xFFFFFFFFL);
    v = (int) (l>>(32-ilo));

    /*** assign b1 ***/
    b1->value = (~mask & b1->value) | (mask & v);
    
    /*** advance len, b1, b2 ***/
    if (first) len = len - (32 - ilo);
    else len -= 32;
    b1 = b1->next;
    b2 = b2->next;
    first = 0;
    }

  /*** result ***/
  free_bigint(b2o);
  canonicalize_bigint(res);
  return res;
  }

/*** bitfield extract (NOTE: bit positions are only 32 bits) ***/
BIGINT *bit_ext_bigint(BIGINT *b1, BIGINT *lo, BIGINT *hi)
  {
  int ilo,ihi,len;
  unsigned long long l;
  BIGINT *res, *b, *b1o;

  /*** check for legal bit positions ***/
  if (bigint_is_negative(lo)||bigint_is_negative(hi)||(compare_bigint(lo,hi)>0))
    return bigint_from_int(0);
  ilo = int_from_bigint(lo);
  ihi = int_from_bigint(hi);
  len = ihi-ilo+1;

  /*** skip to first relevant word ***/
  b1o = b1 = copy_bigint(b1);
  while (ilo>=32)
    {
    if (b1->next==NULL) extend_bigint(b1);
    b1 = b1->next;
    ilo -= 32;
    ihi -= 32;
    }

  /*** start extracting result ***/
  res = b = bigint_from_int(0);
  while (len>0)
    {
    /*** extend if necessary ***/
    if (b1->next==NULL) extend_bigint(b1);

    /*** shift right ***/
    l = (int_to_ull(b1->next->value) << 32) | int_to_ull(b1->value);
    l = l>>ilo;
    b->value = (int) l;
    if (len<32) b->value = b->value & ~(-1<<len);

    /*** advance b1,b,len ***/
    len -= 32;
    if (len>0) { extend_bigint(b); b = b->next; }
    b1 = b1->next;
    }
  
  /*** result ***/
  free_bigint(b1o);
  canonicalize_bigint(res);
  return res;
  }

/*** multiply ***/
BIGINT *mul_bigint(BIGINT *b1, BIGINT *b2)
  {
  int neg=0;
  BIGINT *p1, *p2, *t1, *t2, *e1, *e2, *a, *n32;
  BIGINT *tmp1, *tmp2, *tmp3;

  /*** make arguments positive ***/
  if (bigint_is_negative(b1)) { neg=!neg; t1 = neg_bigint(b1); }
  else t1 = copy_bigint(b1);
  if (bigint_is_negative(b2)) { neg=!neg; t2 = neg_bigint(b2); }
  else t2 = copy_bigint(b2);

  /*** enumerate all terms ***/
  a = bigint_from_int(0);
  e1 = bigint_from_int(0);
  n32 = bigint_from_int(32);
  for (p1=t1; p1!=NULL; p1=p1->next)
    {
    e2 = bigint_from_int(0);
    for (p2=t2; p2!=NULL; p2=p2->next)
      {
      /*** accumulate 64 bit product term ***/
      tmp1 = bigint_from_ull(int_to_ull(p1->value) * int_to_ull(p2->value));
      tmp2 = add_bigint(e1,e2);
      tmp3 = shl_bigint(tmp1,tmp2);
      move_bigint(a,add_bigint(a,tmp3));
      free_bigint(tmp1);
      free_bigint(tmp2);
      free_bigint(tmp3);

      /*** advance e2 ***/
      move_bigint(e2,add_bigint(e2,n32));
      }
    free_bigint(e2);

    /*** advance e1 ***/
    move_bigint(e1,add_bigint(e1,n32));
    }
  free_bigint(e1);

  /*** restore sign ***/
  if (neg) move_bigint(a,neg_bigint(a));

  /*** free and return ***/
  free_bigint(t1);
  free_bigint(t2);
  free_bigint(n32);
  return a;
  }

/*** exponentiate ***/
BIGINT *pow_bigint(BIGINT *b1, BIGINT *b2)
  {
  int negate = 0;
  BIGINT *res,*pow,*t2,*n1,*bit;
  if (bigint_is_negative(b2)) { t2 = neg_bigint(b2); negate=1; }
  else t2 = copy_bigint(b2);
  n1  = bigint_from_int(1);
  res = bigint_from_int(1);
  pow = copy_bigint(b1);
  while (bigint_is_nonzero(t2))
    {
    bit = bit_and_bigint(t2,n1);
    if (bigint_is_nonzero(bit)) move_bigint(res,mul_bigint(res,pow));
    free_bigint(bit);
    move_bigint(t2,shr_bigint(t2,n1));
    move_bigint(pow,mul_bigint(pow,pow));
    }
  if (negate) move_bigint(res,div_bigint(n1,res));
  free_bigint(n1);
  free_bigint(t2);
  free_bigint(pow);
  return res;
  }

/*** return the most significant word of an unsigned bigint ***/
BIGINT *top_word(BIGINT *t)
  {
  while (t->next!=NULL) t=t->next;
  return t;
  }

/*** long division and modulo (TODO: produce more than 1 bit of q at a time!) ***/
void divmod_bigint(BIGINT *n, BIGINT *d, BIGINT **pq, BIGINT **pr)
  {
  BIGINT *q,*r,*t,*s,*n1;
  int neg_n,neg_d;

  /*** check for division by 0 ***/
  if (bigint_is_zero(d))
    {
    if (pq!=NULL) *pq = bigint_from_int(0);
    if (pr!=NULL) *pr = bigint_from_int(0);
    return;
    }

  /*** convert to unsigned operands ***/
  if (bigint_is_negative(n)) {neg_n=1; n = neg_bigint(n);}
  else                       {neg_n=0; n = copy_bigint(n);}
  if (bigint_is_negative(d)) {neg_d=1; d = neg_bigint(d);}
  else                       {neg_d=0; d = copy_bigint(d);}

  /*** start q, r, num_d ***/
  q  = bigint_from_int(0);
  r  = copy_bigint(n);
  n1 = bigint_from_int(1);

  /*** find largest s with r>=(d<<s) ***/
  s = bigint_from_int(0);
  t = copy_bigint(d);
  do
    {
    move_bigint(s,add_bigint(s,n1));
    move_bigint(t,shl_bigint(d,s));
    } while (compare_bigint(r,t)>=0);
  move_bigint(s,sub_bigint(s,n1));

  /*** divide by trial subtraction ***/
  while (compare_bigint(r,d)>=0)
    {
    /*** t=d<<s ***/
    move_bigint(t,shl_bigint(d,s));

    /*** if (r>=t) ***/
    if (compare_bigint(r,t)>=0)
      {
      /*** r=r-d<<s ***/
      move_bigint(t,shl_bigint(d,s));
      move_bigint(r,sub_bigint(r,t));

      /*** q=q+1<<s ***/
      move_bigint(t,shl_bigint(n1,s));
      move_bigint(q,add_bigint(q,t));
      }

    /*** s=s-1 ***/
    move_bigint(s,sub_bigint(s,n1));

#if 0
    /*** debugging ***/
    fprintf(stderr,"d=");
    print_bigint_hex(stderr,d);
    fprintf(stderr," r=");
    print_bigint_hex(stderr,r);
    fprintf(stderr," q=");
    print_bigint_hex(stderr,q);
    fprintf(stderr," s=");
    print_bigint_hex(stderr,s);
    fprintf(stderr,"\n");
#endif
    }

  /*** free ***/
  free_bigint(n);
  free_bigint(d);
  free_bigint(n1);
  free_bigint(s);
  free_bigint(t);

  /*** convert to signed results ***/
  if (neg_n!=neg_d) move_bigint(q,neg_bigint(q));
  if (neg_n) move_bigint(r,neg_bigint(r));

  /*** return results ***/
  if (pq!=NULL) *pq=q;
  else free_bigint(q);
  if (pr!=NULL) *pr=r;
  else free_bigint(r);
  }

/*** divide ***/
BIGINT *div_bigint(BIGINT *b1, BIGINT *b2)
  {
  BIGINT *t;
  divmod_bigint(b1,b2,&t,NULL);
  return t;
  }

/*** modulo ***/
BIGINT *mod_bigint(BIGINT *b1, BIGINT *b2)
  {
  BIGINT *t;
  divmod_bigint(b1,b2,NULL,&t);
  return t;
  }

/*** positive modulo (i.e. between 0 and N-1) ***/
BIGINT *posmod_bigint(BIGINT *b1, BIGINT *b2)
  {
  BIGINT *t;
  if (bigint_is_negative(b2)) return bigint_from_int(0);
  divmod_bigint(b1,b2,NULL,&t);
  if (bigint_is_negative(t)) move_bigint(t,add_bigint(t,b2));
  return t;
  }

/*** absolute value ***/
BIGINT *abs_bigint(BIGINT *b1)
  {
  if (bigint_is_negative(b1)) return neg_bigint(b1);
  else return copy_bigint(b1);
  }

/*** maximum ***/
BIGINT *max_bigint(BIGINT *b1, BIGINT *b2)
  {
  if (compare_bigint(b1,b2)>0) return copy_bigint(b1);
  else                         return copy_bigint(b2);
  }

/*** minimum ***/
BIGINT *min_bigint(BIGINT *b1, BIGINT *b2)
  {
  if (compare_bigint(b1,b2)<0) return copy_bigint(b1);
  else                         return copy_bigint(b2);
  }

/*** random number of given bit length ***/
BIGINT *rand_bigint(BIGINT *b1)
  {
  BIGINT *t1,*t2,*n32,*r;
  if (bigint_is_negative(b1)) return bigint_from_int(0);
  n32 = bigint_from_int(32);
  t1 = copy_bigint(b1);
  r = t2 = bigint_from_int(0);
  while (compare_bigint(t1,n32)>0)
    {
    t2->value=random();
    extend_bigint(t2);
    t2=t2->next;
    move_bigint(t1,sub_bigint(t1,n32));
    }
  t2->value=random()&~(0xFFFFFFFF<<t1->value);
  free_bigint(n32);
  free_bigint(t1);                     
  return r;
  }

/*** string concatenate ***/
BIGINT *strcat_bigint(BIGINT *b1, BIGINT *b2)
  {
  BIGINT *t,*len,*n8;
  if (bigint_is_negative(b1)||bigint_is_negative(b2)) return bigint_from_int(0);
  t    = copy_bigint(b1);
  n8   = bigint_from_int(8);
  len  = bigint_from_int(0);
  while (!bigint_is_negative(t) && bigint_is_nonzero(t))
    {
    move_bigint(len,add_bigint(len,n8));
    move_bigint(t,shr_bigint(t,n8));
    }
  move_bigint(t,shl_bigint(b2,len));
  move_bigint(t,add_bigint(b1,t));
  free_bigint(n8);
  free_bigint(len);
  return t;
  }

/*** convert numerical to hexidecimal string value ***/
BIGINT *hexstr_bigint(BIGINT *b)
  {
  int negate=0;
  BIGINT *str,*n4,*n8,*n15,*t,*ch;
  if (bigint_is_zero(b)) return bigint_from_int('0');
  if (bigint_is_negative(b)) { t = neg_bigint(b); negate = 1; }
  else t = copy_bigint(b); 
  n4  = bigint_from_int(4);
  n8  = bigint_from_int(8);
  n15 = bigint_from_int(15);
  str = bigint_from_int(0);
  while (!bigint_is_negative(t) && bigint_is_nonzero(t))
    {
    ch = bit_and_bigint(t,n15);
    if ((ch->value>=0) && (ch->value<=9)) ch->value += '0';
    else                                  ch->value += 'A' - 10;
    move_bigint(str,shl_bigint(str,n8));
    move_bigint(str,bit_or_bigint(str,ch));
    free_bigint(ch);
    move_bigint(t,shr_bigint(t,n4));
    }
  if (negate)
    {
    move_bigint(str,shl_bigint(str,n8));
    ch = bigint_from_int('-');
    move_bigint(str,bit_or_bigint(str,ch));
    free_bigint(ch);
    }
  free_bigint(n4);
  free_bigint(n8);
  free_bigint(n15);
  free_bigint(t);
  return str;
  }

/*** convert numerical to decimal string value ***/
BIGINT *decstr_bigint(BIGINT *b)
  {
  int negate=0;
  BIGINT *t,*n8,*n10,*ch,*str;
  if (bigint_is_zero(b)) return bigint_from_int('0');
  if (bigint_is_negative(b)) { t = neg_bigint(b); negate = 1; }
  else t = copy_bigint(b);
  n8 = bigint_from_int(8);
  n10 = bigint_from_int(10);
  str = bigint_from_int(0);
  while (!bigint_is_negative(t) && bigint_is_nonzero(t))
    {
    ch = mod_bigint(t,n10);
    ch->value += '0';
    move_bigint(str,shl_bigint(str,n8));
    move_bigint(str,bit_or_bigint(str,ch));
    free_bigint(ch);
    move_bigint(t,div_bigint(t,n10));
    }
  if (negate)
    {
    move_bigint(str,shl_bigint(str,n8));
    ch = bigint_from_int('-');
    move_bigint(str,bit_or_bigint(str,ch));
    free_bigint(ch);
    }
  free_bigint(n8);
  free_bigint(n10);
  free_bigint(t);
  return str;
  }

/********************** parse BIGINT's ***********************/

/*** parse a hex string into a BIGINT ***/
BIGINT *parse_bigint_hex(char *str)
  {
  char ch;
  int len,v,i,negate = 0;
  BIGINT *b,*t1,*t2,*t3,*n4;
  if (str[0]=='-') {negate = 1; str++;}
  len = strlen(str);
  b = bigint_from_int(0);
  n4 = bigint_from_int(4);
  for (i=0; i<len; i++)
    {
    /*** convert character ***/
    ch = str[i];
    if      ((ch>='0') && (ch<='9')) v = ch - '0';
    else if ((ch>='a') && (ch<='f')) v = ch - 'a' + 10;
    else if ((ch>='A') && (ch<='F')) v = ch - 'A' + 10;
    else v = 0;

    /*** accumulate onto b ***/
    t1 = shl_bigint(b,n4);
    t2 = bigint_from_int(v);
    t3 = add_bigint(t1,t2);
    free_bigint(b);
    free_bigint(t1);
    free_bigint(t2);
    b = t3;
    }

  /*** negate if minus ***/
  if (negate)
    {
    t1 = neg_bigint(b);
    free_bigint(b);
    b = t1;
    }

  /*** free and return ***/
  free_bigint(n4);
  return b;
  }

/*** store a string in a BIGINT ***/
BIGINT *parse_bigint_str(char *str)
  {
  int len,v,i;
  BIGINT *b,*t1,*t2,*t3,*n8;
  len = strlen(str);
  b = bigint_from_int(0);
  n8 = bigint_from_int(8);
  for (i=len-1; i>=0; i--)
    {
    /*** convert character ***/
    v = ((char) str[i]) & 0xFF;

    /*** accumulate onto b ***/
    t1 = shl_bigint(b,n8);
    t2 = bigint_from_int(v);
    t3 = add_bigint(t1,t2);
    free_bigint(b);
    free_bigint(t1);
    free_bigint(t2);
    b = t3;
    }

  /*** free and return ***/
  free_bigint(n8);
  return b;
  }

/*** parse a decimal string into a BIGINT ***/
BIGINT *parse_bigint_dec(char *str)
  {
  char ch;
  int v,negate=0;
  BIGINT *ten,*b,*digit;
  if (str[0]=='-') {negate = 1; str++;}
  ten = bigint_from_int(10);
  b = bigint_from_int(0);
  while (*str!=0)
    {
    move_bigint(b,mul_bigint(b,ten));
    ch = *str;
    if ((ch>='0') && (ch<='9')) v = ch - '0';
    else v = 0;
    digit = bigint_from_int(v);
    move_bigint(b,add_bigint(b,digit));
    free_bigint(digit);
    str++;
    }

  /*** negate if minus ***/
  if (negate) move_bigint(b,neg_bigint(b));
  free_bigint(ten);
  return b;
  }

/*** parse in decimal or hex ***/
BIGINT *parse_bigint(char *str)
  {
  int negate=0;
  BIGINT *t;
  if (str[0]=='-') {negate=1; str++;}
  if ((str[0]=='0') && (str[1]=='x')) t = parse_bigint_hex(str+2);
  else                                t = parse_bigint_dec(str);
  if (negate) move_bigint(t,neg_bigint(t));
  return t;
  }

/************************* print BIGINT's *******************************/

/*** print an ASCII string from a BIGINT ***/
void print_bigint_str(FILE *fout, BIGINT *b)
  {
  char ch;
  BIGINT *t,*n8,*n255,*tmp;
  t = copy_bigint(b);
  n8 = bigint_from_int(8);
  n255 = bigint_from_int(255);
  while (!bigint_is_negative(t) && bigint_is_nonzero(t))
    {
    tmp = bit_and_bigint(t,n255);
    ch = (char) tmp->value;
    fprintf(fout,"%c",isprint(ch) ? ch : ' ');
    free_bigint(tmp);
    tmp = shr_bigint(t,n8);
    free_bigint(t);
    t = tmp;
    }
  free_bigint(t);
  free_bigint(n8);
  free_bigint(n255);
  }

/*** print an ASCII string from a BIGINT ***/
char *sprint_bigint_str(char *str, BIGINT *b)
  {
  char ch;
  int i,len;
  BIGINT *t,*n8,*n255,*tmp;
  for (t=b, len=0; t==NULL; t=t->next) len+=4;
  if (str==NULL) str = (char *) leak_malloc(len+1);
  else if (len>=STRMAX) {str[0]=0; return str;}
  t = copy_bigint(b);
  n8 = bigint_from_int(8);
  n255 = bigint_from_int(255);
  i=0;
  while (!bigint_is_negative(t) && bigint_is_nonzero(t))
    {
    tmp = bit_and_bigint(t,n255);
    ch = (char) tmp->value;
    str[i++] = ch;
    free_bigint(tmp);
    tmp = shr_bigint(t,n8);
    free_bigint(t);
    t = tmp;
    }
  str[i]=0;
  free_bigint(t);
  free_bigint(n8);
  free_bigint(n255);
  return str;
  }

/*** print a signed BIGINT in hex ***/
void print_bigint_hex(FILE *fout, BIGINT *b)
  {
  BIGINT *t;
  t = hexstr_bigint(b);
  print_bigint_str(fout,t);
  free_bigint(t);
  }

/*** print a signed BIGINT in decimal ***/
void print_bigint_dec(FILE *fout, BIGINT *b)
  {
  BIGINT *t;
  t = decstr_bigint(b);
  print_bigint_str(fout,t);
  free_bigint(t);
  }

/************************ BIGINT expressions ****************************/

/*** prototype ***/
void parse_bigint_expression_main(LEX *lex, LIST *expression, int precedence,
                                  char *parse_var(LEX *));

/*** append an operator to the RPN expression ***/
void append_bigint_operator(LIST *expression, int type)
  {
  BIOP op;
  op.type = type;
  op.value = NULL;
  list_append_element(expression,&op);
  }

/*** append a numerical constant to the RPN expression ***/
void append_bigint_constant(LIST *expression, char *str)
  {
  BIOP op;
  op.type = BIOP_CONST;
  op.value = parse_bigint(str);
  list_append_element(expression,&op);
  }

/*** append a string constant to the RPN expression ***/
void append_bigint_string(LIST *expression, char *str)
  {
  BIOP op;
  op.type = BIOP_STR;
  op.value = parse_bigint_str(str);
  list_append_element(expression,&op);
  }

/*** append a variable (as a string) to the RPN expression ***/
void append_bigint_variable(LIST *expression, char *str)
  {
  BIOP op;
  op.type = BIOP_VARSTR;
  op.value = parse_bigint_str(str);
  list_append_element(expression,&op);
  }

/*** append a user function (as a string) to the RPN expression ***/
void append_bigint_user(LIST *expression, char *str)
  {
  BIOP op;
  op.type = BIOP_USRSTR;
  op.value = parse_bigint_str(str);
  list_append_element(expression,&op);
  }

/*** parse a single item of an expression ***/
void parse_bigint_expression_item(LEX *lex, LIST *expression,
                                  char *parse_var(LEX *))
  {
  if (lex_is_integer(lex)) /* integer constant */
    {
    lex_push_position(lex);
    lex_do_integer(lex);
    append_bigint_constant(expression, lex_get_token(lex));
    lex_pop_position(lex);
    }
  else if (lex_is_quote(lex)) /* string constant */
    {
    append_bigint_string(expression, lex_eat_quote(lex));
    }
  else if (lex_eatif_sym(lex,"("))
    {
    parse_bigint_expression_main(lex,expression,0,parse_var);
    lex_eatif_sym(lex,")");
    }
  else if (lex_eatif_sym(lex,"~"))
    {
    parse_bigint_expression_item(lex,expression,parse_var);
    append_bigint_operator(expression,BIOP_BIT_NOT);
    }
  else if (lex_eatif_sym(lex,"!"))
    {
    parse_bigint_expression_item(lex,expression,parse_var);
    append_bigint_operator(expression,BIOP_BOOL_NOT);
    }
  else if (lex_eatif_sym(lex,"-"))
    {
    parse_bigint_expression_item(lex,expression,parse_var);
    append_bigint_operator(expression,BIOP_NEG);
    }
  else if (lex_eatif_sym(lex,"+"))
    {
    parse_bigint_expression_item(lex,expression,parse_var);
    }
  else if (lex_eatif_sym(lex,"ABS"))
    {
    lex_eatif_sym(lex,"(");
    parse_bigint_expression_main(lex,expression,0,parse_var);
    lex_eatif_sym(lex,")");
    append_bigint_operator(expression,BIOP_ABS);
    }
  else if (lex_eatif_sym(lex,"HEX"))
    {
    lex_eatif_sym(lex,"(");
    parse_bigint_expression_main(lex,expression,0,parse_var);
    lex_eatif_sym(lex,")");
    append_bigint_operator(expression,BIOP_HEXSTR);
    }
  else if (lex_eatif_sym(lex,"DEC"))
    {
    lex_eatif_sym(lex,"(");
    parse_bigint_expression_main(lex,expression,0,parse_var);
    lex_eatif_sym(lex,")");
    append_bigint_operator(expression,BIOP_DECSTR);
    }
  else if (lex_eatif_sym(lex,"MIN"))
    {
    lex_eatif_sym(lex,"(");
    parse_bigint_expression_main(lex,expression,0,parse_var);
    lex_eatif_sym(lex,",");
    parse_bigint_expression_main(lex,expression,0,parse_var);
    lex_eatif_sym(lex,")");
    append_bigint_operator(expression,BIOP_MIN);
    }
  else if (lex_eatif_sym(lex,"MAX"))
    {
    lex_eat_sym(lex,"(");
    parse_bigint_expression_main(lex,expression,0,parse_var);
    lex_eatif_sym(lex,",");
    parse_bigint_expression_main(lex,expression,0,parse_var);
    lex_eatif_sym(lex,")");
    append_bigint_operator(expression,BIOP_MAX);
    }
  else if (lex_eatif_sym(lex,"POSMOD"))
    {
    lex_eat_sym(lex,"(");
    parse_bigint_expression_main(lex,expression,0,parse_var);
    lex_eatif_sym(lex,",");
    parse_bigint_expression_main(lex,expression,0,parse_var);
    lex_eatif_sym(lex,")");
    append_bigint_operator(expression,BIOP_POSMOD);
    }
  else if (lex_eatif_sym(lex,"RAND"))
    {
    lex_eatif_sym(lex,"(");
    parse_bigint_expression_main(lex,expression,0,parse_var);
    lex_eatif_sym(lex,")");
    append_bigint_operator(expression,BIOP_RAND);
    }
  else if (lex_eatif_sym(lex,"INS"))
    {
    lex_eatif_sym(lex,"(");
    parse_bigint_expression_main(lex,expression,0,parse_var);
    lex_eatif_sym(lex,",");
    parse_bigint_expression_main(lex,expression,0,parse_var);
    lex_eatif_sym(lex,",");
    parse_bigint_expression_main(lex,expression,0,parse_var);
    lex_eatif_sym(lex,",");
    parse_bigint_expression_main(lex,expression,0,parse_var);
    lex_eatif_sym(lex,")");
    append_bigint_operator(expression,BIOP_BIT_INS);
    }
  else if (lex_eatif_sym(lex,"EXT"))
    {
    lex_eatif_sym(lex,"(");
    parse_bigint_expression_main(lex,expression,0,parse_var);
    lex_eatif_sym(lex,",");
    parse_bigint_expression_main(lex,expression,0,parse_var);
    lex_eatif_sym(lex,",");
    parse_bigint_expression_main(lex,expression,0,parse_var);
    lex_eatif_sym(lex,")");
    append_bigint_operator(expression,BIOP_BIT_EXT);
    }
  else /* variable or user function (stored as a string) */
    {
    char *str;
    str=parse_var(lex); // user provided parsing function
    if (!lex_is_whitespace(lex) && lex_eatif_sym(lex,"("))
      {
      while (!lex_eatif_sym(lex,")"))
        {
        parse_bigint_expression_main(lex,expression,0,parse_var);
        lex_eatif_sym(lex,",");
        }
      append_bigint_user(expression,str);
      }
    else append_bigint_variable(expression, str);
    leak_free(str);
    }
  }
  
/*** core parse infix expressions with precedence ***/
void parse_bigint_expression_main(LEX *lex, LIST *expression, int precedence,
                                  char *parse_var(LEX *))
  { 
  parse_bigint_expression_item(lex,expression,parse_var);
  while (!lex_is_eof(lex) && !lex_is_sym(lex,"->"))
    {
    if ((precedence<8)&&lex_eatif_sym(lex,"**"))
      {
      parse_bigint_expression_main(lex,expression,8,parse_var);
      append_bigint_operator(expression,BIOP_POW);
      }
    else if ((precedence<7)&&!lex_is_sym(lex,"**")&&lex_eatif_sym(lex,"*"))
      {
      parse_bigint_expression_main(lex,expression,7,parse_var);
      append_bigint_operator(expression,BIOP_MUL);
      }
    else if ((precedence<7)&&lex_eatif_sym(lex,"/"))
      {
      parse_bigint_expression_main(lex,expression,7,parse_var);
      append_bigint_operator(expression,BIOP_DIV);
      }
    else if ((precedence<7)&&lex_eatif_sym(lex,"%"))
      {
      parse_bigint_expression_main(lex,expression,7,parse_var);
      append_bigint_operator(expression,BIOP_MOD);
      }
    else if ((precedence<7)&&lex_eatif_sym(lex,"<<"))
      {
      parse_bigint_expression_main(lex,expression,7,parse_var);
      append_bigint_operator(expression,BIOP_SHL);
      }
    else if ((precedence<7)&&lex_eatif_sym(lex,">>"))
      {
      parse_bigint_expression_main(lex,expression,7,parse_var);
      append_bigint_operator(expression,BIOP_SHR);
      }
    else if ((precedence<6)&&!lex_is_sym(lex,"++")&&lex_eatif_sym(lex,"+"))
      {
      parse_bigint_expression_main(lex,expression,6,parse_var);
      append_bigint_operator(expression,BIOP_ADD);
      }
    else if ((precedence<6)&&lex_eatif_sym(lex,"-"))
      {
      parse_bigint_expression_main(lex,expression,6,parse_var);
      append_bigint_operator(expression,BIOP_SUB);
      }
    else if ((precedence<5)&&lex_eatif_sym(lex,"<="))
      {
      parse_bigint_expression_main(lex,expression,5,parse_var);
      append_bigint_operator(expression,BIOP_LTE);
      }
    else if ((precedence<5)&&lex_eatif_sym(lex,">="))
      {
      parse_bigint_expression_main(lex,expression,5,parse_var);
      append_bigint_operator(expression,BIOP_GTE);
      }
    else if ((precedence<5)&&lex_eatif_sym(lex,"<"))
      {
      parse_bigint_expression_main(lex,expression,5,parse_var);
      append_bigint_operator(expression,BIOP_LT);
      }
    else if ((precedence<5)&&lex_eatif_sym(lex,">"))
      {
      parse_bigint_expression_main(lex,expression,5,parse_var);
      append_bigint_operator(expression,BIOP_GT);
      }
    else if ((precedence<4)&&lex_eatif_sym(lex,"=="))
      {
      parse_bigint_expression_main(lex,expression,4,parse_var);
      append_bigint_operator(expression,BIOP_E);
      }
    else if ((precedence<4)&&lex_eatif_sym(lex,"!="))
      {
      parse_bigint_expression_main(lex,expression,4,parse_var);
      append_bigint_operator(expression,BIOP_NE);
      }
    else if ((precedence<3)&&lex_eatif_sym(lex,"&&"))
      {
      parse_bigint_expression_main(lex,expression,3,parse_var);
      append_bigint_operator(expression,BIOP_BOOL_AND);
      }
    else if ((precedence<3)&&lex_eatif_sym(lex,"&"))
      {
      parse_bigint_expression_main(lex,expression,3,parse_var);
      append_bigint_operator(expression,BIOP_BIT_AND);
      }
    else if ((precedence<3)&&lex_eatif_sym(lex,"^"))
      {
      parse_bigint_expression_main(lex,expression,3,parse_var);
      append_bigint_operator(expression,BIOP_BIT_XOR);
      }
    else if ((precedence<2)&&lex_eatif_sym(lex,"||"))
      {
      parse_bigint_expression_main(lex,expression,2,parse_var);
      append_bigint_operator(expression,BIOP_BOOL_OR);
      }
    else if ((precedence<2)&&lex_eatif_sym(lex,"|"))
      {
      parse_bigint_expression_main(lex,expression,2,parse_var);
      append_bigint_operator(expression,BIOP_BIT_OR);
      }
    else if ((precedence<1)&&lex_eatif_sym(lex,"++"))
      {
      parse_bigint_expression_main(lex,expression,1,parse_var);
      append_bigint_operator(expression,BIOP_STRCAT);
      }
    else if ((precedence<1)&&lex_eatif_sym(lex,"?"))
      {
      parse_bigint_expression_main(lex,expression,1,parse_var);
      lex_eatif_sym(lex,":");
      parse_bigint_expression_main(lex,expression,1,parse_var);
      append_bigint_operator(expression,BIOP_IF);
      }
    else break;
    }
  }

/*** Parse infix BIGINT expression.  Store variables as strings. ***/
LIST *parse_bigint_expression(LEX *lex,
                              char *parse_var(LEX *))
  {
  LIST *expression;
  expression=list_create(sizeof(BIOP));
  parse_bigint_expression_main(lex,expression,0,parse_var);
  return expression;
  }

/*** free a bigint expression ***/
void free_bigint_expression(LIST *expression,
                            int freeVAR, int freeVARSTR,
                            int freeUSR, int freeUSRSTR,
                            int freeCONST, int freeSTR)
  {
  int i;
  BIOP *op;
  for (i=0; i<expression->max; i++)
    {
    op = &expression->p.op[i];
    if ((freeVAR    && (op->type==BIOP_VAR))    ||
        (freeVARSTR && (op->type==BIOP_VARSTR)) ||
        (freeUSR    && (op->type>=BIOP_USR))    ||
        (freeUSRSTR && (op->type==BIOP_USRSTR)) ||
        (freeCONST  && (op->type==BIOP_CONST))  ||
        (freeSTR    && (op->type==BIOP_STR)))
      free_bigint(op->value);
    }
  list_free(expression);
  }

/*** print RPN expression for debugging purposes ***/
void print_bigint_expression(FILE *fout, LIST *expression)
  {
  int j;
  BIOP op;
  for (j=0; j<expression->max; j++)
    {
    op=expression->p.op[j];
    if (j>0) fprintf(fout," ");
    if (op.type>=BIOP_USR) print_bigint_str(fout,op.value);
    else switch(op.type)
      {
    case BIOP_CONST:
    case BIOP_VAR:    print_bigint_hex(fout,op.value); break;

    case BIOP_STR:
    case BIOP_VARSTR:
    case BIOP_USRSTR: print_bigint_str(fout,op.value); break;

    case BIOP_E:     fprintf(fout,"=="); break;
    case BIOP_NE:    fprintf(fout,"!="); break;
    case BIOP_GTE:   fprintf(fout,">="); break;
    case BIOP_GT:    fprintf(fout,">"); break;
    case BIOP_LTE:   fprintf(fout,"<="); break;
    case BIOP_LT:    fprintf(fout,"<"); break;

    case BIOP_BOOL_AND: fprintf(fout,"&&"); break;
    case BIOP_BOOL_OR:  fprintf(fout,"||"); break;
    case BIOP_BOOL_NOT: fprintf(fout,"!"); break;

    case BIOP_BIT_AND:  fprintf(fout,"&"); break;
    case BIOP_BIT_OR:   fprintf(fout,"|"); break;
    case BIOP_BIT_NOT:  fprintf(fout,"~"); break;
    case BIOP_BIT_XOR:  fprintf(fout,"^"); break;
    case BIOP_SHL:      fprintf(fout,"<<"); break;
    case BIOP_SHR:      fprintf(fout,">>"); break;
    case BIOP_BIT_INS:  fprintf(fout,"INS"); break;
    case BIOP_BIT_EXT:  fprintf(fout,"EXT"); break;

    case BIOP_ADD:    fprintf(fout,"+"); break;
    case BIOP_SUB:    fprintf(fout,"-"); break;
    case BIOP_NEG:    fprintf(fout,"NEG"); break;
    case BIOP_MUL:    fprintf(fout,"*"); break;
    case BIOP_POW:    fprintf(fout,"**"); break;
    case BIOP_DIV:    fprintf(fout,"/"); break;
    case BIOP_MOD:    fprintf(fout,"%%"); break;
    case BIOP_POSMOD: fprintf(fout,"POSMOD"); break;

    case BIOP_ABS:   fprintf(fout,"ABS"); break;
    case BIOP_MIN:   fprintf(fout,"MIN"); break;
    case BIOP_MAX:   fprintf(fout,"MAX"); break;
    case BIOP_RAND:  fprintf(fout,"RAND"); break;
    case BIOP_IF:    fprintf(fout,"?");   break;

    case BIOP_STRCAT: fprintf(fout,"++"); break;
    case BIOP_HEXSTR: fprintf(fout,"HEX"); break;
    case BIOP_DECSTR: fprintf(fout,"DEC"); break;

    default:         assert(0);
      }
    }
  }

/*** try to auto-detect if a bigint expression evaluates to a string ***/
int bigint_expression_is_string(LIST *expression)
  {
  int type;
  if (expression->max<1) return 0;
  type=expression->p.op[expression->max-1].type;
  return (type==BIOP_STR    || type==BIOP_STRCAT || 
          type==BIOP_HEXSTR || type==BIOP_DECSTR);
  }

/*** evaluate an expression ***/
BIGINT *evaluate_bigint_expression(LIST *expression, void *data,
                                   void eval_user(int type, int *tos,
                                                  BIGINT **s, void *data))
  {
  BIGINT *s[STACKMAX];
  int pad=4; // maximum number of arguments needed by any operator
  int tos=0,j;
  BIOP op;

  // process expression list
  for (j=0; j<expression->max; j++)
    {
    while (tos<pad) s[tos++]=bigint_from_int(0); // pad with zeros
    if (tos>=STACKMAX) error("bigint stack overflow");
    op=expression->p.op[j];
    if (op.type>=BIOP_USR) eval_user(op.type,&tos,s,data);
    else switch (op.type)
      {
    case BIOP_VAR:
    case BIOP_VARSTR:
    case BIOP_CONST:
    case BIOP_STR:
    case BIOP_USRSTR: s[tos++]=copy_bigint(op.value); break;

    case BIOP_E:     move_bigint(s[tos-2],eq_bigint(s[tos-2],s[tos-1]));
                     free_bigint(s[tos-1]); tos--; break;
    case BIOP_NE:    move_bigint(s[tos-2],neq_bigint(s[tos-2],s[tos-1]));
                     free_bigint(s[tos-1]); tos--; break;
    case BIOP_GTE:   move_bigint(s[tos-2],gte_bigint(s[tos-2],s[tos-1]));
                     free_bigint(s[tos-1]); tos--; break;
    case BIOP_GT:    move_bigint(s[tos-2],gt_bigint(s[tos-2],s[tos-1]));
                     free_bigint(s[tos-1]); tos--; break;
    case BIOP_LTE:   move_bigint(s[tos-2],lte_bigint(s[tos-2],s[tos-1]));
                     free_bigint(s[tos-1]); tos--; break;
    case BIOP_LT:    move_bigint(s[tos-2],lt_bigint(s[tos-2],s[tos-1]));
                     free_bigint(s[tos-1]); tos--; break;

    case BIOP_BOOL_AND:   move_bigint(s[tos-2],bool_and_bigint(s[tos-2],s[tos-1]));
                          free_bigint(s[tos-1]); tos--; break;
    case BIOP_BOOL_OR:    move_bigint(s[tos-2],bool_or_bigint(s[tos-2],s[tos-1]));
                          free_bigint(s[tos-1]); tos--; break;
    case BIOP_BOOL_NOT:   move_bigint(s[tos-1],bool_not_bigint(s[tos-1])); break;


    case BIOP_BIT_AND:   move_bigint(s[tos-2],bit_and_bigint(s[tos-2],s[tos-1]));
                         free_bigint(s[tos-1]); tos--; break;
    case BIOP_BIT_OR:    move_bigint(s[tos-2],bit_or_bigint(s[tos-2],s[tos-1]));
                         free_bigint(s[tos-1]); tos--; break;
    case BIOP_BIT_XOR:   move_bigint(s[tos-2],bit_xor_bigint(s[tos-2],s[tos-1]));
                         free_bigint(s[tos-1]); tos--; break;
    case BIOP_BIT_NOT:   move_bigint(s[tos-1],bit_not_bigint(s[tos-1])); break;
    case BIOP_SHL:       move_bigint(s[tos-2],shl_bigint(s[tos-2],s[tos-1]));
                         free_bigint(s[tos-1]); tos--; break;
    case BIOP_SHR:       move_bigint(s[tos-2],shr_bigint(s[tos-2],s[tos-1]));
                         free_bigint(s[tos-1]); tos--; break;
    case BIOP_BIT_INS:   move_bigint(s[tos-4],bit_ins_bigint(s[tos-4],s[tos-3],
                                                             s[tos-2],s[tos-1]));
                         free_bigint(s[tos-3]);
                         free_bigint(s[tos-2]);
                         free_bigint(s[tos-1]);
                         tos-=3;
                         break;
    case BIOP_BIT_EXT:   move_bigint(s[tos-3],bit_ext_bigint(s[tos-3],
                                                            s[tos-2],s[tos-1]));
                         free_bigint(s[tos-2]);
                         free_bigint(s[tos-1]);
                         tos-=2;
                         break;

    case BIOP_ADD:   move_bigint(s[tos-2],add_bigint(s[tos-2],s[tos-1]));
                     free_bigint(s[tos-1]); tos--; break;
    case BIOP_SUB:   move_bigint(s[tos-2],sub_bigint(s[tos-2],s[tos-1]));
                     free_bigint(s[tos-1]); tos--; break;
    case BIOP_NEG:   move_bigint(s[tos-1],neg_bigint(s[tos-1])); break;
    case BIOP_MUL:   move_bigint(s[tos-2],mul_bigint(s[tos-2],s[tos-1]));
                     free_bigint(s[tos-1]); tos--; break;
    case BIOP_POW:   move_bigint(s[tos-2],pow_bigint(s[tos-2],s[tos-1]));
                     free_bigint(s[tos-1]); tos--; break;
    case BIOP_DIV:   move_bigint(s[tos-2],div_bigint(s[tos-2],s[tos-1]));
                     free_bigint(s[tos-1]); tos--; break;
    case BIOP_MOD:   move_bigint(s[tos-2],mod_bigint(s[tos-2],s[tos-1]));
                     free_bigint(s[tos-1]); tos--; break;
    case BIOP_POSMOD: move_bigint(s[tos-2],posmod_bigint(s[tos-2],s[tos-1]));
                      free_bigint(s[tos-1]); tos--; break;

    case BIOP_ABS:   move_bigint(s[tos-1],abs_bigint(s[tos-1])); break;
    case BIOP_MAX:   move_bigint(s[tos-2],max_bigint(s[tos-2],s[tos-1]));
                     free_bigint(s[tos-1]); tos--; break;
    case BIOP_MIN:   move_bigint(s[tos-2],min_bigint(s[tos-2],s[tos-1]));
                     free_bigint(s[tos-1]); tos--; break;
    case BIOP_RAND:  move_bigint(s[tos-1],rand_bigint(s[tos-1])); break;
    case BIOP_IF:    if (bigint_is_nonzero(s[tos-3])) assign_bigint(s[tos-3],s[tos-2]);
                     else assign_bigint(s[tos-3],s[tos-1]);
                     free_bigint(s[tos-2]); free_bigint(s[tos-1]); tos-=2; break;

    case BIOP_STRCAT: move_bigint(s[tos-2],strcat_bigint(s[tos-2],s[tos-1]));
                      free_bigint(s[tos-1]); tos--; break;
    case BIOP_HEXSTR: move_bigint(s[tos-1],hexstr_bigint(s[tos-1])); break;
    case BIOP_DECSTR: move_bigint(s[tos-1],decstr_bigint(s[tos-1])); break;

    default:       assert(0);
      }
    }
  
  // free and return
  for (j=0; j<tos; j++) if (j!=tos-1) free_bigint(s[j]);
  return s[tos-1];
  }
