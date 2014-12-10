// "$Id: //depot/user/aubrey/src/lib/libaag/btutil.h#1 $ AAG";
#ifndef __btutil_h__
#define __btutil_h__ 1

/***********************************************************************
*
*                            B T U T I L
*                            ===========
* Purpose:
*    To perform operations on Balanced Binary Search Trees.
*
* Functions:
*    btinit(t, keysiz, infsiz, cmpfunc) - Initializes tree [t].
*    btloc(key, t) - Attempts to locate a node with [key] in tree [t].
*    btins(key, t) - Inserts a node containning [key] in tree [t].
*    btdel(key, t) - Deletes a node containning [key] from tree [t].
*    btselect(minkey, maxkey, t) - Select nodes in [t] (minkey<=key<=maxkey).
*    btget(key, info, t) - Get next selected [key]/[info] from tree [t].
*    btfree(t) - Disposes of memory allocated by tree [t].
*    btppre(fp,  prtfunc, t) - prints contents of [t] to file [fp].(preorder)
*    btpin(fp,   prtfunc, t) - prints contents of [t] to file [fp].(inorder)
*    btppost(fp, prtfunc, t) - prints contents of [t] to file [fp].(postorder)
*    btptree(fp, prtfunc, t) - prints contents of [t] to file [fp].(treeshape)
*    btnodecnt(root_btnp, cnt_p) - counts nodes in [root_btnp], puts count
*                                  in [cnt_p].
*
* Notes:
*    Balanced Binary Search Trees offer excellent run time performance.
*    O(log n) worst case for btloc, btins, and btdel operations.
*
*    Call btinit once in the {ning, before you call any of the others.
*    Call btselect before the first call to btget.  A successful btdel will
*    automatically de-select the deleted node (if it was selected).  A
*    successful btins will not automatically be selected, even if the key
*    is within minkey,maxkey.
*
*    Balancing Binary Trees by Internal Path Reduction.
*    Gaston H. Gonnet            University of Waterloo
*    Communications of the ACM   December 1983 Vol.26 #12
*
*    Handbook of Algorithms and Data Structures
*    Gaston H. Gonnet
*    International Computer Science Series
*    Addison-Wesley Publishers Limited    1984
*
*    A key and the info associated with a key are user defined.  So the
*    user must also supply the functions which operate on them.  The two
*    functions required by this module are a comparision function and
*    a print function.  The comparision function should accept two key
*    parameters and return an INT16 indicating if key1 was less than(-1),
*    equal to(0), or greater than(+1) key2.  The print function should
*    accept three parameters, the first one being an opened FILE, the
*    second a pointer to a key, and the third a pointer to the info.
*
***********************************************************************/

#include <stdio.h>

#include "c_std.h"

#if defined(__cplusplus)
#define CMP_FUNC_ARGS void *,void *
#else
#define CMP_FUNC_ARGS
#endif

/* node structure to hold a key with info in a binary tree */
typedef
   struct sbtnode
      {
      int weight;
      struct sbtnode *left;
      struct sbtnode *right;
      void * keyptr;
      void * infptr;
      }
   BTNODE, *BTNODEPTR;

/* linked list structure of selected nodes */
typedef
   struct sbtselrec
      {
      BTNODEPTR node;
      struct sbtselrec *next;
      }
   BTSELREC,*BTSELPTR;

/* tree with the root, selected nodes, compare function, and key/info sizes */
typedef
   struct sbttree
      {
      BTNODEPTR root;
      BTSELPTR selected;
      int (*cmpfunc)(CMP_FUNC_ARGS);
      int sizkey;  /* in bytes */
      int sizinf;  /* in bytes */
      }
   BTTREE;

/* Functions */


#ifdef __cplusplus
extern "C" {
#endif
void btinit(BTTREE *t,int keysiz,int infsiz, int(*cmpfunc)(CMP_FUNC_ARGS));

void * btloc(void * key,BTTREE *t);
void * btins(void * key, BTTREE *t);
BOOL btdel(void * key,BTTREE *t);

void btisel(BTNODEPTR node,void * minkey,void * maxkey, BTSELPTR *selected);

int btselect(void * minkey,void * maxkey,BTTREE *t);
BOOL btget(void * key,void * *info,BTTREE *t);
BOOL btfget(void * *key,void * *info,BTTREE *t);
void btfree(BTTREE *t);

void btppre(FILE *fp, void (*putln)(), BTTREE *t);

void btpin(FILE *fp, void (*putln)(), BTTREE *t);

void btppost(FILE *fp, void (*putln)(), BTTREE *node);

void btptree(FILE *fp, void (*putln)(), BTTREE *t);

void btnodecnt(BTNODE *node_btnp, int *cnt_p);
#ifdef __cplusplus
}
#endif

#endif

