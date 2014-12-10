// "$Id$ AAG";

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
#include <stdlib.h>
#include <string.h>
#include "c_std.h"
#include "c_lib.h"
#include "btutil.h"

/* Module variables */
MLOCAL BTNODEPTR dnode;    /* BTDEL, BTIDEL */
MLOCAL int numof;          /* BTSELECT, BTISEL */
MLOCAL int (*btcmp)();     /* BTINS, BTDEL, BTSELECT */
MLOCAL int btsizkey;       /* BTINS */
MLOCAL int btsizinf;       /* BTINS */
MLOCAL void  (*btputln)(); /* BTPPRE, BTPIN, BTPPOST, BTPTREE */


/***
*    BTIMEMERR
*   -----------
* Write an error message to stderr informming the user of memory problems.
***/
void btimemerr(char *modulename)

   {   /* btimemerr */

   fprintf(stderr, 
      "***  Error: (btimemerr) Ran out of user Memory in module (%s)  ***\n",
      modulename);
   exit(1);

   }   /* btimemerr */


/***
*    BTIWT
*   ------
* Return weight of [node].
***/
int btiwt(BTNODEPTR node)

   {  /* btiwt */

   if (node == NULL)
      {
      return(1);
      }
   else
      {
      return(node->weight);
      }

   }    /* btiwt */


/***
*    BTILROT
*   --------
* Perform a single left rotation.
***/
void btilrot(BTNODEPTR *node)

   {  /* btilrot */

   /* variables */
   BTNODEPTR temp;

   temp = *node;
   *node = (*node)->right;
   temp->right = (*node)->left;
   (*node)->left = temp;
   /* adjust weight */
   (*node)->weight = temp->weight;
   temp->weight = btiwt(temp->left) + btiwt(temp->right);

   }    /* btilrot */


/***
*    BTIRROT
*   --------
* Perform a single right rotation.
***/
void btirrot(BTNODEPTR *node)

   {  /* btirrot */

   /* variables */
   BTNODEPTR temp;

   temp = *node;
   *node = (*node)->left;
   temp->left = (*node)->right;
   (*node)->right = temp;

   /* adjust weight */
   (*node)->weight = temp->weight;
   temp->weight = btiwt(temp->left) + btiwt(temp->right);

   }    /* btirrot */


/***
*    BTICHKRT
*   ----------
* Check need for rotations, perform if needed.
***/
void btichkrt(BTNODEPTR *node)

   {  /* btichkrt */

   /* variables */
   int wl;
   int wr;

   if (*node != NULL)
      {
      wl = btiwt((*node)->left);
      wr = btiwt((*node)->right);
      if (wr > wl)
         {  /* left rotation needed */
         if (btiwt((*node)->right->right) > wl)
            {  /* single */
            btilrot(node);
            btichkrt(&((*node)->left));
            }
         else if (btiwt((*node)->right->left) > wl)
            {  /* double */
            btirrot(&((*node)->right));
            btilrot(node);
            btichkrt(&((*node)->left));
            btichkrt(&((*node)->right));
            }
         }    /* left rotation needed */
      else if (wl > wr)
         {  /* right rotation needed */
         if (btiwt((*node)->left->left) > wr)
            {  /* single */
            btirrot(node);
            btichkrt(&((*node)->right));
            }
         else if (btiwt((*node)->left->right) > wr)
            {  /* double */
            btilrot(&((*node)->left));
            btirrot(node);
            btichkrt(&((*node)->left));
            btichkrt(&((*node)->right));
            }
         }    /* right rotation needed */
      }    /* node != NULL */

   }    /* btichkrt */


/***
*    BTITFREE
*   ----------
* Recursivly free up memory allocated to the nodes in [node].
***/
void btitfree(BTNODEPTR *node)

   {  /* btitfree */

   if (*node != NULL)
      {
      btitfree(&((*node)->left));
      btitfree(&((*node)->right));
      free((*node)->keyptr);
      free((*node)->infptr);
      free((char *)(*node));
      }

   }    /* btitfree */


/***
*    BTILFREE
*   ----------
* Free up memory allocated to list [l].
***/
void btilfree(BTSELPTR *l)

   {  /* btilfree */

   /* variables */
   BTSELPTR temp;

   while (*l != NULL)
      {
      temp = *l;
      *l = (*l)->next;
      free((char *)temp);
      }

   }    /* btilfree */


/***
*    BTINIT
*   --------
* Initialize tree [t], given, the size (in bytes) of the user's key,
* the size (in bytes) of the user's info, and the address of the function
* to use when comparing two keys.  A tree must be initialized with this
* function before any of the other routines can operate on it properly.
***/
void btinit(BTTREE *t, int keysiz, int infsiz, int (*cmpfunc)())

   {  /* btinit */

   (*t).root = NULL;
   (*t).selected = NULL;
   (*t).cmpfunc = cmpfunc;
   (*t).sizkey = keysiz;
   (*t).sizinf = infsiz;

   }    /* btinit */


/***
*    BTLOC
*   -------
* Attempt to locate [key] in tree [t].  Return a pointer to
* its info, NULL if not found.
***/
void * btloc(void * key, BTTREE *t)

   {  /* btloc */

   /* variables */
   char *result;
   BTNODEPTR node;
   int v;

   result = NULL;
   node = (*t).root;
   while(node != NULL)
      {
      v = (*((*t).cmpfunc))(key, node->keyptr);
      if (v == 0)
         {   /* found */
         result = node->infptr;
         node = NULL;
         }
      else if (v < 0)
         {
         node = node->left;
         }
      else
         {
         node = node->right;
         }
      }    /* while(node != NULL)  */

   return(result);
   }    /* btloc */


/***
*    BTIINS
*   --------
* Recursivily search the nodes of tree [t] for a place to put [key].
***/
char *btiins(char *key, BTNODEPTR *node)

   {  /* btiins */

   /* variables */
   BTNODEPTR result;
   int       v;

   result = NULL;
   if (*node == NULL)
      {  /* new node */
      (*node) = (BTNODEPTR) malloc(sizeof(BTNODE));
      if (! (*node))
         {
         btimemerr("btiins");
         }
      (*node)->left = NULL;
      (*node)->right = NULL;
      (*node)->weight = 2;
      (*node)->keyptr = malloc((unsigned)btsizkey);
      if (! (*node)->keyptr)
         {
         btimemerr("btiins");
         }
      (*node)->infptr = calloc((unsigned)btsizinf, (unsigned)1);
      if (! (*node)->infptr)
         {
         btimemerr("btiins");
         }
      memcpy((*node)->keyptr, key, btsizkey);
      result = (BTNODEPTR) (*node)->infptr;
      }
   else
      {
      v = (*btcmp)(key,(*node)->keyptr);
      if (v == 0)
         {
         result = NULL; /* duplicate key */
         }
      else
         {
         if (v < 0)
            {
            result = (BTNODEPTR) btiins(key, &((*node)->left));
            }
         else
            {
            result = (BTNODEPTR) btiins(key, &((*node)->right));
            }
         (*node)->weight = btiwt((*node)->left) + btiwt((*node)->right);
         btichkrt(node);
         }
      }    /* node != NULL  */

   return((char *) result);

   }    /* btiins */



/***
*    BTINS
*   -------
* Inserts a node containning [key] in tree [t].
* Return a NIL pointer if a duplicate key was found.
***/
void * btins(void * key, BTTREE *t)

   {  /* btins */

   btcmp = (*t).cmpfunc;
   btsizkey = (*t).sizkey;
   btsizinf = (*t).sizinf;

   return(btiins(key, &((*t).root)));

   }    /* btins */


char *fbtiins(void * key, BTNODEPTR *node, void * infptr, BTNODE *nodeptr)
{ 
  
  BTNODEPTR result;
  int       v;
  
  result = NULL;
  if (*node == NULL)
    {  /* new node */
      /* (*node) = (BTNODEPTR) calloc( 1, sizeof(BTNODE)); */
      (*node) = nodeptr;
      if (! (*node))
        {
          btimemerr("btiins");
        }
      (*node)->weight = 2;
      (*node)->keyptr = key;
      if (! (*node)->keyptr)
        {
          btimemerr("btiins");
        }
      (*node)->infptr = infptr;
      if (! (*node)->infptr)
        {
          btimemerr("btiins");
        }
      result = (BTNODEPTR) (*node)->infptr;
    }
  else
    {
      v = (*btcmp)(key,(*node)->keyptr);
      if (v == 0)
        {
          result = NULL; /* duplicate key */
        }
      else
        {
          if (v < 0)
            {
              result = (BTNODEPTR) fbtiins(key, &((*node)->left), 
                                           infptr, nodeptr);
            }
          else
            {
              result = (BTNODEPTR) fbtiins(key, &((*node)->right), 
                                           infptr, nodeptr);
            }
          (*node)->weight = btiwt((*node)->left) + btiwt((*node)->right);
          btichkrt(node);
        }
    }    /* node != NULL  */
  
  return((char *) result);
  
}  


void * fbtins(void * key, BTTREE *t, void * infptr, BTNODE *nodeptr)
{ 
  btcmp = (*t).cmpfunc;
  btsizkey = (*t).sizkey;
  btsizinf = (*t).sizinf;
  return(fbtiins(key, &((*t).root), (void *) infptr, nodeptr));
}    


/***
*    BTIDEL
*   --------
* Recursively search the nodes in tree [t] for [key].
* Delete the node that contains [key] if found.
* Re-balance on the way out.
***/
BOOL btidel(char *key, BTNODEPTR *node)

   {  /* btidel */

   /* variables */
   BOOL result;
   int  v;

   if (*node == NULL)
      {
      result = FALSE;   /* key not found */
      }
   else
      {

      /* search for key to be deleted */
      v = (*btcmp)(key, (*node)->keyptr);
      if (v < 0)
         {
         result = btidel(key, &((*node)->left));
         }
      else if (v > 0)
         {
         result = btidel(key, &((*node)->right));
         }
      /* key found, delete if a descant is NULL */
      else if ((*node)->left == NULL)
         {
         result = TRUE;
         dnode = *node;
         *node = (*node)->right;
         }
      else if ((*node)->right == NULL)
         {
         result = TRUE;
         dnode = *node;
         *node = (*node)->left;
         }
      /* no desc}ant is NULL, rotate heavier side */
      else if (btiwt((*node)->left) > btiwt((*node)->right))
         {  /* left side is heavier, do a right rotation */
         if (btiwt((*node)->left->left) < btiwt((*node)->left->right))
            {
            btilrot(&((*node)->left));
            }
         btirrot(node);
         result = btidel(key, &((*node)->right));
         }
      else
         {  /* right side is heavier, do a left rotation */
         if (btiwt((*node)->right->left) > btiwt((*node)->right->right))
            {
            btirrot(&((*node)->right));
            }
         btilrot(node);
         result = btidel(key, &((*node)->left));
         }

      }    /* node != NULL  */

   /* reconstruct weight information */
   if (*node != NULL)
      {
      (*node)->weight = btiwt((*node)->left) + btiwt((*node)->right);
      }

   return(result);

   }    /* btidel */


/***
*    BTDEL
*   -------
* Delete a node containning [key] from tree [t].
* Return FALSE if [key] not found in tree [t].
* If a node is deleted and was selected, it's automatically de-selected.
***/
BOOL btdel(void * key, BTTREE *t)

   {  /* btdel */

   /* variables */
   BOOL result;
   BTSELPTR prev;
   BTSELPTR curr;

   dnode = NULL;
   btcmp = (*t).cmpfunc;

   result = btidel(key, &((*t).root));

   if (dnode != NULL)
      {

      /* Delete [dnode] from the selected list... */
      prev = NULL;
      curr = (*t).selected;
      while(curr != NULL)
         {
         if (curr->node == dnode)
            {
            if (prev == NULL)
               {
               (*t).selected = curr->next;
               }
            else
               {
               prev->next = curr->next;
               }
            free((char*)curr);
            curr = NULL;
            }
         else
            {
            prev = curr;
            curr = curr->next;
            }
         }

      free(dnode->keyptr);
      free(dnode->infptr);
      free((char *)dnode);
      }

   return(result);

   }    /* btdel */


/***
*    BTISEL
*   --------
* Build a list of pointers [selected] to nodes in [node]
* that contain keys within (minkey, maxkey).  Also update [numof].
***/
void btisel(BTNODEPTR node, void * minkey, void * maxkey, BTSELPTR *selected)

   {  /* btisel */

   /* variables */
   BTSELPTR lp;

   if (node != NULL)
      {
      if ((*btcmp)(node->keyptr, maxkey) > 0)
         {
         btisel(node->left, minkey, maxkey, selected);
         }
      else if ((*btcmp)(node->keyptr, minkey) < 0)
         {
         btisel(node->right, minkey, maxkey, selected);
         }
      else
         {
         btisel(node->right, minkey, maxkey, selected);
         lp = (BTSELPTR) malloc(sizeof(BTSELREC));
         if (! lp)
            {
            btimemerr("btisel");
            }
         lp->next = *selected;
         lp->node = node;
         *selected = lp;
         numof = numof + 1;
         btisel(node->left, minkey, maxkey, selected);
         }
      }    /* (node != NULL) */

   }    /* btisel */


/***
*    BTSELECT
*   ----------
* Selects all nodes in tree [t] such that [minkey]<=keys<=[maxkey].
* Make calls to btget to obtain the selected nodes.
***/
int btselect(void * minkey, void * maxkey, BTTREE *t)

   {  /* btselect */

   numof = 0;
   btcmp = (*t).cmpfunc;
   btilfree(&((*t).selected));
   btisel((*t).root, minkey, maxkey, &((*t).selected));

   return(numof);

   }    /* btselect */


/***
*    BTGET
*   -------
* Get the next node from the list of selected nodes.  Use btselect
* to select nodes.  The function value is TRUE if the key/info of
* a selected node was obtained, FALSE if no nodes were selected.
***/
BOOL btget(void * key, void * *info, BTTREE *t)
{ 
  BOOL result;
  BTNODEPTR p;
  BTSELPTR temp;
  
  result = FALSE;
  if ((*t).selected != NULL)
    {
      result = TRUE;
      temp = (*t).selected;
      (*t).selected = (*t).selected->next;
      p = temp->node;
      memcpy(key, p->keyptr, (*t).sizkey);
      *info = p->infptr;
      free((char *)temp);
    }

  return(result);
}


BOOL btfget(void * *key, void * *info, BTTREE *t)
{
  BTSELPTR temp;
  
  if(t->selected){
    temp = t->selected;
    *info = t->selected->node->infptr;
    *key = t->selected->node->keyptr;
    t->selected = t->selected->next;
    free((char *)temp);
    return(TRUE);
  }
  return(FALSE);
}


/***
*    BTFREE
*   --------
* Disposes of memory allocated to tree [t],
* and any memory allocated by a btselect call.
* Tree [t] will be NULL upon return.
***/
void btfree(BTTREE *t)

   {  /* btfree */

   btitfree(&((*t).root));
   btilfree(&((*t).selected));
   (*t).root = NULL;
   (*t).selected = NULL;

   }    /* btfree */


/***
*    BTIPPRE
*   ---------
* Recursivley traverse the nodes in tree [t] in preorder
* calling btputln at each node.
***/
void btippre(FILE *fp, BTNODEPTR node)
   
   {  /* btippre */

   if (node != NULL)
      {
      (*btputln)(fp, node->keyptr, node->infptr);
      btippre(fp, node->left);
      btippre(fp, node->right);
      }

   }    /* btippre */


/***
*    BTPPRE
*   --------
* Prints the contents of tree [t] to file [fp] in preorder.
***/
void btppre(FILE *fp, void (*putln)(), BTTREE *t)

   {  /* btppre */

   btputln = putln;
   btippre(fp, (*t).root);

   }    /* btppre */


/***
*    BTIPIN
*   --------
* Recursivley traverse the nodes in tree [t] inorder
* calling btputln at each node.
***/
void btipin(FILE *fp, BTNODEPTR node)
   
   {  /* btipin */

   if (node != NULL)
      {
      btipin(fp, node->left);
      (*btputln)(fp, node->keyptr, node->infptr);
      btipin(fp, node->right);
      }

   }    /* btipin */


/***
*    BTPIN
*   -------
* Prints the contents of tree [t] to file [fp] in order.
***/
void btpin(FILE *fp, void (*putln)(), BTTREE *t)

   {  /* btpin */

   btputln = putln;
   btipin(fp, (*t).root);

   }    /* btpin */


/***
*    BTIPPOST
*   ----------
* Recursivley traverse the nodes in tree [t] in post order
* calling btputln at each node.
***/
void btippost(FILE *fp, BTNODEPTR node)
   
   {  /* btippost */

   if (node != NULL)
      {
      btippost(fp, node->left);
      btippost(fp, node->right);
      (*btputln)(fp, node->keyptr, node->infptr);
      }

   }    /* btippost */


/***
*    BTPPOST
*   ---------
* Prints the contents of [t] to file [fp] in postorder.
***/
void btppost(FILE *fp, void (*putln)(), BTTREE *t)

   {  /* btppost */

   btputln = putln;
   btippost(fp, (*t).root);

   }    /* btppost */


/***
*    BTIPTREE
*   -------
* Recursivley traverse the nodes in tree [t] inorder
* calling btputln at each node.
***/ 
void btiptree(FILE *fp, BTNODEPTR node, int l)

   {  /* btiptree */

   /* variables */
   int i;

   if (node != NULL)
      {
      btiptree(fp, node->right, l+1);
      for(i = 1;  i <= l;  i++)
         {
         fprintf(fp, " . ");
         }
      (*btputln)(fp, node->keyptr, node->infptr);
      btiptree(fp, node->left,  l+1);
      }

   }    /* btiptree */


/***
*    BTPTREE
*   ---------
* Prints the contents of tree [t] to file [fp].
***/
void btptree(FILE *fp, void (*putln)(), BTTREE *t)

   {  /* btptree */

   btputln = putln;
   btiptree(fp, (*t).root, 1);

   }    /* btptree */


/***
*    BTNODECNT
*   -----------
* Return the number of nodes attached to NODE_BTNP.
***/
void btnodecnt(BTNODE *node_btnp, int *cnt_p)

   {   /* btnodecnt */

   if (node_btnp != (BTNODE *) 0)
      {
      btnodecnt(node_btnp->left, cnt_p);
      ++(*cnt_p);
      btnodecnt(node_btnp->right, cnt_p);
      }

   }   /* btnodecnt */


/* btutil */

