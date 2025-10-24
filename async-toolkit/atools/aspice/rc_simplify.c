/*****************************************************
 * Copyright 2005 Fulcrum Microsystems               *
 * Copyright 1999 California Institute of Technology *
 * All rights reserved.  Written by Andrew Lines.    *
 *****************************************************/

#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#define REDUCE_MAX_G_SPOKES 8
#define REDUCE_MAX_ITERATIONS 40

static int verbose = 1;

/*** define list data structure ***/
typedef unsigned char byte;
typedef struct _list {
  int max,size,mem,shrink;
  union {
    byte *b;
    struct GC **pgc;
    struct NODE **pn;
  } p;
} LIST;

/*** include more header files ***/
#include "list.h"
#include "misc.h"
#include "leak.h"

/*** a node with a name and some properties ***/
typedef struct NODE {
  char *name;      // node name
  double G,C;      // total G,C over all spokes
  double CLUMP;    // lumped capacitance
  unsigned used:1, // used directly by a non R/C device
    used_wire:1,   // used indirectly via wire alias
    dirty:1;       // spokes/G/C are out-of-date
  int g_spokes,    // number of conductive spokes
    c_spokes,      // number of capacitive spokes
    n_spokes;      // total number of GC spokes
  struct NODE *root;
  struct GC **spokes;
} NODE;

/*** a capacitance and/or conductance between a pair of nodes ***/
typedef struct GC {
  char *name;  // resistor name
  char *layer; // resistor layer
  double W;    // resistor width
  double G;    // conductance
  double C;    // capacitance  
  NODE *a,*b;
} GC;

/*** compare two NODE *'s by name ***/
int pnodecmp(void *p1, void *p2) {
  NODE *pn1 = *((NODE **) p1);
  NODE *pn2 = *((NODE **) p2);
  return strcmp(pn1->name,pn2->name);
}

/*** compare two NODE *'s by C/G ***/
int pnodetaucmp(void *p1, void *p2) {
  NODE *pn1 = *((NODE **) p1);
  NODE *pn2 = *((NODE **) p2);
  if      (pn1->G==0 && pn2->G==0)      return  0;
  else if (pn1->G==0 && pn2->G>0)       return  1;
  else if (pn1->G>0 && pn2->G==0)       return -1;
  else if (pn1->C*pn2->G<pn2->C*pn1->G) return -1;
  else if (pn1->C*pn2->G>pn2->C*pn1->G) return  1;
  return 0;
}

/*** compare two strings, potentially NULL's ***/
int strcmp_null(char *s1, char *s2) {
  if ((s1!=NULL) && (s2==NULL)) return -1;
  if ((s1==NULL) && (s2!=NULL)) return  1;
  if ((s1!=NULL) && (s2!=NULL)) return strcmp(s1,s2);
  return 0;
}

/*** compare two GC *'s ***/
int gccmp(void *p1, void *p2) {
  int c;
  GC *pgc1 = *((GC **) p1);
  GC *pgc2 = *((GC **) p2);
  c = strcmp_null(pgc1->layer,pgc2->layer);
  if (c!=0) return c;
  c = strcmp(pgc1->a->name,pgc2->a->name);
  if (c!=0) return c;
  c = strcmp(pgc1->b->name,pgc2->b->name);
  return c;
}

/*** return root node, flatten the tree ***/
NODE *find_root(NODE *pn) {
  if (pn->root == pn) return pn;
  pn->root = find_root(pn->root);
  return pn->root;
}

/*** find or create a NODE * by name ***/
NODE *find_node(LIST *nodes, char *a) {
  NODE n,*pn;
  int i;
  n.G = 0;
  n.C = 0;
  n.CLUMP = 0;
  n.name = a;
  n.used = 0;
  n.used_wire = 0;
  n.dirty = 1;
  n.g_spokes = 0;
  n.c_spokes = 0;
  n.n_spokes = 0;
  n.spokes = NULL;
  n.root = NULL;
  pn = &n;
  i = find_element_lazy_sort(nodes,&pn,&pnodecmp);
  if (i<0) {
    pn = (NODE *) leak_malloc(sizeof(NODE));
    *pn = n;
    pn->name = leak_strdup(a);
    pn->root = pn;
    list_insert_element_lazy_sort(nodes,&pn,&pnodecmp);
  }
  else pn = nodes->p.pn[i];
  return pn;
}

/*** free memory associated with a node ***/
void free_node(NODE *pn) {
  leak_free(pn->name);
  leak_free(pn);
}

/*** create a wire between two NODE's ***/
void create_wire(NODE *pna, NODE *pnb) {
  NODE *pnt;

  /*** find root nodes ***/
  pna = find_root(pna);
  pnb = find_root(pnb);
  
  /*** pick better root node ***/
  if (pnodecmp(&pna,&pnb)>0) {pnt=pna; pna=pnb; pnb=pnt;}

  /*** lump the nodes together ***/
  pnb->root = pna;
  pna->CLUMP += pnb->CLUMP;
  pnb->CLUMP = 0;

  /*** propagate used_wire marking ***/
  pna->used_wire = pnb->used_wire = 
    pna->used || pnb->used || pna->used_wire || pnb->used_wire;
}

/*** count number of preserved nodes ***/
int count_preserved_nodes(LIST *nodes) {
  NODE *pn;
  int i,count=0;
  for (i=0; i<nodes->max; i++) {
    pn = nodes->p.pn[i];
    if ((pn->root==pn) && (pn->n_spokes>0)) count++;
  }
  return count;
}

/*** output debugging information for nodes***/
void debug_nodes(LIST *nodes) {
  NODE *pn;
  int i;
  for (i=0; i<nodes->max; i++) {
    pn = nodes->p.pn[i];
    fprintf(stderr,
            "node name=%s used=%d used_wire=%d g_spokes=%d c_spokes=%d root=%s\n",
            pn->name,pn->used,pn->used_wire,
            pn->g_spokes,pn->c_spokes,find_root(pn)->name);
  }
}

/*** output essential node aliases ***/
void print_nodes(LIST *nodes) {
  NODE *pn;
  int i;
  for (i=0; i<nodes->max; i++) {
    pn = nodes->p.pn[i];
    if (pn->CLUMP>0) printf("cap (%s) (%g)\n",pn->name,pn->CLUMP);
    if (pn->used && (pn->root!=pn))
      printf("wire (%s, %s)\n",find_root(pn)->name,pn->name);
  }
}

/*** output rc network ***/
void print_gcs(LIST *gcs) {
  GC *pgc;
  NODE *pna,*pnb;
  char name[STRMAX];
  int i;
  list_finish_lazy_sort(gcs,&gccmp);
  for (i=0; i<gcs->max; i++) {
    pgc = gcs->p.pgc[i];
    pna = pgc->a;
    pnb = pgc->b;
    name[0]=0;
    if ((pgc->name!=NULL) && (pgc->layer!=NULL))
      safe_sprintf(name,"%s.%s.%0.0f",pgc->name,pgc->layer,pgc->W);
    if (pgc->C>0) printf("cap (%s, %s) (%g)\n",pna->name,pnb->name,pgc->C);
    if (pgc->G>0) printf("res %s(%s, %s) (%g)\n",name,pna->name,pnb->name,1/pgc->G);
  }
}

/*** combine widths of parallel resistors to preserve peak current density ***/
double parallel_width(double G1, double W1, double G2, double W2) {
  double W;
  if      (G1<=0) W = W2;
  else if (G2<=0) W = W1;
  else if (W1 * G2 > W2 * G1) W = W2 * (G1+G2) / G2;
  else                        W = W1 * (G1+G2) / G1;
  return W;
}

/*** combines a G/C info with existing data ***/
void accumulate_gc(LIST *gcs, NODE *pna, NODE *pnb, double G, double C,
                   char *name, char *layer, double W) {
  NODE *pnt;
  GC gc,*pgc;
  int i;

  /*** skip empty GC ***/
  if (C==0 && G==0) return;

  /*** sort a and b ***/
  if (pnodecmp(&pna,&pnb)>0) {
    pnt=pna; pna=pnb; pnb=pnt;
  }

  /*** find gc ***/
  gc.name = name;
  gc.layer = layer;
  gc.W = W;
  gc.a = pna;
  gc.b = pnb;
  gc.C = C;
  gc.G = G;
  pgc = &gc;
  i = find_element_lazy_sort(gcs,&pgc,&gccmp);
  if (i<0) {
    pgc = leak_malloc(sizeof(GC));
    *pgc = gc;
    if (pgc->name!=NULL)  pgc->name  = leak_strdup(name);
    if (pgc->layer!=NULL) pgc->layer = leak_strdup(layer);
    list_insert_element_lazy_sort(gcs,&pgc,&gccmp);
  }
  else {
    pgc = gcs->p.pgc[i];
    pgc->W = parallel_width(pgc->G,pgc->W,G,W);
    pgc->C += C;
    pgc->G += G;
  }
}

/*** free memory associated with a GC ***/
void free_gc(GC *pgc) {
  if (pgc->name!=NULL)  leak_free(pgc->name);
  if (pgc->layer!=NULL) leak_free(pgc->layer);
  leak_free(pgc);
}

/*** lump parallel g/c caused by new wires, discard zeros and self connects ***/
void canonicalize_gcs(LIST *gcs) {
  LIST *new_gcs;
  GC *pgc,*last=NULL;
  NODE *pna,*pnb;
  int i,full_sort=0;

  /*** replace a/b nodes with root nodes ***/
  for (i=0; i<gcs->max; i++) {
    pgc = gcs->p.pgc[i];
    pna = find_root(pgc->a);
    pnb = find_root(pgc->b);
    if ((pgc->a==pna) && (pgc->b==pnb)) continue; // not changed
    full_sort = 1;
    if (pnodecmp(&pna,&pnb)>0) {pgc->a=pnb; pgc->b=pna;}
    else                       {pgc->a=pna; pgc->b=pnb;}
  }

  /*** sort gcs by layer, a, b ***/
  if (full_sort) list_sort(gcs,&gccmp);
  else list_finish_lazy_sort(gcs,&gccmp);

  /*** merge g/c with same a/b nodes and layer ***/
  new_gcs = list_create(sizeof(GC *));
  for (i=0; i<gcs->max; i++) {
    pgc = gcs->p.pgc[i];
    if ((last!=NULL)&&(gccmp(&last,&pgc)==0)) { // accumulate from last
      pgc->W = parallel_width(pgc->G,pgc->W,last->G,last->W);
      pgc->G += last->G;
      pgc->C += last->C;
      free_gc(last); // discard last
    }
    else if ((last!=NULL)&&((last->G>0)||(last->C>0))&&(last->a!=last->b))
      list_append_element(new_gcs,&last); // add last to new_gcs
    else if (last!=NULL) free_gc(last); // discard last
    last = pgc;
  }
  if ((last!=NULL)&&((last->G>0)||(last->C>0))&&(last->a!=last->b))
    list_append_element(new_gcs,&last); // add last to new_gcs
  else if (last!=NULL) free_gc(last); // discard last
  list_move(gcs,new_gcs);
}

/*** reallocate the spokes array ***/
void realloc_spokes(NODE *pn) {
  if (pn->spokes==NULL) { // spokes was empty
    if (pn->n_spokes>0)
      pn->spokes = (GC **) leak_malloc(pn->n_spokes*sizeof(GC *));
  }
  else { // spokes was non-empty
    if (pn->n_spokes>0)
      pn->spokes = (GC **) leak_realloc(pn->spokes,pn->n_spokes*sizeof(GC *));
    else {leak_free(pn->spokes); pn->spokes=NULL; }
  }
}

/*** count spokes, fill in NODE's spokes array ***/
void count_spokes(LIST *nodes, LIST *gcs) {
  int i,j;
  GC *pgc;
  NODE *pn,*pna,*pnb;
  // clear spokes counts and pointers
  for (i=0; i<nodes->max; i++) {
    pn = nodes->p.pn[i];
    pn->c_spokes = 0;
    pn->g_spokes = 0;
    pn->n_spokes = 0;
    pn->dirty = 0;
  }
  // count spokes
  for (i=0; i<gcs->max; i++) {
    pgc = gcs->p.pgc[i];
    pna = find_root(pgc->a);
    pnb = find_root(pgc->b);
    if (pgc->C>0 || pgc->G>0) {
      pna->n_spokes++;
      pnb->n_spokes++;
    }
    if (pgc->C>0) {
      pna->c_spokes++;
      pnb->c_spokes++;
    }
    if (pgc->G>0) {
      pna->g_spokes++;
      pnb->g_spokes++;
    }
  }
  // reallocate spokes arrays
  for (i=0; i<nodes->max; i++) {
    pn = nodes->p.pn[i];
    realloc_spokes(pn);
    pn->n_spokes = 0;
  }
  // link spokes GC pointers
  for (i=0; i<gcs->max; i++) {
    pgc = gcs->p.pgc[i];
    pna = find_root(pgc->a);
    pnb = find_root(pgc->b);
    if (pgc->C>0 || pgc->G>0) {
      pna->spokes[pna->n_spokes++] = pgc;
      pnb->spokes[pnb->n_spokes++] = pgc;
    }
  }
  // compute C, G
  for (i=0; i<nodes->max; i++) {
    pn = nodes->p.pn[i];
    pn->G = 0;
    pn->C = pn->CLUMP;
    for (j=0; j<pn->n_spokes; j++) {
      pgc = pn->spokes[j];
      pn->C += pgc->C;
      pn->G += pgc->G;
    }
  }
}

/*** zero out dangling devices ***/
int delete_dangling_devices(LIST *gcs) {
  int i,progress=0;
  GC *pgc;
  NODE *pna,*pnb;
  for (i=0; i<gcs->max; i++) {
    pgc = gcs->p.pgc[i];
    pna = pgc->a;
    pnb = pgc->b;
    if (((pna->n_spokes>1) || pna->used_wire) &&
        ((pnb->n_spokes>1) || pnb->used_wire))
      continue; // not dangling
    pgc->C=0;
    pgc->G=0;
    progress++;
  }
  if (verbose && progress>0)
    printf("/* rc_simplify: discarded %d dangling R/C devices */\n",progress);
  return progress;
}

/*** apply minR ***/
int short_small_resistors(LIST *gcs, double minR) {
  int i,progress=0,increases=0;
  GC *pgc;
  NODE *pna,*pnb;
  for (i=0; i<gcs->max; i++) {
    pgc = gcs->p.pgc[i];
    if (pgc->G==0)        continue; // no conductance
    if (pgc->G*minR<=1)   continue; // above minR threshold
    if (pgc->layer!=NULL)
      {
      // can't discard named resistors, so instead increase R to minR
      if (minR>0) {pgc->G = 1/minR; increases++;}
      continue;
      }
    pna = pgc->a;
    pnb = pgc->b;
    create_wire(pna,pnb);
    pgc->C=0;
    pgc->G=0;
    progress++;
  }
  if (verbose && increases>0)
    printf("/* rc_simplify: increased %d small named resistors to minR */\n",
           increases);
  if (verbose && progress>0)
    printf("/* rc_simplify: shorted %d small resistors */\n",progress);
  return progress;
}

/*** apply minC ***/
int lump_small_capacitors(LIST *gcs, double minC) {
  int i,progress=0;
  GC *pgc;
  NODE *pna,*pnb;
  for (i=0; i<gcs->max; i++) {
    pgc = gcs->p.pgc[i];
    if (pgc->C==0)    continue; // no capacitance
    if (pgc->C>=minC) continue; // above minC threshold
    pna = pgc->a;
    pnb = pgc->b;
    pna->CLUMP += pgc->C;
    pnb->CLUMP += pgc->C;
    pgc->C=0;
    progress++;
  }
  if (verbose && progress>0)
    printf("/* rc_simplify: lumped %d small capacitors */\n",progress);
  return progress;
}

/*** return opposite terminal of a node ***/
NODE *opposite_terminal(NODE *pn, GC *pgc) {
  if      (pgc->a==pn) return pgc->b;
  else if (pgc->b==pn) return pgc->a;
  return NULL;
}

/*** reduce nodes ***/
int reduce_fast_nodes(LIST *all_nodes, LIST *gcs, double minRC, int max_g_spokes) {
  int i,j,k,progress=0,has_layers,mixed_layers;
  NODE *pn,*pna,*pnb;
  LIST *nodes;
  GC *pgca,*pgcb;
  double c,g,W;
  char *name;

  // find candidate nodes for RC reduction
  nodes = list_create(sizeof(NODE *));
  for (i=0; i<all_nodes->max; i++) {
    pn = all_nodes->p.pn[i];
    if (pn->root!=pn)                     continue; // only process root nodes
    if (pn->n_spokes==0)                  continue; // node already deleted
    if (pn->used_wire)                    continue; // can't delete node
    if (pn->g_spokes>max_g_spokes)        continue; // too many conductive spokes
    if (pn->C>=minRC*pn->G)               continue; // tau is slow enough
    list_insert_element_lazy_sort(nodes,&pn,&pnodetaucmp);
  }

  // sort by C/G
  list_finish_lazy_sort(nodes,&pnodetaucmp);

  // process nodes in order of increasing tau
  for (i=0; i<nodes->max; i++) {
    pn = nodes->p.pn[i];

    // must count_spokes and do another pass
    if (pn->dirty) continue;

    // additional suitability test for EM
    k = has_layers = mixed_layers = 0;
    for (j=0; j<pn->n_spokes; j++)
      if (pn->spokes[j]->layer!=NULL) {has_layers = 1; k = j;}
    if (has_layers && pn->g_spokes>2) continue; // only do <= 2 conductive spokes
    for (j=0; j<pn->n_spokes; j++)
      if ((pn->spokes[j]->G>0) &&
          (strcmp_null(pn->spokes[j]->layer,pn->spokes[k]->layer)!=0))
        mixed_layers = 1;
    if (mixed_layers) continue; // conductive spokes must have same layer names

    // create new pairwise C/G spokes
    for (j=0; j<pn->n_spokes; j++)
      for (k=j+1; k<pn->n_spokes; k++) {
        pgca = pn->spokes[j];
        pgcb = pn->spokes[k];
        pna = opposite_terminal(pn,pgca);
        pnb = opposite_terminal(pn,pgcb);
        g = pgca->G * pgcb->G / pn->G;
        c = (pgca->C * pgcb->G + pgcb->C * pgca->G) / pn->G;
        pna->dirty = 1;
        pnb->dirty = 1;
        if (has_layers && pgca->G>0 && pgcb->G>0) {
          // keep width and name of narrower resistor
          if (pgca->W<pgcb->W) {W=pgca->W; name=pgca->name;}
          else                 {W=pgcb->W; name=pgcb->name;}
          accumulate_gc(gcs,pna,pnb,g,c,name,pgca->layer,W);
        }
        else accumulate_gc(gcs,pna,pnb,g,c,NULL,NULL,0);
      }

    // delete old CG spokes
    for (j=0; j<pn->n_spokes; j++) {
      pn->spokes[j]->G=0;
      pn->spokes[j]->C=0;
    }
    progress++;
  }
  if (verbose && progress>0)
    printf("/* rc_simplify: eliminated %d fast nodes with <= %d conductive spokes */\n",
           progress,max_g_spokes);
  list_free(nodes);
  return progress;
}

/*** simplify rc netlist from stdin to stdout ***/
int main(int argc, char *argv[]) {
  char line[STRMAX],a[STRMAX],b[STRMAX],id[STRMAX],name[STRMAX],layer[STRMAX];
  double x,minR,minC,minRC,W;
  int i,n,progress,max_spokes;
  int got_minR=0,got_minC=0,got_minRC=0;
  NODE *pna,*pnb,*pn;
  LIST *nodes,*gcs;
  
  /*** parse arguments ***/
  n=0;
  for (i=1; i<argc; i++) {
    if (strncmp(argv[i],"-v",2)==0) {verbose = 1;}
    else if (n==0) {got_minR = sscanf(argv[i],"%lf",&minR); n++;}
    else if (n==1) {got_minC = sscanf(argv[i],"%lf",&minC); n++;}
    else if (n==2) {got_minRC = sscanf(argv[i],"%lf",&minRC); n++;}
  }
  if (!got_minR || !got_minC || !got_minRC) {
    fprintf(stderr,"USAGE: rc_simplify [-v] minR minC minRC < in.rc > out.rc\n");
    return 1;
  }

  /*** parse rc network ***/
  nodes = list_create(sizeof(NODE *));
  gcs = list_create(sizeof(GC *));
  while(fgets(line,STRMAX,stdin)) {
    name[0]=0;
    n=sscanf(line,"%s %s %s %lg %s %s %lf",id,a,b,&x,name,layer,&W);
    if ((strcmp(id,"used")==0)&&(n==2)) {
      pn = find_node(nodes,a);
      pn->used = pn->used_wire = 1;
      pn = find_root(pn);
      pn->used_wire = 1;
    }
    else if ((id[0]=='R')&&(n>=4)) {
      pna = find_node(nodes, a);
      pnb = find_node(nodes, b);
      if (x<=0) create_wire(pna,pnb);
      else if (n==7) accumulate_gc(gcs,pna,pnb,1/x,0,name,layer,W);
      else accumulate_gc(gcs,pna,pnb,1/x,0,NULL,NULL,0);
    }
    else if ((id[0]=='C')&&(n==4)) {
      pna = find_node(nodes, a);
      pnb = find_node(nodes, b);
      accumulate_gc(gcs,pna,pnb,0,x,NULL,NULL,0);
    }
    else printf("%s",line);
  }
  printf("/* rc_simplify: minC=%g minR=%g minRC=%g */\n",minC,minR,minRC);
  printf("/* rc_simplify: initial %d nodes, %d devices */\n",
         nodes->max,gcs->max);
  list_finish_lazy_sort(nodes,&pnodecmp);

  /*** simplify RC network ***/
  do {
    progress = 0;
    canonicalize_gcs(gcs);
    count_spokes(nodes,gcs);
    progress = delete_dangling_devices(gcs);
  } while (progress>0);
  for (max_spokes=1; max_spokes<=REDUCE_MAX_G_SPOKES; max_spokes*=2) {
    n = 0;
    do {
      progress = 0;
      canonicalize_gcs(gcs);
      count_spokes(nodes,gcs);
      progress = reduce_fast_nodes(nodes,gcs,minRC,max_spokes);
      n++;
    } while ((progress>0) && ((n<REDUCE_MAX_ITERATIONS) || (max_spokes<=2)));
  }
  do {
    progress = 0;
    canonicalize_gcs(gcs);
    count_spokes(nodes,gcs);
    progress = short_small_resistors(gcs,minR);
  } while (progress>0);
  do {
    progress = 0;
    canonicalize_gcs(gcs);
    count_spokes(nodes,gcs);
    progress = lump_small_capacitors(gcs,minC);
  } while (progress>0);

  /*** output nodes and devices ***/
  printf("/* rc_simplify: final %d nodes, %d devices */\n",
         count_preserved_nodes(nodes),gcs->max);
  print_nodes(nodes);
  print_gcs(gcs);

  /*** free memory and exit ***/
  for (i=0; i<gcs->max; i++) free_gc(gcs->p.pgc[i]);
  for (i=0; i<nodes->max; i++) {
    pn = nodes->p.pn[i];
    if (pn->spokes!=NULL) leak_free(pn->spokes);
    free_node(pn);
  }
  list_free(gcs);
  list_free(nodes);
  free_temp_list();
  leak_check();  
  return 0;
}
