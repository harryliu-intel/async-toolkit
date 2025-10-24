/*****************************************************
 * Copyright 2005 Fulcrum Microsystems               *
 * Copyright 1999 California Institute of Technology *
 * All rights reserved.  Written by Andrew Lines.    *
 *****************************************************/

#include "aspice.h"

/****************************** COMPARISON FUNCTIONS ******************************/

int paddrcmp (void *p1, void *p2)
  {
  return *((int **)p1) - *((int **)p2);
  }

int pstrcmp (void *p1, void *p2)
  {
  return strcmp(*((char **)p1),*((char **)p2));
  }

int passigncmp(void *p1, void *p2)
  {
  return strcmp (((ASSIGNMENT *)p1)->name,((ASSIGNMENT *)p2)->name);
  }

int passignstrcmp(void *p1, void *p2)
  {
  return strcmp (((ASSIGNMENT *)p1)->name,*((char **)p2));
  }

/*** compare modifiers by type and optional instance ***/
int pmodifiercmp(void *p1, void *p2)
  {
  int c;
  MODIFIER *pm1=*((MODIFIER **)p1), *pm2=*((MODIFIER **)p2);
  c = strcmp(pm1->type,pm2->type);
  if (c!=0) return c;
  if ((pm1->name==NULL) && (pm2->name!=NULL)) return -1;
  if ((pm2->name==NULL) && (pm1->name!=NULL)) return  1;
  if ((pm1->name==NULL) && (pm2->name==NULL)) return  0;
  return strcmp(pm1->name,pm2->name);
  }

/*** compare modifiers by type and optional instance (supports * suffix on type) ***/
int pmodifierprefixcmp(void *p1, void *p2)
  {
  int c,l;
  MODIFIER *pm1=*((MODIFIER **)p1), *pm2=*((MODIFIER **)p2);
  l = strlen(pm1->type);
  if (pm1->type[l-1]=='*') c = strncmp(pm1->type,pm2->type,l-1);
  else c = strcmp(pm1->type,pm2->type);
  if (c!=0) return c;
  if ((pm1->name==NULL) && (pm2->name!=NULL)) return -1;
  if ((pm2->name==NULL) && (pm1->name!=NULL)) return  1;
  if ((pm1->name==NULL) && (pm2->name==NULL)) return  0;
  return strcmp(pm1->name,pm2->name);
  }

/*** compare cells by name and length of nodes and parameters ***/
int pcellcmp (void *p1, void *p2)
  {
  int c;
  PARSECELL *pcell1 = *((PARSECELL **)p1), *pcell2= *((PARSECELL **)p2);
  c = strcmp(pcell1->name,pcell2->name);
  if (c!=0) return c;
  c = (pcell1->nodes->max - pcell2->nodes->max);
  if (c!=0) return c;
  c = (pcell1->parms->max - pcell2->parms->max);
  return c;
  }

/*** compare cell to an action ***/
int pcellactioncmp (void *p1, void *p2)
  {
  int c;
  PARSECELL   *pcell= *((PARSECELL **) p1);
  PARSEACTION *pact= (PARSEACTION *) p2;
  c = strcmp(pcell->name,pact->type);
  if (c!=0) return c;
  c = (pcell->nodes->max - pact->nodes->max);
  if (c!=0) return c;
  c = (pcell->parms->max - pact->parms->max);
  return c;
  }

/*** compare cell name to a string ***/
int pcellnamecmp (void *p1, void *p2)
  {
  PARSECELL *pcell1 = *((PARSECELL **)p1);
  return strcmp(pcell1->name,(char *)p2);
  }
