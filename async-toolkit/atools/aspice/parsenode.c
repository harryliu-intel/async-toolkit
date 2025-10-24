/*****************************************************
 * Copyright 2005 Fulcrum Microsystems               *
 * Copyright 1999 California Institute of Technology *
 * All rights reserved.  Written by Andrew Lines.    *
 *****************************************************/

#include "aspice.h"
#define FIRST_ALIAS_IS_CANONICAL

/*** compare strings ***/
int nodenamecmp(char *p1, char *p2)
  {
  return strcmp(p1,p2);
  }

/*** compare two parsenodes by name ***/
int pparsenodecmp (void *p1, void *p2)
  {
  return nodenamecmp ((*((PARSENODE **)p1))->name,
                      (*((PARSENODE **)p2))->name);
  }

/*** compare a parsenode against a string ***/
int pnodenamecmp (void *p1, void *p2)
  {
  return nodenamecmp ((*((PARSENODE **)p1))->name,*((char **)p2));
  }

/*** find root alias and shorten alias tree ***/
PARSENODE *root_alias(PARSENODE *pn)
  {
  PARSENODE *proot,*pnext;

  proot=pn; while (proot->proot!=NULL) proot=proot->proot;
  while (pn->proot!=NULL) {pnext=pn->proot; pn->proot=proot; pn=pnext;}
  return proot;
  }

/*** alias both the alias loop and the root tree ***/
void alias_parsenodes(PARSENODE *pnode0, PARSENODE *pnode1)
  {
  PARSENODE *pn;

  /*** find root aliases ***/
  pnode0=root_alias(pnode0);
  pnode1=root_alias(pnode1);

  /*** return if already connected ***/
  if (pnode0==pnode1) return;

  /*** alias the roots ***/
  pnode1->proot=pnode0;

  /*** set the fixed/used fields of the root ***/
  pnode0->fixed=(pnode0->fixed||pnode1->fixed);
  pnode0->analog_used=(pnode0->analog_used||pnode1->analog_used);
  pnode0->digital_used=(pnode0->digital_used||pnode1->digital_used);
  pnode0->group_used=(pnode0->group_used||pnode1->group_used);
  pnode0->active_used=(pnode0->active_used||pnode1->active_used);

  /*** splice the alias loops together ***/
  pn=pnode0->palias;
  pnode0->palias=pnode1->palias;
  pnode1->palias=pn;
  }

/*** add a new parsenode to a list if unique ***/
PARSENODE *add_parsenode(LIST *names, LIST *globals, LIST *criticalnets,
                         char *prefix, char *name,
                         int fixed, int unnamed,
			 int analog_used, int active_used,
                         int digital_used, int group_used)
  {
  static int pnum=0; // first node created is more canonical
  PARSENODE *pnode,*proot;
  int j,k,m;
  char fullname[STRMAX],*pc=fullname,c;

  /*** if unique name, add to names ***/
  safe_sprintf(pc,"%s%s",prefix,name);
  j=find_element_lazy_sort(names,&pc,&pnodenamecmp);
  if (j<0)
    {
    pnode=(PARSENODE *) leak_malloc (sizeof(PARSENODE));
    pnode->pnum=pnum++;
    pnode->analog_used=analog_used;
    pnode->active_used=active_used;
    pnode->digital_used=digital_used;
    pnode->group_used=group_used;
    pnode->name=leak_strdup(pc);
    pnode->num=-1;
    pnode->dnum=-1;
    pnode->fixed=fixed;
    pnode->created=0;
    pnode->subnet=0;
    pnode->unnamed=0; /*** named by default ***/
    pnode->internal=0;
    pnode->top_port=0;

    /*** detect subnets ***/
    m = strlen(pc);
    for (k=0; k<m; k++) if (pc[k]==':') {pnode->subnet=(k<m-1 ? 2 : 1); break;}
    m = k; // ignore subnet field for subsequent classifications

    /*** detect internal and unnamed ***/
    for (k=0; k<m; k++) if (pc[k]=='#') pnode->internal=1;
    if (unnamed) pnode->unnamed=4; /* unnamed instance has lowest priority */
    else
      {
      if (pnode->subnet)    pnode->unnamed=1;
      if (pnode->internal)  pnode->unnamed=2;
      for (k=0; k<m; k++) if (pc[k]=='$') pnode->unnamed=3;
      }

    /*** create the node ***/
    pnode->palias=pnode;
    pnode->proot=NULL;
    list_insert_element_lazy_sort(names,&pnode,&pparsenodecmp);

    /*** check if base name is in criticalnets ***/
    c = pc[m];
    pc[m] = 0;
    pnode->critical = criticalnets==NULL || criticalnets->max==0 ||
      (find_element_sorted(criticalnets,&pc,&pstrcmp)>=0);
    pc[m] = c;

    /*** if global, make an alias ***/
    pnode->global=(find_element_sorted(globals,&name,&pstrcmp)>=0);
    if (pnode->global&&(prefix[0]!=0))
      {
      /*** find or create the top-level global node ***/
      k=find_element_lazy_sort(names,&name,&pnodenamecmp);
      if (k<0)
        {
        add_parsenode(names,globals,criticalnets,"",name,0,0,0,0,0,0);
        k=find_element_lazy_sort(names,&name,&pnodenamecmp);
        }
      assert(k>=0);
      /*** make the alias ***/
      alias_parsenodes(names->p.parsenode[k],pnode);
      pnode->global=2;
      }
    }
  else
    {
    pnode=names->p.parsenode[j];
    proot=root_alias(pnode);
    proot->analog_used=(proot->analog_used||analog_used);
    proot->digital_used=(proot->digital_used||digital_used);
    proot->active_used=(proot->active_used||active_used);
    proot->group_used=(proot->group_used||group_used);
    }
  return pnode;
  }

/*** compare legibility of two node names ***/
int better_name_cmp(PARSENODE *pn1, PARSENODE *pn2)
  {
  int l1,l2,d1,d2;

  if (pn1->unnamed<pn2->unnamed) return +1;
  if (pn2->unnamed<pn1->unnamed) return -1;

  if (pn1->subnet<pn2->subnet) return +1;
  if (pn2->subnet<pn1->subnet) return -1;

  if (pn1->internal<pn2->internal) return +1;
  if (pn2->internal>pn1->internal) return -1;

#ifdef FIRST_ALIAS_IS_CANONICAL
  return pn2->pnum - pn1->pnum; // first alias created is considered the most canonical (see BUG 27025)
#else
  l1=d1=0; while(pn1->name[l1]!=0) {d1+=(pn1->name[l1]=='.'); l1++; }
  l2=d2=0; while(pn2->name[l2]!=0) {d2+=(pn2->name[l2]=='.'); l2++; }

  if (d1<d2) return +1;
  if (d2<d1) return -1;
  if (l1<l2) return +1;
  if (l2<l1) return -1;
  return strcmp(pn2->name,pn1->name);
#endif
  }

/*** compare analog nodes by canonical name ***/
int node_cmp(void *p1, void *p2)
  {
  NODE *pn1 = *((NODE **)p1), *pn2 = *((NODE **)p2);
  return pparsenodecmp(&pn1->name,&pn2->name);
  }

/*** compare digital nodes by canonical name ***/
int dnode_cmp(void *p1, void *p2)
  {
  DIGITAL_NODE *dn1 = *((DIGITAL_NODE **)p1), *dn2 = *((DIGITAL_NODE **)p2);
  return pparsenodecmp(&dn1->name,&dn2->name);
  }

/*** lookup a parsenode by name with a prefix ***/
PARSENODE *find_parsenode(CIRCUIT *circuit, char *prefix, char *name)
  {
  int j;
  char fullname[STRMAX],*pc=fullname;
  safe_sprintf(pc,"%s%s",prefix,name);
  j=find_element_sorted(circuit->names,&pc,&pnodenamecmp);
  if (j>=0) return circuit->names->p.parsenode[j];
  else return NULL;
  }

/*** find an analog node by name ***/
NODE *find_node(CIRCUIT *circuit, char *prefix, char *name)
  {
  PARSENODE *pn;
  NODE *node=NULL;
  pn = find_parsenode(circuit,prefix,name);
  if (pn==NULL) return NULL;
  pn = root_alias(pn);
  if (pn->analog_used && (pn->num>=0))
    node = &circuit->nodes[pn->num];
  return node;
  }

/*** find a digital node by name ***/
DIGITAL_NODE *find_digital_node(CIRCUIT *circuit, char *prefix, char *name)
  {
  PARSENODE *pn;
  DIGITAL_NODE *dnode=NULL;
  pn = find_parsenode(circuit,prefix,name);
  if (pn==NULL) return NULL;
  pn = root_alias(pn);
  if (pn->digital_used && !pn->group_used && (pn->dnum>=0))
    dnode = &circuit->dnodes[pn->dnum];
  return dnode;
  }

/*** find a digital group by name ***/
DIGITAL_GROUP *find_digital_group(CIRCUIT *circuit, char *prefix, char *name)
  {
  PARSENODE *pn;
  DIGITAL_GROUP *dgrp=NULL;
  pn = find_parsenode(circuit,prefix,name);
  if (pn==NULL) return NULL;
  pn = root_alias(pn);
  if (pn->digital_used && pn->group_used && (pn->dnum>=0))
    dgrp = &circuit->groups[pn->dnum];
  return dgrp;
  }
