#include "auto.h"
SIZE Size;
FUNC *func_add, *func_sub, *func_mul, *func_div, *func_max, *func_min,
     *func_sqr, *func_exp;

#if 0
#define debug_var(var) print_var(stderr,var)
#else
#define debug_var(var)
#endif
void invalidate_sizing_variables();
void sizing_setup();
double compute_metric();
void free_aliases();
void free_instances();
void free_labels();
void free_circuits();
void free_nodetypes();
void complete_aliases();
void compute_localnodes();
int pcircuit_instances_cmp(void *p1, void *p2);

/*********************** PARSE INSTANCES, CIRCUIT *****************************/

/*** fill in pcircuit fields of instances, remove NULL ones ***/
void leaf_cells()
  {
  int i,j;
  LIST *circuits,*instances,*new;
  char fn_circuit[STRMAX];
  INSTANCE *pinst;
  CIRCUIT *pcircuit;

  /*** lazy create lists ***/
  if (Size.circuits==NULL) Size.circuits=list_create(sizeof(CIRCUIT *));
  if (Size.instances==NULL) Size.instances=list_create(sizeof(INSTANCE));
  circuits=Size.circuits;
  instances=Size.instances;

  /*** go through existing instances ***/
  new=list_create(sizeof(INSTANCE));
  for (i=0; i<instances->max; i++)
    {
    pinst=&instances->p.instance[i];
    j=find_element_lazy_sort(circuits,pinst->type,&pcircuit_str_cmp);
    if (j<0)
      {
      safe_sprintf(fn_circuit,"%s.auto",pinst->type);
      pcircuit=leak_malloc(sizeof(CIRCUIT));
      *pcircuit=read_circuit(pinst->type,fn_circuit,0);
      if (pcircuit->name==NULL)
        {
        ERROR=1; fprintf(stderr,"ERROR: parse_cells can't read type=%s filename=%s.\n",
		pinst->type,fn_circuit);
        return;
        }
      list_insert_element_lazy_sort(circuits,&pcircuit,&pcircuit_cmp);
      }
    else pcircuit=circuits->p.pcircuit[j];
    pinst->pcircuit=pcircuit;
    if (pcircuit!=NULL)
      {
      pcircuit->instances++;
      list_append_element(new,pinst);
      }
    }
  list_move(instances,new);
  list_sort(instances,&instance_cmp);
  list_sort(circuits,&pcircuit_instances_cmp);
  }

/*** create label list from localnodes of aliases ***/
LIST *create_labels()
  {
  LIST *labels,*aliases;
  LABEL label;
  ALIAS *pal;
  LOCALNODE *pln;
  int i,j;
  labels=list_create(sizeof(LABEL));
  aliases=Size.aliases;
  label.paint=find_paint("space");
  label.position=LABEL_POSITION_CENTER;
  for (i=0; i<aliases->max; i++)
    {
    pal=aliases->p.palias[i];
    if (pal->root!=NULL) continue;
    if (pal->global) continue;
    for (j=0; j<pal->localnodes->max; j++)
      {
      pln=&pal->localnodes->p.localnode[j];
      label.alias=pal;
      label.x0=pln->pinstance->x0;
      label.y0=pln->pinstance->y0;
      label.x1=pln->pinstance->x1;
      label.y1=pln->pinstance->y1;
      list_append_element(labels,&label);
      }
    }
  return labels;
  }

/*** load instances from magic or cadence, find labels ***/
void parse_instances(int Cadence, char *fn)
  {
  free(Size.labels);
  if (Size.aliases==NULL) Size.aliases=list_create(sizeof(ALIAS *));
  if (Size.instances==NULL) Size.instances=list_create(sizeof(INSTANCE));
  if (Cadence) parse_cadence(fn,Size.aliases,Size.instances);
  else 
    {
    Size.labels=list_create(sizeof(LABEL));
    parse_mag(fn,"",identity_transform(),Size.labels,Size.aliases,Size.instances,1);
    }
  leaf_cells();
  complete_aliases();
  compute_localnodes();
  if (Cadence) Size.labels=create_labels();
  }

/*** debugging ***/
void debug_instances()
  {
  int i;
  LIST *instances=Size.instances;
  printf("INSTANCES:\n");
  if (instances==NULL) return;
  for (i=0; i<instances->max; i++)
    printf("%s %s\n",instances->p.instance[i].pcircuit->name,
                     instances->p.instance[i].name);
  }

/*** debugging ***/
void debug_circuits()
  {
  int i;
  CIRCUIT *pcircuit;
  LIST *circuits=Size.circuits;
  printf("CIRCUITS:\n");
  if (circuits==NULL) return;
  for (i=0; i<circuits->max; i++)
    {
    pcircuit=circuits->p.pcircuit[i];
    print_circuit(stdout,pcircuit,1,1,1,1,1,1);
    }
  }

/*** save circuits ***/
void save_circuits()
  {
  int i;
  FILE *file;
  CIRCUIT *pcircuit;
  LIST *circuits=Size.circuits;
  if (circuits==NULL) return;
  for (i=0; i<circuits->max; i++) 
    {
    pcircuit=circuits->p.pcircuit[i];
    if (pcircuit->fixed==1)
      {
      if (get(pcircuit->metric)>0)
	printf("Fixed Circuit %s has Metric=%g\n",pcircuit->name,get(pcircuit->metric));
      continue;
      }
    file=fopen(pcircuit->filename,"wt");
    if (file==NULL)
      {
      ERROR=1; fprintf(stderr,"ERROR: save_circuits can't write %s.\n",pcircuit->filename);
      return;
      }
    print_circuit(file,pcircuit,1,0,1,1,1,1);
    fclose(file);
    }
  }

/*********************** PARSE ALIASES and CAPACITANCES **************************/

/*** compare names of two aliases ***/
int alias_cmp(void *p1, void *p2)
  {
  return strcmp((*((ALIAS **)p1))->name,(*((ALIAS **)p2))->name);
  }

/*** compare name of alias to given name ***/
int alias_name_cmp(void *p1, void *p2)
  {
  return strcmp((*((ALIAS **)p1))->name,(char *)p2);
  }

/*** find root alias, shorten alias tree at the same time ***/
ALIAS *root_al(ALIAS *pn)
  {
  if (pn->root==NULL) return pn;
  pn->root=root_al(pn->root);
  return pn->root;
  }

/*** find an alais ***/
ALIAS *find_alias(LIST *aliases, char *name)
  {
  int i;
  i=find_element_lazy_sort(aliases,name,&alias_name_cmp);
  if (i<0) return NULL;
  else return aliases->p.palias[i];
  }

/*** create or find a named alias ***/
ALIAS *create_alias(LIST *aliases, char *name)
  {
  int j;
  ALIAS *pn;
  j=find_element_lazy_sort(aliases,name,&alias_name_cmp);
  if (j>=0) return root_al(aliases->p.palias[j]);
  else
    {
    pn=(ALIAS *) leak_malloc(sizeof(ALIAS));
    pn->name=leak_strdup(name);
    pn->global=(find_element_sorted(GlobalNames,&name,&str_cmp)>=0);
    pn->cap=0;
    pn->res=0;
    pn->root=NULL;
    pn->next=pn;
    pn->localnodes=NULL;
    list_insert_element_lazy_sort(aliases,&pn,&alias_cmp);
    return root_al(pn);
    }
  }

/*** alias two aliases together: must be done before creating rules! ***/
void connect_aliases(ALIAS *pn1, ALIAS *pn2)
  {
  ALIAS *pn;
  pn1=root_al(pn1);
  pn2=root_al(pn2);
  if (pn1==pn2) return;
  pn2->root=pn1;
  pn=pn1->next; pn1->next=pn2->next; pn2->next=pn;
  pn1->cap+=pn2->cap;
  pn1->res+=pn2->res;
  pn1->global=pn2->global=(pn1->global||pn2->global);
  pn2->cap=0;
  pn2->res=0;
  }

/*** read in alaises from .connect file ***/
void parse_connect(char *filename)
  {
  FILE *file;
  LEX *lex;
  ALIAS *pn1,*pn2;
  LIST *aliases;
  if (Size.aliases==NULL) Size.aliases=list_create(sizeof(ALIAS *));
  aliases=Size.aliases;
  file=fopen(filename,"rt");
  if (file==NULL) return;
  lex=lex_file_with_name(file,filename);
  while(!lex_is_eof(lex))
    {
    lex_eat_sym(lex,"connect");
    pn1=create_alias(aliases,parse_nodename(lex));
    pn2=create_alias(aliases,parse_nodename(lex));
    connect_aliases(pn1,pn2);
    }
  lex_free(lex);
  fclose(file);
  }

/*** read in capacitances (and resistances) ***/
int parse_cap(char *filename)
  {
  char *name;
  FILE *file;
  LEX *lex;
  ALIAS *pn;
  LIST *aliases;
  if (Size.aliases==NULL) Size.aliases=list_create(sizeof(ALIAS *));
  aliases=Size.aliases;
  file=fopen(filename,"rt");
  if (file==NULL) return 0;
  lex=lex_file_with_name(file,filename);
  lex_define_whitespace(lex," \t");
  while(!lex_is_eof(lex))
    {
    name=parse_nodename(lex);
    pn=create_alias(aliases,name);
    pn->cap+=lex_eat_real(lex)*Size.extcap;
    if (lex_is_real(lex)) pn->res+=lex_eat_real(lex);
    lex_eatif_sym(lex,"\n");
    }
  lex_free(lex);
  fclose(file);
  return 1;
  }

/*** make sure alias exists for all node instances ***/
void complete_aliases()
  {
  int i,j,k;
  INSTANCE instance;
  RULE *prule;
  char fullname[STRMAX];
  LIST *instances,*aliases;

  if (Size.instances==NULL) return;
  if (Size.aliases==NULL) Size.aliases=list_create(sizeof(ALIAS *));
  instances=Size.instances;
  aliases=Size.aliases;

  for (i=0; i<instances->max; i++)
    {
    instance=instances->p.instance[i];
    for (j=0; j<instance.pcircuit->rules->max; j++)
      {
      prule=&instance.pcircuit->rules->p.rule[j];
      safe_sprintf(fullname,"%s%s",instance.name,prule->target);
      create_alias(aliases,fullname);
      for (k=0; k<prule->guard->max; k++)
	{
        safe_sprintf(fullname,"%s%s",instance.name,prule->guard->p.pc[k]);
        create_alias(aliases,fullname);
	}
      }
    }
  }

/**************** CONVERT INSTANCE SPACE TO TYPE SPACE *******************/

/*** compare localnodes by instance/name address ***/
int localnode_instance_cmp(void *p1, void *p2)
  {
  int c;
  LOCALNODE *pn1,*pn2;
  pn1=(LOCALNODE *)p1;
  pn2=(LOCALNODE *)p2;
  c=pn1->pinstance - pn2->pinstance;
  if (c!=0) return c;
  return strcmp(pn1->name,pn2->name);
  }

/*** compare localnodes by circuit/name address ***/
int localnode_circuit_cmp(void *p1, void *p2)
  {
  int c;
  LOCALNODE *pn1,*pn2;
  pn1=(LOCALNODE *)p1;
  pn2=(LOCALNODE *)p2;
  c=pn1->pcircuit - pn2->pcircuit;
  if (c!=0) return c;
  return strcmp(pn1->name,pn2->name);
  }

/*** look for a given instance ***/
void compute_localnode(INSTANCE *pinstance, char *name)
  {
  ALIAS *pal;
  char fullname[STRMAX];
  LOCALNODE ln;
  LIST *aliases=Size.aliases;
  if (aliases==NULL) return;
  safe_sprintf(fullname,"%s%s",pinstance->name,name);
  pal=find_alias(aliases,fullname);
  if (pal==NULL) return;
  pal=root_al(pal);
  ln.pinstance=pinstance;
  ln.pcircuit=pinstance->pcircuit;
  ln.name=name;
  if (find_element_lazy_sort(pal->localnodes,&ln,&localnode_instance_cmp)<0)
    {
    ln.name=get_name(ln.name);
    list_insert_element_lazy_sort(pal->localnodes,&ln,&localnode_instance_cmp);
    }
  }

/*** fill in ALIAS->localnodes ***/
void compute_localnodes()
  {
  int i,j,k;
  INSTANCE *pinstance;
  RULE *prule;
  ALIAS *pal;
  LIST *instances,*aliases;
  if (Size.instances==NULL) return;
  if (Size.aliases==NULL) Size.aliases=list_create(sizeof(ALIAS *));
  instances=Size.instances;
  aliases=Size.aliases;
  for (i=0; i<aliases->max; i++)
    {
    pal=aliases->p.palias[i];
    if (pal->root==NULL) pal->localnodes=list_create(sizeof(LOCALNODE));
    }
  for (i=0; i<instances->max; i++)
    {
    pinstance=&instances->p.instance[i];
    for (j=0; j<pinstance->pcircuit->rules->max; j++)
      {
      prule=&pinstance->pcircuit->rules->p.rule[j];
      compute_localnode(pinstance,prule->target);
      for (k=0; k<prule->guard->max; k++)
        compute_localnode(pinstance,prule->guard->p.pc[k]);
      }
    }
  }

/*** debugging ***/
void debug_aliases()
  {
  int i,j;
  ALIAS *pal,*root;
  LOCALNODE *pln;
  LIST *aliases=Size.aliases;
  printf("ALIASES:\n");
  if (aliases==NULL) return;
  for (i=0; i<aliases->max; i++)
    {
    pal=aliases->p.palias[i];
    root=pal->root;
    printf("%s root=%s cap=%g res=%g localnodes=",
	   pal->name,(root!=NULL?root->name:"(null)"),pal->cap,pal->res);
    if (pal->root == NULL) for (j=0; j<pal->localnodes->max; j++)
      {
      pln=&pal->localnodes->p.localnode[j];
      printf("(%s,%s,%s) ",pln->pinstance->name,pln->pcircuit->name,pln->name);
      }
    printf("\n");
    }
  }

/****************************** NODETYPE CONSTRUCTION ****************************/

/*** compare nodetypes for equal localnodes list ***/
int nodetype_cmp(void *p1, void *p2)
  {
  int c;
  NODETYPE *nt1,*nt2;
  nt1=(NODETYPE *)p1;
  nt2=(NODETYPE *)p2;
  c=nt1->localnodes->max - nt2->localnodes->max;
  if (c!=0) return c;
  return list_compare(nt1->localnodes,nt2->localnodes,&localnode_circuit_cmp);
  }

/*** create nodetype list ***/
void create_nodetypes()
  {
  int i,j;
  ALIAS *pn;
  LOCALNODE ln;
  NODETYPE nt,*pnt;
  LIST *aliases,*nodetypes;

  if (Size.aliases==NULL) return;
  if (Size.nodetypes==NULL) Size.nodetypes=list_create(sizeof(NODETYPE));
  aliases=Size.aliases;
  nodetypes=Size.nodetypes;

  for (i=0; i<aliases->max; i++)
    {
    /*** construct a nodetype ***/
    pn=aliases->p.palias[i];
    if (pn->root!=NULL) continue;
    nt.localnodes=list_create(sizeof(LOCALNODE));
    nt.worst_alias=leak_strdup(pn->name);
    nt.totcap=nt.mincap=nt.maxcap=pn->cap;
    nt.maxres=pn->res;
    nt.maxcap=max(nt.maxcap,Size.mincap);
    nt.instances=1;
    nt.metric=0;

    /*** add unique instances of localnodes to nt.localnodes ***/
    for (j=0; j<pn->localnodes->max; j++)
      {
      ln=pn->localnodes->p.localnode[j];
      if (find_element_lazy_sort(nt.localnodes,&ln,&localnode_instance_cmp)<0)
        list_insert_element_lazy_sort(nt.localnodes,&ln,&localnode_instance_cmp);
      }

    /*** sort by circuit instead of instance, add it to nodetypes list ***/
    if (nt.localnodes->max==0) {leak_free(nt.worst_alias); list_free(nt.localnodes); continue;}
    list_sort(nt.localnodes,&localnode_circuit_cmp);

    /*** add unique nodetype to list ***/
    j=find_element_lazy_sort(nodetypes,&nt,&nodetype_cmp);
    if (j>=0)
      {
      pnt=&nodetypes->p.nodetype[j];
      pnt->instances+=nt.instances;
      if (nt.maxcap>pnt->maxcap)
	{leak_free(pnt->worst_alias); pnt->worst_alias=nt.worst_alias;}
      pnt->mincap=min(pnt->mincap,nt.mincap);
      pnt->totcap+=nt.totcap;
      pnt->maxcap=max(pnt->maxcap,nt.maxcap);
      pnt->maxres=max(pnt->maxres,nt.maxres);
      list_free(nt.localnodes);
      }
    else list_insert_element_lazy_sort(nodetypes,&nt,&nodetype_cmp);
    }
  }

/*** fill in pload pointer of local nodes, check if external ***/
void complete_localnodes()
  {
  int i,j,k;
  NODETYPE *pnt;
  LOCALNODE *pln;
  LIST *nodetypes=Size.nodetypes;
  if (Size.nodetypes==NULL) return;
  for (i=0; i<nodetypes->max; i++)
    {
    pnt=&nodetypes->p.nodetype[i];
    for (j=0; j<pnt->localnodes->max; j++)
      {
      pln=&pnt->localnodes->p.localnode[j];
      k=find_element_sorted(pln->pcircuit->loads,pln->name,&load_str_cmp);
      assert(k>=0);
      pln->pload=&pln->pcircuit->loads->p.load[k];
      }
    }
  }

/*** Identify all localnodes which are connected via nodetype to a specified circuit/name ***/
LIST *find_localnodes(CIRCUIT *pcircuit, char *name)
  {
  int i,j;
  LOCALNODE find_ln,ln;
  NODETYPE *pnt;
  LIST *localnodes;
  localnodes=list_create(sizeof(LOCALNODE));
  if (Size.nodetypes==NULL) return localnodes;
  find_ln.pinstance=NULL;
  find_ln.pload=NULL;
  find_ln.pcircuit=pcircuit;
  find_ln.name=name;
  for (i=0; i<Size.nodetypes->max; i++)
    {
    pnt=&Size.nodetypes->p.nodetype[i];
    j=find_element_sorted(pnt->localnodes,&find_ln,&localnode_circuit_cmp);
    if (j<0) continue;
    for (j=0; j<pnt->localnodes->max; j++)
      {
      ln=pnt->localnodes->p.localnode[j];
      ln.pinstance=NULL;
      ln.pload=NULL;
      if (find_element_lazy_sort(localnodes,&ln,&localnode_circuit_cmp)<0)
	list_insert_element_lazy_sort(localnodes,&ln,&localnode_circuit_cmp);
      }
    }
  list_finish_lazy_sort(localnodes,&localnode_circuit_cmp);
  return localnodes;
  }

/*** debug find_localnodes ***/
void debug_type_aliases()
  {
  int i,j,k;
  CIRCUIT *pcircuit;
  LOAD *pload;
  LIST *localnodes;
  LOCALNODE *pln;
  sizing_setup();
  for (i=0; i<Size.circuits->max; i++)
    {
    pcircuit=Size.circuits->p.pcircuit[i];
    for (j=0; j<pcircuit->loads->max; j++)
      {
      pload=&pcircuit->loads->p.load[j];
      fprintf(stderr,"Type aliases for %s/%s:\n",pcircuit->name,pload->name);
      localnodes=find_localnodes(pcircuit,pload->name);
      for (k=0; k<localnodes->max; k++)
	{
        pln=&localnodes->p.localnode[k];
	fprintf(stderr,"  %s:%s\n",pln->pcircuit->name,pln->name);
        }
      list_free(localnodes);
      }
    }
  }

/*** test if a nodetype is external ***/
int nodetype_is_external(NODETYPE *pnt)
  {
  return pnt->localnodes->max>1;
  }

/*** test if a nodetype is dynamic ***/
int nodetype_is_dynamic(NODETYPE *pnt)
  {
  int j;
  LOCALNODE ln;
  STATICIZER stat;
  for (j=0; j<pnt->localnodes->max; j++)
    {
    ln=pnt->localnodes->p.localnode[j];
    stat.name=ln.name;
    if (find_element(ln.pcircuit->staticizers,&stat,&staticizer_cmp)>=0) return 1;
    }
  return 0;
  }

/*** debugging ***/
void print_nodetype(NODETYPE *pnt, int do_internal)
  {
  LOCALNODE ln;
  char name[STRMAX];
  int j,len,col;
  if (pnt==NULL) return;
  if (~do_internal&&(!nodetype_is_external(pnt))) return;
  printf("Instances=%d WorstCase=%s Max/Avg=%.4g\n"
	 " RC=%.4gps Res=%.4g Min=%.4g Avg=%.4g Max=%.4g Load=%.4g Gate=%.0f%%\n",
	 pnt->instances,
	 pnt->worst_alias,
	 pnt->instances*pnt->maxcap/pnt->totcap,
	 1e12*(get(pnt->maximum)-0.5*pnt->maxcap)*pnt->maxres,
	 pnt->maxres,
	 1e15*pnt->mincap,
	 1e15*pnt->totcap/pnt->instances,
	 1e15*pnt->maxcap,
	 1e15*get(pnt->maximum),
	 100*get(pnt->gateload)*Size.gatecap/get(pnt->maximum));
  col=0;
  for (j=0; j<pnt->localnodes->max; j++)
    {
    ln=pnt->localnodes->p.localnode[j];
    safe_sprintf(name," %s/%s(%.4g)",ln.pcircuit->name,ln.name,get(ln.pload->internal));
    len=strlen(name);
    if ((col>0)&&(len+col>80)) {printf("\n"); col=0;}
    printf("%s",name);
    col+=len;
    }
  printf("\n");
  }

/*** sort nodetypes by precomputed metric ***/
int nodetype_metric_cmp(void *p1, void *p2)
  {
  double f1,f2;
  NODETYPE *pnt1=(NODETYPE *)p1,*pnt2=(NODETYPE *)p2;
  f1=pnt1->metric;
  f2=pnt2->metric;
  if      (f1<f2) return -1;
  else if (f2>f1) return 1;
  return 0;
  }

/*** debugging ***/
#define ORDER_RC 0
#define ORDER_C 1
#define ORDER_R 2
#define ORDER_WIRE_C_FRAC 3
#define ORDER_WIRE_R_FRAC 4
void debug_nodetypes(LEX *lex)
  {
  NODETYPE nt;
  int i,internal=0,external=0,dynamic=0,combinational=0,sort_order;
  double cutoff=0;
  LIST *nodetypes;
  if (Size.nodetypes==NULL) return;

  /*** parse options ***/
  if      (lex_eatif_keyword(lex,"internal")) internal=1;
  else if (lex_eatif_keyword(lex,"external"))  external=1;
  else internal=external=1;
  if      (lex_eatif_keyword(lex,"dynamic")) dynamic=1;
  else if (lex_eatif_keyword(lex,"static"))  combinational=1;
  else dynamic=combinational=1;
  if      (lex_eatif_keyword(lex,"rc")) sort_order=ORDER_RC;
  else if (lex_eatif_keyword(lex,"c")) sort_order=ORDER_C;
  else if (lex_eatif_keyword(lex,"r")) sort_order=ORDER_R;
  else if (lex_eatif_keyword(lex,"wire_c_fraction")) sort_order=ORDER_WIRE_C_FRAC;
  else if (lex_eatif_keyword(lex,"wire_r_fraction")) sort_order=ORDER_WIRE_R_FRAC;
  else return;
  if (lex_is_real(lex)) cutoff=lex_eat_real(lex);

  /*** review nodes ***/
  nodetypes=list_create(sizeof(NODETYPE));
  for (i=0; i<Size.nodetypes->max; i++)
    {
    nt=Size.nodetypes->p.nodetype[i];

    /*** filter nodes ***/
    if (nodetype_is_external(&nt)) {if (!external) continue;}
    else {if (!internal) continue;}
    if (nodetype_is_dynamic(&nt)) {if (!dynamic) continue;}
    else {if (!combinational) continue;}

    /*** compute and filter by metric ***/
    if      (sort_order==ORDER_RC) nt.metric=(get(nt.maximum)-0.5*nt.maxcap)*nt.maxres;
    else if (sort_order==ORDER_C)  nt.metric=get(nt.maximum);
    else if (sort_order==ORDER_R)  nt.metric=nt.maxres;
    else if (sort_order==ORDER_WIRE_C_FRAC) nt.metric=nt.maxcap/get(nt.maximum);
    else if (sort_order==ORDER_WIRE_R_FRAC) nt.metric=0; /* hard to compute */
    else continue;
    if (nt.metric<cutoff) continue;
    list_append_element(nodetypes,&nt);
    }

  /*** sort and print results ***/
  list_sort(nodetypes,&nodetype_metric_cmp);
  if      (internal&&!external) printf("INTERNAL ");
  else if (external&&!internal) printf("EXTERNAL ");
  if      (dynamic&&!combinational) printf("DYNAMIC ");
  else if (!dynamic&&combinational) printf("COMBINATIONAL ");
  printf("NODETYPES ");
  if (sort_order==ORDER_RC) printf("rc ");
  else if (sort_order==ORDER_C) printf("c ");
  else if (sort_order==ORDER_R) printf("r ");
  else if (sort_order==ORDER_WIRE_C_FRAC) printf("wire_c_fraction");
  else if (sort_order==ORDER_WIRE_R_FRAC) printf("wire_r_fraction");
  printf(">%g\n",cutoff);
  for (i=0; i<nodetypes->max; i++) print_nodetype(&nodetypes->p.nodetype[i],0);
  }

/*** compute total gate load in lambda ***/
double total_gate_width()
  {
  int i;
  double C=0;
  LIST *nodetypes=Size.nodetypes;
  NODETYPE *pnt;
  if (nodetypes==NULL) return 0;
  for (i=0; i<nodetypes->max; i++)
    {
    pnt=&nodetypes->p.nodetype[i];
    C+=get(pnt->gateload)*pnt->instances;
    }
  return C;
  }

/*** compute average switching cap in fF/node ***/
double average_switching_cap(int include_wires)
  {
  int i;
  double C=0,N=0;
  LIST *nodetypes=Size.nodetypes;
  NODETYPE *pnt;
  if (nodetypes==NULL) return 0;
  for (i=0; i<nodetypes->max; i++)
    {
    pnt=&nodetypes->p.nodetype[i];
    C+=get(pnt->gateload)*Size.gatecap*pnt->instances + (include_wires ? pnt->totcap : 0);
    N+=pnt->instances;
    }
  return 1e15*C/N;
  }

/*** print some info about the sizing ***/
double print_info()
  {
  double C,CG,W,t,T;
  C=average_switching_cap(1);
  CG=average_switching_cap(0);
  W=total_gate_width();
  t=get(Size.delay)*1e9;
  T=compute_metric();
  printf("Results:");
  printf(" t= %.3g (ns)",t);
  printf(" E= %.3g (fF/node)",C);
  printf(" T= %.3g",sqrt(T));
  printf(" Ett= %.3g",t*t*C);
  printf(" W= %.3g",W);
  printf(" Gate= %.0f%%\n",CG/C*100);
  fflush(stdout);
  return t*t*C;
  }

/************************* IDENTIFY EXTERNAL PINS OF CIRCUITS *******************/

/*** create pin directives for all nodes in circuits which are connected via nodetypes ***/
void identify_pins()
  {
  NODETYPE *pnt;
  LOCALNODE *pln;
  DIRECTIVE dir;
  int i,j;
  for (i=0; i<Size.nodetypes->max; i++)
    {
    pnt=&Size.nodetypes->p.nodetype[i];
    if (pnt->localnodes->max<2) continue; /* ignore it if it doesn't connect anything */
    for (j=0; j<pnt->localnodes->max; j++)
      {
      pln=&pnt->localnodes->p.localnode[j];
      dir.name=pln->name;
      dir.type=7; /* ambiguous pin */
      dir.pos=-1;
      if (find_element_sorted(pln->pcircuit->directives,&dir,&directive_cmp)<0)
	list_insert_element_sorted(pln->pcircuit->directives,&dir,&directive_cmp);
      } 
    }
  }

/*************************** RULE RESISTANCE/DELAYBIAS **************************/

/*** return delaybias of a rule ***/
double rule_delaybias(RULE *prule)
  {
  int series;
  series=prule->guard->max;
  if (prule->setdelay>0)
    return prule->setdelay; /* precisely specified delay */
  else if      (!prule->dir&&(series<=Size.Nmax)&&(series>0))
    return Size.Ndelaybias[series]*prule->delaybias;
  else if ( prule->dir&&(series<=Size.Pmax)&&(series>0))
    return Size.Pdelaybias[series]*prule->delaybias;
  ERROR=1; fprintf(stderr,"ERROR: unspecified R for dir=%d and %d gates in series.\n",
	  prule->dir,series);
  return 0;
  }

/*** return resistance of a rule ***/
double rule_resistance(RULE *prule)
  {
  int series;
  series=prule->guard->max;
  if      (!prule->dir&&(series<=Size.Nmax)&&(series>0))
    return Size.Nresistance[series];
  else if ( prule->dir&&(series<=Size.Pmax)&&(series>0))
    return Size.Presistance[series];
  ERROR=1; fprintf(stderr,"ERROR: unspecified R for dir=%d and %d gates in series.\n",
	  prule->dir,series);
  return 10000;
  }

/******************************* LOAD/DELAY ************************************/

/*** build formulae for rule delays within a circuit ***/
void compute_rule_delays(CIRCUIT *circuit)
  {
  int i,j;
  RULE *pr;
  LOAD *pl;

  for (i=0; i<circuit->rules->max; i++)
    {
    pr=&circuit->rules->p.rule[i];
    if (!pr->independent) continue;
    for (j=0; j<circuit->loads->max; j++)
      {
      pl=&circuit->loads->p.load[j];
      if (pl->name==pr->target)
	{
        /*** pr->delay = pl->maximum*rule_resistance(pr)/pr->width ***/
	fmla_var(pr->delay,pl->maximum);
	fmla_constant(pr->delay,rule_resistance(pr));
        fmla_func(pr->delay,func_mul);
	fmla_var(pr->delay,pr->width);
	fmla_func(pr->delay,func_div);
        }
      }
    }

  /*** debugging ***/
  for (i=0; i<circuit->rules->max; i++)
    {
    pr=&circuit->rules->p.rule[i];
    debug_var(pr->delay);
    }
  }

/*** build formulae for internal loads within a circuit ***/
void compute_internal_gate_loads(CIRCUIT *circuit)
  {
  int i,k;
  LIST *rules,*shared,*gates;
  GATE *pgate;
  LOAD *pload;
  char str[STRMAX];

  /*** clear circuit node capacitances ***/
  for (i=0; i<circuit->loads->max; i++)
    {
    pload=&circuit->loads->p.load[i];
    safe_sprintf(str,"INT(%s/%s)",circuit->name,pload->name);
    pload->internal=new_var(str,0);
    safe_sprintf(str,"WORST(%s/%s)",circuit->name,pload->name);
    pload->maximum=new_var(str,0);
    }

  /*** compute minimal gate network ***/
  rules=list_dup(circuit->rules);
  unique_num=0;
  list_sort(rules,&rule_decreasing_length_cmp);
  while(rules->max>0)
    {
    shared=shared_rules(rules,circuit->properties,circuit->feet,circuit->lessfolding);
    gates=minimal_gate_network(shared,circuit->properties);
    for (i=0; i<gates->max; i++)
      {
      pgate=&gates->p.gate[i];
      k=find_element_sorted(circuit->loads,pgate->g,&load_str_cmp);
      assert(k>=0);
      pload=&circuit->loads->p.load[k];
      fmla_var(pload->internal,pgate->width);
      if (pload->internal->formula->max>1) fmla_func(pload->internal,func_add);
      }
    list_free(shared);
    list_free(gates);
    }
  list_free(rules);

  /*** convert empty formulae to constants ***/
  for (i=0; i<circuit->loads->max; i++)
    {
    pload=&circuit->loads->p.load[i];
    if (pload->internal->formula->max==0) fmla_constant(pload->internal,0);
    debug_var(pload->internal);
    }
  }

/*** construct a formula for the max/total load of a nodetype ***/
/*** also max up load.maximum/load.minimum for all localnodes ***/
void compute_load_on_nodetype(NODETYPE *pnt)
  {
  int i;
  LOCALNODE ln;
  LOAD *pl;
  char str[STRMAX];

  /*** pnt->gateload = SUM(ln.internal) ***/
  safe_sprintf(str,"GATELOAD(%s)",pnt->worst_alias);
  pnt->gateload=new_var(str,0);
  for (i=0; i<pnt->localnodes->max; i++)
    {
    ln=pnt->localnodes->p.localnode[i];
    fmla_var(pnt->gateload,ln.pload->internal);
    if (i>0) fmla_func(pnt->gateload,func_add);
    }
  if (pnt->gateload->formula->max==0) fmla_constant(pnt->gateload,0);
  debug_var(pnt->gateload);

  /*** pnt->maximum = pnt->gateload*Size.gatecap + pnt->maxcap ***/
  safe_sprintf(str,"MAXLOAD(%p)",pnt);
  pnt->maximum=new_var(str,0);
  fmla_var(pnt->maximum,pnt->gateload);
  fmla_constant(pnt->maximum,Size.gatecap);
  fmla_func(pnt->maximum,func_mul);
  fmla_constant(pnt->maximum,pnt->maxcap);
  fmla_func(pnt->maximum,func_add);
  debug_var(pnt->maximum);

  /*** pl->maximum=max(pl->maximum,pnt->maximum) ***/
  for (i=0; i<pnt->localnodes->max; i++)
    {
    pl=pnt->localnodes->p.localnode[i].pload;
    fmla_var(pl->maximum,pnt->maximum);
    if (pl->maximum->formula->max>1) fmla_func(pl->maximum,func_max);
    }
  }

/*** return the internal gate/total loads per cell for debugging purposes ***/
void circuit_loads(CIRCUIT *circuit, double *gate, double *maximum)
  {
  int i;
  LOAD *pl;
  *gate=*maximum=0;
  for (i=0; i<circuit->loads->max; i++)
    {
    pl=&circuit->loads->p.load[i];
    *gate+=get(pl->internal);
    *maximum+=get(pl->maximum);
    }
  }

/********************************* FIND PATHS ***************************************/

/*** check if path ends here (or force it to) ***/
int path_ends(CIRCUIT *circuit, RULE *rule, int endpath, int *progress)
  {
  int end=0,fanin=0,i,j;
  RULE *pr;
  char *pc;
  for (i=0; i<rule->guard->max; i++)
    {
    pc=rule->guard->p.pc[i];
    if ((strcmp(pc,"_SReset")==0)||(strcmp(pc,"_PReset")==0)||
	(strcmp(pc,"Vdd")==0)||(strcmp(pc,"GND")==0)) continue;
    fanin=0;
    for (j=0; j<circuit->rules->max; j++)
      {
      pr=&circuit->rules->p.rule[j];
      if ((pr->target==pc)&&(pr->dir!=rule->dir))
	{
	if (endpath&&(pr->endpath==0)) {pr->endpath=1; (*progress)++;}
        fanin++;
        if (pr->endpath>0) end++;
	}
      }
    if (fanin==0) end++;
    }
  return end;
  }

/*** find paths starting at current rule ***/
void follow_path(CIRCUIT *circuit, RULE *rule, PATH path, LIST *paths)
  {
  int i,j,end,junk=0;
  RULE *pr;
  char *pc;
  PATH newpath;

  /*** terminate path ***/
  for (i=0; i<path.rules->max; i++) if (path.rules->p.prule[i]==rule) rule=NULL;
  if (rule==NULL)
    {
    list_append_element(paths,&path);
    if (paths->max>10000)
      {
      fprintf(stderr,"Too damn many paths in %s\n",circuit->name);
      print_paths(stderr,paths);
      exit(1);
      }
    return;
    }

  /*** add current rule to path ***/
  list_append_element(path.rules,&rule);

  /*** if path ends, terminate it ***/
  end=path_ends(circuit,rule,0,&junk);
  if (end>0)
    {
    follow_path(circuit,NULL,path,paths);
    return;
    }

  /*** otherwise, recurse backward to fanin rules ***/
  for (i=0; i<rule->guard->max; i++)
    {
    pc=rule->guard->p.pc[i];
    if ((strcmp(pc,"_SReset")==0)||(strcmp(pc,"_PReset")==0)||
	(strcmp(pc,"Vdd")==0)||(strcmp(pc,"GND")==0)) continue;
    for (j=0; j<circuit->rules->max; j++)
      {
      pr=&circuit->rules->p.rule[j];
      if ((pr->target==pc)&&(pr->dir!=rule->dir))
	{
	assert(pr->endpath==0);
	newpath=path;
	newpath.rules=list_dup(newpath.rules);
	follow_path(circuit,pr,newpath,paths);
	}
      }
    }

  /*** free original path ***/
  list_free(path.rules);
  }

/*** free paths ***/
void free_paths(CIRCUIT *circuit)
  {
  int i;
  if (circuit->paths==NULL) return;
  for (i=0; i<circuit->paths->max; i++)
    {
    list_free(circuit->paths->p.path[i].rules);
    free_var(circuit->paths->p.path[i].delay);
    }
  list_free(circuit->paths);
  circuit->paths=NULL;
  }

/*** mark rules which drive no internal dependents as endpaths ***/
void mark_outputs(CIRCUIT *circuit)
  {
  int i,j,k,dependents;
  RULE *pr1,*pr2;
  for (i=0; i<circuit->rules->max; i++)
    {
    pr1=&circuit->rules->p.rule[i];
    dependents=0;
    for (j=0; j<circuit->rules->max; j++)
      {
      pr2=&circuit->rules->p.rule[j];
      if (pr1->dir==pr2->dir) continue; /* must be inverse monotonic */
      for (k=0; k<pr2->guard->max; k++)
	if (pr1->target==pr2->guard->p.pc[k]) dependents++; /* found a use of pr1->target */
      }
    if ((dependents==0)&&(pr1->endpath==0))
      {
      fprintf(stderr,"WARNING: marking %s/%s%s as endpath.\n",circuit->name,pr1->target,pr1->dir?"+":"-");
      pr1->endpath=1;
      }
    }
  }

/*** find all latency paths through a circuit ***/
void find_paths(CIRCUIT *circuit)
  {
  int i,progress;
  PATH path;
  RULE *pr;

  /*** mark implied endpaths ***/
  mark_outputs(circuit);
  do
    {
    progress=0;
    for (i=0; i<circuit->rules->max; i++)
      {
      pr=&circuit->rules->p.rule[i];
      if (path_ends(circuit,pr,0,&progress)) path_ends(circuit,pr,1,&progress);
      }
    } while (progress>0);

  /*** generate rule paths starting from endpath rules ***/
  free_paths(circuit);
  circuit->paths=list_create(sizeof(PATH));
  for (i=0; i<circuit->rules->max; i++)
    {
    pr=&circuit->rules->p.rule[i];
    if (pr->endpath==0) continue;
    path.rules=list_create(sizeof(RULE *));
    path.delay=NULL;
    path.reference=0;
    follow_path(circuit,pr,path,circuit->paths);
    }

  if (circuit->paths->max>0) 
    fprintf(stderr,"  Found %d paths in %s\n",circuit->paths->max,circuit->name);
  }

/************************************ FIND CATPATHS *********************************************/

/*** recursively build up a catpath ***/
void follow_catpath(LIST *catpaths, CIRCUIT *pcircuit, CATPATH catpath)
  {
  PATH *pp,*pp2;
  RULE *pr,*pr2;
  LOCALNODE *pln;
  LIST *lns,*localnodes;
  CATPATH newcatpath;
  int i,j,Ncatpaths=0,Nendpaths=0;
  
  /*** find all localnodes which feed into this catpath ***/
  pp=catpath.paths->p.ppath[catpath.paths->max-1];
  pr=pp->rules->p.prule[pp->rules->max-1];
  localnodes=list_create(sizeof(LOCALNODE));
  for (i=0; i<pr->guard->max; i++)
    {
    lns=find_localnodes(pcircuit,pr->guard->p.pc[i]);
    for (j=0; j<lns->max; j++)
      if (find_element_lazy_sort(localnodes,&lns->p.localnode[j],&localnode_circuit_cmp)<0)
	list_insert_element_lazy_sort(localnodes,&lns->p.localnode[j],&localnode_circuit_cmp);
    list_free(lns);
    }

  /*** search for catpaths/endpaths among the localnodes ***/
  for (i=0; i<localnodes->max; i++)
    {
    pln=&localnodes->p.localnode[i];
    for (j=0; j<pln->pcircuit->paths->max; j++)
      {
      pp2=&pln->pcircuit->paths->p.path[j];
      pr2=pp2->rules->p.prule[0];
      if (strcmp(pr2->target,pln->name)!=0) continue;
      if (pr2->dir==pr->dir) continue;
      if (pr2->endpath==1) {Nendpaths++; continue;}
      printf("Concatenating %s/%s%s to %s/%s%s\n",
	     pln->pcircuit->name,pr2->target,pr2->dir?"+":"-",
	     pcircuit->name,pr->target,pr->dir?"+":"-");
      assert(pr2->endpath==2);
      Ncatpaths++;
      newcatpath.metric=NULL;
      newcatpath.paths=list_dup(catpath.paths);
      list_append_element(newcatpath.paths,&pp2);
      follow_catpath(catpaths,pln->pcircuit,newcatpath);
      }
    }

  /*** add to catpaths list if necessary ***/
  if (((Nendpaths>0)||(Ncatpaths==0))&&(find_element_lazy_sort(catpaths,&catpath,&catpath_cmp)<0))
      list_insert_element_lazy_sort(catpaths,&catpath,&catpath_cmp);
  else list_free(catpath.paths);
  list_free(localnodes);
  }

/*** concatenate paths ***/
void concatenate_paths()
  {
  CIRCUIT *pcircuit;
  PATH *pp;
  RULE *pr;
  CATPATH catpath;
  int i,j;

  /*** for every circuit ***/
  for (i=0; i<Size.circuits->max; i++)
    {
    pcircuit=Size.circuits->p.pcircuit[i];
    pcircuit->catpaths=list_create(sizeof(CATPATH));

    /*** for each endpath ***/
    for (j=0; j<pcircuit->paths->max; j++)
      {
      pp=&pcircuit->paths->p.path[j];
      pr=pp->rules->p.prule[0];
      if (pr->endpath!=1) continue;

      /*** construct a new set of catpaths starting with this path ***/
      catpath.paths=list_create(sizeof(PATH *));
      list_append_element(catpath.paths,&pp);
      catpath.metric=NULL;
      follow_catpath(pcircuit->catpaths,pcircuit,catpath);
      }

    list_finish_lazy_sort(pcircuit->catpaths,&catpath_cmp);
    }
  }

/**************************************** METRIC *********************************************/

/*** fill in just the path metrics ***/
void create_path_metrics(CIRCUIT *circuit)
  {
  int i,j;
  PATH *pp;
  RULE *pr;
  char str[STRMAX];

  /*** fill in formulas for path.delay, path.reference ***/
  for (i=0; i<circuit->paths->max; i++)
    {
    pp=&circuit->paths->p.path[i];
    pp->reference=0;
    safe_sprintf(str,"PATHDELAY(%s:%d)",circuit->name,i);
    pp->delay=new_var(str,0);
    for (j=0; j<pp->rules->max; j++)
      {
      pr=pp->rules->p.prule[j];
      pp->reference+=rule_delaybias(pr);
      fmla_var(pp->delay,pr->delay);
      if (j>0) fmla_func(pp->delay,func_add);
      }
    debug_var(pp->delay);
    }
  }

/*** create a formula for path delay violations in this circuit ***/
void create_metric(CIRCUIT *circuit)
  {
  int i,j,N=0;
  PATH *pp;
  CATPATH *pcp;
  RULE *pr;
  char str[STRMAX];
  double ref;
  VAR *var;

  /*** create circuit.metric ***/
  safe_sprintf(str,"METRIC(%s)",circuit->name);
  circuit->metric=new_var(str,0);

  /*** fill in formulaes for catpath.metric, accumulate to circuit.metric ***/
  for (i=0; i<circuit->catpaths->max; i++)
    {
    pcp=&circuit->catpaths->p.catpath[i];
    safe_sprintf(str,"PATHMETRIC(%s:%d)",circuit->name,i);
    pcp->metric=new_var(str,0);
    ref=0;
    for (j=0; j<pcp->paths->max; j++)
      {
      pp=pcp->paths->p.ppath[j];
      fmla_var(pcp->metric,pp->delay);
      if (j>0) fmla_func(pcp->metric,func_add);
      ref+=pp->reference;
      }
    fmla_constant(pcp->metric,ref);
    fmla_var(pcp->metric,Size.delay);
    fmla_func(pcp->metric,func_mul);
    fmla_func(pcp->metric,func_div);
    fmla_constant(pcp->metric,1.0);
    fmla_func(pcp->metric,func_sub);
    fmla_constant(pcp->metric,0.0);
    fmla_func(pcp->metric,func_max);
    fmla_func(pcp->metric,func_sqr);
    debug_var(pcp->metric);

    /*** add to metric ***/
    fmla_var(circuit->metric,pcp->metric);
    if (N>0) fmla_func(circuit->metric,func_add);
    N++;
    }

  /*** accumulate in setdelay constraints to circuit.metric ***/
  for (i=0; i<circuit->rules->max; i++)
    {
    pr=&circuit->rules->p.rule[i];
    if (pr->setdelay==0) continue;
    var=new_var("SETDEL",0);
    fmla_var(var,pr->delay);
    fmla_constant(var,pr->setdelay);
    fmla_var(var,Size.delay);
    fmla_func(var,func_mul);
    fmla_func(var,func_div);
    fmla_constant(var,1.0);
    fmla_func(var,func_sub);
    fmla_func(var,func_sqr);
    debug_var(var);

    /*** add to metric ***/
    fmla_var(circuit->metric,var);
    if (N>0) fmla_func(circuit->metric,func_add);
    N++;
    }

  /*** normalize circuit.metric by number of terms ***/
  if (N>0)
    {
    fmla_constant(circuit->metric,N);
    fmla_func(circuit->metric,func_div);
    }
  debug_var(circuit->metric);
  }

/*** evaluate worst tau or constraint metric ***/
double compute_metric()
  {
  int i;
  CIRCUIT *pcircuit;
  double m,T=0;
  int N=0;
  for (i=0; i<Size.circuits->max; i++)
    {
    pcircuit=Size.circuits->p.pcircuit[i];
    m=get(pcircuit->metric);
    pcircuit->delay=get(Size.delay);
    T+=m*pcircuit->instances;
    N+=pcircuit->instances;
    }
  return T/N;
  }

/*** debug path violations of a circuit ***/
void debug_circuit_metric(CIRCUIT *pcircuit)
  {
  int i,j;
  double del,ref,worst=1;
  CATPATH *pcp;
  PATH *pp;
  fprintf(stderr,"CONSTRAINT VIOLATIONS IN %s, Metric=%g:\n",
	  pcircuit->name,get(pcircuit->metric));
  for (i=0; i<pcircuit->catpaths->max; i++)
    {
    pcp=&pcircuit->catpaths->p.catpath[i];
    del=ref=0;
    for (j=0; j<pcp->paths->max; j++)
      {
      pp=pcp->paths->p.ppath[j];
      del+=get(pp->delay);
      ref+=pp->reference;
      }
    ref*=get(Size.delay);
    if (del>ref)
      {
      fprintf(stderr,"  DEL/REF=%g: ",del/ref);
      print_catpath(stderr,pcp);
      fprintf(stderr,"\n");
      worst=max(worst,del/ref);
      }
    }
  fprintf(stderr,"  WORST DEL/REF=%g IN %s\n",worst,pcircuit->name);
  }

/*** debug all violations of metric ***/
void debug_metrics()
  {
  int i;
  CIRCUIT *pcircuit;
  int N=Size.circuits->max;
  for (i=0; i<N; i++)
    {
    pcircuit=Size.circuits->p.pcircuit[i];
    if (get(pcircuit->metric)>0) debug_circuit_metric(pcircuit);
    }
  }

/*************************** Debugging/External formulae **********************/

void debug_formulae()
  {
  int i,j,n;
  FILE *fp;
  CIRCUIT *pcircuit;
  PATH *pp;
  NODETYPE *pnt;
  LIST *defined_vars;
  fp=fopen("size.fmla","wt");
  if (fp==NULL) return;
  defined_vars = list_create(sizeof(VAR *));

  /*** all formulae for loads ***/
  n=1;
  for (i=0; i<Size.nodetypes->max; i++)
    {
    pnt=&Size.nodetypes->p.nodetype[i];
    n=print_var_tree(fp,pnt->gateload,defined_vars,n);
    }

  /*** all formulae for path delays ***/
  for (i=0; i<Size.circuits->max; i++)
    {
    pcircuit=Size.circuits->p.pcircuit[i];
    for (j=0; j<pcircuit->paths->max; j++)
      {
      pp=&pcircuit->paths->p.path[j];
      n=print_var_tree(fp,pp->delay,defined_vars,n);
      }
    }
  fprintf(fp,"\n");

  /*** all path constraints ***/
  n=1;
  for (i=0; i<Size.circuits->max; i++)
    {
    pcircuit=Size.circuits->p.pcircuit[i];
    for (j=0; j<pcircuit->paths->max; j++)
      {
      pp=&pcircuit->paths->p.path[j];
      fprintf(fp,"constraint(%d) = var%p - %g;\n",n++,pp->delay,pp->reference*get(Size.delay));
      }
    }
  fprintf(fp,"\n");

  /*** energy expression ***/
  fprintf(fp,"energy = ...\n    ");
  for (i=0; i<Size.nodetypes->max; i++)
    {
    pnt=&Size.nodetypes->p.nodetype[i];
    if (i>0) fprintf(fp," ...\n  + ");
    fprintf(fp,"%5d * var%p",pnt->instances,pnt->gateload);
    }
  fprintf(fp,";\n");
  fclose(fp);
  list_free(defined_vars);
  }

/******************************* RELAXATION STUFF *************************************/

/*** reset all widths in a circuit ***/
void reset_widths(CIRCUIT *circuit)
  {
  int i;
  RULE *prule;
  for (i=0; i<circuit->rules->max; i++)
    {
    prule=&circuit->rules->p.rule[i];
    prule->max_width=4;
    prule->min_width=1e10;
    prule->min_pnt=NULL;
    prule->max_pnt=NULL;
    }
  }

/*** report max change in a rule's width ***/
double compute_delta(CIRCUIT *circuit)
  {
  int i;
  double x,delta=0;
  RULE *prule;
  for (i=0; i<circuit->rules->max; i++)
    {
    prule=&circuit->rules->p.rule[i];
    x=fabs(get(prule->width) - prule->max_width);
    delta=max(x,delta);
    if ((prule->max_width > get(prule->width))&&(prule->max_width > Size.max_width))
      {
      ERROR=1; fprintf(stderr,"ERROR: runaway transistor width in circuit %s:\n  ",
		       circuit->name);
      print_rule(stderr,*prule);
      return -1;
      }
    }
  return delta;
  }

/*** relax width ***/
void minmax_rule_width(RULE *prule, NODETYPE *pnt, double C)
  {
  double w,bias;
  if      (Size.BiasType==0) bias=rule_delaybias(prule);
  else if (Size.BiasType==1) bias=rule_delaybias(prule)*sqrt(0.5*prule->guard->max);
  else if (Size.BiasType==2) bias=prule->extrabias;
  else bias=1;
  w=C*rule_resistance(prule)/(get(Size.delay)*bias);
  if (w>prule->max_width) {prule->max_width=w; prule->max_pnt=pnt;}
  if (w<prule->min_width) {prule->min_width=w; prule->min_pnt=pnt;}
  }

/*** update new_width of rules relating to a nodetype ***/
void relax_nodetype(NODETYPE *pnt)
  {
  int i,j;
  RULE *prule;
  LOCALNODE ln;
  double load;
  load=get(pnt->maximum);
  for (i=0; i<pnt->localnodes->max; i++)
    {
    ln=pnt->localnodes->p.localnode[i];
    ln.pcircuit->delay=get(Size.delay);
    for (j=0; j<ln.pcircuit->rules->max; j++)
      {
      prule=&ln.pcircuit->rules->p.rule[j];
      if (strcmp(prule->target,ln.name)==0)
	{
        if ((pnt->localnodes->max>1)&&(prule->endpath==0)) prule->endpath=1;
	minmax_rule_width(prule,pnt,load);
        }
      }
    }
  }

/*** relaxation inner loop ***/
double inner_relax(int update)
  {
  int i,j;
  double x,delta=0;
  CIRCUIT *pcircuit;
  RULE *pr;

  /*** reset widths ***/
  for (i=0; i<Size.circuits->max; i++)
    reset_widths(Size.circuits->p.pcircuit[i]);

  /*** relax nodetypes between circuits ***/
  for (i=0; i<Size.nodetypes->max; i++)
    relax_nodetype(&Size.nodetypes->p.nodetype[i]);

  /*** compute deltas ***/
  for (i=0; i<Size.circuits->max; i++)
    {
    pcircuit=Size.circuits->p.pcircuit[i];
    if (pcircuit->fixed) continue;
    x=compute_delta(pcircuit);
    if (x==-1) return -1;
    delta=max(x,delta);
    }

  /*** assign rule.width=max_width ***/
  if (update) for (i=0; i<Size.circuits->max; i++)
    {
    pcircuit=Size.circuits->p.pcircuit[i];
    if (pcircuit->fixed) continue;
    for (j=0; j<pcircuit->rules->max; j++)
      {
      pr=&pcircuit->rules->p.rule[j];
      if (update) set(pr->lnw,log(pr->max_width));
      }
    }

  return delta;
  }

/*** set extrabias to delay/Size.delay ***/
void set_extrabias()
  {
  int i,j;
  CIRCUIT *pcircuit;
  RULE *pr;
  for (i=0; i<Size.circuits->max; i++)
    {
    pcircuit=Size.circuits->p.pcircuit[i];
    if (pcircuit->fixed==1) continue;
    for (j=0; j<pcircuit->rules->max; j++)
      {
      pr=&pcircuit->rules->p.rule[j];
      pr->extrabias=get(pr->delay)/get(Size.delay);
      }
    }
  }

/*** check for rules with widely different min_width/max_width ***/
void bifurcation_hints(double cutoff)
  {
  int i,j,banner;
  CIRCUIT *pcircuit;
  RULE *pr;

  /*** relax without changing widths ***/
  inner_relax(0);

  /*** search through circuits and rules for hints ***/
  for (i=0; i<Size.circuits->max; i++)
    {
    pcircuit=Size.circuits->p.pcircuit[i];
    banner=1;
    for (j=0; j<pcircuit->rules->max; j++)
      {
      pr=&pcircuit->rules->p.rule[j];
      if (pr->min_width*cutoff<pr->max_width)
	{
	if (banner) printf("SUGGEST BIFURCATION of %s since:\n",pcircuit->name);
	banner=0;
	printf("  %s%s has W=%g for nodetype\n    ",
	       pr->target,pr->dir?"+":"-",pr->min_width);
	print_nodetype(pr->min_pnt,1);
	printf("  %s%s has W=%g for nodetype\n    ",
	       pr->target,pr->dir?"+":"-",pr->max_width);
	print_nodetype(pr->max_pnt,1);
        }
      }
    }
  }

/********************** interface to minimize.c ********************/

void invalidate_sizing_variables()
  {
  CIRCUIT *pcircuit;
  RULE *pr;
  int i,j;
  for (i=0; i<Size.circuits->max; i++)
    {
    pcircuit=Size.circuits->p.pcircuit[i];
    if (pcircuit->fixed) continue;
    for (j=0; j<pcircuit->rules->max; j++)
      {
      pr=&pcircuit->rules->p.rule[j];
      if (pr->independent) invalidate(pr->lnw);
      }
    }
  }

int sizeof_sizing_state()
  {
  CIRCUIT *pcircuit;
  RULE *pr;
  int i,j,n=0;
  for (i=0; i<Size.circuits->max; i++)
    {
    pcircuit=Size.circuits->p.pcircuit[i];
    if (pcircuit->fixed) continue;
    for (j=0; j<pcircuit->rules->max; j++)
      {
      pr=&pcircuit->rules->p.rule[j];
      if (pr->independent) n++;
      }
    }
  return n;
  }

int unpack_sizing_state(double *p)
  {
  CIRCUIT *pcircuit;
  RULE *pr;
  double lnw;
  int i,j,n=0;
  for (i=0; i<Size.circuits->max; i++)
    {
    pcircuit=Size.circuits->p.pcircuit[i];
    if (pcircuit->fixed) continue;
    for (j=0; j<pcircuit->rules->max; j++)
      {
      pr=&pcircuit->rules->p.rule[j];
      if (pr->independent)
	{
	lnw=min(p[n],Size.max_lnw);
	lnw=max(lnw,Size.min_lnw);
	set(pr->lnw,lnw);
	n++;
        }
      }
    }
  return n;
  }

int pack_sizing_state(double *p)
  {
  CIRCUIT *pcircuit;
  RULE *pr;
  int i,j,n=0;
  for (i=0; i<Size.circuits->max; i++)
    {
    pcircuit=Size.circuits->p.pcircuit[i];
    if (pcircuit->fixed) continue;
    for (j=0; j<pcircuit->rules->max; j++)
      {
      pr=&pcircuit->rules->p.rule[j];
      if (pr->independent) p[n++]=get(pr->lnw);
      }
    }
  return n;
  }

double sizing_energy(PARAMETERS *parms, double *p)
  {
  double E,T;
  unpack_sizing_state(p);
  E=average_switching_cap(1);
  T=compute_metric();
  return E + Size.constraint_weight*T; /* weighted combination */
  }

void sizing_down(PARAMETERS *parms, double *p, double *d)
  {
  double TINY=1e-6;
  int i,N;
  double E0,E,op;
  E0=sizing_energy(parms,p);
  N=sizeof_sizing_state();
  for (i=0; i<N; i++)
    {
    op=p[i];
    p[i]+=TINY;
    E=sizing_energy(parms,p);
    d[i]=(E0-E)/TINY;
    p[i]=op;
    }
  }

/******************************* MAIN SIZE ROUTINE ****************************/

/*** sort circuit *'s by instances ***/
int pcircuit_instances_cmp(void *p1, void *p2)
  {
  return (*(CIRCUIT **)p2)->instances - (*(CIRCUIT **)p1)->instances;
  }

/*** sizing setup ***/
void sizing_setup()
  {
  int i;
  static int has_been_setup=0;
  if (has_been_setup) return;
  has_been_setup=1;
  create_nodetypes();
  free_instances();
  free_aliases();
  free_labels();
  identify_pins();
  complete_localnodes();
  for (i=0; i<Size.circuits->max; i++)
    {
    compute_internal_gate_loads(Size.circuits->p.pcircuit[i]);
    compute_rule_delays(Size.circuits->p.pcircuit[i]);
    }
  for (i=0; i<Size.nodetypes->max; i++)
    compute_load_on_nodetype(&Size.nodetypes->p.nodetype[i]);
  for (i=0; i<Size.nodetypes->max; i++)
    relax_nodetype(&Size.nodetypes->p.nodetype[i]);
  for (i=0; i<Size.circuits->max; i++) find_paths(Size.circuits->p.pcircuit[i]);
  concatenate_paths(); /* concatenate catpaths to following paths */
  for (i=0; i<Size.circuits->max; i++) create_path_metrics(Size.circuits->p.pcircuit[i]);
  for (i=0; i<Size.circuits->max; i++) create_metric(Size.circuits->p.pcircuit[i]);
  }

/*** optimize sizes with relaxation ***/
void relax(double tolerance)
  {
  int count=0;
  double delta=0;
  sizing_setup();
  do
    {
    delta=inner_relax(1);
    printf("Relaxing: iteration %d, delta=%g\n",count,delta);
    count++;
    } while (delta>tolerance);
  }

/*** save files, print information ***/
void snapshot()
  {
  set_extrabias();
  save_circuits();
  print_info();
  }

/*** switch temporarily fixed and unfixed circuits ***/
void toggle_fixed()
  {
  int i;
  CIRCUIT *pcircuit;
  for (i=0; i<Size.circuits->max; i++)
    {
    pcircuit=Size.circuits->p.pcircuit[i];
    pcircuit->fixed=2-pcircuit->fixed;
    }
  }

#if 0
/*** optimize sizes with minimization ***/
void size(int noisy)
  {
  int i,N;
  double *p,w,ln_shrink;
  sizing_setup();
  signal(SIGINT,snapshot); /* Ctrl-C forces a snapshot */
  if (noisy) snapshot();
  N=sizeof_sizing_state();
  p=(double *) leak_malloc(sizeof(double)*N);
  pack_sizing_state(p);
  printf("N=%d\n",N);
  ln_shrink=log(Size.shrink_factor);
  for (w=Size.initial_weight; w<Size.final_weight; w*=Size.step_weight)
    {
    printf("Shrinking widths by %g\n",Size.shrink_factor);
    for (i=0; i<N; i++) p[i]+=ln_shrink; /* w:=shrink_factor*w */
    printf("Constraint weight=%g\n",w);
    Size.constraint_weight=w;
    minimize(stdout,NULL,(ENERGY *)&sizing_energy,(DOWN *)&sizing_down,N,p,ceil(N*Size.iteration_ratio),1e-10,0);
    if (noisy) snapshot();
    }
  printf("Cleanup, Constraint weight=%g\n",Size.final_weight);
  Size.constraint_weight=Size.final_weight;
  minimize(stdout,NULL,(ENERGY *)&sizing_energy,(DOWN *)&sizing_down,N,p,ceil(N*Size.cleanup_ratio),1e-10,0);
  if (noisy) snapshot();
  unpack_sizing_state(p);
  leak_free(p);
  set_extrabias();
  signal(SIGINT,SIG_DFL); /* Ctrl-C back to normal */
  }
#else
/*** optimize sizes with peicewise minimization ***/
void size(int noisy)
  {
  int i,N;
  double *p,w;
  CIRCUIT *pcircuit;
  sizing_setup();
  signal(SIGINT,snapshot); /* Ctrl-C forces a snapshot */
  toggle_fixed();
  if (noisy) snapshot();
  for (w=Size.initial_weight; w<Size.final_weight; w*=Size.step_weight)
    {
    Size.constraint_weight=w;
    for (i=0; i<Size.circuits->max; i++) /*** size all circuits independently ***/
      {
      pcircuit=Size.circuits->p.pcircuit[i];
      if (pcircuit->fixed==2) pcircuit->fixed=0;
      else continue;
      N=sizeof_sizing_state();
      p=(double *) leak_malloc(sizeof(double)*N);
      pack_sizing_state(p);
      printf("Sizing %s, N=%d, Constraint weight=%g\n",pcircuit->name,N,w);
      minimize(stdout,NULL,(ENERGY *)&sizing_energy,(DOWN *)&sizing_down,N,p,ceil(N*Size.iteration_ratio),1e-10,0);
      unpack_sizing_state(p);
      leak_free(p);
      pcircuit->fixed=2;
      }
    if (noisy) snapshot();
    }
  toggle_fixed();
  set_extrabias();
  signal(SIGINT,SIG_DFL); /* Ctrl-C back to normal */
  }
#endif

/*** golden section search for minimum Ett ***/
void sweep(double a, double c, double inc)
  {
  double b,fa,fb,fc,t,ft,STEP=0.38197;
  b=(c-a)*STEP+a;
  set(Size.delay,a); size(0); fa=print_info();
  set(Size.delay,b); size(0); fb=print_info();
  set(Size.delay,c); size(0); fc=print_info();
  while(fabs(a-c)>inc)
    {
    if (fabs(b-a)>fabs(c-b))
      {
      t=b+(a-b)*STEP;
      set(Size.delay,t); size(0); ft=print_info();
      if (ft<fb) {c=b; fc=fb; b=t; fb=ft;}
      else {a=t; fa=ft;}
      }
    else
      {
      t=b+(c-b)*STEP;
      set(Size.delay,t); size(0); ft=print_info();
      if (ft<fb) {a=b; fa=fb; b=t; fb=ft;}
      else {c=t; fc=ft;}
      }
    }
  }

/********************** PARSE SIZING COMMANDS ******************************/

void free_aliases()
  {
  int i;
  ALIAS *pal;
  if (Size.aliases!=NULL)
    {
    for (i=0; i<Size.aliases->max; i++)
      {
      pal=Size.aliases->p.palias[i];
      if (pal->localnodes!=NULL) list_free(pal->localnodes);
      leak_free(pal->name);
      leak_free(pal);
      }
    list_free(Size.aliases);
    Size.aliases=NULL;
    }
  }

void free_instances()
  {
  int i;
  if (Size.instances!=NULL)
    {
    for (i=0; i<Size.instances->max; i++)
      leak_free(Size.instances->p.instance[i].name);
    list_free(Size.instances);
    Size.instances=NULL;
    }
  }

void free_labels()
  {
  if (Size.labels!=NULL)
    {
    list_free(Size.labels);
    Size.labels=NULL;
    }
  }

void free_nodetypes()
  {
  int i;
  if (Size.nodetypes!=NULL)
    {
    for (i=0; i<Size.nodetypes->max; i++)
      list_free(Size.nodetypes->p.nodetype[i].localnodes);
    list_free(Size.nodetypes);
    Size.nodetypes=NULL;
    }
   }

void free_circuits()
  {
  int i;
  if (Size.circuits!=NULL)
    {
    for (i=0; i<Size.circuits->max; i++)
      free_circuit(Size.circuits->p.pcircuit[i]);
    list_free(Size.circuits);
    Size.circuits=NULL;
    }
  }

/*** free lists used for sizing ***/
void clear_sizing_data()
  {
  free_aliases();
  free_instances();
  free_labels();
  free_nodetypes();
  free_circuits();
  }

/*** some user defined recalc functions ***/
double square(double x) {return x*x;}
double smooth_max(double x, double y) {return max(x,y);}

/*** set defaults ***/
void default_sizing_options()
  {
  int i;

  /*** N resistances ***/
  Size.Nmax=7;
  Size.Nresistance[0]=0;
  Size.Nresistance[1]=8500;
  Size.Nresistance[2]=17000;
  Size.Nresistance[3]=25500;
  Size.Nresistance[4]=34000;
  Size.Nresistance[5]=42500;
  Size.Nresistance[6]=51000;
  Size.Nresistance[7]=59500;
  for (i=0; i<=Size.Nmax; i++) Size.Ndelaybias[i]=1;

  /*** P resistances ***/
  Size.Pmax=3;
  Size.Presistance[0]=0;
  Size.Presistance[1]=18000;
  Size.Presistance[2]=36000;
  Size.Presistance[3]=54000;
  for (i=0; i<=Size.Pmax; i++) Size.Pdelaybias[i]=1;

  /*** other settings ***/
  Size.mincap=20e-15;
  Size.wirecap=0.04e-15;
  Size.wireres=0;
  Size.gatecap=0.65e-15;
  Size.extcap=1e-15;
  Size.xscale=1;
  Size.yscale=1;
  Size.delay=new_var("DELAY",45e-12);
  Size.max_width=20000;
  Size.min_width=8;
  Size.max_lnw=log(Size.max_width);
  Size.min_lnw=log(Size.min_width);
  Size.initial_weight=1;
  Size.final_weight=1e8;
  Size.step_weight=100;
  Size.shrink_factor=1.0;
  Size.iteration_ratio=0.05;
  Size.cleanup_ratio=0.25;
  Size.BiasType=0; /*** delaybias sqrt(seriesgates) applies only to relax ***/

  /*** initialize lists ***/
  Size.circuits=NULL;
  Size.instances=NULL;
  Size.aliases=NULL;
  Size.nodetypes=NULL;

  /*** allocate recalc functions ***/
  func_add=new_func(OPADD,"+",NULL);
  func_sub=new_func(OPSUB,"-",NULL);
  func_mul=new_func(OPMUL,"*",NULL);
  func_div=new_func(OPDIV,"/",NULL);
  func_max=new_func(OPMAX,"MAX",NULL);
  func_min=new_func(OPMIN,"MIN",NULL);
  func_sqr=new_func(OPFUNC1,"SQR",(void *)(&square));
  func_exp=new_func(OPFUNC1,"EXP",(void *)(&exp));
  }

/*** dibugging ***/
void debug_sizing_options()
  {
  int i;
  printf("SIZING OPTIONS:\n");
  printf("mincap=%g\n",Size.mincap);
  printf("extcap=%g\n",Size.extcap);
  printf("gatecap=%g\n",Size.gatecap);
  printf("wirecap=%g wireres=%g xscale=%g yscale=%g\n",
	 Size.wirecap,Size.wireres,Size.xscale,Size.yscale);
  printf("delay=%g\n",get(Size.delay));
  printf("max_width=%g max_lnw=%g\n",Size.max_width,Size.max_lnw);
  printf("min_width=%g min_lnw=%g\n",Size.min_width,Size.min_lnw);
  printf("Nresistance=");
  for (i=1; i<=Size.Nmax; i++) printf("%g ",Size.Nresistance[i]);
  printf("\n");
  printf("Presistance=");
  for (i=1; i<=Size.Pmax; i++) printf("%g ",Size.Presistance[i]);
  printf("\n");
  printf("Ndelaybias=");
  for (i=1; i<=Size.Nmax; i++) printf("%g ",Size.Ndelaybias[i]);
  printf("\n");
  printf("Pdelaybias=");
  for (i=1; i<=Size.Pmax; i++) printf("%g ",Size.Pdelaybias[i]);
  printf("\n");
  }

/*** parse an array of reals for resistance/delaybias ***/
void parse_series_parameters(LEX *lex, double *x, int *max)
  {
  int i=1;
  do
    {
    x[i++]=lex_eat_real(lex);
    } while(lex_is_real(lex));
  *max=i-1;
  }

/*** parse sizing related commands ***/
int parse_sizing_options(LEX *lex, int argc, char *argv[])
  {
  char fn[STRMAX];

  /*** set sizing parameters ***/
  if      (lex_eatif_keyword(lex,"delay"))
    set(Size.delay,lex_eat_real_arg(lex,argc,argv));
  else if (lex_eatif_keyword(lex,"mincap"))
    Size.mincap=lex_eat_real_arg(lex,argc,argv);
  else if (lex_eatif_keyword(lex,"extcap"))
    Size.extcap=lex_eat_real_arg(lex,argc,argv);
  else if (lex_eatif_keyword(lex,"gatecap"))
    Size.gatecap=lex_eat_real_arg(lex,argc,argv);
  else if (lex_eatif_keyword(lex,"wirecap"))
    Size.wirecap=lex_eat_real_arg(lex,argc,argv);
  else if (lex_eatif_keyword(lex,"wireres"))
    Size.wireres=lex_eat_real_arg(lex,argc,argv);
  else if (lex_eatif_keyword(lex,"xscale"))
    Size.xscale=lex_eat_real_arg(lex,argc,argv);
  else if (lex_eatif_keyword(lex,"yscale"))
    Size.yscale=lex_eat_real_arg(lex,argc,argv);
  else if (lex_eatif_keyword(lex,"initial_weight"))
    Size.initial_weight=lex_eat_real_arg(lex,argc,argv);
  else if (lex_eatif_keyword(lex,"final_weight"))
    Size.final_weight=lex_eat_real_arg(lex,argc,argv);
  else if (lex_eatif_keyword(lex,"step_weight"))
    Size.step_weight=lex_eat_real_arg(lex,argc,argv);
  else if (lex_eatif_keyword(lex,"shrink_factor"))
    Size.shrink_factor=lex_eat_real_arg(lex,argc,argv);
  else if (lex_eatif_keyword(lex,"iteration_ratio"))
    Size.iteration_ratio=lex_eat_real_arg(lex,argc,argv);
  else if (lex_eatif_keyword(lex,"cleanup_ratio"))
    Size.cleanup_ratio=lex_eat_real_arg(lex,argc,argv);
  else if (lex_eatif_keyword(lex,"BiasType"))
    Size.BiasType=(int)lex_eat_real_arg(lex,argc,argv);
  else if (lex_eatif_keyword(lex,"max_width"))
    {
    Size.max_width=lex_eat_real_arg(lex,argc,argv);
    Size.max_lnw=log(Size.max_width);
    }
  else if (lex_eatif_keyword(lex,"min_width"))
    {
    Size.min_width=lex_eat_real_arg(lex,argc,argv);
    Size.min_lnw=log(Size.min_width);
    }
  else if (lex_eatif_keyword(lex,"Nresistance"))
    parse_series_parameters(lex,Size.Nresistance,&Size.Nmax);
  else if (lex_eatif_keyword(lex,"Presistance"))
    parse_series_parameters(lex,Size.Presistance,&Size.Pmax);
  else if (lex_eatif_keyword(lex,"Ndelaybias"))
    parse_series_parameters(lex,Size.Ndelaybias,&Size.Nmax);
  else if (lex_eatif_keyword(lex,"Pdelaybias"))
    parse_series_parameters(lex,Size.Pdelaybias,&Size.Pmax);
  else if (lex_eatif_keyword(lex,"sizing_options"))
    debug_sizing_options();

  /*** debugging information ***/
  else if (lex_eatif_keyword(lex,"debug_circuits"))  debug_circuits();
  else if (lex_eatif_keyword(lex,"debug_instances")) debug_instances();
  else if (lex_eatif_keyword(lex,"debug_aliases"))   debug_aliases();
  else if (lex_eatif_keyword(lex,"debug_nodetypes")) debug_nodetypes(lex);
  else if (lex_eatif_keyword(lex,"debug_metric"))    debug_metrics();
  else if (lex_eatif_keyword(lex,"debug_type_aliases")) debug_type_aliases();
  else if (lex_eatif_keyword(lex,"debug_formulae"))
    {
    sizing_setup();
    debug_formulae();
    }
 
  /*** output information ***/
  else if (lex_eatif_keyword(lex,"save_circuits"))      save_circuits();
  else if (lex_eatif_keyword(lex,"bifurcation_hints"))
    bifurcation_hints(lex_eat_real_arg(lex,argc,argv));
  else if (lex_eatif_keyword(lex,"print_caps"))         print_caps(0);
  else if (lex_eatif_keyword(lex,"print_aspice_caps"))  print_caps(1);
  else if (lex_eatif_keyword(lex,"switching_cap"))      print_info();

  /*** read in necessary information, update data structures ***/
  else if (lex_eatif_keyword(lex,"clear_sizing_data"))
    clear_sizing_data();
  else if (lex_eatif_keyword(lex,"read_connect"))
    {
    safe_sprintf(fn,"%s.connect",lex_eat_quote_arg(lex,argc,argv));
    parse_connect(fn);
    }
  else if (lex_eatif_keyword(lex,"estimate_cap"))
    {
    fprintf(stderr,"Estimating R/C from spanning trees\n");
    spanning_trees(Size.labels,Size.wirecap,Size.wireres);
    }
  else if (lex_eatif_keyword(lex,"read_cap"))
    {
    safe_sprintf(fn,"%s.cap",lex_eat_quote_arg(lex,argc,argv));
    if (parse_cap(fn)) fprintf(stderr,"Reading R/C from %s\n",fn);
    else
      {
      fprintf(stderr,"Estimating R/C from spanning trees\n");
      spanning_trees(Size.labels,Size.wirecap,Size.wireres);
      }
    }
  else if (lex_eatif_keyword(lex,"read_magic"))
    {
    parse_instances(0,lex_eat_quote_arg(lex,argc,argv));
    }
  else if (lex_eatif_keyword(lex,"read_instances"))
    {
    parse_instances(1,lex_eat_quote_arg(lex,argc,argv));
    }
  else if (lex_eatif_keyword(lex,"debug_labels"))
    {
    debug_labels(Size.labels);
    }

  /*** do a sizing relaxation ***/
  else if (lex_eatif_keyword(lex,"relax")) relax(lex_eat_real_arg(lex,argc,argv));
  else if (lex_eatif_keyword(lex,"size"))  size(1);
  else if (lex_eatif_keyword(lex,"sweep"))
    {
    double a,c,inc;
    a=lex_eat_real_arg(lex,argc,argv);
    c=lex_eat_real_arg(lex,argc,argv);
    inc=lex_eat_real_arg(lex,argc,argv);
    sweep(a,c,inc);
    }

  /*** return values ***/
  else return 0;
  return 1;
  }
