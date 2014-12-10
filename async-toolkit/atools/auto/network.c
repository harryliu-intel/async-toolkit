#include "auto.h"
#undef DEBUG_NETWORK
#define SHARED_BY_MAXWIDTH

/*** compare dags by name ***/
int pdag_cmp(void *p1, void *p2)
  {
  DAG *pdag1=*((DAG **)p1),*pdag2=*((DAG **)p2);
  return strcmp(pdag1->name,pdag2->name);
  }

/*** compare dag against a name ***/
int pdag_name_cmp(void *p1, void *p2)
  {
  DAG *pdag=*((DAG **)p1);
  char *pc=(char *)p2;
  return strcmp(pdag->name,pc);
  }

/*** find ultimate root alias of a DAG node (can't shorted linked list!) ***/
DAG *root_alias(DAG *dag)
  {
  if (dag->alias==NULL) return dag;
  else return root_alias(dag->alias);
  }

/*** create a dag node ***/
DAG *create_dag(LIST *dags, char *name, int terminal)
  {
  DAG *dag;
  int i;

  /*** try to find existing dag ***/
  i=find_element_lazy_sort(dags,name,&pdag_name_cmp);
  if (i>=0) return dags->p.pdag[i];

  /*** create a new one ***/
  dag=(DAG *) leak_malloc(sizeof(DAG));
  dag->branches=list_create(sizeof(BRANCH));
  dag->name=name;
  dag->alias=NULL;
  dag->mark=0;
  dag->terminal=terminal;
  list_insert_element_lazy_sort(dags,&dag,&pdag_cmp);
  return dag;
  }

/*** free dags via the linked list (leave width VARs!) ***/
void free_dags(LIST *dags)
  {
  int i;
  DAG *dag;
  for (i=0; i<dags->max; i++)
    {
    dag=dags->p.pdag[i];
    list_free(dag->branches);
    leak_free(dag);
    }
  list_free(dags);
  }

/*** compare two branches by backward/reverse, then gate name ***/
int branch_gate_cmp(void *p1, void *p2)
  {
  int c;
  BRANCH *pbr1,*pbr2;
  pbr1=(BRANCH *)p1;
  pbr2=(BRANCH *)p2;
  if ((!pbr1->backward)&&(pbr2->backward)) return -1;
  if ((pbr1->backward)&&(!pbr2->backward)) return +1;
  if ((!pbr1->reverse)&&(pbr2->reverse)) return -1;
  if ((pbr1->reverse)&&(!pbr2->reverse)) return +1;
  c = strcmp(pbr1->gate,pbr2->gate);
  if (c!=0) return c;
#ifndef SHARED_BY_MAXWIDTH
  c = pbr1->width - pbr2->width;
#endif
  return c;
  }

/*** compare two branches by backward/reverse, then gate name, then root alias ***/
int branch_cmp(void *p1, void *p2)
  {
  int c;
  BRANCH *pbr1,*pbr2;
  pbr1=(BRANCH *)p1;
  pbr2=(BRANCH *)p2;
  c=branch_gate_cmp(pbr1,pbr2);
  if (c!=0) return c;
  return strcmp(root_alias(pbr1->dag)->name,root_alias(pbr2->dag)->name);
  }

/*** add a new branch to a dag node ***/
void add_branch(DAG *dag, DAG *target, char *gate, VAR *width, int reverse, int backward)
  {
  BRANCH branch;
  branch.gate=gate;
  branch.dag=target;
  branch.reverse=reverse;
  branch.backward=backward;
  branch.width=width;
  list_append_element(dag->branches,&branch);
  }

/*** convert a dag's branches into canonical form ***/
void canonical_branches(DAG *dag)
  {
  int i;
  BRANCH *pbr1,*pbr2;
  VAR *width;
  if (dag->alias!=NULL) return;
  list_sort(dag->branches,&branch_cmp);
  for (i=1; i<dag->branches->max; i++)
    {
    pbr1=&dag->branches->p.branch[i-1];
    pbr2=&dag->branches->p.branch[i];
    if (branch_cmp(pbr1,pbr2)==0)
      {
#ifdef SHARED_BY_MAXWIDTH
      width=new_var("BRW",0);
      fmla_var(width,pbr1->width);
      fmla_var(width,pbr2->width);
      fmla_func(width,func_max);
#else
      width=pbr1->width;
#endif
#ifdef DEBUG_NETWORK
      printf("merging branches from %s to %s via %s, width=max(%g,%g)=%g\n",
	     dag->name,pbr1->dag->name,pbr1->gate,get(pbr1->width),get(pbr2->width),get(width));
#endif
      pbr1->width=width;
      list_remove_element(dag->branches,i--);
      }
    }
  }

/*** add a rule to DAG graph (no gate sharing yet) ***/
void add_rule_to_dags(LIST *dags, DAG *dag, RULE *rule)
  {
  int i;
  char *gate;
  DAG *newdag;
  for (i=0; i<rule->guard->max; i++)
    {
    gate=rule->guard->p.pc[i];
    if (i<rule->guard->max-1) newdag=create_dag(dags,unique_name(),0);
    else                      newdag=create_dag(dags,rule->target,1);
    add_branch(dag,newdag,gate,rule->width,rule->reverse,0);
    add_branch(newdag,dag,gate,rule->width,rule->reverse,1);
    dag=newdag;
    }
  }

/*** check if two nodes are exclusive ***/
int is_exclusive(char *targ1, char *targ2, int dir, LIST *properties)
  {
  PROPERTY prop;
  int i,e1,e2;
  if (strcmp(targ1,targ2)==0) return 0; /* not exclusive with itself */
  for (i=0; i<properties->max; i++)
    {
    prop=properties->p.prop[i];
    if (prop.sense==dir) continue; /* wrong direction */
    e1=find_element(prop.items,&targ1,&str_cmp);
    e2=find_element(prop.items,&targ2,&str_cmp);
    if ((e1>=0)&&(e2>=0)) return 1; /* found a property which says targ1/targ2 are exclusive */
    }
  return 0;
  }

/*** check new gate for exclusion with any of the gates in guard ***/
int check_exclusion(LIST *guard, char *gate, int dir, LIST *properties)
  {
  int i;
  for (i=0; i<guard->max; i++)
    if (is_exclusive(guard->p.pc[i],gate,dir,properties)) return 1;
  return 0;
  }

/*** compare two rules by dir, target, then sorted guards ***/
int rule_cmp(void *p1, void *p2)
  {
  RULE *pr1=((RULE *)p1),*pr2=((RULE *)p2);
  LIST *g1,*g2;
  int c;
  if (pr1->dir<pr2->dir) return -1;
  if (pr2->dir<pr1->dir) return +1;
  c=strcmp(pr1->target,pr2->target);
  if (c!=0) return c;
  g1=list_dup(pr1->guard);
  g2=list_dup(pr2->guard);
  list_sort(g1,&str_cmp);
  list_sort(g2,&str_cmp);
  c=list_compare(g1,g2,&str_cmp);
  list_free(g1);
  list_free(g2);
  return c;
  }

/*** recursively verify dags against rules ***/
int recursive_dag_verify(LIST *rules, DAG *dag, RULE *pr, LIST *properties)
  {
  BRANCH *pbr;
  DAG *newdag;
  int i,j,ok=1;
  dag->mark=1;
  for (i=0; ok&&(i<dag->branches->max); i++)
    {
    pbr=&dag->branches->p.branch[i];
    newdag=root_alias(pbr->dag);
    if (newdag->mark) continue; /* newdag already visited */
    if (check_exclusion(pr->guard,pbr->gate,pr->dir,properties)) continue; /* this path is exclusive */
    list_append_element(pr->guard,&pbr->gate);
    if (newdag->terminal==1)
      {
#ifdef DEBUG_NETWORK
      printf("SNEAK ");
      print_rule(stdout,*pr);
#endif
      ok=0; /* reached another shared node via sneak path */
      }
    else if (newdag->terminal==2) /* reached a power supply */
      {
      for (j=0; j<rules->max; j++) if (rule_cmp(&rules->p.rule[j],pr)==0) break;
      if (j==rules->max) ok=0; /* an extra term */
#ifdef DEBUG_NETWORK
      printf("%s ",ok ? "OK" : "BAD");
      print_rule(stdout,*pr);
#endif
      }
    else ok=recursive_dag_verify(rules,newdag,pr,properties);
    list_remove_element(pr->guard,pr->guard->max-1);
    }
  dag->mark=0;
  return ok;
  }

/*** verify that all terms in dag are in original rules (and no sneak paths) ***/
int verify_dags(LIST *dags, LIST *rules, int dir, LIST *properties)
  {
  int i,ok=1;
  DAG *dag;
  RULE rule;
  for (i=0; ok&&(i<dags->max); i++)
    {
    dag=dags->p.pdag[i];
    if (dag->alias!=NULL) continue;
    if (dag->terminal!=1) continue;
    rule.target=dag->name;
    rule.dir=dir;
    rule.guard=list_create(sizeof(char *));
    rule.width=NULL;
    ok=recursive_dag_verify(rules,dag,&rule,properties);
    list_free(rule.guard);
    }
  return ok;
  }

/*** try to merge a specified pair of dags ***/
int try_merge(LIST *dags, DAG *pdag1, DAG *pdag2, LIST *rules, int dir, LIST *properties)
  {
  DAG old1,old2;
  int ok;

  /*** find root aliases, save old state ***/
  pdag1=root_alias(pdag1);
  pdag2=root_alias(pdag2);
  old1=*pdag1; old1.branches=list_dup(pdag1->branches);
  old2=*pdag2; old2.branches=list_dup(pdag2->branches);

  /*** merge the dags ***/
  pdag2->alias=pdag1;
  list_append_list(pdag1->branches,pdag2->branches);
  list_realloc(pdag2->branches,0);

  /*** verify the dag network, free or restore old state ***/
  ok=verify_dags(dags,rules,dir,properties);
  if (ok)
    {
    list_free(old1.branches);
    list_free(old2.branches);
#ifdef DEBUG_NETWORK
    printf("merging %s and %s\n",pdag1->name,pdag2->name);
#endif
    }
  else
    {
    list_free(pdag1->branches);
    list_free(pdag2->branches);
    *pdag1=old1;
    *pdag2=old2;
    }

  return ok;
  }

/*** try merging up each dags' branches ***/
void merge_dags(LIST *dags, LIST *rules, int dir, LIST *properties)
  {
  int i,j,progress;
  DAG *pdag;
  BRANCH *pbr1,*pbr2;
  do
    {
    progress=0;
    for (i=0; i<dags->max; i++)
      {
      pdag=dags->p.pdag[i];
      if (pdag->alias!=NULL) continue;
      canonical_branches(pdag);
      for (j=1; j<pdag->branches->max; j++)
	{
	pbr1=&pdag->branches->p.branch[j-1];
	pbr2=&pdag->branches->p.branch[j];
	if (branch_gate_cmp(pbr1,pbr2)==0)
	  if (try_merge(dags,pbr1->dag,pbr2->dag,rules,dir,properties))
	    {progress++; break;}
	}
      }
    } while (progress);
  }

/*** convert the DAG graph into a list of gates ***/
LIST *dag_gates(LIST *dags, int dir)
  {
  int i,j;
  BRANCH *pbr;
  LIST *gates;
  DAG *dag;
  GATE gate;
  gates=list_create(sizeof(GATE));
  for (i=0; i<dags->max; i++)
    {
    dag=dags->p.pdag[i];
    if (dag->alias!=NULL) continue;
    for (j=0; j<dag->branches->max; j++)
      {
      pbr=&dag->branches->p.branch[j];
      if (pbr->backward) continue;
      gate.s=dag->name;
      gate.g=pbr->gate;
      gate.d=root_alias(pbr->dag)->name;
      gate.dir=dir;
      gate.width=pbr->width;
      list_append_element(gates,&gate);
      }
    }
  return gates;
  }

/*** generate minimal gate network for exclusive rules ***/
LIST *minimal_gate_network(LIST *rules, LIST *properties)
  {
  LIST *dags,*gates;
  DAG *root;
  int i,dir;

  /*** info about rules ***/
  if (rules->max<1) return NULL;
  dir=rules->p.rule[0].dir;

#ifdef DEBUG_NETWORK
  printf("Shared Rules:\n");
  for (i=0; i<rules->max; i++) print_rule(stdout,rules->p.rule[i]);
#endif

  /*** create dag graph from rules ***/
  dags=list_create(sizeof(DAG *));
  root=create_dag(dags,dir ? VDD : GND,2);
  for (i=0; i<rules->max; i++) add_rule_to_dags(dags,root,&rules->p.rule[i]);

  /*** merging up network ***/
  merge_dags(dags,rules,dir,properties);

  /*** make the gates ***/
  gates=dag_gates(dags,dir);

  /*** free memory and return ***/
  free_dags(dags);
  return gates;
  }
