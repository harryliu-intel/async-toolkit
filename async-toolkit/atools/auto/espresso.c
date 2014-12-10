#include "auto.h"

/*** allocate memory for a Kcube ***/
short *allocate_Kcube(LIST *properties)
  {
  short *Kcube;
  int i,size=1;
  for (i=0; i<properties->max; i++) size*=properties->p.prop[i].items->max;
  Kcube=(short *) leak_malloc(size*sizeof(short));
  if (Kcube==NULL) error("Kcube too big");
  for (i=0; i<size; i++) Kcube[i]=0;
  return Kcube;
  }

/*** add coverage count to Kcube, return minimum coverage count of rule ***/
int Kcube_term(short *Kcube, LIST *properties, RULE *pr, int add, int d, int pos)
  {
  PROPERTY *pprop;
  int i,j,val;

  /*** leaf case ***/
  if (d>=properties->max)
    {
    Kcube[pos]+=add;
    return Kcube[pos];
    }

  /*** recurse to next dimension of Kcube ***/
  pprop=&properties->p.prop[d];
  for (i=0; i<pprop->items->max; i++) for (j=0; j<pr->guard->max; j++)
    if (pprop->items->p.pc[i]==pr->guard->p.pc[j])
      return Kcube_term(Kcube,properties,pr,add,d+1,pprop->items->max*pos+i);

  /*** recurse via all choices of current dimension ***/
  val=255;
  for (i=0; i<pprop->items->max; i++)
    {
    j=Kcube_term(Kcube,properties,pr,add,d+1,pprop->items->max*pos+i);
    val=min(j,val);
    }
  return val;
  }

/*** test if a guard is mentioned in properties list ***/
int guard_in_properties(LIST *properties, char *pg)
  {
  int i,j;
  PROPERTY *pprop;
  for (i=0; i<properties->max; i++)
    {
    pprop=&properties->p.prop[i];
    for (j=0; j<pprop->items->max; j++)
      if (pg==pprop->items->p.pc[j]) return 1;
    }
  return 0;
  }

/*** check if a property is relevant to a set of rules ***/
int property_has_guard(PROPERTY *pprop, LIST *rules)
  {
  RULE *pr;
  char *pg;
  int i,j,k;
  for (i=0; i<rules->max; i++)
    {
    pr=&rules->p.rule[i];
    for (j=0; j<pr->guard->max; j++)
      {
      pg=pr->guard->p.pc[j];
      for (k=0; k<pprop->items->max; k++)
	if (pg==pprop->items->p.pc[k]) return 1;
      }
    }
  return 0;
  }

/*** compare exclusion properties ***/
int prop_cmp(void *p1, void *p2)
  {
  PROPERTY *pprop1=(PROPERTY *)p1, *pprop2=(PROPERTY *)p2;
  if (pprop1->sense<pprop2->sense) return -1;
  if (pprop1->sense>pprop2->sense) return +1;
  return list_compare(pprop1->items,pprop2->items,&str_cmp);
  }

/*** optimize rules with same target:dir ***/
void optimize_logic(LIST *rules, LIST *never, LIST *original_properties)
  {
  int i,j,coverage;
  RULE *pr;
  char *pg;
  short *Kcube;
  LIST *properties;
  PROPERTY *pprop;

  /*** create short list of unique relevant properties ***/
  properties=list_create(sizeof(PROPERTY));
  for (i=0; i<original_properties->max; i++)
    {
    pprop=&original_properties->p.prop[i];
    if (property_has_guard(pprop,rules)||property_has_guard(pprop,never))
      if (find_element_lazy_sort(properties,pprop,&prop_cmp)<0)
	list_insert_element_lazy_sort(properties,pprop,&prop_cmp);
    }

  /*** allocate multirail K-cube ***/
  Kcube=allocate_Kcube(properties);
  
  /*** fill in Kcube with seed terms ***/
  for (i=0; i<rules->max; i++)
    Kcube_term(Kcube,properties,&rules->p.rule[i],1,0,0);

  /*** fill in Kcube with never terms ***/
  for (i=0; i<never->max; i++)
    Kcube_term(Kcube,properties,&never->p.rule[i],1,0,0);

  /*** try to grow seed terms ***/
  for (i=0; i<rules->max; i++)
    {
    pr=&rules->p.rule[i];
    for (j=pr->guard->max-1; j>=0; j--)
      {
      pg=pr->guard->p.pc[j];
      if (!guard_in_properties(properties,pg)) continue;

      /*** try eliminating one literal from guard ***/
      list_remove_element(pr->guard,j);
      coverage=Kcube_term(Kcube,properties,pr,0,0,0);
      list_insert_element(pr->guard,&pg,j);

      /*** now handle cases ***/
      if (coverage>0)
	{
        Kcube_term(Kcube,properties,pr,-1,0,0);
        list_remove_element(pr->guard,j);
        Kcube_term(Kcube,properties,pr,1,0,0);
	}
      }
    }

  /*** eliminate redundant terms, from longest terms to shortest ***/
  list_sort(rules,&rule_length_cmp);
  for (i=rules->max-1; i>=0; i--)
    {
    pr=&rules->p.rule[i];
    coverage=Kcube_term(Kcube,properties,pr,0,0,0);
    if (coverage>1)
      {
      Kcube_term(Kcube,properties,pr,-1,0,0);
      list_remove_element(rules,i);
      }
    }

  /*** free Kcube, properties ***/
  leak_free(Kcube);
  list_free(properties);
  }

/*** non-WC multirail optimization of logic ***/
void espresso(CIRCUIT *circuit)
  {
  LIST *op_rules,*new_rules;
  RULE *pr1,*pr2;
  int i;

  /*** grab rules by target:dir ***/
  new_rules=list_create(sizeof(RULE));
  while (circuit->rules->max>0)
    {
    pr1=&circuit->rules->p.rule[0];
    op_rules=list_create(sizeof(RULE));
    list_append_element(op_rules,pr1);
    pr1=&op_rules->p.rule[0];
    list_remove_element(circuit->rules,0);
    for (i=0; i<circuit->rules->max; i++)
      {
      pr2=&circuit->rules->p.rule[i];
      if (ruletarget_cmp(pr1,pr2)==0)
	{
        list_append_element(op_rules,pr2);
        list_remove_element(circuit->rules,i--);
	}
      }

    /*** process rules for same target:dir ***/
    optimize_logic(op_rules,circuit->never,circuit->properties);
    list_append_list(new_rules,op_rules);
    list_free(op_rules);
    }
  list_move(circuit->rules,new_rules);
  }
