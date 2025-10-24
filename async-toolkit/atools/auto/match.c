#include "auto.h"

/*** try to match names; if match, add to renames list ***/
int match_name(char *name1, char *name2, LIST *renames)
  {
  RENAME rename;
  char *name;

  /*** translate name ***/
  name=translate_name(name2,renames);

  /*** if no translation exists, rename to match ***/
  if (name==NULL)
    {
    rename.old=get_name(name2);
    rename.new=get_name(name1);
    list_insert_element_lazy_sort(renames,&rename,&rename_cmp);
    return 1;
    }

  /*** check for matching globals (stripping ! from the end) ***/
  if (name[strlen(name)-1]=='!')
    {
    if (strlen(name)!=strlen(name1)+1) return 0;
    if (strncmp(name,name1,strlen(name1))!=0) return 0;
    return 1;
    }

  /*** check for matching non-globals ***/
  return (name==name1);
  }

/*** try to match rules; if match, append new renamings ***/
int match_rule(RULE *rule1, RULE *rule2, LIST *renames)
  {
  int i;
  char *pc1,*pc2;
  LIST *oldrenames;

  /*** try matching rule1 to rule2 ***/
  if (rule1->dir!=rule2->dir) return 0;
  if (rule1->guard->max!=rule2->guard->max) return 0;
  oldrenames=list_dup(renames);
  if (!match_name(rule1->target,rule2->target,renames)) goto mismatch;
  for (i=0; i<rule1->guard->max; i++)
    {
    pc1=rule1->guard->p.pc[i];
    pc2=rule2->guard->p.pc[i];
    if (!match_name(pc1,pc2,renames)) goto mismatch;
    }

  /*** set width of library rule ***/
  set(rule2->lnw,get(rule1->lnw));
  get(rule2->width); /* HACK: force update */

  /*** match ***/
  list_free(oldrenames);
  return 1;

  mismatch:
  list_move(renames,oldrenames);
  return 0;
  }

/*** try to match properties; if match, append new renamings ***/
int match_property(PROPERTY *prop1, PROPERTY *prop2, LIST *renames)
  {
  int i;
  LIST *oldrenames;
  if (prop1->sense!=prop2->sense) return 0;
  if (prop1->items->max!=prop2->items->max) return 0;
  oldrenames=list_dup(renames);
  for (i=0; i<prop1->items->max; i++)
    {
    if (!match_name(prop1->items->p.pc[i],prop2->items->p.pc[i],renames))
      goto mismatch;
    }
  list_free(oldrenames);
  return 1;

  mismatch:
  list_move(renames,oldrenames);
  return 0;
  }

/*** try to match subcircuit's nth item in category to a subset of the circuit ***/
/*** if match, appends to renames ***/
/*** if match, removes matched items from circuit (except properties) ***/
int match_circuit(CIRCUIT *circuit, CIRCUIT *sub, LIST *renames, LIST *stretches,
                  int category, int n, int *rulenum)
  {
  LIST *oldrenames;
  RULE *rule1,*rule2,saverule;
  STATICIZER stat1,stat2;
  char *target;
  PROPERTY prop1,prop2;
  STRETCH stretch;
  int i,j,newrulenum;

  /*** pick next category ***/
  if ((category==0)&&(n>=sub->rules->max))       {category++; n=0;}
  if ((category==1)&&(n>=1))                     {category++; n=0;}
  if ((category==2)&&(n>=sub->staticizers->max)) {category++; n=0;}
  if ((category==3)&&(n>=sub->properties->max))  {category++; n=0;}

  /*** save renames in case of mismatch ***/
  oldrenames=list_dup(renames);
  newrulenum=*rulenum;

  /*** search for match of nth item of subcircuit ***/
  if (category==0) /*** match rules ***/
    {
    rule2=&sub->rules->p.rule[n];
    for (i=0; i<circuit->rules->max; i++)
      {
      rule1=&circuit->rules->p.rule[i];
      saverule=circuit->rules->p.rule[i];
      if (match_rule(rule1,rule2,renames))
        {
        newrulenum=min(*rulenum,rule1->num);
        list_remove_element(circuit->rules,i);
        if (match_circuit(circuit,sub,renames,stretches,category,n+1,&newrulenum)) goto match;
        list_insert_element(circuit->rules,&saverule,i);
        list_copy(renames,oldrenames);
        }
      }
    }
  else if (category==1) /*** check that half-operators are fully matched ***/
    {
    for (i=0; i<sub->rules->max; i++)
      {
      rule2=&sub->rules->p.rule[i];
      target=translate_name(rule2->target,renames);
      for (j=0; j<circuit->rules->max; j++)
        {
        rule1=&circuit->rules->p.rule[j];
        if ((rule1->dir==rule2->dir)&&(rule1->target==target)) goto mismatch;
        }
      }
    if (match_circuit(circuit,sub,renames,stretches,category,n+1,&newrulenum)) goto match;
    }
  else if (category==2) /*** match staticizers ***/
    {
    stat2=sub->staticizers->p.staticizer[n];
    for (i=0; i<circuit->staticizers->max; i++)
      {
      stat1=circuit->staticizers->p.staticizer[i];
      if (match_name(stat1.name,stat2.name,renames))
        {
        newrulenum=min(*rulenum,stat1.rulenum);
        list_remove_element(circuit->staticizers,i);
        if (match_circuit(circuit,sub,renames,stretches,category,n+1,&newrulenum)) goto match;
        list_insert_element(circuit->staticizers,&stat1,i);
        list_copy(renames,oldrenames);
        }
      }
    }
  else if (category==3) /*** match properties ***/
    {
    prop2=sub->properties->p.prop[n];
    for (i=0; i<circuit->properties->max; i++)
      {
      prop1=circuit->properties->p.prop[i];
      if (match_property(&prop1,&prop2,renames))
        {
        if (match_circuit(circuit,sub,renames,stretches,category,n+1,&newrulenum)) goto match;
        list_copy(renames,oldrenames);
        }
      }
    }
  else if (category==4) /*** match requirements and compute stretches ***/
    {
    for (i=0; i<sub->requirements->max; i++)
      if (evaluate_expression(sub->requirements->p.plist[i])==0) goto mismatch;
    for (i=0; i<sub->stretches->max; i++)
      {
      stretch=sub->stretches->p.stretch[i];
      stretch.amount=rint(evaluate_expression(stretch.expression));
      stretch.expression=NULL;
      list_insert_element_lazy_sort(stretches,&stretch,&stretch_cmp);
      }
    goto match;
    }

  /*** mismatch ***/
  mismatch:
  list_move(renames,oldrenames);
  return 0;

  /*** match ***/
  match:
  list_free(oldrenames);
  *rulenum=newrulenum;
  return 1;
  }
