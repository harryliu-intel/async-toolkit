/*****************************************************
 * Copyright 2005 Fulcrum Microsystems               *
 * Copyright 1999 California Institute of Technology *
 * All rights reserved.  Written by Andrew Lines.    *
 *****************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>
#include <readline/readline.h>
#include <readline/history.h>
#include "misc.h"
#include "leak.h"
#include "newlex.h"
#include "bigint.h"

typedef struct _variable
  {
  char *name;
  BIGINT *value;
  } VARIABLE;

typedef struct _group
  {
  char *name;
  struct _list *variables;
  } GROUP;

typedef unsigned char byte;
typedef struct _list
  {
  int max,size,mem,shrink;
  union
    {
    byte *b;
    VARIABLE **pvar;
    GROUP **pgrp;
    BIOP *op;
    } p;
  } LIST;

#define BIOP_GRP (BIOP_USR+100)

#include "list.h"

/*** sample compare of variables ***/
int pvar_cmp(void *p1, void *p2)
  {
  VARIABLE *pv1=*((VARIABLE **)p1);
  VARIABLE *pv2=*((VARIABLE **)p2);
  return strcmp(pv1->name,pv2->name);
  }

/*** sample compare of variable versus name ***/
int pvar_name_cmp(void *p1, void *p2)
  {
  VARIABLE *pv1=*((VARIABLE **)p1);
  char *pc=(char *)p2;
  return strcmp(pv1->name,pc);
  }

/*** sample compare of groups ***/
int pgrp_cmp(void *p1, void *p2)
  {
  GROUP *pv1=*((GROUP **)p1);
  GROUP *pv2=*((GROUP **)p2);
  return strcmp(pv1->name,pv2->name);
  }

/*** sample compare of group versus name ***/
int pgrp_name_cmp(void *p1, void *p2)
  {
  GROUP *pv1=*((GROUP **)p1);
  char *pc=(char *)p2;
  return strcmp(pv1->name,pc);
  }

/*** find or create a variable ***/
VARIABLE *find_or_create_variable(LIST *variables, char *name)
  {
  int i;
  VARIABLE *pvar;
  i=find_element_lazy_sort(variables,name,&pvar_name_cmp);
  if (i>=0)
    {
    leak_free(name);
    pvar = variables->p.pvar[i];
    }
  else
    {
    pvar=leak_malloc(sizeof(VARIABLE));
    pvar->name=name;
    pvar->value=bigint_from_int(0);
    list_insert_element_lazy_sort(variables,&pvar,&pvar_cmp);
    }
  return pvar;
  }

/*** create or re-define group with list of VARIBALE *'s ***/
GROUP *find_or_create_group(LIST *groups, char *name, LIST *variables)
  {
  int i;
  GROUP *pgrp;
  i=find_element_lazy_sort(groups,name,&pgrp_name_cmp);
  if (i>=0)
    {
    leak_free(name);
    pgrp=groups->p.pgrp[i];
    if (variables!=NULL)
      {
      list_free(pgrp->variables);
      pgrp->variables=variables;
      }
    }
  else
    {
    pgrp=leak_malloc(sizeof(GROUP));
    pgrp->name=name;
    if (variables==NULL) pgrp->variables=list_create(sizeof(VARIABLE *));
    else pgrp->variables=variables;
    list_insert_element_lazy_sort(groups,&pgrp,&pgrp_cmp);
    }
  return pgrp;
  }

/*** print a group ***/
void print_group(GROUP *pgrp)
  {
  int i;
  VARIABLE *pvar;
  for (i=0; i<pgrp->variables->max; i++)
    {
    pvar=pgrp->variables->p.pvar[i];
    printf("%s = ",pgrp->variables->p.pvar[i]->name);
    print_bigint_hex(stdout,pvar->value);
    printf("\n");
    }
  }

/*** Substitute variables and user functions from strings to values ***/
void substitute_expression_variables(LIST *expression, LIST *variables, LIST *groups)
  {
  int i,j;
  BIOP *op;
  char name[STRMAX];
  for (j=0; j<expression->max; j++)
    {
    op = &expression->p.op[j];
    if (op->type == BIOP_VARSTR)
      {
      op->type = BIOP_VAR;
      sprint_bigint_str(name,op->value);
      i=find_element_lazy_sort(variables,name,&pvar_name_cmp);
      if (i>=0) assign_bigint(op->value,variables->p.pvar[i]->value);
      else move_bigint(op->value,bigint_from_int(0));
      }
    else if (op->type == BIOP_USRSTR)
      {
      sprint_bigint_str(name,op->value);
      i=find_element_lazy_sort(groups,name,&pgrp_name_cmp);
      if (i>=0) op->type=BIOP_GRP+i;
      }
    }
  }

/*** user provided parsing function for variables/function ***/
char *parse_var(LEX *lex)
  {
  char *str;
  str=lex_eatif_id(lex);
  return leak_strdup(str);
  }

/*** user provided evaluation function ***/
void eval_user(int type, int *tos, BIGINT **stack, void *data)
  {
  LIST *groups;
  GROUP *pgrp;
  VARIABLE *pvar;
  int igrp,ivar;
  groups = (LIST *) data;
  igrp = type-BIOP_GRP;
  if ((*tos<1) || (igrp<0) || (igrp>=groups->max))
    {
    fprintf(stderr,"WARNING: error in eval_user.\n");
    move_bigint(stack[*tos-1],bigint_from_int(0));
    return;
    }
  pgrp = groups->p.pgrp[igrp];
  ivar = int_from_bigint(stack[*tos-1]);
  if ((ivar<0) || (ivar>=pgrp->variables->max))
    {
    fprintf(stderr,"WARNING: out-of-bounds group access.\n");
    move_bigint(stack[*tos-1],bigint_from_int(0));
    return;
    }
  pvar=pgrp->variables->p.pvar[ivar];
  move_bigint(stack[*tos-1],copy_bigint(pvar->value));
  }

/*** test it out ***/
int main(int argc, char *argv[])
  {
  BIGINT *b;
  VARIABLE *pvar;
  GROUP *pgrp;
  int batch;
  LEX *lex;
  LIST *expression,*variables,*groups;
  char *line=NULL,*oldline=NULL;
  int j;

  variables=list_create(sizeof(VARIABLE *));
  groups=list_create(sizeof(GROUP *));
  batch=((argc==2)&&(strcmp(argv[1],"-batch")==0));
  if (batch) line=malloc(1024);
  while(1)
    {
    /*** get the line with readline ***/
    if (batch) 
      {
      line=fgets(line,1024,stdin);
      if (line==NULL) break;
      }
    else
      {
      line=readline("calculator> ");
      if (line==NULL) break;
      if ((line[0]!=0)&&((oldline==NULL)||(strcmp(line,oldline)!=0))) add_history(line);
      if (oldline!=NULL) free(oldline);
      oldline=line;
      }
    lex=lex_string(line);

    /*** handle commands ***/
    if (lex_eatif_keyword(lex,"help"))
      {
      printf("Biginteger calculator commands:\n"
             "  help -- print help banner\n"
             "  set var expression -- set a variable\n"
             "  set grp(index_expression) expression -- set a group member\n"
             "  group grp(var1,var2,...) -- define a group\n"
             "  group grp -- print members of group\n"
             "  expression -- evaluate an expression\n");
      }
    else if (lex_eatif_keyword(lex,"set"))
      {
      char *name=parse_var(lex);
      if (!lex_is_whitespace(lex) && lex_eatif_sym(lex,"("))
        {
        pgrp=find_or_create_group(groups,name,NULL);
        expression=parse_bigint_expression(lex,parse_var);
        substitute_expression_variables(expression,variables,groups);
        b=evaluate_bigint_expression(expression,groups,eval_user);
        j=int_from_bigint(b);
        free_bigint_expression(expression,1,1,1,1,1,1);
        if ((j<0) || (j>=pgrp->variables->max))
          {
          fprintf(stderr,"WARNING: out-of-bounds group index in lvalue.\n");
          pvar=NULL;
          }
        else pvar=pgrp->variables->p.pvar[j];
        lex_eatif_sym(lex,")");
        }
      else pvar=find_or_create_variable(variables,name);
      if (pvar!=NULL)
        {
        expression=parse_bigint_expression(lex,parse_var);
        print_bigint_expression(stdout,expression);
        printf("\n");
        substitute_expression_variables(expression,variables,groups);
        if (!lex_is_eof(lex)) lex_warning(lex,"basic expression");
        b=evaluate_bigint_expression(expression,groups,eval_user);
        move_bigint(pvar->value,b);
        free_bigint_expression(expression,1,1,1,1,1,1);
        }
      }
    else if (lex_eatif_keyword(lex,"group"))
      {
      char *name=parse_var(lex);
      if (!lex_is_whitespace(lex) && lex_eatif_sym(lex,"("))
        {
        LIST *group_variables;
        group_variables=list_create(sizeof(VARIABLE *));
        while (!lex_eatif_sym(lex,")"))
          {
          char *str;
          str = parse_var(lex);
          pvar = find_or_create_variable(variables,str);
          list_append_element(group_variables,&pvar);
          lex_eatif_sym(lex,",");
          }
        pgrp=find_or_create_group(groups,name,group_variables);
        }
      else pgrp=find_or_create_group(groups,name,NULL);
      print_group(pgrp);
      }
    else
      {
      expression=parse_bigint_expression(lex,parse_var);
      print_bigint_expression(stdout,expression);
      printf("\n");
      substitute_expression_variables(expression,variables,groups);
      if (!lex_is_eof(lex)) lex_warning(lex,"basic expression");
      b = evaluate_bigint_expression(expression,groups,eval_user);
      printf("= ");
      print_bigint_hex(stdout,b);
      printf(" = \"");
      print_bigint_str(stdout,b);
      printf("\"\n");
      free_bigint(b);
      free_bigint_expression(expression,1,1,1,1,1,1);
      }

    /*** more readline stuff ***/
    lex_free(lex);
    }

  /*** free and exit ***/
  for (j=0; j<variables->max; j++)
    {
    pvar = variables->p.pvar[j];
    leak_free(pvar->name);
    free_bigint(pvar->value);
    leak_free(pvar);
    }
  for (j=0; j<groups->max; j++)
    {
    pgrp = groups->p.pgrp[j];
    leak_free(pgrp->name);
    list_free(pgrp->variables);
    leak_free(pgrp);
    }
  list_free(variables);
  list_free(groups);
  free_temp_list();
  free_bigint_memory();
  leak_check();
  return 0;
  }
