#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>
#include <readline/readline.h>
#include <readline/history.h>

typedef struct _variable
  {
  char *name;
  double value;
  } VARIABLE;

typedef unsigned char byte;
typedef struct _list
  {
  int max,size,mem,shrink;
  union
    {
    byte *b;
    VARIABLE **pvar;
    } p;
  } LIST;

#include "newlex.h"
#include "misc.h"
#include "leak.h"
#include "list.h"
#include "expression.h"

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

/*** sample assign ***/
void assign_variable(LIST *variables, char *name, double value)
  {
  int i;
  VARIABLE *pvar;
  i=find_element_lazy_sort(variables,name,&pvar_name_cmp);
  if (i>=0) {leak_free(name); variables->p.pvar[i]->value=value;}
  else
    {
    pvar=leak_malloc(sizeof(VARIABLE));
    pvar->name=name;
    pvar->value=value;
    list_insert_element_lazy_sort(variables,&pvar,&pvar_cmp);
    }
  }

/*** sample lex2dp ***/
double *lex2dp(LEX *lex, void *data)
  {
  LIST *variables=(LIST *) data;
  int i;
  i=find_element_lazy_sort(variables,lex_eatif_id(lex),&pvar_name_cmp);
  if (i>=0) return &variables->p.pvar[i]->value;
  else      return NULL;
  }

/*** test it out ***/
int main(int argc, char *argv[])
  {
  int batch,j;
  LEX *lex;
  LIST *expression,*variables;
  char *line=NULL,*oldline=NULL;
  variables=list_create(sizeof(VARIABLE *));
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

    /*** handle command ***/
    if (lex_eatif_keyword(lex,"set"))
      {
      char *name=leak_strdup(lex_eatif_id(lex));
      expression=parse_expression(lex,lex2dp,(void *)variables);
      if (!lex_is_eof(lex)) lex_warning(lex,"basic expression");
      assign_variable(variables,name,evaluate_expression(expression));
      list_free(expression);
      }
    else
      {
      expression=parse_expression(lex,lex2dp,(void *)variables);
      if (!lex_is_eof(lex)) lex_warning(lex,"basic expression");
      printf("%0.15g\n",evaluate_expression(expression));
      list_free(expression);
      }

    /*** more readline stuff ***/
    lex_free(lex);
    }

  /*** free and exit ***/
  for (j=0; j<variables->max; j++)
    {
    VARIABLE *pvar = variables->p.pvar[j];
    leak_free(pvar->name);
    leak_free(pvar);
    }
  list_free(variables);
  free_temp_list();
  leak_check();
  return 0;
  }
