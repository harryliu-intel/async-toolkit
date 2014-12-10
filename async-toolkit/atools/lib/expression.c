/*****************************************************
 * Copyright 2005 Fulcrum Microsystems               *
 * Copyright 1999 California Institute of Technology *
 * All rights reserved.  Written by Andrew Lines.    *
 *****************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <math.h>

typedef struct _op
  {
  int type;
  double constant;
  double *variable;
  } OP;

typedef unsigned char byte;
typedef struct _list
  {
  int max,size,mem,shrink;
  union
    {
    byte *b;
    OP *op;
    } p;
  } LIST;

#include "newlex.h"
#include "misc.h"
#include "list.h"
#include "expression.h"

#define min(a,b) ((a)<(b)?(a):(b))
#define max(a,b) ((a)>(b)?(a):(b))
#define STACKMAX 1024

#define OP_VAR 0
#define OP_CONST 1
#define OP_E 2
#define OP_NE 3
#define OP_GT 4
#define OP_GTE 5
#define OP_LT 6
#define OP_LTE 7
#define OP_AND 8
#define OP_OR 9
#define OP_NOT 10
#define OP_ADD 11
#define OP_SUB 12
#define OP_MUL 13
#define OP_DIV 14
#define OP_ABS 15
#define OP_MAX 16
#define OP_MIN 17
#define OP_IF 18
#define OP_MINUS 19
#define OP_INT 20
#define OP_POW 21
#define OP_EXP 22
#define OP_LOG 23

/*** prototype ***/
void parse_expression_main(LEX *lex, LEX2DP *lex2dp, void *data,
                           LIST *expression, int precedence);

/*** add an operator to the RPN expression ***/
void add_operator(LIST *expression, int type)
  {
  OP op;
  op.type=type;
  op.constant=0;
  op.variable=NULL;
  list_append_element(expression,&op);
  }

/*** add a constant to the RPN expression ***/
void add_constant(LIST *expression, double constant)
  {
  OP op;
  op.type=OP_CONST;
  op.constant=constant;
  op.variable=NULL;
  list_append_element(expression,&op);
  }

/*** add a variable to the RPN expression ***/
void add_variable(LIST *expression, double *variable)
  {
  OP op;
  op.type=OP_VAR;
  op.constant=0;
  op.variable=variable;
  list_append_element(expression,&op);
  }

/*** parse a single item of an expression ***/
void parse_expression_item(LEX *lex, LEX2DP *lex2dp, void *data,
                           LIST *expression)
  {
  if (lex_is_real(lex)) add_constant(expression,lex_eat_real(lex));
  else if (lex_is_integer(lex)) /* non-decimal */
    { add_constant(expression, (double)lex_eat_integer(lex)); }
  else if (lex_eatif_sym(lex,"("))
    {
    parse_expression_main(lex,lex2dp,data,expression,0);
    lex_eatif_sym(lex,")");
    }
  else if (lex_eatif_sym(lex,"~"))
    {
    parse_expression_item(lex,lex2dp,data,expression);
    add_operator(expression,OP_NOT);
    }
  else if (lex_eatif_sym(lex,"-"))
    {
    parse_expression_item(lex,lex2dp,data,expression);
    add_operator(expression,OP_MINUS);
    }
  else if (lex_eatif_sym(lex,"+"))
    {
    parse_expression_item(lex,lex2dp,data,expression);
    }
  else if (lex_eatif_sym(lex,"ABS"))
    {
    lex_eatif_sym(lex,"(");
    parse_expression_main(lex,lex2dp,data,expression,0);
    lex_eatif_sym(lex,")");
    add_operator(expression,OP_ABS);
    }
  else if (lex_eatif_sym(lex,"INT"))
    {
    lex_eatif_sym(lex,"(");
    parse_expression_main(lex,lex2dp,data,expression,0);
    lex_eatif_sym(lex,")");
    add_operator(expression,OP_INT);
    }
  else if (lex_eatif_sym(lex,"LOG"))
    {
    lex_eatif_sym(lex,"(");
    parse_expression_main(lex,lex2dp,data,expression,0);
    lex_eatif_sym(lex,")");
    add_operator(expression,OP_LOG);
    }
  else if (lex_eatif_sym(lex,"EXP"))
    {
    lex_eatif_sym(lex,"(");
    parse_expression_main(lex,lex2dp,data,expression,0);
    lex_eatif_sym(lex,")");
    add_operator(expression,OP_EXP);
    }
  else if (lex_eatif_sym(lex,"MIN"))
    {
    lex_eatif_sym(lex,"(");
    parse_expression_main(lex,lex2dp,data,expression,0);
    lex_eatif_sym(lex,",");
    parse_expression_main(lex,lex2dp,data,expression,0);
    lex_eatif_sym(lex,")");
    add_operator(expression,OP_MIN);
    }
  else if (lex_eatif_sym(lex,"MAX"))
    {
    lex_eatif_sym(lex,"(");
    parse_expression_main(lex,lex2dp,data,expression,0);
    lex_eatif_sym(lex,",");
    parse_expression_main(lex,lex2dp,data,expression,0);
    lex_eatif_sym(lex,")");
    add_operator(expression,OP_MAX);
    }
  else add_variable(expression,lex2dp(lex,data));
  }
  
/*** core parse infix expressions with precedence ***/
void parse_expression_main(LEX *lex, LEX2DP *lex2dp, void *data,
                           LIST *expression, int precedence)
  { 
  parse_expression_item(lex,lex2dp,data,expression);
  while (!lex_is_eof(lex))
    {
    if ((precedence<8)&&lex_eatif_sym(lex,"^"))
      {
      parse_expression_main(lex,lex2dp,data,expression,8);
      add_operator(expression,OP_POW);
      }
    else if ((precedence<7)&&lex_eatif_sym(lex,"*"))
      {
      parse_expression_main(lex,lex2dp,data,expression,7);
      add_operator(expression,OP_MUL);
      }
    else if ((precedence<7)&&lex_eatif_sym(lex,"/"))
      {
      parse_expression_main(lex,lex2dp,data,expression,7);
      add_operator(expression,OP_DIV);
      }
    else if ((precedence<6)&&lex_eatif_sym(lex,"+"))
      {
      parse_expression_main(lex,lex2dp,data,expression,6);
      add_operator(expression,OP_ADD);
      }
    else if ((precedence<6)&&lex_eatif_sym(lex,"-"))
      {
      parse_expression_main(lex,lex2dp,data,expression,6);
      add_operator(expression,OP_SUB);
      }
    else if ((precedence<5)&&lex_eatif_sym(lex,"<="))
      {
      parse_expression_main(lex,lex2dp,data,expression,5);
      add_operator(expression,OP_LTE);
      }
    else if ((precedence<5)&&lex_eatif_sym(lex,">="))
      {
      parse_expression_main(lex,lex2dp,data,expression,5);
      add_operator(expression,OP_GTE);
      }
    else if ((precedence<5)&&lex_eatif_sym(lex,"<"))
      {
      parse_expression_main(lex,lex2dp,data,expression,5);
      add_operator(expression,OP_LT);
      }
    else if ((precedence<5)&&lex_eatif_sym(lex,">"))
      {
      parse_expression_main(lex,lex2dp,data,expression,5);
      add_operator(expression,OP_GT);
      }
    else if ((precedence<4)&&lex_eatif_sym(lex,"=="))
      {
      parse_expression_main(lex,lex2dp,data,expression,4);
      add_operator(expression,OP_E);
      }
    else if ((precedence<4)&&lex_eatif_sym(lex,"!="))
      {
      parse_expression_main(lex,lex2dp,data,expression,4);
      add_operator(expression,OP_NE);
      }
    else if ((precedence<3)&&lex_eatif_sym(lex,"&"))
      {
      parse_expression_main(lex,lex2dp,data,expression,3);
      add_operator(expression,OP_AND);
      }
    else if ((precedence<2)&&lex_eatif_sym(lex,"|"))
      {
      parse_expression_main(lex,lex2dp,data,expression,2);
      add_operator(expression,OP_OR);
      }
    else if ((precedence<1)&&lex_eatif_sym(lex,"?"))
      {
      parse_expression_main(lex,lex2dp,data,expression,0);
      lex_eatif_sym(lex,":");
      parse_expression_main(lex,lex2dp,data,expression,0);
      add_operator(expression,OP_IF);
      }
    else break;
    }
  }

/*** user interface for parsing expressions ***/
LIST *parse_expression(LEX *lex, LEX2DP *lex2dp, void *data)
  {
  LIST *expression;
  expression=list_create(sizeof(OP));
  parse_expression_main(lex,lex2dp,data,expression,0);
  return expression;
  }

/*** print RPN expression for debugging purposes ***/
void print_expression(FILE *fout, LIST *expression)
  {
  int j;
  OP op;

  if (expression==NULL) {printf("NONE\n"); return;}
  for (j=0; j<expression->max; j++)
    {
    op=expression->p.op[j];
    switch(op.type)
      {
    case OP_VAR:   if (op.variable==NULL) fprintf(fout,"NULL ");
                   else fprintf(fout,"%g ",*op.variable);
                   break;
    case OP_CONST: fprintf(fout,"%g ",op.constant); break;
    case OP_E:     fprintf(fout,"== "); break;
    case OP_NE:    fprintf(fout,"!= "); break;
    case OP_GTE:   fprintf(fout,">= "); break;
    case OP_GT:    fprintf(fout,"> "); break;
    case OP_LTE:   fprintf(fout,"<= "); break;
    case OP_LT:    fprintf(fout,"< "); break;
    case OP_AND:   fprintf(fout,"& "); break;
    case OP_OR:    fprintf(fout,"| "); break;
    case OP_NOT:   fprintf(fout,"~ "); break;
    case OP_ADD:   fprintf(fout,"+ "); break;
    case OP_SUB:   fprintf(fout,"- "); break;
    case OP_POW:   fprintf(fout,"^ "); break;
    case OP_MUL:   fprintf(fout,"* "); break;
    case OP_DIV:   fprintf(fout,"/ "); break;
    case OP_ABS:   fprintf(fout,"ABS "); break;
    case OP_INT:   fprintf(fout,"INT"); break;
    case OP_EXP:   fprintf(fout,"EXP"); break;
    case OP_LOG:   fprintf(fout,"LOG"); break;
    case OP_MIN:   fprintf(fout,"MIN "); break;
    case OP_MAX:   fprintf(fout,"MAX "); break;
    case OP_IF:    fprintf(fout,"? "); break;
    case OP_MINUS: fprintf(fout,"NEG "); break;
    default:       assert(0);
      }
    }
  fprintf(fout,"\n");
  }

/*** evaluate an expression ***/
double evaluate_expression(LIST *expression)
  {
  double s[STACKMAX];
  int tos=0,j;
  OP op;

  for (j=0; j<expression->max; j++)
    {
    op=expression->p.op[j];
    switch (op.type)
      {
    case OP_VAR:   s[tos++]=(op.variable!=NULL) ? *op.variable : 0; break;
    case OP_CONST: s[tos++]=op.constant; break;
    case OP_E:     s[tos-2]=(s[tos-2]==s[tos-1]); tos--; break;
    case OP_NE:    s[tos-2]=(s[tos-2]!=s[tos-1]); tos--; break;
    case OP_GTE:   s[tos-2]=(s[tos-2]>=s[tos-1]); tos--; break;
    case OP_GT:    s[tos-2]=(s[tos-2]>s[tos-1]);  tos--; break;
    case OP_LTE:   s[tos-2]=(s[tos-2]<=s[tos-1]); tos--; break;
    case OP_LT:    s[tos-2]=(s[tos-2]<s[tos-1]);  tos--; break;
    case OP_AND:   s[tos-2]=((s[tos-2]!=0)&&(s[tos-1]!=0)); tos--; break;
    case OP_OR:    s[tos-2]=((s[tos-2]!=0)||(s[tos-1]!=0)); tos--; break;
    case OP_NOT:   s[tos-1]=(s[tos-1]==0); break;
    case OP_ADD:   s[tos-2]=s[tos-2]+s[tos-1]; tos--; break;
    case OP_SUB:   s[tos-2]=s[tos-2]-s[tos-1]; tos--; break;
    case OP_POW:   s[tos-2]=pow(s[tos-2],s[tos-1]); tos--; break;
    case OP_MUL:   s[tos-2]=s[tos-2]*s[tos-1]; tos--; break;
    case OP_DIV:   s[tos-2]=s[tos-2]/s[tos-1]; tos--; break;
    case OP_ABS:   s[tos-1]=fabs(s[tos-1]); break;
    case OP_MIN:   s[tos-2]=min(s[tos-2],s[tos-1]); tos--; break;
    case OP_MAX:   s[tos-2]=max(s[tos-2],s[tos-1]); tos--; break;
    case OP_IF:    s[tos-3]=(s[tos-3]!=0)?s[tos-2]:s[tos-1]; tos-=2; break;
    case OP_MINUS: s[tos-1]=-s[tos-1]; break;
    case OP_INT:   s[tos-1]=(int)s[tos-1]; break;
    case OP_EXP:   s[tos-1]=exp(s[tos-1]); break;
    case OP_LOG:   s[tos-1]=log(s[tos-1]); break;
    default:       assert(0);
      }
    }
  assert(tos==1);
  return s[0];
  }
