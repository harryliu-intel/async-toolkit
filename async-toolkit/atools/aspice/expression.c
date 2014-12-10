/*****************************************************
 * Copyright 2005 Fulcrum Microsystems               *
 * Copyright 1999 California Institute of Technology *
 * All rights reserved.  Written by Andrew Lines.    *
 *****************************************************/

#include "aspice.h"

/** sign **/
double sgn(double x) {
  if (x>=0) return 1;
  else return 0;
}

/*
 * Gaussian distribution function with absolute variation.  Uses
 * Irwin-Hall approximation which is resaonable to 6 sigma.  The
 * standard deviation is abs_variation/num_sigmas for convenience of
 * specification.  TODO: more precision?
 */
double agauss(double nominal_val, double abs_variation, double num_sigmas) {
  int i;
  double x=-6;
  if (monte_carlo_seed<=0) return nominal_val; // monte-carlo disabled
  for (i=0; i<12; i++) x += drand48();
  x = nominal_val + x * abs_variation / num_sigmas;
  return x;
}

/* Function defined in TSMC28nm BSIM4 deck */
double selmin(double par1, double par2) {
  return abs(par1)<abs(par2) ? par1 : -par2;
}

/* RPN expression evaluator */
inline int evaluate_op(OP op, double *s, int tos, double val, double time,
                       double *true, double *false, double *mid)
  {
  switch (op.type)
    {
    case OP_CONST: s[tos++]=op.value; break;
    case OP_TIME:  s[tos++]=time; break;
    case OP_TRUE:  *true=op.value;  *mid=(*true+*false)/2; break;
    case OP_FALSE: *false=op.value; *mid=(*true+*false)/2; break;
    case OP_E:     s[tos-2]=(s[tos-2]==s[tos-1]?*true:*false); tos--; break;
    case OP_NE:    s[tos-2]=(s[tos-2]!=s[tos-1]?*true:*false); tos--; break;
    case OP_GTE:   s[tos-2]=(s[tos-2]>=s[tos-1]?*true:*false); tos--; break;
    case OP_GT:    s[tos-2]=(s[tos-2]>s[tos-1]?*true:*false);  tos--; break;
    case OP_LTE:   s[tos-2]=(s[tos-2]<=s[tos-1]?*true:*false); tos--; break;
    case OP_LT:    s[tos-2]=(s[tos-2]<s[tos-1]?*true:*false);  tos--; break;
    case OP_AND:   s[tos-2]=((s[tos-2]>*mid)&&(s[tos-1]>*mid)?*true:*false);
                   tos--; break;
    case OP_OR:    s[tos-2]=((s[tos-2]>*mid)||(s[tos-1]>*mid)?*true:*false);
                   tos--; break;
    case OP_ADD:   s[tos-2]=s[tos-2]+s[tos-1]; tos--; break;
    case OP_SUB:   s[tos-2]=s[tos-2]-s[tos-1]; tos--; break;
    case OP_MUL:   s[tos-2]=s[tos-2]*s[tos-1]; tos--; break;
    case OP_DIV:   s[tos-2]=s[tos-2]/s[tos-1]; tos--; break;
    case OP_NOT:   s[tos-1]=(s[tos-1]>*mid?*false:*true); break;
    case OP_PLUS:  break;
    case OP_MINUS: s[tos-1]=-s[tos-1]; break;
    case OP_BOOL:  s[tos-1]=(s[tos-1]>*mid?*true:*false); break;
    case OP_MAX:   s[tos-2]=max(s[tos-1],s[tos-2]); tos--; break;
    case OP_MIN:   s[tos-2]=min(s[tos-1],s[tos-2]); tos--; break;
    case OP_IF:    s[tos-3]=(s[tos-3]>*mid?s[tos-2]:s[tos-1]); tos-=2; break;
    case OP_ABS:   s[tos-1]=fabs(s[tos-1]); break;
    case OP_EXP:   s[tos-1]=exp(s[tos-1]); break;
    case OP_LOG:   s[tos-1]=log(s[tos-1]); break;
    case OP_SGN:   s[tos-1]=sgn(s[tos-1]); break;
    case OP_SIN:   s[tos-1]=sin(s[tos-1]); break;
    case OP_COS:   s[tos-1]=cos(s[tos-1]); break;
    case OP_TAN:   s[tos-1]=tan(s[tos-1]); break;
    case OP_SINH:  s[tos-1]=sinh(s[tos-1]); break;
    case OP_COSH:  s[tos-1]=cosh(s[tos-1]); break;
    case OP_TANH:  s[tos-1]=tanh(s[tos-1]); break;
    case OP_PWR:   s[tos-2]=pow(s[tos-2],s[tos-1]); tos--; break;
    case OP_SQRT:  s[tos-1]=sqrt(s[tos-1]); break;
    case OP_INT:   s[tos-1]=(int) s[tos-1]; break;
    case OP_FLOOR: s[tos-1]=floor(s[tos-1]); break;
    case OP_CEIL:  s[tos-1]=ceil(s[tos-1]); break;
    case OP_SELMIN: s[tos-2]=selmin(s[tos-2],s[tos-1]); tos--; break;
    case OP_AGAUSS: s[tos-3]=agauss(s[tos-3],s[tos-2],s[tos-1]); tos-=2; break;
    default:       s[tos++]=val; break;
    }
  return tos;
  }

double evaluate_by_nodes(int Nops, OP *ops, int Nnodes, NODE **nodes, double time)
  {
  double s[STACKMAX],val,mid,true,false;
  int tos=0,j;
  OP op;
  true=1;
  false=0;
  mid=(true+false)/2;
  for (j=0; j<Nops; j++)
    {
    op = ops[j];
    val = op.type>=0 ? nodes[op.type]->V : 0;
    tos = evaluate_op(op,s,tos,val,time,&true,&false,&mid);
    }
  assert(tos==1);
  return s[0];
  }

double evaluate(LIST *ops, double time, double *v)
  {
  double s[STACKMAX],val,mid,true,false;
  int tos=0,j;
  OP op;
  true=1;
  false=0;
  mid=(true+false)/2;
  for (j=0; j<ops->max; j++)
    {
    op = ops->p.op[j];
    val = op.type>=0 ? v[op.type] : 0;
    tos = evaluate_op(op,s,tos,val,time,&true,&false,&mid);
    }
  assert(tos==1);
  return s[0];
  }

void parse_expression(LEX *lex, LIST *ops, LIST *nodes, int precedence);

void parse_function(LEX *lex, LIST *ops, LIST *nodes, int type)
  {
  OP op;
  lex_eat_id(lex);
  lex_eat_sym(lex,"(");
  do {
    parse_expression(lex,ops,nodes,0);
  } while (lex_eatif_sym(lex,","));
  lex_eat_sym(lex,")");
  op.value=0;
  op.type=type;
  list_append_element(ops,&op);
  }

void parse_item(LEX *lex, LIST *ops, LIST *nodes)
  {
  OP op;
  int j;
  char *str;

  /*** constants ***/
  op.value=0;
  if (lex_is_keyword(lex,"TIME"))
    {
    lex_eat_id(lex);
    op.type=OP_TIME;
    list_append_element(ops,&op);
    }
  else if (lex_is_real(lex))
    {
    op.type=OP_CONST;
    op.value=lex_eat_real(lex);
    list_append_element(ops,&op);
    }
  /*** parantheses ***/
  else if (lex_is_sym(lex,"("))
    {
    lex_eat_sym(lex,"(");
    parse_expression(lex,ops,nodes,0);
    lex_eat_sym(lex,")");
    }
  /*** functions ***/
  else if (lex_is_keyword(lex,"BOOL")||lex_is_keyword(lex,"bool"))
    parse_function(lex,ops,nodes,OP_BOOL);
  else if (lex_is_keyword(lex,"ABS")||lex_is_keyword(lex,"abs"))
    parse_function(lex,ops,nodes,OP_ABS);
  else if (lex_is_keyword(lex,"SIN")||lex_is_keyword(lex,"sin"))
    parse_function(lex,ops,nodes,OP_SIN);
  else if (lex_is_keyword(lex,"COS")||lex_is_keyword(lex,"cos"))
    parse_function(lex,ops,nodes,OP_COS);
  else if (lex_is_keyword(lex,"EXP")||lex_is_keyword(lex,"exp"))
    parse_function(lex,ops,nodes,OP_EXP);
  else if (lex_is_keyword(lex,"LOG")||lex_is_keyword(lex,"log"))
    parse_function(lex,ops,nodes,OP_LOG);
  else if (lex_is_keyword(lex,"SGN")||lex_is_keyword(lex,"sgn"))
    parse_function(lex,ops,nodes,OP_SGN);
  else if (lex_is_keyword(lex,"MAX")||lex_is_keyword(lex,"max"))
    parse_function(lex,ops,nodes,OP_MAX);
  else if (lex_is_keyword(lex,"MIN")||lex_is_keyword(lex,"min"))
    parse_function(lex,ops,nodes,OP_MIN);
  else if (lex_is_keyword(lex,"SINH")||lex_is_keyword(lex,"sinh"))
    parse_function(lex,ops,nodes,OP_SINH);
  else if (lex_is_keyword(lex,"COSH")||lex_is_keyword(lex,"cosh"))
    parse_function(lex,ops,nodes,OP_COSH);
  else if (lex_is_keyword(lex,"TAN")||lex_is_keyword(lex,"tan"))
    parse_function(lex,ops,nodes,OP_TAN);
  else if (lex_is_keyword(lex,"TANH")||lex_is_keyword(lex,"tanh"))
    parse_function(lex,ops,nodes,OP_TANH);
  else if (lex_is_keyword(lex,"PWR")||lex_is_keyword(lex,"pwr"))
    parse_function(lex,ops,nodes,OP_PWR);
  else if (lex_is_keyword(lex,"SQRT")||lex_is_keyword(lex,"sqrt"))
    parse_function(lex,ops,nodes,OP_SQRT);
  else if (lex_is_keyword(lex,"INT")||lex_is_keyword(lex,"int"))
    parse_function(lex,ops,nodes,OP_INT);
  else if (lex_is_keyword(lex,"FLOOR")||lex_is_keyword(lex,"floor"))
    parse_function(lex,ops,nodes,OP_FLOOR);
  else if (lex_is_keyword(lex,"CEIL")||lex_is_keyword(lex,"ceil"))
    parse_function(lex,ops,nodes,OP_CEIL);
  else if (lex_is_keyword(lex,"SELMIN")||lex_is_keyword(lex,"selmin"))
    parse_function(lex,ops,nodes,OP_SELMIN);
  else if (lex_is_keyword(lex,"AGAUSS")||lex_is_keyword(lex,"agauss"))
    parse_function(lex,ops,nodes,OP_AGAUSS);

  /*** unary operators ***/
  else if (lex_is_sym(lex,"~"))
    {
    lex_eat_sym(lex,"~");
    parse_item(lex,ops,nodes);
    op.type=OP_NOT;
    list_append_element(ops,&op);
    }
  else if (lex_is_sym(lex,"+"))
    {
    lex_eat_sym(lex,"+");
    parse_item(lex,ops,nodes);
    op.type=OP_PLUS;
    list_append_element(ops,&op);
    }
  else if (lex_is_sym(lex,"-"))
    {
    lex_eat_sym(lex,"-");
    parse_item(lex,ops,nodes);
    op.type=OP_MINUS;
    list_append_element(ops,&op);
    }
  /*** variables ***/
  else
    {
    str=super_parse_nodename(lex);
    j=find_element(nodes,&str,&pstrcmp);
    if (j<0)
      {
      list_append_element(nodes,&str);
      op.type=nodes->max-1;
      }
    else {op.type=j;}
    list_append_element(ops,&op);
    }
  }

void parse_expression(LEX *lex, LIST *ops, LIST *nodes, int precedence)
  {
  OP op;
  char *sym[12]={"=","!=",">=",">","<=","<","&","|","+","-","*","/"};
  int type[12]={OP_E,OP_NE,OP_GTE,OP_GT,OP_LTE,OP_LT,OP_AND,OP_OR,OP_ADD,
                OP_SUB,OP_MUL,OP_DIV},
      prec[12]={4,4,5,5,5,5,3,2,6,6,7,7},j;

  parse_item(lex,ops,nodes);
  while (!lex_is_sym(lex,"->") && !lex_is_sym(lex,"'") && !lex_is_eof(lex))
    {
    /*** binary operators ***/
    for (j=0; j<12; j++) if (lex_is_sym(lex,sym[j])) break;
    if (j<12)
      {
      if (precedence>=prec[j]) break;
      lex_eat_sym(lex,sym[j]);
      parse_expression(lex,ops,nodes,prec[j]);
      op.type=type[j];
      op.value=0;
      list_append_element(ops,&op);
      }
    /*** ternary operator ***/
    else if (lex_is_sym(lex,"?"))
      {
      if (precedence>0) break;
      lex_eat_sym(lex,"?");
      parse_expression(lex,ops,nodes,0);
      lex_eat_sym(lex,":");
      parse_expression(lex,ops,nodes,0);
      op.type=OP_IF;
      op.value=0;
      list_append_element(ops,&op);
      }
    /*** done with expression ***/
    else break;
    }
  }

void print_expression(FILE *fout, LIST *ops, LIST *nodes)
  {
  int j;
  OP op;

  for (j=0; j<ops->max; j++)
    {
    op=ops->p.op[j];
    switch(op.type)
      {
    case OP_CONST: fprintf(fout,"%g ",op.value); break;
    case OP_TIME:  fprintf(fout,"TIME "); break;
    case OP_E:     fprintf(fout,"= "); break;
    case OP_NE:    fprintf(fout,"!= "); break;
    case OP_GT:    fprintf(fout,"> "); break;
    case OP_GTE:   fprintf(fout,">= "); break;
    case OP_LT:    fprintf(fout,"< "); break;
    case OP_LTE:   fprintf(fout,"<= "); break;
    case OP_AND:   fprintf(fout,"& "); break;
    case OP_OR:    fprintf(fout,"| "); break;
    case OP_NOT:   fprintf(fout,"~ "); break;
    case OP_TRUE:  fprintf(fout,"TRUE=%g ",op.value); break;
    case OP_FALSE: fprintf(fout,"FALSE=%g ",op.value); break;
    case OP_ADD:   fprintf(fout,"+ "); break;
    case OP_SUB:   fprintf(fout,"- "); break;
    case OP_MUL:   fprintf(fout,"* "); break;
    case OP_DIV:   fprintf(fout,"/ "); break;
    case OP_BOOL:  fprintf(fout,"BOOL "); break;
    case OP_ABS:   fprintf(fout,"ABS "); break;
    case OP_SIN:   fprintf(fout,"SIN "); break;
    case OP_COS:   fprintf(fout,"COS "); break;
    case OP_EXP:   fprintf(fout,"EXP "); break;
    case OP_LOG:   fprintf(fout,"LOG "); break;
    case OP_SGN:   fprintf(fout,"SGN "); break;
    case OP_MAX:   fprintf(fout,"MAX "); break;
    case OP_MIN:   fprintf(fout,"MIN "); break;
    case OP_TAN:   fprintf(fout,"TAN "); break;
    case OP_TANH:  fprintf(fout,"TANH "); break;
    case OP_SINH:  fprintf(fout,"SINH "); break;
    case OP_COSH:  fprintf(fout,"COSH "); break;
    case OP_IF:    fprintf(fout,"? "); break;
    case OP_PLUS:  fprintf(fout,"PLUS "); break;
    case OP_MINUS: fprintf(fout,"MINUS "); break;
    case OP_PWR:   fprintf(fout,"PWR "); break;
    case OP_SQRT:  fprintf(fout,"SQRT "); break;
    case OP_INT:   fprintf(fout,"INT "); break;
    case OP_FLOOR: fprintf(fout,"FLOOR "); break;
    case OP_CEIL:  fprintf(fout,"CEIL "); break;
    case OP_SELMIN: fprintf(fout,"SELMIN "); break;
    case OP_AGAUSS: fprintf(fout,"AGAUSS "); break;
    default:       fprintf(fout,"%s ",nodes->p.pc[op.type]); break;
      }
    }
  }

int get(LIST *assignments, char *name, double *pv)
  {
  int j,k;
  LIST *l;

  for (j=assignments->max-1; j>=0; j--)
    {
    l=assignments->p.list[j];
    k=find_element_lazy_sort(l,&name,&passignstrcmp);
    if (k>=0) {*pv=l->p.assignment[k].value; return 1;}
    }
  *pv=0;
  return 0;
  }

void assign(LIST *assignments, char *name, double value)
  {
  int j;
  LIST *l;
  ASSIGNMENT a;

  assert(assignments->max>0);
  l=assignments->p.list[assignments->max-1];
  j=find_element_lazy_sort(l,&name,&passignstrcmp);
  if (j<0)
    {
    a.name=leak_strdup(name);
    a.value=value;
    list_insert_element_lazy_sort(l,&a,&passigncmp);
    }
  else l->p.assignment[j].value=value;
  }

void add_assignment_level(LIST *assignments)
  {
  LIST *l;

  l=list_create(sizeof(ASSIGNMENT));
  list_append_element(assignments,&l);
  }

void remove_assignment_level(LIST *assignments)
  {
  LIST *l;
  int j;

  assert(assignments->max>0);
  l=assignments->p.list[assignments->max-1];
  list_remove_element(assignments,assignments->max-1);
  for (j=0; j<l->max; j++) leak_free(l->p.assignment[j].name);
  list_free(l);
  }

LIST *duplicate_assignment_list(LIST *l)
  {
  int i;
  l = list_dup(l);
  for (i=0; i<l->max; i++)
    l->p.assignment[i].name=leak_strdup(l->p.assignment[i].name);
  return l;
  }

int assignment_list_cmp(void *p1, void *p2)
  {
  int i,c;
  LIST *pm1 = (LIST *) p1, *pm2 = (LIST *) p2;
  for (i=0; i<pm1->max && i<pm2->max; i++)
    {
    c = strcmp(pm1->p.assignment[i].name,pm2->p.assignment[i].name);
    if (c!=0) return c;
    if (pm1->p.assignment[i].value<pm2->p.assignment[i].value) return -1;
    if (pm1->p.assignment[i].value>pm2->p.assignment[i].value) return +1;
    }
  if (pm1->max<pm2->max) return -1;
  if (pm1->max>pm2->max) return +1;
  return 0;
  }

double evaluate_by_name(LIST *assignments, LIST *ops, LIST *nodes)
  {
  LIST *v;
  double x;
  int j;

  v=list_create(sizeof(double));
  for (j=0; j<nodes->max; j++)
    {
    x=0;
    if (!get(assignments,nodes->p.pc[j],&x))
      fprintf(stderr,"WARNING: variable %s unassigned, assuming 0.\n",nodes->p.pc[j]);
    list_append_element(v,&x);
    }
  x=evaluate(ops,0,v->p.f);
  list_free(v);
  return x;
  }

LIST *evaluate_parms(LIST *assignments, LIST *fmlas)
  {
  int j;
  double x;
  LIST *parms;

  parms=list_create(sizeof(double));
  for (j=0; j<fmlas->max; j++)
    {
    x=evaluate_by_name(assignments,fmlas->p.fmla[j].ops,
                                   fmlas->p.fmla[j].nodes);
    list_append_element(parms,&x);
    }
  return parms;
  }

FMLA create_fmla()
  {
  FMLA fmla;

  fmla.nodes=list_create(sizeof(char *));
  fmla.ops=list_create(sizeof(OP));
  return fmla;
  }

void free_fmla(FMLA fmla)
  {
  list_free(fmla.nodes);
  list_free(fmla.ops);
  }
