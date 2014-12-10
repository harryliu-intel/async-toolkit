/*****************************************************
 * Copyright 2005 Fulcrum Microsystems               *
 * Copyright 1999 California Institute of Technology *
 * All rights reserved.  Written by Andrew Lines.    *
 *****************************************************/

#include <stdlib.h>
#include <math.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include "misc.h"
#define STACKMAX 16384

typedef unsigned char byte;
typedef struct _list
  {
  int max,size,mem,shrink;
  union
    {
    byte *b;
    struct _fmla *fmla;
    struct _var  **pvar;
    struct _func **pfunc;
    struct _derivative **pderiv;
    } p;
  } LIST;

#include "misc.h"
#include "recalc.h"
#include "list.h"
#include "leak.h"

#define max(a,b) (a>b ? a:b)
#define min(a,b) (a<b ? a:b)

/*** global variables for standard recalc functions ***/
FUNC *func_add,*func_sub,*func_mul,*func_div;
FUNC *func_e,*func_ne,*func_l,*func_le,*func_g,*func_ge;
FUNC *func_if,*func_max,*func_min,*func_abs,*func_neg;
FUNC *func_and,*func_or;
FUNC *func_sin,*func_cos,*func_tan,*func_asin,*func_acos,*func_atan;
FUNC *func_exp,*func_log,*func_pow,*func_sqr;

/*** LIST comparison functions ***/

int func_cmp(void *p1, void *p2)
  {
  return strcmp((*((FUNC **)p1))->name,(*((FUNC **)p2))->name);
  }

int func_str_cmp(void *p1, void *p2)
  {
  return strcmp ((*((FUNC **)p1))->name,*((char **)p2));
  }

/** NOTE: this function is suspicous, and may not work! **/
int pvar_cmp(void *p1, void *p2)
  {
  return *((VAR **)p1) - *((VAR **)p2);
  }

int var_str_cmp(void *p1, void *p2)
  {
  return strcmp((*((VAR **)p1))->name,*((char **)p2));
  }

int var_cmp(void *p1, void *p2)
  {
  return strcmp((*((VAR **)p1))->name,(*((VAR **)p2))->name);
  }

int derivative_cmp(void *p1, void *p2)
  {
  int c;
  DERIVATIVE *pd1=*((DERIVATIVE **)p1),*pd2=*((DERIVATIVE **)p2);
  c = var_cmp(&pd1->top,&pd2->top);
  if (c==0) c = var_cmp(&pd1->bot,&pd2->bot);
  return c;
  }

/*** recalc functions ***/

void var_error(VAR *var, char *str)
  {
  fprintf(stderr,"ERROR: %s on variable %s.\n",str,var->name);
  exit(1);
  }

double get(VAR *var)
  {
  int i,tos=0;
  double stack[STACKMAX];
  FMLA fmla;

  if (var->valid) return var->val;
  if (var->visited) var_error(var,"recursion");
  var->visited=1;
  for (i=0; i<var->formula->max; i++)
    {
    fmla=var->formula->p.fmla[i];
    switch (fmla.op)
      {
      default:
      case OPERR:   var_error(var,"attempted to evaluate OPERR");
                    break;
      case OPFUNC1: if (tos>=1)
                      stack[tos-1]=fmla.u.func->u.func1(stack[tos-1]);
                    break;
      case OPFUNC2: if (tos>=2) 
                      stack[tos-2]=fmla.u.func->u.func2
                       (stack[tos-2],stack[tos-1]); 
                    tos--;
                    break;
      case OPFUNC3: if (tos>=3)
                      stack[tos-3] =
                        fmla.u.func->u.func3(stack[tos-3],stack[tos-2],stack[tos-1]);
                    tos-=2;
                    break;
      case OPCONSTANT: stack[tos]=fmla.u.constant;
                    tos++;
                    break;
      case OPVAR:   stack[tos]=get(fmla.u.var);
                    tos++;
                    break;
      case OPADD:   if (tos>=2) stack[tos-2]+=stack[tos-1];
                    tos--;
                    break;
      case OPSUB:   if (tos>=2) stack[tos-2]-=stack[tos-1];
                    tos--;
                    break;
      case OPMUL:   if (tos>=2) stack[tos-2]*=stack[tos-1];
                    tos--;
                    break;
      case OPDIV:   if (tos>=2) stack[tos-2]/=stack[tos-1];
                    tos--;
                    break;
      case OPNEG:   if (tos>=1) stack[tos-1]=-stack[tos-1];
                    break;
      case OPSQR:   if (tos>=1) stack[tos-1]*=stack[tos-1];
                    break;
      case OPMAX:   if (tos>=2) stack[tos-2]=max(stack[tos-1],stack[tos-2]);
                    tos--;
                    break;
      case OPMIN:   if (tos>=2) stack[tos-2]=min(stack[tos-1],stack[tos-2]);
                    tos--;
                    break;
      case OPABS:   if (tos>=1) stack[tos-1]=fabs(stack[tos-1]);
                    break;
      }
    if (tos<0) var_error(var,"evaluation stack underflow");
    if (tos>=STACKMAX) var_error(var,"evaluation stack overflow");
    }
  if (tos>0) var->val=stack[tos-1];
  var->valid=1;
  var->visited=0;
  return var->val;
  }
  
void invalidate(VAR *var)
  {
  int i;
  if (var->valid)
    {
    for (i=0; i<var->dep->max; i++) invalidate(var->dep->p.pvar[i]);
    var->valid=0;
    }    
  }
  
void set(VAR *var, double val)
  {
  if (var->val!=val) {var->val=val; invalidate(var);}
  }

/*** formula building/modifying interface functions ***/

void append_var(LIST *formula, VAR *ref)
  {
  FMLA fmla;
  fmla.op=OPVAR;
  fmla.u.var=ref;
  list_append_element(formula,&fmla);
  }
  
void append_constant(LIST *formula, double constant)
  {
  FMLA fmla;
  fmla.op=OPCONSTANT;
  fmla.u.constant=constant;
  list_append_element(formula,&fmla);
  }
  
void append_func(LIST *formula, FUNC *func)
  {
  FMLA fmla;
  fmla.op=func->op;
  fmla.u.func=func;
  list_append_element(formula,&fmla);
  }

void fmla_var(VAR *var, VAR *ref)
  {
  append_var(var->formula,ref);
  set_add_element(ref->dep,&var,&pvar_cmp);
  invalidate(var);
  }
  
void fmla_constant(VAR *var, double constant)
  {
  append_constant(var->formula,constant);
  invalidate(var);
  }
  
void fmla_func(VAR *var, FUNC *func)
  {
  append_func(var->formula,func);
  invalidate(var);
  }

void fmla_fmla(VAR *var, LIST *formula)
  {
  int i;
  FMLA fmla;
  for (i=0; i<formula->max; i++)
    {
    fmla=formula->p.fmla[i];
    if (fmla.op==OPVAR) set_add_element(fmla.u.var->dep,&var,&pvar_cmp);
    list_append_element(var->formula,&fmla);
    }
  invalidate(var);
  }

void clear_fmla(VAR *var)
  {
  int i;

  for (i=0; i<var->formula->max; i++)
    if (var->formula->p.fmla[i].op==OPVAR)
      set_remove_element(var->formula->p.fmla[i].u.var->dep,
        &var,&pvar_cmp);
  list_realloc(var->formula,0);
  invalidate(var);
  }
  
VAR *new_var(char *name, double val)
  {
  VAR *var;

  var=(VAR *) leak_malloc(sizeof(VAR));
  if (name==NULL) var->name=NULL;
  else var->name=leak_strdup(name);
  var->valid=0;
  var->visited=0;
  var->val=val;
  var->formula=list_create(sizeof(FMLA));
  var->dep=list_create(sizeof(VAR *));
  return var;
  }

/** get base index of subexpression, searching down from position N **/
int base_index(LIST *formula, int N)
  {
  FMLA fmla;
  if ((N<0)||(N>=formula->max)) return -1;
  fmla = formula->p.fmla[N];
  switch (fmla.op)
    {
    case OPNEG:
    case OPSQR:
    case OPABS:
    case OPFUNC1:
      N=base_index(formula,N-1);
      return N;
    case OPADD:
    case OPSUB:
    case OPMUL:
    case OPDIV:
    case OPMAX:
    case OPMIN:
    case OPFUNC2:
      N=base_index(formula,N-1);
      N=base_index(formula,N-1);
      return N;
    case OPFUNC3:
      N=base_index(formula,N-1);
      N=base_index(formula,N-1);
      N=base_index(formula,N-1);
      return N;
    }
  return N;
  }

/** extract subexpression list from N left **/
LIST *chain_expression(LIST *formula, int N)
  {
  LIST *expr;
  int i,base;
  base = base_index(formula,N);
  expr = list_create(sizeof(FMLA));
  if (base<0) return expr;
  for (i=base; i<=N; i++) list_append_element(expr,&formula->p.fmla[i]);
  return expr;
  }

/** test if a formula is a constant **/
int formula_is_constant(LIST *formula)
  {
  return (formula->max==1) && (formula->p.fmla[0].op==OPCONSTANT);
  }

/** test if formula ends in a specific function **/
int formula_ends_with_func(LIST *formula, FUNC *func)
  {
  FMLA fmla;
  if (formula->max==0) return 0;
  fmla = formula->p.fmla[formula->max-1];
  if (fmla.op == OPCONSTANT) return 0;
  if (fmla.op == OPVAR) return 0;
  if (fmla.u.func != func) return 0;
  return 1;
  }

/** if a formula is a constant, get value **/
double formula_get_constant(LIST *formula)
  {
  if (!formula_is_constant(formula)) return 0;
  return formula->p.fmla[0].u.constant;
  }

/** simplify a formula **/
void simplify_formula(LIST *formula)
  {
  FMLA fmla;
  VAR *var;
  LIST *a,*b,*c;
  int N,top_a,top_b,top_c;
  int ac,bc,cc,a0,b0,a1,b1;
  N = formula->max-1;
  if (N<0) return;
  fmla=formula->p.fmla[N];
  top_b = N-1;
  top_a = base_index(formula,top_b)-1;
  top_c = base_index(formula,top_a)-1;
  a = chain_expression(formula,top_a);
  b = chain_expression(formula,top_b);
  c = chain_expression(formula,top_c);
  simplify_formula(a);
  simplify_formula(b);
  simplify_formula(c);
  list_realloc(formula,0);
  list_append_list(formula,c);
  list_append_list(formula,a);
  list_append_list(formula,b);
  list_append_element(formula,&fmla);
  ac = formula_is_constant(a);
  bc = formula_is_constant(b);
  cc = formula_is_constant(c);
  a0 = ac && (formula_get_constant(a)==0);
  b0 = bc && (formula_get_constant(b)==0);
  a1 = ac && (formula_get_constant(a)==1);
  b1 = bc && (formula_get_constant(b)==1);
  switch(fmla.op)
    {
    case OPVAR:
      var = fmla.u.var;
      if (formula_is_constant(var->formula)) // inline constants
        {
        list_realloc(formula,0);
        append_constant(formula,formula_get_constant(var->formula));
        }
      break;

    case OPNEG:
      if (b0) // negate zero
        {
        list_remove_element(formula,formula->max-1);
        }
      else if (bc) // negate constant
        {
        list_realloc(formula,0);
        append_constant(formula,-formula_get_constant(b));
        }
      else if (formula_ends_with_func(b,func_neg)) // cancel double negatives
        {
        list_remove_list(formula,formula->max-2,2);
        }
      break;
      
    case OPADD:
      if (ac && bc) // add constants
        {
        list_realloc(formula,0);
        append_constant(formula,formula_get_constant(a) + formula_get_constant(b));
        }
      else if (a0) // a is zero
        {
        list_realloc(formula,0);
        list_append_list(formula,b);
        }
      else if (b0) // b is zero
        {
        list_realloc(formula,0);
        list_append_list(formula,a);
        }
      break;

    case OPSUB:
      if (ac && bc) // subtract constants
        {
        list_realloc(formula,0);
        append_constant(formula,formula_get_constant(a) - formula_get_constant(b));
        }
      else if (a0) // a is zero
        {
        list_realloc(formula,0);
        list_append_list(formula,b);
        append_func(formula,func_neg);
        }
      else if (b0) // b is zero
        {
        list_realloc(formula,0);
        list_append_list(formula,a);
        }
      break;

    case OPMUL:
      if (a0 || b0) // multiply by zero
        {
        list_realloc(formula,0);
        append_constant(formula,0);
        }
      else if (ac && bc) // multiply constants
        {
        list_realloc(formula,0);
        append_constant(formula,formula_get_constant(a) * formula_get_constant(b));
        }
      else if (a1) // multiply by 1
        {
        list_realloc(formula,0);
        list_append_list(formula,b);
        }
      else if (b1) // multiply by 1
        {
        list_realloc(formula,0);
        list_append_list(formula,a);
        }
      break;

    case OPDIV:
      if (ac && bc) // divide constants
        {
        list_realloc(formula,0);
        append_constant(formula,formula_get_constant(a) / formula_get_constant(b));
        }
      else if (a0) // numerator is zero
        {
        list_realloc(formula,0);
        append_constant(formula,0);
        }
      else if (b1) // denominator is one
        {
        list_realloc(formula,0);
        list_append_list(formula,a);
        }
      break;

    case OPMAX:
      if (ac && bc) // max of constants
        {
        list_realloc(formula,0);
        append_constant(formula,max(formula_get_constant(a),formula_get_constant(b)));
        }
      break;

    case OPMIN:
      if (ac && bc) // min of constants
        {
        list_realloc(formula,0);
        append_constant(formula,min(formula_get_constant(a),formula_get_constant(b)));
        }
      break;

    case OPSQR:
      if (bc) // square of constant
        {
        list_realloc(formula,0);
        append_constant(formula,formula_get_constant(b)*formula_get_constant(b));
        }
      break;

    case OPABS:
      if (bc) // absolute value of constant
        {
        list_realloc(formula,0);
        append_constant(formula,fabs(formula_get_constant(b)));
        }
      break;

    case OPFUNC1:
      if (bc) // unary function of constant
        {
        list_realloc(formula,0);
        append_constant(formula,fmla.u.func->u.func1(formula_get_constant(b)));
        }
      break;

    case OPFUNC2:
      if (ac && bc) // binary function of constants
        {
        list_realloc(formula,0);
        append_constant(formula,fmla.u.func->u.func2(formula_get_constant(a),
                                                     formula_get_constant(b)));
        }
      break;

    case OPFUNC3:
      if (fmla.u.func==func_if) // check for idential choices in IF
        {
        if (cc && ac && (formula_get_constant(c)==formula_get_constant(a)))
          {
          list_realloc(formula,0);
          append_constant(formula,formula_get_constant(c));
          }
        }
      break;
    }

  list_free(a);
  list_free(b);
  list_free(c);
  }

/** append_func then simplify_formula **/
void append_func_simplify(LIST *formula, FUNC *func)
  {
  append_func(formula,func);
  simplify_formula(formula);
  }

/** get formula for derivative of the sub-expression from N left **/
LIST *chain_derivative(LIST *variables, LIST *derivatives, 
                       LIST *formula, int N, VAR *bot)
  {
  VAR *var;
  LIST *deriv;
  LIST *a,*b,*da,*db;
  FMLA fmla;
  int top_a,top_b;
  deriv = list_create(sizeof(FMLA));
  if (N<0) {fprintf(stderr,"ERROR: underflow in chain_derivative.\n"); exit(1);}
  fmla=formula->p.fmla[N];
  
  /** look at last element of formula **/
  switch(fmla.op)
    {
    case OPCONSTANT:
      append_constant(deriv,0);
      break;
    case OPVAR:
      if (fmla.u.var->formula->max==0) // independent variable
        {
        append_constant(deriv,fmla.u.var==bot); // 0 or 1
        }
      else // recurse for dependent variable
        {
        var = derivative_var(variables,derivatives,fmla.u.var,bot);
        append_var(deriv,var);
        }
      break;
    case OPADD:
      top_b = N-1;
      top_a = base_index(formula,top_b)-1;
      da = chain_derivative(variables,derivatives,formula,top_a,bot);
      db = chain_derivative(variables,derivatives,formula,top_b,bot);
      list_append_list(deriv,da);
      list_append_list(deriv,db);
      append_func(deriv,func_add);
      list_free(da);
      list_free(db);
      break;
    case OPSUB:
      top_b = N-1;
      top_a = base_index(formula,top_b)-1;
      da = chain_derivative(variables,derivatives,formula,top_a,bot);
      db = chain_derivative(variables,derivatives,formula,top_b,bot);
      list_append_list(deriv,da);
      list_append_list(deriv,db);
      append_func(deriv,func_sub);
      list_free(da);
      list_free(db);
      break;
    case OPMUL:
      top_b = N-1;
      top_a = base_index(formula,top_b)-1;
      a = chain_expression(formula,top_a);
      b = chain_expression(formula,top_b);
      da = chain_derivative(variables,derivatives,formula,top_a,bot);
      db = chain_derivative(variables,derivatives,formula,top_b,bot);
      list_append_list(deriv,da);
      list_append_list(deriv,b);
      append_func(deriv,func_mul);
      list_append_list(deriv,db);
      list_append_list(deriv,a);
      append_func(deriv,func_mul);
      append_func(deriv,func_add);
      list_free(a);
      list_free(b);
      list_free(da);
      list_free(db);
      break;
    case OPDIV:
      top_b = N-1;
      top_a = base_index(formula,top_b)-1;
      a = chain_expression(formula,top_a);
      b = chain_expression(formula,top_b);
      da = chain_derivative(variables,derivatives,formula,top_a,bot);
      db = chain_derivative(variables,derivatives,formula,top_b,bot);
      list_append_list(deriv,da);
      list_append_list(deriv,b);
      append_func(deriv,func_div);
      list_append_list(deriv,a);
      list_append_list(deriv,db);
      append_func(deriv,func_mul);
      list_append_list(deriv,b);
      append_func(deriv,func_sqr);
      append_func(deriv,func_div);
      append_func(deriv,func_sub);
      list_free(a);
      list_free(b);
      list_free(da);
      list_free(db);
      break;
    case OPMAX:
      top_b = N-1;
      top_a = base_index(formula,top_b)-1;
      a = chain_expression(formula,top_a);
      b = chain_expression(formula,top_b);
      da = chain_derivative(variables,derivatives,formula,top_a,bot);
      db = chain_derivative(variables,derivatives,formula,top_b,bot);
      list_append_list(deriv,da);
      list_append_list(deriv,db);
      list_append_list(deriv,a);
      list_append_list(deriv,b);
      append_func(deriv,func_ge);
      append_func(deriv,func_if);
      list_free(a);
      list_free(b);
      list_free(da);
      list_free(db);
      break;
    case OPMIN:
      top_b = N-1;
      top_a = base_index(formula,top_b)-1;
      a = chain_expression(formula,top_a);
      b = chain_expression(formula,top_b);
      da = chain_derivative(variables,derivatives,formula,top_a,bot);
      db = chain_derivative(variables,derivatives,formula,top_b,bot);
      list_append_list(deriv,da);
      list_append_list(deriv,db);
      list_append_list(deriv,a);
      list_append_list(deriv,b);
      append_func(deriv,func_le);
      append_func(deriv,func_if);
      list_free(a);
      list_free(b);
      list_free(da);
      list_free(db);
      break;
    case OPNEG:
      top_a = N-1;
      da = chain_derivative(variables,derivatives,formula,top_a,bot);
      list_append_list(deriv,da);
      append_func(deriv,func_neg);
      list_free(da);
      break;
    case OPSQR:
      top_a = N-1;
      a = chain_expression(formula,top_a);
      da = chain_derivative(variables,derivatives,formula,top_a,bot);
      list_append_list(deriv,da);
      list_append_list(deriv,a);
      append_func(deriv,func_mul);
      append_constant(deriv,2);
      append_func(deriv,func_mul);
      list_free(a);
      list_free(da);
      break;
    case OPABS:
      top_a = N-1;
      a = chain_expression(formula,top_a);
      da = chain_derivative(variables,derivatives,formula,top_a,bot);
      list_append_list(deriv,da);
      append_constant(deriv,0);
      list_append_list(deriv,da);
      append_func(deriv,func_sub);
      list_append_list(deriv,a);
      append_constant(deriv,0);
      append_func(deriv,func_ge);
      append_func(deriv,func_if);
      list_free(a);
      list_free(da);
      break;
    case OPFUNC1:
      if (fmla.u.func == func_exp)
        {
        top_a = N-1;
        a = chain_expression(formula,top_a);
        da = chain_derivative(variables,derivatives,formula,top_a,bot);
        list_append_list(deriv,da);
        list_append_list(deriv,a);
        append_func(deriv,func_exp);
        append_func(deriv,func_mul);
        list_free(a);
        list_free(da);
        break;
        }
    default:
      fprintf(stderr,"ERROR: tried to differentiate function op=%d.\n",fmla.op);
      exit(1);
      break;
    }
  simplify_formula(deriv);
  return deriv;
  }

/** Compute the derivative of top wrt bot.  Cache it for future use. **/
VAR *derivative_var(LIST *variables, LIST *derivatives,
                    VAR *top, VAR *bot)
  {
  DERIVATIVE d,*pd;
  VAR *var;
  LIST *deriv_formula;
  char name[STRMAX];
  int i;

  /** look for cached derivative **/
  d.var = NULL;
  d.top = top;
  d.bot = bot;
  pd = &d;
  i = find_element_lazy_sort(derivatives,&pd,&derivative_cmp);
  if (i>=0) return derivatives->p.pderiv[i]->var;

  /** create new variable **/
  safe_sprintf(name,"d%s_d%s",top->name,bot->name);
  var = new_var(name,0);
  list_insert_element_lazy_sort(variables,&var,&var_cmp);

  /*** create formula for derivative **/
  if (bot->formula->max!=0) // bad for bot to be dependent
    var_error(var,"can't differentiate by dependent variable");
  else if (top->formula->max==0) // independent variable
    fmla_constant(var,top==bot); // 0 or 1
  else // dependent variable
    {
    deriv_formula=chain_derivative(variables,derivatives,
                                   top->formula,top->formula->max-1,bot);
    fmla_fmla(var,deriv_formula);
    list_free(deriv_formula);
    }

  /** cache this derivative for reuse later **/
  pd = (DERIVATIVE *) leak_malloc(sizeof(DERIVATIVE));
  pd->var = var;
  pd->top = top;
  pd->bot = bot;
  list_insert_element_lazy_sort(derivatives,&pd,&derivative_cmp);
  return var;
  }

void free_var(VAR *var)
  {
  int i,j;
  VAR *dep;

  if (var->name!=NULL) leak_free(var->name);
  clear_fmla(var);
  for (i=0; i<var->dep->max; i++)
    {
    dep=var->dep->p.pvar[i];
    for (j=0; j<dep->formula->max; j++)
      if ((dep->formula->p.fmla[j].op==OPVAR)&&
          (dep->formula->p.fmla[j].u.var==var))
        dep->formula->p.fmla[j].op=OPERR;
    invalidate(dep);
    }
  list_free(var->formula);
  list_free(var->dep);
  leak_free(var);
  }

void free_variables(LIST *variables)
  {
  int i;
  for (i=0; i<variables->max; i++) free_var(variables->p.pvar[i]);
  list_free(variables);
  }

void free_derivatives(LIST *derivatives)
  {
  int i;
  for (i=0; i<derivatives->max; i++) leak_free(derivatives->p.pderiv[i]);
  list_free(derivatives);
  }

/*** input and output routines ***/

void decode_fmla(LIST *formula, char *s)
  {
  int i,j;
  FMLA fmla;

  j=0; s[0]=0;
  for (i=0; i<formula->max; i++)
    {
    fmla=formula->p.fmla[i];
    switch (fmla.op)
      {
      case OPVAR: snprintf(s+j,STRMAX-j,"%s ",fmla.u.var->name);
                  j=strlen(s);
                  break;
      case OPCONSTANT: snprintf(s+j,STRMAX-j,"%.8g ",fmla.u.constant);
                  j=strlen(s);
                  break;
      case OPERR: snprintf(s+j,STRMAX-j,"ERR ");
                  j=strlen(s);
                  break;
      default:    snprintf(s+j,STRMAX-j,"%s ",fmla.u.func->name);
                  j=strlen(s);
                  break;
      }
    }
  }
  
void print_var(FILE *fp, VAR *var)
  {
  char decode[STRMAX];

  if (var->formula->max==0)
    fprintf(fp,"%s=%.8g\n",var->name,get(var));
  else
    {
    decode_fmla(var->formula,decode);
    fprintf(fp,"%s=(%s)=%.8g\n",var->name,decode,get(var));
    }
  }

int print_var_infix(FILE *fp, VAR *var, int N)
  {
  int i,tos=0;
  FMLA fmla;
  char *stack[STACKMAX];
  char str[STRMAX];
  fprintf(fp,"var%p = ",var);
  if (var->formula->max==0) /* independent variable */
    {
    fprintf(fp,"independent(%d); %% %s\n",N,var->name);
    return N+1;
    }
  for (i=0; i<var->formula->max; i++)
    {
    fmla=var->formula->p.fmla[i];
    switch (fmla.op)
      {
      case OPVAR:
	if (snprintf(str,STRMAX,"var%p",fmla.u.var)>=STRMAX)
          var_error(var,"print_var_infix string overflow");

	stack[tos++]=leak_strdup(str);
	break;
      case OPCONSTANT:
	if (snprintf(str,STRMAX,"%.8g",fmla.u.constant)>=STRMAX)
          var_error(var,"print_var_infix string overflow");
	stack[tos++]=leak_strdup(str);
	break;
      case OPABS:
      case OPSQR:
      case OPNEG:
      case OPFUNC1:
	if (tos<1) var_error(var,"print_var_infix stack underflow");
	if (snprintf(str,STRMAX,"%s(%s)",fmla.u.func->name,stack[tos-1])>=STRMAX)
          var_error(var,"print_var_infix string overflow");
	leak_free(stack[tos-1]); tos--;
	stack[tos++]=leak_strdup(str);
	break;
      case OPMAX:
      case OPMIN:
      case OPFUNC2:
	if (tos<2) var_error(var,"print_var_infix stack underflow");
	if (snprintf(str,STRMAX,"%s(%s,%s)",fmla.u.func->name,
                     stack[tos-2],stack[tos-1])>=STRMAX)
          var_error(var,"print_var_infix string overflow");
	leak_free(stack[tos-1]); tos--;
	leak_free(stack[tos-1]); tos--;
	stack[tos++]=leak_strdup(str);
	break;
      case OPFUNC3:
	if (tos<3) var_error(var,"print_var_infix stack underflow");
	if (snprintf(str,STRMAX,"%s(%s,%s,%s)",fmla.u.func->name,
                     stack[tos-3],stack[tos-2],stack[tos-1])>=STRMAX)
          var_error(var,"print_var_infix string overflow");
	leak_free(stack[tos-1]); tos--;
	leak_free(stack[tos-1]); tos--;
	leak_free(stack[tos-1]); tos--;
	stack[tos++]=leak_strdup(str);
	break;
      case OPADD:
      case OPSUB:
      case OPMUL:
      case OPDIV:
	if (tos<2) var_error(var,"print_var_infix stack underflow");
        if (snprintf(str,STRMAX,"(%s %s %s)",stack[tos-2],fmla.u.func->name,
                     stack[tos-1])>=STRMAX)
          var_error(var,"print_var_infix string overflow");
	leak_free(stack[tos-1]); tos--;
	leak_free(stack[tos-1]); tos--;
	stack[tos++]=leak_strdup(str);
	break;
      default:
      case OPERR:
	var_error(var,"print_var_infix attempted to print OPERR");
	break;
      }
    if (tos>=STACKMAX)
      var_error(var,"print_var_infix stack overflow");
    }
  fprintf(fp,"%s; %% %s\n",stack[tos-1],var->name);
  for (i=0; i<tos; i++) leak_free(stack[i]);
  return N;
  }

int print_var_tree(FILE *fp, VAR *var, LIST *defined_vars, int N)
  {
  int i;
  if (find_element_lazy_sort(defined_vars,var,&pvar_cmp)>=0) return N;
  for (i=0; i<var->formula->max; i++) if (var->formula->p.fmla[i].op == OPVAR)
    N=print_var_tree(fp,var->formula->p.fmla[i].u.var,defined_vars,N);
  N=print_var_infix(fp,var,N);
  list_insert_element_lazy_sort(defined_vars,var,&pvar_cmp);
  return N;
  }
			 
char *get_tok(char *line, char *tok)
  {
  int i,j;

  i=j=0;
  while (isspace(line[i])) i++;
  while ((j<STRMAX-1)&&(!isspace(line[i]))&&(line[i]!=0)) {tok[j]=line[i]; i++; j++;}
  while (isspace(line[i])) i++;
  tok[j]=0;
  return &line[i];
  }

VAR *find_var(LIST *variables, char *name) {
  int k;
  k=find_element_lazy_sort(variables,&name,&var_str_cmp);
  if (k<0) return NULL;
  else return variables->p.pvar[k];
}

VAR *parse_rpn (LIST *variables, LIST *functions, char *line)
  {
  char tok[STRMAX],*p=tok;
  int k;
  VAR *var;
  double x;

  /*** find or create the variable ***/
  line=get_tok(line,tok);
  if (tok[0]==0) return NULL;
  k=find_element_lazy_sort(variables,&p,&var_str_cmp);
  if (k<0) {
    var=new_var(tok,0);
    list_insert_element_lazy_sort(variables,&var,&var_cmp);
  }
  else {
    var=variables->p.pvar[k];
    clear_fmla(var);
  }

  /*** eat the equal sign, if any ***/
  line=get_tok(line,tok);
  if (strcmp(tok,"=")!=0) return var;

  /*** parse the formula ***/
  line=get_tok(line,tok);
  while (tok[0]!=0)
    {
    if (sscanf(tok,"%lf",&x)==1) fmla_constant(var,x);
    else
      {
      k=find_element_lazy_sort(variables,&p,&var_str_cmp);
      if (k>=0) fmla_var(var,variables->p.pvar[k]);
      else
        {
        k=find_element_sorted(functions,&p,&func_str_cmp);
        if (k>=0) fmla_func(var,functions->p.pfunc[k]);
        else {clear_fmla(var); return NULL;}
        }
      }
    line=get_tok(line,tok);
    }
  
  return var;
  }
  
/*** routines related to defining functions ***/
  
FUNC *new_func(int op, char *name, void *p)
  {
  FUNC *func;

  func=(FUNC *) leak_malloc(sizeof(FUNC));
  func->op=op;
  if (name==NULL) func->name=NULL;
  else func->name=(char *) leak_strdup(name);
  func->u.func=(FUNC *) p;
  return func;
  }
  
void free_func(FUNC *func)
  {
  if (func->name!=NULL) leak_free(func->name);
  leak_free(func);
  }

/*** some functions for use in recalc ***/

double f_e(double a, double b) {return (a==b);}
double f_ne(double a, double b) {return (a!=b);}
double f_g(double a, double b) {return (a>b);}
double f_ge(double a, double b) {return (a>=b);}
double f_l(double a, double b) {return (a<b);}
double f_le(double a, double b) {return (a<=b);}
double f_and(double a, double b) {return (a!=0)&&(b!=0);}
double f_or(double a, double b) {return (a!=0)||(b!=0);}
double f_sqr(double x) {return x*x;}
double f_if(double a, double b, double c) {return (c ? a : b);}

/*** allocate and free the standard recalc functions ***/
  
LIST *alloc_functions()
  {
  LIST *functions;

  functions=list_create(sizeof(FUNC *));

  func_add=new_func(OPADD,"+",NULL);
  list_append_element(functions,&func_add);
  func_sub=new_func(OPSUB,"-",NULL);
  list_append_element(functions,&func_sub);
  func_mul=new_func(OPMUL,"*",NULL);
  list_append_element(functions,&func_mul);
  func_div=new_func(OPDIV,"/",NULL);
  list_append_element(functions,&func_div);
  func_sqr=new_func(OPSQR,"SQR",NULL);
  list_append_element(functions,&func_sqr);
  func_neg=new_func(OPNEG,"NEG",NULL);
  list_append_element(functions,&func_neg);

  func_max=new_func(OPMAX,"MAX",NULL);
  list_append_element(functions,&func_max);
  func_min=new_func(OPMIN,"MIN",NULL);
  list_append_element(functions,&func_min);
  func_abs=new_func(OPABS,"ABS",NULL);
  list_append_element(functions,&func_abs);

  func_e=new_func(OPFUNC2,"=",(void *)(&f_e));
  list_append_element(functions,&func_e);
  func_ne=new_func(OPFUNC2,"!=",(void *)(&f_ne));
  list_append_element(functions,&func_ne);
  func_g=new_func(OPFUNC2,">",(void *)(&f_g));
  list_append_element(functions,&func_g);
  func_ge=new_func(OPFUNC2,">=",(void *)(&f_ge));
  list_append_element(functions,&func_ge);
  func_l=new_func(OPFUNC2,"<",(void *)(&f_l));
  list_append_element(functions,&func_l);
  func_le=new_func(OPFUNC2,"<=",(void *)(&f_le));
  list_append_element(functions,&func_le);

  func_and=new_func(OPFUNC2,"&&",(void *)(&f_and));
  list_append_element(functions,&func_and);
  func_or=new_func(OPFUNC2,"||",(void *)(&f_or));
  list_append_element(functions,&func_or);

  func_if=new_func(OPFUNC3,"IF",(void *)(&f_if));
  list_append_element(functions,&func_if);

  func_sin=new_func(OPFUNC1,"SIN",(void *)(&sin));
  list_append_element(functions,&func_sin);
  func_cos=new_func(OPFUNC1,"COS",(void *)(&cos));
  list_append_element(functions,&func_cos);
  func_tan=new_func(OPFUNC1,"TAN",(void *)(&tan));
  list_append_element(functions,&func_tan);
  func_asin=new_func(OPFUNC1,"ASIN",(void *)(&asin));
  list_append_element(functions,&func_asin);
  func_acos=new_func(OPFUNC1,"ACOS",(void *)(&acos));
  list_append_element(functions,&func_acos);
  func_atan=new_func(OPFUNC1,"ATAN",(void *)(&atan));
  list_append_element(functions,&func_atan);

  func_exp=new_func(OPFUNC1,"EXP",(void *)(&exp));
  list_append_element(functions,&func_exp);
  func_log=new_func(OPFUNC1,"LOG",(void *)(&log));
  list_append_element(functions,&func_log);
  func_pow=new_func(OPFUNC2,"^",(void *)(&pow));
  list_append_element(functions,&func_pow);

  list_sort(functions,&func_cmp);
  return(functions);
  }

void free_functions(LIST *functions)
  {
  int i;

  for (i=0; i<functions->max; i++) free_func(functions->p.pfunc[i]);
  list_free(functions);
  }
