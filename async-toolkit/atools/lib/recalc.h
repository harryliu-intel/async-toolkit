/*****************************************************
 * Copyright 2005 Fulcrum Microsystems               *
 * Copyright 1999 California Institute of Technology *
 * All rights reserved.  Written by Andrew Lines.    *
 *****************************************************/

/*** constant definitions ***/

#define OPERR   ((int) 0)
#define OPFUNC1 ((int) 1)
#define OPFUNC2 ((int) 2)
#define OPFUNC3 ((int) 3)
#define OPVAR   ((int) 4)
#define OPCONSTANT ((int) 5)
#define OPADD ((int) 6)
#define OPSUB ((int) 7)
#define OPMUL ((int) 8)
#define OPDIV ((int) 9)
#define OPMAX ((int) 10)
#define OPMIN ((int) 11)
#define OPABS ((int) 12)
#define OPSQR ((int) 13)
#define OPNEG ((int) 14)

/*** type definitions ***/
  
typedef double FUNC1(double);
typedef double FUNC2(double,double);
typedef double FUNC3(double,double,double);

typedef struct _fmla
  {
  int op;
  union 
    {
    struct _var *var;
    struct _func *func;
    double constant;
    } u;
  } FMLA;

typedef struct _var
  {
  char *name;
  int valid:1,visited:1;
  double val;
  LIST *formula;
  LIST *dep;
  } VAR;

typedef struct _derivative
  {
  VAR *var;
  VAR *top;
  VAR *bot;
  } DERIVATIVE;

typedef struct _func
  {
  int op;
  union
    {
    void *func;
    FUNC1 *func1;
    FUNC2 *func2;
    FUNC3 *func3;
    } u;
  char *name;
  } FUNC;

/*** function prototypes ***/

int var_cmp(void *, void *);
int pvar_cmp(void *, void *);
int var_str_cmp(void *, void *);
int derivative_cmp(void *, void *);
int func_cmp(void *, void *);
int func_str_cmp(void *, void *);

double get(VAR *);
void set(VAR *, double);
void invalidate(VAR *);

void fmla_var(VAR *, VAR *);
void fmla_func(VAR *, FUNC *);
void fmla_constant(VAR *, double);
void clear_fmla(VAR *);
VAR *new_var(char *, double);
void free_var(VAR *);
void free_variables(LIST *variables);
void free_derivatives(LIST *derivatives);
VAR *derivative_var(LIST *variables, LIST *derivatives, VAR *top, VAR *bot);

VAR *find_var(LIST *, char *);
VAR *parse_rpn(LIST *, LIST *, char *);
void decode_fmla(LIST *, char *);
void print_var(FILE *, VAR *);
int print_var_infix(FILE *fp, VAR *var, int N);
int print_var_tree(FILE *fp, VAR *var, LIST *defined_vars, int N);
void print_derivative(FILE *fp, DERIVATIVE *deriv);

FUNC *new_func(int, char *, void *);
void free_func(FUNC *);
LIST *alloc_functions();
void free_functions(LIST *);

/*** global variables for standard recalc functions ***/
extern FUNC *func_add,*func_sub,*func_mul,*func_div;
extern FUNC *func_e,*func_ne,*func_l,*func_le,*func_g,*func_ge;
extern FUNC *func_if,*func_max,*func_min,*func_abs;
extern FUNC *func_and,*func_or;
extern FUNC *func_sin,*func_cos,*func_tan,*func_asin,*func_acos,*func_atan;
extern FUNC *func_exp,*func_log,*func_pow,*func_sqr;
