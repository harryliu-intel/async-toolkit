/*** list type ***/
typedef unsigned char byte;
typedef struct _list {
  int max,size,mem,shrink;
  union {
    byte *b;
    struct _list **plist;
    struct _fmla *fmla;
    struct _var  **pvar;
    struct _func **pfunc;
    struct _derivative **pderiv;
  } p;
} LIST;

/*** definitions ***/
#define FILEMAX   256 /* maximum number of open files */
#define LEVELSMAX  10 /* maximum number of gates in series */

/*** include files ***/
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <assert.h>
#include <string.h>
#include <ctype.h>
#include <signal.h>
#include <unistd.h>
#include "leak.h"
#include "newlex.h"
#include "misc.h"
#include "list.h"
#include "minimize.h"
#include "recalc.h"
#include "path.h"

/*** macros ***/
#define max(a,b) (a>b?a:b)
#define min(a,b) (a<b?a:b)
#define round(x) ((int) rint(x))

/*** types ***/
typedef struct _solve {
  double constraint_weight,
    initial_weight,
    final_weight,
    iteration_ratio,
    step_weight,
    min_gradient,
    tolerance;
  int analytic;
  LIST *current_group;
  LIST *variables, *functions, *groups;
  LIST *derivatives;
  VAR *energy,*constraint;
  VAR **dE, **dC;
} SOLVE;

/*** prototypes ***/
void init_solve();
void parse(FILE *fin, int argc, char *argv[]);
void free_solve();
extern int ERROR;
extern FILE *output;
