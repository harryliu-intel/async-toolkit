/*****************************************************
 * Copyright 2005 Fulcrum Microsystems               *
 * Copyright 1999 California Institute of Technology *
 * All rights reserved.  Written by Andrew Lines.    *
 *****************************************************/

/*** LIST data structures ***/

typedef unsigned char byte;
typedef struct _list
  {
  int max,size,mem,shrink;
  union
    {
    byte *b;
    char **pc;
    int *i;
    double *f;
    struct _panel **panel;
    struct _curve **curve;
    struct _named *named;
    struct _namesfile *namesfile;
    } p;
  } LIST;

#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>
#include <assert.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <readline/readline.h>
#ifndef Alpha
#include <readline/history.h>
#endif
#include "newlex.h"
#include "misc.h"
#include "leak.h"
#include "list.h"
#include "tracelib.h"
#include "glob.h"

#define MAX_PANELS 256

typedef struct _curve
  {
  char *name,*file;
  float *t,*v;
  int Nsteps,node,timestamp,is_subnet;
  struct _curve *alias,*subnet;
  } CURVE;

typedef struct _named
  {
  struct _curve *curve;
  LIST *curves;
  } NAMED;

typedef struct _panel
  {
  int number;
  char *prefix,*file;
  FILE *gnuplot;
  float tmin,tmax,vmin,vmax;
  LIST *curves; // all curves on this panel
  LIST *named; // organized by the names the user asked for
  } PANEL;

typedef struct _namesfile
  {
  char *file;
  LIST *names;
  time_t timestamp;
  } NAMESFILE;
