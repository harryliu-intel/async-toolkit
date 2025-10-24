/*****************************************************
 * Copyright 2005 Fulcrum Microsystems               *
 * Copyright 1999 California Institute of Technology *
 * All rights reserved.  Written by Andrew Lines.    *
 *****************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "vector.h"
#include "minimize.h"

/*** constants ***/

#define GOLD          1.61803
#define STEP          0.38197
#define TINY          1e-10
#define TOLERANCE     1e-3
#define BRACKET_STEPS 100
#define GOLDEN_STEPS  10

/***
 * Find line minimum with golden section search.  Consult Numerical
 * Recipes.
 ***/
double line_minimum(void *parameters, ENERGY *energy, 
                    int n, double *v, double *dir, double *vv)
  {
  static double initial_interval=TINY;
  double a,b,c,t,fa,fb,fc,ft,quit_interval;
  int i,steps=0;

  /*** pick two points along dir at 0 and initial_interval ***/
  a=0;
  v_scale(n,dir,a,vv);
  v_add(n,v,vv,vv);
  fa=energy(parameters,vv);

  b=initial_interval;
  v_scale(n,dir,b,vv);
  v_add(n,v,vv,vv);
  fb=energy(parameters,vv);

  /*** sort them by energy ***/
  if (fb>fa)
    {
    t=a; a=b; b=t;
    ft=fa; fa=fb; fb=ft;
    }
  
  /*** attempt to bracket the minimum ***/
  while (1)
    {
    c=b+(b-a)*GOLD;
    v_scale(n,dir,c,vv);
    v_add(n,v,vv,vv);
    fc=energy(parameters,vv);
    if (fc<fb)
      {
      a=b; fa=fb;
      b=c; fb=fc;
      }
    else break;
    steps++;
    if (steps>BRACKET_STEPS)
      {
      fprintf(stderr,"WARNING: %d steps in line_minimum\n",steps);
      break;
      }
    }

  /*** golden section search for minimum ***/
  steps=0;
  quit_interval=TOLERANCE*fabs(a-c);
  while((fabs(a-c)>quit_interval) && (steps<GOLDEN_STEPS))
    {
    if (fabs(b-a)>fabs(c-b))
      {
      t=b+(a-b)*STEP;
      v_scale(n,dir,t,vv);
      v_add(n,v,vv,vv);
      ft=energy(parameters,vv);
      if (ft<=fb)
        {
        c=b; fc=fb;
        b=t; fb=ft;
        }
      else
        {
        a=t; fa=ft;
        }
      }
    else
      {
      t=b+(c-b)*STEP;
      v_scale(n,dir,t,vv);
      v_add(n,v,vv,vv);
      ft=energy(parameters,vv);
      if (ft<fb)
        {
        a=b; fa=fb;
        b=t; fb=ft;
        }
      else
        {
        c=t; fc=ft;
        }
      }
    steps++;
    }

  /*** set vv to new minimum ***/
  v_scale(n,dir,b,vv);
  v_add(n,v,vv,v);

  /*** adaptively adjust initial_interval ***/
  b=fabs(b);
  initial_interval=0.75*initial_interval+0.25*b;
  if (initial_interval<TINY) initial_interval=TINY;
  
  /*** return maximum absolute change in any independent variable ***/
  a=0;
  for (i=0; i<n; i++) if (fabs(vv[i])>a) a=fabs(vv[i]);
  return a;
  }

/***
 * Conjugate gradient descent.  Polak-Ribiere formula.  Resets if it
 * gets stuck.  Consult Numerical Recipes.
 ***/
int minimize(FILE *record, void *parameters, ENERGY *energy, DOWN *down,
              int n, double *p, int iterations, double min_gradient, double tolerance)
  {
  int i;
  double *h,*g,*g1,mg,mg1,step,step1,beta;
  double min_magsqr=min_gradient*min_gradient;

  h= (double *) malloc (n*sizeof(double));
  g= (double *) malloc (n*sizeof(double));
  g1=(double *) malloc (n*sizeof(double));

  step=tolerance*10;
  down(parameters,p,h);
  v_copy(n,h,g);
  mg=v_magsqr(n,g);
  if (record!=NULL)
    fprintf(record," 0 E=%.10g dE=%.10g\n",
            energy(parameters,p),sqrt(mg));
  for (i=1; (i<=iterations)&&(mg>=min_magsqr)&&(step>=tolerance); i++)
    {
    step1=line_minimum(parameters,energy,n,p,h,g1);
    step=0.9*step+0.1*step1; // exponentially weighted moving average
    down(parameters,p,g1);
    mg1=v_magsqr(n,g1);
    beta=(mg1-v_dot(n,g,g1))/mg;
    if (beta<0) beta=0;
    v_scale(n,h,beta,h);
    v_add(n,g1,h,h);
    v_copy(n,g1,g);
    mg=mg1;
    if (record!=NULL)
      fprintf(record," %d E=%.10g dE=%.10g Step=%.10g\n",
              i,energy(parameters,p),sqrt(mg),step1);
    }

  free(h);
  free(g);
  free(g1);
  return i-1;
  }

/*** steepest descent line minimizations ***/
int steepest_descent(FILE *record, void *parameters,
              ENERGY *energy, DOWN *down,
              int n, double *p, int iterations, double min_gradient, double tolerance)
  {
  int i;
  double *g,*t,mg,step,step1;
  double min_magsqr=min_gradient*min_gradient;

  g=(double *) malloc (n*sizeof(double));
  t=(double *) malloc (n*sizeof(double));

  step=10*tolerance;
  down(parameters,p,g);
  mg=v_magsqr(n,g);
  for (i=1; (i<=iterations)&&(mg>=min_magsqr)&&(step>=tolerance); i++)
    {
    step1=line_minimum(parameters,energy,n,p,g,t);
    step=0.9*step+0.1*step1;
    down(parameters,p,g);
    mg=v_magsqr(n,g);
    if (record!=NULL)
      fprintf(record," %d E=%.10g dE=%.10g Step=%.10g\n",
        i,energy(parameters,p),sqrt(mg),step1);
    }

  free(g);
  free(t);
  return i-1;
  }
