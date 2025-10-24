/*****************************************************
 * Copyright 2005 Fulcrum Microsystems               *
 * Copyright 1999 California Institute of Technology *
 * All rights reserved.  Written by Andrew Lines.    *
 *****************************************************/

#include <math.h>
#include "vector.h"

double v_dot (int max, double *a, double *b)
  {
  int i;
  double x;
 
  x=0;
  for (i=0; i<max; i++) x+=a[i]*b[i];
  return x;
  }

double v_mag (int max, double *a)
  {
  int i;
  double x;

  x=0;
  for (i=0; i<max; i++) x+=a[i]*a[i];
  return sqrt(x);
  }

double v_magsqr (int max, double *a)
  {
  int i;
  double x;

  x=0;
  for (i=0; i<max; i++) x+=a[i]*a[i];
  return x;
  }

void v_add (int max, double *a, double *b, double *c)
  {
  int i;

  for (i=0; i<max; i++) c[i]=a[i]+b[i];
  }

void v_sub (int max, double *a, double *b, double *c)
  {
  int i;

  for (i=0; i<max; i++) c[i]=a[i]-b[i];
  }

void v_scale (int max, double *a, double b, double *c)
  {
  int i;

  for (i=0; i<max; i++) c[i]=a[i]*b;
  }

void v_copy (int max, double *a, double *b)
  {
  int i;
  
  for (i=0; i<max; i++) b[i]=a[i];
  }

void v_zero (int max, double *a)
  {
  int i;
 
  for (i=0; i<max; i++) a[i]=0;
  }

void v_normalize (int max, double *a, double *b)
  {
  int i;
  double x;

  x=0;
  for (i=0; i<max; i++) x+=a[i]*a[i];
  if (x>=0) x=1/sqrt(x);
  for (i=0; i<max; i++) b[i]=a[i]*x;
  }

void v_project (int max, double *a, double *b, double *c)
  {
  int i;
  double x,y;

  x=y=0;
  for (i=0; i<max; i++) {x+=a[i]*b[i]; y+=b[i]*b[i];}
  if (y>=0) x/=y;
  else x=0;
  v_scale(max,b,x,c);
  } 

void v_cross (double *a, double *b, double *c)
  {
  c[0]=a[1]*b[2]-a[2]*b[1];
  c[1]=a[2]*b[0]-a[0]*b[2];
  c[2]=a[0]*b[1]-a[1]*b[0];
  }
