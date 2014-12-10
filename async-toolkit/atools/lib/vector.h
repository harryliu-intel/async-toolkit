/*****************************************************
 * Copyright 2005 Fulcrum Microsystems               *
 * Copyright 1999 California Institute of Technology *
 * All rights reserved.  Written by Andrew Lines.    *
 *****************************************************/

/*** Vector function prototypes ***/

double v_dot (int max, double *a, double *b);
double v_mag (int max, double *a);
double v_magsqr (int max, double *a);
void v_add (int max, double *a, double *b, double *c);
void v_sub (int max, double *a, double *b, double *c);
void v_scale (int max, double *a, double b, double *c);
void v_copy (int max, double *a, double *b);
void v_zero (int max, double *a);
void v_normalize (int max, double *a, double *b);
void v_project (int max, double *a, double *b, double *c);
void v_cross (double *a, double *b, double *c);
