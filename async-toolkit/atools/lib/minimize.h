/*****************************************************
 * Copyright 2005 Fulcrum Microsystems               *
 * Copyright 1999 California Institute of Technology *
 * All rights reserved.  Written by Andrew Lines.    *
 *****************************************************/

/*** type declarations ***/

typedef double ENERGY(void *, double *);
typedef void DOWN(void *, double *, double *);

/*** Minimization function prototypes ***/

int minimize(FILE *record, void *parameters,
             ENERGY *energy, DOWN *down,
             int n, double *p, int iterations,
             double min_gradient, double tolerance);

int steepest_descent(FILE *record, void *parameters,
                     ENERGY *energy, DOWN *down,
                     int n, double *p, int iterations,
                     double min_gradient, double tolerance);
