/*****************************************************
 * Copyright 2005 Fulcrum Microsystems               *
 * Copyright 1999 California Institute of Technology *
 * All rights reserved.  Written by Andrew Lines.    *
 *****************************************************/

/*** model constants ***/

#define NMOS  1
#define PMOS -1

#define NTYPE  1
#define PTYPE -1

/*** physical constants ***/

#define CONSTCtoK  273.15

/**** deck of models for specified file, lib, temperature ***/
typedef struct DECK
  {
  double temperature;
  char *filename;
  char *lib;
  char *dev;
  LIST *parameters; /* list of ASSIGNMENT's */
  LIST *models;
  } DECK;

/*** generic transistor model ***/
typedef struct MODEL
  {
  int bsim,type;
  double Wmin,Wmax,Lmin,Lmax,temp;
  DECK *deck;
  } MODEL;

/*** specific transistor models and functions ***/

#include "bsim4/bsim4.h"

/*** external function prototypes ***/

void MODEL_SetCorner(char *corner);
void MODEL_SetModel(int bsim, LIST *assignments, char *filename, char *lib, char *dev,
                    double Temp);
double MODEL_ParseExpression(LEX *lex, LIST *assignments);
MODEL *MODEL_FindModel(double Type, double W, double L);
void MODEL_Finish();
void MODEL_FreeMemory();
