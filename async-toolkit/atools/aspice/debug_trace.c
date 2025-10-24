/*****************************************************
 * Copyright 2005 Fulcrum Microsystems               *
 * Copyright 1999 California Institute of Technology *
 * All rights reserved.  Written by Andrew Lines.    *
 *****************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <assert.h>
#include <string.h>
#include "tracelib.h"
#include "misc.h"
#include "leak.h"

int main (int argc, char *argv[])
  {
  float *M;
  char filename[STRMAX];
  FILE *file;
  int timestamp,Nnodes,Nsteps,order,i,j,header=0;

  /*** check usage ***/
  if (argc<2) {fprintf(stderr,"USAGE: debug_trace tracefile [-header]\n"); return 1;}
  if ((argc==3)&&(strcmp(argv[2],"-header")==0)) header=1;

  /*** read trace file Nnodes, order ***/
  safe_sprintf(filename,"%s.trace",argv[1]);
  file=fopen(filename,"rb");
  if (file==NULL) {fprintf(stderr,"ERROR: can't open %s.\n",filename); exit(1);}
  if (!get_header(file,&timestamp,&Nnodes,&Nsteps,&order))
    {fprintf(stderr,"ERROR: bad header.\n"); exit(1);}

  /*** debugging info ***/
  printf("order=%d, timestamp=%d, Nnodes=%d, Nsteps=%d\n",order,timestamp,Nnodes,Nsteps);
  if (header) return 0;

  /*** get data ***/
  M=(float *) leak_malloc(sizeof(float)*Nnodes*Nsteps);
  get_all_values(file,Nnodes,Nsteps,order,M);

  /*** print data ***/
  for (i=0; i<Nnodes; i++)
    {
    printf("/******************** node %d ********************/\n",i);
    for (j=0; j<Nsteps; j++)
      {
      printf("%8g  ",M[Nsteps*i+j]);
      if (j%5==4) printf("\n");
      }
    printf("\n");
    }
  leak_free(M);
  leak_check();
  return 0;
  }
