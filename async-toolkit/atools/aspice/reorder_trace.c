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
#define REORDERMEM 980*1024*1024 /*** use 980MB to reorder ***/
               
int main (int argc, char *argv[])
  {
  int ok=0;
  char infilename[STRMAX],outfilename[STRMAX];
  if (argc==2)
    {
    safe_sprintf(infilename,"%s.trace",argv[1]);
    ok = reorder_trace_in_place(infilename,REORDERMEM);
    if (!ok) fprintf(stderr,"WARNING: unable to reorder trace file.\n");
    }
  else if (argc==3)
    {
    safe_sprintf(infilename,"%s.trace",argv[1]);
    safe_sprintf(outfilename,"%s.trace",argv[2]);
    ok = reorder_trace(infilename,outfilename,REORDERMEM);
    if (!ok) fprintf(stderr,"WARNING: unable to reorder trace file.\n");
    return 1;
    }
  else fprintf(stderr,"USAGE: reorder_trace infile [outfile]\n");
  return !ok;
  }
