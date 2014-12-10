/*****************************************************
 * Copyright 2005 Fulcrum Microsystems               *
 * Copyright 1999 California Institute of Technology *
 * All rights reserved.  Written by Andrew Lines.    *
 *****************************************************/

#include <stdlib.h>
#include <math.h>
#include <stdio.h>
#include <string.h>
#include <sys/stat.h>
#include <sys/time.h>
#include <sys/types.h>
#include <sys/resource.h>
#include "misc.h"

void error(char *msg)
  {
  fprintf(stderr,"ERROR: %s\n",msg);  
  exit(1);
  }
  
void warning(char *msg)
  {
  fprintf(stderr,"WARNING: %s\n",msg);  
  }
  
/*** check for memory allocation failures ***/
void *check_alloc(void *test, char *file, unsigned line)
  {
  if (test==NULL)
    {
    fprintf(stderr,"ERROR: out of memory in file=%s line=%u\n",file,line);
    exit(1);
    }
  return test;
  }

/*** get the user time in seconds ***/
double user_time()
  {
  struct rusage time;
  getrusage(0,&time);
  return ((double) time.ru_utime.tv_usec)*1e-6 + (double) time.ru_utime.tv_sec +
         ((double) time.ru_stime.tv_usec)*1e-6 + (double) time.ru_stime.tv_sec;
  }

/*** check for string overruns in sprintf ***/
void check_sprintf(char *file, unsigned line, int max, int len)
  {
  if (len>=max)
    {
    fprintf(stderr,"ERROR: string length %d is longer than %d in file=%s line=%u\n",
            len,max,file,line);
    exit(1);
    }
  }

/*** open a file, creating directories if necessary ***/
FILE *mkdir_fopen(const char *path, const char *mode)
  {
  int i;
  char name[STRMAX];
  safe_sprintf(name,"%s",path);
  for (i=0; i<strlen(name); i++)
    {
    if (name[i]=='/')
      {
      name[i]=0;
      mkdir(name,S_IRWXU|S_IRWXG|S_IRWXO);
      name[i]='/';
      }
    }
  return fopen(path,mode); 
  }
