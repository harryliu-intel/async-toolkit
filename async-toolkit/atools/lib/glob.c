/*****************************************************
 * Copyright 2005 Fulcrum Microsystems               *
 * Copyright 1999 California Institute of Technology *
 * All rights reserved.  Written by Andrew Lines.    *
 *****************************************************/

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "misc.h"

/*** does this appears to be a legal glob? ***/
int is_glob(char *glob)
  {
  int stars=0,questionmarks=0,len,j;
  len=strlen(glob);
  for (j=0; j<strlen(glob); j++)
    {
    stars += (glob[j]=='*');
    questionmarks += (glob[j]=='?');
    }
  return (stars==1) || ((stars==0) && (questionmarks>0));
  }

/*** check if a glob matches a name (supports only a single *) ***/
int glob_matches(char *glob, char *name)
  {
  int j,k,globN,nameN;
  
  /*** get strlens ***/
  globN=strlen(glob)+1;
  nameN=strlen(name)+1;

  /*** compare forwards from the start ***/
  for (j=0, k=0; (j<globN)&&(k<nameN); j++,k++)
    if (glob[j]=='*') break;
    else if ((glob[j]!='?') && (glob[j]!=name[k])) return 0;

  /*** compare backwards from the end ***/
  for (j=globN-1, k=nameN-1; (j>=0)&&(k>=0); j--, k--)
    if (glob[j]=='*') break;
    else if ((glob[j]!='?') && (glob[j]!=name[k])) return 0;
  
  return 1;
  }
