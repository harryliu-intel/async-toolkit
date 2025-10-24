/*****************************************************
 * Copyright 2005 Fulcrum Microsystems               *
 * Copyright 1999 California Institute of Technology *
 * All rights reserved.  Written by Andrew Lines.    *
 *****************************************************/

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <pwd.h>
#include "leak.h"
#include "newlex.h"
#include "misc.h"

/*** looks for a filename and extension in specified search path ***/
/*** path is : separated and supports ~ and $ ***/
/*** returns expanded filename, or NULL if none exists ***/
char *find_filename(char *filename, char *extension, char *path)
  {
  int progress;
  LEX *lex;
  FILE *file;
  struct passwd *pw;
  char old[STRMAX],new[STRMAX],fullname[STRMAX],*pc;

  /*** try without path prefix ***/
  safe_sprintf(fullname,"%s%s",filename,extension);
  file=fopen(fullname,"rt");
  if (file!=NULL)
    {
    fclose(file);
    return leak_strdup(fullname);
    }

  /*** expand $, ~, and ~name repeatedly ***/
  strcpy(old,path);
  do
    {
    progress=0;
    new[0]=0;
    lex=lex_string(old);
    while(!lex_is_eof(lex))
      {
      if      (lex_eatif_sym(lex,":")) pc=":";
      else if (lex_eatif_sym(lex,"~"))
        {
        if (lex_is_id(lex))
	  {
          pw=getpwnam(lex_eat_id(lex));
          if (pw==NULL) {lex_eat_until(lex,":"); pc="";}
          else pc=pw->pw_dir;
	  }
        else
	  {
          pc=getenv("HOME");
          }
        progress=1;
	}
      else if (lex_eatif_sym(lex,"$"))
        {
        pc=lex_eat_id(lex);
        pc=getenv(pc);
        progress=1;
        }
      else if (lex_eatif_sym(lex,".")) pc=".";
      else pc=lex_eat_until(lex,".:~$ ");
      if (pc!=NULL) strcat(new,pc);
      }
    lex_free(lex);
    strcpy(old,new);
    } while(progress);

  /*** now try every : separated prefix ***/
  lex=lex_string(old);
  while(!lex_is_eof(lex))
    {
    pc=lex_eat_until(lex,":");
    safe_sprintf(fullname,"%s/%s%s",pc,filename,extension);
    file=fopen(fullname,"rt");
    if (file!=NULL)
      {
      lex_free(lex);
      fclose(file);
      return leak_strdup(fullname);
      }
    lex_eatif_sym(lex,":");
    }
  lex_free(lex);

  return NULL;  
  }

/*** does SETENV for each key = "value" line in a file ***/
int setenv_from_file(char *filename)
  {
  char str[STRMAX];
  FILE *file;
  LEX *lex;
  char *key,*value;
  file=fopen(filename,"rt");
  if (file==NULL) return 0;
  lex=lex_file_with_name(file,filename);
  while (!lex_is_eof(lex))
    {
    key=leak_strdup(lex_eat_id(lex));
    lex_eat_sym(lex,"=");
    value=lex_eat_quote(lex);
    if (getenv(key)==NULL)
      {
      safe_sprintf(str,"%s=%s",key,value);
      putenv(strdup(str));
      }
    leak_free(key);
    }
  lex_free(lex);
  fclose(file);
  return 1;
  }
