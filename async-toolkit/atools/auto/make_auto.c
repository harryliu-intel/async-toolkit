#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "newlex.h"
#include "path.h"
#include "leak.h"

void usage_exit()
  {
  fprintf(stderr,"USAGE: make_auto [-cast castfile] [-cell cellname]\n");
  fprintf(stderr,"       Makes .auto targets as specified in ./directives\n");
  fprintf(stderr,"       -cast restricts listing to cells defined in 'castfile'.\n");
  fprintf(stderr,"       -cell restricts listing to cell types or subtypes named 'cellname'.\n");
  exit(-1);
  }


/*** reads a "directives" file in the current directory and makes .auto files ***/
int main(int argc, char **argv)
  {
  FILE *file;
  LEX *lex;
  char *pc,*castfile=NULL,*castpath=NULL,*typename,*subtypename;
  char *do_cell=NULL,*do_cast=NULL;
  int i,do_line;

  /*** read command line ***/
  i=0;
  while (++i!=argc) 
    {
    if (argv[i][0]=='-') 
      {
      if (strcmp(argv[i],"-cast") == 0) 
        {
        if (++i==argc) usage_exit();
        do_cast=argv[i];
        }
      else if (strcmp(argv[i],"-cell") == 0)
        {
        if (++i==argc) usage_exit();
        do_cell=argv[i];
        }
      else if (strcmp(argv[i],"-help") == 0 ||
               strcmp(argv[i],"--help") == 0 ||
               strcmp(argv[i],"-h") == 0)
        usage_exit();
      else 
        {
        fprintf(stderr,"ERROR: Unrecognized option '%s'\n",argv[i]);
        usage_exit();
        }
      }
    else usage_exit();
    }

  /*** read ~/.castrc ***/
  setenv_from_file(find_filename(".castrc","","~"));

  /*** open the directives file ***/ 
  file=fopen("directives","rt");
  lex=lex_file(file);
  lex_define_whitespace(lex," \t");
  if ((file==NULL)||(lex==NULL)) usage_exit();

  /*** read through directives outputting calls of make_auto_csh ***/
  while (!lex_is_eof(lex))
    {
    if (lex_eatif_sym(lex,"\n")); /* skip blank lines */
    else if (lex_eatif_sym(lex,"#")) /* change castfile */
      {
      lex_do_whitespace(lex);
      castfile=leak_strdup(lex_eat_until(lex," \t\n"));
      castpath=find_filename(castfile,".cast","$CAST_PATH");
      if (castpath==NULL) {fprintf(stderr,"ERROR: can't find %s.cast\n",castfile); return 1;}
      lex_eat_sym(lex,"\n");
      }
    else
      {
      /*** parse the type and subtype ***/
      if (castpath==NULL) {fprintf(stderr,"ERROR: no castpath specified\n"); return 1;}
      typename=leak_strdup(lex_eat_until(lex," :\n\t"));
      if (lex_eatif_sym(lex,":")) subtypename=leak_strdup(lex_eat_until(lex," \n\t"));
      else                        subtypename=leak_strdup(typename);

      /*** filter ***/
      do_line=(do_cast==NULL || strcmp(do_cast,castfile)==0) &&
	      (do_cell==NULL || strcmp(do_cell,typename)==0 || strcmp(do_cell,subtypename)==0);

      /*** print the line and directives ***/
      if (do_line) printf("make_auto_csh %s \"%s\" \"%s\" ",castpath,typename,subtypename); 
      while (!lex_is_sym(lex,"\n")&&!lex_is_eof(lex))
	{
	while (lex_eatif_sym(lex,"\\")) lex_eat_sym(lex,"\n"); /* continue on next line */
        pc=lex_eat_until(lex," \t\n");
        if (do_line) printf("\"@%s\" ",pc);
        }
      if (do_line) printf("\n");
      lex_eat_sym(lex,"\n");
      }
    }

  /*** fuck cleanup ***/
  return 0;
  }
