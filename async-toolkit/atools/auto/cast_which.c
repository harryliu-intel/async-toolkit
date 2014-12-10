#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "newlex.h"
#include "path.h"

void barf()
  {
  fprintf(stderr,"USAGE: cast_which castfile\n");
  fprintf(stderr,"  uses ~/.castrc or env variables to set search path\n");
  fprintf(stderr,"  .cast extensions use $CAST_PATH\n");
  fprintf(stderr,"  .auto/.in extensions use $AUTO_PATH\n");
  fprintf(stderr,"  .mag/.ext extensions use $EXT_PATH\n");
  fprintf(stderr,"  .asp/.bsim3 extensions use $ASPICE_PATH\n");
  fprintf(stderr,"  .dsim extensions use $DSIM_PATH\n");
  exit(1);
  }

/*** uses ~/.castrc or env variables to find a CAST related file ***/
int main(int argc, char *argv[])
  {
  char *file,*extension;

  /*** read ~/.castrc ***/
  setenv_from_file(find_filename(".castrc","","~"));

  /*** banner ***/
  if (argc!=2) barf();

  /*** find extension ***/
  extension=argv[1];
  extension = strrchr(extension, '.');
  if (extension==0) barf();
  *extension=0;
  extension++;

  /*** cases ***/
  if      (strcmp(extension,"cast")==0)  file=find_filename(argv[1],".cast","$CAST_PATH");
  else if (strcmp(extension,"mag")==0)   file=find_filename(argv[1],".mag","$EXT_PATH");
  else if (strcmp(extension,"ext")==0)   file=find_filename(argv[1],".mag","$EXT_PATH");
  else if (strcmp(extension,"auto")==0)  file=find_filename(argv[1],".auto","$AUTO_PATH");
  else if (strcmp(extension,"in")==0)    file=find_filename(argv[1],".in","$AUTO_PATH");
  else if (strcmp(extension,"asp")==0)   file=find_filename(argv[1],".asp","$ASPICE_PATH");
  else if (strcmp(extension,"bsim3")==0) file=find_filename(argv[1],".bsim3","$ASPICE_PATH");
  else if (strcmp(extension,"dsim")==0)  file=find_filename(argv[1],".dsim","$DSIM_PATH");
  else barf();
  if (file==NULL) return 1;
  printf("%s\n",file);
  return 0;
  }
