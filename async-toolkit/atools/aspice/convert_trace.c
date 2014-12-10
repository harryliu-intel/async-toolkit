/**
 * Convert *.fsdb or *.csdf into Aplot .names and *.trace file format.
 * Uses Synopsis fsdb2tbl for fsdb files.  Replaces older fsdb2aplot
 * and csdf2aplot utilities.  Uses only 1GB memory but is still very
 * slow.
 **/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include "tracelib.h"
#define MAXLINE 16384
#define REORDERMEM 980*1024*1024 /*** use 980MB to reorder ***/

int translate=0;
int linesize=0;
char *line=NULL;

/*** usage banner ***/
void usage() {
  fprintf(stderr,"USAGE: convert_trace\n");
  fprintf(stderr,"  [--fsdb infile | --csdf infile]\n");
  fprintf(stderr,"  [--translate]\n");
  fprintf(stderr,"  [--time-step s]\n");
  fprintf(stderr,"  [--sigfile signal-list-file]\n");
  fprintf(stderr,"  [--scale-time scale_factor]\n");
  fprintf(stderr,"  <outfilebase>\n");
  exit(1);
}

/*** get a line from a file with unpredictable line length ***/
int myfgets(FILE *fp) {
  int ch;
  int len=0;
  while(1) {
    ch = fgetc(fp);
    if (ch == EOF || ch == '\n')
      return len;
    if (len >= linesize-1) {
      linesize += MAXLINE;
      line = realloc(line,linesize);
    }
    line[len++] = ch;
    line[len] = 0;
  }
}

/*** translate from GDS2 name to CAST name and strip v(*) ***/
char *gds2cast(char *name) {
  if (translate) {
    int strip=0;
    char *s=name, *t=name;
    if (*s=='\'') { s++; strip++; }
    if (!strncasecmp(s,"v(",2)) { s+=2; strip++; }
    while(*s) {
      if      (!strncmp(s,"_D_",3))   { s+=3; *(t++) = '.'; }
      else if (!strncmp(s,"_l_",3))   { s+=3; *(t++) = '['; }
      else if (!strncmp(s,"_r_",3))   { s+=3; *(t++) = ']'; }
      else if (!strncmp(s,"_C_",3))   { s+=3; *(t++) = ','; }
      else if (!strncmp(s,"_U_",3))   { s+=3; *(t++) = '_'; }
      else if (!strncmp(s,"_3a_",4))  { s+=4; *(t++) = ':'; }
      else                            { *(t++) = *(s++); }
    }
    *(t-strip)=0;
  }
  return name;
}

/*** convert fsdb or csdf to names and trace ***/
int main(int argc, char *argv[]) {
  FILE *fp, *fn, *ft;
  char *infile=NULL, *basename=NULL, *sigfile=NULL;
  int   i, ok, nsig, fsdb=0, csdf=0, first_tok;
  float d, scale_time = 1, time_step=1e-12;
  char  filename[2048], cmd[MAXLINE], *tok, c;

  // parse command line arguments
  for(i=1; i<argc; i++) {
    if (! strncmp(argv[i],"--", 2)) {
      if (! strncmp(argv[i],"--translate",11))
        translate=1;
      else if (!strncmp(argv[i], "--sigfile",9) && i < argc)
        sigfile = argv[++i];
      else if (!strncmp(argv[i], "--fsdb",6) && i < argc)
        { fsdb=1; infile = argv[++i];}
      else if (!strncmp(argv[i], "--csdf",6) && i < argc)
        { csdf=1; infile = argv[++i];}
      else if (!strncmp(argv[i], "--scale-time",12) && i < argc)
        sscanf(argv[++i],"%f",&scale_time);
      else if (!strncmp(argv[i], "--time-step",11) && i < argc)
        sscanf(argv[++i],"%f",&time_step);
      else usage();
    }
    else if (!basename) basename=argv[i++];
    else usage();
  }
  if (!basename) usage();

  // open output names and trace files
  strcpy(filename, basename);
  strcat(filename, ".names");
  if (!((fn = fopen(filename, "w")))) {
    fprintf(stderr, "Cannot write names file %s\n", filename);
    usage();
  }
  strcpy(filename, basename);
  strcat(filename, ".trace");
  if (!((ft = fopen(filename, "w")))) {
    fprintf(stderr, "Cannot write trace file %s\n", filename);
    usage();
  }

  // setup
  line = malloc(MAXLINE);
  linesize = MAXLINE;

  if (fsdb) {
    // convert from fsdb
    char *xa = getenv("XA_SCRIPT");
    if (!xa) xa = "xa";
    int err = snprintf(cmd, sizeof(cmd),
                       "%s fsdb2tbl -hn -step %g -prec 9 -o /dev/stdout", xa, time_step/scale_time/1e-9);
    if (err < 0 || err >= sizeof(cmd)) {
      fprintf(stderr, "Command too long, truncated to %s\n", cmd);
      usage();
    }
    if (sigfile && strlen(sigfile)) {
      strcat(cmd, " -s '");
      strcat(cmd, sigfile);
      strcat(cmd, "'");
    }
    strcat(cmd, " -i '");
    strcat(cmd, infile);
    strcat(cmd, "'");
    if (!((fp = popen(cmd, "r")))) {
      fprintf(stderr, "Cannot do command\n%s\n", cmd);
      usage();
    }

    // write names file
    myfgets(fp);
    tok = strtok(line," ");
    if (!tok) {
      fprintf(stderr, "ERROR: fsdb2tbl has no output\n");
      return 1;
    }
    if (!strcmp(tok, "Time")) *tok = 't';
    fprintf(fn, "%s\n", gds2cast(tok));
    nsig=1;
    while((tok = strtok(NULL, " \n"))) {
      fprintf(fn, "%s\n", gds2cast(tok));
      nsig++;
    }
    fclose(fn);

    // write trace file
    i = ORDER_ORIGINAL;
    fwrite(&i, sizeof(int), 1, ft);
    i = time(NULL);
    fwrite(&i, sizeof(int), 1, ft);
    fwrite(&nsig, sizeof(int), 1, ft);
    while(myfgets(fp)) {
      tok = strtok(line, " ");
      sscanf(tok, "%e", &d);
      d *= scale_time;
      fwrite(&d,sizeof(float),1,ft);
      while((tok = strtok(NULL, " \n"))) {
        sscanf(tok, "%e", &d);
        fwrite(&d,sizeof(float),1,ft);
      }
    }
    pclose(fp);
  } else if (csdf) {
    // convert from csdf
    if (!(fp = fopen(infile, "r"))) {
      fprintf(stderr, "ERROR: cannot read csdf file %s\n",infile);
      return 1;
    }

    // write names file
    nsig=1;
    c=' ';
    while (myfgets(fp)) {
      first_tok=1;
      if      (!strncmp(line,"#H",2)) c='H';
      else if (!strncmp(line,"#N",2)) {
        c='N';
        tok = strtok(line, " ");
        fprintf(fn,"time\n");
        first_tok=0;
      }
      else if (!strncmp(line,"#C",2)) break;
      else if (!strncmp(line,"#;",2)) break;
      if (c=='N') {
        while ((tok = strtok(first_tok ? line : NULL," "))) {
          first_tok=0;
          fprintf(fn,"%s\n",gds2cast(tok));
          nsig++;
        }
      }
    }
    fclose(fn);

    // write trace file
    i = ORDER_ORIGINAL;
    fwrite (&i, sizeof (i), 1, ft);
    i = time(NULL);
    fwrite (&i, sizeof (i), 1, ft);
    fwrite (&nsig, sizeof (i), 1, ft);
    do {
      first_tok=1;
      if (!strncmp(line,"#;",2)) break; // end of file
      if (!strncmp(line,"#C",2)) {
        tok = strtok(line," "); // skip #C
        tok = strtok(NULL," "); // first value is time
        sscanf(tok, "%e", &d);
        d *= scale_time;
        fwrite(&d,sizeof(float),1,ft);
        tok = strtok(NULL," "); // this value is count of values, ignore
        first_tok=0;
      }
      while ((tok = strtok(first_tok ? line : NULL," "))) { // voltage values
        sscanf(tok, "%e", &d);
        fwrite(&d,sizeof(float),1,ft);
        first_tok=0;
      }
    } while (myfgets(fp));
    fclose(fp);
  } else usage();
  
  // free and close
  free(line);
  fclose(ft);

  // reorder trace file in place
  ok = reorder_trace_in_place(filename,REORDERMEM);
  if (!ok) fprintf(stderr,"WARNING: unable to reorder trace file.\n");
  return 0;
}
