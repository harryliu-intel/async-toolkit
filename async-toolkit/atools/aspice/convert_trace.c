/* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. */
/* SPDX-License-Identifier: Apache-2.0 */

/**
 * Convert *.fsdb into Aplot *.names and *.trace file format.  Uses
 * Synopsis fsdb2ns for fsdb files.
 **/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <math.h>
#include "tracelib.h"
#define MAXLINE 16384

int translate=0;
int linesize=0;
char *line=NULL;

/*** usage banner ***/
void usage() {
  fprintf(stderr,"USAGE: convert_trace\n");
  fprintf(stderr,"  [--fsdb infile]\n");
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

/** linear interpolate PWL **/
void write_values(FILE *ft, int timesteps, float time_step, int n, float *times, float *values) {
  int m=0,time;
  for (time=0; time<timesteps; time++) {
    while (m+1<n && !(times[m]<=time*time_step && time*time_step<=times[m+1])) m++; // bracket
    int   m1=m+1<n ? m+1 : m;
    float t0=times[m];
    float t1=times[m1];
    float v0=values[m];
    float v1=values[m1];
    float t=time*time_step;
    float d=v0 + (v1-v0) * (t-t0)/(t1-t0);
    fwrite(&d,sizeof(float),1,ft);
  }
}

/*** convert fsdb to names and trace ***/
int main(int argc, char *argv[]) {
  FILE *fp, *fn, *ft;
  char *infile=NULL, *basename=NULL, *sigfile=NULL;
  int   i, nsig=1, fsdb=0;
  float scale_time = 1, time_step=1e-12, max_time=0;
  char  filename[2048], cmd[MAXLINE], *tok;

  // parse command line arguments
  for(i=1; i<argc; i++) {
    if (! strncmp(argv[i],"--", 2)) {
      if (! strncmp(argv[i],"--translate",11))
        translate=1;
      else if (i+1<argc && !strncmp(argv[i], "--sigfile",9))
        sigfile = argv[++i];
      else if (i+1<argc && !strncmp(argv[i], "--fsdb",6))
        { fsdb=1; infile = argv[++i];}
      else if (i+1<argc && !strncmp(argv[i], "--scale-time",12))
        sscanf(argv[++i],"%f",&scale_time);
      else if (i+1<argc && !strncmp(argv[i], "--time-step",11))
        sscanf(argv[++i],"%f",&time_step);
      else usage();
    }
    else if (!basename) basename=argv[i++];
    else usage();
  }
  if (!basename || !fsdb) usage();

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

  // convert from fsdb
  char *fulcrum = getenv("FULCRUM");
  if (!fulcrum) fulcrum = "fulcrum";
  int err = snprintf(cmd, sizeof(cmd),
                     "%s fsdb2ns -fmt pwl -step ps ", fulcrum);
  if (err < 0 || err >= sizeof(cmd)) {
    fprintf(stderr, "Command too long, truncated to %s\n", cmd);
    usage();
  }
  if (sigfile && strlen(sigfile)) {
    fprintf(stderr,"-sigfile not supported");
  }
  strcat(cmd, "'");
  strcat(cmd, infile);
  strcat(cmd, "'");
  strcat(cmd," 2> /dev/null");
  if (!((fp = popen(cmd, "r")))) {
    fprintf(stderr, "Cannot do command\n%s\n", cmd);
    usage();
  }

  // first pass to count signals and max time
  int n=0,max_points=0;
  while (myfgets(fp)) {
    if (line[0]=='V' || line[0]=='I') {
      nsig++;
      n=0;
    } else if (line[0]=='+') {
      float time,val;
      sscanf(line,"+ %gps %g",&time,&val);
      time*=1e-12;
      n++;
      if(time>max_time) max_time=time;
      if(n>max_points) max_points=n;
    } else if (strcmp(line,"td=0ps")) {
      fprintf(stderr,"bad line=%s\n",line);
    }
  }
  pclose(fp);

  // write trace header
  i = ORDER_REORDERED;
  fwrite(&i, sizeof(int), 1, ft);
  i = time(NULL);
  fwrite(&i, sizeof(int), 1, ft);
  fwrite(&nsig, sizeof(int), 1, ft);

  // write time
  int timesteps=ceil(max_time/time_step);
  fprintf(fn,"time\n");
  for (i=0; i<timesteps; i++) {
    float time=time_step*i;
    fwrite(&time,sizeof(float),1,ft);
  }

  // second pass to output names and trace
  fp = popen(cmd, "r");
  float *times=malloc(max_points*sizeof(float));
  float *values=malloc(max_points*sizeof(float));
  n=0;
  while (myfgets(fp)) {
    if (line[0]=='V') {
      if (n>0) write_values(ft,timesteps,time_step,n,times,values);
      tok=strtok(line," ");
      tok=strtok(NULL," ");
      fprintf(fn,"%s\n",gds2cast(tok));
      nsig++;
      n=0;
    } else if (line[0]=='I') {
      if (n>0) write_values(ft,timesteps,time_step,n,times,values);
      tok=strtok(line," ");
      tok=strtok(NULL," ");
      fprintf(fn,"i(%s)\n",gds2cast(tok));
      nsig++;
      n=0;
    } else if (line[0]=='+') {
      float time,val;
      sscanf(line,"+ %gps %g",&time,&val);
      time*=1e-12;
      times[n]=time;
      values[n]=val;
      n++;
    }
  }
  if (n>0) write_values(ft,timesteps,time_step,n,times,values);

  // free and close
  free(times);
  free(values);
  free(line);
  fclose(ft);
  fclose(fn);
  pclose(fp);
  return 0;
}
