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
#include <unistd.h>
#include "tracelib.h"
#include "leak.h"
#include "misc.h"

const int ONE=1;
const char *ENDIAN=(char *) &ONE;

/*** fwrite which warns of failure and retries ***/
void safe_fwrite(void *ptr, size_t size, size_t nmemb, FILE *stream)
  {
  long oldpos;
  oldpos=ftell(stream);
  while (fwrite(ptr,size,nmemb,stream)!=nmemb)
    {
    fprintf(stderr,"WARNING: fwrite failed, trying again in a minute.\n");
    sleep(60);
    fseek(stream,oldpos,SEEK_SET);
    }
  }

/*** byte order independent file access ***/

int swap_endian_int(int x)
  {
  int y=x;
  if (ENDIAN[0]==0)
    {
    char *bx=(char *)(&x),*by=(char *)(&y);
    by[0]=bx[3];
    by[1]=bx[2];
    by[2]=bx[1];
    by[3]=bx[0];
    }
  assert(sizeof(int)==4);
  return y;
  }

float swap_endian_float(float x)
  {
  float y=x;
  if (ENDIAN[0]==0)
    {
    char *bx=(char *)(&x),*by=(char *)(&y);
    by[0]=bx[3];
    by[1]=bx[2];
    by[2]=bx[1];
    by[3]=bx[0];
    }
  assert(sizeof(float)==4);
  return y;
  }

char read_char(FILE *stream)
  {
  char x;
  fread(&x,sizeof(char),1,stream);
  return x;
  }

void write_char(FILE *stream, char x)
  {
  safe_fwrite(&x,sizeof(char),1,stream);
  }   

int read_int(FILE *stream)
  {
  int x;
  fread(&x,sizeof(int),1,stream);
  return swap_endian_int(x);
  }

void write_int(FILE *stream, int x)
  {
  x=swap_endian_int(x);
  safe_fwrite(&x,sizeof(int),1,stream);
  }

float read_float(FILE *stream)
  {
  float x;
  fread(&x,sizeof(float),1,stream);
  return swap_endian_float(x);
  }

void write_float(FILE *stream, float x)
  {
  x=swap_endian_float(x);
  safe_fwrite(&x,sizeof(float),1,stream);
  } 

/*** get header and length of trace file ***/
/*** should call this before using other trace functions ***/
/*** returns 1 if successful ***/
int get_header(FILE *tracefile, int *timestamp, int *Nnodes, int *Nsteps, int *order)
  {
  /*** read Nnodes, order from trace file header ***/
  fseek(tracefile,0,SEEK_SET);
  *order=read_int(tracefile);
  *timestamp=read_int(tracefile);
  *Nnodes=read_int(tracefile);
  if ((*Nnodes<=0)||((*order!=ORDER_ORIGINAL)&&(*order!=ORDER_REORDERED))) return 0;

  /*** compute Nsteps from tracefile size ***/
  fseek(tracefile,0,SEEK_END);
  *Nsteps=(ftell(tracefile)-HEADERSIZE)/(*Nnodes*sizeof(float));
  return 1;
  }

/*** returns 1D array of node values over timesteps ***/
/*** if (order==ORDER_REORDERED) trace is by node then time ***/
/*** else                        trace is by time then node ***/
void get_node_values(FILE *tracefile, long node, long Nnodes, long Nsteps, int order, float *v)
  {
  int i;
  long pos,dpos;

  /*** find initial position and increment per timestep ***/
  if (order==ORDER_REORDERED) {pos = HEADERSIZE + Nsteps*node*sizeof(float); dpos=sizeof(float);}
  else                        {pos = HEADERSIZE + node*sizeof(float); dpos=Nnodes*sizeof(float);}

  /*** seek through tracefile and fill in array of values ***/
  for(i=0; i<Nsteps; i++, pos+=dpos)
    {
    fseek(tracefile,pos,SEEK_SET);
    v[i]=read_float(tracefile);
    }
  }

/*** returns a 2D array of M=value[Nnodes,Nsteps] ***/
void get_all_values(FILE *tracefile, long Nnodes, long Nsteps, int order, float *M)
  {
  long i,j;
  fseek(tracefile,HEADERSIZE,SEEK_SET);
  if (order==ORDER_REORDERED)
    {
    for (i=0; i<Nnodes; i++)
      for (j=0; j<Nsteps; j++)
	M[Nsteps*i+j]=read_float(tracefile);
    }
  else
    {
    for (j=0; j<Nsteps; j++)
      for (i=0; i<Nnodes; i++)
	M[Nsteps*i+j]=read_float(tracefile);
    }
  }

/*** returns a 2D array of M=value[Nsteps,Nnodes] ***/
void get_all_values_transposed(FILE *tracefile, int Nnodes, int Nsteps, int order, float *M)
  {
  long i,j;
  fseek(tracefile,HEADERSIZE,SEEK_SET);
  if (order==ORDER_REORDERED)
    {
    for (i=0; i<Nnodes; i++)
      for (j=0; j<Nsteps; j++)
	M[Nnodes*j+i]=read_float(tracefile);
    }
  else
    {
    for (j=0; j<Nsteps; j++)
      for (i=0; i<Nnodes; i++)
	M[Nnodes*j+i]=read_float(tracefile);
    }
  }

/*** transpose matrix, with reads fragmented for each step ***/
void transpose_with_fragmented_steps(FILE *fin, FILE *fout,
                                     int Nnodes, int Nsteps, int mem)
  {
  long from,to,step,node,Nmax,pos;
  float *M = NULL;

  /*** allocate scratch memory ***/
  while(1) {
    Nmax=mem/(Nsteps*sizeof(float)); /* how many nodes to transpose at once */
    if (Nmax<1) Nmax=1;
    M=(float *) malloc(Nmax*Nsteps*sizeof(float));
    if (M!=NULL) break;
    if (mem<1024*1024)
      {fprintf(stderr,"ERROR: can't allocate 1MB reorder buffer.\n"); exit(1);}
    mem=mem/2;
    fprintf(stderr,"WARNING: reducing reorder buffer to %dMB\n.",mem/1024/1024);
  }

  /*** transpose blocks of nodes at once ***/
  for (from=0; from<Nnodes; from=to)
    {
    /*** current range of nodes ***/
    to=from+Nmax;
    if (to>Nnodes) to=Nnodes;

    /*** read non-consecutive blocks of nodes ***/
    for (step=0; step<Nsteps; step++)
      {
      pos=HEADERSIZE+(step*Nnodes+from)*sizeof(float);
      fseek(fin,pos,SEEK_SET);
      for (node=from; node<to; node++) M[node-from + Nmax*step]=read_float(fin);
      }

    /*** write consecutive blocks of steps ***/
    for (node=from; node<to; node++)
      {
      for (step=0; step<Nsteps; step++) write_float(fout,M[node-from + Nmax*step]);
      }
    }

  /*** free scratch memory ***/
  free(M);
  }

/*** transpose matrix, with writes fragmented for each node ***/
void transpose_with_fragmented_nodes(FILE *fin, FILE *fout,
                                     int Nnodes, int Nsteps, int mem)
  {
  long from,to,step,node,Nmax,pos;
  float *M = NULL;

  /*** allocate scratch memory ***/
  while(1) {
    Nmax=mem/(Nnodes*sizeof(float)); /* how many steps to transpose at once */
    if (Nmax<1) Nmax=1;
    M=(float *) malloc(Nmax*Nnodes*sizeof(float));
    if (M!=NULL) break;
    if (mem<1024*1024)
      {fprintf(stderr,"ERROR: can't allocate 1MB reorder buffer.\n"); exit(1);}
    mem=mem/2;
    fprintf(stderr,"WARNING: reducing reorder buffer to %dMB.\n",mem/1024/1024);
  }

  /*** transpose blocks of steps at once ***/
  for (from=0; from<Nsteps; from=to)
    {
    /*** current range of nodes ***/
    to=from+Nmax;
    if (to>Nsteps) to=Nsteps;

    /*** read consecutive blocks of steps ***/
    for (step=from; step<to; step++)
      {
      for (node=0; node<Nnodes; node++) M[step-from + Nmax*node]=read_float(fin);
      }

    /*** write non-consecutive blocks of nodes ***/
    for (node=0; node<Nnodes; node++)
      {
      pos=HEADERSIZE+(node*Nsteps+from)*sizeof(float);
      fseek(fout,pos,SEEK_SET);
      for (step=from; step<to; step++) write_float(fout,M[step-from + Nmax*node]);
      }
    }

  /*** free scratch memory ***/
  free(M);
  }

/*** reorders tracefile, returns 1 if successful ***/
int reorder_trace(char *intracefilename, char *outtracefilename, int mem)
  {
  FILE *fin,*fout;
  int timestamp,Nnodes,Nsteps,order;

  /*** read old header ***/
  fin=fopen(intracefilename,"rb");
  if (fin==NULL) return 1; // ignore empty tracefiles
  bigbuffer(fin);
  if (!get_header(fin,&timestamp,&Nnodes,&Nsteps,&order)) return 0;
  if ((Nsteps==0)||(Nnodes==0)||(order!=ORDER_ORIGINAL)) return 0;

  /*** write new header ***/
  fout=fopen(outtracefilename,"wb");
  if (fout==NULL) return 0;
  bigbuffer(fout);
  write_int(fout,ORDER_CHANGING);
  write_int(fout,timestamp);
  write_int(fout,Nnodes);
  fflush(fout);

  /*** transpose data ***/
  fseek(fin,HEADERSIZE,SEEK_SET);
  fseek(fout,HEADERSIZE,SEEK_SET);
  if (Nsteps<Nnodes) transpose_with_fragmented_steps(fin,fout,Nnodes,Nsteps,mem);
  else               transpose_with_fragmented_nodes(fin,fout,Nnodes,Nsteps,mem);

  /*** finish fout header ***/
  fseek(fout,0,SEEK_SET);
  write_int(fout,ORDER_REORDERED);

  /*** close files ***/
  fclose(fin);
  fclose(fout);
  return 1;
  }

/*** reorder a trace file then replace the original safely ***/
int reorder_trace_in_place(char *filename1, int mem)
  {
  char filename2[STRMAX];
  safe_sprintf(filename2,"%s.new",filename1);
  if (reorder_trace(filename1,filename2,mem))
    {
    unlink(filename1);
    if (link(filename2,filename1)==0) unlink(filename2);
    return 1;
    }
  return 0;
  }
