/*****************************************************
 * Copyright 2005 Fulcrum Microsystems               *
 * Copyright 1999 California Institute of Technology *
 * All rights reserved.  Written by Andrew Lines.    *
 *****************************************************/

#define ORDER_ORIGINAL 0
#define ORDER_REORDERED 1
#define ORDER_CHANGING 2
#define HEADERSIZE 3*sizeof(int)

#define FILE_BUF_SIZE 1024*1024 // default system BUFSIZ is 8192
#define bigbuffer(stream) setvbuf(stream,NULL,_IOFBF,FILE_BUF_SIZE);

float swap_endian_float(float x);
char read_char(FILE *stream);
void write_char(FILE *stream, char x);
int read_int(FILE *stream);
void write_int(FILE *stream, int x);
float read_float(FILE *stream);
void write_float(FILE *stream, float x);
int get_header(FILE *tracefile, int *timestamp, int *Nnodes, int *Nsteps, int *order);
void get_node_values(FILE *tracefile, long node, long Nnodes, long Nsteps, int order, float *x);
void get_all_values(FILE *tracefile, long Nnodes, long Nsteps, int order, float *M);
void get_all_values_transposed(FILE *tracefile, int Nnodes, int Nsteps, int order, float *M);
int reorder_trace(char *infile, char *outfile, int mem);
int reorder_trace_in_place(char *basename, int mem);
