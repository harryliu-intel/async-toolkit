/* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. */
/* SPDX-License-Identifier: Apache-2.0 */

#include <assert.h>
#include <stdio.h>
#include "ztrace.h"
#include "etaoin.h"
#include "mac_freqs.h"

int
test_trace(int argc, char **argv)
{
  FILE *fp;
  ztrace_header_t header;
  int nodeid;
  int i;

  assert (argc >= 2);
  
  fp = fopen("run_i3_1ps.ztrace", "rb");

  if (!ztrace_get_header(fp, &header)) {
    perror("ztrace_get_header");
    exit(1);
  }

  /*ztrace_debug_header(&header);*/

  for (i = 1; i < argc; ++i)
  {
    #define FNBUFLEN 100
    FILE *ofp;
    char fnbuff[FNBUFLEN];
    float *time, *data;

    nodeid = atoi(argv[i]);

    time  = malloc(header.nsteps * sizeof(float));
    data  = malloc(header.nsteps * sizeof(float));

    snprintf(fnbuff, FNBUFLEN, "%d.dat", nodeid);

    ofp = fopen(fnbuff, "w");

    ztrace_get_node_values(fp, &header, 0, time);
    ztrace_get_node_values(fp, &header, nodeid, data);
    
    for (int i = 0; i < header.nsteps; ++i)
      fprintf(ofp, "%10g %10g\n", time[i], data[i]);

    fclose(ofp);
  }
  
  return 0;
}

void
test_arith(const char *path, const FreqTable_t freqs)
{
  FILE             *fp         = fopen(path, "rb");
  ArithDecoder_t   *decoder    = ArithDecoder_New(freqs);
  int               c          = EOF;

  assert(fp);
  
  while ((c = getc(fp)) != EOF) {
    ArithDecoder_NewChar(decoder, c);
  }
  
  ArithDecoder_NewEof(decoder);
  {
    size_t  n = 10;
    char *buf = malloc(n);

    while (!ArithDecoder_Reset(decoder, buf, &n)) {
      n *= 2;
      buf = realloc(buf, n);
    }

    buf[n] = '\000';

    printf("Decoded %zu:\n====>%s<====\n", n, buf);
  
    ArithDecoder_Free(decoder);
    free(buf);
  }
  fclose(fp);
}

int
main(int argc, char **argv)
{

  test_trace (argc, argv);

  if(0) test_arith("test.out", etaoin_freqs);
}
