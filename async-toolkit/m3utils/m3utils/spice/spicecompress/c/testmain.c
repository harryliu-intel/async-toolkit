#include <stdio.h>
#include "ztrace.h"

int
main(int argc, char **argv)
{
  FILE *fp;
  ztrace_header_t header;
  
  fp = fopen("run_i3_1ps.ztrace", "rb");

  if (!ztrace_get_header(fp, &header)) {
    perror("ztrace_get_header");
    exit(1);
  }

  /*ztrace_debug_header(&header);*/

  {
    float *time, *data;
    time  = malloc(header.nsteps * sizeof(float));
    data  = malloc(header.nsteps * sizeof(float));

    ztrace_get_node_values(fp, &header, 0, time);
    ztrace_get_node_values(fp, &header, 1, data);
    
  }
  
  return 0;
}
