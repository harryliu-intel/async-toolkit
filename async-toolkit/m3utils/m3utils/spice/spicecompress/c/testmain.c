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

  ztrace_debug_header(&header);
  
  return 0;
}
