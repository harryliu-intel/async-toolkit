
/*
 *  Spice trace waveform decompression
 *
 * 
 *  Author: mika.nystroem@intel.com
 *  January 2023
 *
 */

#ifndef __ZTRACE_H
#define __ZTRACE_H

#include <stdint.h>
#include <sys/time.h>
#include <stdio.h>
#include "arithdecode.h"

typedef uint8_t ztrace_arithencoding_t;

typedef struct {
  double min;
  double max;
} ztrace_norm_t;
  
typedef struct {
  uint32_t                bytes;  /* bytes taken by this node */
  uint64_t                start;  /* start position of this node on disk */
  ztrace_norm_t           norm;
  ztrace_arithencoding_t  code;
  int                     decimate;
} ztrace_node_header_t;

typedef struct {
  /* TraceFile.Header */
  int                     version;
  time_t                  ctime;
  long                    nwaves;

  /* ZtraceFile.Metadata */
  long                    nsteps;
  long                    dirStart;
  ztrace_node_header_t   *directory;
} ztrace_header_t;

int ztrace_get_header(FILE *fp, ztrace_header_t *header);

#define ZTRACE_VERSION_Unreordered      0
#define ZTRACE_VERSION_Reordered        1
#define ZTRACE_VERSION_Modifying      255
#define ZTRACE_VERSION_CompressedV1   256

void ztrace_debug_header(const ztrace_header_t *header);

#endif /* !__ZTRACE_H */
