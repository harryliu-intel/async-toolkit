
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
#include "rep16.h"

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

#define ArithConstants_ZeroCode     0
#define ArithConstants_MinCode    (ArithConstants_ZeroCode)
#define ArithConstants_MaxCode      1
#define ArithConstants_LinearCode 254
#define ArithConstants_DenseCode  255

typedef  FreqTable_t  Ztrace_CodeBook_t;

/* C is such a stupid language */
static const FreqTable_t ArithConstants_EqualCode = {

  1, 1, 1, 1,   1, 1, 1, 1,   1, 1, 1, 1,   1, 1, 1, 1,      
  1, 1, 1, 1,   1, 1, 1, 1,   1, 1, 1, 1,   1, 1, 1, 1,      
  1, 1, 1, 1,   1, 1, 1, 1,   1, 1, 1, 1,   1, 1, 1, 1,      
  1, 1, 1, 1,   1, 1, 1, 1,   1, 1, 1, 1,   1, 1, 1, 1,      
                                                             
  1, 1, 1, 1,   1, 1, 1, 1,   1, 1, 1, 1,   1, 1, 1, 1,      
  1, 1, 1, 1,   1, 1, 1, 1,   1, 1, 1, 1,   1, 1, 1, 1,      
  1, 1, 1, 1,   1, 1, 1, 1,   1, 1, 1, 1,   1, 1, 1, 1,      
  1, 1, 1, 1,   1, 1, 1, 1,   1, 1, 1, 1,   1, 1, 1, 1,      
                                                             
  1, 1, 1, 1,   1, 1, 1, 1,   1, 1, 1, 1,   1, 1, 1, 1,      
  1, 1, 1, 1,   1, 1, 1, 1,   1, 1, 1, 1,   1, 1, 1, 1,      
  1, 1, 1, 1,   1, 1, 1, 1,   1, 1, 1, 1,   1, 1, 1, 1,      
  1, 1, 1, 1,   1, 1, 1, 1,   1, 1, 1, 1,   1, 1, 1, 1,      
                                                             
  1, 1, 1, 1,   1, 1, 1, 1,   1, 1, 1, 1,   1, 1, 1, 1,      
  1, 1, 1, 1,   1, 1, 1, 1,   1, 1, 1, 1,   1, 1, 1, 1,      
  1, 1, 1, 1,   1, 1, 1, 1,   1, 1, 1, 1,   1, 1, 1, 1,      
  1, 1, 1, 1,   1, 1, 1, 1,   1, 1, 1, 1,   1, 1, 1, 1,      
                                                             
  1                                                          
    };
  

void ztrace_get_node_values(FILE                  *fp,
                            const ztrace_header_t *header,
                            long                   idx,
                            float                 *arr);

typedef struct {
  poly_segment16_t *segs;
  size_t            n;     /* allocated size */
  size_t            sz;    /* valid segs */
} poly_segment16_seq_t;

  
#endif /* !__ZTRACE_H */
