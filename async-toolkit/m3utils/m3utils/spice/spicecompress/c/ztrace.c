
/*
 *  Spice trace waveform decompression
 *
 * 
 *  Author: mika.nystroem@intel.com
 *  January 2023
 *
 */

#include <stdint.h>
#include <sys/time.h>
#include <stdio.h>
#include <errno.h>
#include "arithdecode.h"
#include "ztrace.h"

static int
read_float(FILE *stream, double *x)
{
  float y;
  if (!fread(&y, sizeof(float), 1, stream)) return 0;
  *x = y;
  return 1;
}

static int
read_int(FILE *stream, int *x)
{
  int res;
  res = fread(x, sizeof(int), 1, stream);
  return res;
}

static int
read_uint64_t(FILE *stream, uint64_t *x)
{
  int res;
  res = fread(x, sizeof(uint64_t), 1, stream);
  return res;
}

static int
ztrace_node_header_read(FILE *fp, ztrace_node_header_t *dirent)
{
  {
    int bytes;
    if (!read_int(fp, &bytes)) return 0;
    dirent->bytes = bytes;
  }

  {
    uint64_t start;
    if (!read_uint64_t(fp, &start)) return 0;
    dirent->start = start;
  }
  
  {
    double min;
    if (!read_float(fp, &min)) return 0;
    dirent->norm.min = min;
  }

  {
    double max;
    if (!read_float(fp, &max)) return 0;
    dirent->norm.max = max;
  }

  {
    unsigned char c;
    if ((c = getc(fp)) == EOF) return 0;
    dirent->code = c;
  }

  {
    unsigned char decimate;
    if ((decimate = getc(fp)) == EOF) return 0;
    dirent->decimate = decimate;
  }

  return 1;
}

int
ztrace_get_header(FILE *fp, ztrace_header_t *header)
{
  if (fseek(fp, 0, SEEK_SET) < 0)           return 0;
  if (!read_int(fp, &(header->version))) return 0;

  if (header->version != ZTRACE_VERSION_CompressedV1) return 0;

  /* here we are reading the correct file format */
  
  {
    int timestamp;
    if (!read_int(fp, &timestamp)) return 0;
    header->ctime = timestamp;
  }
  {
    int Nnodes;
    if (!read_int(fp, &Nnodes)) return 0;
    header->nwaves = Nnodes;
  }
  {
    int nsteps;
    if (!read_int(fp, &nsteps)) return 0;
    header->nsteps = nsteps;
  }

  {
    long dirStart;
    if ((dirStart = ftell(fp)) < 0) return 0;
    header->dirStart = dirStart;
  }

  header->directory = malloc(header->nwaves * sizeof(ztrace_node_header_t));

  {
    int i;
    for (i = 0; i < header->nwaves; ++i)
      if (!ztrace_node_header_read(fp, &(header->directory[i]))) return 0;
  }
  return 1;
}

static void
debug_node_header(int i, const ztrace_node_header_t *dirent)
{
  printf("dirent[%d] start %ld bytes %d (%lf , %lf) code %d decimate %d\n",
         i,
         dirent->start,
         dirent->bytes,
         dirent->norm.min,
         dirent->norm.max,
         dirent->code,
         dirent->decimate);
}

void
ztrace_debug_header(const ztrace_header_t *header)
{
  if (header->version != ZTRACE_VERSION_CompressedV1) {
    printf("ztrace_debug_header unknown header version %d", header->version);
    return;
  }

  printf("ztrace_debug_header : version %d ctime %ld nwaves %ld nsteps %ld dirStart %ld\n",
         header->version,
         header->ctime,
         header->nwaves,
         header->nsteps,
         header->dirStart);
  {
    int i;
    for (i = 0; i < header->nwaves; ++i)
      debug_node_header(i, &(header->directory[i]));
  }
  
      
}
