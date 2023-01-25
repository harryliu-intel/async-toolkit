 
/*
 *  Spice trace waveform decompression
 *
 * 
 *  Author: mika.nystroem@intel.com
 *  January 2023
 *
 */

#include <assert.h>
#include <stdint.h>
#include <sys/time.h>
#include <stdio.h>
#include <errno.h>
#include "ztrace_read.h"
#include "arithdecode.h"
#include "ztrace.h"

static int
read_node_header(FILE *fp, ztrace_node_header_t *dirent)
{
  {
    int bytes;
    if (!ztrace_read_int(fp, &bytes)) return 0;
    dirent->bytes = bytes;
  }

  {
    uint64_t start;
    if (!ztrace_read_uint64_t(fp, &start)) return 0;
    dirent->start = start;
  }
  
  {
    double min;
    if (!ztrace_read_double(fp, &min)) return 0;
    dirent->norm.min = min;
  }

  {
    double max;
    if (!ztrace_read_double(fp, &max)) return 0;
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
  if (!ztrace_read_int(fp, &(header->version))) return 0;

  if (header->version != ZTRACE_VERSION_CompressedV1) return 0;

  /* here we are reading the correct file format */
  
  {
    int timestamp;
    if (!ztrace_read_int(fp, &timestamp)) return 0;
    header->ctime = timestamp;
  }
  {
    int Nnodes;
    if (!ztrace_read_int(fp, &Nnodes)) return 0;
    header->nwaves = Nnodes;
  }
  {
    int nsteps;
    if (!ztrace_read_int(fp, &nsteps)) return 0;
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
      if (!read_node_header(fp, &(header->directory[i]))) return 0;
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

static char *
ArithDecode(const char *data, size_t *n, ztrace_arithencoding_t code)
/* 
   n holds size of encoded data on call,
   on return it holds size of decoded data 

   passed-in buffer is constant and not touched
   returned buffer is malloced and must be freed by client
*/
{
  ArithDecoder_t *decoder;
  // Ztrace_CodeBook_t ft;
  int i;

  decoder = ArithDecoder_New(ArithConstants_EqualCode);

  for (i = 0; i < *n; ++i) {
    ArithDecoder_NewChar(decoder, data[i]);
  }
  ArithDecoder_NewEof(decoder);

  
  {
    char *buf = malloc(*n);
    
    while (!ArithDecoder_Reset(decoder, buf, n)) {
      *n *= 2;
      buf = realloc(buf, *n);
    }
  
    ArithDecoder_Free(decoder);

    return buf;
  }
}

/**********************************************************************/

#define POLY_SEGMENT16_DEF_SIZE 10

poly_segment16_seq_t *
poly_segment16_seq_new(void)
{
  poly_segment16_seq_t *res;

  res = (poly_segment16_seq_t *)malloc(sizeof(poly_segment16_seq_t));

  res->n    = POLY_SEGMENT16_DEF_SIZE;
  res->segs = (poly_segment16_t *)malloc(res->n * sizeof(poly_segment16_t));
  res->sz   = 0;

  return res;
}

void
poly_segment16_seq_addhi(poly_segment16_seq_t *seq, const poly_segment16_t *seg)
{
  if (seq->sz == seq->n) {
    seq->n *= 2;
    seq->segs = realloc(seq->segs, seq->n * sizeof(poly_segment16_t));
  }

  seq->segs[(seq->sz)++] = *seg;
}

poly_segment16_seq_t *
poly_segment16_serial_construct(const char *buf, size_t n)
{
  size_t                 p    = 0;
  rep16_header_t         header;
  poly_segment16_seq_t  *seq;
  int                    hi;

  hi = -1; /* last index that was read */

  seq = poly_segment16_seq_new();
  
  p += Rep16Stream_ReadHeader(buf + p, n - p, &header);

  while (p / 2 < header.nwords) {
    poly_segment16_t seg;
    p += Rep16Stream_ReadT(buf + p, n - p, &seg.r);
    if (seg.r.order == 0 || seg.r.reset) {
      seg.lo = hi + 1;
      hi    = hi + seg.r.count;
    } else {
      seg.lo = hi;
      hi    = hi + seg.r.count - 1;
    }
    seg.n = seg.r.count;
    poly_segment16_seq_addhi(seq, &seg);
  }
  assert (p / 2 == header.nwords);

  assert(hi == header.npoints - 1);

  return seq;
}

void
poly_segment16_free(poly_segment16_seq_t *segments)
{
  free(segments->segs);
  segments->segs = NULL; 
  free(segments);
}

/**********************************************************************/

void
FixupNextC0NoCheck(const poly_segment16_seq_t *seq,
                   int                         i,
                   double                     *a)
/* this is a reduced version of the Modula-3 code

   it does not attempt to check targMaxDev */
  
{
  poly_segment16_t *seg;
  rep16_t          *new;

  seg = &(seq->segs[i]);
  new = &(seg->r);

  assert(seg->n != 0);

  if (i != seq->sz - 1) {
    poly_segment16_t *nxt;
    int               j;
    int               n;

    j = i + 1;
    n = seq->sz;

    do {
      nxt = &(seq->segs[j]);
      if (nxt->n != 0)
        break;
      ++j;
    } while (j != n);

    if (j == n)
      return;

    if (nxt->r.order != 0  && !nxt->r.reset) {
      nxt->r.c0 = Rep16_FromFloat0(Rep16_EvalPoly(new, new->count - 1));
    }

    assert (nxt == &(seq->segs[j])); /* already updated */
  }    
}
                   
static void
Reconstruct(const poly_segment16_seq_t *seq,
            double                     *a,
            size_t                      nsteps)
{
  size_t n;
  int lastHi;
  int expectLastHi;
  int j;
  
  n      = seq->sz;
  lastHi = -1;

  for (j = 0; j < n; ++j) {
    int i;
    const poly_segment16_t *seg;

    seg = &(seq->segs[j]);

    assert(seg->n == 0 || seg->r.count == seg->n);

    if (seg->r.order == 0 || seg->r.reset)
      expectLastHi = seg->lo - 1;
    else
      expectLastHi = seg->lo;

    assert(!(seg->n != 0 && lastHi != expectLastHi)); /* X chaining */

    if (seg->n != 0)
      lastHi = seg->lo + seg->n - 1;

    for (i = 0; i < seg->n; ++i) {
      double y;
      y = Rep16_EvalPoly(&(seg->r), i);
      a[seg->lo + i] = y;
    }

    if (seg->n != 0 && j != n - 1) {
      FixupNextC0NoCheck(seq, j, a);
    }
  }
  
}

static void
DecompressArray(const char *buf, size_t n, double *rarr, size_t nsteps)
{
  poly_segment16_seq_t *segments;
  segments = poly_segment16_serial_construct(buf, n);
  Reconstruct(segments, rarr, nsteps);
  poly_segment16_free(segments);
}

void
ztrace_get_node_values(FILE                  *fp,
                       const ztrace_header_t *header,
                       long                   idx,
                       float                 *arr)
{
  ztrace_node_header_t *dirent;

  dirent = &(header->directory[idx]);

  if        (dirent->code == ArithConstants_DenseCode ) {
    int i;
    if (fseek(fp, dirent->start, SEEK_SET) < 0) { perror("fseek"); exit(1); }
    for (i = 0; i < header->nsteps; ++i) {
      if (!ztrace_read_float(fp, arr + i)) {
        perror("ztrace_read_float");
        exit(1);
      }
    }
  } else if (dirent->code == ArithConstants_LinearCode) {
    int    i;
    double lfl   = header->nsteps - 1;
    double range = dirent->norm.max - dirent->norm.min;
    
    for (i = 0; i < header->nsteps; ++i) {
      double ifl   = i;
      double ratio = ifl / lfl;
      double v     = ratio * range + dirent->norm.min;
      
      arr[i] = v;
    }
  } else {
    char *data;
    char *decoded;
    
    if (fseek(fp, dirent->start, SEEK_SET) < 0) { perror("fseek"); exit(1); }
    data = malloc(dirent->bytes);

    if (fread(data, dirent->bytes, 1, fp) < 1) {
      perror("fread");
      exit(1);
    }

    {
      size_t n = dirent->bytes;
      double *darr = (double *)malloc(header->nsteps * sizeof(double));
      int i;
      
      decoded = ArithDecode(data, &n, dirent->code);
      DecompressArray(decoded, n, darr, header->nsteps);

      for (i = 0; i < header->nsteps; ++i) /* convert to single prec */
        arr[i] = darr[i];

      free(darr);
    }
    
  }
    
  
}
