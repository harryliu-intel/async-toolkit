
/*
 *  Arithmetic coding -- decoder side 
 *
 * 
 *  Author: mika.nystroem@intel.com
 *  January 2023
 *
 */

#include <assert.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include "arithdecode.h"

#if 0 /* unused for decoder only */
static ArithProbability_t
GetProbability(const FreqTable_Cum_t cum, char c)
{
  return ((ArithProbability_t){ cum[(int)c],
                                cum[(int)c + 1],
                                cum[CumTableSize - 1] });
}
#endif 

void
FreqTable_Accumulate(const FreqTable_t t, FreqTable_Cum_t cum)
{
  int i;
  memset(cum, 0, sizeof(FreqTable_Cum_t));

  for (i = 1; i < CumTableSize - 1; ++i)
    cum[i] = cum[i - 1] + MAX(t[i - 1], 1);
}

/**********************************************************************/

void
ArithDecoder_Init(ArithDecoder_t     *de,
                  const FreqTable_t   freqs,
                  realloc_func_t     *realloc)
{
  /* ArithCode.Init */
  memcpy(de->up.freqs, freqs, sizeof(FreqTable_t));
  FreqTable_Accumulate(freqs, de->up.cum);
  
  de->up.lo       = 0;
  de->up.hi       = ArithBits_MaxCode;

  /* ArithCode.InitDecoder */
  de->nextBit      = 0;
  de->iptr         = 0;
  de->disconnected = 0;
  de->tailBytes    = 0;
  de->realloc      = realloc;
  
  de->buff         = NULL;
  de->bufSz        = ARITHDECODE_INIT_BUF_SZ;
  de->buff         = de->realloc(de->buff, de->bufSz);
  de->nextC        = 0;
}

ArithDecoder_t *
ArithDecoder_New(const FreqTable_t freqs)
{
  ArithDecoder_t *new = malloc(sizeof(ArithDecoder_t));
  ArithDecoder_Init(new, freqs, realloc);
  return new;
}

void
ArithDecoder_Free(ArithDecoder_t *de)
{
  free(de->buff);
  free(de);
}

typedef unsigned int Encode_t;  /* actually [0..ORD(LAST(CHAR)) + 1] */
typedef unsigned int Bit_t;     /* actually [0..1] */

#define EOF ((Encode_t)256)

static Boolean_t
GetBit(ArithDecoder_t *de, Bit_t *bit)
{
  assert(False);
}

static ArithProbability_t
GetChar(ArithDecoder_t *de, CodeValue_t scaledValue, Encode_t *c)
{
  int i;
  for (i = 0; i < CumTableSize; ++i)
    if (scaledValue < de->up.cum[i + 1]) {
      *c = i;
      return ((ArithProbability_t){ de->up.cum[(int)i],
                                    de->up.cum[(int)i + 1],
                                    de->up.cum[CumTableSize - 1] });
    }
  assert(0); /* not reached */
  return (ArithProbability_t){ 0, 0, 0};
  
}

void
NewByte(ArithDecoder_t *de, char c)
{
  /* record new byte in our structures */
  if (de->nextC == de->bufSz) {
    de->bufSz = 2 * de->bufSz;
    de->buff = de->realloc(de->buff, de->bufSz);
  }
  de->buff[de->nextC++] = c;
}

Boolean_t
ArithDecoder_NewChar(ArithDecoder_t *de, char newChar)
{
  Encode_t c;
  Bit_t    b;

  de->thisByte = newChar;
  de->nextBit = 0;

  /* reader is ready */

  if (!de->disconnected) {
    while (de->iptr < ArithBits_CodeBits) {
      int gotNext = GetBit(de, &b);

      if (gotNext) {
        de->value  = de->value >> 1;
        de->value += b;
        ++(de->iptr);
        if (de->iptr == ArithBits_CodeBits) {
          /* Decode done priming buffer */
        }
      } else {
        return False;
      }
    }
  }

  while(True) {
    if (!de->disconnected) {
      Bits_t             range       = de->up.hi - de->up.lo + 1ULL;
      Bits_Freq_t        lastCum     = de->up.cum[CumTableSize - 1];
      Bits_t             scaledValue =
             ((de->value - de->up.lo + 1ULL) * lastCum - 1ULL) / range;
      ArithProbability_t p           = GetChar(de, scaledValue, &c);

      if (c == EOF) {
        return True;
      }
      NewByte(de, c);

      {
        Bits_t newHi = de->up.lo + (range * p.hi) / p.count - 1ULL;
        Bits_t newLo = de->up.lo + (range * p.lo) / p.count;

        de->up.hi = newHi;
        de->up.lo = newLo;
      }
      
    }
    
    while(True) {
      if (!de->disconnected) {
        if        (de->up.hi <  ArithBits_OneHalf) {
          /* skip */
        } else if (de->up.lo >= ArithBits_OneHalf) {
          de->value    -= ArithBits_OneHalf;
          de->up.lo    -= ArithBits_OneHalf;
          de->up.hi    -= ArithBits_OneHalf;
        } else if (de->up.lo >= ArithBits_OneFourth &&
                   de->up.hi <  ArithBits_ThreeFourths) {
          de->value    -= ArithBits_OneFourth;
          de->up.lo    -= ArithBits_OneFourth;
          de->up.hi    -= ArithBits_OneFourth;
        } else {
          break;
        }

        de->up.lo <<= 1;
        de->up.hi <<= 1;
        ++(de->up.hi);
      }
      de->disconnected = False;

      if (GetBit(de, &b)) {
        de->value <<= 1;
        de->value += b;
      } else {
        de->disconnected = True;
        return False; /* disconnect -- is this value right? */
      }
    }
  }
}

Boolean_t
ArithDecoder_NewEof(ArithDecoder_t *de)
{
  assert(de->nextBit == 8);
  assert(de->disconnected);

  de->tailBytes = (ArithBits_CodeBits - 8 - 1) / 8 + 1;
  return ArithDecoder_NewChar(de, 0);  
}

Boolean_t
ArithDecoder_Reset(ArithDecoder_t *de, char *dest, size_t *buflen)
{
  if (*buflen < de->nextC)
    return False;

  *buflen = de->nextC;
  memcpy(dest, de->buff, de->nextC);
  de->nextC = 0;
  return True;
}
