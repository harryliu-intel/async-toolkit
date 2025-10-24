/* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. */
/* SPDX-License-Identifier: Apache-2.0 */


/*
 *  Arithmetic coding -- decoder side 
 *
 * 
 *  Author: mika.nystroem@intel.com
 *  January 2023
 *
 */


#ifndef __ARITHDECODE_H
#define __ARITHDECODE_H

#include <stdint.h>
#include <sys/types.h>
#include <stdlib.h>
#include "boolean.h"

#define ArithBits_Bits          (64ULL)       /* size of Word.T */
#define ArithBits_CodeBits      ((ArithBits_Bits / 2ULL) + 1ULL)
#define ArithBits_FreqBits      ((ArithBits_Bits / 2ULL) - 2ULL)
#define ArithBits_MaxCode       ((1ULL << ArithBits_CodeBits) - 1ULL)
#define ArithBits_MaxFreq       ((1ULL << ArithBits_FreqBits) - 1ULL)
#define ArithBits_OneHalf       (1ULL << (ArithBits_CodeBits - 1ULL))
#define ArithBits_OneFourth     (1ULL << (ArithBits_CodeBits - 2ULL))
#define ArithBits_ThreeFourths  (ArithBits_OneHalf + ArithBits_OneFourth)

typedef uint64_t Bits_t;
typedef uint64_t Bits_Freq_t;
typedef uint32_t Cardinal_t;
typedef Bits_t   CodeValue_t;

#define FreqTableSize (256 + 1) /* all of char + EOF */
typedef Bits_Freq_t FreqTable_t[FreqTableSize];
#define CumTableSize  (FreqTableSize + 1)
typedef Bits_Freq_t FreqTable_Cum_t[CumTableSize];

typedef struct {
  Bits_Freq_t lo;
  Bits_Freq_t hi;
  Bits_Freq_t count;
} ArithProbability_t;

typedef struct {
  FreqTable_t      freqs;
  FreqTable_Cum_t  cum;
  Bits_t           lo;
  Bits_t           hi;
  Cardinal_t       bits;
  Cardinal_t       bytes;
} ArithCoder_t;

typedef void *(realloc_func_t(void *, size_t)); /* type of realloc() */
typedef void  (free_func_t(void *));            /* type of free() */

#define ARITHDECODE_INIT_BUF_SZ (16)

typedef struct {
  ArithCoder_t     up;
  CodeValue_t      value;
  unsigned char    thisByte;
  unsigned char    nextBit;
  Cardinal_t       iptr;
  int              disconnected;
  Cardinal_t       tailBytes;

  char            *buff;
  Cardinal_t       nextC; /* next point to be written */
  Cardinal_t       bufSz; /* size of current buffer */
  realloc_func_t  *realloc;
  free_func_t     *free;
} ArithDecoder_t;

/**********************************************************************/

ArithProbability_t FreqTable_GetProbability(const FreqTable_Cum_t cum, char c);
void FreqTable_Accumulate(const FreqTable_t t, FreqTable_Cum_t cum);

/**********************************************************************/

/*
 * initialization and freeing 
 */

ArithDecoder_t *ArithDecoder_New(const FreqTable_t freqs);
/* new a decoder, fully initialized */

void ArithDecoder_Free(ArithDecoder_t *de);
/* free a decoder with all associated things */

/*
 * standard usage : push a character (or EOF)
 * and read out decoded results 
 */

Boolean_t ArithDecoder_NewChar(ArithDecoder_t *de, char newChar);
/* push a character into the decoder */

Boolean_t ArithDecoder_NewEof(ArithDecoder_t *de);
/* push an EOF into the decoder */

Boolean_t ArithDecoder_Reset(ArithDecoder_t *de, char *dest, size_t *buflen);
/* if buflen big enough:
   read out results into a dest buffer, reset decoder
   return True

   if buflen not big enough:
   return False
*/

#endif /* !__ARITHDECODE_H */
