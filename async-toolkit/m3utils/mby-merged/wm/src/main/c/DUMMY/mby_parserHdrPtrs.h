#ifndef _mby_parserHdrPtrs_H
#define _mby_parserHdrPtrs_H

#include "uint.h"

#include "mby_const.h"

typedef struct {
  uint8                    OFFSET                  [MBY_N_PARSER_PTRS];
  fm_bool                  OFFSET_VALID            [MBY_N_PARSER_PTRS];
  uint8                    PROT_ID                 [MBY_N_PARSER_PTRS + 1]; /* manually edited, sorry */
} mby_parserHdrPtrs;

void mby_parserHdrPtrs_serialize(uint64 *s, const mby_parserHdrPtrs *t);

void mby_parserHdrPtrs_deserialize(const uint64 *s, mby_parserHdrPtrs *t);

#define mby_parserHdrPtrs_serial_qwords 24
typedef uint64 mby_parserHdrPtrs_serial_t[mby_parserHdrPtrs_serial_qwords];

#endif /* !_mby_parserHdrPtrs_H */
