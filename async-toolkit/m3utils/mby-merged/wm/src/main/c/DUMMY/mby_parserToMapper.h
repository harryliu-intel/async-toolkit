#ifndef _mby_parserToMapper_H
#define _mby_parserToMapper_H

#include "uint.h"

#include "mby_parserHdrPtrs.h"
#include "mby_const.h"

typedef struct {
  uint16                   PA_ADJ_SEG_LEN;
  uint2                    PA_CSUM_OK;
  fm_bool                  PA_DROP;
  fm_bool                  PA_EX_DEPTH_EXCEED;
  fm_bool                  PA_EX_PARSING_DONE;
  uint8                    PA_EX_STAGE;
  fm_bool                  PA_EX_TRUNC_HEADER;
  fm_bool                  PA_FLAGS                [MBY_N_PARSER_FLAGS];
  uint16                   PA_KEYS                 [MBY_N_PARSER_KEYS];
  fm_bool                  PA_KEYS_VALID           [MBY_N_PARSER_KEYS];
  fm_bool                  PA_L3LEN_ERR;
  uint16                   PA_PACKET_TYPE;
  mby_parserHdrPtrs        PA_HDR_PTRS;
  uint32                   RX_PORT;
  uint32                   RX_LENGTH;
} mby_parserToMapper;

void mby_parserToMapper_serialize(uint64 *s, const mby_parserToMapper *t);

void mby_parserToMapper_deserialize(const uint64 *s, mby_parserToMapper *t);

#define mby_parserToMapper_serial_qwords 243
typedef uint64 mby_parserToMapper_serial_t[mby_parserToMapper_serial_qwords];

#endif /* !_mby_parserToMapper_H */
