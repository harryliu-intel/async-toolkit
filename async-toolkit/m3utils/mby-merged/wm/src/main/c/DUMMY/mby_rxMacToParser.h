#ifndef _mby_rxMacToParser_H
#define _mby_rxMacToParser_H

#include "uint.h"

#include "mby_const.h"

typedef struct {
  uint32                   RX_PORT;
  uint32                   RX_LENGTH;
  uint8                    SEG_DATA                [MBY_PA_MAX_SEG_LEN];
} mby_rxMacToParser;

void mby_rxMacToParser_serialize(uint64 *s, const mby_rxMacToParser *t);

void mby_rxMacToParser_deserialize(const uint64 *s, mby_rxMacToParser *t);

#define mby_rxMacToParser_serial_qwords 194
typedef uint64 mby_rxMacToParser_serial_t[mby_rxMacToParser_serial_qwords];

#endif /* !_mby_rxMacToParser_H */
