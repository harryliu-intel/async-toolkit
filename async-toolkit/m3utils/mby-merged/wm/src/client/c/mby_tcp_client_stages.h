#ifndef MBY_TCP_CLIENT_STAGES_H
#define MBY_TCP_CLIENT_STAGES_H

#include "mby_parser.h"
#include "mby_mapper.h"
#include "mby_classifier.h"

#include "mby_modifier.h"

/* a standard stage is set up for remote calling like this */
#define SIMPLE_STAGE_PROTO(nm)                       \
  int wm_##nm ( nm##_in_t     const * const in,      \
                nm##_out_t          * const out)
                
#define SIMPLE_STAGE_IMPL(nm)                                           \
  SIMPLE_STAGE_PROTO(nm)                                                \
  {                                                                     \
  return                                                                \
  wm_do_stage(#nm, in, sizeof(*in), NULL, out, sizeof(*out), NULL);     \
  }

SIMPLE_STAGE_PROTO(Parser);
SIMPLE_STAGE_PROTO(Mapper);
SIMPLE_STAGE_PROTO(Classifier);

/* modifier uses both rx_data and tx_data so it's more complicated */
int
wm_Modifier(Modifier_in_t   const * const in,
            Modifier_out_t        * const out,
            varchar_t       const * const rx_data,
            varchar_t             * const tx_data);

// a version for dv (another layer of wrapper) - use fixed-length data 

#define MBY_MAX_PACKET_LEN 32*1024 // ...for example...

int
dv_Modifier(Modifier_in_t   const * const in,
            Modifier_out_t        * const out,
            unsigned char           const rx_data_a[MBY_MAX_PACKET_LEN],
            unsigned int            const rx_length,
            unsigned char                 tx_data_a[MBY_MAX_PACKET_LEN],
            unsigned int          *       tx_length);

#endif /* ! MBY_TCP_CLIENT_STAGES_H */
