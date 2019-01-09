#ifndef _MODEL_STAGES_H
#define _MODEL_STAGES_H

#include "varchar.h"

#define STAGE_PROTOX(nm)                                \
  void nm##_stage(nm##_rstate_t    const * const r  ,   \
                  nm##_wstate_t    const * const w  ,   \
                  nm##_in_t        const * const in ,   \
                  nm##_out_t             * const out)

#define PASTE_NX(A,B)     A ## B

#define PASTE(A,B)        PASTE_NX(A,B)

#define STAGE_PROTO(nm)                                                 \
  void nm##_stage(TOP_MAP                  const * const r  ,           \
                  PASTE(TOP_MAP,__addr)    const * const w  ,           \
                  nm##_in_t                const * const in ,           \
                  nm##_out_t                     * const out,           \
                  varchar_t                const * const rx_data,       \
                  varchar_t                      * const tx_data)


#endif /*!_MODEL_STAGES_H*/
