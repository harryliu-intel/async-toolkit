/* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. */
/* SPDX-License-Identifier: Apache-2.0 */

// -*- mode:c -*-

// Copyright (C) 2019 Intel Corporation

#ifndef MODEL_STAGES_H
#define MODEL_STAGES_H

#include "varchar.h"

#define TOP_MAP mby_top_map

#define STAGE_PROTOX(nm)                                \
  void nm##_stage(nm##_rstate_t    const * const r  ,   \
                  nm##_wstate_t    const * const w  ,   \
                  nm##_in_t        const * const in ,   \
                  nm##_out_t             * const out)

/* non-expanding paste */
#define PASTE_NX(A,B)     A ## B

/* expanding paste */
#define PASTE(A,B)        PASTE_NX(A,B)

/* non-expanding stringify */
#define STR_NX(A)        #A

/* expanding stringify */
#define STR(A)           STR_NX(A)

#define STAGE_PROTO(nm)                                                 \
  void nm##_stage(                                                      \
                  TOP_MAP                  const * const r  ,           \
                  PASTE(TOP_MAP,__addr)    const * const w  ,           \
                  nm##_in_t                const * const in ,           \
                  nm##_out_t                     * const out,           \
                  varchar_t                const * const rx_data,       \
                  varchar_t                      * const tx_data)

#define STAGE_DEFINE(nm)                                                \
  void nm##_stage_void(                                                 \
                       void            const * const r,                 \
                       void            const * const w,                 \
                       void            const * const in,                \
                       void                  * const out,               \
                       varchar_t       const * const rx_data,           \
                       varchar_t             * const tx_data)           \
  { nm##_stage((TOP_MAP                  const *)  r  ,                 \
               (PASTE(TOP_MAP,__addr)    const *)  w  ,                 \
               (nm##_in_t                const *)  in ,                 \
               (nm##_out_t                     *)  out,                 \
               rx_data,                                                 \
               tx_data);                                                \
  }                                                                     \
  STAGE_PROTO(nm)

void model_stages_destroy(void); // clean up everything

/**********************************************************************/
/* Private things below here -- do not call directly or touch         */

typedef void (*model_stages_voidstar_func_t)(void      const * const r      ,
                                             void      const * const w      ,
                                             void      const * const in     ,
                                             void            * const out    ,
                                             varchar_t const * const rx_data,
                                             varchar_t       * const tx_data);
// WARNING : this typedef must be kept synchronized with
// the Modula-3 type ModelStagesC.Func

void model_stages_register(
   const char                    * top_map_name,
   const char                    * stage_name,
   model_stages_voidstar_func_t    stage_func,
   size_t                          r_size,
   size_t                          w_size,
   size_t                          in_size,
   size_t                          out_size
                           );

typedef struct model_stages_info {
  // WARNING : this typedef MUST be kept synchronized with
  // the Modula-3 type ModelStagesC.Info
  char                           * top_map_name;
  char                           * stage_name;
  model_stages_voidstar_func_t     stage_func;
  size_t                           r_size, w_size, in_size, out_size;
  struct model_stages_info       * next;
} model_stages_info_t;

extern model_stages_info_t * model_stages;
// WARNING : this declaration must be kept synchronized with
// Modula-3 object ModelStagesC.model_stages

#define REGISTRAR_PROTO()                                            \
  void PASTE(model_stages_registrar_,TOP_MAP)(void)

#define STAGE_REGISTER(nm)                                              \
  model_stages_register(                                                \
                        STR(TOP_MAP),                                   \
                        STR_NX(nm),                                     \
                        nm##_stage_void,                                \
                        sizeof(TOP_MAP),                                \
                        sizeof(PASTE(TOP_MAP,__addr)),                  \
                        sizeof(nm##_in_t),                              \
                        sizeof(nm##_out_t)                              \
                                                                        )

#endif // MODEL_STAGES_H
