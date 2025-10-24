/* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. */
/* SPDX-License-Identifier: Apache-2.0 */

#include "model_stages.h"
#include <malloc.h>
#include <stdio.h>
#include <string.h>

model_stages_info_t * model_stages = NULL;

void
model_stages_register(
   const char                    * top_map_name,
   const char                    * stage_name,
   model_stages_voidstar_func_t    stage_func,
   size_t                          r_size,
   size_t                          w_size,
   size_t                          in_size,
   size_t                          out_size
                           )
{
  model_stages_info_t *new=malloc(sizeof(model_stages_info_t));
  
  printf("%s(\"%s\", \"%s\", r_size=%lu, w_size=%lu, in_size=%lu, out_size=%lu)\n", __func__,
         top_map_name, stage_name, r_size, w_size, in_size, out_size);

  new->top_map_name      = strdup(top_map_name);
  new->stage_name        = strdup(stage_name);
  new->stage_func        = stage_func;
  new->r_size            = r_size;
  new->w_size            = w_size;
  new->in_size           = in_size;
  new->out_size          = out_size;
  new->next              = model_stages;
  model_stages           = new;
}

static void
destroy_one(model_stages_info_t *info)
{
  free(info->top_map_name);
  free(info->stage_name);
  free(info);
}

void
model_stages_destroy(void)
{
  model_stages_info_t *p, *q;

  p = model_stages;
  while (p) {
    q = p->next;
    destroy_one(p);
    p = q;
  }
}
