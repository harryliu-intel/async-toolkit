/* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. */
/* SPDX-License-Identifier: Apache-2.0 */

#include "model_c_write.h"

static void (*c2m3write)(void *, unsigned long value)=(void *)0;

void
write_field(void *field, unsigned long value)
{
  c2m3write(field, value);
}

void
set_model_c2m3callback(void (*f)(void *, unsigned long value))
{
  c2m3write = f;
}
