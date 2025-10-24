/* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. */
/* SPDX-License-Identifier: Apache-2.0 */

#ifndef __PLATFORM_TYPES_H
#define __PLATFORM_TYPES_H

#include "fm_std.h" /* from std/intel on x86 */

typedef struct
{
  /* Logical port number. */
  fm_int      logPort;
  
  /* Physical port number. */
  fm_int      physPort;
  
} fm_platformPort;

#endif /* !__PLATFORM_TYPES_H */
