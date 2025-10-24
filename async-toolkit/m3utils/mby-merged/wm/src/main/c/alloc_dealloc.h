/* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. */
/* SPDX-License-Identifier: Apache-2.0 */

#ifndef _ALLOC_DEALLOC_H
#define _ALLOC_DEALLOC_H

#include <stdlib.h>

typedef void     *(*alloc_func_t  )(size_t);
typedef void      (*dealloc_func_t)(void *);

#endif /* !_ALLOC_DEALLOC_H */
