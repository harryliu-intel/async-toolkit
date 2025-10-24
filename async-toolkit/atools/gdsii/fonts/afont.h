/* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. */
/* SPDX-License-Identifier: Apache-2.0 */


typedef struct fontdef FONT;
typedef struct chardef CHARDEF;

struct chardef {
   int dwidth;
   int map[32];
};

struct fontdef {
   short ascent, descent, boxx, boxy;
   CHARDEF cmap[128];
};

