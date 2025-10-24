/* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. */
/* SPDX-License-Identifier: Apache-2.0 */

/*****************************************************
 * Copyright 2005 Fulcrum Microsystems               *
 * Copyright 1999 California Institute of Technology *
 * All rights reserved.  Written by Andrew Lines.    *
 *****************************************************/

/*** maximum length of statically allocated strings ***/
#define STRMAX 16384
#define safe_sprintf(s,f,args...) \
        check_sprintf(__FILE__,__LINE__,STRMAX,snprintf(s,STRMAX,f,args))

/*** prototypes ***/
void error(char *);
void warning(char *);
void *check_alloc(void *, char *file, unsigned line);
double user_time();
void check_sprintf(char *file, unsigned line, int max, int len);
FILE *mkdir_fopen(const char *path, const char *mode);
