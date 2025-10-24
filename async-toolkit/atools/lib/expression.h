/* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. */
/* SPDX-License-Identifier: Apache-2.0 */

/*****************************************************
 * Copyright 2005 Fulcrum Microsystems               *
 * Copyright 1999 California Institute of Technology *
 * All rights reserved.  Written by Andrew Lines.    *
 *****************************************************/

/*** function prototypes ***/
typedef double *LEX2DP(LEX *lex, void *data);
LIST *parse_expression(LEX *lex, LEX2DP *lex2dp, void *data);
void print_expression(FILE *fout, LIST *expression);
double evaluate_expression(LIST *expression);
