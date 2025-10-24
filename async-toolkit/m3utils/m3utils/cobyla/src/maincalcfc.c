/* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. */
/* SPDX-License-Identifier: Apache-2.0 */

/* maincalcfc.f -- translated by f2c (version 20200916).
   You must link the resulting object file with libf2c:
	on Microsoft Windows system, link with libf2c.lib;
	on Linux or Unix systems, link with .../path/to/libf2c.a -lm
	or, if you install libf2c.a in a standard place, with -lf2c -lm
	-- in that order, at the end of the command line, as in
		cc *.o -lf2c -lm
	Source for libf2c is in /netlib/f2c/libf2c.zip, e.g.,

		http://www.netlib.org/f2c/libf2c.zip
*/

#include "f2c.h"

/* Common Block Declarations */

struct {
    integer nprob;
} _BLNK__;

#define _BLNK__1 _BLNK__

doublereal maincalcfc_(integer *n, integer *m, doublereal *x, doublereal *con)
{
    /* System generated locals */
    doublereal ret_val, d__1, d__2, d__3, d__4, d__5, d__6, d__7;

    /* Parameter adjustments */
    --con;
    --x;

    /* Function Body */
    if (_BLNK__1.nprob == 1) {

/*     Test problem 1 (Simple quadratic) */

/* Computing 2nd power */
	d__1 = x[1] + 1.f;
/* Computing 2nd power */
	d__2 = x[2];
	ret_val = d__1 * d__1 * 10.f + d__2 * d__2;
    } else if (_BLNK__1.nprob == 2) {

/*    Test problem 2 (2D unit circle calculation) */

	ret_val = x[1] * x[2];
/* Computing 2nd power */
	d__1 = x[1];
/* Computing 2nd power */
	d__2 = x[2];
	con[1] = 1.f - d__1 * d__1 - d__2 * d__2;
    } else if (_BLNK__1.nprob == 3) {

/*     Test problem 3 (3D ellipsoid calculation) */

	ret_val = x[1] * x[2] * x[3];
/* Computing 2nd power */
	d__1 = x[1];
/* Computing 2nd power */
	d__2 = x[2];
/* Computing 2nd power */
	d__3 = x[3];
	con[1] = 1.f - d__1 * d__1 - d__2 * d__2 * 2.f - d__3 * d__3 * 3.f;
    } else if (_BLNK__1.nprob == 4) {

/*     Test problem 4 (Weak Rosenbrock) */

/* Computing 2nd power */
	d__2 = x[1];
/* Computing 2nd power */
	d__1 = d__2 * d__2 - x[2];
/* Computing 2nd power */
	d__3 = x[1] + 1.f;
	ret_val = d__1 * d__1 + d__3 * d__3;
    } else if (_BLNK__1.nprob == 5) {

/*     Test problem 5 (Intermediate Rosenbrock) */

/* Computing 2nd power */
	d__2 = x[1];
/* Computing 2nd power */
	d__1 = d__2 * d__2 - x[2];
/* Computing 2nd power */
	d__3 = x[1] + 1.f;
	ret_val = d__1 * d__1 * 10.f + d__3 * d__3;
    } else if (_BLNK__1.nprob == 6) {

/*     Test problem 6 (Equation (9.1.15) in Fletcher's book) */

	ret_val = -x[1] - x[2];
/* Computing 2nd power */
	d__1 = x[1];
	con[1] = x[2] - d__1 * d__1;
/* Computing 2nd power */
	d__1 = x[1];
/* Computing 2nd power */
	d__2 = x[2];
	con[2] = 1.f - d__1 * d__1 - d__2 * d__2;
    } else if (_BLNK__1.nprob == 7) {

/*     Test problem 7 (Equation (14.4.2) in Fletcher's book) */

	ret_val = x[3];
	con[1] = x[1] * 5.f - x[2] + x[3];
/* Computing 2nd power */
	d__1 = x[1];
/* Computing 2nd power */
	d__2 = x[2];
	con[2] = x[3] - d__1 * d__1 - d__2 * d__2 - x[2] * 4.f;
	con[3] = x[3] - x[1] * 5.f - x[2];
    } else if (_BLNK__1.nprob == 8) {

/*     Test problem 8 (Rosen-Suzuki) */

/* Computing 2nd power */
	d__1 = x[1];
/* Computing 2nd power */
	d__2 = x[2];
/* Computing 2nd power */
	d__3 = x[3];
/* Computing 2nd power */
	d__4 = x[4];
	ret_val = d__1 * d__1 + d__2 * d__2 + d__3 * d__3 * 2.f + d__4 * d__4 
		- x[1] * 5.f - x[2] * 5.f - x[3] * 21.f + x[4] * 7.f;
/* Computing 2nd power */
	d__1 = x[1];
/* Computing 2nd power */
	d__2 = x[2];
/* Computing 2nd power */
	d__3 = x[3];
/* Computing 2nd power */
	d__4 = x[4];
	con[1] = 8.f - d__1 * d__1 - d__2 * d__2 - d__3 * d__3 - d__4 * d__4 
		- x[1] + x[2] - x[3] + x[4];
/* Computing 2nd power */
	d__1 = x[1];
/* Computing 2nd power */
	d__2 = x[2];
/* Computing 2nd power */
	d__3 = x[3];
/* Computing 2nd power */
	d__4 = x[4];
	con[2] = 10.f - d__1 * d__1 - d__2 * d__2 * 2.f - d__3 * d__3 - d__4 *
		 d__4 * 2.f + x[1] + x[4];
/* Computing 2nd power */
	d__1 = x[1];
/* Computing 2nd power */
	d__2 = x[2];
/* Computing 2nd power */
	d__3 = x[3];
	con[3] = 5.f - d__1 * d__1 * 2.f - d__2 * d__2 - d__3 * d__3 - x[1] * 
		2.f + x[2] + x[4];
    } else if (_BLNK__1.nprob == 9) {

/*     Test problem 9 (Hock and Schittkowski 100) */

/* Computing 2nd power */
	d__1 = x[1] - 10.f;
/* Computing 2nd power */
	d__2 = x[2] - 12.f;
/* Computing 4th power */
	d__3 = x[3], d__3 *= d__3;
/* Computing 2nd power */
	d__4 = x[4] - 11.f;
/* Computing 6th power */
	d__5 = x[5], d__5 *= d__5;
/* Computing 2nd power */
	d__6 = x[6];
/* Computing 4th power */
	d__7 = x[7], d__7 *= d__7;
	ret_val = d__1 * d__1 + d__2 * d__2 * 5.f + d__3 * d__3 + d__4 * d__4 
		* 3.f + d__5 * (d__5 * d__5) * 10.f + d__6 * d__6 * 7.f + 
		d__7 * d__7 - x[6] * 4.f * x[7] - x[6] * 10.f - x[7] * 8.f;
/* Computing 2nd power */
	d__1 = x[1];
/* Computing 4th power */
	d__2 = x[2], d__2 *= d__2;
/* Computing 2nd power */
	d__3 = x[4];
	con[1] = 127.f - d__1 * d__1 * 2.f - d__2 * d__2 * 3.f - x[3] - d__3 *
		 d__3 * 4.f - x[5] * 5.f;
/* Computing 2nd power */
	d__1 = x[3];
	con[2] = 282.f - x[1] * 7.f - x[2] * 3.f - d__1 * d__1 * 10.f - x[4] 
		+ x[5];
/* Computing 2nd power */
	d__1 = x[2];
/* Computing 2nd power */
	d__2 = x[6];
	con[3] = 196.f - x[1] * 23.f - d__1 * d__1 - d__2 * d__2 * 6.f + x[7] 
		* 8.f;
/* Computing 2nd power */
	d__1 = x[1];
/* Computing 2nd power */
	d__2 = x[2];
/* Computing 2nd power */
	d__3 = x[3];
	con[4] = d__1 * d__1 * -4.f - d__2 * d__2 + x[1] * 3.f * x[2] - d__3 *
		 d__3 * 2.f - x[6] * 5.f + x[7] * 11.f;
    } else if (_BLNK__1.nprob == 10) {

/*     Test problem 10 (Hexagon area) */

	ret_val = (x[1] * x[4] - x[2] * x[3] + x[3] * x[9] - x[5] * x[9] + x[
		5] * x[8] - x[6] * x[7]) * -.5f;
/* Computing 2nd power */
	d__1 = x[3];
/* Computing 2nd power */
	d__2 = x[4];
	con[1] = 1.f - d__1 * d__1 - d__2 * d__2;
/* Computing 2nd power */
	d__1 = x[9];
	con[2] = 1.f - d__1 * d__1;
/* Computing 2nd power */
	d__1 = x[5];
/* Computing 2nd power */
	d__2 = x[6];
	con[3] = 1.f - d__1 * d__1 - d__2 * d__2;
/* Computing 2nd power */
	d__1 = x[1];
/* Computing 2nd power */
	d__2 = x[2] - x[9];
	con[4] = 1.f - d__1 * d__1 - d__2 * d__2;
/* Computing 2nd power */
	d__1 = x[1] - x[5];
/* Computing 2nd power */
	d__2 = x[2] - x[6];
	con[5] = 1.f - d__1 * d__1 - d__2 * d__2;
/* Computing 2nd power */
	d__1 = x[1] - x[7];
/* Computing 2nd power */
	d__2 = x[2] - x[8];
	con[6] = 1.f - d__1 * d__1 - d__2 * d__2;
/* Computing 2nd power */
	d__1 = x[3] - x[5];
/* Computing 2nd power */
	d__2 = x[4] - x[6];
	con[7] = 1.f - d__1 * d__1 - d__2 * d__2;
/* Computing 2nd power */
	d__1 = x[3] - x[7];
/* Computing 2nd power */
	d__2 = x[4] - x[8];
	con[8] = 1.f - d__1 * d__1 - d__2 * d__2;
/* Computing 2nd power */
	d__1 = x[7];
/* Computing 2nd power */
	d__2 = x[8] - x[9];
	con[9] = 1.f - d__1 * d__1 - d__2 * d__2;
	con[10] = x[1] * x[4] - x[2] * x[3];
	con[11] = x[3] * x[9];
	con[12] = -x[5] * x[9];
	con[13] = x[5] * x[8] - x[6] * x[7];
	con[14] = x[9];
    }
    return ret_val;
} /* maincalcfc_ */

