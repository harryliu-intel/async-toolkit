/* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. */
/* SPDX-License-Identifier: Apache-2.0 */

/* $Id: drdist.c,v 1.2 2005/11/23 10:11:02 mika Exp $ */

/* drdist.f -- translated by f2c (version 20000817).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/


/**  barf  [ba:rf]  2.  "He suggested using FORTRAN, and everybody barfed."

	- From The Shogakukan DICTIONARY OF NEW ENGLISH (Second edition) */


#include <stdio.h>

typedef long int integer;
typedef unsigned long int uinteger;
typedef char *address;
typedef short int shortint;

typedef double real;
typedef double doublereal;

typedef struct { real r, i; } complex;
typedef struct { doublereal r, i; } doublecomplex;
typedef long int logical;
typedef char integer1;
#ifdef INTEGER_STAR_8	/* Adjust for integer*8. */
typedef long long longint;		/* system-dependent */
typedef unsigned long long ulongint;	/* system-dependent */
#endif

#define TRUE_ (1)
#define FALSE_ (0)

/* Extern is for use with -E */
#ifndef Extern
#define Extern extern
#endif

/* I/O stuff */

#ifdef f2c_i2
/* for -i2 */
typedef short flag;
typedef short ftnlen;
typedef short ftnint;
#else
typedef long int flag;
typedef long int ftnlen;
typedef long int ftnint;
#endif


#define VOID void

#define dabs(x) (doublereal)fabs(x)
#define min(a,b) ((a) <= (b) ? (a) : (b))
#define max(a,b) ((a) >= (b) ? (a) : (b))

/* procedure parameter types for -A and -C++ */

#define F2C_proc_par_types 1
typedef int /* Unknown procedure type */ (*U_fp)();
typedef integer (*I_fp)();
typedef real (*R_fp)();
typedef doublereal (*D_fp)(), (*E_fp)();
typedef /* Double Complex */ VOID (*Z_fp)();
typedef logical (*L_fp)();
typedef /* Character */ VOID (*H_fp)();
typedef /* Subroutine */ int (*S_fp)();
/* E_fp is for real functions when -R is not specified */
typedef doublereal E_f;	/* real function with -R not specified */

/* undef any lower-case symbols that your C compiler predefines, e.g.: */


#ifdef KR_headers
double pow();
double pow_dd(ap, bp) doublereal *ap, *bp;
#else
#undef abs
#include "math.h"
#ifdef __cplusplus
extern "C" {
#endif
double pow_dd(doublereal *ap, doublereal *bp)
#endif
{
return(pow(*ap, *bp) );
}
#ifdef __cplusplus
}
#endif
#ifdef __cplusplus
extern "C" {
#endif

#ifdef KR_headers
double pow_ri(ap, bp) real *ap; integer *bp;
#else
double pow_ri(real *ap, integer *bp)
#endif
{
double pow, x;
integer n;
unsigned long u;

pow = 1;
x = *ap;
n = *bp;

if(n != 0)
        {
        if(n < 0)
                {
                n = -n;
                x = 1/x;
                }
        for(u = n; ; )
                {
                if(u & 01)
                        pow *= x;
                if(u >>= 1)
                        x *= x;
                else
                        break;
                }
        }
return(pow);
}
#ifdef __cplusplus
}
#endif





/* Common Block Declarations */

struct fishcom_1_ {
    integer n1m, n2m;
    real bigda, smd, bigd;
    integer num;
    real smg, glg, err;
};

#define fishcom_1 (*(struct fishcom_1_ *) &fishcom_)

static struct {
    real v[1000];
} blek_;

#define blek_1 blek_

/* Initialized data */

struct {
    integer e_1[2];
    real e_2[3];
    integer e_3;
    real e_4[3];
    } fishcom_ = { {100, 100}, {(real)500., (real)1e-30, (real)1e30}, 20, {(
	    real)1e-30, (real)1e30, (real)1e-6} };


/* Table of constant values */

static doublereal c_b121 = .33333333333333331;
static doublereal c_b122 = .66666666666666663;

doublereal fishin_(alpha, n1, n2)
real *alpha;
integer *n1, *n2;
{
    /* Initialized data */

    static integer numbin = 50;

    /* Format strings */

    /* System generated locals */
    integer i__1;
    real ret_val, r__1;
    doublereal d__1, d__2;

    /* Builtin functions */
    double sqrt(), exp(), pow_ri(), pow_dd(), log();

    /* Local variables */ extern doublereal fish_(); static integer
isiz; static real f, g, h__; static integer i__, j; static real x, y,
z__, denom; extern doublereal phinv_(); static real x1, x2, y1, y2;
static integer ic, ii, nd; static real yy; static integer ib1, ib2,
ib3, nd1, ict; extern doublereal phi_(); static integer ish; static
real alp1;


/* ----------------------------------------------------------------------- */
/* CALCULATES THE INVERSE ÒFÓ VALUE GIVEN THE CONFIDENCE COEFFICIENT */
/*   ALPHA AND THE DEGREES OF FREEDOM(N). */
/* ----------------------------------------------------------------------- */
/*     .. Scalar Arguments .. */
/*     .. */
/*     .. Scalars in Common .. */
/*     .. */
/*     .. Local Scalars .. */
/*     .. */
/*     .. External Functions .. */
/*     .. */
/*     .. Intrinsic Functions .. */
/*     .. */
/*     .. Common blocks .. */
/*     .. */
/*     .. Statement Functions .. */
/*     .. */
/*     .. Data statements .. */
/*    2     100,100,500,1.0E-300,1.0E+300,20,1.0E-300,1.0E+300,1.0E-6/ */
/*     .. */
/*     .. Statement Function definitions .. */
/*     .. */
    y1 = (real) (*n1);
    y2 = (real) (*n2);
/* ----------------------------------------------------------------------- */
/*  ADJUST FOR DEGREES OF FREEDOM EQUAL TO 1 */
/* ----------------------------------------------------------------------- */
    if (*n1 == 1) {
	y1 = (real)2.;
    }
    if (*n2 == 1) {
	y2 = (real)2.;
    }
/* ----------------------------------------------------------------------- */
/* CALL PHINV TO GET INVERSE NORMAL VALUE OF 1.-ALPHA */
/* ----------------------------------------------------------------------- */
    r__1 = (real)1. - *alpha;
    x = phinv_(&r__1);
/* ----------------------------------------------------------------------- */
/*  COMPUTE LAMDA VALUE */
/* ----------------------------------------------------------------------- */
/* Computing 2nd power */
    r__1 = x;
    y = (r__1 * r__1 - (real)3.) / (real)6.;
    ic = 0;
/* ----------------------------------------------------------------------- */
/*  COMPUTE THE INITIAL APPROXIMATION TO THE INVERSE 'F' FUNCTION */
/* ----------------------------------------------------------------------- */
    y1 = (real)1. / (y1 - (real)1.);
    y2 = (real)1. / (y2 - (real)1.);
    h__ = (real)2. / (y1 + y2);
/*     WRITE(6,921) X,Y,Y1,Y2,H */
/* L9000: */
    x = x * sqrt(h__ + y) / h__ - (y1 - y2) * (y + (real).83333333333333337 
	    - (real)2. / (h__ * (real)3.));
    x = exp(x * (real)2.);
    isiz = 1;
    if (*n1 > fishcom_1.n1m || *n2 > fishcom_1.n2m) {
	isiz = 2;
    }
/* ----------------------------------------------------------------------- */
/*  COMPUTE THE CONSTANT TO THE 'F' DISTRIBUTION,TESTING FOR N1 AND/OR N2 */
/*   ODD OR EVEN. */
/* ----------------------------------------------------------------------- */
    g = (real)1.;
    ib1 = 2;
    if (*n1 % 2 == 0) {
	goto L10;
    }
    g = (real)1.7724539;
    ib1 = 1;
L10:
    ib2 = 2;
    if (*n2 % 2 == 0) {
	goto L20;
    }
    g *= (real)1.7724539;
    ib2 = 1;
L20:
    ib3 = 2;
    if ((*n1 + *n2) % 2 == 0) {
	goto L30;
    }
    g /= (real)1.7724539;
    ib3 = 1;
L30:
    if (ib1 + ib2 != 2) {
	g *= (real)2.;
    }
    if (*n1 + *n2 <= 3) {
	goto L60;
    }
    nd = *n1 + *n2 - 2 - ib3;
    nd1 = nd + 1;
    ish = (*n1 - ib3) / 2 << 1;
/*     WRITE(6,922) X,G,IB1,IB2,IB3,ND,ISH */
/* L9010: */
    if (nd < 0) {
	goto L50;
    }
    i__1 = nd1;
    for (ii = 1; ii <= i__1; ii += 2) {
	i__ = ii - 1;
	if (ib1 + i__ <= *n1 - 2) {
	    g *= ib1 + i__;
	}
	j = i__ - ish + ib2;
	if (j > 0 && j <= *n2 - 2) {
	    g *= j;
	}
	if (g < fishcom_1.smg || g > fishcom_1.glg) {
	    goto L100;
	}
	g /= ib3 + i__;
/* L40: */
    }
L50:
/*     WRITE (6,916)  G */
/* L9020: */
/* ----------------------------------------------------------------------- */
/*  COMPUTE THE VALUE OF FISHIN */
/* ----------------------------------------------------------------------- */
    switch ((int)isiz) {
	case 1:  goto L60;
	case 2:  goto L70;
    }
L60:
    y2 = *n2 / (*n2 + *n1 * x);
    y1 = (real)1. - y2;
    y = g * ((real)1. - *alpha - fish_(&x, n1, n2)) / sqrt(pow_ri(&y1, n1) * 
	    pow_ri(&y2, n2)) + (real)1.;
/*     F=FISH  (X,N1,N2) */
/*     WRITE(6,917) Y,X,F */
/* L9030: */
    goto L80;
L70:
/* ----------------------------------------------------------------------- */
/*         N1 GT 100 OR N2 GT 100, USE Z APPROXIMATION FOR Q RATHER */
/*      THAN FISH. */
/* ----------------------------------------------------------------------- */
    y2 = *n2 / (*n2 + *n1 * x);
    y1 = (real)1. - y2;
    d__1 = (doublereal) x;
    d__2 = (doublereal) x;
    z__ = (pow_dd(&d__1, &c_b121) * ((real)1. - (real)2. / ((real)9. * *n2)
	    ) - ((real)1. - (real)2. / ((real)9. * *n1))) / sqrt((real)2. 
	    / ((real)9. * *n1) + (real)2. / ((real)9. * *n2) * pow_dd(&
	    d__2, &c_b122));
    f = phi_(&z__);
/*     WRITE(6,917) Y,X,F,Z */
    denom = *n1 / (real)2. * log(y1) + *n2 / (real)2. * log(y2);
/*     WRITE(6,918) DENOM */
/* L9040: */
    if (dabs(denom) >= fishcom_1.bigda) {
	goto L100;
    }
    denom = exp(denom);
/*     IF (DENOM.LT.1.0E-75.OR.DENOM.GT.1.0E75) WRITE (6,902) */

    fprintf(stderr, " DENOM = %f\n", denom);
    if (denom < fishcom_1.smd || denom > fishcom_1.bigd) {
      fprintf(stderr, " DENOM OUT OF RANGE\n");
    }
    y = g * ((real)1. - *alpha - f) / denom + (real)1.;
    yy = g * ((real)1. - *alpha - f) / denom;
L80:
    ret_val = x * y;
/* L9070: */
/* ----------------------------------------------------------------------- */
/*  IF FISHIN IS NEGATIVE, RESET FISHIN TO .5*LAST APPROXIMATION(X). */
/* ----------------------------------------------------------------------- */
    if (y < (real)0.) {
	ret_val = x * (real).5;
    }
/* ----------------------------------------------------------------------- */
/*  IF THE ABSOLUTE VALUE OF THE DIFFERENCE IS LESS THAN .5E-6, RETURN */
/* ----------------------------------------------------------------------- */
    if ((r__1 = x / ret_val - (real)1., dabs(r__1)) < fishcom_1.err) {
	goto L90;
    }
/* ----------------------------------------------------------------------- */
/*  IF THE RELATIVE VALUE OF THE DIFFERENCE IS LESS THAN .5E-6, RETURN */
/* ----------------------------------------------------------------------- */
    if ((r__1 = x - ret_val, dabs(r__1)) < (real)5e-7) {
	goto L90;
    }
    ++ic;
    if (ic > fishcom_1.num) {
	return ret_val;
    }
/* ----------------------------------------------------------------------- */
/*  SET THE APPROXIMATION EQUAL TO FISHIN AND CONTINUE TO ITERATE */
/* ----------------------------------------------------------------------- */
    x = ret_val;
    switch ((int)isiz) {
	case 1:  goto L60;
	case 2:  goto L70;
    }
L90:
    return ret_val;
L100:
    alp1 = (real)1. - *alpha;
    if (*n1 < 500 || *n2 < 500) {
	goto L220;
    }
/*                  WRITE(6,923) I,G */
/* L9080: */
    if (*alpha < (real).5) {
	goto L140;
    }
/* L110: */
    ict = 0;
L120:
    ++ict;
    if (ict > numbin) {
	goto L130;
    }
    z__ = (x - *n2 / (*n2 - (real)2.)) / (*n2 / (*n2 - (real)2.) * sqrt((
	    real)2. * (*n1 + *n2 - (real)2.) / (*n1 * (*n2 - (real)4.))));
    f = phi_(&z__);
    if (f < alp1) {
	goto L130;
    }
    x += (real)-.1;
    goto L120;
L130:
    x1 = x;
    x2 = (real)1.;
    goto L180;
L140:
    ict = 0;
L150:
    ++ict;
    if (ict > numbin) {
	goto L170;
    }
    z__ = (x - *n2 / (*n2 - (real)2.)) / (*n2 / (*n2 - (real)2.) * sqrt((
	    real)2. * (*n1 + *n2 - (real)2.) / (*n1 * (*n2 - (real)4.))));
/* L160: */
    f = phi_(&z__);
    if (f > alp1) {
	goto L170;
    }
    x += (real)1.;
    goto L150;
L170:
    x1 = (real)1.;
    x2 = x;
L180:
    x = (x1 + x2) * (real).5;
    z__ = (x - *n2 / (*n2 - (real)2.)) / (*n2 / (*n2 - (real)2.) * sqrt((
	    real)2. * (*n1 + *n2 - (real)2.) / (*n1 * (*n2 - (real)4.))));
    f = phi_(&z__);
    if ((r__1 = x - x1, dabs(r__1)) - fishcom_1.err <= (real)0.) {
	goto L220;
    } else {
	goto L190;
    }
L190:
    if ((r__1 = f - alp1) < (real)0.) {
	goto L200;
    } else if (r__1 == 0) {
	goto L220;
    } else {
	goto L210;
    }
L200:
    x1 = x;
    goto L180;
L210:
    x2 = x;
    goto L180;
L220:
    ret_val = x;
    return ret_val;
} /* fishin_ */

/* Subroutine */ int overfl_(i__)
integer *i__;
{
/*     .. Scalar Arguments .. */
/*     .. */
    *i__ = 1;
    return 0;
} /* overfl_ */

doublereal cbrt_(x)
real *x;
{
    /* System generated locals */
    real ret_val;
    doublereal d__1;

    /* Builtin functions */
    double pow_dd();

/*     .. Scalar Arguments .. */
/*     .. */
    d__1 = (doublereal) (*x);
    ret_val = pow_dd(&d__1, &c_b121);
    return ret_val;
} /* cbrt_ */

doublereal fish_(f, n1, n2)
real *f;
integer *n1, *n2;
{
    /* System generated locals */
    integer i__1;
    real ret_val;

    /* Builtin functions */
    double pow_ri(), sqrt(), acos();

    /* Local variables */
    static real h__;
    static integer i__, m;
    static real x, y, d1, d2;
    static logical e1, e2, e3;
    static integer m1;
    static real dn;
    static integer ii;
    static real dt;
    extern doublereal phi_();

/*     .. Scalar Arguments .. */
/*     .. */
/*     .. Local Scalars .. */
/*     .. */
/*     .. External Functions .. */
/*     .. */
/*     .. Intrinsic Functions .. */
/*     .. */
    if (*n1 >= 100 && *n2 >= 100) {
	goto L90;
    }
/* ----------------------------------------------------------------------- */
/*     INITIALIZATION AND SETTING OF LOGICAL SWITCHES  TO .TRUE. IF */
/*     THE DEGREES OF FREEDOM ARE EVEN */
/* ----------------------------------------------------------------------- */
    e1 = FALSE_;
    e2 = FALSE_;
    e3 = FALSE_;
    if (*n1 % 2 == 0) {
	e1 = TRUE_;
    }
    if (*n2 % 2 == 0) {
	e2 = TRUE_;
    }
    x = *n2 / (*n2 + *n1 * *f);
    if (! (e1 || e2)) {
	goto L50;
    }
    if (e1 && ! e2) {
	goto L20;
    }
    if (! e1 && e2) {
	goto L10;
    }
    if (*n1 <= *n2) {
	goto L20;
    }
/* ----------------------------------------------------------------------- */
/*   INITIALIZATION FOR SECOND DEGREE OF FREEDOM EVEN  AND LESS THAN */
/*      FIRST DEGREE OF FREEDOM IF IT TOO IS EVEN */
/* ----------------------------------------------------------------------- */
L10:
    i__ = *n1;
    *n1 = *n2;
    *n2 = i__;
    x = (real)1. - x;
    e3 = TRUE_;
/* ----------------------------------------------------------------------- */
/*     INITIALIZATION FOR FIRST DEGREE OF FREEDOM EVEN AND LESS THAN */
/*   SECOND DEGREE OF FREEDOM IF IT IS EVEN */
/* ----------------------------------------------------------------------- */
L20:
    y = (real)1. - x;
/* ----------------------------------------------------------------------- */
/*     CALCULATION OF PROBABILITY FOR AT LEAST ONE DEGREE OF FREEDOM EVEN */
/* ----------------------------------------------------------------------- */
    ret_val = (real)0.;
    h__ = sqrt(pow_ri(&x, n2));
    m = *n1 / 2 - 1;
/*     DO  3 I=0,M */
    m1 = m + 1;
    i__1 = m1;
    for (ii = 1; ii <= i__1; ++ii) {
	i__ = ii - 1;
	ret_val += h__;
	h__ = h__ * y * (*n2 + i__ * (real)2.) / ((i__ + (real)1.) * (real)
		2.);
/* L30: */
    }
    if (e3) {
	goto L40;
    }
/* ----------------------------------------------------------------------- */
/*       ADJUST CALCULATED PROBABILITY IF ITS ONES COMPLEMENT WAS */
/*    CALCULATED ORIGINALLY */
/* ----------------------------------------------------------------------- */
    ret_val = (real)1. - ret_val;
    return ret_val;
L40:
    i__ = *n1;
    *n1 = *n2;
    *n2 = i__;
    return ret_val;
/* ----------------------------------------------------------------------- */
/*     CALCULATION OF THE PROBABILITY FOR BOTH DEGREES OF FREEDOM ODD */
/* ----------------------------------------------------------------------- */
L50:
    y = (real)1. - x;
    h__ = sqrt(x * y) * (real).63661977;
    ret_val = acos(sqrt(x)) * (real).63661977;
    if (*n2 == 1) {
	goto L70;
    }
    m = *n2 - 2;
    i__1 = m;
    for (i__ = 1; i__ <= i__1; i__ += 2) {
	ret_val += h__;
	h__ = h__ * x * (i__ + 1) / (i__ + 2);
/* L60: */
    }
L70:
    if (*n1 == 1) {
	return ret_val;
    }
    h__ *= *n2;
    m = *n1 - 2;
    i__1 = m;
    for (i__ = 1; i__ <= i__1; i__ += 2) {
	ret_val -= h__;
	h__ = h__ * y * (*n2 + i__) / (i__ + 2);
/* L80: */
    }
    return ret_val;
L90:
    d1 = (real) (*n1);
    d2 = (real) (*n2);
    dt = d1 / d2 * *f;
    dn = sqrt((d2 * (real)2. - (real)1.) * dt) - sqrt(d1 * (real)2. - (
	    real)1.);
    x = dn / sqrt(dt + (real)1.);
    ret_val = phi_(&x);
    return ret_val;
} /* fish_ */

doublereal phinv_(p)
real *p;
{
    /* System generated locals */
    real ret_val, r__1;

    /* Builtin functions */
    double log(), sqrt(), exp();

    /* Local variables */
    static integer i__, k;
    static real z__, t3, pt, xt, t4p, t5p;
    extern doublereal phi_();
    static real php;



/*     .. Scalar Arguments .. */
/*     .. */
/*     .. Local Scalars .. */
/*     .. */
/*     .. External Functions .. */
/*     .. */
/*     .. Intrinsic Functions .. */
/*     .. */
    if (*p == (real)1.) {
	goto L80;
    }
    if (*p == (real)0.) {
	goto L90;
    }
    if (*p > (real)1.) {
	goto L100;
    }
    if (*p < (real)0.) {
	goto L100;
    }
    k = 1;
    if (*p > (real).5) {
	goto L40;
    }
L10:
    t3 = sqrt(log(*p) * (real)-2.);
    t4p = t3 * (real).802853 + (real)2.515517 + t3 * (real).010328 * t3;
    t5p = t3 * (real)1.432788 + (real)1. + t3 * (real).189269 * t3 + t3 * (
	    real).001308 * t3 * t3;
    xt = t3 - t4p / t5p;
    xt = -xt;
/* L20: */
    for (i__ = 1; i__ <= 100; ++i__) {
	php = exp(xt * (real)-.5 * xt);
	pt = phi_(&xt);
	if ((r__1 = *p - pt, dabs(r__1)) < *p * (real)4e-8) {
	    goto L50;
	}
	z__ = (*p - pt) * (real)2.50662827 / php;
	xt += z__;
/* L30: */
    }
    goto L50;
L40:
    *p = (real)1. - *p;
    k = 2;
    goto L10;
L50:
    switch ((int)k) {
	case 1:  goto L60;
	case 2:  goto L70;
    }
L60:
    ret_val = xt;
    return ret_val;
L70:
    ret_val = -xt;
    *p = (real)1. - *p;
    return ret_val;
L80:
    ret_val = (real)1e38;
    return ret_val;
L90:
    ret_val = (real)-1e38;
    return ret_val;
L100:
    fprintf(stderr, "ARGUMENT NOT A PROBABILITY = %f\n", *p);
    return ret_val;
} /* phinv_ */

doublereal studin_(n, p)
integer *n;
real *p;
{
    /* System generated locals */
    real ret_val;
    doublereal d__1, d__2;

    /* Builtin functions */
    double sqrt(), pow_dd();

    /* Local variables */
    static real base, coef, yalf, vari, void__, xpan, ypan;
    static integer i__;
    extern /* Subroutine */ int gamma_();
    static real t, delta;
    extern doublereal expan_(), phinv_();
    static real xn, yexpan, fac, alf, eps, var;
    static logical swh;

/*     .. Scalar Arguments .. */
/*     .. */
/*     .. Local Scalars .. */
/*     .. */
/*     .. External Functions .. */
/*     .. */
/*     .. External Subroutines .. */
/*     .. */
/*     .. Intrinsic Functions .. */
/*     .. */
    eps = (real)1e-4;
    xn = (real) (*n);
    swh = FALSE_;
    t = phinv_(p);
    for (i__ = 1; i__ <= 25; ++i__) {
	if (swh) {
	    goto L10;
	}
	alf = (xn + (real)1.) / 2;
	gamma_(&alf, &xpan, &void__);
	if (xpan == (real)0.) {
	    goto L30;
	}
	yalf = xn / (real)2.;
	gamma_(&yalf, &yexpan, &void__);
	if (yexpan == (real)0.) {
	    goto L30;
	}
	ypan = (real)1. / yexpan;
	fac = sqrt(xn * (real)3.1415926);
	coef = xpan * ypan / fac;
L10:
	d__1 = (doublereal) (t * t / xn + (real)1.);
	d__2 = (doublereal) (-alf);
	base = pow_dd(&d__1, &d__2);
	vari = coef * base;
	swh = TRUE_;
	var = expan_(n, &t);
	delta = (*p - var) / vari;
	t += delta;
	if (dabs(delta) - eps <= (real)0.) {
	    goto L30;
	} else {
	    goto L20;
	}
L20:
	;
    }
L30:
    ret_val = t;
    return ret_val;
} /* studin_ */

/* Subroutine */ int cdfvec_(n, p)
integer *n;
real *p;
{
    /* System generated locals */
    integer i__1, i__2;
    real r__1;

    /* Builtin functions */
    double log(), exp(), sqrt();

    /* Local variables */
    static real a, b, c__, d__, e;
    static integer j, k, l, m;
    static real x;
    static integer kk, nn;
    extern doublereal phi_(), zot_();

/* ----------------------------------------------------------------------- */
/*  V(K) IS THE CUMULATIVE DISTRIBUTION FUNCTIONAL VALUE OF THE BINOMIAL */
/*  DISTRIBUTION FOR PARAMETERS N AND P AT THE VALUE K */
/* ----------------------------------------------------------------------- */
/*     .. Scalar Arguments .. */
/*     .. */
/*     .. Arrays in Common .. */
/*     .. */
/*     .. Local Scalars .. */
/*     .. */
/*     .. External Functions .. */
/*     .. */
/*     .. Intrinsic Functions .. */
/*     .. */
/*     .. Common blocks .. */
/*     .. */
    b = log((real)1. - *p);
    c__ = *n * b;
    blek_1.v[0] = exp(c__);
    nn = *n + 1;
    blek_1.v[nn - 1] = (real)1.;
    if (*n > 1000) {
	goto L30;
    }
    a = log(*p);
    e = zot_(n);
    m = *n * *p;
    l = m + 1;
    i__1 = m;
    for (k = 2; k <= i__1; ++k) {
	j = *n - k + 1;
	i__2 = k - 1;
	d__ = e - zot_(&i__2) - zot_(&j) + (k - 1) * a + j * b;
	blek_1.v[k - 1] = blek_1.v[k - 2] + exp(d__);
/* L10: */
    }
/*     DO 2 K=N,L,-1 */
    i__1 = *n;
    for (kk = l; kk <= i__1; ++kk) {
	k = *n - kk + l;
	j = *n - k;
	d__ = e - zot_(&k) - zot_(&j) + k * a + j * b;
	blek_1.v[k - 1] = blek_1.v[k] - exp(d__);
/* L20: */
    }
    return 0;
L30:
    x = sqrt(*n * *p * ((real)1. - *p));
    l = *n - 1;
    i__1 = l;
    for (k = 1; k <= i__1; ++k) {
	r__1 = (k - *n * *p) / x;
	blek_1.v[k] = phi_(&r__1);
/* L40: */
    }
    return 0;
} /* cdfvec_ */

doublereal phi_(x)
real *x;
{
    /* System generated locals */
    real ret_val, r__1;

    /* Builtin functions */
    double exp();

    /* Local variables */
    static real t, y, z__;
    static logical upper;

/* ----------------------------------------------------------------------- */

/*  PHI CALCULATES THE AREA UNDER THE NORMAL CURVE */
/*  A TRANSFORMATION AND J-FRACTION ARE USED ( SEE METHOD ) */

/* ----------------------------------------------------------------------- */
/*     .. Scalar Arguments .. */
/*     .. */
/*     .. Local Scalars .. */
/*     .. */
/*     .. Intrinsic Functions .. */
/*     .. */
    if (*x < (real)-13.27) {
	goto L30;
    }
    if (*x > (real)8.5) {
	goto L40;
    }
    if (*x != (real)0.) {
	goto L10;
    }
    ret_val = (real).5;
    return ret_val;
L10:
    upper = *x > (real)0.;
    z__ = dabs(*x);
    y = exp(-z__ * z__ / (real)2.) * (real).56418953027302;
    z__ /= (real)1.4142135623731;
    t = (real)0.;
    r__1 = y / z__;
    if ((dabs(r__1)) > (real)0.) {
	t = y / (z__ - (real)6.918367561873e-6 + (real).5002535090039 / (
		z__ + (real).012386797611409 + (real).77267300865878 / (z__ 
		- (real)4.3263982143053 + (real)73.456287718055 / (z__ + (
		real)15.04087136429 + (real)6.20862456572356 / (z__ + (
		real)8.8971612130791 + (real)49.182171845874 / (z__ - (
		real)2.5108230069509 - (real)2.8225972942737 / (z__ - (
		real).97597917308472 + (real)24.244213526837 / (z__ + (
		real)4.8008570125081 + (real).49227853919002 / (z__ + (
		real)7.6621170927661 + (real)50.285619125788 / (z__ - (
		real)4.6529284984655))))))))));
    }
    t /= (real)2.;
    if (upper) {
	goto L20;
    }
    ret_val = t;
    return ret_val;
L20:
    ret_val = (real)1. - t;
    return ret_val;
L30:
    ret_val = (real)0.;
    return ret_val;
L40:
    ret_val = (real)1.;
    return ret_val;
} /* phi_ */

doublereal expan_(n, t)
integer *n;
real *t;
{
    /* System generated locals */
    integer i__1;
    real ret_val, r__1;

    /* Builtin functions */
    double sqrt(), atan(), cos(), sin();

    /* Local variables */
    static real a;
    static integer i__, j;
    static real r__, v, w, an, th, su;

/*     .. Scalar Arguments .. */
/*     .. */
/*     .. Local Scalars .. */
/*     .. */
/*     .. Intrinsic Functions .. */
/*     .. */
    an = (real) (*n);
    th = atan(*t / sqrt(an));
    if (*n == 1) {
	goto L70;
    }
    j = *n - 2;
    if (*n % 2 == 0) {
	goto L40;
    }
/* L10: */
    w = cos(th);
    su = w;
    if (*n == 3) {
	goto L30;
    }
    r__ = (real)0.;
    v = (real)1.;
    i__1 = j;
    for (i__ = 3; i__ <= i__1; i__ += 2) {
	r__ += (real)2.;
	v += (real)2.;
/* Computing 2nd power */
	r__1 = cos(th);
	w = w * (r__ / v) * (r__1 * r__1);
	su += w;
/* L20: */
    }
L30:
    a = (th + sin(th) * su) * (real).63661976296290734;
    goto L80;
L40:
    w = (real)1.;
    su = (real)1.;
    if (*n == 2) {
	goto L60;
    }
    r__ = (real)-1.;
    v = (real)0.;
    i__1 = j;
    for (i__ = 2; i__ <= i__1; i__ += 2) {
	r__ += (real)2.;
	v += (real)2.;
/* Computing 2nd power */
	r__1 = cos(th);
	w = w * r__ / v * (r__1 * r__1);
	su = w + su;
/* L50: */
    }
L60:
    a = sin(th) * su;
    goto L80;
L70:
    a = th * (real).63661976296290734;
L80:
    a = a * (real).5 + (real).5;
    ret_val = a;
    return ret_val;
} /* expan_ */

doublereal pois_(n, xmu)
integer *n;
real *xmu;
{
    /* System generated locals */
    integer i__1;
    real ret_val;

    /* Builtin functions */
    double log(), exp();

    /* Local variables */
    static integer i__;
    static real s, w, x, z__;

/*     .. Scalar Arguments .. */
/*     .. */
/*     .. Local Scalars .. */
/*     .. */
/*     .. Intrinsic Functions .. */
/*     .. */
    s = (real)0.;
    ret_val = (real)1.;
    if (*n == 0) {
	goto L20;
    }
    x = log(*xmu);
    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {
	w = (real) i__;
	z__ = w * x;
	s += log(w);
	ret_val += exp(z__ - s);
/* L10: */
    }
L20:
    ret_val = exp(-(*xmu)) * ret_val;
    return ret_val;
} /* pois_ */

doublereal bin_(n, p, m)
integer *n;
real *p;
integer *m;
{
    /* System generated locals */
    integer i__1, i__2;
    real ret_val, r__1, r__2;

    /* Builtin functions */
    double sqrt(), log(), exp();

    /* Local variables */
    static real c__, d__, e;
    static integer i__, j;
    static real x, y;
    static integer ii, nn;
    extern doublereal phi_(), zot_();

/*     .. Scalar Arguments .. */
/*     .. */
/*     .. Local Scalars .. */
/*     .. */
/*     .. External Functions .. */
/*     .. */
/*     .. Intrinsic Functions .. */
/*     .. */
    if (*n <= 1000) {
	goto L10;
    }
    x = sqrt(*n * *p * ((real)1. - *p));
    r__1 = (*m - *n * *p + (real).5) / x;
    r__2 = (-(*n) * *p - (real).5) / x;
    ret_val = phi_(&r__1) - phi_(&r__2);
    return ret_val;
L10:
    ret_val = (real)1.;
    if (*m == *n) {
	return ret_val;
    }
    x = log(*p);
    y = log((real)1. - *p);
    e = zot_(n);
    nn = *n * *p;
    if (*m > nn) {
	goto L30;
    }
    c__ = *n * y;
    ret_val = exp(c__);
    if (*m == 0) {
	return ret_val;
    }
    i__1 = *m;
    for (i__ = 1; i__ <= i__1; ++i__) {
	i__2 = *n - i__;
	d__ = e - zot_(&i__) - zot_(&i__2) + i__ * x + (*n - i__) * y;
	ret_val += exp(d__);
/* L20: */
    }
    return ret_val;
L30:
    j = *m + 1;
/*     DO 4 I=N,J,-1 */
    i__1 = *n;
    for (ii = j; ii <= i__1; ++ii) {
	i__ = *n - ii + j;
	i__2 = *n - i__;
	d__ = e - zot_(&i__) - zot_(&i__2) + i__ * x + (*n - i__) * y;
	ret_val -= exp(d__);
/* L40: */
    }
    return ret_val;
} /* bin_ */

doublereal hytric_(k, n, n1, nr)
integer *k, *n, *n1, *nr;
{
    /* System generated locals */
    integer i__1, i__2, i__3, i__4, i__5;
    real ret_val;

    /* Builtin functions */
    double exp();

    /* Local variables */
    static real a, b;
    static integer j, k1, kl;
    extern doublereal fctrlg_();
    static real sum;

/*     .. Scalar Arguments .. */
/*     .. */
/*     .. Local Scalars .. */
/*     .. */
/*     .. External Functions .. */
/*     .. */
/*     .. Intrinsic Functions .. */
/*     .. */
    if (*k < min(*nr,*n1)) {
	goto L20;
    }
L10:
    ret_val = (real)1.;
    return ret_val;
L20:
/* Computing MAX */
    i__1 = 0, i__2 = *n1 + *nr - *n;
    kl = max(i__1,i__2);
    ret_val = (real)0.;
    if (*k < kl) {
	return ret_val;
    }
    a = fctrlg_(nr);
    i__1 = *n - *nr;
    i__2 = *n - *n1;
    b = fctrlg_(&i__1) + fctrlg_(n1) + fctrlg_(&i__2) - fctrlg_(n);
    sum = (real)0.;
    k1 = *k + 1;
    i__1 = k1;
    for (j = 1; j <= i__1; ++j) {
	if (j - 1 - kl < 0) {
	    goto L30;
	}
	i__2 = j - 1;
	i__3 = *nr - j + 1;
	i__4 = *n1 - j + 1;
	i__5 = *n - *nr - *n1 + j - 1;
	sum += exp(a + b - fctrlg_(&i__2) - fctrlg_(&i__3) - fctrlg_(&i__4) - 
		fctrlg_(&i__5));
L30:
	;
    }
    ret_val = sum;
    if (ret_val - (real)1. <= (real)0.) {
	goto L40;
    } else {
	goto L10;
    }
L40:
    return ret_val;
} /* hytric_ */

doublereal fctrlg_(n)
integer *n;
{
    /* System generated locals */
    real ret_val;

    /* Builtin functions */
    double log();

    /* Local variables */
    static real a;
    extern /* Subroutine */ int gamma_();
    static real af, afl;

/*     .. Scalar Arguments .. */
/*     .. */
/*     .. Local Scalars .. */
/*     .. */
/*     .. External Subroutines .. */
/*     .. */
/*     .. Intrinsic Functions .. */
/*     .. */
    a = (real) (*n);
    ret_val = (real)0.;
    if (*n == 0) {
	return ret_val;
    }
    gamma_(&a, &af, &afl);
    ret_val = afl + log(a);
    return ret_val;
} /* fctrlg_ */

doublereal betinc_(x, a, b)
real *x, *a, *b;
{
    /* Initialized data */

    static real a1 = (real)-.577191652;
    static real a2 = (real).988205891;
    static real a3 = (real)-.897056937;
    static real a4 = (real).918206857;
    static real a5 = (real)-.756704078;
    static real a6 = (real).482199394;
    static real a7 = (real)-.193527818;
    static real a8 = (real).035868343;

    /* System generated locals */
    integer i__1;
    real ret_val, r__1;
    doublereal d__1, d__2, d__3, d__4;

    /* Builtin functions */
    double sqrt(), pow_dd();

    /* Local variables */
    extern doublereal cbrt_();
    static real c__, d__, f;
    static integer i__;
    static real p, t[3], v, y;
    static integer n1, n2, n3;
    extern doublereal phi_(), chi2_();

/*     .. Scalar Arguments .. */
/*     .. */
/*     .. Local Scalars .. */
/*     .. */
/*     .. Local Arrays .. */
/*     .. */
/*     .. External Functions .. */
/*     .. */
/*     .. Intrinsic Functions .. */
/*     .. */
/*     .. Data statements .. */
/*     .. */
    if (*x > (real)0. && *x < (real)1.) {
	goto L20;
    }
/* L10: */
    ret_val = *x;
    return ret_val;
L20:
    if (*a + *b < (real)130.) {
	goto L40;
    }
    if ((*a + *b - (real)1.) * ((real)1. - *x) < (real).8) {
	goto L30;
    }
    r__1 = *b * *x;
    c__ = cbrt_(&r__1);
    r__1 = *a * ((real)1. - *x);
    d__ = cbrt_(&r__1);
    r__1 = (c__ * ((real)3. - (real)1. / (*b * (real)3.)) - d__ * ((real)
	    3. - (real)1. / (*a * (real)3.))) / sqrt(c__ * c__ / *b + d__ * 
	    d__ / *a);
    ret_val = phi_(&r__1);
    return ret_val;
L30:
    i__ = *b * (real)2.;
    v = *b * (real)2.;
/*     BETINC=1.-CHI2((1.-X)*((3.-X)*(A+B-1.)-B+1.),I,$99) */
    r__1 = ((real)1. - *x) * (((real)3. - *x) * (*a + *b - (real)1.) - *b 
	    + (real)1.);
    ret_val = (real)1. - chi2_(&r__1, &v);
    return ret_val;
L40:
    if (*x < (real).5) {
	goto L50;
    }
    y = (real)1. - *x;
    c__ = *b;
    d__ = *a;
    goto L60;
L50:
    c__ = *a;
    d__ = *b;
    y = *x;
L60:
    n1 = c__;
    n2 = d__;
    n3 = c__ + d__;
    t[0] = c__ - n1;
    t[1] = d__ - n2;
    t[2] = c__ + d__ - n3;
    p = (real)1.;
    i__1 = n3;
    for (i__ = 1; i__ <= i__1; ++i__) {
	if (i__ <= n1) {
	    p /= t[0] + i__;
	}
	if (i__ < n2) {
	    p /= t[1] + i__;
	}
	if (i__ < n3) {
	    p *= t[2] + i__;
	}
/* L70: */
    }
    if (n2 == 0) {
	p *= d__;
    }
    if (n3 == 0) {
	p /= c__ + d__;
    }
    for (i__ = 1; i__ <= 3; ++i__) {
	f = t[i__ - 1];
	t[i__ - 1] = f * (a1 + f * (a2 + f * (a3 + f * (a4 + f * (a5 + f * (
		a6 + f * (a7 + f * a8))))))) + (real)1.;
/* L80: */
    }
    d__1 = (doublereal) y;
    d__2 = (doublereal) c__;
    d__3 = (doublereal) ((real)1. - y);
    d__4 = (doublereal) d__;
    p = p * pow_dd(&d__1, &d__2) * pow_dd(&d__3, &d__4) * t[2] / (t[0] * t[1])
	    ;
    i__ = 0;
    ret_val = (real)0.;
    f = c__ + (real)1.;
    t[0] = c__ + d__;
L90:
    ret_val += p;
    p = p * y * ((t[0] + i__) / (f + i__));
    ++i__;
    if (p / ret_val > (real)5e-6) {
	goto L90;
    }
    if (! (*x < (real).5)) {
	ret_val = (real)1. - ret_val;
    }
    return ret_val;
} /* betinc_ */

doublereal gamin_(x, a)
real *x, *a;
{
    /* Initialized data */

    static real a1 = (real)-.577191652;
    static real a2 = (real).988205891;
    static real a3 = (real)-.897056937;
    static real a4 = (real).918206857;
    static real a5 = (real)-.756704078;
    static real a6 = (real).482199394;
    static real a7 = (real)-.193527818;
    static real a8 = (real).035868343;

    /* System generated locals */
    integer i__1;
    real ret_val;

    /* Builtin functions */
    double log(), exp();

    /* Local variables */
    static real f;
    static integer i__, n;
    static real p;

/*     .. Scalar Arguments .. */
/*     .. */
/*     .. Local Scalars .. */
/*     .. */
/*     .. Intrinsic Functions .. */
/*     .. */
/*     .. Data statements .. */
/*     .. */
    if (*x > (real)0.) {
	goto L10;
    }
    ret_val = *x;
    return ret_val;
L10:
    n = *a;
    f = *a - n;
/* ----------------------------------------------------------------------- */
/*     CALCULATION OF COMPLETE GAMMA FUNCTION */
/* ----------------------------------------------------------------------- */
    p = exp(-(*x) + f * log(*x)) / (f * (a1 + f * (a2 + f * (a3 + f * (a4 + f 
	    * (a5 + f * (a6 + f * (a7 + f * a8))))))) + (real)1.);
    if (n == 0) {
	goto L30;
    }
    i__1 = n;
    for (i__ = 1; i__ <= i__1; ++i__) {
	p = p * *x / (f + i__);
/* L20: */
    }
L30:
    ret_val = p;
    f = *a + (real)1.;
/* ----------------------------------------------------------------------- */
/*     CALCULATION OF APPROXIMATING SUM TO INCOMPLETE GAMMA FUNCTIONS */
/* ----------------------------------------------------------------------- */
L40:
    p = p * *x / f;
    if (p / ret_val < (real)5e-8) {
	goto L50;
    }
    ret_val += p;
    f += (real)1.;
    goto L40;
L50:
    return ret_val;
} /* gamin_ */

/* Subroutine */ int gamma_(alpha, expan, yexpan)
real *alpha, *expan, *yexpan;
{
    /* Initialized data */

    static real c__[26] = { (real)1.,(real).57721566,(real)-.65587807,(
	    real)-.042002635,(real).16653861,(real)-.042197734,(real)
	    -.0096219715,(real).0072189432,(real)-.0011651675,(real)
	    -2.1524167e-4,(real)1.2805028e-4,(real)-2.0134854e-5,(real)
	    -1.2504934e-6,(real)1.1330272e-6,(real)-2.0563384e-7,(real)
	    6.116095e-9,(real)5.0020075e-9,(real)-1.1812746e-9,(real)
	    1.043427e-10,(real)7.7823e-12,(real)-3.6968e-12,(real)5.1e-13,(
	    real)-2.06e-14,(real)-5.4e-15,(real)1.4e-15,(real)1e-16 };

    /* System generated locals */
    integer i__1;

    /* Builtin functions */
    double pow_ri(), log(), exp();

    /* Local variables */
    static real frac;
    static integer i__, k;
    static real z__, real__, a1, a2, a3, a5, gl, yi;
    static integer num;

/* ---------------------------------------------------------------------- */
/*  COMPUTES GAMMA(A)  AND  LOG(GAMMA(A)) */
/*  EXPAN = GAMMA(A), YEXPAN = LOG(GAMMA(A)) */
/* ---------------------------------------------------------------------- */
/*     .. Scalar Arguments .. */
/*     .. */
/*     .. Local Scalars .. */
/*     .. */
/*     .. Local Arrays .. */
/*     .. */
/*     .. Intrinsic Functions .. */
/*     .. */
/*     .. Data statements .. */
/*     .. */
    if (*alpha > (real)8.) {
	goto L60;
    }
    num = *alpha;
    real__ = (real) num;
    frac = *alpha - real__;
    if (frac <= (real)1e-8) {
	goto L40;
    }
    *yexpan = (real)0.;
    for (i__ = 1; i__ <= 26; ++i__) {
	*yexpan += c__[i__ - 1] * pow_ri(&frac, &i__);
/* L10: */
    }
    gl = log(*yexpan);
    if (num == 0) {
	goto L30;
    }
    i__1 = num;
    for (k = 1; k <= i__1; ++k) {
	z__ = frac + k - (real)1.;
	gl -= log(z__);
/* L20: */
    }
L30:
    *yexpan = -gl;
    *expan = (real)0.;
    if (gl > (real)-650.) {
	*expan = exp(-gl);
    }
    return 0;
L40:
    gl = (real)0.;
    --num;
    i__1 = num;
    for (i__ = 1; i__ <= i__1; ++i__) {
	yi = (real) i__;
	gl -= log(yi);
/* L50: */
    }
    goto L30;
L60:
    a1 = *alpha * (real)12.;
    a2 = *alpha * *alpha;
    a3 = a1 * (real)30. * a2;
    a5 = a1 * (real)105. * a2 * a2;
    gl = -(*alpha - (real).5) * log(*alpha) + *alpha - (real)1. / a1 + (
	    real)1. / a3 - (real)1. / a5 - (real).91893853;
    goto L30;
} /* gamma_ */

doublereal zap_(h__)
real *h__;
{
    /* System generated locals */
    real ret_val;

    /* Local variables */
    extern /* Subroutine */ int gamma_();
    static real ex;

/* ----------------------------------------------------------------------- */
/*   ZAP = GAMMA(H) */
/* ----------------------------------------------------------------------- */
/*     .. Scalar Arguments .. */
/*     .. */
/*     .. Local Scalars .. */
/*     .. */
/*     .. External Subroutines .. */
/*     .. */
    gamma_(h__, &ret_val, &ex);
    return ret_val;
} /* zap_ */

doublereal zot_(n)
integer *n;
{
    /* System generated locals */
    real ret_val;

    /* Local variables */
    extern /* Subroutine */ int gamma_();
    static real al, ex;

/* ----------------------------------------------------------------------- */
/*   ZOT = ALOG( N FACTORIAL ) */
/* ----------------------------------------------------------------------- */
/*     .. Scalar Arguments .. */
/*     .. */
/*     .. Local Scalars .. */
/*     .. */
/*     .. External Subroutines .. */
/*     .. */
    al = (real) (*n + 1);
    gamma_(&al, &ex, &ret_val);
    return ret_val;
} /* zot_ */

doublereal chi2_(x, v)
real *x, *v;
{
    /* System generated locals */
    integer i__1;
    real ret_val, r__1;

    /* Builtin functions */
    double log(), exp(), sqrt();

    /* Local variables */
    extern doublereal cbrt_();
    static real a, b, c__, d__, e, g;
    static integer i__, j, k, n;
    static real p, s, y, z__, v1;
    static integer in;
    extern /* Subroutine */ int overfl_();
    extern doublereal phi_(), zip_();

/* ----------------------------------------------------------------------- */
/*  CHI2(X,V) = TO A VALUE OF THE CHI-SQUARE DISTRIBUTION FUNCTION AT THE */
/*  POINT X, WITH V DEGREES OF FREEDOM */
/* ----------------------------------------------------------------------- */
/*     .. Scalar Arguments .. */
/*     .. */
/*     .. Local Scalars .. */
/*     .. */
/*     .. External Functions .. */
/*     .. */
/*     .. External Subroutines .. */
/*     .. */
/*     .. Intrinsic Functions .. */
/*     .. */
    ret_val = (real)0.;
    if (*x < (real)0.) {
	return ret_val;
    }
    v1 = (real) ((integer) (*v));
    if ((r__1 = *v - v1, dabs(r__1)) < (real)1e-8) {
	goto L50;
    }
L10:
    r__1 = (*v + (real)2.) / (real)2.;
    a = *v / (real)2. * (log(*x) - (real).69314718) - *x / (real)2. - zip_(
	    &r__1);
    if (dabs(a) > (real)88.028) {
	goto L40;
    }
    b = log(*x);
    s = (real)0.;
    d__ = (real)0.;
    for (j = 1; j <= 1000; ++j) {
	s += log(*v + (j << 1));
	c__ = j * b - s;
	if (dabs(c__) > (real)88.028) {
	    goto L40;
	}
	e = a + c__;
	if (exp(e) < (real)1e-8) {
	    goto L30;
	}
	d__ += exp(c__);
/* L20: */
    }
L30:
    ret_val = exp(a) * (d__ + (real)1.);
    return ret_val;
L40:
    y = (real).22222222 / *v;
    r__1 = *x / *v;
    z__ = (cbrt_(&r__1) - ((real)1. - y)) / sqrt(y);
    ret_val = phi_(&z__);
    return ret_val;
L50:
    n = *v;
    if (n == 1) {
	goto L10;
    }
    if (n == 2) {
	goto L140;
    }
    if (*x < *v) {
	goto L100;
    }
    a = (real)-.918938533 - *x / (real)2.;
    z__ = exp(a);
/* L60: */
    if (n % 2 == 0) {
	goto L80;
    }
    r__1 = sqrt(*x);
    p = (real)1. - phi_(&r__1);
    b = log(*x);
    k = (n - 1) / 2;
    s = (real)0.;
    d__ = (real)0.;
    i__ = 0;
    i__1 = k;
    for (j = 1; j <= i__1; ++j) {
	s += log((real) ((j << 1) - 1));
	c__ = (j - (real).5) * b - s;
	if (dabs(c__) > (real)88.028) {
	    goto L40;
	}
	d__ += exp(c__);
	overfl_(&i__);
	if (i__ == 1) {
	    goto L40;
	}
/* L70: */
    }
    ret_val = (real)1. - (p * (real)2. + z__ * (real)2. * d__);
    return ret_val;
L80:
    k = (n - 2) / 2;
    b = log(*x);
    s = (real)0.;
    d__ = (real)0.;
    i__ = 0;
    i__1 = k;
    for (j = 1; j <= i__1; ++j) {
	s += log((real) (j << 1));
	c__ = j * b - s;
	if (dabs(c__) > (real)88.028) {
	    goto L40;
	}
	d__ += exp(c__);
	overfl_(&i__);
	if (i__ == 1) {
	    goto L40;
	}
/* L90: */
    }
    ret_val = (real)1. - z__ * (real)2.50662827 * (d__ + (real)1.);
    if (ret_val < (real)0.) {
	goto L40;
    }
    return ret_val;
L100:
    if (n % 2 == 0) {
	goto L110;
    }
    g = sqrt(*x);
    ret_val = phi_(&g) * (real)2. - (real)1.;
    g /= (real)1.25331414;
    in = 3;
    goto L120;
L110:
    in = 4;
    g = *x / (real)2.;
    if (g > (real)88.028) {
	goto L40;
    }
    ret_val = (real)1. - exp(-g);
L120:
    if (n < 3) {
	return ret_val;
    }
    g *= exp(-(*x) / (real)2.);
    i__1 = n;
    for (i__ = in; i__ <= i__1; i__ += 2) {
	ret_val -= g;
	g = g * *x / i__;
/* L130: */
    }
    return ret_val;
L140:
    ret_val = (real)1. - exp(-(*x) / (real)2.);
    return ret_val;
} /* chi2_ */

doublereal zip_(a)
real *a;
{
    /* System generated locals */
    real ret_val;

    /* Local variables */
    extern /* Subroutine */ int gamma_();
    static real dum;

/* ----------------------------------------------------------------------- */
/*   ZIP = ALOG( GAMMA(A) ) */
/* ----------------------------------------------------------------------- */
/*     .. Scalar Arguments .. */
/*     .. */
/*     .. Local Scalars .. */
/*     .. */
/*     .. External Subroutines .. */
/*     .. */
    gamma_(a, &dum, &ret_val);
    return ret_val;
} /* zip_ */

