/* main.f -- translated by f2c (version 20200916).
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

/* Table of constant values */

static integer c__1 = 1;

/* ------------------------------------------------------------------------------ */
/*     Main program of test problems in Report DAMTP 1992/NA5. */
/* ------------------------------------------------------------------------------ */
/* Main program */ int MAIN__(void)
{
    /* Format strings */
    static char fmt_10[] = "(/7x,\002Output from test problem 1 (Simple quad"
	    "ratic)\002)";
    static char fmt_20[] = "(/7x,\002Output from test problem 2 (2D unit cir"
	    "cle \002,\002calculation)\002)";
    static char fmt_30[] = "(/7x,\002Output from test problem 3 (3D ellipsoi"
	    "d \002,\002calculation)\002)";
    static char fmt_40[] = "(/7x,\002Output from test problem 4 (Weak Rosenb"
	    "rock)\002)";
    static char fmt_50[] = "(/7x,\002Output from test problem 5 (Intermediat"
	    "e \002,\002Rosenbrock)\002)";
    static char fmt_60[] = "(/7x,\002Output from test problem 6 (Equation"
	    " \002,\002(9.1.15) in Fletcher)\002)";
    static char fmt_70[] = "(/7x,\002Output from test problem 7 (Equation"
	    " \002,\002(14.4.2) in Fletcher)\002)";
    static char fmt_80[] = "(/7x,\002Output from test problem 8 (Rosen-Suzuk"
	    "i)\002)";
    static char fmt_90[] = "(/7x,\002Output from test problem 9 (Hock and"
	    " \002,\002Schittkowski 100)\002)";
    static char fmt_100[] = "(/7x,\002Output from test problem 10 (Hexagon a"
	    "rea)\002)";
    static char fmt_150[] = "(/5x,\002Least squares error in variables =\002"
	    ",1pe16.6)";
    static char fmt_170[] = "(2x,\002---------------------------------------"
	    "-------\002,\002--------------------\002)";

    /* System generated locals */
    integer i__1;
    doublereal d__1;

    /* Builtin functions */
    integer s_wsfe(cilist *), e_wsfe(void);
    double sqrt(doublereal);
    integer do_fio(integer *, char *, ftnlen);
    /* Subroutine */ int s_stop(char *, ftnlen);

    /* Local variables */
    static integer i__, m, n;
    static doublereal w[3000], x[10];
    extern /* Subroutine */ int maincalcfc_();
    static integer iact[51];
    static doublereal temp, xopt[10];
    static integer icase;
    static doublereal tempa, tempb, tempc, tempd, rhobeg;
    extern /* Subroutine */ int cobyla_(integer *, integer *, doublereal *, 
	    doublereal *, doublereal *, integer *, integer *, doublereal *, 
	    integer *, U_fp);
    static doublereal rhoend;
    static integer maxfun, iprint;

    /* Fortran I/O blocks */
    static cilist io___1 = { 0, 6, 0, fmt_10, 0 };
    static cilist io___5 = { 0, 6, 0, fmt_20, 0 };
    static cilist io___6 = { 0, 6, 0, fmt_30, 0 };
    static cilist io___7 = { 0, 6, 0, fmt_40, 0 };
    static cilist io___8 = { 0, 6, 0, fmt_50, 0 };
    static cilist io___9 = { 0, 6, 0, fmt_60, 0 };
    static cilist io___10 = { 0, 6, 0, fmt_70, 0 };
    static cilist io___11 = { 0, 6, 0, fmt_80, 0 };
    static cilist io___12 = { 0, 6, 0, fmt_90, 0 };
    static cilist io___13 = { 0, 6, 0, fmt_100, 0 };
    static cilist io___28 = { 0, 6, 0, fmt_150, 0 };
    static cilist io___29 = { 0, 6, 0, fmt_170, 0 };


    for (_BLNK__1.nprob = 1; _BLNK__1.nprob <= 10; ++_BLNK__1.nprob) {
	if (_BLNK__1.nprob == 1) {

/*     Minimization of a simple quadratic function of two variables. */

	    s_wsfe(&io___1);
	    e_wsfe();
	    n = 2;
	    m = 0;
	    xopt[0] = -1.f;
	    xopt[1] = 0.f;
	} else if (_BLNK__1.nprob == 2) {

/*     Easy two dimensional minimization in unit circle. */

	    s_wsfe(&io___5);
	    e_wsfe();
	    n = 2;
	    m = 1;
	    xopt[0] = sqrt(.5f);
	    xopt[1] = -xopt[0];
	} else if (_BLNK__1.nprob == 3) {

/*     Easy three dimensional minimization in ellipsoid. */

	    s_wsfe(&io___6);
	    e_wsfe();
	    n = 3;
	    m = 1;
	    xopt[0] = 1.f / sqrt(3.f);
	    xopt[1] = 1.f / sqrt(6.f);
	    xopt[2] = -.33333333333333331f;
	} else if (_BLNK__1.nprob == 4) {

/*     Weak version of Rosenbrock's problem. */

	    s_wsfe(&io___7);
	    e_wsfe();
	    n = 2;
	    m = 0;
	    xopt[0] = -1.f;
	    xopt[1] = 1.f;
	} else if (_BLNK__1.nprob == 5) {

/*     Intermediate version of Rosenbrock's problem. */

	    s_wsfe(&io___8);
	    e_wsfe();
	    n = 2;
	    m = 0;
	    xopt[0] = -1.f;
	    xopt[1] = 1.f;
	} else if (_BLNK__1.nprob == 6) {

/*     This problem is taken from Fletcher's book Practical Methods of */
/*     Optimization and has the equation number (9.1.15). */

	    s_wsfe(&io___9);
	    e_wsfe();
	    n = 2;
	    m = 2;
	    xopt[0] = sqrt(.5f);
	    xopt[1] = xopt[0];
	} else if (_BLNK__1.nprob == 7) {

/*     This problem is taken from Fletcher's book Practical Methods of */
/*     Optimization and has the equation number (14.4.2). */

	    s_wsfe(&io___10);
	    e_wsfe();
	    n = 3;
	    m = 3;
	    xopt[0] = 0.f;
	    xopt[1] = -3.f;
	    xopt[2] = -3.f;
	} else if (_BLNK__1.nprob == 8) {

/*     This problem is taken from page 66 of Hock and Schittkowski's book Test */
/*     Examples for Nonlinear Programming Codes. It is their test problem Number */
/*     43, and has the name Rosen-Suzuki. */

	    s_wsfe(&io___11);
	    e_wsfe();
	    n = 4;
	    m = 3;
	    xopt[0] = 0.f;
	    xopt[1] = 1.f;
	    xopt[2] = 2.f;
	    xopt[3] = -1.f;
	} else if (_BLNK__1.nprob == 9) {

/*     This problem is taken from page 111 of Hock and Schittkowski's */
/*     book Test Examples for Nonlinear Programming Codes. It is their */
/*     test problem Number 100. */

	    s_wsfe(&io___12);
	    e_wsfe();
	    n = 7;
	    m = 4;
	    xopt[0] = 2.330499f;
	    xopt[1] = 1.951372f;
	    xopt[2] = -.4775414f;
	    xopt[3] = 4.365726f;
	    xopt[4] = -.624487f;
	    xopt[5] = 1.038131f;
	    xopt[6] = 1.594227f;
	} else if (_BLNK__1.nprob == 10) {

/*     This problem is taken from page 415 of Luenberger's book Applied */
/*     Nonlinear Programming. It is to maximize the area of a hexagon of */
/*     unit diameter. */

	    s_wsfe(&io___13);
	    e_wsfe();
	    n = 9;
	    m = 14;
	}
	for (icase = 1; icase <= 2; ++icase) {
	    i__1 = n;
	    for (i__ = 1; i__ <= i__1; ++i__) {
/* L120: */
		x[i__ - 1] = 1.f;
	    }
	    rhobeg = .5f;
	    rhoend = .001f;
	    if (icase == 2) {
		rhoend = 1e-4f;
	    }
	    iprint = 1;
	    maxfun = 2000;
	    cobyla_(&n, &m, x, &rhobeg, &rhoend, &iprint, &maxfun, w, iact, (
		    U_fp)maincalcfc_);
	    if (_BLNK__1.nprob == 10) {
		tempa = x[0] + x[2] + x[4] + x[6];
		tempb = x[1] + x[3] + x[5] + x[7];
		tempc = .5f / sqrt(tempa * tempa + tempb * tempb);
		tempd = tempc * sqrt(3.f);
		xopt[0] = tempd * tempa + tempc * tempb;
		xopt[1] = tempd * tempb - tempc * tempa;
		xopt[2] = tempd * tempa - tempc * tempb;
		xopt[3] = tempd * tempb + tempc * tempa;
		for (i__ = 1; i__ <= 4; ++i__) {
/* L130: */
		    xopt[i__ + 3] = xopt[i__ - 1];
		}
	    }
	    temp = 0.f;
	    i__1 = n;
	    for (i__ = 1; i__ <= i__1; ++i__) {
/* L140: */
/* Computing 2nd power */
		d__1 = x[i__ - 1] - xopt[i__ - 1];
		temp += d__1 * d__1;
	    }
	    s_wsfe(&io___28);
	    d__1 = sqrt(temp);
	    do_fio(&c__1, (char *)&d__1, (ftnlen)sizeof(doublereal));
	    e_wsfe();
/* L160: */
	}
	s_wsfe(&io___29);
	e_wsfe();
/* L180: */
    }
    s_stop("", (ftnlen)0);
    return 0;
} /* MAIN__ */

