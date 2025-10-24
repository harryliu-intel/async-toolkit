/* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. */
/* SPDX-License-Identifier: Apache-2.0 */

/* cobylb.f -- translated by f2c (version 20200916).
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

/* Table of constant values */

static integer c__1 = 1;

/* Subroutine */ int cobylb_(integer *n, integer *m, integer *mpp, doublereal 
	*x, doublereal *rhobeg, doublereal *rhoend, integer *iprint, integer *
	maxfun, doublereal *con, doublereal *sim, doublereal *simi, 
	doublereal *datmat, doublereal *a, doublereal *vsig, doublereal *veta,
	 doublereal *sigbar, doublereal *dx, doublereal *w, integer *iact, 
	D_fp calcfc, integer *itag)
{
    /* Format strings */
    static char fmt_10[] = "(/3x,\002The initial value of RHO is\002,1pe13.6"
	    ",2x,\002and PARMU is set to zero.\002)";
    static char fmt_50[] = "(/3x,\002Return from subroutine COBYLA because t"
	    "he \002,\002MAXFUN limit has been reached.\002)";
    static char fmt_70[] = "(/3x,\002NFVALS =\002,i5,3x,\002F =\002,1pe13.6,"
	    "4x,\002MAXCV =\002,1pe13.6/3x,\002X =\002,1pe13.6,1p4e15.6)";
    static char fmt_80[] = "(1pe19.6,1p4e15.6)";
    static char fmt_210[] = "(/3x,\002Return from subroutine COBYLA because"
	    " \002,\002rounding errors are becoming damaging.\002)";
    static char fmt_410[] = "(/3x,\002Increase in PARMU to\002,1pe13.6)";
    static char fmt_580[] = "(/3x,\002Reduction in RHO to\002,1pe13.6,\002  "
	    "and PARMU =\002,1pe13.6)";
    static char fmt_590[] = "(/3x,\002Normal return from subroutine COBYL"
	    "A\002)";

    /* System generated locals */
    integer sim_dim1, sim_offset, simi_dim1, simi_offset, datmat_dim1, 
	    datmat_offset, a_dim1, a_offset, i__1, i__2, i__3;
    doublereal d__1, d__2;

    /* Builtin functions */
    integer s_wsfe(cilist *), do_fio(integer *, char *, ftnlen), e_wsfe(void);
    double sqrt(doublereal);

    /* Local variables */
    static doublereal f;
    static integer i__, j, k, l, mp, np, iz;
    static doublereal phi, rho, sum, beta, cmin, cmax;
    static integer ivmc;
    static doublereal weta;
    static integer ivmd;
    static doublereal temp, wsig, gamma;
    static integer iflag;
    static doublereal alpha, delta, denom, tempa, barmu;
    static integer nbest, ifull, iptem, jdrop;
    static doublereal ratio, vmold, parmu, error, vmnew;
    static integer ibrnch;
    static doublereal edgmax, pareta, prerec, phimin, parsig;
    static integer isdirn, nfvals, izdota;
    static doublereal cvmaxm, dxsign, prerem;
    static integer iptemp;
    static doublereal resmax, cvmaxp;
    static integer idxnew;
    static doublereal resnew, trured;
    extern /* Subroutine */ int trstlp_(integer *, integer *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, integer *, integer *, 
	    doublereal *, doublereal *, doublereal *, doublereal *, 
	    doublereal *, doublereal *);

    /* Fortran I/O blocks */
    static cilist io___11 = { 0, 6, 0, fmt_10, 0 };
    static cilist io___18 = { 0, 6, 0, fmt_50, 0 };
    static cilist io___22 = { 0, 6, 0, fmt_70, 0 };
    static cilist io___23 = { 0, 6, 0, fmt_80, 0 };
    static cilist io___28 = { 0, 6, 0, fmt_210, 0 };
    static cilist io___48 = { 0, 6, 0, fmt_410, 0 };
    static cilist io___60 = { 0, 6, 0, fmt_580, 0 };
    static cilist io___61 = { 0, 6, 0, fmt_70, 0 };
    static cilist io___62 = { 0, 6, 0, fmt_80, 0 };
    static cilist io___63 = { 0, 6, 0, fmt_590, 0 };
    static cilist io___64 = { 0, 6, 0, fmt_70, 0 };
    static cilist io___65 = { 0, 6, 0, fmt_80, 0 };



/*     Set the initial values of some parameters. The last column of SIM holds */
/*     the optimal vertex of the current simplex, and the preceding N columns */
/*     hold the displacements from the optimal vertex to the other vertices. */
/*     Further, SIMI holds the inverse of the matrix that is contained in the */
/*     first N columns of SIM. */

    /* Parameter adjustments */
    a_dim1 = *n;
    a_offset = 1 + a_dim1;
    a -= a_offset;
    simi_dim1 = *n;
    simi_offset = 1 + simi_dim1;
    simi -= simi_offset;
    sim_dim1 = *n;
    sim_offset = 1 + sim_dim1;
    sim -= sim_offset;
    datmat_dim1 = *mpp;
    datmat_offset = 1 + datmat_dim1;
    datmat -= datmat_offset;
    --x;
    --con;
    --vsig;
    --veta;
    --sigbar;
    --dx;
    --w;
    --iact;
    --itag;

    /* Function Body */
    iptem = min(*n,5);
    iptemp = iptem + 1;
    np = *n + 1;
    mp = *m + 1;
    alpha = .25f;
    beta = 2.1f;
    gamma = .5f;
    delta = 1.1f;
    rho = *rhobeg;
    parmu = 0.f;
    if (*iprint >= 2) {
	s_wsfe(&io___11);
	do_fio(&c__1, (char *)&rho, (ftnlen)sizeof(doublereal));
	e_wsfe();
    }
    nfvals = 0;
    temp = 1.f / rho;
    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {
	sim[i__ + np * sim_dim1] = x[i__];
	i__2 = *n;
	for (j = 1; j <= i__2; ++j) {
	    sim[i__ + j * sim_dim1] = 0.f;
/* L20: */
	    simi[i__ + j * simi_dim1] = 0.f;
	}
	sim[i__ + i__ * sim_dim1] = rho;
/* L30: */
	simi[i__ + i__ * simi_dim1] = temp;
    }
    jdrop = np;
    ibrnch = 0;

/*     Make the next call of the user-supplied subroutine CALCFC. These */
/*     instructions are also used for calling CALCFC during the iterations of */
/*     the algorithm. */

L40:
    if (nfvals >= *maxfun && nfvals > 0) {
	if (*iprint >= 1) {
	    s_wsfe(&io___18);
	    e_wsfe();
	}
	goto L600;
    }
    ++nfvals;
    f = (*calcfc)(n, m, &x[1], &con[1], &itag[1]);
    resmax = 0.f;
    if (*m > 0) {
	i__1 = *m;
	for (k = 1; k <= i__1; ++k) {
/* L60: */
/* Computing MAX */
	    d__1 = resmax, d__2 = -con[k];
	    resmax = (real) max(d__1,d__2);
	}
    }
    if (nfvals == *iprint - 1 || *iprint == 3) {
	s_wsfe(&io___22);
	do_fio(&c__1, (char *)&nfvals, (ftnlen)sizeof(integer));
	do_fio(&c__1, (char *)&f, (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&resmax, (ftnlen)sizeof(doublereal));
	i__1 = iptem;
	for (i__ = 1; i__ <= i__1; ++i__) {
	    do_fio(&c__1, (char *)&x[i__], (ftnlen)sizeof(doublereal));
	}
	e_wsfe();
	if (iptem < *n) {
	    s_wsfe(&io___23);
	    i__1 = *n;
	    for (i__ = iptemp; i__ <= i__1; ++i__) {
		do_fio(&c__1, (char *)&x[i__], (ftnlen)sizeof(doublereal));
	    }
	    e_wsfe();
	}
    }
    con[mp] = f;
    con[*mpp] = resmax;
    if (ibrnch == 1) {
	goto L440;
    }

/*     Set the recently calculated function values in a column of DATMAT. This */
/*     array has a column for each vertex of the current simplex, the entries of */
/*     each column being the values of the constraint functions (if any) */
/*     followed by the objective function and the greatest constraint violation */
/*     at the vertex. */

    i__1 = *mpp;
    for (k = 1; k <= i__1; ++k) {
/* L90: */
	datmat[k + jdrop * datmat_dim1] = con[k];
    }
    if (nfvals > np) {
	goto L130;
    }

/*     Exchange the new vertex of the initial simplex with the optimal vertex if */
/*     necessary. Then, if the initial simplex is not complete, pick its next */
/*     vertex and calculate the function values there. */

    if (jdrop <= *n) {
	if (datmat[mp + np * datmat_dim1] <= f) {
	    x[jdrop] = sim[jdrop + np * sim_dim1];
	} else {
	    sim[jdrop + np * sim_dim1] = x[jdrop];
	    i__1 = *mpp;
	    for (k = 1; k <= i__1; ++k) {
		datmat[k + jdrop * datmat_dim1] = datmat[k + np * datmat_dim1]
			;
/* L100: */
		datmat[k + np * datmat_dim1] = con[k];
	    }
	    i__1 = jdrop;
	    for (k = 1; k <= i__1; ++k) {
		sim[jdrop + k * sim_dim1] = -rho;
		temp = 0.f;
		i__2 = jdrop;
		for (i__ = k; i__ <= i__2; ++i__) {
/* L110: */
		    temp -= simi[i__ + k * simi_dim1];
		}
/* L120: */
		simi[jdrop + k * simi_dim1] = temp;
	    }
	}
    }
    if (nfvals <= *n) {
	jdrop = nfvals;
	x[jdrop] += rho;
	goto L40;
    }
L130:
    ibrnch = 1;

/*     Identify the optimal vertex of the current simplex. */

L140:
    phimin = datmat[mp + np * datmat_dim1] + parmu * datmat[*mpp + np * 
	    datmat_dim1];
    nbest = np;
    i__1 = *n;
    for (j = 1; j <= i__1; ++j) {
	temp = datmat[mp + j * datmat_dim1] + parmu * datmat[*mpp + j * 
		datmat_dim1];
	if (temp < phimin) {
	    nbest = j;
	    phimin = temp;
	} else if (temp == phimin && parmu == 0.f) {
	    if (datmat[*mpp + j * datmat_dim1] < datmat[*mpp + nbest * 
		    datmat_dim1]) {
		nbest = j;
	    }
	}
/* L150: */
    }

/*     Switch the best vertex into pole position if it is not there already, */
/*     and also update SIM, SIMI and DATMAT. */

    if (nbest <= *n) {
	i__1 = *mpp;
	for (i__ = 1; i__ <= i__1; ++i__) {
	    temp = datmat[i__ + np * datmat_dim1];
	    datmat[i__ + np * datmat_dim1] = datmat[i__ + nbest * datmat_dim1]
		    ;
/* L160: */
	    datmat[i__ + nbest * datmat_dim1] = temp;
	}
	i__1 = *n;
	for (i__ = 1; i__ <= i__1; ++i__) {
	    temp = sim[i__ + nbest * sim_dim1];
	    sim[i__ + nbest * sim_dim1] = 0.f;
	    sim[i__ + np * sim_dim1] += temp;
	    tempa = 0.f;
	    i__2 = *n;
	    for (k = 1; k <= i__2; ++k) {
		sim[i__ + k * sim_dim1] -= temp;
/* L170: */
		tempa -= simi[k + i__ * simi_dim1];
	    }
/* L180: */
	    simi[nbest + i__ * simi_dim1] = tempa;
	}
    }

/*     Make an error return if SIGI is a poor approximation to the inverse of */
/*     the leading N by N submatrix of SIG. */

    error = 0.f;
    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {
	i__2 = *n;
	for (j = 1; j <= i__2; ++j) {
	    temp = 0.f;
	    if (i__ == j) {
		temp += -1.f;
	    }
	    i__3 = *n;
	    for (k = 1; k <= i__3; ++k) {
/* L190: */
		temp += simi[i__ + k * simi_dim1] * sim[k + j * sim_dim1];
	    }
/* L200: */
/* Computing MAX */
	    d__1 = error, d__2 = abs(temp);
	    error = (real) max(d__1,d__2);
	}
    }
    if (error > .1f) {
	if (*iprint >= 1) {
	    s_wsfe(&io___28);
	    e_wsfe();
	}
	goto L600;
    }

/*     Calculate the coefficients of the linear approximations to the objective */
/*     and constraint functions, placing minus the objective function gradient */
/*     after the constraint gradients in the array A. The vector W is used for */
/*     working space. */

    i__2 = mp;
    for (k = 1; k <= i__2; ++k) {
	con[k] = -datmat[k + np * datmat_dim1];
	i__1 = *n;
	for (j = 1; j <= i__1; ++j) {
/* L220: */
	    w[j] = datmat[k + j * datmat_dim1] + con[k];
	}
	i__1 = *n;
	for (i__ = 1; i__ <= i__1; ++i__) {
	    temp = 0.f;
	    i__3 = *n;
	    for (j = 1; j <= i__3; ++j) {
/* L230: */
		temp += w[j] * simi[j + i__ * simi_dim1];
	    }
	    if (k == mp) {
		temp = -temp;
	    }
/* L240: */
	    a[i__ + k * a_dim1] = temp;
	}
    }

/*     Calculate the values of sigma and eta, and set IFLAG=0 if the current */
/*     simplex is not acceptable. */

    iflag = 1;
    parsig = alpha * rho;
    pareta = beta * rho;
    i__1 = *n;
    for (j = 1; j <= i__1; ++j) {
	wsig = 0.f;
	weta = 0.f;
	i__2 = *n;
	for (i__ = 1; i__ <= i__2; ++i__) {
/* Computing 2nd power */
	    d__1 = simi[j + i__ * simi_dim1];
	    wsig += d__1 * d__1;
/* L250: */
/* Computing 2nd power */
	    d__1 = sim[i__ + j * sim_dim1];
	    weta += d__1 * d__1;
	}
	vsig[j] = 1.f / sqrt(wsig);
	veta[j] = sqrt(weta);
	if (vsig[j] < parsig || veta[j] > pareta) {
	    iflag = 0;
	}
/* L260: */
    }

/*     If a new vertex is needed to improve acceptability, then decide which */
/*     vertex to drop from the simplex. */

    if (ibrnch == 1 || iflag == 1) {
	goto L370;
    }
    jdrop = 0;
    temp = pareta;
    i__1 = *n;
    for (j = 1; j <= i__1; ++j) {
	if (veta[j] > temp) {
	    jdrop = j;
	    temp = veta[j];
	}
/* L270: */
    }
    if (jdrop == 0) {
	i__1 = *n;
	for (j = 1; j <= i__1; ++j) {
	    if (vsig[j] < temp) {
		jdrop = j;
		temp = vsig[j];
	    }
/* L280: */
	}
    }

/*     Calculate the step to the new vertex and its sign. */

    temp = gamma * rho * vsig[jdrop];
    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {
/* L290: */
	dx[i__] = temp * simi[jdrop + i__ * simi_dim1];
    }
    cvmaxp = 0.f;
    cvmaxm = 0.f;
    i__1 = mp;
    for (k = 1; k <= i__1; ++k) {
	sum = 0.f;
	i__2 = *n;
	for (i__ = 1; i__ <= i__2; ++i__) {
/* L300: */
	    sum += a[i__ + k * a_dim1] * dx[i__];
	}
	if (k < mp) {
	    temp = datmat[k + np * datmat_dim1];
/* Computing MAX */
	    d__1 = cvmaxp, d__2 = -sum - temp;
	    cvmaxp = (real) max(d__1,d__2);
/* Computing MAX */
	    d__1 = cvmaxm, d__2 = sum - temp;
	    cvmaxm = (real) max(d__1,d__2);
	}
/* L310: */
    }
    dxsign = 1.f;
    if (parmu * (cvmaxp - cvmaxm) > sum + sum) {
	dxsign = -1.f;
    }

/*     Update the elements of SIM and SIMI, and set the next X. */

    temp = 0.f;
    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {
	dx[i__] = dxsign * dx[i__];
	sim[i__ + jdrop * sim_dim1] = dx[i__];
/* L320: */
	temp += simi[jdrop + i__ * simi_dim1] * dx[i__];
    }
    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {
/* L330: */
	simi[jdrop + i__ * simi_dim1] /= temp;
    }
    i__1 = *n;
    for (j = 1; j <= i__1; ++j) {
	if (j != jdrop) {
	    temp = 0.f;
	    i__2 = *n;
	    for (i__ = 1; i__ <= i__2; ++i__) {
/* L340: */
		temp += simi[j + i__ * simi_dim1] * dx[i__];
	    }
	    i__2 = *n;
	    for (i__ = 1; i__ <= i__2; ++i__) {
/* L350: */
		simi[j + i__ * simi_dim1] -= temp * simi[jdrop + i__ * 
			simi_dim1];
	    }
	}
/* L360: */
	x[j] = sim[j + np * sim_dim1] + dx[j];
    }
    goto L40;

/*     Calculate DX=x(*)-x(0). Branch if the length of DX is less than 0.5*RHO. */

L370:
    iz = 1;
    izdota = iz + *n * *n;
    ivmc = izdota + *n;
    isdirn = ivmc + mp;
    idxnew = isdirn + *n;
    ivmd = idxnew + *n;
    trstlp_(n, m, &a[a_offset], &con[1], &rho, &dx[1], &ifull, &iact[1], &w[
	    iz], &w[izdota], &w[ivmc], &w[isdirn], &w[idxnew], &w[ivmd]);
    if (ifull == 0) {
	temp = 0.f;
	i__1 = *n;
	for (i__ = 1; i__ <= i__1; ++i__) {
/* L380: */
/* Computing 2nd power */
	    d__1 = dx[i__];
	    temp += d__1 * d__1;
	}
	if (temp < rho * .25f * rho) {
	    ibrnch = 1;
	    goto L550;
	}
    }

/*     Predict the change to F and the new maximum constraint violation if the */
/*     variables are altered from x(0) to x(0)+DX. */

    resnew = 0.f;
    con[mp] = 0.f;
    i__1 = mp;
    for (k = 1; k <= i__1; ++k) {
	sum = con[k];
	i__2 = *n;
	for (i__ = 1; i__ <= i__2; ++i__) {
/* L390: */
	    sum -= a[i__ + k * a_dim1] * dx[i__];
	}
	if (k < mp) {
	    resnew = (real) max(resnew,sum);
	}
/* L400: */
    }

/*     Increase PARMU if necessary and branch back if this change alters the */
/*     optimal vertex. Otherwise PREREM and PREREC will be set to the predicted */
/*     reductions in the merit function and the maximum constraint violation */
/*     respectively. */

    barmu = 0.f;
    prerec = datmat[*mpp + np * datmat_dim1] - resnew;
    if (prerec > 0.f) {
	barmu = sum / prerec;
    }
    if (parmu < barmu * 1.5f) {
	parmu = barmu * 2.f;
	if (*iprint >= 2) {
	    s_wsfe(&io___48);
	    do_fio(&c__1, (char *)&parmu, (ftnlen)sizeof(doublereal));
	    e_wsfe();
	}
	phi = datmat[mp + np * datmat_dim1] + parmu * datmat[*mpp + np * 
		datmat_dim1];
	i__1 = *n;
	for (j = 1; j <= i__1; ++j) {
	    temp = datmat[mp + j * datmat_dim1] + parmu * datmat[*mpp + j * 
		    datmat_dim1];
	    if (temp < phi) {
		goto L140;
	    }
	    if (temp == phi && parmu == 0.f) {
		if (datmat[*mpp + j * datmat_dim1] < datmat[*mpp + np * 
			datmat_dim1]) {
		    goto L140;
		}
	    }
/* L420: */
	}
    }
    prerem = parmu * prerec - sum;

/*     Calculate the constraint and objective functions at x(*). Then find the */
/*     actual reduction in the merit function. */

    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {
/* L430: */
	x[i__] = sim[i__ + np * sim_dim1] + dx[i__];
    }
    ibrnch = 1;
    goto L40;
L440:
    vmold = datmat[mp + np * datmat_dim1] + parmu * datmat[*mpp + np * 
	    datmat_dim1];
    vmnew = f + parmu * resmax;
    trured = vmold - vmnew;
    if (parmu == 0.f && f == datmat[mp + np * datmat_dim1]) {
	prerem = prerec;
	trured = datmat[*mpp + np * datmat_dim1] - resmax;
    }

/*     Begin the operations that decide whether x(*) should replace one of the */
/*     vertices of the current simplex, the change being mandatory if TRURED is */
/*     positive. Firstly, JDROP is set to the index of the vertex that is to be */
/*     replaced. */

    ratio = 0.f;
    if (trured <= 0.f) {
	ratio = 1.f;
    }
    jdrop = 0;
    i__1 = *n;
    for (j = 1; j <= i__1; ++j) {
	temp = 0.f;
	i__2 = *n;
	for (i__ = 1; i__ <= i__2; ++i__) {
/* L450: */
	    temp += simi[j + i__ * simi_dim1] * dx[i__];
	}
	temp = abs(temp);
	if (temp > ratio) {
	    jdrop = j;
	    ratio = temp;
	}
/* L460: */
	sigbar[j] = temp * vsig[j];
    }

/*     Calculate the value of ell. */

    edgmax = delta * rho;
    l = 0;
    i__1 = *n;
    for (j = 1; j <= i__1; ++j) {
	if (sigbar[j] >= parsig || sigbar[j] >= vsig[j]) {
	    temp = veta[j];
	    if (trured > 0.f) {
		temp = 0.f;
		i__2 = *n;
		for (i__ = 1; i__ <= i__2; ++i__) {
/* L470: */
/* Computing 2nd power */
		    d__1 = dx[i__] - sim[i__ + j * sim_dim1];
		    temp += d__1 * d__1;
		}
		temp = sqrt(temp);
	    }
	    if (temp > edgmax) {
		l = j;
		edgmax = temp;
	    }
	}
/* L480: */
    }
    if (l > 0) {
	jdrop = l;
    }
    if (jdrop == 0) {
	goto L550;
    }

/*     Revise the simplex by updating the elements of SIM, SIMI and DATMAT. */

    temp = 0.f;
    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {
	sim[i__ + jdrop * sim_dim1] = dx[i__];
/* L490: */
	temp += simi[jdrop + i__ * simi_dim1] * dx[i__];
    }
    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {
/* L500: */
	simi[jdrop + i__ * simi_dim1] /= temp;
    }
    i__1 = *n;
    for (j = 1; j <= i__1; ++j) {
	if (j != jdrop) {
	    temp = 0.f;
	    i__2 = *n;
	    for (i__ = 1; i__ <= i__2; ++i__) {
/* L510: */
		temp += simi[j + i__ * simi_dim1] * dx[i__];
	    }
	    i__2 = *n;
	    for (i__ = 1; i__ <= i__2; ++i__) {
/* L520: */
		simi[j + i__ * simi_dim1] -= temp * simi[jdrop + i__ * 
			simi_dim1];
	    }
	}
/* L530: */
    }
    i__1 = *mpp;
    for (k = 1; k <= i__1; ++k) {
/* L540: */
	datmat[k + jdrop * datmat_dim1] = con[k];
    }

/*     Branch back for further iterations with the current RHO. */

    if (trured > 0.f && trured >= prerem * .1f) {
	goto L140;
    }
L550:
    if (iflag == 0) {
	ibrnch = 0;
	goto L140;
    }

/*     Otherwise reduce RHO if it is not at its least value and reset PARMU. */

    if (rho > *rhoend) {
	rho *= .5f;
	if (rho <= *rhoend * 1.5f) {
	    rho = *rhoend;
	}
	if (parmu > 0.f) {
	    denom = 0.f;
	    i__1 = mp;
	    for (k = 1; k <= i__1; ++k) {
		cmin = datmat[k + np * datmat_dim1];
		cmax = cmin;
		i__2 = *n;
		for (i__ = 1; i__ <= i__2; ++i__) {
/* Computing MIN */
		    d__1 = cmin, d__2 = datmat[k + i__ * datmat_dim1];
		    cmin = (real) min(d__1,d__2);
/* L560: */
/* Computing MAX */
		    d__1 = cmax, d__2 = datmat[k + i__ * datmat_dim1];
		    cmax = (real) max(d__1,d__2);
		}
		if (k <= *m && cmin < cmax * .5f) {
		    temp = (real) max(cmax,0.) - cmin;
		    if (denom <= 0.f) {
			denom = temp;
		    } else {
			denom = (real) min(denom,temp);
		    }
		}
/* L570: */
	    }
	    if (denom == 0.f) {
		parmu = 0.f;
	    } else if (cmax - cmin < parmu * denom) {
		parmu = (cmax - cmin) / denom;
	    }
	}
	if (*iprint >= 2) {
	    s_wsfe(&io___60);
	    do_fio(&c__1, (char *)&rho, (ftnlen)sizeof(doublereal));
	    do_fio(&c__1, (char *)&parmu, (ftnlen)sizeof(doublereal));
	    e_wsfe();
	}
	if (*iprint == 2) {
	    s_wsfe(&io___61);
	    do_fio(&c__1, (char *)&nfvals, (ftnlen)sizeof(integer));
	    do_fio(&c__1, (char *)&datmat[mp + np * datmat_dim1], (ftnlen)
		    sizeof(doublereal));
	    do_fio(&c__1, (char *)&datmat[*mpp + np * datmat_dim1], (ftnlen)
		    sizeof(doublereal));
	    i__1 = iptem;
	    for (i__ = 1; i__ <= i__1; ++i__) {
		do_fio(&c__1, (char *)&sim[i__ + np * sim_dim1], (ftnlen)
			sizeof(doublereal));
	    }
	    e_wsfe();
	    if (iptem < *n) {
		s_wsfe(&io___62);
		i__1 = *n;
		for (i__ = iptemp; i__ <= i__1; ++i__) {
		    do_fio(&c__1, (char *)&x[i__], (ftnlen)sizeof(doublereal))
			    ;
		}
		e_wsfe();
	    }
	}
	goto L140;
    }

/*     Return the best calculated values of the variables. */

    if (*iprint >= 1) {
	s_wsfe(&io___63);
	e_wsfe();
    }
    if (ifull == 1) {
	goto L620;
    }
L600:
    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {
/* L610: */
	x[i__] = sim[i__ + np * sim_dim1];
    }
    f = datmat[mp + np * datmat_dim1];
    resmax = datmat[*mpp + np * datmat_dim1];
L620:
    if (*iprint >= 1) {
	s_wsfe(&io___64);
	do_fio(&c__1, (char *)&nfvals, (ftnlen)sizeof(integer));
	do_fio(&c__1, (char *)&f, (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&resmax, (ftnlen)sizeof(doublereal));
	i__1 = iptem;
	for (i__ = 1; i__ <= i__1; ++i__) {
	    do_fio(&c__1, (char *)&x[i__], (ftnlen)sizeof(doublereal));
	}
	e_wsfe();
	if (iptem < *n) {
	    s_wsfe(&io___65);
	    i__1 = *n;
	    for (i__ = iptemp; i__ <= i__1; ++i__) {
		do_fio(&c__1, (char *)&x[i__], (ftnlen)sizeof(doublereal));
	    }
	    e_wsfe();
	}
    }
    *maxfun = nfvals;
    return 0;
} /* cobylb_ */

