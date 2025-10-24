(* $Id: drdist.i3,v 1.4 2010/02/10 13:50:59 mika Exp $ *)

INTERFACE drdist;

(* from drdist.f -->(f2c)--> drdist.c *)

<* EXTERNAL fishin_ *>
PROCEDURE Fishin(VAR alpha : LONGREAL; VAR n1, n2 : INTEGER) : LONGREAL;
(* inverse F-dist *)

<* EXTERNAL fish_ *>
PROCEDURE Fish(VAR f : LONGREAL; VAR n1, n2 : INTEGER) : LONGREAL;
(* F-dist *)

<* EXTERNAL phinv_ *>
PROCEDURE Phinv(READONLY p : LONGREAL) : LONGREAL;
(* Inverse (cum.) normal *)

<* EXTERNAL studin_ *>
PROCEDURE Studin(VAR n : INTEGER; VAR p : LONGREAL) : LONGREAL;
(* Inverse Student's t *)

<* EXTERNAL phi_ *>
PROCEDURE Phi(READONLY x : LONGREAL) : LONGREAL;
(* (Cum.) Normal *)

<* EXTERNAL expan_ *>
PROCEDURE Expan(VAR n : INTEGER; VAR t : LONGREAL) : LONGREAL;
(* Student's t distribution *)

<* EXTERNAL pois_ *>
PROCEDURE Pois(VAR n : INTEGER; VAR xmu : LONGREAL) : LONGREAL;

<* EXTERNAL bin_ *>
PROCEDURE Bin(VAR n : INTEGER; VAR p : LONGREAL; VAR m : INTEGER) : LONGREAL;
  
<* EXTERNAL hytric_ *>
PROCEDURE Hytric(VAR k, n, n1, nr : INTEGER) : LONGREAL;

<* EXTERNAL fctrlg_ *>
PROCEDURE Fctrlg(VAR n : INTEGER) : LONGREAL;

<* EXTERNAL betinc_ *>
PROCEDURE Betinc(VAR x, a, b : LONGREAL) : LONGREAL;

<* EXTERNAL gamin_ *>
PROCEDURE Gamin(VAR x, a : LONGREAL) : LONGREAL;

<* EXTERNAL zap_ *>
PROCEDURE Zap(VAR h__ : LONGREAL) : LONGREAL;

<* EXTERNAL zot_ *>
PROCEDURE Zot(VAR n : INTEGER) : LONGREAL;

<* EXTERNAL chi2_ *>
PROCEDURE Chi2(VAR x, v : LONGREAL) : LONGREAL;
(* chi squared *)

<* EXTERNAL zip_ *>
PROCEDURE Zip(VAR a : LONGREAL) : LONGREAL;

<* EXTERNAL gamma_ *>
PROCEDURE Gamma(VAR alpha : LONGREAL; 
                (* OUT *) VAR expan, yexpan : LONGREAL) : INTEGER; (* retval 0 *)
END drdist.
