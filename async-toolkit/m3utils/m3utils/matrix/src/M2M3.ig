(* $Id: M2M3.ig,v 1.5 2008/02/04 00:10:46 mika Exp $ *)

GENERIC INTERFACE M2M3(M2);

PROCEDURE MulMV(READONLY a : M2.M; READONLY b : M2.V; VAR prod : M2.V);
PROCEDURE MulMC(READONLY a : M2.M; READONLY b : M2.M; VAR res : M2.V);
PROCEDURE MulMVC(READONLY a : M2.M; READONLY b : M2.V; VAR res : M2.M);

PROCEDURE IndexedDot(READONLY v : M2.V; 
                     READONLY idx : ARRAY OF CARDINAL;
                     READONLY w : M2.V) : M2.Base;

PROCEDURE Delta(READONLY v : M2.V; VAR d : M2.V);

PROCEDURE MulTransposeMM(READONLY a,b : M2.M; VAR prod : M2.M);

END M2M3.
