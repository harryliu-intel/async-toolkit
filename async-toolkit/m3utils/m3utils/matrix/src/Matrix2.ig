(* $Id$ *)

GENERIC INTERFACE Matrix2(Elem);
IMPORT Matrix;

CONST Brand = "Matrix2(" & Elem.Brand & ")";

TYPE Base = Elem.T;

TYPE M = ARRAY OF ARRAY OF Elem.T;
TYPE V = ARRAY OF          Elem.T;

PROCEDURE MulMM(READONLY a, b : M; VAR res : M);

PROCEDURE MulMV(READONLY a : M; READONLY v : V; VAR res : V);

PROCEDURE MeanM(READONLY m : M) : Elem.T;
PROCEDURE MeanSqM(READONLY m : M) : Elem.T;
PROCEDURE DevSqM(READONLY m : M) : Elem.T; (* sum of deviations from mean *)
PROCEDURE SumM(READONLY m : M) : Elem.T;
PROCEDURE SumSqM(READONLY m : M) : Elem.T;
PROCEDURE SumDiffSqM(READONLY m, n : M) : Elem.T; (* sum of sq. differences *)

PROCEDURE MeanV(READONLY v : V) : Elem.T;
PROCEDURE MeanSqV(READONLY v : V) : Elem.T;
PROCEDURE DevSqV(READONLY v : V) : Elem.T; (* sum of deviations from mean *)
PROCEDURE SumV(READONLY v : V) : Elem.T;
PROCEDURE SumSqV(READONLY v : V) : Elem.T;
PROCEDURE SumDiffSqV(READONLY m, n : V) : Elem.T; (* sum of sq. differences *)

PROCEDURE NewM(dims : Matrix.Dim) : REF M;

PROCEDURE GetDim(READONLY m : M) : Dim;

TYPE
  Dim = RECORD
    rows,cols : INTEGER;
  END;

PROCEDURE FormatM(READONLY m : M) : TEXT;
PROCEDURE FormatV(READONLY v : V) : TEXT;

(**********************************************************************)

PROCEDURE MulTransposeMM(READONLY a,b : M; VAR c : M);
  (* returns aTb in c *)

PROCEDURE AddToDiagonal(VAR m : M; a : Base);
  (* increase diagonal elements in-place *)

PROCEDURE Det(READONLY m : M) : Base;

PROCEDURE ExtractRowAsVector(READONLY m : M; r : CARDINAL; VAR res : V);

PROCEDURE SetCol(VAR m : M; c : CARDINAL; READONLY col : V);


END Matrix2.
