(* Copyright (c) 2000 Mika Nystrom.  All Rights Reserved. *)
(* $Id$ *)
INTERFACE Matrix;

EXCEPTION
  DimensionMismatch;
  NotSquare;
  Singular(CARDINAL);  (* arg is index of bad func. *)

TYPE 
  T = REF ARRAY OF ARRAY OF LONGREAL;
  Vector = REF ARRAY OF LONGREAL;
  Dim = RECORD
    rows,cols : INTEGER;
  END;
    

(* make matrix from vector *)
PROCEDURE ColVector(vector : Vector) : T;
PROCEDURE RowVector(vector : Vector) : T;

(* standard matrix ops *)
PROCEDURE Add(a,b : T): T            RAISES { DimensionMismatch };
PROCEDURE Sub(a,b : T): T            RAISES { DimensionMismatch };
PROCEDURE Mul(a,b : T): T            RAISES { DimensionMismatch };
PROCEDURE Scale(a : LONGREAL; m : T) : T;
PROCEDURE Det(a : T): LONGREAL       RAISES { NotSquare };

(* return a unit matrix of the specified size *)
PROCEDURE Unit(dim : Dim) : T RAISES { NotSquare } ;
(* a zero matrix *)
PROCEDURE Zero(dim : Dim) : T;
(* just a matrix (uninitialized) *)
PROCEDURE New(dim : Dim) : T;

(* get dimensions of existing matrix *)
(* if rows is zero, cols is meaningless *)
PROCEDURE GetDim(m : T) : Dim;

(* format a matrix for printing *)
PROCEDURE Format(m : T) : TEXT;

PROCEDURE Transpose(a : T): T;

(* Decompose an LU matrix *)
PROCEDURE U(m : T) : T;
PROCEDURE L(m : T) : T;

(* Zap small entries *)
PROCEDURE Zap(m : T; threshold : LONGREAL ) : T;


END Matrix.
