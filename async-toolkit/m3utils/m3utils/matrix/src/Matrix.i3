(* Copyright (c) 2000 Mika Nystrom.  All Rights Reserved. *)
(* $Id$ *)
INTERFACE Matrix;
IMPORT LRVector;

EXCEPTION
  DimensionMismatch;
  NotSquare;
  Singular(CARDINAL);  (* arg is index of bad func. *)

TYPE 
  T = REF ARRAY OF ARRAY OF LONGREAL;
  Vector = LRVector.T;
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
PROCEDURE Trace(a : T): LONGREAL       RAISES { NotSquare };

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
PROCEDURE FormatVector(v : Vector) : TEXT;

PROCEDURE Transpose(a : T): T;

(* Decompose an LU matrix *)
PROCEDURE U(m : T) : T;
PROCEDURE L(m : T) : T;

(* Zap small entries *)
PROCEDURE Zap(m : T; threshold : LONGREAL ) : T;

(* extract row, col *)
PROCEDURE ExtractRowAsVector(m : T; r : CARDINAL) : Vector;
PROCEDURE ExtractColAsVector(m : T; c : CARDINAL) : Vector;

PROCEDURE ExtractRow(m : T; r : CARDINAL) : T;
PROCEDURE ExtractCol(m : T; c : CARDINAL) : T;

PROCEDURE SetCol(m : T; c : CARDINAL; col : Vector);

(* mean and variance of all the matrix elements... *)
PROCEDURE Mean(m : T) : LONGREAL;
PROCEDURE MeanSq(m : T) : LONGREAL;
PROCEDURE DevSq(m : T) : LONGREAL; (* sum of deviations from mean *)

END Matrix.





