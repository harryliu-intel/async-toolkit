(* Copyright (c) 2000, 2008 Mika Nystrom.  All Rights Reserved. *)
(* $Id$ *)
GENERIC MODULE LU2(M, M3, F);
FROM Matrix IMPORT Singular;
IMPORT Env;

(* there has got to be a better way to code these things *)
PROCEDURE DecomposeR(VAR m : M.M; 
                     VAR vv : M.V;
                     indx : REF ARRAY OF INTEGER; 
                     VAR d : M.Base) RAISES { Singular } =

  (* LU decomposition of a square matrix by Crout's method *)
  (* Numerical Recipes in FORTRAN 2nd ed. pp. 38--39 *)
  (* numbers in comments refer to ENDDO statements in the book *)

  (* MODIFIES m , indx , d *)
  (* ENSURES m' contains LU decomposition of m *)

  CONST
    Tiny = FLOAT(1.0d-10,M.Base);
  VAR
    imax :=  -1; (* make it something that will crash if wrong *) 
    last := LAST(m);
  BEGIN
    d := FLOAT(1,M.Base);

    <* ASSERT FIRST(m) = 0 AND 
              FIRST(m) = FIRST(m[0]) AND 
              LAST(m) = LAST(m[0]) *>

    FOR row := 0 TO last DO VAR aamax := FLOAT(0,M.Base); BEGIN
      (* find largest element on row *)
      FOR col := 0 TO last DO aamax := MAX(aamax, ABS(m[row,col])) END;
      IF aamax = FLOAT(0,M.Base) THEN RAISE Singular(row) END;
      vv[row] := FLOAT(1,M.Base) / aamax
    END END; (* FOR row *)

    (* loop over cols *)
    FOR col := 0 TO last DO

      FOR row := 0 TO col - 1 DO VAR sum := m[row,col]; BEGIN
        FOR k:= 0 TO row - 1 DO sum := sum - m[row,k] * m[k,col] END;
        m[row,col] := sum
      END END; (* 14 *)

      VAR aamax := FLOAT(0,M.Base); BEGIN
        FOR row := col TO last DO
          VAR
            sum := m[row,col];
          BEGIN
            FOR k:= 0 TO col - 1 DO sum := sum - m[row,k] * m[k,col] END;
            m[row,col] := sum;
            (*<* ASSERT vv[row] >= FLOAT(0,M.Base) *>*)
            WITH dum = vv[row] * ABS(sum) DO IF dum >= aamax THEN
              imax := row;
              aamax := dum
            END END
          END
        END (* 16 *)
      END;

      IF col # imax THEN
        FOR k:= 0 TO last DO VAR dum := m[imax,k]; BEGIN
          m[imax,k] := m[col,k]; m[col,k] := dum 
        END END; (* 17 *)
        d := -d;
        vv[imax] := vv[col]
      END; (* IF *)
      indx[col] := imax;
      IF m[col,col] = FLOAT(0,M.Base) THEN m[col,col] := Tiny END; 
      IF col # last THEN VAR dum := FLOAT(1,M.Base) / m[col,col]; BEGIN
          FOR row:=col + 1 TO last DO m[row,col] := m[row,col] * dum END
      END END
    END (* FOR col *) (* 19 *)
  END DecomposeR;

PROCEDURE BackSubstitute(READONLY m : M.M; 
                         READONLY indx : REF ARRAY OF INTEGER; 
                         VAR b : M.V) =
  BEGIN
    IF UseFortran THEN
      F.BackSubstitute(m,indx,b)
    ELSE
      M3.BackSubstitute(m,indx,b)
    END
  END BackSubstitute;

VAR UseFortran := Env.Get("FORTRANMATH") # NIL;

BEGIN END LU2.




