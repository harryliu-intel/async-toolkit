(* $Id$ *)

GENERIC MODULE M2M3(M2);

PROCEDURE MulMV(READONLY a : M2.M; READONLY b : M2.V; VAR prod : M2.V) =
  BEGIN
    WITH aRows = NUMBER(a),
         aCols = NUMBER(a[0]) DO
      FOR row:= 0 TO aRows - 1 DO
        prod[row] := FLOAT(0,M2.Base);
        FOR term := 0 TO aCols - 1 DO
          prod[row] := prod[row] + a[row,term] * b[term]
        END
      END
    END
  END MulMV;

PROCEDURE MulMC(READONLY a : M2.M; READONLY b : M2.M; VAR prod : M2.V) =
  BEGIN
    WITH aRows = NUMBER(a),
         aCols = NUMBER(a[0]) DO
      FOR row:= 0 TO aRows - 1 DO
        prod[row] := FLOAT(0,M2.Base);
        FOR term := 0 TO aCols - 1 DO
          prod[row] := prod[row] + a[row,term] * b[term,0]
        END
      END
    END
  END MulMC;

PROCEDURE MulMVC(READONLY a : M2.M; READONLY b : M2.V; VAR prod : M2.M) =
  BEGIN
    WITH aRows = NUMBER(a),
         aCols = NUMBER(a[0]) DO
      FOR row:= 0 TO aRows - 1 DO
        prod[row,0] := FLOAT(0,M2.Base);
        FOR term := 0 TO aCols - 1 DO
          prod[row,0] := prod[row,0] + a[row,term] * b[term]
        END
      END
    END
  END MulMVC;

BEGIN END M2M3.
