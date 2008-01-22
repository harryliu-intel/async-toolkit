GENERIC MODULE M2M3(M2);

PROCEDURE MulMV(READONLY a : M2.M; READONLY b : M2.V; VAR prod : M2.V) =
  BEGIN
    WITH aRows = NUMBER(a),
         aCols = NUMBER(a[0]) DO
      FOR row:= 0 TO aRows - 1 DO
        FOR term := 0 TO aCols - 1 DO
          prod[row] := prod[row] + a[row,term] * b[term];
        END
      END
    END
  END MulMV;

BEGIN END M2M3.
