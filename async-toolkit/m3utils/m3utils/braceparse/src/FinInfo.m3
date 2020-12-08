MODULE FinInfo;

PROCEDURE Add(READONLY a, b : T) : T =
  VAR
    res : T;
  BEGIN
    FOR i := FIRST(res) TO LAST(res) DO
      res[i] := a[i] + b[i]
    END;
    RETURN res
  END Add;

BEGIN END FinInfo.
    
