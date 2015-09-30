MODULE Args;
IMPORT Params;

PROCEDURE CommandLine(): REF T =
  VAR
    result := NEW(REF T, Params.Count-1);
  BEGIN
    FOR i := 0 TO LAST(result^) DO
      result[i] := Params.Get(i+1);
    END;
    RETURN result;
  END CommandLine;

BEGIN
END Args.
