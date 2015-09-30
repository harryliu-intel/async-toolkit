(* $Id: CITArgs.m3,v 1.1 2009/04/01 07:16:26 mika Exp $ *)

MODULE CITArgs;
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
END CITArgs.
