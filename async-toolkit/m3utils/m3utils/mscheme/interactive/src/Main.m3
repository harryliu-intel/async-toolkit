(* $Id$ *)

MODULE Main;
IMPORT Scheme, Params, Pathname;

BEGIN 

  WITH arr = NEW(REF ARRAY OF Pathname.T, Params.Count-1) DO
    FOR i := 1 TO Params.Count-1 DO arr[i-1] := Params.Get(i) END;
    WITH scm = NEW(Scheme.T).init(arr^) DO
      scm.readEvalWriteLoop()
    END
  END

END Main.
