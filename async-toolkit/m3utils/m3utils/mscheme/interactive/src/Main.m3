(* $Id$ *)

(*
  Copyright (c) 2008, Generation Capital Ltd.  All rights reserved.

  Author: Mika Nystrom <mika@alum.mit.edu>
*)


MODULE Main;
IMPORT SchemeM3, Scheme, Params, Pathname, Csighandler;
IMPORT Debug, OSError, Wr, AL;

TYPE 
  Interrupter = Scheme.Interrupter OBJECT
  OVERRIDES
    interrupt := Interrupt;
  END;

PROCEDURE Interrupt(<*UNUSED*>i : Interrupter) : BOOLEAN =
  BEGIN
    IF Csighandler.have_signal() = 1 THEN 
      Csighandler.clear_signal();
      RETURN TRUE
    ELSE
      RETURN FALSE
    END
  END Interrupt;

BEGIN 
  Csighandler.install_int_handler();

  WITH arr = NEW(REF ARRAY OF Pathname.T, Params.Count) DO
    arr[0] := "require";
    FOR i := 1 TO Params.Count-1 DO arr[i] := Params.Get(i) END;
    TRY
      WITH scm = NEW(SchemeM3.T).init(arr^) DO
        scm.readEvalWriteLoop(NEW(Interrupter))
      END
    EXCEPT
      Scheme.E(err) =>
      Debug.Error("Couldn't initialize Scheme interpreter from files : " & err)
    |
      OSError.E (err) =>
      Debug.Error("Main: Couldn't initialize Scheme from files : OSError.E : "&
        AL.Format(err))
    |
      Wr.Failure (err) =>
      Debug.Error("Main: Couldn't initialize Scheme from files : Wr.Failure : "&
        AL.Format(err))
    END
  END

END Main.
