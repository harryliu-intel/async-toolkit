(* $Id$ *)

(*
  Copyright (c) 2008, Generation Capital Ltd.  All rights reserved.

  Author: Mika Nystrom <mika@alum.mit.edu>
*)


MODULE Main;
IMPORT Scheme, Params, Pathname, Csighandler;

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

  WITH arr = NEW(REF ARRAY OF Pathname.T, Params.Count-1) DO
    FOR i := 1 TO Params.Count-1 DO arr[i-1] := Params.Get(i) END;
    WITH scm = NEW(Scheme.T).init(arr^) DO
      scm.readEvalWriteLoop(NEW(Interrupter))
    END
  END

END Main.
