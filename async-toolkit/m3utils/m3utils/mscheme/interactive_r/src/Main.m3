(* $Id$ *)

(*
  Copyright (c) 2008, Generation Capital Ltd.  All rights reserved.

  Author: Mika Nystrom <mika@alum.mit.edu>
*)


MODULE Main;
IMPORT Pathname, Params, Scheme, Debug, OSError, ReadLineError, NetObj;
IMPORT AL, IP, ReadLine;
FROM SchemeReadLine IMPORT MainLoop;
IMPORT Thread;

<*FATAL Thread.Alerted*>

BEGIN 

  WITH arr = NEW(REF ARRAY OF Pathname.T, Params.Count-1) DO
    FOR i := 1 TO Params.Count-1 DO arr[i-1] := Params.Get(i) END;
    TRY
      WITH scm = NEW(Scheme.T).init(arr^) DO
        MainLoop(NEW(ReadLine.T).init(), scm)
      END
    EXCEPT
      Scheme.E(err) => Debug.Error("Caught Scheme.E : " & err)
    |
      IP.Error(err) => Debug.Error("Caught IP.Error : " & AL.Format(err))
    |
      OSError.E(err) => 
        Debug.Error("Caught NetObj.Error : " & AL.Format(err))
    |
      ReadLineError.E(err) => 
        Debug.Error("Caught ReadLineError.E : " & AL.Format(err))
    |
      NetObj.Error(err) => Debug.Error("Caught NetObj.Error : " & 
                                        AL.Format(err))
    END
  END

END Main.
