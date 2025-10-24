(* $Id: FactorialResults.m3,v 1.1 2005/03/24 10:50:43 mika Exp $ *)

MODULE FactorialResults;
IMPORT Rd, FileRd, OSError;
IMPORT Thread;

<* FATAL Thread.Alerted *>

REVEAL
  T = Public BRANDED Brand OBJECT
  OVERRIDES
    init := Init;
  END;

PROCEDURE Init(t : T; directory, name : TEXT) : T RAISES { OSError.E,
                                                           Rd.Failure } =
  VAR 
    rd := FileRd.Open(directory & "/" & name);
  BEGIN
    TRY
      RETURN t.initFromRd(rd)
    FINALLY
      Rd.Close(rd)
    END
  END Init;

BEGIN END FactorialResults.
