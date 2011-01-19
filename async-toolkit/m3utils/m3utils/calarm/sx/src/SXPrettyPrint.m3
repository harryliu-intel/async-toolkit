(* $Id$ *)

MODULE SXPrettyPrint;
IMPORT Wr, SX, SXRoot, Thread;
IMPORT RTBrand;
IMPORT Debug;
IMPORT Fmt;

PROCEDURE Helper(wr : Wr.T; depth : CARDINAL; sx : SX.T) 
  RAISES { Thread.Alerted, Wr.Failure } =
  <*FATAL RTBrand.NotBranded*>
  VAR
    name  := Debug.UnNil(sx.getName());
    type  := SXRoot.TypeNames[sx.type()];
    brand := RTBrand.Get(sx);
    debug := sx.debugInfo();
  BEGIN
    FOR i := 1 TO depth DO
      Wr.PutChar(wr, '>');
    END;
    
    Wr.PutText(wr, Fmt.F("%s %s %s", name, type, brand, debug));
    Wr.PutChar(wr, '\n');

    VAR
      iter := sx.dependsOn();
      ssx : SX.T;
    BEGIN
      WHILE iter.next(ssx) DO
        Helper(wr, depth+1, ssx)
      END
    END
  END Helper;

PROCEDURE Put(wr : Wr.T; sx : SX.T) RAISES { Wr.Failure, Thread.Alerted } =
  BEGIN Helper(wr, 0, sx) END Put; 

BEGIN END SXPrettyPrint.
