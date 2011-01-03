(* $Id$ *)

MODULE Main;
IMPORT SXInt;
IMPORT Wr, Stdio, SXSelect, SX, Thread, Random;

TYPE
  Reader = Thread.Closure OBJECT
    sx : SXInt.T;
  OVERRIDES
    apply := RApply;
  END;

PROCEDURE RApply(r : Reader) : REFANY = 
  BEGIN
    SX.Lock1(r.sx);
    LOOP
      SXSelect.Wait1(r.sx);
      WITH val = r.sx.value() DO
        IF val = 0 THEN
          Wr.PutChar(Stdio.stdout, '.');
          Wr.PutChar(Stdio.stdout, '\n')
        ELSIF val > 9 THEN 
          Wr.PutChar(Stdio.stdout, 'X')
        ELSE
          Wr.PutChar(Stdio.stdout, VAL(ORD('0')+val,CHAR))
        END
      END;
      Wr.Flush(Stdio.stdout)
    END
  END RApply;

VAR
  sx : SXInt.Var := NEW(SXInt.Var).init();
  rand := NEW(Random.Default).init();
BEGIN
  EVAL Thread.Fork(NEW(Reader, sx := sx));
  LOOP
    WITH num = rand.integer(1,10000) DO
      FOR i := 1 TO num DO
        sx.set(i)
      END;
      sx.set(0);
      Thread.Pause(5.0d0)
    END
  END
END Main.
