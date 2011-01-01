(* $Id$ *)

UNSAFE MODULE UCTime;
IMPORT XTime AS Time;
IMPORT UtimeOpsC;
IMPORT M3toC;
FROM Ctypes IMPORT char_star, int_star;
IMPORT Text;

PROCEDURE ctime(clock : Time.T; keepNL, showTZ : BOOLEAN) : TEXT =
  VAR
    clockI := TRUNC(clock);
    buff : ARRAY [0..25] OF CHAR;
    tm := UtimeOpsC.make_T();
  BEGIN
    TRY
    WITH clockP = LOOPHOLE(ADR(clockI), int_star),
         
         ct = M3toC.CopyStoT(UtimeOpsC.ctime_r(clockP,
                                            LOOPHOLE(ADR(buff), char_star))),
         noNL = Text.Sub(ct, 0, Text.Length(ct)-1) DO
      IF showTZ THEN
        WITH locl = UtimeOpsC.localtime_r(clock, tm),
             tzName = M3toC.CopyStoT(UtimeOpsC.Get_zone(locl)) DO
          IF keepNL THEN
            RETURN noNL & " " & tzName & "\n"
          ELSE
            RETURN noNL & " " & tzName 
          END
        END
      ELSE
        IF keepNL THEN
          RETURN ct
        ELSE
          RETURN noNL
        END
      END
    END
    FINALLY
      UtimeOpsC.delete_T(tm)
    END
  END ctime;

BEGIN END UCTime.
