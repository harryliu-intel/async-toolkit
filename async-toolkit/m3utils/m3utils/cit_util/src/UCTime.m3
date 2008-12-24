(* $Id$ *)

UNSAFE MODULE UCTime;
IMPORT Time;
IMPORT UtimeR;
IMPORT M3toC;
FROM Ctypes IMPORT char_star, long_star;
FROM Utime IMPORT struct_tm;
IMPORT Text;

PROCEDURE ctime(clock : Time.T; keepNL, showTZ : BOOLEAN) : TEXT =
  VAR
    clockI := TRUNC(clock);
    buff : ARRAY [0..25] OF CHAR;
    tm : struct_tm;
  BEGIN
    WITH clockP = LOOPHOLE(ADR(clockI), long_star),
         
         ct = M3toC.CopyStoT(UtimeR.ctime_r(clockP,
                                            LOOPHOLE(ADR(buff), char_star))),
         noNL = Text.Sub(ct, 0, Text.Length(ct)-1) DO
      IF showTZ THEN
        WITH locl = UtimeR.localtime_r(clockP, ADR(tm)),
             tzName = M3toC.CopyStoT(locl.tm_zone) DO
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
  END ctime;

BEGIN END UCTime.
