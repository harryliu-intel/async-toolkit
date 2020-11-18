MODULE StdfRd;
IMPORT Rd;
IMPORT StdfE;
IMPORT Thread;
IMPORT Debug;
FROM Fmt IMPORT Int, F, Pad, Align;

VAR doDebug := TRUE;
    
PROCEDURE Chars(rd : Rd.T; VAR len : CARDINAL; VAR x : ARRAY OF CHAR)
  RAISES { StdfE.E, Thread.Alerted, Rd.Failure, Rd.EndOfFile } =
  BEGIN
    WITH attempt = MIN(len, NUMBER(x)),
         got     = Rd.GetSub(rd, SUBARRAY(x, 0, attempt)) DO
      DEC(len, got);

      IF doDebug THEN
        VAR
          str := "";
        BEGIN
          FOR i := 0 TO got-1 DO
            str := str & Pad(Int(ORD(x[i]), base := 16),2,padChar:='0',align := Align.Right) & " "
          END;
          Debug.Out(F("StdfRd.Chars idx %s len %s bufsiz %s got %s str %s",
                      Int(Rd.Index(rd)),
                      Int(len), Int(NUMBER(x)), Int(got), str))
        END
      END;
        
      IF attempt < NUMBER(x) THEN
        RAISE StdfE.E("short read")
      END;

      IF got < attempt THEN RAISE Rd.EndOfFile END;
    END
  END Chars;

PROCEDURE Char(rd : Rd.T; VAR len : CARDINAL) : CHAR
  RAISES { StdfE.E, Thread.Alerted, Rd.Failure, Rd.EndOfFile } =
  BEGIN
    IF len < 1 THEN
      IF doDebug THEN
        Debug.Out(F("StdfRd.Char idx %s len %s short read",
                    Int(Rd.Index(rd)),
                    Int(len)))
      END;
      RAISE StdfE.E("short read")
    END;
    
    WITH c = Rd.GetChar(rd) DO
      IF doDebug THEN
        Debug.Out(F("StdfRd.Char idx %s len %s char %02s",
                    Int(Rd.Index(rd)),
                    Int(len),
                    Int(ORD(c),base:=16)))
      END;
      DEC(len);
      RETURN c
    END
  END Char;

PROCEDURE U1(rd : Rd.T; VAR len : CARDINAL) : [0..255]
  RAISES { StdfE.E, Thread.Alerted, Rd.Failure, Rd.EndOfFile } =
  BEGIN
    RETURN ORD(Char(rd, len))
  END U1;

PROCEDURE U2(rd : Rd.T; VAR len : CARDINAL) : [0..65535]
  RAISES { StdfE.E, Thread.Alerted, Rd.Failure, Rd.EndOfFile } =
  VAR
    res : [0..65535] := 0;
  BEGIN
    res := U1(rd, len);
    res := res + 256 * U1(rd,len);
    RETURN res
  END U2;

BEGIN END StdfRd.

