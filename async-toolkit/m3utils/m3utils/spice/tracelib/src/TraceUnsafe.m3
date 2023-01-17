UNSAFE MODULE TraceUnsafe;
IMPORT TraceHeader;
IMPORT Rd;
IMPORT Debug;
FROM Fmt IMPORT Int, F;
IMPORT Thread;
IMPORT UnsafeRd;
IMPORT Wx;

<*FATAL Thread.Alerted*>

PROCEDURE GetHeader(tRd : Rd.T; nNodes : CARDINAL) : TraceHeader.T
  RAISES { Rd.Failure, Rd.EndOfFile } =
  VAR
    h : TraceHeader.T;
    ibuff       := NEW(REF ARRAY OF CHAR, 4);
  BEGIN
    Rd.Seek(tRd, 0);

    IF Rd.GetSub(tRd, ibuff^) < 4 THEN RAISE Rd.EndOfFile END;
    Debug.Out("int=" & Int(LOOPHOLE(ibuff,REF ARRAY OF INTEGER)[0]));

    IF Rd.GetSub(tRd, ibuff^) < 4 THEN RAISE Rd.EndOfFile END;
    Debug.Out("int=" & Int(LOOPHOLE(ibuff,REF ARRAY OF INTEGER)[0]));

    IF Rd.GetSub(tRd, ibuff^) < 4 THEN RAISE Rd.EndOfFile END;
    Debug.Out("int=" & Int(LOOPHOLE(ibuff,REF ARRAY OF INTEGER)[0]));

    h.start  := Rd.Index(tRd);
    Rd.Seek(tRd, LAST(CARDINAL));
    h.end    := Rd.Index(tRd);
    h.nNodes := nNodes;
    
    WITH len  = h.end - h.start,
         samp = len DIV 4 DO
      h.steps := samp DIV nNodes;
      
      Debug.Out(F("start %s end %s len %s samples %s steps %s",
                  Int(h.start), Int(h.end), Int(len), Int(samp),
                  Int(h.steps)) & F(" rem %s", Int(samp MOD nNodes)))
    END;
    RETURN h
  END GetHeader;

PROCEDURE GetDataArray(tRd        : Rd.T;
                       READONLY h : TraceHeader.T;
                       id         : CARDINAL;
                       VAR arr    : ARRAY OF LONGREAL)
  RAISES { Rd.Failure, Rd.EndOfFile } =

  PROCEDURE GetLR(VAR x : LONGREAL) RAISES { Rd.Failure, Rd.EndOfFile } =
    BEGIN
      WITH got = Rd.GetSub(tRd, buff^) DO
        <*ASSERT got <= 4*>

        IF got < 4 THEN RAISE Rd.EndOfFile END;
        
        WITH r = LOOPHOLE(buff, REF ARRAY OF REAL) DO
          x := FLOAT(r[0],LONGREAL)
        END
      END
    END GetLR;

  VAR
    buff        := NEW(REF ARRAY OF CHAR, 4);
    (* why can't this be stack-allocated? *)

  BEGIN
    Rd.Seek(tRd, 12 + 4 * h.steps * id);
    WITH lastValid = MIN(LAST(arr), h.steps - 1) DO
      FOR i := FIRST(arr) TO lastValid DO
        GetLR(arr[i])
      END;
      FOR i := lastValid + 1 TO LAST(arr) DO
        arr[i] := 0.0d0
      END
    END
  END GetDataArray;

PROCEDURE GetBytes(rd : Rd.T; bytes : CARDINAL) : TEXT
  RAISES { Rd.Failure, Rd.EndOfFile } =
  VAR
    wx := Wx.New();
  BEGIN
    LOCK rd DO
      FOR i := 0 TO bytes - 1 DO
        WITH c = UnsafeRd.FastGetChar(rd) DO
          Wx.PutChar(wx, c)
        END
      END;
      RETURN Wx.ToText(wx)
    END
  END GetBytes;

BEGIN END TraceUnsafe.
