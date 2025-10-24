MODULE PolySegment16Serial;
IMPORT PolySegment16Seq AS Seq;
IMPORT Rd, Wr;
IMPORT Rep16Stream;
IMPORT PolySegment16;
IMPORT Rep16;
FROM Fmt IMPORT F, Int, FN, Bool;
IMPORT Thread;
IMPORT Debug;

VAR DoDebug := Debug.DebugThis("PolySegment16Serial");
    
PROCEDURE Write(wr : Wr.T; seq : Seq.T; min, max : LONGREAL)
  RAISES { Wr.Failure, Thread.Alerted } =
  VAR
    bytes := 0;
    header : Rep16.Header;
  BEGIN
    FOR i := 0 TO seq.size() - 1 DO
      bytes := bytes + Rep16Stream.Bytes(seq.get(i).r)
    END;

    <*ASSERT bytes MOD 2 = 0*>
    header.nwords := bytes DIV 2;
    WITH last = seq.get(seq.size() - 1) DO
      header.npoints := last.lo + last.n
    END;

    header.min := min;
    header.max := max;

    Rep16Stream.WriteHeader(wr, header);

    IF DoDebug THEN
      Debug.Out(F("PolySegment16Serial.Write : header %s",
                  Rep16.FormatHeader(header)))
    END;
    
    FOR i := 0 TO seq.size() - 1 DO
      WITH cur = seq.get(i) DO
        Rep16Stream.WriteT(wr, cur.r)
      END
    END
  END Write;

PROCEDURE Read(rd : Rd.T; seq : Seq.T; VAR header : Rep16.Header)
  RAISES { Rd.Failure, Rd.EndOfFile, Error, Thread.Alerted } =
  VAR
    bytes : CARDINAL := 0;
    seg : PolySegment16.T;
    hi := -1; (* last index that was read *)
  BEGIN
    EVAL Rep16Stream.ReadHeader(rd, header);

    IF DoDebug THEN
      Debug.Out(F("PolySegment16Serial.Read : header %s",
                  Rep16.FormatHeader(header)))
    END;
    
    WHILE bytes DIV 2 < header.nwords DO
      bytes := bytes + Rep16Stream.ReadT(rd, seg.r);
      IF seg.r.order = 0 OR seg.r.reset THEN
        seg.lo := hi + 1;
        hi     := hi + seg.r.count
      ELSE
        seg.lo := hi;
        hi     := hi + seg.r.count - 1
        (* if order of current seg is not 0, and we don't reset,
           there is one point overlap w/ previous *)
      END;
      seg.n := seg.r.count;
      seq.addhi(seg);

      IF DoDebug THEN
        Debug.Out(FN("PolySegment16Serial.Read : seg[%s] : lo %s n %s r { count %s order %s reset %s }",
                     ARRAY OF TEXT { Int(seq.size() - 1),
                                     Int(seg.lo),
                                     Int(seg.n),
                                     Int(seg.r.count),
                                     Int(seg.r.order),
                                     Bool(seg.r.reset) }))
      END
    END;

    IF bytes DIV 2 # header.nwords THEN
      RAISE Error(F("bytes (%s) DIV 2 # header.nwords (%s)",
                    Int(bytes), Int(header.nwords)))
    END;

    IF hi # header.npoints - 1 THEN
      RAISE Error(F("hi (%s) # header.npoints - 1 (%s)",
                    Int(hi),
                    Int(header.npoints - 1)))
    END

  END Read;

BEGIN END PolySegment16Serial.
