MODULE Main;
IMPORT ParseParams, Fmt, Scan;
IMPORT Time;
IMPORT Rd, Wr, FileRd, FileWr;
IMPORT FS;
IMPORT Math;
IMPORT IO;
FROM Fmt IMPORT F;
IMPORT Stdio;
IMPORT LongRealSeq;
IMPORT OSError, Thread;
IMPORT FloatMode, Lex;
IMPORT Pathname;
IMPORT LongrealArraySort;

<*FATAL Thread.Alerted, OSError.E, Wr.Failure, Rd.Failure*>
<*FATAL Lex.Error, FloatMode.Trap*>
<*FATAL Rd.EndOfFile*>

CONST Dir = "dir";
CONST N   = 1000;

PROCEDURE Write() =
  BEGIN
    FOR i := 0 TO N-1 DO
      WITH fn = Dir & "/" & Fmt.Pad(Fmt.Int(i), length := 8, padChar := '0'),
           wr = FileWr.Open(fn) DO
        Wr.PutText(wr, Fmt.LongReal(Time.Now()));
        Wr.PutChar(wr, '\n');
        Wr.Close(wr);

        Thread.Pause(0.001d0)
      END
    END
  END Write;

PROCEDURE Read() = 
  VAR
    n := 0;
    seq := NEW(LongRealSeq.T).init();
    p : Pathname.T;
    rd : Rd.T;
  BEGIN
    WHILE n < N DO
      WITH iter = FS.Iterate(Dir) DO
        WHILE iter.next(p) DO
          TRY
            WITH fn  = Dir & "/" & p DO
              rd := FileRd.Open(fn);
              WITH ln  = Rd.GetLine(rd),
                   tm  = Scan.LongReal(ln),
                   now = Time.Now() DO
                seq.addhi(now-tm);
                Rd.Close(rd);
                FS.DeleteFile(fn);
                INC(n)
              END
            END
          EXCEPT
            Rd.EndOfFile => 
            (* try again later *)
            Rd.Close(rd)
          END
        END
      END
    END;

    VAR
      min   := LAST (LONGREAL);
      max   := FIRST(LONGREAL);
      sum   := 0.0d0;
      sumSq := 0.0d0;
      arr   := NEW(REF ARRAY OF LONGREAL, seq.size());
    BEGIN
      FOR i := 0 TO seq.size()-1 DO
        arr[i] := seq.get(i)
      END;
      
      LongrealArraySort.Sort(arr^);
      
      FOR i := FIRST(arr^) TO LAST(arr^) DO
        WITH e = arr[i] DO
          min := MIN(min, e);
          max := MAX(max, e);
          sum := sum + e;
          sumSq := sumSq + e*e
        END
      END;

      WITH nn = FLOAT(N, LONGREAL),
           mean = sum / nn,
           var  = (nn/(nn-1.0d0)) * (sumSq / nn - mean * mean),
           sdev = Math.sqrt(var) DO
        IO.Put(F("min = %s\n", Fmt.LongReal(min)));
        IO.Put(F("max = %s\n", Fmt.LongReal(max)));
        IO.Put(F("mean = %s\n", Fmt.LongReal(mean)));
        IO.Put(F("var = %s\n", Fmt.LongReal(var)));
        IO.Put(F("sdev = %s\n", Fmt.LongReal(sdev)))
      END;

      FOR i := 0 TO 100 BY 10 DO
        IO.Put(F("percentile %5s = %s\n", 
                 Fmt.Int(i), 
                 Fmt.LongReal(Percentile(arr^, FLOAT(i,LONGREAL)))))
      END
    END
  END Read;


PROCEDURE Percentile(READONLY a : ARRAY OF LONGREAL;
                     pct : LONGREAL) : LONGREAL =
  VAR
    r := pct / 100.0d0;
    z := FLOAT(NUMBER(a), LONGREAL) * r;
  BEGIN
    IF    z >= FLOAT(LAST(a), LONGREAL) THEN
      RETURN a[LAST(a)] 
    ELSIF z <= FLOAT(FIRST(a), LONGREAL) THEN
      RETURN a[FIRST(a)]
    ELSE
      WITH lo  = FLOOR(z),
           hi  = CEILING(z),
           mix = z - FLOAT(lo, LONGREAL),
           v   = (1.0d0-mix) * a[lo] + mix * a[hi] DO
        RETURN v
      END
    END
  END Percentile;

BEGIN
  WITH pp = NEW(ParseParams.T).init(Stdio.stderr) DO
    IF pp.keywordPresent("-read") THEN
      Read()
    ELSIF pp.keywordPresent("-write") THEN
      Write()
    END;
    pp.skipParsed();
    pp.finish()
  END
END Main.
