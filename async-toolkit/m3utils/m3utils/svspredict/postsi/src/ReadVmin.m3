MODULE ReadVmin EXPORTS ReadVmin, ReadStats;
IMPORT Rd, Lex, FloatMode;
IMPORT Thread;
IMPORT LongRealSeq AS LRSeq;
IMPORT Math;
IMPORT Scan;
IMPORT LongrealArraySort AS LRSort;
IMPORT Debug;
FROM Fmt IMPORT Int, F;
IMPORT TextReader;

<*FATAL Thread.Alerted*>

PROCEDURE Read(rd             : Rd.T;
               Unit           : LONGREAL; 
               VAR n          : CARDINAL;
               VAR mean, sdev : LONGREAL;
               VAR title      : TEXT)
  RAISES { Lex.Error, FloatMode.Trap, Rd.Failure, TextReader.NoMore } =
  VAR
    seq := NEW(LRSeq.T).init();
    
  BEGIN
    ReadFile(rd, ARRAY OF LRSeq.T { seq }, title);
    VAR
      m, s : LONGREAL;
    BEGIN
      DoStats(seq, n, m, s);
      mean := m * Unit;
      sdev := s * Unit
    END
  END Read;

PROCEDURE ReadFile(rd            : Rd.T;
                   READONLY into : ARRAY OF LRSeq.T;
                   VAR title     : TEXT)
  RAISES { Lex.Error, FloatMode.Trap, Rd.Failure, TextReader.NoMore } =
  BEGIN
    TRY
      VAR
        line : TEXT;
        reader : TextReader.T;
        val : LONGREAL;
        lno := 0;
      BEGIN
        INC(lno);
        title := Rd.GetLine(rd);
        LOOP
          INC(lno);
          line := Rd.GetLine(rd);
          reader := NEW(TextReader.T).init(line);
          FOR i := FIRST(into) TO LAST(into) DO
            WITH seq = into[i] DO
              val  := Scan.LongReal(reader.nextE(","));
              seq.addhi(val);
            END
          END
        END
      END
    EXCEPT
      Rd.EndOfFile => (* ok *)
    END;
  END ReadFile;
  
  
PROCEDURE DoStats(seq            : LRSeq.T;
                  VAR n          : CARDINAL;
                  VAR mean, sdev : LONGREAL) =
  VAR
    arr := NEW(REF ARRAY OF LONGREAL, seq.size());
  BEGIN
    Debug.Out(F("seq.size() = %s", Int(seq.size())));
    n   := seq.size();

    FOR i := 0 TO seq.size() - 1 DO
      arr[i] := seq.get(i)
    END;
    
    LRSort.Sort(arr^); (* not needed, eh? *)

    VAR
      sumsq, sum := 0.00d0;
      nf         := FLOAT(NUMBER(arr^),LONGREAL);
      corr       := Math.sqrt(nf / ( nf - 1.0d0 ));
      var : LONGREAL;
    BEGIN
      FOR i := 0 TO NUMBER(arr^) - 1 DO
        sumsq := sumsq + arr[i] * arr[i];
        sum   := sum + arr[i]
      END;
      mean := sum / nf;
      var  := corr * ( sumsq / nf - (mean * mean) );
      sdev := Math.sqrt(var);
    END

  END DoStats;

BEGIN END ReadVmin.
