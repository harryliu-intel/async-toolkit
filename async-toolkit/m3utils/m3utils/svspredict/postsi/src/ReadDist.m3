MODULE ReadDist;
IMPORT Rd, Lex, FloatMode;
IMPORT Thread;
IMPORT LongRealSeq AS LRSeq;
IMPORT Math;
IMPORT Scan;
IMPORT LongrealArraySort AS LRSort;

<*FATAL Thread.Alerted*>

PROCEDURE Read(rd             : Rd.T;
               Unit           : LONGREAL; 
               VAR n          : CARDINAL;
               VAR mean, sdev : LONGREAL;
               VAR title      : TEXT)
  RAISES { Lex.Error, FloatMode.Trap } =
  VAR
    seq := NEW(LRSeq.T).init();
    arr : REF ARRAY OF LONGREAL;
    
  BEGIN
    TRY
      VAR
        line : TEXT;
        val : LONGREAL;
        lno := 0;
      BEGIN
        INC(lno);
        title := Rd.GetLine(rd);
        LOOP
          INC(lno);
          line := Rd.GetLine(rd);
          val  := Scan.LongReal(line) * Unit;
          seq.addhi(val);
        END
      END
    EXCEPT
      Rd.EndOfFile => (* ok *)
    END;

    arr := NEW(REF ARRAY OF LONGREAL, seq.size());
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
  END Read;

BEGIN END ReadDist.
