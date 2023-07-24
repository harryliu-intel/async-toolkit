MODULE Main;

(*
  Compute the max of a randomly selected subset of values as a function
  of the size of the subset.  Average over a number of samples of 
  each size.
  
  Computes also the max of the reduced random variable, w.r.t. the 
  sample mean and standard deviation.  No correction term applied.
  
  In gnuplot use logscale x, or use stats paper.  (?)

*)

   

IMPORT Stdio;
IMPORT Rd;
IMPORT LongRealSeq AS LRSeq;
IMPORT LongrealArraySort AS LRArraySort;
IMPORT Scan;
IMPORT Random;
IMPORT Debug;
FROM Fmt IMPORT Int, F, LongReal;
IMPORT Wr;
IMPORT Math;

CONST LR = LongReal;


VAR
  rand := NEW(Random.Default).init();
  
PROCEDURE SelectRandom(READONLY a : ARRAY OF LONGREAL;
                       VAR      b : ARRAY OF LONGREAL) =
  (* random selection WITH replacement *)
  BEGIN
    FOR i := FIRST(b) TO LAST(b) DO
      WITH idx = rand.integer(FIRST(a), LAST(a)) DO
        b[i] := a[idx]
      END
    END
  END SelectRandom;
  
PROCEDURE EvalMax(READONLY a : ARRAY OF LONGREAL; s : CARDINAL) : LONGREAL =
  VAR
    scratch := NEW(REF ARRAY OF LONGREAL, s);
  BEGIN
    IF s = NUMBER(a) THEN
      scratch^ := a;
      LRArraySort.Sort(scratch^);
      RETURN scratch[LAST(scratch^)]
    ELSE
      CONST
        Samples = 1000;
      VAR
        sumres := 0.0d0;
      BEGIN
        FOR i := 0 TO Samples - 1 DO
          SelectRandom(a, scratch^);
          LRArraySort.Sort(scratch^);
          sumres := sumres + scratch[LAST(scratch^)]
        END;
        RETURN sumres / FLOAT(Samples, LONGREAL)
      END
    END
  END EvalMax;
  
VAR
  rd  := Stdio.stdin;
  seq := NEW(LRSeq.T).init();
  mean, sdev : LONGREAL;
  
BEGIN
  TRY
    LOOP
      WITH line = Rd.GetLine(rd),
           lr   = Scan.LongReal(line) DO
        seq.addhi(lr)
      END
    END
  EXCEPT
    Rd.EndOfFile => Rd.Close(rd)
  END;

  WITH a = NEW(REF ARRAY OF LONGREAL, seq.size()) DO
    FOR i := FIRST(a^) TO LAST(a^) DO
      a[i] := seq.get(i)
    END;

    (* a contains all the input .... *)
    Debug.Out(F("array contains %s points", Int(NUMBER(a^))));

    VAR
      sum, sumsq := 0.0d0;
      n := FLOAT(NUMBER(a^), LONGREAL);
    BEGIN
      FOR i := FIRST(a^) TO LAST(a^) DO
        sum := sum + a[i];
        sumsq := sumsq + a[i] * a[i]
      END;

      mean := sum / n;
      Debug.Out  ("Mean " & LR(mean));
      WITH var = sumsq / n - (sum / n) * (sum / n) DO
        sdev := Math.sqrt(var);
        Debug.Out("Sdev " & LR(sdev))
      END
    END;

    VAR
      div := NUMBER(a^);
    BEGIN
      WHILE div # 0 DO
        WITH emax = EvalMax(a^, div),
             rmax = ((emax - mean) / sdev) DO
          Wr.PutText(Stdio.stdout, F("%s %s %s\n", Int(div), LR(emax), LR(rmax)))
        END;

        div := div DIV 2
      END
    END
  END
  
END Main.
