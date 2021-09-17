MODULE Main;

(* Usage: histogram <ofn> <# buckets> *)

IMPORT Histogram;
IMPORT LongrealArraySort;
IMPORT Params;
IMPORT Scan;
IMPORT LongRealSeq AS LRSeq;
IMPORT Stdio, Rd;

VAR
  ofn     := Params.Get(1);
  buckets := Scan.Int(Params.Get(2));

  rd := Stdio.stdin;
  line : TEXT;
  data : REF ARRAY OF LONGREAL;
  dataSeq := NEW(LRSeq.T).init();

BEGIN
  TRY
    LOOP
      line := Rd.GetLine(rd);
      WITH new = Scan.LongReal(line) DO
        dataSeq.addhi(new);
      END
    END
  EXCEPT
    Rd.EndOfFile => (* skip *)
  END;

  data := NEW(REF ARRAY OF LONGREAL, dataSeq.size());
  FOR i := 0 TO dataSeq.size() - 1 DO
    data[i] := dataSeq.get(i)
  END;

  LongrealArraySort.Sort(data^);
  
  Histogram.Do(ofn, data^, TRUE, buckets)

END Main.
