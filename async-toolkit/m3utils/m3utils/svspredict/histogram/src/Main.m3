MODULE Main;

(* Usage: histogram <ofn> <# buckets> *)

IMPORT Histogram;
IMPORT LongrealArraySort;
IMPORT Params;
IMPORT Scan;
IMPORT LongRealSeq AS LRSeq;
IMPORT Stdio, Rd;
IMPORT ParseParams;
IMPORT Pathname;
IMPORT Debug;

VAR
  ofn     :Pathname.T;
  buckets : CARDINAL;

  rd := Stdio.stdin;
  line : TEXT;
  data : REF ARRAY OF LONGREAL;
  dataSeq := NEW(LRSeq.T).init();
  pp := NEW(ParseParams.T).init(Stdio.stderr);

  min := Histogram.AutomaticMin;
  max := Histogram.AutomaticMax;
  
BEGIN
  TRY

    IF pp.keywordPresent("-min") THEN
      min := pp.getNextLongReal()
    END;
    
    IF pp.keywordPresent("-max") THEN
      max := pp.getNextLongReal()
    END;
    
    pp.skipParsed();
    ofn     := pp.getNext();
    buckets := pp.getNextInt();
    pp.finish()
  EXCEPT
    ParseParams.Error => Debug.Error("Can't parse command line")
  END;
  
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
  
  Histogram.Do(ofn, data^, TRUE, buckets, forceMin := min, forceMax := max)

END Main.
