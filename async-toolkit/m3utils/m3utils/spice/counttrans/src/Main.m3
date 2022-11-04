MODULE Main;
IMPORT Trace;
FROM Fmt IMPORT F, Int, LongReal;
IMPORT Debug;
IMPORT OSError, Rd;
IMPORT ParseParams;
IMPORT Params;
IMPORT Stdio;
IMPORT AL;
IMPORT LongRealSeq;
IMPORT IntPair;
IMPORT IntLRPair;
IMPORT IntLRArraySort;
IMPORT IntPairArraySort;
IMPORT Wr;
IMPORT FileWr;

CONST
  Usage = "";
  BaseVdd = 0.75d0;
  LR = LongReal;
  
VAR
  root := "xa";
  trace : Trace.T;
  nSteps, nNodes : CARDINAL;
  pp := NEW(ParseParams.T).init(Stdio.stderr);
  tarr, arr : REF ARRAY OF LONGREAL;
  cutoff := BaseVdd / 2.0d0;
  minNode : CARDINAL := 1;
  maxNode : CARDINAL := LAST(CARDINAL);
  tranCnts : REF ARRAY OF IntPair.T;
  minReps : REF ARRAY OF IntLRPair.T;
  
PROCEDURE WorkOn(idx            : CARDINAL;
                 READONLY ta, a : ARRAY OF LONGREAL;
                 cross          : LONGREAL) =
  VAR
    pn := a[FIRST(a)];
    pt : LONGREAL;
    seq := NEW(LongRealSeq.T).init();
    minRep := LAST(LONGREAL);
  BEGIN
    FOR i := FIRST(a) TO LAST(a) DO
      WITH t = ta[i],
           n =  a[i] DO
        IF n > cross AND pn <= cross THEN
          WITH delV  = n - pn,
               delT  = t - pt,
               delV0 = cross - pn,
               delT0 = delV0 / delV * delT DO
            seq.addhi(pt + delT0)
          END
        END;
        pn := n;
        pt := t
      END
    END;
    FOR j := 1 TO seq.size() - 1 DO
      WITH rep = seq.get(j) - seq.get(j - 1) DO
        minRep := MIN(rep, minRep)
      END
    END;
    Debug.Out(F("Node %s has %s transitions", Int(idx), Int(seq.size())));
    tranCnts[idx] := IntPair.T { idx, seq.size() };
    minReps[idx] := IntLRPair.T { idx, minRep }
  END WorkOn;
  
BEGIN
  TRY
    IF pp.keywordPresent("-r") THEN
      root := pp.getNext()
    END;

    IF pp.keywordPresent("-c") THEN
      cutoff := pp.getNextLongReal()
    END;

    IF pp.keywordPresent("-range") THEN
      minNode := pp.getNextInt();
      maxNode := pp.getNextInt()
    END;
    
    pp.skipParsed();
    pp.finish();
  EXCEPT
    ParseParams.Error => Debug.Error("Can't parse command-line parameters\nUsage: " & Params.Get(0) & " " & Usage)
  END;

  TRY
    trace := NEW(Trace.T).init(root);
  EXCEPT
    OSError.E(x) => Debug.Error("Trouble opening input trace : OSError.E : " & AL.Format(x))
  |
    Rd.Failure(x) => Debug.Error("Trouble opening input trace : Rd.Failure : " & AL.Format(x))
  |
    Rd.EndOfFile =>
    Debug.Error(F("Short read opening input trace"))
  END;

  nSteps := trace.getSteps();
  nNodes := trace.getNodes();

  Debug.Out(F("%s steps, %s nodes", Int(nSteps), Int(nNodes)));

  arr := NEW(REF ARRAY OF LONGREAL, nSteps);
  tarr := NEW(REF ARRAY OF LONGREAL, nSteps);
  tranCnts := NEW(REF ARRAY OF IntPair.T, nNodes);
  minReps := NEW(REF ARRAY OF IntLRPair.T, nNodes);
  
  trace.getTimeData(tarr^);

  maxNode := MIN(maxNode, nNodes - 1);
  
  FOR i := minNode TO maxNode DO
    Debug.Out(F("working on node %s", Int(i)));
    TRY
      trace.getNodeData(i, arr^);
      WorkOn(i, tarr^, arr^, cutoff)
    EXCEPT
      Rd.Failure(x) => Debug.Error(F("Trouble reading node %s from input trace : Rd.Failure : %s", Int(i), AL.Format(x)))
    |
      Rd.EndOfFile =>
      Debug.Error(F("Short read reading node %s from input trace", Int(i)))
    END
  END;

  IntLRArraySort.Sort(SUBARRAY(minReps^, minNode, maxNode - minNode + 1),
                      cmp := IntLRPair.CompareK2);
  IntPairArraySort.Sort(SUBARRAY(tranCnts^, minNode, maxNode - minNode + 1),
                        cmp := IntPair.CompareK2);

  VAR
    wr : Wr.T;
  BEGIN
    wr := FileWr.Open("trancnts.dat");
    FOR i := minNode TO maxNode DO
      WITH rec = tranCnts[i] DO
        Wr.PutText(wr,
                   F("%s %s %s transitions\n",
                     trace.getCanonicalName(rec.k1),
                     Int(rec.k1),
                     Int(rec.k2)))
      END
    END;
    Wr.Close(wr);
    wr := FileWr.Open("minreps.dat");
    FOR i := maxNode TO minNode BY -1 DO
      WITH rec = minReps[i] DO
        Wr.PutText(wr,
                   F("%s %s %s minrep\n",
                     trace.getCanonicalName(rec.k1),
                     Int(rec.k1),
                     LR(rec.k2)))
      END
    END;
    Wr.Close(wr)
  END;
  
END Main.
