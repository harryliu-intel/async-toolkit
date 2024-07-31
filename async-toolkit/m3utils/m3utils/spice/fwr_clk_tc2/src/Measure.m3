MODULE Measure EXPORTS Main;
IMPORT FS;
IMPORT Pathname;
IMPORT Debug;
FROM Fmt IMPORT F, Int, LongReal, FN, Bool;
IMPORT CitTextUtils;
IMPORT Trace;
IMPORT TransitionFinder;
IMPORT TraceFile;
IMPORT Rd, Wr;
IMPORT OSError;
IMPORT Math;
IMPORT FileRd;
IMPORT FileWr;
IMPORT ParseParams;
IMPORT Stdio;
IMPORT Params;
IMPORT Scan;

CONST LR = LongReal;

TYPE TA = ARRAY OF TEXT;

EXCEPTION Error;

TYPE
  Result = RECORD
    pw, idelay : LONGREAL;
  END;
  
PROCEDURE DoOne(root             : Pathname.T;
                srcNode, tgtNode : Node) : Result
  RAISES  { Error } =
  BEGIN
    TRY
      VAR
        trace := NEW(Trace.T).init(root);

        
        sIdx, tIdx   : Trace.NodeId;
        hadSrcNode := trace.getNodeIdx(srcNode.name, sIdx);
        hadTgtNode := trace.getNodeIdx(tgtNode.name, tIdx);

        stf    := NEW(TransitionFinder.T).init(trace,
                                               0.5d0 * srcNode.true,
                                               0.1d0 * srcNode.true);
        ttf    := NEW(TransitionFinder.T).init(trace,
                                               0.5d0 * tgtNode.true,
                                               0.1d0 * tgtNode.true);

        sTrans := stf.forNode(sIdx);
        tTrans := ttf.forNode(tIdx);

      BEGIN
        IF NOT hadSrcNode THEN
          Debug.Error(F("Couldnt find source node %s", srcNode.name))
        END;
        IF NOT hadTgtNode THEN
          Debug.Error(F("Couldnt find target node %s", tgtNode.name))
        END;
        
        <*ASSERT hadSrcNode AND hadTgtNode*>
        Debug.Out(F("src %s transitions", Int(sTrans.size())));
        Debug.Out(F("tgt %s transitions", Int(tTrans.size())));

        IF tTrans.size() < 2 THEN RAISE Error END;
        
        WITH s0 = sTrans.get(0),
             t0 = tTrans.get(0),
             t1 = tTrans.get(1),
             idelay = t0.at - s0.at,
             pw = t1.at - t0.at DO
          <*ASSERT t0.dir = 1 *>
          <*ASSERT t1.dir = -1 *>
          Debug.Out(F("s0 @            %s", LR(s0.at)));
          Debug.Out(F("t0 @            %s", LR(t0.at)));
          Debug.Out(F("t1 @            %s", LR(t1.at)));
          Debug.Out(F("pulse width     %s", LR(pw)));
          Debug.Out(F("insertion delay %s", LR(idelay)));
          RETURN Result { pw := pw, idelay := idelay }
        END
      END
    EXCEPT
      OSError.E, Rd.EndOfFile, Rd.Failure, TraceFile.FormatError => RAISE Error
    END
  END DoOne;

TYPE
  Node = RECORD
    name : TEXT;
    true : LONGREAL;
  END;
    
CONST
  Zsfx = ".ztrace";
  SkipIter = "@1"; (* skip the nominal iteration *)
VAR
  iter := FS.Iterate(".");
  fn : TEXT;
  nf, sumPw, sumSqPw, sumId, sumSqId := 0.0d0;
  haveError := FALSE;
  pp                        := NEW(ParseParams.T).init(Stdio.stderr);
  src, tgt : Node;
  vTrue := FIRST(LONGREAL);
  inputWidth := 1000.0d-12;
BEGIN
  TRY
    IF pp.keywordPresent("-vtrue") THEN
      vTrue := pp.getNextLongReal()
    END;

    IF pp.keywordPresent("-tgt") THEN
      tgt := Node { pp.getNext(), vTrue }
    ELSIF pp.keywordPresent("-vtgt") THEN
      WITH v = Scan.LongReal(pp.getNext()),
           n = pp.getNext() DO
        tgt := Node { n, v }
      END
    ELSE
      Debug.Error("Must specify -[v]tgt")
    END;

    IF pp.keywordPresent("-src") THEN
      src := Node { pp.getNext(), vTrue }
    ELSIF pp.keywordPresent("-vsrc") THEN
      WITH v = Scan.LongReal(pp.getNext()),
           n = pp.getNext() DO
        src := Node { n, v }
      END
    ELSE
      Debug.Error("Must specify -[v]src")
    END;
    
    IF pp.keywordPresent("-inputwidth") THEN
      inputWidth := pp.getNextLongReal()
    END;
    
  EXCEPT
    ParseParams.Error => Debug.Error("Can't parse command-line parameters\nUsage: " & Params.Get(0) & "\n" )
  END;
    
  WHILE iter.next(fn) DO
    IF CitTextUtils.HaveSuffix(fn, Zsfx) THEN
      WITH root = CitTextUtils.RemoveSuffix(fn, Zsfx) DO
        IF NOT CitTextUtils.HaveSuffix(root, SkipIter) THEN
          TRY
            WITH r = DoOne(root, src, tgt) DO
              nf := nf + 1.0d0;
              sumPw := sumPw + r.pw;
              sumSqPw := sumSqPw + r.pw * r.pw;
              sumId := sumId + r.idelay;
              sumSqId := sumSqId + r.idelay * r.idelay
            END
          EXCEPT
            Error => haveError:= TRUE
          END
        END
      END
    END
  END;

  Debug.Out(F("nf=%s sumPw=%s sumId=%s", LR(nf), LR(sumPw), LR(sumId)));
  
  WITH meanPw = sumPw / nf,
       varPw  = sumSqPw / nf - meanPw * meanPw,
       sdevPw = Math.sqrt(varPw * nf / (nf - 1.0d0)),
       
       meanId = sumId / nf,
       varId  = sumSqId / nf - meanId * meanId,
       sdevId = Math.sqrt(varId * nf / (nf - 1.0d0)),
       
       mrd  = FileRd.Open("measure.dat"),
       line = Rd.GetLine(mrd),
       mwr  = FileWr.Open("results.dat") DO
    Wr.PutText(mwr, line);
    Wr.PutText(mwr, FN(",%s,%s,%s,%s,%s,%s,%s\n", TA{Bool(NOT haveError), LR(nf), LR(meanPw), LR(meanPw - inputWidth), LR(sdevPw), LR(meanId), LR(sdevId)} ));
    Rd.Close(mrd);
    Wr.Close(mwr);
  END
END Measure.
