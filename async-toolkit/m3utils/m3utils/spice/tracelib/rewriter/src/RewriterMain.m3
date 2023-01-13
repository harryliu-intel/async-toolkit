MODULE RewriterMain EXPORTS Main;
IMPORT ParseParams;
IMPORT Stdio;

IMPORT Trace;
IMPORT TraceRewriter;

IMPORT Params;
IMPORT Debug;
IMPORT TraceOp;
IMPORT Pathname;
IMPORT TextSeq;
IMPORT Text;
FROM Fmt IMPORT F, Int;
IMPORT SchemeStubs;
IMPORT Scheme;
IMPORT SchemeM3;
IMPORT SchemeReadLine;
IMPORT ReadLine;
IMPORT DistRewriter;
FROM TraceOp IMPORT MakeGetNode;

TYPE NodeId = Trace.NodeId;

CONST Usage   = "";
      doDebug = TRUE;

PROCEDURE MakePower(vi, ii : NodeId) : TraceOp.T =
  VAR
    vn := NEW(TraceOp.GetNode, nodeid := vi);
    in := NEW(TraceOp.GetNode, nodeid := ii);
  BEGIN
    RETURN NEW(TraceOp.Times, a := vn, b := in)
  END MakePower;

PROCEDURE MakeSum(READONLY ops : ARRAY OF TraceOp.T) : TraceOp.T =
  BEGIN
    IF NUMBER(ops) = 1 THEN
      RETURN ops[0]
    ELSE
      RETURN NEW(TraceOp.Plus,
                 a := ops[0],
                 b := MakeSum(SUBARRAY(ops, 1, NUMBER(ops) - 1)))
    END
  END MakeSum;

PROCEDURE Seq1(txt : TEXT) : TextSeq.T =
  VAR
    res := NEW(TextSeq.T).init();
  BEGIN
    res.addhi(txt);
    RETURN res
  END Seq1;

PROCEDURE AddNamedOp(op : TraceOp.T; nm : TEXT) =
  BEGIN
    IF master THEN
      drew.addNamedOp(op, nm, relPrec)
    ELSE
      rew.addhiOp(op, Seq1(nm), relPrec := relPrec, noArith := FALSE)
    END
  END AddNamedOp;

PROCEDURE TheProgram() =
  (* this is the test program *)
  BEGIN
    
    CONST
      v1n = "dut.stage[22].digest1.pipeY.latch[3].bit[15].v1(M1n.M0)";
      v2n = "dut.stage[22].digest1.pipeY.latch[3].bit[15].v2(M1n.M0)";
      v3n = "dut.stage[22].digest1.pipeY.latch[3].bit[15].v3(M1n.M0)";
      i1n = "dut.stage[22].digest1.pipeY.latch[3].bit[15].i1(M1n.M0)";
      i2n = "dut.stage[22].digest1.pipeY.latch[3].bit[15].i2(M1n.M0)";
      i3n = "dut.stage[22].digest1.pipeY.latch[3].bit[15].i3(M1n.M0)";
      
    VAR
      tr := NEW(Trace.T).init(root);
      v1i, v2i, v3i, i1i, i2i, i3i : Trace.NodeId;
    BEGIN
      WITH hadIt = tr.getNodeIdx(v1n, v1i) DO <*ASSERT hadIt*> END;
      WITH hadIt = tr.getNodeIdx(v2n, v2i) DO <*ASSERT hadIt*> END;
      WITH hadIt = tr.getNodeIdx(v3n, v3i) DO <*ASSERT hadIt*> END;
      WITH hadIt = tr.getNodeIdx(i1n, i1i) DO <*ASSERT hadIt*> END;
      WITH hadIt = tr.getNodeIdx(i2n, i2i) DO <*ASSERT hadIt*> END;
      WITH hadIt = tr.getNodeIdx(i3n, i3i) DO <*ASSERT hadIt*> END;
      
      WITH p1 = MakePower(v1i, i1i),
           p2 = MakePower(v2i, i2i),
           p3 = MakePower(v3i, i3i),

           kcl  = MakeSum(ARRAY OF TraceOp.T { MakeGetNode(i1i),
                                               MakeGetNode(i2i),
                                               MakeGetNode(i3i) }),
           pall = MakeSum(ARRAY OF TraceOp.T { p1, p2, p3 }),
           eall = NEW(TraceOp.Integrate, a := pall) DO
      
        AddNamedOp(p1  , "p1");
        AddNamedOp(p2  , "p2");
        AddNamedOp(p3  , "p3");
        AddNamedOp(pall, "pall");
        AddNamedOp(eall, "eall");
        AddNamedOp(kcl , "kcl")
      END
    END;
    
  END TheProgram;
  
PROCEDURE GetPaths(extras : TextSeq.T) : REF ARRAY OF Pathname.T = 
  CONST
    fixed = ARRAY OF Pathname.T { "require", "m3" };
  VAR
    res := NEW(REF ARRAY OF Pathname.T, NUMBER(fixed) + extras.size());
  BEGIN
    FOR i := 0 TO NUMBER(fixed) - 1 DO
      res[i] := fixed[i]
    END;
    FOR i := NUMBER(fixed) TO extras.size() + NUMBER(fixed) - 1 DO
      res[i] := extras.remlo()
    END;
    RETURN res
  END GetPaths;

PROCEDURE Flush() =
  BEGIN
    IF master THEN
      drew.flush()
    ELSE
      rew.flush()
    END
  END Flush;
  
VAR
  pp            := NEW(ParseParams.T).init(Stdio.stderr);
  rewriterPath  := Params.Get(0);
  master, slave := FALSE; (* if both FALSE, then we do all ourselves *)
  relPrec       := 0.001d0;
  
  rew      : TraceRewriter.T;
  drew     : DistRewriter.T;
  root     : Pathname.T;
  doScheme : BOOLEAN;

  extra := NEW(TextSeq.T).init();

  nthreads : CARDINAL := 1;

BEGIN
  Debug.Out("RewriterMain rewriterPath " & rewriterPath);
  
  TRY
    slave    := pp.keywordPresent("-slave");
    master   := pp.keywordPresent("-master");
    doScheme := pp.keywordPresent("-scm");

    IF master AND pp.keywordPresent("-threads") THEN
      nthreads := pp.getNextInt()
    END;
    
    IF pp.keywordPresent("-root") THEN    
      root := pp.getNext()
    END;

    IF pp.keywordPresent("-rewriter") THEN
      rewriterPath := pp.getNext()
    END;

    pp.skipParsed();

    IF doScheme THEN
      WITH n = NUMBER(pp.arg^) - pp.next DO
        FOR i := 0 TO n - 1 DO
          extra.addhi(pp.getNext())
        END
      END
    END;
    
    pp.finish()
  EXCEPT
    ParseParams.Error =>
    Debug.Error("Can't parse command-line parameters\nUsage: " & Params.Get(0) & " " & Usage)
  END;

  <*ASSERT NOT (slave AND master)*>

  IF slave THEN
    DistRewriter.RunSlave(root)
  ELSE
    rew := NEW(TraceRewriter.T).init(root, rewriterPath);

    IF master THEN
      drew := NEW(DistRewriter.T).init(root,
                                       nthreads,
                                       rewriterPath,
                                       rew)
    END;
    
    IF doScheme THEN
      SchemeStubs.RegisterStubs();
    TRY
      WITH scm = NEW(SchemeM3.T).init(GetPaths(extra)^) DO
        SchemeReadLine.MainLoop(NEW(ReadLine.Default).init(), scm)
      END
    EXCEPT
      Scheme.E(err) => Debug.Error("Caught Scheme.E : " & err)
    END
    ELSE
      TheProgram();
      Flush()
    END
  END;

END RewriterMain.
