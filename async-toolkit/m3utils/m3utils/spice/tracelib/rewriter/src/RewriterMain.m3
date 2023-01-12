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

TYPE NodeId = Trace.NodeId;


CONST Usage = "";


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
    rew.addhiOp(op, Seq1(nm), relPrec := relPrec, noArith := FALSE)
  END AddNamedOp;
  
VAR
  pp := NEW(ParseParams.T).init(Stdio.stderr);
  myPath := Params.Get(0);
  rew : TraceRewriter.T;
  root : Pathname.T;
  
  master, slave := FALSE; (* if both FALSE, then we do all ourselves *)

  relPrec := 0.001d0;
  
BEGIN
  Debug.Out("RewriterMain myPath " & myPath);
  
  TRY
    slave  := pp.keywordPresent("-slave");
    master := pp.keywordPresent("-master");

    pp.skipParsed();

    root := pp.getNext();

    pp.finish()
  EXCEPT
    ParseParams.Error => Debug.Error("Can't parse command-line parameters\nUsage: " & Params.Get(0) & " " & Usage)
  END;

  rew := NEW(TraceRewriter.T).init(root, myPath);

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

         pall = MakeSum(ARRAY OF TraceOp.T { p1, p2, p3 }),
         eall = NEW(TraceOp.Integrate, a := pall) DO

      AddNamedOp(p1  , "p1");
      AddNamedOp(p2  , "p2");
      AddNamedOp(p3  , "p3");
      AddNamedOp(pall, "pall");
      AddNamedOp(eall, "eall")
    END
  END;

  rew.flush()


END RewriterMain.
