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
IMPORT TextReader;
IMPORT Rd;
IMPORT Text;
IMPORT DistZTrace;
FROM Fmt IMPORT F, Int;
IMPORT Pickle;

TYPE NodeId = Trace.NodeId;

CONST Usage = "";
      TE = Text.Equal;
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
    rew.addhiOp(op, Seq1(nm), relPrec := relPrec, noArith := FALSE)
  END AddNamedOp;

PROCEDURE RunSlave() =
  VAR
    rd := Stdio.stdin;
    tr := NEW(Trace.T).init(root);
    kw : TEXT;
  BEGIN
    TRY
    LOOP
      WITH line   = Rd.GetLine(rd),
           reader = NEW(TextReader.T).init(line) DO
        IF doDebug THEN
          Debug.Out(F("RewriterMain.RunSlave line \"%s\"", line));
        END;
        IF reader.next(" ", kw, TRUE) THEN
          IF TE(kw, "P") THEN
            WITH id        = reader.getInt(),
                 prec      = reader.getLR(),
                 ref       = Pickle.Read(rd),
                 arr       = NEW(REF ARRAY OF LONGREAL, tr.getSteps()) DO

              <*ASSERT ISTYPE(ref, TraceOp.T)*>

              NARROW(ref, TraceOp.T).exec(tr, arr^);

              DistZTrace.WriteOut(Stdio.stdout,
                                  arr^,
                                  id,
                                  FALSE,
                                  relPrec,
                                  FALSE,
                                  FALSE)
            END
          END
        END
      END
    END
    EXCEPT
      Rd.EndOfFile => (* skip *)
    END
  END RunSlave;

PROCEDURE TheProgram() =
  BEGIN
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
  END TheProgram;
  
VAR
  pp            := NEW(ParseParams.T).init(Stdio.stderr);
  myPath        := Params.Get(0);
  master, slave := FALSE; (* if both FALSE, then we do all ourselves *)
  relPrec       := 0.001d0;
  
  rew  : TraceRewriter.T;
  root : Pathname.T;
  
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

  <*ASSERT NOT (slave AND master)*>

  IF slave THEN
    RunSlave()
  ELSE
    TheProgram()
  END;

END RewriterMain.
