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
FROM TraceOp IMPORT MakeGetNode, MakeScale, MakeTimes, MakePlus;
IMPORT RegEx;
IMPORT DevTermTbl;
IMPORT DevArcs;
IMPORT DevTerms;
IMPORT ArcSetDef;
IMPORT CardSeq;
IMPORT RefSeq;
IMPORT Wr;
IMPORT TraceFile;
IMPORT Rd;
IMPORT OSError;
IMPORT Matrix;
IMPORT RTRefStats;
IMPORT FileWr;
IMPORT AL;
IMPORT IP, NetObj;
IMPORT Thread;
IMPORT ReadLineError;
IMPORT ArithConstants;

<*FATAL Thread.Alerted*>

TYPE NodeId = Trace.NodeId;

CONST Usage   = "";
      Verbose = FALSE;

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

PROCEDURE AddNamedOp(op : TraceOp.T; nm : TEXT; encoding : ArithConstants.Encoding := ArithConstants.Automatic)
  RAISES { Matrix.Singular, OSError.E, Rd.EndOfFile, Rd.Failure, TraceFile.FormatError, Wr.Failure } =
  BEGIN
    IF master THEN
      drew.addNamedOp(op, nm, relPrec)
    ELSE
      EVAL rew.addhiOp(op, Seq1(nm), relPrec := relPrec, encoding := encoding)
    END
  END AddNamedOp;

PROCEDURE TheTestProgram(tr : Trace.T)
  RAISES { Matrix.Singular, OSError.E, Rd.EndOfFile, Rd.Failure, TraceFile.FormatError, Wr.Failure } =
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
    
  END TheTestProgram;

PROCEDURE GetNumberPrefix(str : TEXT; VAR rem : TEXT) : CARDINAL =
  VAR
    res := 0;
  BEGIN
    FOR i := 0 TO Text.Length(str) DO
      WITH c = Text.GetChar(str, i) DO
        CASE c OF
          '0' .. '9' => res := res * 10 + ORD(c) - ORD('0')
        ELSE
          rem := Text.Sub(str, i);
          RETURN res
        END
      END
    END;

    rem := "";
    RETURN res
  END GetNumberPrefix;
  
PROCEDURE FindMyDevices(tr : Trace.T) : DevTermTbl.T =
  <*FATAL RegEx.Error*>
  VAR
    allNames := tr.allNames();
    iter     := allNames.iterate();
    regex    := RegEx.Compile("v[1-9][0-9]*([^()]*)$");

    n        : TEXT;
    rem      : TEXT;

    arcs     := NEW(DevTermTbl.Default).init();
    vMatches := 0;
    terms    : DevTerms.T;
  BEGIN
    (* 
       this seeks out the circuit elements for which we have all 
       terminal currents and voltages

       for those, we compute the power at each terminal and the total
       power for the element, the KCL check, and the integral
    *)
    Debug.Out("Seeking voltage probes...");
    
    WHILE iter.next(n) DO
      WITH start = RegEx.Execute(regex, n) DO
        IF start # -1 THEN
          WITH suffix = Text.Sub(n, start       ),
               prefix = Text.Sub(n,     0, start),
               idx    = GetNumberPrefix(Text.Sub(n, start + 1), rem),
               key    = DevArcs.T { prefix, rem } DO

            IF Verbose THEN
              Debug.Out(F("match %s : suffix %s / idx %s rem %s",
                          n,
                          suffix,
                          Int(idx),
                          rem
              ))
            END;

            INC(vMatches);
            
            IF NOT arcs.get(key, terms) THEN
              terms := DevTerms.Empty;
            END;
            
            terms.terms := terms.terms + SET OF DevTerms.Index { idx };
            terms.max   := MAX(terms.max, idx);
            
            EVAL arcs.put(key, terms)
          END
        END
      END
    END;

    Debug.Out(F("vMatches %s : potential devices %s",
                Int(vMatches), Int(arcs.size())));
    
    (* now seek out those devices as currents *)

    VAR
      iter := arcs.iterate();
      a : DevArcs.T;
      fails := NEW(ArcSetDef.T).init();
    BEGIN
      WHILE iter.next(a, terms) DO
        FOR i := FIRST(DevTerms.Index) TO terms.max DO
          IF i IN terms.terms THEN
            WITH iName = FormatArcs(a, "i", i) DO
              IF NOT allNames.member(iName) THEN
                IF NOT fails.insert(a) THEN
                  Debug.Warning(F("Missing %s, removing arcs for device", iName))
                END
              END
            END
          END
        END
      END;

      (* delete devices that don't have matching currents *)
      
      VAR
        iter := fails.iterate();
        a : DevArcs.T;
      BEGIN
        WHILE iter.next(a) DO
          EVAL arcs.delete(a, terms)
        END
      END
    END;

    Debug.Out(F("Actual devices %s", Int(arcs.size())));
    
    RETURN arcs
    
  END FindMyDevices;

PROCEDURE ThePowerProgram(tr : Trace.T; allNodes : BOOLEAN)
  RAISES { OSError.E, Rd.EndOfFile, Rd.Failure, Wr.Failure, Matrix.Singular, TraceFile.FormatError } =
  VAR
    devs     := FindMyDevices(tr);

    a : DevArcs.T;
    t : DevTerms.T;

    iter   := devs.iterate();
    ndevs := 0;
    
  BEGIN

    WHILE ndevs < maxDevs AND iter.next(a, t) DO
      AddPowerMeasurements(tr, a, t, allNodes);
      INC(ndevs)
    END
    
  END ThePowerProgram;

PROCEDURE AddPowerMeasurements(tr       : Trace.T;
                               a        : DevArcs.T;
                               t        : DevTerms.T;
                               allNodes : BOOLEAN)
  RAISES { Matrix.Singular, OSError.E, Rd.EndOfFile, Rd.Failure, TraceFile.FormatError, Wr.Failure }  =
  VAR
    iNodes := NEW(CardSeq.T).init();
    vNodes := NEW(CardSeq.T).init();
    pseq   := NEW(RefSeq.T).init();
  BEGIN
    FOR i := FIRST(DevTerms.Index) TO t.max DO
      IF i IN t.terms THEN
        (* this node exists *)
        WITH iNode = GetNode(tr, a, "i", i),
             vNode = GetNode(tr, a, "v", i),
             pi    = MakePower(vNode, iNode) DO
          iNodes.addhi(iNode);
          vNodes.addhi(vNode);
          pseq.addhi(pi);
          AddNamedOp(pi, FormatArcs(a, "p", i));
        END
      END
    END;

    VAR
      kclA, palA, sumV := NEW(REF ARRAY OF TraceOp.T, pseq.size());
      nf         := FLOAT(pseq.size(), LONGREAL);
    BEGIN
      FOR i := FIRST(kclA^) TO LAST(kclA^) DO
        kclA[i] := MakeGetNode(iNodes.get(i));
        sumV[i] := MakeGetNode(vNodes.get(i));
        palA[i] := pseq.get(i)
      END;
      WITH kcl  = MakeSum(kclA^),

           (* 
              we find that KCL doesn't quite hold, at least for our level
              of accuracy in XA,
              and it is probably giving rise to an accumulated energy error
           *)
           
           vave = MakeScale(MakeSum(sumV^), -1.0d0 / nf), (* correction term *)
           kclP = MakeTimes(vave, kcl),
           
           pall  = MakeSum(palA^),
           pallC = MakePlus(pall, kclP),
           eall  = NEW(TraceOp.Integrate, a := pall),
           eallC = NEW(TraceOp.Integrate, a := pallC)
       DO
        IF allNodes THEN
          AddNamedOp(kcl , FormatArcs(a, "kcl" , -1));
          AddNamedOp(pall, FormatArcs(a, "pall", -1));
          AddNamedOp(pallC, FormatArcs(a, "pallc", -1));
          AddNamedOp(eall, FormatArcs(a, "eall", -1));
        END;
        AddNamedOp(eallC, FormatArcs(a, "eallc", -1));
      END
    END
  END AddPowerMeasurements;

PROCEDURE FormatArcs(a      : DevArcs.T;
                     ident  : TEXT;
                     idx    : [-1 .. LAST(CARDINAL) ]) : TEXT =
  BEGIN
    IF idx = -1 THEN
      RETURN F("%s%s%s", a.pfx, ident, a.sfx)
    ELSE
      RETURN F("%s%s%s%s", a.pfx, ident, Int(idx), a.sfx)
    END
  END FormatArcs;

PROCEDURE GetNode(tr     : Trace.T;
                  a      : DevArcs.T;
                  ident  : TEXT;
                  idx    : CARDINAL) : Trace.NodeId =
  VAR
    nodeIdx : Trace.NodeId;
  BEGIN
    WITH nm    = FormatArcs(a, ident, idx),
         hadIt = tr.getNodeIdx(nm, nodeIdx) DO
      IF NOT hadIt THEN
        Debug.Error("Couldnt find node in tracefile : " & nm)
      END;
      RETURN nodeIdx
    END
  END GetNode;
                     
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

PROCEDURE Flush()
  RAISES { OSError.E, Rd.EndOfFile, Rd.Failure, TraceFile.FormatError, Wr.Failure } =
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

  doPower  : BOOLEAN;

  maxDevs  : CARDINAL := LAST(CARDINAL);
  allNodes : BOOLEAN;
  phase    : TEXT;
BEGIN
  TRY
    slave    := pp.keywordPresent("-slave");
    master   := pp.keywordPresent("-master");
    doScheme := pp.keywordPresent("-scm");
    allNodes := pp.keywordPresent("-allnodes");

    IF NOT doScheme THEN
      doPower  := pp.keywordPresent("-power")
    END;

    IF master AND pp.keywordPresent("-threads") THEN
      nthreads := pp.getNextInt()
    END;
    
    IF pp.keywordPresent("-root") THEN    
      root := pp.getNext()
    END;

    IF pp.keywordPresent("-rewriter") THEN
      rewriterPath := pp.getNext()
    END;

    IF pp.keywordPresent("-maxdevs") THEN
      maxDevs := pp.getNextInt()
    END;

    IF pp.keywordPresent("-memreport") THEN
      TRY
        WITH interval = pp.getNextLongReal(),
             repwr    = FileWr.Open("rewriter_memreport.out"),
             reporter = NEW(RTRefStats.Reporter).init(interval, wr := repwr) DO
          reporter.start()
        END
      EXCEPT
        OSError.E => Debug.Error("OSError.E creating memory report file")
      END
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

  Debug.Out("RewriterMain rewriterPath " & rewriterPath);

  IF slave THEN
    DistRewriter.RunSlave(root)
  ELSE
    TRY
      rew := NEW(TraceRewriter.T).init(root, rewriterPath);
    EXCEPT
      OSError.E(x) =>
      Debug.Error(F("TraceRewriter.Init %s : OSError.E : %s",
                    root, AL.Format(x)))
    |
      Rd.Failure(x) =>
      Debug.Error(F("TraceRewriter.Init %s : Rd.Failure : %s",
                    root, AL.Format(x)))
    |
      Rd.EndOfFile =>
      Debug.Error(F("TraceRewriter.Init %s : short read",
                    root))
    |
      TraceFile.FormatError =>
      Debug.Error(F("TraceRewriter.Init %s : trace file format error",
                    root))
    END;

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
    |
      IP.Error, NetObj.Error => Debug.Error("Network error creating scheme")
    |
      ReadLineError.E => Debug.Error("Readline error in scheme")
    END
    ELSE
      TRY

        phase := "program";
        IF doPower THEN
          ThePowerProgram(rew.shareTrace(), allNodes)
        ELSE
          TheTestProgram(rew.shareTrace())
        END;

        phase := "flush";
        Flush()

      EXCEPT
        Matrix.Singular => Debug.Error(F("Numerical error in %s",phase))
      |
        OSError.E(x) => Debug.Error(F("OSError.E in %s : %s", phase, AL.Format(x)))
      |
        Rd.EndOfFile => Debug.Error(F("Short read in %s", phase))
      |
        Rd.Failure(x) =>Debug.Error(F("Rd.Failure in %s : %s", phase, AL.Format(x)))
      |
        TraceFile.FormatError =>Debug.Error(F("Corrupted trace file in %s", phase))
      |
        Wr.Failure(x) =>Debug.Error(F("Wr.Failure in %s : %s", phase, AL.Format(x)))
      END
    END
  END
END RewriterMain.
