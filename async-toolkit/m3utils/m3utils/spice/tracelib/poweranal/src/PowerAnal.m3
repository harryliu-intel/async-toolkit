MODULE PowerAnal EXPORTS Main;
IMPORT Trace;
IMPORT ParseParams;
IMPORT Debug;
IMPORT Stdio;
IMPORT Params;
IMPORT Pathname;
IMPORT RegEx;
IMPORT Text;
FROM Fmt IMPORT F, Int, LongReal, Style;
IMPORT RefSeq;
IMPORT TraceInterpolator;
IMPORT OSError;
IMPORT Rd;
IMPORT SpiceFormat;
IMPORT SpiceError;
IMPORT AL;
IMPORT FileRd;
IMPORT Wr;
IMPORT SpiceCircuit;
IMPORT SpiceObject;
IMPORT TextTextTbl;
IMPORT NameControl;
IMPORT SortedTextLongRealTbl AS TextLRTbl;
IMPORT SortedTextSortedTextLRTbl AS TextTextLRTbl;
FROM CitTextUtils IMPORT RemovePrefix, RemoveSuffix;
IMPORT FileWr;
IMPORT SortedTextRefTbl AS TextRefTbl;
IMPORT Thread;
IMPORT SortedTextTextSeqTbl AS TextTextSeqTbl;
IMPORT TextSeq;

<*FATAL Thread.Alerted*>

CONST Usage   = "";
      Verbose = FALSE;
      LR      = LongReal;
      TE      = Text.Equal;
      
TYPE
  Measurement = OBJECT
    nm, pfx, sfx : TEXT;
    E            : LONGREAL;
  END;

PROCEDURE Strip(nm : TEXT) : TEXT =
  BEGIN
    RETURN RemoveSuffix(RemovePrefix(nm, ".eallc("), ")")
  END Strip;
      
PROCEDURE DoReports(tr : Trace.T) =
  <*FATAL RegEx.Error*>
  VAR
    allNames := tr.allNames();
    iter     := allNames.iterate();

    regex    := RegEx.Compile("\\.eallc([^()]*)$");
    (* the coding of the regex strips the point from the matched names *)

    n        : TEXT;

    matches  := NEW(RefSeq.T).init();
    idx      : Trace.NodeId;
    scratch  := NEW(REF REF ARRAY OF LONGREAL);

    totE := 0.0d0;

    typeNm : TEXT;
    
  BEGIN
    WHILE iter.next(n) DO
      WITH start = RegEx.Execute(regex, n) DO
        IF start # -1 THEN
          WITH suffix = Text.Sub(n, start       ),
               prefix = Text.Sub(n,     0, start) DO
            IF Verbose THEN
              Debug.Out(F("Found match %s , %s", prefix, suffix))
            END;

            matches.addhi(NEW(Measurement,
                              nm := n, pfx := prefix, sfx := suffix));
            IF matches.size() = maxDevs THEN EXIT END;
          END
        END
      END
    END;

    Debug.Out(F("%s matches", Int(matches.size())));

    FOR i := 0 TO matches.size() - 1 DO
      WITH m     = NARROW(matches.get(i), Measurement),
           found = tr.getNodeIdx(m.nm, idx),
           int   = NEW(TraceInterpolator.T).init(tr, idx, scratch),
           loE   = int.eval(frTime),
           hiE   = int.eval(toTime) DO
        <*ASSERT found*>
        m.E  := hiE  - loE;
        totE := totE + m.E;

        Debug.Out(F("%s , %s : (%s, %s) = %s",
                    m.pfx, m.sfx, LR(loE), LR(hiE), LR(m.E)))

      END
    END;

    Debug.Out(F("Timeseries analysis complete, total energy %s", LR(totE)));

    FOR i := 0 TO matches.size() - 1 DO
      WITH m         = NARROW(matches.get(i), Measurement),
           foundCell = hierCells.get(m.pfx, typeNm)
       DO
        RecordPathE(m);
        
        IF NOT foundCell THEN
          Debug.Warning("Couldn't find cell named " & m.pfx)
        ELSE
          RecordCellE(typeNm, m)
        END
      END
    END;

    (* the path report totalizes, so we need to do it first, before the 
       types report *)
    WITH wr = FileWr.Open("paths.prpt") DO
      DoPathReport(wr);
      Wr.Close(wr)
    END;

    WITH wr = FileWr.Open("types0.prpt") DO
      DoTypeReportOld(wr);
      Wr.Close(wr)
    END      ;
    WITH wr = FileWr.Open("types.prpt") DO
      DoTypeReport(wr);
      Wr.Close(wr)
    END      ;
    

  END DoReports;

PROCEDURE DoTypeReportOld(wr : Wr.T) =
  VAR
    iter := cellData.iterateOrdered();
    tn : TEXT;
    tab : TextLRTbl.T;
    mn : TEXT;
    e, te  : LONGREAL;
    
    dt := toTime - frTime;
  CONST
    Sci = Style.Sci;
    Fix = Style.Fix;
  BEGIN
    WHILE iter.next(tn, tab) DO
      te := 0.0d0;
      WITH jter = tab.iterate() DO
        WHILE jter.next(mn, e) DO
          te := te + e
        END
      END;
      Wr.PutText(wr, F("TYPE %-53s %13s %13s\n",
                       tn,
                       LR(te/dt/freq, prec := 4, style := Sci),
                       LR(te, prec := 4, style := Sci)
      ));
      WITH jter = tab.iterateOrdered() DO
        WHILE jter.next(mn, e) DO
          Wr.PutText(wr, F("    DEV  %-49s %13s %13s %-25s\n",
                           Strip(mn),
                           LR(e/dt/freq, prec := 4, style := Sci),
                           LR(e,    prec := 4, style := Sci),
                           LR(e/te/freq, prec := 3, style := Fix)))
        END
      END
    END
  END DoTypeReportOld;

PROCEDURE DoTypeReport(wr : Wr.T) =
  VAR
    iter := instances.iterateOrdered();
    tn : TEXT;
    seq : TextSeq.T;
    sum : LONGREAL;
    dt := toTime - frTime;
  CONST
    Sci = Style.Sci;
    Fix = Style.Fix;
  BEGIN
    WHILE iter.next(tn, seq) DO
      Debug.Out(F("Reporting on type %s : %s instances", tn, Int(seq.size())));

      (* find every instance and totalize *)
      sum := 0.0d0;
      FOR i := 0 TO seq.size() - 1 DO
        WITH fqn = seq.get(i),
             pn  = MakePaths(fqn, FALSE) DO
          IF pn # NIL THEN
            sum := sum + pn.e
          END
        END
      END;
      Debug.Out(F("type %s : e = %s", tn, LR(sum)));
      Wr.PutText(wr, F("TYPE %-53s %13s %13s\n",
                       tn,
                       LR(sum/dt/freq, prec := 4, style := Sci),
                       LR(sum, prec := 4, style := Sci)
      ));
     END
  END DoTypeReport;

PROCEDURE DoPathReport(wr : Wr.T) =

  PROCEDURE Totalize(p : PathNode) : LONGREAL = 
    (* totalize the e, in postorder traversal *)
    VAR
      iter := p.children.iterate();
      r : REFANY;
      nm : TEXT;
    BEGIN
      WHILE iter.next(nm, r) DO
        p.e := p.e + Totalize(r)
      END;
      RETURN p.e
    END Totalize;

  PROCEDURE DumpPaths(p : PathNode; depth : CARDINAL; parentE : LONGREAL) =
    (* dump names and energies, in preorder *)
    CONST
      Sci = Style.Sci;
      Fix = Style.Fix;
      Col1 = 80;
    VAR
      spaces := depth * 2;
      dt     := toTime - frTime;
    BEGIN
      Spaces(spaces);
      Wr.PutText(wr, F("%-"&Int(MAX(Col1 - spaces,0))&"s %13s %13s %-13s\n",
                       p.nm,
                       LR(p.e/dt/freq, prec := 4, style := Sci),
                       LR(p.e, prec := 4, style := Sci),
                       LR(p.e/parentE, style := Fix, prec := 3)));
      VAR
        iter := p.children.iterateOrdered();
        r : REFANY;
        nm : TEXT;
      BEGIN
        WHILE iter.next(nm, r) DO
          DumpPaths(r, depth + 1, p.e)
        END
      END
    END DumpPaths;

  PROCEDURE Spaces(n : CARDINAL) =
    BEGIN
      FOR i := 0 TO n - 1 DO
        Wr.PutChar(wr, ' ')
      END
    END Spaces;
    
  BEGIN
    EVAL Totalize(paths);

    DumpPaths(paths, 0, paths.e)
  END DoPathReport;    

VAR cellData  := NEW(TextTextLRTbl.Default).init();
    instances := NEW(TextTextSeqTbl.Default).init();
    
PROCEDURE RecordTypeInstance(type : TEXT; fqn : TEXT) =
  VAR
    seq : TextSeq.T;
  BEGIN
    IF NOT instances.get(type, seq) THEN
      seq := NEW(TextSeq.T).init();
      EVAL instances.put(type, seq)
    END;
    seq.addhi(fqn)
  END RecordTypeInstance;
    
PROCEDURE RecordCellE(typeNm : TEXT;
                      m      : Measurement) =
  VAR
    e   : LONGREAL := 0.0d0;
    tbl : TextLRTbl.T;
    
  BEGIN
    IF NOT cellData.get(typeNm, tbl) THEN
      tbl := NEW(TextLRTbl.Default).init();
      EVAL cellData.put(typeNm, tbl)
    END;
    EVAL tbl.get(m.sfx, e);
    e := e + m.E;
    EVAL tbl.put(m.sfx, e)
  END RecordCellE;

TYPE
  PathNode = OBJECT
    e        : LONGREAL;
    up       : PathNode;
    nm       : TEXT;
    children : TextRefTbl.T;
  END;

VAR
  paths    := NEW(PathNode,            (* sentinel *)
                  e        := 0.0d0,
                  up       := NIL,
                  nm       := "",
                  children := NEW(TextRefTbl.Default).init());
  
PROCEDURE Next(q : PathNode; nm : TEXT; doNew : BOOLEAN) : PathNode =
  VAR
    r : REFANY;
  BEGIN
    IF q.children.get(nm, r) THEN
      RETURN r
    ELSE
      IF doNew THEN
        WITH new = NEW(PathNode,
                       e  := 0.0d0,
                       up := q,
                       nm := nm,
                       children := NEW(TextRefTbl.Default).init()) DO

          Debug.Out(F("Next created %s . %s", FormatPathNode(q), nm));
          
          EVAL q.children.put(nm, new);
          RETURN new
        END
      ELSE
        Debug.Warning(F("not found : %s . %s", FormatPathNode(q), nm));
        RETURN NIL
      END
    END
  END Next;

PROCEDURE FormatPathNode(p : PathNode) : TEXT =
  BEGIN
    IF p.up = NIL THEN
      RETURN ""
    ELSE
      RETURN FormatPathNode(p.up) & "." & p.nm
    END
  END FormatPathNode;
  
PROCEDURE MakePaths(nm : TEXT; new := TRUE) : PathNode =
  VAR
    p := 0;
    q := paths;
    n := Text.Length(nm);
  BEGIN
    FOR i := 0 TO n - 1 DO
      WITH c = Text.GetChar(nm, i) DO
        IF c = '.' THEN
          WITH ppfx = Text.Sub(nm, p, i - p) DO
            q := Next(q, ppfx, new);
            IF NOT new AND q = NIL THEN RETURN q END;
            p := i + 1
          END
        END
      END
    END;
    WITH ppfx = Text.Sub(nm, p) DO
      q := Next(q, ppfx, new);
      IF NOT new AND q = NIL THEN RETURN q END;
    END;
    RETURN q
  END MakePaths;

PROCEDURE RecordPathE(m : Measurement) =
  VAR
    p := MakePaths(m.pfx);
  BEGIN
    p.e := p.e + m.E
  END RecordPathE;

PROCEDURE ppLR(kw : TEXT) : LONGREAL =
  BEGIN
    IF NOT pp.keywordPresent(kw) THEN
      Debug.Error("Must specify " & kw)
    END;
    RETURN pp.getNextLongReal()
  END ppLR;

PROCEDURE MapName(nm : TEXT) : TEXT =
  VAR
    i := 0;
  BEGIN
    WHILE Text.GetChar(nm, i) = 'X' DO INC(i) END;
    IF i # 0 THEN nm := Text.Sub(nm, i) END;
    RETURN NameControl.Gds2Cast(nm)
  END MapName;
  
PROCEDURE VisitSpice(ckt  : SpiceCircuit.T;
                     path : TEXT)
  (* path contains path so far, including trailing dot *)
  RAISES { Wr.Failure } =
  VAR
    elems := ckt.elements;
    type : SpiceCircuit.T;
  BEGIN
    FOR i := 0 TO elems.size() - 1 DO
      
      WITH elem = elems.get(i) DO
        TYPECASE elem OF
          SpiceObject.X (x) =>
          WITH castName = MapName(x.name),
               fqn      = path & "X" & castName DO

            IF TRUE THEN
              Debug.Out(F("X object nm %s type %s", fqn, x.type))
            END;
            
            EVAL hierCells.put(fqn, x.type);

            RecordTypeInstance(x.type, fqn);

            WITH hadIt = spice.subCkts.get(x.type, type) DO
              <*ASSERT hadIt*>
            END;

            VisitSpice(type, fqn & ".")
          END
        ELSE
          (* skip *)
        END
      END
    END
          
  END VisitSpice;

VAR
  pp            := NEW(ParseParams.T).init(Stdio.stderr);
  traceFn        : Pathname.T;
  frTime, toTime : LONGREAL;
  freq           : LONGREAL;
  maxDevs        := LAST(CARDINAL);
  spiceFn        : Pathname.T := NIL;
  spice          : SpiceFormat.T := NIL;
  rootType       : TEXT := NIL;
  dutName               := "";
  hierCells      := NEW(TextTextTbl.Default).init();
  
BEGIN
  TRY
    IF pp.keywordPresent("-trace") THEN    
      traceFn := pp.getNext()
    END;

    IF pp.keywordPresent("-spice") THEN    
      spiceFn := pp.getNext();

      IF pp.keywordPresent("-root") THEN    
        rootType := pp.getNext()
      END;

      IF pp.keywordPresent("-dut") THEN
        dutName := pp.getNext()
      END
      
    END;

    IF pp.keywordPresent("-maxdevs") THEN
      maxDevs := pp.getNextInt()
    END;

    frTime := ppLR("-from");
    toTime := ppLR("-to");
    freq   := ppLR("-freq");
    
    pp.skipParsed();
    pp.finish()
  EXCEPT
    ParseParams.Error =>
    Debug.Error("Can't parse command-line parameters\nUsage: " & Params.Get(0) & " " & Usage)
  END;

  IF spiceFn # NIL THEN
    TRY
      Debug.Out("Parsing spice deck...");
      WITH spiceRd = FileRd.Open(spiceFn) DO
        spice := SpiceFormat.ParseSpice(spiceRd, ".", spiceFn);
        Rd.Close(spiceRd)
      END
    EXCEPT
      OSError.E(e) =>
      Debug.Error(F("Can't open top level file %s : OSError.E : %s",
                    spiceFn, AL.Format(e)))
    |
      SpiceError.E(e) =>
      Debug.Error(F("Parsing input : caught SpiceError.E : %s at line %s of file %s",
                    e.msg, Int(e.lNo), Debug.UnNil(e.fn)))
    END
  END;

  IF spice # NIL THEN
    Debug.Out("Visiting spice tree...");
    VAR
      rootCkt : SpiceCircuit.T;
    BEGIN
      IF rootType = NIL THEN
        rootCkt := spice.topCkt
      ELSE
        WITH hadIt = spice.subCkts.get(rootType, rootCkt) DO
          IF NOT hadIt THEN
            Debug.Error(F("Unknown root type %s", rootType))
          END
        END
      END;

      WITH xname = dutName,
           xtype = rootCkt.name DO
        IF TRUE THEN
          Debug.Out(F("X object nm %s type %s", xname, xtype))
        END;

        EVAL hierCells.put(xname, xtype);

        RecordTypeInstance(xtype, xname);

      END;

      IF TE(dutName, "") THEN
        VisitSpice(rootCkt, "")
      ELSE
        VisitSpice(rootCkt, dutName & ".")
      END
    END
  END;

  Debug.Out("Loading trace...");
  VAR
    trace := NEW(Trace.T).init(traceFn);
  BEGIN
    DoReports(trace)
  END
END PowerAnal.
