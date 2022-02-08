MODULE Main;
IMPORT Stdio, ParseParams;
IMPORT OSError;
IMPORT Rd;
IMPORT FileRd;
IMPORT AL;
IMPORT Debug;
FROM Fmt IMPORT F, Int;
IMPORT Aliases;
IMPORT Text;
IMPORT Thread;
IMPORT TextTextTbl;
IMPORT TextGateTbl;
IMPORT Gate;
IMPORT TextRd;
IMPORT gateLexStd;
IMPORT gateParseStd;
IMPORT Edge;
IMPORT EdgeSeq;
IMPORT RefList;
IMPORT TextReader;
IMPORT TextList;
IMPORT TextSet, TextSetDef;
IMPORT GateExpr;
IMPORT Pathname;
IMPORT Wr;
IMPORT TextUtils;
IMPORT FileWr;
IMPORT Wx;
IMPORT FmtTime, Time, Date;
IMPORT Params;
IMPORT TextSeq;

CONST TE = Text.Equal;

EXCEPTION SyntaxError;

PROCEDURE MyLexError(<*UNUSED*>lexer : gateLexStd.T;
                     message         : TEXT) =
  BEGIN
    Debug.Warning("Syntax error in gate lexer: " & message) 
  END MyLexError;
  
PROCEDURE ParseFunction(fxn : TEXT) : Gate.T RAISES { SyntaxError } =
  VAR
    lexer  := NEW(gateLexStd.T, error := MyLexError);
    parser := NEW(gateParseStd.T);
    rd     := NEW(TextRd.T).init(fxn);
  BEGIN
    EVAL lexer.setRd(rd);
    EVAL parser.setLex(lexer);
    WITH res = parser.parse() DO
      IF res = NIL THEN RAISE SyntaxError END
    END;

    IF parser.gate.expr = NIL THEN
      (* happens for ANTENNA *)
      RAISE SyntaxError
    END;
    parser.gate.fanins := GateExpr.Fanins(parser.gate.expr);
    
    RETURN parser.gate
  END ParseFunction;

<*UNUSED*>
PROCEDURE OldUnbrace(txt : TEXT) : TEXT =
  BEGIN
    WITH n = Text.Length(txt),
         f = Text.GetChar(txt, 0),
         l = Text.GetChar(txt, n - 1) DO
      IF n # 0 AND f = '{' AND l = '}' THEN
        RETURN Text.Sub(txt, 1, n - 2)
      ELSE
        RETURN txt
      END
    END
  END OldUnbrace;

PROCEDURE Unbrace(txt : TEXT) : TEXT =
  BEGIN
    RETURN TextUtils.FilterOut(txt, remove := SET OF CHAR { '{', '}' })
  END Unbrace;
  
PROCEDURE ParseAliases(rd : Rd.T) RAISES { Rd.Failure, Thread.Alerted } =
  VAR
    tokenizer := NEW(Aliases.Tokenizer).init(rd);
    tok : TEXT;
    sep : CHAR;
    pin, net, old, fxn, nam, typ : TEXT := NIL;
    gat : Gate.T;
    
  PROCEDURE Get(VAR t : TEXT)
    RAISES { Rd.Failure, Thread.Alerted } =
    VAR
      s : CHAR;
    BEGIN
      WITH got = tokenizer.token(t, s) DO
        <*ASSERT got*>
      END
    END Get;

  PROCEDURE MustGet(tokMustB : TEXT; sepMustB : CHAR)
    RAISES { Rd.Failure, Thread.Alerted } =
    VAR
      t : TEXT;
      s : CHAR;
    BEGIN
      WITH got = tokenizer.token(t, s) DO
        IF NOT got THEN
          Debug.Error(F("ParseAliases.MustGet: expecting token %s at line %s, nothing read",
                        tokMustB, Int(tokenizer.whatLine())))
        END;

        IF NOT TE(t, tokMustB) THEN
          Debug.Error(F("ParseAliases.MustGet: expecting token %s at line %s, got %s",
                        tokMustB, Int(tokenizer.whatLine()), t))
        END;

        IF s # sepMustB THEN
          Debug.Error(F("ParseAliases.MustGet: expecting separator %s at line %s, got %s",
                        Text.FromChar(sepMustB),
                        Int(tokenizer.whatLine()),
                        Text.FromChar(s)))
        END
      END
    END MustGet;
    
  BEGIN
    WHILE tokenizer.token(tok, sep) DO
      IF    TE(tok, "Pin") AND sep = ':' THEN
        Get(pin);
        MustGet("Net", ':');
        Get(net);
        net := Unbrace(net);
        Debug.Out(F("alias pin %s net %s", pin, net));
        IF pinNets.get(pin, old) AND NOT TE(old, net) THEN
          Debug.Error(F("Ambiguous pin->net mapping.  pin %s maps to both net %s and net %s!", pin, old, net))
        END;
        EVAL pinNets.put(pin, net);
        IF fxn # NIL THEN
          <*ASSERT nam # NIL*>
          <*ASSERT typ # NIL*>

          IF cellTypes.get(nam, old) AND NOT TE(old, typ) THEN
            Debug.Error(F("Ambiguous cell->type mapping.  cell %s maps to both type %s and type %s!", nam, old, typ))
          END;
          EVAL cellTypes.put(nam, typ);

          IF NOT typeGates.get(typ, gat) THEN
            TRY
              gat := ParseFunction(fxn);
              Debug.Out(F("parsed type %s : %s", typ, Gate.Format(gat)));
            EXCEPT
              SyntaxError =>
              gat := Gate.T { NIL, NIL, NIL };
              Debug.Warning("Couldn't parse function " & fxn)
            END;
            EVAL typeGates.put(typ, gat)
            (* puts a gate with NILs if parsefunction fails *)
          END;
          
          fxn := NIL;
          nam := NIL;
          typ := NIL;
        END
      ELSIF TE(tok, "Function") AND sep = ':' THEN
        Get(fxn);
        Debug.Out(F("fxn %s", fxn))
      ELSIF TE(tok, "Name") AND sep = ' ' THEN
        Get(nam);
        nam := Unbrace(nam);
        Debug.Out(F("name %s", nam))
      ELSIF TE(tok, "Type") AND sep = ' ' THEN
        Get(typ);
        Debug.Out(F("type %s", typ))
      ELSIF FALSE THEN
      END
    END
  END ParseAliases;

CONST
  CktDelims = " \t\n\r";
  CktDelimSet = SET OF CHAR { ' ', '\t', '\n', '\r' };

PROCEDURE GetToken(txt : TEXT; idx : CARDINAL) : TEXT =
  BEGIN
    WITH reader = NEW(TextReader.T).init(txt),
         lst    = reader.shatter(CktDelims, "", TRUE) DO
      RETURN TextList.Nth(lst, idx)
    END
  END GetToken;

PROCEDURE EmptyLine(line : TEXT) : BOOLEAN =
  BEGIN
    FOR i := 0 TO Text.Length(line) - 1 DO
      IF NOT Text.GetChar(line, i) IN CktDelimSet THEN
        RETURN FALSE
      END
    END;
    RETURN TRUE
  END EmptyLine;

PROCEDURE EndsIn(line, sfx : TEXT) : BOOLEAN = 
  BEGIN
    WITH reader = NEW(TextReader.T).init(line),
         lst    = reader.shatter(CktDelims, "", TRUE),
         n      = TextList.Length(lst) DO
      RETURN n # 0 AND TE(TextList.Nth(lst, n - 1), sfx)
    END
  END EndsIn;

TYPE
  Problem = RECORD
    asyncs  : TextSet.T;
    outputs : TextSet.T;
    paths   : RefList.T;
  END;
  
PROCEDURE ParseCircuit(rd : Rd.T; fn : Pathname.T) : Problem
  RAISES { Rd.Failure, Thread.Alerted } =
  VAR
    first : TEXT := NIL;
    from, to, last, firstNet, lastNet, fromNet, toNet : TEXT;
    path : EdgeSeq.T;
    paths : RefList.T;
    asyncs, outputs := NEW(TextSetDef.T).init();
    lNo : CARDINAL := 0;
  BEGIN
    Debug.Out("ParseCircuit...");
    TRY
      LOOP
        WITH line = Rd.GetLine(rd) DO
          INC(lNo);
          IF    EmptyLine(line) THEN
            IF first = NIL THEN
              Debug.Warning(F("parsing circuit, %s:%s, no first in path",
                            fn, Int(lNo)))
            ELSE
              (* first is non-NIL *)
              IF last = NIL THEN
                Debug.Error(F("parsing circuit, %s:%s, no last in path",
                              fn, Int(lNo)))
              END;
            
              Debug.Out(F("got path from %s -> %s", first, last));
              EVAL asyncs.insert(first);
              EVAL outputs.insert(last);
              first    := NIL;
              last     := NIL;
              paths    := RefList.Cons(path, paths);
              path     := NIL;
              firstNet := NIL;
              lastNet  := NIL;
            END
          ELSIF TE(GetToken(line, 0) , "AFIFO") THEN
            path := NEW(EdgeSeq.T).init();
          ELSIF EndsIn(line, "->") THEN
            fromNet := GetToken(line, 0);
            from    := LookupPinNet(fromNet);
            IF first = NIL THEN
              firstNet := fromNet;
              first := from;
            END;
            WITH line2 = Rd.GetLine(rd) DO
              INC(lNo);
              toNet   := GetToken(line2, 0);
              to      := LookupPinNet(toNet);
              lastNet := toNet;
              last    := to;
            END;
            Debug.Out(F("edge %s -> %s", from, to));
            WITH edge = Edge.T { from, to } DO
              path.addhi(edge);
            END
          END
        END
      END
    EXCEPT
      Rd.EndOfFile => (* skip *)
    END;

    Debug.Out(F("%s outputs", Int(outputs.size())));
    Debug.Out(F("%s async inputs", Int(asyncs.size())));

    RETURN Problem { asyncs, outputs, paths }
  END ParseCircuit;

PROCEDURE LookupTypeGate(typ : TEXT) : Gate.T =
  VAR
    gat : Gate.T;
  BEGIN
    WITH hadIt = typeGates.get(typ, gat) DO
      IF NOT hadIt THEN Debug.Error("No definition for gate type " & typ) END;
      RETURN gat
    END
  END LookupTypeGate;
  
PROCEDURE AnalyzeCircuit(READONLY problem : Problem)
  RAISES { Thread.Alerted } =
  VAR
    iter := problem.outputs.iterate();
    op : TEXT;
    citer := cellTypes.iterate();
    cellOutputs := NEW(TextTextTbl.Default).init();
    typ : TEXT;
    net : TEXT;
    nam : TEXT;
  BEGIN
    (* build lookup table for outputs -> gates *)
    WHILE citer.next(nam, typ) DO
      WITH gate    = LookupTypeGate(typ) DO
        IF gate.tgt # NIL THEN
          WITH gateOut = nam & "/" & gate.tgt,
               haveIt  = pinNets.get(gateOut, net) DO
            IF NOT haveIt THEN
              Debug.Error("No net for gate output " & gateOut)
            END;
            Debug.Out(F("Gate instance %s, type %s : output %s = %s",
                        nam, typ, gateOut, net));
            EVAL cellOutputs.put(net, nam)
          END
        END
      END
    END;
    
    WHILE iter.next(op) DO
      AnalyzeOutput(op, problem.asyncs, cellOutputs)
    END
  END AnalyzeCircuit;

PROCEDURE AnalyzeOutput(outp        : TEXT;
                        asyncs      : TextSet.T;
                        outputCells : TextTextTbl.T)
  RAISES { Thread.Alerted } =
  BEGIN
    Debug.Out("Analyzing output " & outp);
    IF asyncs.member(outp) THEN
      Debug.Error(F("output %s member of async set, not sensible!", outp))
    END;


    (* find fanins of output *)
    WITH fanins = FaninCone(outp, outputCells, asyncs) DO
      ProduceGlitchFile(outp, fanins, asyncs)
    END
  END AnalyzeOutput;

PROCEDURE ProduceGlitchFile(outp   : TEXT;
                            fanins : GlitchSpec;
                            asyncs : TextSet.T)
  RAISES { Thread.Alerted } =
  VAR
    ofn := outDir & "/" & TextUtils.Replace(outp, "/", "__") & ".glitch";
    wr : Wr.T;
  BEGIN
    TRY
      wr := FileWr.Open(ofn);
    EXCEPT
      OSError.E(x) =>
        Debug.Error(F("Trouble opening file \"%s\" for writing : OSError.E : %s",
                      ofn,
                      AL.Format(x)))
    END;

    TRY
      Wr.PutText(wr,
                 F("# glitch file\n" &
                 "# %s\n" &
                 "# %s\n" &
                 "# %s\n\n",
                 outp,
                 FmtParams(),
                 FmtDate()));

      Wr.PutText(wr, F("output %s\n\n", outp));

      VAR
        iter := fanins.leafNodes.iterate();
        n : TEXT;
      BEGIN
        WHILE iter.next(n) DO
          IF asyncs.member(n) THEN
            Wr.PutText(wr, F("async %s\n", n))
          END
        END;
        Wr.PutChar(wr, '\n');
      END;

      VAR
        iter := fanins.gateInstances.iterate();
        nam : TEXT;
      BEGIN
        WHILE iter.next(nam) DO
          WITH type = LookupCellType(nam),
               gate = LookupTypeGate(type) DO
            IF TE(gate.tgt, "Q") THEN
              Wr.PutText(wr, F("# skipped flop %s / %s\n\n", nam, type))
            ELSE
              Wr.PutText(wr, F("# %s / %s\n", nam, type));
              Wr.PutText(wr, "# " & Gate.Format(gate));
              Wr.PutChar(wr, '\n');
              Wr.PutText(wr,
                         F("gate %s\n",
                           Gate.Format(RenameTerminals(gate, nam),
                                       ass := " = ",
                                       not := "~")));
              Wr.PutChar(wr, '\n');
            END
          END
        END
      END;

      Wr.PutChar(wr, '\n');
      Wr.PutText(wr, "# END GLITCH PROBLEM\n");
      
      Wr.Close(wr)

    EXCEPT
      Wr.Failure(x) =>
      Debug.Error("I/O error writing output file " & ofn & " : Wr.Failure : " & AL.Format(x))
    END
  END ProduceGlitchFile;

PROCEDURE RenameTerminals(g       : Gate.T;
                          nam     : TEXT) : Gate.T =

  PROCEDURE RenamePin(n : TEXT) : TEXT =
    VAR
      fqpn := nam & "/" & n;
    BEGIN
      RETURN LookupPinNet(fqpn)
    END RenamePin;

  PROCEDURE RenameExpr(x : GateExpr.T) : GateExpr.T =
    BEGIN
      TYPECASE x OF
        GateExpr.Expr(x) =>
        CASE x.op OF
          GateExpr.Op.And, GateExpr.Op.Or, GateExpr.Op.Xor =>
          RETURN NEW(GateExpr.Expr,
                     nm := x.nm,
                     op := x.op,
                     a := RenameExpr(x.a),
                     b := RenameExpr(x.b))
        |
          GateExpr.Op.Not =>
          RETURN NEW(GateExpr.Expr,
                     nm := x.nm,
                     op := x.op,
                     a := RenameExpr(x.a),
                     b := x.b)
          
        END
      |
        GateExpr.Named(n) => RETURN GateExpr.New(RenamePin(n.nm))
      ELSE
        <*ASSERT FALSE*>
      END
    END RenameExpr;
    
  VAR
    res : Gate.T;
  BEGIN
    res.tgt    := RenamePin(g.tgt);
    res.expr   := RenameExpr(g.expr);
    res.fanins := GateExpr.Fanins(res.expr);
    RETURN res
  END RenameTerminals;

PROCEDURE FmtParams() : TEXT =
  VAR
    wx := Wx.New();
  BEGIN
    FOR i := 0 TO Params.Count - 1 DO
      Wx.PutChar(wx, ' ');
      Wx.PutText(wx, Params.Get(i))
    END;
    RETURN Wx.ToText(wx)
  END FmtParams;

VAR
  startTime := Time.Now();
  
PROCEDURE FmtDate() : TEXT =
  BEGIN
    RETURN FmtTime.Long(startTime, Date.Local)
  END FmtDate;
  
TYPE
  GlitchSpec = RECORD
    gateInstances : TextSet.T;
    leafNodes     : TextSet.T;
  END;

PROCEDURE FaninCone(net         : TEXT;
                    outputCells : TextTextTbl.T;
                    asyncs      : TextSet.T) : GlitchSpec =
  (* return set of gate-instance names in fanin cone of given net *)

  CONST
    MaxDepth = 4000;
    
  VAR
    res := GlitchSpec { NEW(TextSetDef.T).init(),
                        NEW(TextSetDef.T).init() };

    errorSeq := NEW(TextSeq.T).init();

    seen := NEW(TextSetDef.T).init();

  PROCEDURE Recurse(n : TEXT; depth : CARDINAL) : BOOLEAN =
    VAR
      cellNam : TEXT;
    BEGIN
      IF seen.insert(n) THEN
        (* we are looping, skip it *)
        RETURN TRUE
      END;
      
      IF depth > MaxDepth THEN RETURN FALSE END;
      
      WITH hadIt = outputCells.get(n, cellNam) DO
        
        (* a given circuit node is either the output of a cell
           OR it is an ultimate fanin *)
        
        IF NOT hadIt OR asyncs.member(n) THEN
          (* it's a leaf if it is either actually a leaf,
             or if it is an async input *)
          EVAL res.leafNodes.insert(n)
        ELSE
          EVAL res.gateInstances.insert(cellNam);
          
          WITH type  = LookupCellType(cellNam),
               gate  = LookupTypeGate(type) DO
            <*ASSERT gate.fanins # NIL*>
            <*ASSERT cellNam # NIL*>
            VAR
              iter := gate.fanins.iterate();
              f : TEXT;
            BEGIN
              WHILE iter.next(f) DO
                <*ASSERT f # NIL*>
                WITH fqp   = cellNam & "/" & f,
                     fiNet = LookupPinNet(fqp),
                     res   = Recurse(fiNet, depth + 1) DO
                  IF NOT res THEN
                    errorSeq.addlo(fiNet); errorSeq.addlo(fqp);
                    RETURN FALSE
                  END
                END
              END
            END
          END
        END
      END;
      RETURN TRUE
    END Recurse;
    
  BEGIN
    IF NOT Recurse(net, 0) THEN
      VAR
        wx := Wx.New();
      BEGIN
        Wx.PutText(wx, "TooDeep: recursion depth: ");
        FOR i := 0 TO errorSeq.size() - 1 DO
          Wx.PutText(wx, errorSeq.get(i));
          Wx.PutChar(wx, ' ')
        END;
        Debug.Error("FaninCone " & Wx.ToText(wx))
      END
    END;
    RETURN res
  END FaninCone;
  
PROCEDURE LookupPinNet(pin : TEXT) : TEXT =
  (* look up pin and return net *)
  VAR
    net : TEXT;
  BEGIN
    <*ASSERT pin # NIL*>
    WITH haveIt = pinNets.get(pin, net) DO
      IF NOT haveIt THEN Debug.Error("No mapping for pin " & pin) END;
      RETURN net
    END
  END LookupPinNet;

PROCEDURE LookupCellType(cell : TEXT) : TEXT =
  (* look up cell and return type *)
  VAR
    type : TEXT;
  BEGIN
    WITH haveIt = cellTypes.get(cell, type) DO
      IF NOT haveIt THEN Debug.Error("No type mapping for cell " & cell) END;
      RETURN type
    END
  END LookupCellType;
  
VAR
  pp      := NEW(ParseParams.T).init(Stdio.stderr);
  fRd, aRd : Rd.T;
  pinNets, cellTypes := NEW(TextTextTbl.Default).init();
  typeGates := NEW(TextGateTbl.Default).init();
  outDir : Pathname.T := ".";

  fFn, aFn : Pathname.T;
  
BEGIN
  TRY
    IF NOT pp.keywordPresent("-f") THEN
      Debug.Error("No -f")
    END;

    fFn := pp.getNext();
    TRY
      fRd := FileRd.Open(fFn)
    EXCEPT
      OSError.E(x) =>
      Debug.Error(F("Trouble opening file \"%s\" for reading : OSError.E : %s",
                    fFn,
                    AL.Format(x)))
    END;

    IF NOT pp.keywordPresent("-a") THEN
      Debug.Error("No -a")
    END;
    aFn := pp.getNext();
    TRY
      aRd := FileRd.Open(aFn)
    EXCEPT
      OSError.E(x) =>
      Debug.Error(F("Trouble opening file \"%s\" : OSError.E : %s",
                    aFn,
                    AL.Format(x)))
    END;

    IF pp.keywordPresent("-d") THEN
      outDir := pp.getNext()
    END;

  EXCEPT
    ParseParams.Error => Debug.Error("Can't parse command line")
  END;

  <*FATAL Thread.Alerted*>
  BEGIN
    TRY
      ParseAliases(aRd);
      
      WITH problem = ParseCircuit(fRd, fFn) DO
        AnalyzeCircuit(problem)
      END
    EXCEPT
      Rd.Failure(x) => Debug.Error("I/O error reading inputs : Rd.Failure : " &
        AL.Format(x))
    END
  END
END Main.
