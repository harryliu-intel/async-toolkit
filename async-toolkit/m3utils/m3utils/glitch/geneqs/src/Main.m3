MODULE Main;
IMPORT Stdio, ParseParams;
IMPORT OSError;
IMPORT Rd;
IMPORT FileRd;
IMPORT AL;
IMPORT Debug;
FROM Fmt IMPORT F;
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

CONST TE = Text.Equal;

PROCEDURE ParseFunction(fxn : TEXT) : Gate.T =
  VAR
    lexer := NEW(gateLexStd.T);
    parser := NEW(gateParseStd.T);
    rd := NEW(TextRd.T).init(fxn);
  BEGIN
    EVAL lexer.setRd(rd);
    EVAL parser.setLex(lexer);
    EVAL parser.parse();
    
    RETURN parser.gate
  END ParseFunction;
  
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
        <*ASSERT got*>
        <*ASSERT TE(t, tokMustB)*>
        <*ASSERT s = sepMustB*>
      END
    END MustGet;
    
  BEGIN
    WHILE tokenizer.token(tok, sep) DO
      IF    TE(tok, "Pin") AND sep = ':' THEN
        Get(pin);
        MustGet("Net", ':');
        Get(net);
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

          IF NOT typeGates.get(typ, gat) THEN
            gat := ParseFunction(fxn);
            Debug.Out(F("parsed type %s : %s", typ, Gate.Format(gat)));
            EVAL typeGates.put(typ, gat)
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
  
PROCEDURE ParseCircuit(rd : Rd.T) RAISES { Rd.Failure, Thread.Alerted } =
  VAR
    first : TEXT := NIL;
    from, to, last : TEXT;
    path : EdgeSeq.T;
    paths : RefList.T;
  BEGIN
    TRY
      LOOP
        WITH line = Rd.GetLine(rd) DO
          IF    EmptyLine(line) THEN
            Debug.Out(F("got path from %s -> %s", first, last));
            first := NIL;
            last := NIL;
            paths := RefList.Cons(path, paths);
            path := NIL;
          ELSIF TE(GetToken(line, 0) , "AFIFO") THEN
            path := NEW(EdgeSeq.T).init();
          ELSIF EndsIn(line, "->") THEN
            from := LookupPin(GetToken(line, 0));
            IF first = NIL THEN
              first := from
            END;
            WITH line2 = Rd.GetLine(rd) DO
              to := LookupPin(GetToken(line2, 0));
              last := to;
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
    END
  END ParseCircuit;

PROCEDURE LookupPin(pin : TEXT) : TEXT =
  VAR
    net : TEXT;
  BEGIN
    WITH haveIt = pinNets.get(pin, net) DO
      IF NOT haveIt THEN Debug.Error("No mapping for pin " & pin) END;
      RETURN net
    END
  END LookupPin;
  
VAR
  pp      := NEW(ParseParams.T).init(Stdio.stderr);
  fRd, aRd : Rd.T;
  pinNets, cellTypes := NEW(TextTextTbl.Default).init();
  typeGates := NEW(TextGateTbl.Default).init();
BEGIN
  TRY
    IF NOT pp.keywordPresent("-f") THEN
      Debug.Error("No -f")
    END;
    WITH fn = pp.getNext() DO
      TRY
        fRd := FileRd.Open(fn)
      EXCEPT
        OSError.E(x) =>
        Debug.Error(F("Trouble opening file \"%s\" : OSError.E : %s",
                      fn,
                      AL.Format(x)))
      END
    END;

    IF NOT pp.keywordPresent("-a") THEN
      Debug.Error("No -a")
    END;
    WITH fn = pp.getNext() DO
      TRY
        aRd := FileRd.Open(fn)
      EXCEPT
        OSError.E(x) =>
        Debug.Error(F("Trouble opening file \"%s\" : OSError.E : %s",
                      fn,
                      AL.Format(x)))
      END
    END;

  EXCEPT
    ParseParams.Error => Debug.Error("Can't parse command line")
  END;

  ParseAliases(aRd);

  ParseCircuit(fRd)
  
END Main.
