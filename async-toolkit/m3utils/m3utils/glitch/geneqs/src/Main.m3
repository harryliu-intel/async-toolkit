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

CONST TE = Text.Equal;

PROCEDURE ParseFunction(fxn : TEXT) : Gate.T =
  VAR
    res : Gate.T;
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
    
  PROCEDURE Get(VAR t : TEXT) =
    VAR
      s : CHAR;
    BEGIN
      WITH got = tokenizer.token(t, s) DO
        <*ASSERT got*>
      END
    END Get;

  PROCEDURE MustGet(tokMustB : TEXT; sepMustB : CHAR) =
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

PROCEDURE ParseCircuit(rd : Rd.T) =
  BEGIN
  END ParseCircuit;
  
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
