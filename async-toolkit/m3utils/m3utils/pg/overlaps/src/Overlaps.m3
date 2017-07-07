MODULE Overlaps EXPORTS Main;
IMPORT CSVParse, ParseParams, Stdio, TextUtils, Debug, Rd, FileRd;
IMPORT AL;
IMPORT Pathname, OSError, Thread, Fmt, Text;
FROM Fmt IMPORT F, Int;
IMPORT Word;
IMPORT BitSpec, BitSpecSeq;
IMPORT Scan;
IMPORT FloatMode, Lex;
IMPORT Wr;
IMPORT TextSetDef;
IMPORT Process;

<*FATAL Thread.Alerted*>

TYPE
  Address = BitSpecSeq.T;

  Field  = { Name, Pattern };

CONST
  FieldNames = ARRAY Field OF TEXT { "name", "pattern" };

PROCEDURE IsDigit(c : CHAR): BOOLEAN =
  BEGIN RETURN '0' <= c AND c <= '9' END IsDigit;

CONST White = SET OF CHAR { ' ', '\t' };
      
PROCEDURE IsWhite(c : CHAR) : BOOLEAN =
  BEGIN RETURN c IN White END IsWhite;
  
PROCEDURE ParseLiteral(txt : TEXT) : Address =

  PROCEDURE Peek(q : CARDINAL) : CHAR =
    BEGIN
      RETURN Text.GetChar(txt, q)
    END Peek;
    
  BEGIN
    Debug.Out(F("Parsing literal \"%s\"", txt));
    TRY
      WITH attempt = Scan.Unsigned(txt, defaultBase := 10) DO
        RETURN LiteralInt(attempt)
      END
    EXCEPT
      Lex.Error, FloatMode.Trap => (*skip*)
    END;
    (* not decimal *)
    VAR
      p := 0;
      len := Text.Length(txt);
    BEGIN
      (* eat whitespace, if any *)
      WHILE IsWhite(Peek(p)) DO INC(p) END;
      (* eat width specifier, if any *)
      WHILE IsDigit(Peek(p)) DO INC(p) END;

      <*ASSERT Peek(p) = '\''*>
      INC(p);
      CASE Peek(p) OF
        'h' => RETURN LiteralInt(Scan.Unsigned(Text.Sub(txt, p+1),
                                               defaultBase := 16))
      |
        'b' =>
        VAR
          seq := NEW(Address).init();
        BEGIN
          FOR i := p+1 TO len-1 DO
            CASE Text.GetChar(txt,i) OF
              '0' => seq.addlo(BitSpec.T.B0)
            |
              '1' => seq.addlo(BitSpec.T.B1)
            |
              '?' => seq.addlo(BitSpec.T.BQ)
            END
          END;
          RETURN seq
        END
      END
    END
  END ParseLiteral;

PROCEDURE LiteralInt(w : Word.T) : Address =
  VAR
    res := NEW(Address).init();
  BEGIN
    IF w = 0 THEN
      (* special case so that zero doesnt have zero width *)
      res.addhi(BitSpec.T.B0);
      RETURN res
    END;
    
    WHILE w # 0 DO
      res.addhi(VAL(Word.Extract(w,0,1),BitSpec.T));
      w := Word.RightShift(w, 1)
    END;
    RETURN res
  END LiteralInt;

VAR widest := ARRAY Field OF CARDINAL { 0, .. };

TYPE
  Data = BRANDED OBJECT END;

  SplitD = ARRAY [0..1] OF Data;
  
  Split = Data OBJECT
    down := SplitD { NIL, NIL };
  END;
  
  Register = Data OBJECT
    nm, patString : TEXT;
    pattern : Address;
  END;

VAR
  tab : Data := NIL;
  msb : [-1..LAST(CARDINAL)] := -1;
  
PROCEDURE AddRegister(r : Register) =
  VAR
    regMsb := r.pattern.size()-1;
  BEGIN
    Debug.Out("AddRegister regMsb=" & Int(regMsb));
    WHILE regMsb > msb DO
      tab := NEW(Split, down := SplitD { tab, NIL });
      INC(msb)
    END;
    VAR
      q := tab;
      z := msb;
    BEGIN
      WHILE z > regMsb DO
        q := NARROW(q,Split).down[0];
        DEC(z)
      END;
      <*ASSERT z = regMsb*>
      Recurse(q,r,z)
    END
  END AddRegister;

VAR overlaps := NEW(TextSetDef.T).init();

PROCEDURE Recurse(q : Data; r : Register; z : CARDINAL) =

  PROCEDURE Do(p : [0..1]) =
    VAR
      s := NARROW(q,Split);
    BEGIN
      IF z = 0 THEN
        IF s.down[p] # NIL THEN
          WITH o = NARROW(s.down[p],Register) DO
            EVAL overlaps.insert(
                     F("OVERLAP %s (%s) <->\n"&
                       "        %s (%s)",
                       o.nm, o.patString,
                       r.nm, r.patString))
          END
        ELSE
          s.down[p] := r
        END
      ELSE
        IF s.down[p] = NIL THEN
          s.down[p] := NEW(Split)
        END;
        Recurse(s.down[p], r, z-1)
      END
    END Do;
    
  BEGIN
    WITH b = r.pattern.get(z) DO
      IF b IN SET OF BitSpec.T { BitSpec.T.B0, BitSpec.T.BQ } THEN
        Do(0)
      END;
      IF b IN SET OF BitSpec.T { BitSpec.T.BQ, BitSpec.T.B1 } THEN
        Do(1)
      END;
    END
  END Recurse;
    
PROCEDURE ProcessBuf(b : ARRAY Field OF TEXT) =
  BEGIN
    IF Debug.GetLevel() >= 100 THEN
      FOR f := FIRST(Field) TO LAST(Field) DO
        Debug.Out(F("ProcessBuf : %s : %s", FieldNames[f], b[f]))
      END
    END;

    FOR f := FIRST(Field) TO LAST(Field) DO
      WITH l = Text.Length(b[f]) DO
        IF l > widest[f] THEN
          Debug.Out(F("widest %s <- %s", FieldNames[f], b[f]));
          widest[f] := l
        END
      END
    END;

    WITH pattern  = ParseLiteral(b[Field.Pattern]),
         register = NEW(Register,
                        nm        := b[Field.Name],
                        pattern   := pattern,
                        patString := b[Field.Pattern]) DO
      AddRegister(register)
    END
  END ProcessBuf;

VAR
  buf : ARRAY Field OF TEXT;
  csv : CSVParse.T;
  fn : Pathname.T;
  rd : Rd.T;
BEGIN
  TRY
    WITH pp = NEW(ParseParams.T).init(Stdio.stderr) DO
      pp.skipParsed();
      fn := pp.getNext();
      pp.finish();
    END;
  EXCEPT
    ParseParams.Error => Debug.Error("Command-line params wrong.")
  END;
  
  TRY
    rd := FileRd.Open(fn);
    csv := NEW(CSVParse.T).init(rd);
    
    LOOP
      TRY
        csv.startLine();
        FOR i := FIRST(Field) TO LAST(Field) DO
          buf[i] := csv.cell()
        END;
        IF NOT TextUtils.HavePrefix(buf[FIRST(buf)], "//") THEN
          ProcessBuf(buf)
        END
      EXCEPT
        CSVParse.EndOfLine => (* wrong syntax *)
      END
    END
  EXCEPT
    Rd.Failure(x) =>
    Debug.Error("I/O error while reading input : Rd.Failure : "& AL.Format(x))
  |
    OSError.E(x) =>
    Debug.Error("Error while opening input : OSError.E : "& AL.Format(x))
  |
    Rd.EndOfFile =>
    (* done *)
    TRY Rd.Close(rd) EXCEPT ELSE <*ASSERT FALSE*> END
  END;

  Wr.PutText(Stdio.stdout, Fmt.Int(overlaps.size()) & " overlaps\n");
  VAR
    iter := overlaps.iterate();
    msg : TEXT;
  BEGIN
    WHILE iter.next(msg) DO
      Wr.PutText(Stdio.stdout, msg);
      Wr.PutChar(Stdio.stdout, '\n')
    END;
    Wr.Close(Stdio.stdout)
  END;
  IF overlaps.size() # 0 THEN 
    Process.Exit(1)
  END 
END Overlaps.
