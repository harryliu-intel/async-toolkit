MODULE Main;
FROM RegEx IMPORT Memory, Pattern;
IMPORT Stdio;
IMPORT CardSet;
IMPORT Rd; 
IMPORT CardSetDef;
IMPORT NumberedObject, NumberedObjectArraySort;
IMPORT RegEx;
IMPORT RefSeq;
IMPORT Text;
IMPORT Thread;
IMPORT Debug;
IMPORT Fmt; FROM Fmt IMPORT F;
IMPORT TextSeq;
IMPORT TextReader;
IMPORT TextTextTbl;
IMPORT Wr;
IMPORT TaggedToken, TaggedTokenSeq;
FROM TaggedToken IMPORT Token, TokenNames;
IMPORT Wx;

<*FATAL Rd.Failure, Thread.Alerted, Wr.Failure*>

CONST TE = Text.Equal;

TYPE
  TwoLine = OBJECT
    value : TEXT;
    second : TEXT;
  END;

VAR
  startpoint, endpoint := NEW(TwoLine);
  scenario, pathGroup, pathType := NEW(REF TEXT);

PROCEDURE ArgMatcher(a : REFANY) =
  BEGIN
    WITH m = mem[1],
         s = Text.Sub(line, m.start, m.stop-m.start) DO
      TYPECASE a OF
        REF TEXT(rt) => rt^ := s
      |
        TwoLine(tl) => 
        tl.value := s;
        tl.second := Rd.GetLine(rd)
      END
    END
  END ArgMatcher;

PROCEDURE MatchArg(label : TEXT; res : REFANY) =
  BEGIN
    Match(label & " *\\([^ ]*\\)", ArgMatcher, res);
  END MatchArg;

PROCEDURE GatherMatchers() =
  BEGIN
    MatchArg("Startpoint:", startpoint);
    MatchArg("Endpoint:", endpoint);
    MatchArg("Scenario:", scenario);
    MatchArg("Path Group:", pathGroup);
    MatchArg("Path Type:", pathType);
    Match   ("^ *Point", DoPoint);
    Match   ("Attributes:", DoAttributes);
  END GatherMatchers;

VAR attrs : TextTextTbl.T;

VAR emptyPat := RegEx.Compile("^ *$");
VAR attrPat  := RegEx.Compile("^ *\\([^ ]*\\) - \\(.*\\)$");

PROCEDURE IsEmptyLine(line : TEXT) : BOOLEAN = 
  BEGIN RETURN RegEx.Execute(emptyPat, line) >= 0 END IsEmptyLine;

PROCEDURE DoAttributes(<*UNUSED*>a:REFANY) =
  BEGIN
    attrs := NEW(TextTextTbl.Default).init();
    LOOP
      line := Rd.GetLine(rd);
      IF IsEmptyLine(line) THEN EXIT END;
      WITH s = RegEx.Execute(attrPat, line, mem := mem)  DO
        Debug.Out(F("line=\"%s\"", line));
        <*ASSERT s>=0*>
        WITH lab = Text.Sub(line, mem[1].start, mem[1].stop-mem[1].start),
             val = Text.Sub(line, mem[2].start, mem[2].stop-mem[2].start) DO
          Debug.Out(F("attrs.put(\"%s\",\"%s\")", lab, val));
          EVAL attrs.put(lab, val)
        END
      END
    END
  END DoAttributes;

VAR cols : TextSeq.T;

TYPE 
  TaggedPat = RECORD
    token : Token;
    pat   : RegEx.Pattern;
  END;

VAR TaggedPats := ARRAY Token OF TaggedPat {
  (* in precedence order *)
  MakePat(Token.Line, "--*"),
  MakePat(Token.Int, "[0-9][0-9]*"), 
  MakePat(Token.Fix, "[0-9\\-][0-9]*\\.[0-9][0-9]*"),
  MakePat(Token.ParenStuff, "(.*)"),
  MakePat(Token.String, ".*") 
};

TYPE
  Line  = TaggedTokenSeq.T;
  Table = RefSeq.T;

PROCEDURE MakePat(tok : Token; pattern : TEXT) : TaggedPat =
  BEGIN
    RETURN TaggedPat { tok, RegEx.Compile("^\\(" & pattern & "\\)$") }
  END MakePat;

PROCEDURE UnbalancedParens(str : TEXT) : BOOLEAN =
  VAR p := 0; BEGIN
    FOR i := 0 TO Text.Length(str)-1 DO
      WITH c = Text.GetChar(str, i) DO
        IF c = '(' THEN INC(p) ELSIF c = ')' THEN DEC(p) END
      END
    END;

    IF    p = 0 THEN RETURN FALSE
    ELSIF p > 0 THEN RETURN TRUE 
    ELSE             <*ASSERT FALSE*>
    END
       
  END UnbalancedParens;

PROCEDURE DoPoint(<*UNUSED*>a : REFANY) =

  PROCEDURE More() : BOOLEAN =
    BEGIN RETURN reader.next(" ", token, skipNulls := TRUE) END More;

  VAR
    reader   := NEW(TextReader.T).init(line);
    slackPat := RegEx.Compile("^ * slack .*$");
    token : TEXT;
    mem := NEW(REF Memory);
    t := NEW(Table).init();
    l : Line;
  BEGIN
    (* start a table *)

    cols := NEW(TextSeq.T).init();
    WHILE More() DO
      Debug.Out("Adding column \"" & token & "\"");
      cols.addhi(token)
    END;

    REPEAT
      line := Rd.GetLine(rd);
      Debug.Out("Table line \"" & line & "\"");
      (* tokenize ... *)

      l := NEW(Line).init();

      reader   := NEW(TextReader.T).init(line);
      
      WHILE More() DO
        WHILE UnbalancedParens(token) DO
          VAR save := token; BEGIN
            EVAL More(); (* hmmm, crash on unbalanced *)
            token := save & " " & token
          END
        END;

        IF Debug.GetLevel() > 9 THEN
          Debug.Out("Handling token \"" & token & "\"")
        END;
        WITH len = Text.Length(token) DO
          FOR i := FIRST(TaggedPats) TO LAST(TaggedPats) DO
            WITH s = RegEx.Execute(TaggedPats[i].pat, token, mem := mem) DO
              (*
              Debug.Out(F("testing against %s s=%s start=%s stop=%s len=%s",
                          TokenNames[i], Fmt.Int(s), Fmt.Int(mem[1].start),
                          Fmt.Int(mem[1].stop), Fmt.Int(len)));
              *)
              IF s>=0 AND mem[1].stop-mem[1].start = len THEN
                Debug.Out("token is " & TokenNames[i]);
                l.addhi(TaggedToken.T { token := i, str := token });
                EXIT
              END
            END
          END
        END
      END;
      t.addhi(l)
    UNTIL RegEx.Execute(slackPat, line) >= 0;
    
    DecorateTokens(t);
    DumpTable(t)
  END DoPoint;

PROCEDURE FindCol(str : TEXT) : INTEGER =
  BEGIN
    FOR i := 0 TO cols.size()-1 DO
      IF TE(cols.get(i), str) THEN RETURN i END
    END;
    RETURN -1
  END FindCol;

PROCEDURE IsAttr(attr : TEXT) : BOOLEAN =
  VAR dummy : TEXT; 
  BEGIN 
    IF attrs = NIL THEN RETURN FALSE ELSE RETURN attrs.get(attr,dummy) END
  END IsAttr;

PROCEDURE DecorateTokens(t : Table) =
  VAR
    mnf, nf := 0;
    pointCol := 0;
    fanoutCol := FindCol("Fanout");
    transCol := FindCol("Trans");
    incrCol := FindCol("Incr");
    attrCol := FindCol("Attributes");
    pathCol := FindCol("Path");

    cc, cf : CARDINAL;
  BEGIN
    FOR i := 0 TO t.size()-1 DO
      WITH l = NARROW(t.get(i),Line) DO
        nf := 0;
        FOR j := 0 TO l.size()-1 DO
          WITH tok = l.get(j) DO
            IF tok.token = Token.Fix THEN INC(nf) END;
          END
        END;
        mnf := MAX(mnf,nf)
      END
    END;
    
    (* mnf is max # of Fixed fields on a line *)

    FOR i := 0 TO t.size()-1 DO
      WITH l = NARROW(t.get(i),Line) DO
        nf := 0;
        FOR j := 0 TO l.size()-1 DO
          WITH tok = l.get(j) DO
            IF tok.token = Token.Fix THEN INC(nf) END;
          END
        END;

        cc := 0;
        cf := 0;
        FOR j := 0 TO l.size()-1 DO
          WITH tok = l.get(j) DO
            IF tok.token = Token.Int THEN
              cc := fanoutCol
            ELSIF tok.token = Token.Fix THEN
              IF cc = fanoutCol THEN 
                cc := transCol+ 1 
              ELSIF nf < mnf-1 THEN
                cc := MAX(pathCol - (nf - cf) +1,cc+1)
              ELSE
                cc := MAX(transCol,cc+1)
              END
            END;
            IF cc = pathCol AND tok.token = Token.String AND IsAttr(tok.str) THEN
              INC(cc)
            END;
            IF cc = attrCol AND tok.token = Token.Fix THEN
              INC(cc)
            END;

            IF tok.token = Token.Fix THEN INC(cf) END;

            VAR tok2 := tok; BEGIN tok2.col := cc; l.put(j,tok2) END

          END
        END
      END
    END
  END DecorateTokens;

PROCEDURE Zero(VAR a : ARRAY OF CARDINAL) =
  BEGIN FOR i := FIRST(a) TO LAST(a) DO a[i] := 0 END END Zero;

PROCEDURE CalcWidths(t : Table) : REF ARRAY OF CARDINAL =
  VAR
    tmp, res := NEW(REF ARRAY OF CARDINAL, cols.size());
  BEGIN
    FOR j := FIRST(res^) TO LAST(res^) DO
      res[j] := Text.Length(cols.get(j))
    END;
    FOR i := 0 TO t.size()-1 DO
      Zero(tmp^);
      WITH l = NARROW(t.get(i),Line) DO
        IF l.size() > 0 AND l.get(0).token # Token.Line THEN
          FOR j := 0 TO l.size()-1 DO
            WITH tok = l.get(j) DO
              INC(tmp[tok.col],Text.Length(tok.str)+1);
            END
          END;
          FOR j := FIRST(res^) TO LAST(res^) DO
            res[j] := MAX(res[j],tmp[j])
          END
        END
      END
    END;
    RETURN res
  END CalcWidths;

PROCEDURE DumpTable(t : Table) =

  PROCEDURE Push(newCol : CARDINAL) =
    BEGIN
      REPEAT
        Wx.ToWr(wx, Stdio.stdout);
        FOR i := Wx.GetLength(wx) TO w[col]-1 DO
          Wr.PutChar(Stdio.stdout, ' ')
        END;
        Wr.PutText(Stdio.stdout, " | ");
        Wx.Reset(wx);
        INC(col);
      UNTIL col = newCol
    END Push;

  VAR
    w := CalcWidths(t);
    wx := Wx.New();
    col : CARDINAL;
  BEGIN
    FOR j := 0 TO cols.size()-1 DO
      Wx.PutText(wx, cols.get(j));
      Push(j+1)
    END;
    Wr.PutChar(Stdio.stdout,'\n');
    FOR i := 0 TO t.size()-1 DO
      WITH l = NARROW(t.get(i),Line) DO
        col := 0;
        IF l.size() > 0 AND l.get(0).token # Token.Line THEN
          FOR j := 0 TO l.size()-1 DO
            WITH tok = l.get(j) DO
              IF tok.col # col THEN Push(tok.col) END;
              WITH tok = l.get(j) DO
                Wx.PutText(wx, tok.str);
                Wx.PutChar(wx, ' ')
              END
            END
          END
        END
      END;
      Push(cols.size()-1+1);
      Wr.PutChar(Stdio.stdout,'\n');
    END
  END DumpTable;

(**********************************************************************)

TYPE 
  P = PROCEDURE (a : REFANY);

  M = NumberedObject.T OBJECT 
    pat : Pattern;
    p   : P;
    a   : REFANY;
  END;

PROCEDURE Match(pat : TEXT; p : P; a : REFANY := NIL; seq := LAST(CARDINAL)) =
  BEGIN
    WITH m = NEW(M, pat := RegEx.Compile(pat), p := p, a := a, num := seq) DO
      mSeq.addhi(m)
    END
  END Match;

PROCEDURE Prepare() =
  BEGIN
    arr := NEW(REF ARRAY OF NumberedObject.T, mSeq.size());
    FOR i := FIRST(arr^) TO LAST(arr^) DO
      arr[i] := mSeq.get(i)
    END;
    NumberedObjectArraySort.Sort(arr^)
  END Prepare;

VAR
  rd   := Stdio.stdin;
  soFar  : CardSet.T;
  arr    : REF ARRAY OF NumberedObject.T;
  line : TEXT;
  pos  : CARDINAL;
  mem  := NEW(REF Memory);
  doneLine : BOOLEAN;
  mSeq := NEW(RefSeq.T).init();

BEGIN
  GatherMatchers();

  Prepare();

  TRY
    LOOP
      doneLine := FALSE;
      line := Rd.GetLine(rd);
      soFar := NEW(CardSetDef.T).init();
      FOR i := FIRST(arr^) TO LAST(arr^) DO
        WITH m   = NARROW(arr[i], M),
             res = RegEx.Execute(m.pat, line, mem := mem) DO
          IF res >= 0 THEN
            Debug.Out("matched rule " & Fmt.Int(i) & " : line \"" & line & "\"");
            pos := res;
            EVAL soFar.insert(m.num);
            m.p(m.a)
          END;
          
          IF doneLine THEN EXIT END
        END 
      END
    END
  EXCEPT
    Rd.EndOfFile => Rd.Close(rd)
  END
END Main.
