MODULE IdStyles;
IMPORT TextSeq;
IMPORT CharSeq;
IMPORT Text;

PROCEDURE Parse(id : TEXT; case : Case; sep : Sep) : TextSeq.T =
  VAR
    res := NEW(TextSeq.T).init();
    cur := NEW(CharSeq.T).init();

  PROCEDURE Pop() =
    (* pop off a word *)
    BEGIN
      IF cur.size() # 0 THEN
        WITH str = NEW(REF ARRAY OF CHAR, cur.size())^ DO
          FOR i := FIRST(str) TO LAST(str) DO
            str[i] := cur.get(i)
          END;
          res.addhi(Text.FromChars(str));
          EVAL cur.init()
        END
      END
    END Pop;

  PROCEDURE Push(z : CHAR) = BEGIN cur.addhi(z) END Push;
    
  BEGIN
    FOR i := 0 TO Text.Length(id)-1 DO
      WITH c = Text.GetChar(id, i),
           d = toLower[c] DO
        IF    case = Case.Camel AND IsUpper(c) THEN
          Pop();
          Push(d)
        ELSIF sep = Sep.Underscore AND c = '_' THEN
          Pop()
        ELSE
          Push(d)
        END
      END
    END;
    Pop();
    RETURN res
  END Parse;

PROCEDURE Format(seq : TextSeq.T; case : Case; sep : Sep) : TEXT =
  VAR
    s := NEW(CharSeq.T).init();
  BEGIN
    FOR i := 0 TO seq.size()-1 DO
      IF i # 0 AND sep = Sep.Underscore THEN
        s.addhi('_')
      END;
      WITH w = seq.get(i) DO
        FOR j := 0 TO Text.Length(w)-1 DO
          WITH c = Text.GetChar(w, j) DO
            <*ASSERT NOT IsUpper(c)*>
            CASE case OF
              Case.Lower => s.addhi(c)
            |
              Case.Upper => s.addhi(toUpper[c])
            |
              Case.Camel =>
              IF j = 0 THEN s.addhi(toUpper[c]) ELSE s.addhi(c) END
            END
          END
        END
      END
    END;
    WITH a = NEW(REF ARRAY OF CHAR, s.size())^ DO
      FOR i := FIRST(a) TO LAST(a) DO
        a[i] := s.get(i)
      END;
      RETURN Text.FromChars(a)
    END
  END Format;

PROCEDURE Convert(id : TEXT; frCase, toCase : Case; frSep, toSep : Sep) : TEXT=
  BEGIN RETURN Format(Parse(id, frCase, frSep), toCase, toSep) END Convert;

PROCEDURE IsLower(c : CHAR) : BOOLEAN =
  BEGIN RETURN c >= 'a' AND c <= 'z' END IsLower;

PROCEDURE IsUpper(c : CHAR) : BOOLEAN =
  BEGIN RETURN c >= 'A' AND c <= 'Z' END IsUpper;
  
VAR
  toLower : ARRAY CHAR OF CHAR;
  toUpper : ARRAY CHAR OF CHAR;
  
BEGIN
  FOR i := FIRST(CHAR) TO LAST(CHAR) DO
    toLower[i] := i;
    toUpper[i] := i;

    IF IsLower(i) THEN toUpper[i] := VAL(ORD(i)-ORD('a')+ORD('A'),CHAR) END;
    IF IsUpper(i) THEN toLower[i] := VAL(ORD(i)-ORD('A')+ORD('a'),CHAR) END
  END
END IdStyles.
