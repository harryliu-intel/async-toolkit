MODULE M3Ident;
IMPORT Text;
IMPORT CharSeq;
FROM Fmt IMPORT Int;
IMPORT CitTextUtils;
IMPORT Scan;
IMPORT Lex, FloatMode;
IMPORT Debug;

CONST doDebug = FALSE;

CONST OK = SET OF CHAR { 'a'..'z' , 'A'..'Z' , '0'..'9' };
      
CONST Prefix = "m3__";

VAR Pfxa := GetChars(Prefix);

PROCEDURE GetChars(str : TEXT) : REF ARRAY OF CHAR =
  VAR
    res := NEW(REF ARRAY OF CHAR, Text.Length(str));
  BEGIN
    FOR i := FIRST(res^) TO LAST(res^) DO
      res[i] := Text.GetChar(str, i)
    END;
    RETURN res
  END GetChars;

PROCEDURE Escape(str : TEXT) : TEXT =
  VAR
    seq := NEW(CharSeq.T).init();
  BEGIN
    FOR i := FIRST(Pfxa^) TO LAST(Pfxa^) DO
      seq.addhi(Pfxa[i])
    END;
    FOR i := 0 TO Text.Length(str) - 1 DO
      WITH c = Text.GetChar(str, i) DO
        IF c IN OK THEN
          seq.addhi(c)
        ELSE
          seq.addhi('_');
          WITH num = Int(ORD(c)) DO
            FOR j := 0 TO Text.Length(num) - 1 DO
              seq.addhi(Text.GetChar(num, j))
            END
          END;
          seq.addhi('_')
        END
      END
    END;
    RETURN StuffCharSeq(seq)
  END Escape;

PROCEDURE StuffCharSeq(seq : CharSeq.T) : TEXT =
  VAR
    arr := NEW(REF ARRAY OF CHAR, seq.size());
  BEGIN
    FOR i := 0 TO seq.size() - 1 DO
      arr[i] := seq.get(i)
    END;
    RETURN Text.FromChars(arr^)
  END StuffCharSeq;
  
PROCEDURE Unescape(str : TEXT) : TEXT RAISES { Error } =
  VAR
    seq := NEW(CharSeq.T).init();
    i := 0;
  BEGIN
    str := CitTextUtils.CheckPrefix(str, Prefix);

    IF str = NIL THEN RAISE Error END;

    IF doDebug THEN Debug.Out("str = " & str) END;
    
    WITH lim = Text.Length(str) DO
      WHILE i < lim DO
        WITH c = Text.GetChar(str, i) DO
          IF c = '_' THEN
            INC(i);
            VAR
              s := i;
              d := Text.GetChar(str, i);
            BEGIN
              WHILE i < lim AND d # '_' DO
                INC(i);
                d := Text.GetChar(str, i);
              END;
              (* i = lim OR d = '_' *)
              
              TRY
                WITH sub = Text.Sub(str, s, i - s) DO
                  IF doDebug THEN Debug.Out("sub = " & sub) END;
                  WITH num = Scan.Int(sub) DO
                    
                    IF num < 0 OR num > ORD(LAST(CHAR)) THEN
                      RAISE Error
                    ELSE
                      seq.addhi(VAL(num, CHAR));
                      INC(i)
                    END
                  END
                END
              EXCEPT
                FloatMode.Trap, Lex.Error => RAISE Error
              END
            END
          ELSE
            seq.addhi(c);
            INC(i)
          END(*IF*)
        END
      END
    END;

    RETURN StuffCharSeq(seq)
    
  END Unescape;

BEGIN END M3Ident.
