MODULE LibertyCsv;
IMPORT SchemePair;
IMPORT SchemeLongReal;
IMPORT TextReader;
IMPORT RefSeq;
IMPORT Lex, FloatMode;
IMPORT Wx;
IMPORT Scan;
IMPORT Fmt;

PROCEDURE ToList(csvString : TEXT) : SchemePair.T =
  VAR
    reader := NEW(TextReader.T).init(csvString);
    res : SchemePair.T;
    refs := NEW(RefSeq.T).init();
    word : TEXT;
  BEGIN
    WHILE reader.next(",", word, skipNulls := TRUE) DO
      TRY
        WITH lr  = Scan.LongReal(word),
             ref = SchemeLongReal.FromLR(lr) DO
          refs.addhi(ref)
        END
      EXCEPT
        Lex.Error, FloatMode.Trap => refs.addhi(NIL)
      END
    END;

    FOR i := refs.size() - 1 TO 0 BY -1 DO
      res := NEW(SchemePair.T, first := refs.get(i), rest := res)
    END;
    RETURN res
  END ToList;

PROCEDURE ToCsv(p : SchemePair.T) : TEXT =
  VAR
    wx := Wx.New();
  BEGIN
    WHILE p # NIL DO
      VAR
        this : TEXT;
      BEGIN
        TYPECASE p.first OF
          NULL => this := "NULL"
        |
          SchemeLongReal.T(slr) => this := Fmt.LongReal(slr^)
        ELSE
          this := "ERROR"
        END;
        Wx.PutText(wx, this)
      END;
      IF p.rest # NIL THEN
        Wx.PutText(wx, ", ")
      END;
      p := p.rest
    END;
    RETURN Wx.ToText(wx)
  END ToCsv;
  
BEGIN END LibertyCsv.
