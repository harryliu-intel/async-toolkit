MODULE TextTextTblExtras;
IMPORT TextTextTbl;
IMPORT TextReader;
IMPORT TextList;
IMPORT Text;
FROM Debug IMPORT S;

CONST
  DebugLevel = 30;


PROCEDURE ScanLine(src: TEXT; dest: T) =
  VAR
    words := NEW(TextReader.T).init(src).shatter(" =\t", "", TRUE);
    value: TEXT;
  BEGIN
    S("line = " & src, DebugLevel);
    words := TextList.ReverseD(words);
    IF words # NIL THEN
      value := words.head;
      S("value = " & value, DebugLevel);
      words := words.tail;
      WHILE words # NIL DO
        S("key = " & words.head, DebugLevel);
        EVAL dest.put(words.head, value);
        words := words.tail;
      END;
    END;
  END ScanLine;

PROCEDURE ScanMore(src: TEXT; dest: T) =
  VAR
    tr := NEW(TextReader.T).init(src);
    line: TEXT;
  BEGIN
    WHILE tr.next(",\n", line, TRUE) DO
      ScanLine(line, dest);
    END;
  END ScanMore;

PROCEDURE Scan(src: TEXT): T =
  VAR
    sizeHint := Text.Length(src) DIV 20;
    result := NEW(TextTextTbl.Default).init(sizeHint);
  BEGIN
    ScanMore(src, result);
    RETURN result;
  END Scan;

PROCEDURE ReverseMore(tbl: T; dest: T) =
  VAR
    iter := tbl.iterate();
    key, value: TEXT;
  BEGIN
    WHILE iter.next(key, value) DO
      EVAL dest.put(value, key);
    END;
  END ReverseMore;

PROCEDURE Reverse(tbl: T): T =
  VAR
    sizeHint := tbl.size();
    result := NEW(TextTextTbl.Default).init(sizeHint);
  BEGIN
    ReverseMore(tbl, result);
    RETURN result;
  END Reverse;

BEGIN
END TextTextTblExtras.
