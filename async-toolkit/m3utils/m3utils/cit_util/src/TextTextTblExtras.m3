MODULE TextTextTblExtras;
IMPORT TextTextTbl;
IMPORT TextReader;
IMPORT TextList;
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

PROCEDURE Scan(src: TEXT): T =
  VAR
    result := NEW(TextTextTbl.Default).init();
    tr := NEW(TextReader.T).init(src);
    line: TEXT;
  BEGIN
    WHILE tr.next(",\n", line, TRUE) DO
      ScanLine(line, result);
    END;
    RETURN result;
  END Scan;

BEGIN
END TextTextTblExtras.
