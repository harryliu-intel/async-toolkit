MODULE Main;
IMPORT IdStyles;
IMPORT Rd, Wr, Stdio;
IMPORT ParseParams;
FROM IdStyles IMPORT Case, Sep;
IMPORT Params;
IMPORT Text;
IMPORT Debug;
IMPORT Thread;

<*FATAL Thread.Alerted*>

CONST TE = Text.Equal;
      
CONST Usage = "-is <none|underscore|hyphen> -os <none|underscore|hyphen> -ic <lower|upper|camel> -oc <lower|upper|camel>";

PROCEDURE DoUsage() : TEXT =
  BEGIN RETURN Params.Get(0) & ": usage: " & Usage END DoUsage;

PROCEDURE ParseSep(txt : TEXT) : Sep RAISES { ParseParams.Error } =
  BEGIN
    FOR i := FIRST(Sep) TO LAST(Sep) DO
      IF TE(IdStyles.SepNames[i],txt) THEN RETURN i END
    END;
    RAISE ParseParams.Error
  END ParseSep;

PROCEDURE ParseCase(txt : TEXT) : Case RAISES { ParseParams.Error } =
  BEGIN
    FOR i := FIRST(Case) TO LAST(Case) DO
      IF TE(IdStyles.CaseNames[i],txt) THEN RETURN i END
    END;
    RAISE ParseParams.Error
  END ParseCase;

VAR
  iSep, oSep : Sep;
  iCase, oCase : Case;
  rd : Rd.T;
  wr : Wr.T;
BEGIN
  TRY
    WITH pp = NEW(ParseParams.T).init(Stdio.stderr) DO
      IF pp.keywordPresent("-is") THEN
        iSep := ParseSep(pp.getNext())
      END;
      IF pp.keywordPresent("-os") THEN
        oSep := ParseSep(pp.getNext())
      END;
      IF pp.keywordPresent("-ic") THEN
        iCase := ParseCase(pp.getNext())
      END;
      IF pp.keywordPresent("-oc") THEN
        oCase := ParseCase(pp.getNext())
      END;
      pp.skipParsed();
      pp.finish()
    END
  EXCEPT
    ParseParams.Error => Debug.Error("Command-line params wrong:\n" & DoUsage())
  END;

  rd := Stdio.stdin;
  wr := Stdio.stdout;
  
  TRY
    VAR
      in := Rd.GetLine(rd);
    BEGIN
      Wr.PutText(wr, IdStyles.Convert(in, iCase, oCase, iSep, oSep));
      Wr.PutChar(wr, '\n');
      Wr.Close(wr)
    END
  EXCEPT
  END
END Main.
      
      
