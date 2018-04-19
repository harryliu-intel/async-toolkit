MODULE Main;
IMPORT rdlLexExt;
IMPORT rdlParseExt;
IMPORT Stdio;
IMPORT ParseParams;
IMPORT Params;
IMPORT Debug;
IMPORT IO;
IMPORT RdlPropertyBody;
FROM Fmt IMPORT F;
IMPORT TextSetDef;

CONST
  Usage = "[--print-user-def-properties]";

PROCEDURE DoUsage() : TEXT =
  BEGIN RETURN Params.Get(0) & ": usage: " & Usage END DoUsage;

VAR
  lexer  := NEW(rdlLexExt.T, userDefProperties := NEW(TextSetDef.T).init());
  parser := NEW(rdlParseExt.T, lexer := lexer);
  rd     := Stdio.stdin;
  printUserDefProperties : BOOLEAN;
BEGIN
  (* command-line args: *)
  TRY
    WITH pp = NEW(ParseParams.T).init(Stdio.stderr) DO
      printUserDefProperties :=
          pp.keywordPresent("--print-user-def-properties");

      pp.skipParsed();
      pp.finish()
    END
  EXCEPT
    ParseParams.Error => Debug.Error("Command-line params wrong:\n" & DoUsage())
  END;

  EVAL lexer.setRd(rd);
  (* set lexer input *)
  
  EVAL parser.setLex(lexer).parse();
  (* set parser lexer and call the parse method *)

  (* generate output: *)
  IF printUserDefProperties THEN
    VAR
      iter := parser.userDefProperties.iterate();
      p : TEXT;
      q : RdlPropertyBody.T;
    BEGIN
      WHILE iter.next(p, q) DO
        IO.Put(F("USER-DEF-PROPERTY %s\n", p))
      END
    END
  END
END Main.
