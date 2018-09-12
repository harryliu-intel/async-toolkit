MODULE Main;
IMPORT rdlLexExt;
IMPORT rdlParseExt;
IMPORT Stdio;
IMPORT ParseParams;
IMPORT Params;
IMPORT Debug; 
IMPORT TextSetDef;
IMPORT RegAddrmap;
IMPORT Pathname;
IMPORT GenViews, GenViewsM3, GenViewsScala, GenViewsC;
IMPORT Text;

CONST TE = Text.Equal;

CONST Usage = "-top <top map name> [-L|-language m3|scala|c]";

PROCEDURE DoUsage() : TEXT =
  BEGIN RETURN Params.Get(0) & ": usage: " & Usage END DoUsage;

TYPE Lang = { M3, Scala, C };

CONST LangNames = ARRAY Lang OF TEXT { "m3", "scala", "c" };
  
VAR
  lexer  := NEW(rdlLexExt.T, userDefProperties := NEW(TextSetDef.T).init());
  parser := NEW(rdlParseExt.T, lexer := lexer);
  rd     := Stdio.stdin;
  tgtmap : RegAddrmap.T := NIL;
  tgtmapNm : TEXT;
  outDir : Pathname.T := "build/src";
  gv : GenViews.T;
  lang := Lang.M3;
  
BEGIN
  (* command-line args: *)
  TRY
    WITH pp = NEW(ParseParams.T).init(Stdio.stderr) DO
      IF pp.keywordPresent("-top") THEN
        tgtmapNm := pp.getNext()
      END;
      IF pp.keywordPresent("-o") THEN
        outDir := pp.getNext()
      END;
      IF pp.keywordPresent("-L") OR pp.keywordPresent("-language") THEN
        VAR langT := pp.getNext();
            success := FALSE;
        BEGIN
          FOR i := FIRST(Lang) TO LAST(Lang) DO
            IF TE(langT, LangNames[i]) THEN
              lang := i;
              success := TRUE
            END
          END;
          IF NOT success THEN
            Debug.Error("Unsupported target language : " & langT & "\n" & DoUsage())
          END;
        END
      END;
      
      pp.skipParsed();
      pp.finish()
    END;
    IF tgtmapNm = NIL THEN RAISE ParseParams.Error END
  EXCEPT
    ParseParams.Error => Debug.Error("Command-line params wrong:\n" & DoUsage())
  END;

  CASE lang OF
    Lang.M3    => gv := NEW(GenViewsM3.T)
  |
    Lang.Scala => gv := NEW(GenViewsScala.T)
  |
    Lang.C     => gv := NEW(GenViewsC.T)
  END;  

  EVAL lexer.setRd(rd);
  (* set lexer input *)
  
  EVAL parser.setLex(lexer).parse();
  (* set parser lexer and call the parse method *)

  Debug.Out("Done parsing.");

  WITH rdlTgt = parser.defSymtab.lookup(tgtmapNm) DO
    IF rdlTgt = NIL THEN
      Debug.Error("Top symbol not found")
    END;
    tgtmap := gv.decorate(rdlTgt, "").comp
  END;

  gv.gen(tgtmap, outDir)
END Main.
