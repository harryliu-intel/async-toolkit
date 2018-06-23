MODULE Main;
IMPORT rdlLexExt;
IMPORT rdlParseExt;
IMPORT Stdio;
IMPORT ParseParams;
IMPORT Params;
IMPORT Debug; 
FROM Fmt IMPORT F;
IMPORT TextSetDef;
IMPORT RTName;
IMPORT RdlComponentDef, RdlComponentDefClass;
IMPORT RdlComponentDefType, RdlComponentDefElem;
IMPORT RdlArray, BigInt;
IMPORT RdlComponentInstElemList;
IMPORT RegField, RegFieldSeq;
IMPORT RegAddrmap;
IMPORT RegReg;
IMPORT RdlComponentDefElemList;
IMPORT RegRegfile;
IMPORT Fmt;
IMPORT RegModula3 AS Tgt; 
IMPORT RegModula3Naming AS TgtNaming;
IMPORT RegModula3Generators AS TgtGenerators;
IMPORT RegChild, RegChildSeq;
IMPORT Text;
IMPORT RegComponent;
IMPORT OSError, Wr, AL;
IMPORT Thread;
IMPORT Pathname;
IMPORT FS;
IMPORT GenViewsM3;

<*FATAL Thread.Alerted*>

CONST Usage = "-top <top map name>";

PROCEDURE DoUsage() : TEXT =
  BEGIN RETURN Params.Get(0) & ": usage: " & Usage END DoUsage;


  
VAR
  lexer  := NEW(rdlLexExt.T, userDefProperties := NEW(TextSetDef.T).init());
  parser := NEW(rdlParseExt.T, lexer := lexer);
  rd     := Stdio.stdin;
  tgtmap : RegAddrmap.T := NIL;
  tgtmapNm : TEXT;
  outDir : Pathname.T := "build/src";
  gv := NEW(GenViewsM3.T);
  
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
      
      pp.skipParsed();
      pp.finish()
    END;
    IF tgtmapNm = NIL THEN RAISE ParseParams.Error END
  EXCEPT
    ParseParams.Error => Debug.Error("Command-line params wrong:\n" & DoUsage())
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
